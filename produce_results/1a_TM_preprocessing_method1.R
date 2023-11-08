source("packages_and_data.R")
source("function/functions_for_topic_modelling.R")
eurosystem_metadata <- readRDS(here(data_path, "eurosystem_metadata_inflation.rds")) %>% 
  arrange(file_name)
text_inflation <- readRDS(here(data_path, "eurosystem_text_inflation.rds")) %>% 
  arrange(document_id)

####################### preparing text for topic modelling ####################

#' ## Stop Words
#' We first constitute the list of stop_words. We use an extended list, in which we 
#' still want to keep some words.
#' 

stop_words_increased <- stop_words %>% 
  rbind(data.frame("word" = stopwords::data_stopwords_nltk$en, lexicon = "nltk")) %>% 
  rbind(data.frame("word" = stopwords::data_stopwords_stopwordsiso$en, lexicon = "iso"))

to_keep <- c("research",
             "announce",
             "cause",
             "causes",
             "cases",
             "computer",
             "fact",
             "facts",
             "general",
             "good",
             "goods",
             "group",
             "groups",
             "information",
             "interest",
             "invention",
             "lower",
             "member",
             "members",
             "uk",
             "zero")

stop_words_increased <- stop_words_increased %>%
  filter(! word %in% to_keep) %>% 
  select(word) %>% 
  unique

#' ## Tokenising the texts
#' 
#' We first fix the larger ngrams we want to keep
n_gram <- 3
columns <- paste0("word_", 1:n_gram)

#' we proceed to tokenisation and cleaning
#' For later: improving by statistical methods the selection of ngrams
#' 

term_list <- text_inflation %>% 
  unnest_tokens(word, paragraphs, token = "ngrams", n_min = 1, n = n_gram) %>% 
  as.data.table %>% 
  .[, word := str_replace_all(word, "ﬁ", "fi")] %>% 
  .[, (columns) := tstrsplit(word, " ")] %>% 
  .[, `:=` (unigram = is.na(word_2) & is.na(word_3),
            bigram = !is.na(word_2) & is.na(word_3))] %>% 
  .[, ngram := ifelse(unigram == TRUE, "unigram", NA)] %>% 
  .[, ngram := ifelse(bigram == TRUE, "bigram", ngram)] %>% 
  .[, ngram := ifelse(is.na(ngram), "trigram", ngram)] %>% 
  .[, `:=` (word_2 = ifelse(is.na(word_2), "", word_2),
            word_3 = ifelse(is.na(word_3), "", word_3))] %>% 
  .[, (columns) := map(.SD, lemmatize_words), .SDcols = columns] %>% 
  .[, (columns) := map(.SD, ~str_remove(., "’.*")), .SDcols = columns] %>% 
  filter(if_all(starts_with("word_"), ~ ! str_detect(., "[:digit:]")),
         if_all(starts_with("word_"), ~ str_count(.) != 1)) %>%
  anti_join(stop_words_increased, by = c("word_1" = "word")) %>% 
  anti_join(stop_words_increased, by = c("word_2" = "word")) %>% 
  anti_join(stop_words_increased, by = c("word_3" = "word")) %>% 
  unite(term, word_1, word_2, word_3, sep = " ") %>% 
  .[, term := str_trim(term, "both")] %>%  
  select(-word, -unigram, -bigram)

to_remove <- c("www\\.",
               "für$",
               "^und$",
               "^buch$",
               "bundesbank\\.de",
               "strasse")

term_list_filtered <- term_list %>% 
  filter(! str_detect(term, paste0(to_remove, collapse = "|"))) %>% 
  arrange(document_id)

saveRDS(term_list_filtered, here(data_path,
                                 "topic_modelling", 
                                 "TM_term_list_inflation.rds")) 
