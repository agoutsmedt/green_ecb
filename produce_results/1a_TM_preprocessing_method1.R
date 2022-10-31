source("packages_and_data.R")
source("function/functions_for_topic_modelling.R")
eurosystem_metadata <- readRDS(here(data_path, "eurosystem_metadata.rds")) %>% 
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

#' `term_list_filtered <- readRDS(here(data_path, "topic_modelling","TM_term_list_inflation.rds"))`
#' We will now produce different set of data depending on different filtering parameters:
#' 
hyper_grid <- expand.grid(
  upper_share = c(0.35), # remove a word if it is appearing in more than upper_share% of the docs
  lower_share = c(0.005), # remove a word if it is appearing in less than lower_share% of the docs
  min_word = 20, # the min number of words in a doc
  max_word = Inf, # the max number of words in a doc
  prop_word = 1) # keep the top prop_word% of the words (in terms of occurrence)

#' The first step is to use a function to create a list of words data depending on different
#' feature selection criteria (listed in `hyper_grid`).

data_set <- create_topicmodels_dataset(hyper_grid, 
                                       term_list_filtered, 
                                       document_name = "document_id")

metadata_to_join <- eurosystem_metadata %>% 
  select(file = file_name, central_bank, year, date) %>%
  mutate(date = dmy(date),
         year = as.integer(year)) %>% 
  inner_join(select(text_inflation, document_id, document_name, file, paragraphs)) %>% 
  filter(document_id %in% unique(data_set$data[[1]]$document)) %>% 
  arrange(document_id) 

#data_set <- create_stm(data_set)

dfm <- cast_dfm(data_set$data[[1]], document, term, count_per_doc) 
stm_corpus <- asSTMCorpus(dfm, data = metadata_to_join)

#' We can create now the metadata
#' 
#' 
 
data_set <- list("data_set" = data_set, "dfm" = dfm, "stm_corpus" = stm_corpus)

saveRDS(data_set, here(data_path, 
                       "topic_modelling",
                       "TM_data_set_inflation.rds"))

#' `data_set <- readRDS(here(data_path, "topic_modelling", "TM_data_set_inflation.rds"))`
#' The second step is to use the different data sets to create stm objects and them to fit 
#' topic models for different number of topics.
#' 

#searchK_topic <- searchK(data_set$stm[[i]]$documents,
#                         data_set$stm[[i]]$vocab,
 #                        prevalence = ~s(year)+central_bank,
  #                       content = ~central_bank,
   #                      data = data_set$stm[[i]]$meta,
    #                     K = 0,
     #                    emtol = 1e-04,
      #                   ngroups = 4)

#saveRDS(topic_model, here(data_path, 
       #                   "topic_modelling",
        #                  "TM_searchK.rds"))

out <- prepDocuments(data_set$stm_corpus$documents,
                     data_set$stm_corpus$vocab,
                     data_set$stm_corpus$data)

nb_topics <- seq(20, 60, 10)
for(K in nb_topics){
  topic_model <- stm(out$documents,
                     out$vocab,
                     data = out$meta,
                     init.type = "Spectral",
                     K = K,
                     emtol = 1e-02,
                     ngroups = 8)
  
  saveRDS(topic_model, here(data_path, 
                            "topic_modelling",
                            paste0("TM_", K, ".rds")))
}

# setting up parallel process
#plan(multisession, workers = 2)

#topic_number <- seq(40, 110, 10) 
#many_models <- create_many_models(data_set, topic_number, max.em.its = 800, seed = 1989)

#' The third step is to calculate different statistics for each model and produce 
#' different plots summarising these statistics.

tuning_results <- stm_results(many_models)
#' If needed, we can save the result: 
#' `saveRDS(tuning_results, here(data_path, "topic_modelling", "topic_models.rds"))`.
#' 
#' And reload them at the beginning of a new session: 
#' `tuning_results <- readRDS(here(eer_data, "3_Topic_modelling", "topic_models.rds"))`.
#'
#' We can now project the different statistics to choose the best model(s).
plot_topic_models  <- plot_topicmodels_stat(tuning_results, nb_terms = 300)

plot_topic_models$summary %>%
  ggplotly() %>% 
  htmlwidgets::saveWidget(here(picture_path, "topic_modelling", "tuning_topicmodels_summary.html"))

ragg::agg_png(here(picture_path, "topic_modelling", "tuning_topicmodels_coherence_vs_exclusivity.png"),
              width = 20, height = 15, units = "cm", res = 300)
plot_topic_models$exclusivity_coherence
invisible(dev.off())

plot_topic_models$exclusivity_coherence_mean %>%
  ggplotly() %>% 
  htmlwidgets::saveWidget(here(picture_path, "topic_modelling", "tuning_topicmodels_frex_general.html"))

#' For remembering the different preprocessing steps: 
#' `tuning_results %>% select(lower_share, min_word, trigram, preprocessing_id) %>% unique`

#' If we want to look at the stat in an interactive framework, we can do:
#' 
#' - `plot_topic_models$summary %>% ggplotly()`;
#' - `plot_topic_models$exclusivity_coherence_mean %>% ggplotly()`

# We can plot different number of topics to see what's the most intuitive results

#' We plot the terms with the highest FREX value for each topic:
#' `topic_model <- readRDS(here(data_path,"topic_modelling", "TM_60.rds"))`
#' `data_set <- readRDS(here(data_path,"topic_modelling", "TM_data_set_inflation.rds"))`

top_terms <- extract_top_terms(topic_model,
                               data_set$data_set$data[[1]],
                               nb_terms = 15,
                               frexweight = 0.3)
k <- topic_model$settings$dim$K

top_beta_graph <- top_terms %>%
  ungroup() %>% 
  filter(measure == "beta") %>% 
  mutate(topic_name = paste0("topic ", topic),
         term = tidytext::reorder_within(term, value, topic)) %>%
  ggplot(aes(value, term, fill = fct_reorder(topic_name, topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ fct_reorder(topic_name, topic), scales = "free", ncol = 9) +
  scale_y_reordered()

ggsave(here("pictures", glue::glue("TM_top_beta_{k}topics.png")), top_beta_graph,
       device = ragg::agg_png,
       width = 50, height = 40, units = "cm", res = 300)

top_frex_graph <- top_terms %>%
  ungroup() %>% 
  filter(measure == "frex") %>% 
  mutate(topic_name = paste0("topic ", topic),
         term = tidytext::reorder_within(term, value, topic)) %>%
  ggplot(aes(value, term, fill = fct_reorder(topic_name, topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ fct_reorder(topic_name, topic), scales = "free", ncol = 9) +
  scale_y_reordered() +
  coord_cartesian(xlim=c(0.98,1))

ggsave(here("pictures", glue::glue("TM_top_frex_{k}topics.png")), top_frex_graph,
       device = ragg::agg_png,
       width = 50, height = 40, units = "cm", res = 300)

texts <- data.table(document_id = stm_corpus$documents %>% names()) %>% 
  left_join(text_inflation)
test <- findThoughts(topic_model, 
                     texts = out$meta$paragraphs,
                     n = 6, 
                     topics = 9)
dt <- make.dt(topic_model, meta = out$meta)
texts <- dt[, docnum[order(Topic2, decreasing=TRUE)][1:5]]
dt[texts]


#saveRDS(filter(tuning_results, preprocessing_id == 1, K == 50), 
 #       here(eer_data, "3_Topic_modelling", "chosen_topic_models.rds"))