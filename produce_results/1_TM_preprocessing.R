source("packages_and_data.R")
source("function/functions_for_topic_modelling.R")
eurosystem_metadata <- readRDS(here(data_path, "eurosystem_metadata.rds"))
eurosystem_text <- readRDS(here(data_path, "eurosystem_text.rds"))

####################### preparing text for topic modelling ####################

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

n_gram <- 3
columns <- paste0("word_", 1:n_gram)

term_list <- eurosystem_text %>% 
  unnest_tokens(word, text, token = "ngrams", n_min = 1, n = n_gram) %>% 
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
  mutate(document_name = paste0(file, "_page", page)) %>% 
  arrange(document_name)
         
saveRDS(term_list_filtered, here(data_path,
                        "topic_modelling", 
                        "TM_term_list.rds")) 

#' `term_list_filtered <- readRDS(here(data_path, "topic_modelling","TM_term_list.rds"))`
#' We will now produce different set of data depending on different filtering parameters:
#' 
hyper_grid <- expand.grid(
  upper_share = c(0.35), # remove a word if it is appearing in more than upper_share% of the docs
  lower_share = c(0.005, 0.01, 0.02), # remove a word if it is appearing in less than lower_share% of the docs
  min_word = 15, # the min number of words in a doc
  max_word = Inf, # the max number of words in a doc
  prop_word = 1) # keep the top prop_word% of the words (in terms of occurrence)

#' The first step is to use a function to create a list of words data depending on different
#' feature selection criteria (listed in `hyper_grid`).

data_set <- create_topicmodels_dataset(hyper_grid, 
                                       term_list_filtered, 
                                       document_name = "document_name")
data_set <- create_stm(data_set)


#' We can create now the metadata
#' 

metadata_to_join <- eurosystem_metadata %>% 
  select(file = file_name, central_bank, year, date) %>%
  mutate(date = dmy(date)) %>% 
  left_join(select(eurosystem_text, document_name, file))
  
metadata_to_join <- metadata_to_join %>% 
  ungroup() %>% 
  left_join(metadata_to_join) %>% 
  select(document_name, central_bank, year, date) %>% 
  unique

for(i in 1:nrow(data_set)){
  metadata <- data.table("document_name" = rownames(data_set$dfm[[i]]))
  
  metadata <- metadata %>% 
    left_join(metadata_to_join)
    
  data_set$stm[[i]]$meta$year <- as.integer(metadata$year)
  data_set$stm[[i]]$meta$date <- as.integer(metadata$date)
  data_set$stm[[i]]$meta$central_bank <- metadata$central_bank
}

saveRDS(data_set, here(data_path, 
                       "topic_modelling",
                       "TM_data_set.rds"))

#' `data_set <- readRDS(here(data_path, "topic_modelling", "TM_data_set.rds"))`
#' The second step is to use the different data sets to create stm objects and them to fit 
#' topic models for different number of topics.
#' 

i = 1
searchK_topic <- searchK(data_set$stm[[i]]$documents,
                         data_set$stm[[i]]$vocab,
                         prevalence = ~s(year)+central_bank,
                         content = ~central_bank,
                         data = data_set$stm[[i]]$meta,
                         K = 0,
                         emtol = 1e-04,
                         ngroups = 4)

saveRDS(topic_model, here(data_path, 
                           "topic_modelling",
                           "TM_searchK.rds"))

nb_topics <- seq(30, 120, 10)
for(K in nb_topics){
topic_model <- stm(data_set$stm[[i]]$document,
                   data_set$stm[[i]]$vocab,
                   prevalence = ~s(year)+central_bank,
                   content = ~central_bank,
                   data = data_set$stm[[i]]$meta,
                   init.type = "Spectral",
                   K = K,
                   emtol = 1e-04,
                   ngroups = 4)

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
#' `topic_model <- readRDS(here(data_path,"topic_modelling", "TM_50.rds"))`
#' `data_set <- readRDS(here(data_path,"topic_modelling", "TM_data_set.rds"))`

top_terms <- extract_top_terms(topic_model,
                               data_set$data[[2]],
                               nb_terms = 15,
                               frexweight = 0.3)
k <- topic_model$settings$dim$K

top_beta_graph <- top_terms %>%
  ungroup() %>% 
  filter(measure == "beta") %>% 
  mutate(topic = paste0("topic ", topic),
         term = tidytext::reorder_within(term, value, topic)) %>%
  ggplot(aes(value, term, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 9) +
  scale_y_reordered()


ragg::agg_png(here("pictures", glue("TM_top_beta_{k}topics.png")),
              width = 50, height = 40, units = "cm", res = 300)
top_beta_graph
invisible(dev.off())

top_frex_graph <- top_terms %>%
  ungroup() %>% 
  filter(measure == "frex") %>% 
  mutate(topic = paste0("topic ", topic),
         term = tidytext::reorder_within(term, value, topic)) %>%
  ggplot(aes(value, term, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 9) +
  scale_y_reordered() +
  coord_cartesian(xlim=c(0.98,1))


ragg::agg_png(here("pictures", glue("TM_top_frex_{k}topics.png")),
              width = 50, height = 40, units = "cm", res = 300)
top_frex_graph
invisible(dev.off())

saveRDS(filter(tuning_results, preprocessing_id == 1, K == 50), 
        here(eer_data, "3_Topic_modelling", "chosen_topic_models.rds"))