source("packages_and_data.R")
source("function/functions_for_topic_modelling.R")
eurosystem_metadata <- readRDS(here(data_path, "eurosystem_metadata.rds"))
eurosystem_text <- readRDS(here(data_path, "eurosystem_text.rds"))

####################### preparing text for topic modelling ####################

term_list <- data.table(eurosystem_text) %>% 
  unnest_tokens(word, text, token = "ngrams", n_min = 1, n = 3) %>% 
  separate(word, into = c("word_1", "word_2", "word_3"), sep = " ") %>% 
  mutate(unigram = is.na(word_2) & is.na(word_3),
         bigram = !is.na(word_2) & is.na(word_3),
         ngram = ifelse(unigram == TRUE, "unigram", NA),
         ngram = ifelse(bigram == TRUE, "bigram", ngram),
         ngram = ifelse(is.na(ngram), "trigram", ngram),
         word_2 = ifelse(is.na(word_2), "", word_2),
         word_3 = ifelse(is.na(word_3), "", word_3),
#         word_1 = str_remove_all(word_1, paste0(remove_expressions, collapse = "|")),
#         word_2 = str_remove_all(word_2, paste0(remove_expressions, collapse = "|")),
#         word_3 = str_remove_all(word_3, paste0(remove_expressions, collapse = "|")),
         word_1 = textstem::lemmatize_words(word_1),
         word_2 = textstem::lemmatize_words(word_2),
         word_3 = textstem::lemmatize_words(word_3)) %>%
  filter(! str_detect(word_1, "[:digit:]"),
         ! str_detect(word_2, "[:digit:]"),
         ! str_detect(word_3, "[:digit:]"),
         str_count(word_1) != 1,
         str_count(word_2) != 1,
         str_count(word_3) != 1) %>%
  anti_join(stop_words, by = c("word_1" = "word")) %>% 
  anti_join(stop_words, by = c("word_2" = "word")) %>% 
  anti_join(stop_words, by = c("word_3" = "word")) %>% 
  unite(term, word_1, word_2, word_3, sep = " ") %>% 
  mutate(term = str_trim(term, "both")) %>% 
#  filter(! term %in% uninformative_words) %>% 
  select(-unigram, -bigram) %>% 
  data.table()

remove_terms <- c("^_",
                  "european central bank")

term_list <- term_list %>% 
  mutate(term = str_replace_all(term, "ﬁ", "fi")) %>% 
  filter(! str_detect(term, paste0(remove_terms, collapse = "|"))) %>% 
  mutate(document_name = paste0(file, "_page", page))

saveRDS(term_list, here(data_path,
                        "topic_modelling", 
                        "TM_term_list.rds")) 

#' We will now produce different set of data depending on different filtering parameters:
#' 
hyper_grid <- expand.grid(
  upper_share = c(0.4), # remove a word if it is appearing in more than upper_share% of the docs
  lower_share = c(0.005, 0.01, 0.02), # remove a word if it is appearing in less than lower_share% of the docs
  min_word = 15, # the min number of words in a doc
  max_word = Inf, # the max number of words in a doc
  prop_word = 1) # keep the top prop_word% of the words (in terms of occurrence)

#' The first step is to use a function to create a list of words data depending on different
#' feature selection criteria (listed in `hyper_grid`).

data_set <- create_topicmodels_dataset(hyper_grid, 
                                       term_list, 
                                       document_name = "document_name")

saveRDS(data_set, here(data_path, 
                       "topic_modelling",
                       "TM_data_set.rds"))

#' The second step is to use the different data sets to create stm objects and them to fit 
#' topic models for different number of topics.
#' 

# setting up parallel process
plan(multisession, workers = 2)

data_set <- create_stm(data_set)
topic_number <- seq(40, 110, 10) 
many_models <- create_many_models(data_set, topic_number, max.em.its = 800, seed = 1989)

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

top_terms <- extract_top_terms(tuning_results[i],
                               tuning_results[i]$data[[1]],
                               nb_terms = 20,
                               frexweight = 0.3)

top_terms_graph <- top_terms %>%
  filter(measure == "frex") %>% 
  inner_join(topics_with_com[, c("id", "color", "Com_ID", "new_id", "com_color")], by = c("topic" = "id")) %>% 
  mutate(topic = paste0("topic ", topic),
         term = reorder_within(term, value, topic)) %>%
  ggplot(aes(value, term)) +
  scale_fill_identity() +
  geom_col(aes(fill = com_color), show.legend = FALSE) +
  facet_wrap(~ fct_reorder(topic, new_id), scales = "free") +
  scale_y_reordered() +
  coord_cartesian(xlim=c(0.96,1))

ragg::agg_png(here(tm_picture_path, glue("TM_top_terms_{i}topics.png")),
              width = 50, height = 40, units = "cm", res = 300)
top_terms_graph
invisible(dev.off())

saveRDS(filter(tuning_results, preprocessing_id == 1, K == 50), 
        here(eer_data, "3_Topic_modelling", "chosen_topic_models.rds"))