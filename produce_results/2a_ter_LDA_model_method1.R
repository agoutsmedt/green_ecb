############ Loading packages and files #########################
source("packages_and_data.R")
source("function/functions_for_topic_modelling.R")
eurosystem_metadata <- readRDS(here(data_path, "eurosystem_metadata_inflation.rds")) %>% 
  arrange(file_name)
eurosystem_text <- readRDS(here(data_path, "eurosystem_text_inflation.rds")) %>% 
  arrange(document_id)
term_list_filtered <- readRDS(here(data_path, "topic_modelling","TM_term_list_inflation.rds")) %>% 
  arrange(file)

#' We transform the list of term pre-established in a Document Term Matrix and we filter.
#' (Ideally, different filters should be tested)
dtm <- term_list_filtered %>%
  .[, freq := .N, by = c("document_id", "term")] %>%  
  select(doc_id = document_id, term, freq) %>% 
  document_term_matrix()
dtm_filtered   <- dtm_remove_lowfreq(dtm, minfreq = 10) %>% 
  dtm_remove_tfidf(prob = 0.50)

#' We use the `ldatuning` package to test different value of the number of topics.
optimised_lda <- ldatuning::FindTopicsNumber(
  dtm_filtered,
  topics = seq(from = 20, to = 90, by = 10),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 3,
  verbose = TRUE
)
saveRDS(optimised_lda, here(data_path, 
                  "topic_modelling",
                  paste0("LDA_optimized.rds")))

ldatuning::FindTopicsNumber_plot(optimised_lda)

#' We select the number of topics depending on the analysis above.
K = 30
lda <- topicmodels::LDA(dtm_filtered, k = K, method = "Gibbs", control = list(seed = 1989))

saveRDS(lda, here(data_path, 
                          "topic_modelling",
                          paste0("LDA_", K, ".rds")))

#' ## Analysing results
#' 
#' We can now analyse the results of the model saved
#' `lda <- readRDS(here(data_path, "topic_modelling", paste0("LDA_50.rds")))`
#' 
#' We can plot the top frex and beta words for each topic

beta_lda <- tidy(lda, matrix = "beta")
top_beta_graph <- beta_lda %>%
  group_by(topic) %>% 
  slice_max(order_by = beta, n = 20, with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(topic_name = paste0("topic ", topic),
         term = tidytext::reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = fct_reorder(topic_name, topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ fct_reorder(topic_name, topic), scales = "free", ncol = 9) +
  scale_y_reordered()


ggsave(here("pictures", glue::glue("TM_top_beta_{K}_topics_LDA.png")), top_beta_graph,
       device = ragg::agg_png,
       width = 50, height = 40, units = "cm", res = 300)

frex_lda <- calculate_frex(lda, 15, 0.5, topic_method = "LDA")
top_frex_graph <- frex_lda %>%
  group_by(topic) %>% 
  slice_max(order_by = frex, n = 20, with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(topic_name = paste0("topic ", topic),
         term = tidytext::reorder_within(term, frex, topic)) %>%
  ggplot(aes(frex, term, fill = fct_reorder(topic_name, topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ fct_reorder(topic_name, topic), scales = "free", ncol = 9) +
  scale_y_reordered() +
  coord_cartesian(xlim=c(0.99,1))

ggsave(here("pictures", glue::glue("TM_top_frex_{K}_topics_LDA.png")), top_frex_graph,
       device = ragg::agg_png,
       width = 50, height = 40, units = "cm", res = 300)


#' We give names to the topics, from the beta values (frex are sometimes too specific)
topic_name <- beta_lda %>%
  group_by(topic) %>% 
  slice_max(order_by = beta, n = 5, with_ties = FALSE) %>% 
  mutate(topic_name = paste0(term, collapse = "; ")) %>% 
  select(topic, topic_name) %>% 
  unique

#' ## Analysing texts

lda_data <- tidy(lda, matrix = "gamma") %>% 
  left_join(topic_name) %>% 
  rename(document_id = document) %>% 
  left_join(eurosystem_text) %>% 
  left_join(select(eurosystem_metadata, year, date, title, description, speaker_cleaned, central_bank, file = file_name))

saveRDS(lda_data, here(data_path, 
                  "topic_modelling",
                  paste0("LDA_", K, "_data.rds")))
