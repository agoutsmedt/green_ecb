############ Loading packages and files #########################
source("packages_and_data.R")
source("function/functions_for_topic_modelling.R")
eurosystem_metadata <- readRDS(here(data_path, "eurosystem_metadata.rds")) %>% 
  arrange(file_name)
eurosystem_text <- readRDS(here(data_path, "eurosystem_text_inflation.rds")) %>% 
  arrange(document_id)
term_list_filtered <- readRDS(here(data_path, "topic_modelling","TM_term_list_inflation.rds"))

#' We transform the list of term pre-established in a Document Term Matrix and we filter.
#' (Ideally, different filters should be tested)
dtm   <- term_list_filtered %>%
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
lda <- topicmodels::LDA(dtm_filtered, k = K, control = list(seed = 1989))

saveRDS(lda, here(data_path, 
                          "topic_modelling",
                          paste0("LDA_", K, ".rds")))

#' We can now analyse the results

data_lda <- tidy(lda, matrix = "beta", log = TRUE)
top_beta_graph <- data_lda %>%
  group_by(topic) %>% 
  slice_max(order_by = beta, n = 20, with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(topic_name = paste0("topic ", topic),
         term = tidytext::reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = fct_reorder(topic_name, topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ fct_reorder(topic_name, topic), scales = "free", ncol = 7) +
  scale_y_reordered()


ggsave(here("pictures", glue::glue("TM_top_beta_20topics_LDA.png")), top_beta_graph,
       device = ragg::agg_png,
       width = 50, height = 40, units = "cm", res = 300)

texts_lda <- tidy(lda, matrix = "gamma") %>% 
  group_by(topic) %>% 
  slice_max(order_by = gamma, n = 10)


ctm <- topicmodels::CTM(dtm, k = 20, control = list(seed = 1989), verbose = 1)



