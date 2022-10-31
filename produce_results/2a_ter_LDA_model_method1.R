############ Loading packages and files #########################
source("packages_and_data.R")
source("function/functions_for_topic_modelling.R")
eurosystem_metadata <- readRDS(here(data_path, "eurosystem_metadata.rds")) %>% 
  arrange(file_name)
eurosystem_text <- readRDS(here(data_path, "eurosystem_text_inflation.rds")) %>% 
  arrange(document_id)
term_list_filtered <- readRDS(here(data_path, "topic_modelling","TM_term_list_inflation.rds"))


dtm   <- term_list_filtered %>%
  .[, freq := .N, by = c("document_id", "term")] %>%  
  select(doc_id = document_id, term, freq) %>% 
  document_term_matrix()
dtm_filtered   <- dtm_remove_lowfreq(dtm, minfreq = 10) %>% 
  dtm_remove_tfidf(prob = 0.50)

lda <- topicmodels::LDA(dtm_filtered, k = 30, control = list(seed = 1989))

data_lda <- tidy(lda, matrix = "beta")
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



