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
nb_docs <- length(unique(term_list_filtered$document_id))
dtm <- term_list_filtered %>%
  .[, total_freq := .N, by = "term"] %>% 
  .[, doc_freq := .N, by = c("document_id", "term")] %>%  
  unique() %>% 
  .[, share_of_doc := .N/nb_docs, by = "term"]

# We want to have a look at what we are deleting
look_words_keep <- FALSE
if(look_words_keep){
dtm <- dtm %>% 
  mutate(threshold_nb_doc = case_when(share_of_doc > 0.70 ~ ">70%",
                                    share_of_doc > 0.60 ~ ">60%",
                                    share_of_doc > 0.50 ~ ">50%",
                                    share_of_doc > 0.40 ~ ">40%",
                                    share_of_doc > 0.30 ~ ">30%",
                                    TRUE ~ "<30%"),
         threshold_freq = case_when(total_freq < 10 ~ "<10",
                                    total_freq < 15 ~ "<15",
                                    total_freq < 20 ~ "<20",
                                    total_freq < 25 ~ "<25",
                                    total_freq < 30 ~ "<30",
                                    TRUE ~ ">30")) %>% 
  select(term, share_of_doc, total_freq, threshold_nb_doc, threshold_freq) %>% 
  unique %>% 
  View()
}

list_dtm <- tibble("total_freq" = c(5,10,20)) %>% 
  mutate(data = map(total_freq, ~filter(dtm,
                                 share_of_doc < 0.50,
                                 total_freq >= .) %>% 
               cast_dtm(document_id, term, doc_freq)))




#' We use the `ldatuning` package to test different value of the number of topics.

optimisation_data <- list()
for(i in seq_along(list_dtm$total_freq)){
optimised_lda <- ldatuning::FindTopicsNumber(
  list_dtm$data[[i]],
  topics = seq(from = 30, to = 110, by = 5),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  return_models = TRUE,
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2,
  verbose = TRUE
)
saveRDS(optimised_lda, here(data_path, 
                            "topic_modelling",
                            paste0("LDA_optimized_preprocessing_", i, ".rds")))
optimisation_data[[paste(i)]] <- tibble(optimised_lda)
}

optimised_lda <- bind_rows(optimisation_data, .id = "preprocessing") %>% 
  pivot_longer(matches("\\d$"), names_to = "measure", values_to = "values") %>% 
  mutate(direction = ifelse(str_detect(measure, "^Cao|^Arun"), "minimize", "maximize")) %>% #
  group_by(measure) %>% 
  mutate(rescaled_values = scales::rescale(values))

saveRDS(optimised_lda, here(data_path, 
                            "topic_modelling",
                            "LDA_optimized_preprocessing_all.rds"))

#' Looking at results and chosing best values.
#' Loading optimised LDA:
#' `optimised_lda <- readRDS(here(data_path, "topic_modelling", "LDA_optimized_preprocessing_all.rds"))`


best_values <- optimised_lda %>% 
  group_by(preprocessing, measure) %>% 
  mutate(best_values = ifelse(direction == "minimize", min(rescaled_values), max(rescaled_values))) %>% 
  filter(best_values == rescaled_values)

chosen_values <- data.table(preprocessing = c(3, 3, 3, 2, 2, 1),
                            topics = c(70, 85, 120, 55, 130, 100))

plot_tuning <- ggplot(optimised_lda, aes(topics, rescaled_values, linetype = measure, color = as.factor(preprocessing))) + 
  geom_vline(data = chosen_values, 
             aes(xintercept = topics, color = as.factor(preprocessing)), 
             linetype = "dashed", 
             alpha = 0.5,
             linewidth = 1) +
  geom_line(alpha = 0.7, linewidth = 1) + 
  geom_point(data = best_values, aes(y = best_values), 
             size = 5,
             pch = 4) +
  scale_color_viridis_d(direction = -1, end = 0.8,
                        name = "Vocabulary Pruning",
                        labels = c("Terms occurrence > 5",
                                   "Terms occurrence > 10",
                                   "Terms occurrence > 20")) +
  scale_linetype_manual(name = "Quality Metrics",
                        values = c("solid",
                            "dotdash",
                            "dotted",
                            "dashed")) +
  facet_wrap(~direction, ncol = 1, scales = "free_y") +
  theme_bw() +
  scale_x_continuous(n.breaks = 20) +
  labs(title = "Tuning of topics number according to four measures",
       y = "Rescaled values of measures (0 to 1)",
       x = "Number of topics")

plotly::ggplotly(plot_tuning) %>% 
  htmlwidgets::saveWidget(here("pictures", "tuning_topicmodels.html"))

ggsave(here("pictures", "LDA_tuning.png"),
       device = ragg::agg_png,
       width = 30, height = 20, units = "cm", res = 300)

#' We select a restrictive set of topics and preprocessing we want to explore
#' 

optimised_lda <- optimised_lda %>% 
  ungroup() %>% 
  mutate(chosen_model = FALSE)
for(i in 1:nrow(chosen_values)){
  optimised_lda <- optimised_lda %>% 
    mutate(chosen_model = ifelse(preprocessing == chosen_values$preprocessing[i] & topics == chosen_values$topics[i],
                                 TRUE,
                                 chosen_model))
}

saveRDS(optimised_lda, here(data_path, 
                            "topic_modelling",
                            "LDA_optimized_preprocessing_all.rds"))

#' We select the number of topics depending on the analysis above.
#' 
K = 70
lda <- topicmodels::LDA(list_dtm$data[[1]], k = K, method = "Gibbs", control = list(seed = 1989))


saveRDS(lda, here(data_path, 
                          "topic_modelling",
                          paste0("LDA_", K, ".rds")))

#' ## Analysing results
#' 
#' We can now analyse the results of the model saved
#' `lda <- readRDS(here(data_path, "topic_modelling", paste0("LDA_70.rds")))`
#' `list_lda <- readRDS(here(data_path, "topic_modelling", "LDA_optimized_preprocessing_all.rds"))`
#' 
#' We can plot the top frex and beta words for each topic
tidy(optimised_lda$LDA_model[[1]], matrix = "gamma")
tidy(optimised_lda$LDA_model[[1]])

beta_lda <- tidy(lda, matrix = "beta")
top_beta_graph <- beta_lda %>%
  group_by(topic) %>% 
  slice_max(order_by = beta, n = 20, with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(topic_name = paste0("topic ", topic),
         term = tidytext::reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = fct_reorder(topic_name, topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ fct_reorder(topic_name, topic), scales = "free", ncol = 10) +
  scale_y_reordered()


ggsave(here("pictures", glue::glue("TM_top_beta_{K}_topics_LDA.png")), top_beta_graph,
       device = ragg::agg_png,
       width = 60, height = 50, units = "cm", res = 300)

frex_lda <- calculate_frex(lda, 15, 0.5, topic_method = "LDA")
top_frex_graph <- frex_lda %>%
  group_by(topic) %>% 
  slice_max(order_by = frex, n = 20, with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(topic_name = paste0("topic ", topic),
         term = tidytext::reorder_within(term, frex, topic)) %>%
  ggplot(aes(frex, term, fill = fct_reorder(topic_name, topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ fct_reorder(topic_name, topic), scales = "free", ncol = 10) +
  scale_y_reordered() +
  coord_cartesian(xlim=c(0.99,1))

ggsave(here("pictures", glue::glue("TM_top_frex_{K}_topics_LDA.png")), top_frex_graph,
       device = ragg::agg_png,
       width = 60, height = 50, units = "cm", res = 300)


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
  left_join(select(eurosystem_metadata, year, date, title, description, speaker_cleaned, central_bank, file = file_name, pdf_link))

saveRDS(lda_data, here(data_path, 
                  "topic_modelling",
                  paste0("LDA_", K, "_data.rds")))
