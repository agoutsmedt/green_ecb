source("packages_and_data.R")
source("function/functions_for_topic_modelling.R")

list_roll <- readRDS(here(data_path,"rollingLDA", "roll_full.rds"))
optimised_lda <- readRDS(here(data_path, "topic_modelling", "LDA_optimized_preprocessing_all.rds"))

#' We will extract the `nb_top_words` for each topics in each model
nb_top_words <- 20

rollinglda_topwords <- tibble(model_id = str_c("ldarolling_", seq(30, 90, 20))) %>% 
  mutate(top_words = map(str_extract(model_id, "\\d+"),
                         ~ pluck(list_roll, ., "lda", "topics") %>% 
                           topWords(nb_top_words) %>% 
                           as_tibble() %>% 
                           pivot_longer(everything(), names_to = "topic", values_to = "term") %>% 
                           mutate(topic = str_remove(topic, "V") %>% as.integer()))) %>% 
  unnest(top_words)

lda_topwords <- optimised_lda %>% 
  select(preprocessing, topics, LDA_model, chosen_model) %>%
  filter(chosen_model == TRUE) %>% 
  unique() %>% 
  arrange(preprocessing, topics) %>% 
  mutate(model_id = paste0("LDA_", preprocessing, "_", topics)) %>% 
  select(model_id, LDA_model) %>% 
  mutate(top_words = map(1:n(), ~ pluck(LDA_model, .) %>% 
                           tidy(matrix = "beta") %>% 
                           group_by(topic) %>% 
                           slice_max(beta, n = nb_top_words, with_ties = FALSE))) %>% 
  select(-LDA_model) %>% 
  unnest(top_words) %>% 
  select(-beta)

#' We merge the top words coming from rollinglda models and from LDA models 
topwords <- lda_topwords %>% 
  bind_rows(rollinglda_topwords)

#' The function generates a list of `nb_check_words` among the top words of each
#' topic in each model, and add an intruder. A random identifier is given to the couple
#' model/topic number to avoid biases in selection.
intruders_table <- generate_intruder_list(topwords, nb_check_words = 6) 

saveRDS(intruders_table, here(data_path,
                              "intruders_check_list.rds"))

#' We now generate a random extract of `nb_topic_check` topics per model, each time 
#' different for each checker.
#' `intruders_table <- readRDS(here(data_path, "intruders_check_list.rds"))`

save_intruders <- FALSE
if(save_intruders){
  nb_topic_check <- 30
  if(googledrive::drive_get("Check ECB topic intruders") %>% nrow() == 0){
    googlesheets4::gs4_create("Check ECB topic intruders")
  }
  
  checkers <- c("Aurélien", "Clément", "Aurélien_bis", "Tanguy", "Antoine", "Morgane")
  for(checker in checkers){
    check_table <- intruders_table %>% 
      distinct(model_id, check_id, list_words) %>% 
      group_by(model_id) %>% 
      slice_sample(n = nb_topic_check) %>%
      ungroup() %>% 
      select(check_id, list_words) %>% 
      mutate(intruder_number = NA_integer_) %>% 
      arrange(check_id)
    
    googlesheets4::sheet_write(check_table,
                               googledrive::drive_get("Check ECB topic intruders")$id,
                               glue::glue("intruders table {checker}"))
    
    googlesheets4::range_autofit(googledrive::drive_get("Check ECB topic intruders")$id,
                                 sheet = glue::glue("intruders table {checker}"))
  }
  
  googlesheets4::sheet_delete(googledrive::drive_get("Check ECB topic intruders")$id,
                              "Feuille 1")
}

intruders_checked <- list()
for(checker in checkers){
  drive_table <- googledrive::drive_get("Check ECB topic intruders")$id %>% 
    googlesheets4::read_sheet(sheet = glue::glue("intruders table {checker} filled")) %>% 
    mutate(checker = checker)
  
  intruders_checked[[checker]] <- drive_table
}

#' if necessary, reimport the saved list of intruders:
#' `intruders_table <- readRDS(here(data_path, "intruders_check_list.rds"))`

intruders_checked <- bind_rows(intruders_checked) %>% 
  left_join(select(intruders_table, model_id, topic, check_id, rank_intruder) %>% unique())

stats_intruders <- intruders_checked %>% 
  filter(! is.na(intruder_number)) %>% 
  group_by(checker, model_id) %>% 
  mutate(weird_topic = (sum(intruder_number == 0)/n() * 100) %>% round(., digits = 2),
         no_idea = (sum(intruder_number == 8)/n() * 100) %>% round(., digits = 2),
         wrong_intruder = (sum(intruder_number != rank_intruder)/n() * 100) %>% round(., digits = 2),
         gross_true_intruder = (sum(intruder_number == rank_intruder)/n() * 100) %>% round(., digits = 2),
         corrected_true_intruder = (sum(intruder_number == rank_intruder)/(n() - sum(intruder_number == 8)) * 100) %>% round(., digits = 2)) %>% 
  distinct(checker, model_id, weird_topic, no_idea, wrong_intruder, gross_true_intruder, corrected_true_intruder) %>% 
  pivot_longer(where(is_double), names_to = "measure", values_to = "percentage")

intruders_plot <- stats_intruders  %>% 
  filter(checker %in% c("Aurélien", "Clément", "Tanguy"),
         str_detect(model_id, "^LDA"),
         measure == "corrected_true_intruder") %>% 
  group_by(model_id, measure) %>% 
  mutate(mean_percentage = mean(percentage)) %>% 
  ungroup() %>%  
  distinct(model_id, measure, mean_percentage) %>%
  ggplot(aes(x = reorder_within(model_id, mean_percentage, measure), y = mean_percentage)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_x_reordered() +
  labs(x = NULL,
       y = "Percentage of good intruders") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
ggsave(here("pictures", "plot_intruder.png"),
       intruders_plot,
       device = ragg::agg_png,
       width = 30, height = 18, units = "cm", res = 300)

stats_intruders  %>% 
  filter(checker %in% c("Aurélien", "Clément", "Tanguy"),
         str_detect(model_id, "^LDA")) %>% 
  group_by(model_id, measure) %>% 
  mutate(mean_percentage = mean(percentage)) %>% 
  ungroup() %>%  
  distinct(model_id, measure, mean_percentage) %>%
ggplot(aes(x = reorder_within(model_id, mean_percentage, measure), y = mean_percentage, fill = model_id)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(~ measure, scales = "free") +
  labs(x = NULL,
       y = "Percentage") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
