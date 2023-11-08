source("packages_and_data.R")
source("function/functions_for_topic_modelling.R")
<<<<<<< HEAD
package_list <- c("vegan")

for(package in package_list){
  if(package %in% installed.packages() == FALSE){
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}

eurosystem_metadata <- readRDS(here(data_path, "eurosystem_metadata_inflation.rds")) %>% 
  arrange(file_name)
eurosystem_text <- readRDS(here(data_path, "eurosystem_text_inflation.rds")) %>% 
  arrange(document_id)

#' Following the track of intruders
K <- 120
preprocessing_value <- "3"

lda_data <- readRDS(here(data_path, "topic_modelling", "LDA_optimized_preprocessing_all.rds")) %>% 
  filter(preprocessing == preprocessing_value,
                           topics == K) %>% 
  slice(1) %>% 
  pluck("LDA_model", 1)

saveRDS(lda_data, here(data_path, 
                  "topic_modelling",
                  paste0("LDA_", K, ".rds")))

#' ## Analysing results
#' 
#' We can now analyse the results of the model saved
#' `lda <- readRDS(here(data_path, "topic_modelling", paste0("LDA_120.rds")))`
#' `list_lda <- readRDS(here(data_path, "topic_modelling", "LDA_optimized_preprocessing_all.rds"))`
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
  facet_wrap(~ fct_reorder(topic_name, topic), scales = "free", ncol = 12) +
  scale_y_reordered()


ggsave(here("pictures", glue::glue("TM_top_beta_{K}_topics_LDA.png")), top_beta_graph,
       device = ragg::agg_png,
       width = 70, height = 60, units = "cm", res = 300)

frex_lda <- calculate_frex(lda, 15, 0.5, topic_method = "LDA")
top_frex_graph <- frex_lda %>%
  group_by(topic) %>% 
  slice_max(order_by = frex, n = 20, with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(topic_name = paste0("topic ", topic),
         term = tidytext::reorder_within(term, frex, topic)) %>%
  ggplot(aes(frex, term, fill = fct_reorder(topic_name, topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ fct_reorder(topic_name, topic), scales = "free", ncol = 12) +
  scale_y_reordered() +
  coord_cartesian(xlim=c(0.99,1))

ggsave(here("pictures", glue::glue("TM_top_frex_{K}_topics_LDA.png")), top_frex_graph,
       device = ragg::agg_png,
       width = 70, height = 60, units = "cm", res = 300)


#' We give names to the topics, from the beta values (frex are sometimes too specific)
topic_name <- beta_lda %>%
  group_by(topic) %>% 
  slice_max(order_by = beta, n = 5, with_ties = FALSE) %>% 
  mutate(topic_name = paste0(term, collapse = "; ")) %>% 
  select(topic, topic_name) %>% 
  unique

#' We identify topics that are on inflation
#' 
inflation_topics <- beta_lda %>% 
  group_by(topic) %>% 
  slice_max(order_by = beta, n = 5, with_ties = FALSE) %>% 
  mutate(inflation_topic = str_detect(term, "inflation|price") & 
           ! str_detect(term, "asset price")) %>%
  distinct(topic, inflation_topic) %>% 
  filter(inflation_topic == TRUE)

topic_name <- topic_name %>% 
  left_join(inflation_topics) %>% 
  mutate(inflation_topic = if_else(is.na(inflation_topic), FALSE, inflation_topic))

#' ## Analysing texts

lda_data <- tidy(lda, matrix = "gamma") %>% 
  left_join(topic_name) %>% 
  rename(document_id = document) %>% 
  left_join(eurosystem_text) %>% 
  left_join(select(eurosystem_metadata, year, date, title, description, speaker_cleaned, file = file_name, pdf_link))

saveRDS(lda_data, here(data_path, 
                       "topic_modelling",
                       paste0("LDA_", K, "_data.rds")))
=======
K = 70
lda_data <- readRDS(here(data_path, 
             "topic_modelling",
             paste0("LDA_", K, "_data.rds")))
>>>>>>> 94e4769367dc1858bff47b054eabd826cd2681e7


#' We first explore the data per year. 
rects <- tribble(~start, ~end,
                 "1998-11-20", "2011-11-08",
                 "2011-11-08", "2021-09-01",
                 "2021-09-01", "2023-02-01") %>% 
  mutate(start = date(start),
         end = date(end))

topic_per_date <- lda_data %>% 
  mutate(topic_name = paste0("topic ", topic, ": ", str_wrap(topic_name, 30))) %>% 
  ggplot(aes(date, gamma, color = topic_name)) +
  geom_smooth(show.legend = FALSE, method = "loess", span = 0.2) +
  geom_vline(xintercept = date(c("2011-11-08", "2021-09-01"))) +
 # geom_rect(data=rects, inherit.aes=FALSE, aes(xmin=start, xmax=end,), ymax = 1, ymin = 0, color="transparent", fill="grey", alpha=0.3) +
  facet_wrap(~ topic_name) +
  theme(strip.text = element_text (size = 9)) 

ggsave(here("pictures", glue::glue("TM_LDA_topic_per_date.png")), topic_per_date,
       device = ragg::agg_png,
<<<<<<< HEAD
       width = 70, height = 65, units = "cm", res = 300)

for(i in unique(lda_data$topic)){
  
data <- lda_data %>% 
  filter(topic == i)
#  mutate(topic_name = paste0("topic ", topic, ": ", str_wrap(topic_name, 30))) %>% 
plot <- ggplot(data, aes(date, gamma, color = topic_name)) +
  geom_smooth(show.legend = FALSE, method = "loess", span = 0.2) +
  geom_vline(xintercept = date(c("2011-11-08", "2021-09-01"))) +
  theme(strip.text = element_text (size = 9)) +
  labs(title = glue::glue("Topic {i} - {unique(data$topic_name)}"),
       x = NULL,
       y = "Prevalence of topic")

ggsave(here("pictures/topics/", glue::glue("topic_{i}.png")), plot,
       device = ragg::agg_png,
       width = 20, height = 15, units = "cm", res = 300)
}
=======
       width = 60, height = 55, units = "cm", res = 300)

cb_focus <- c("european central bank",
              "bundesbank",
              "bank of france")
              
topic_per_date_cb <- lda_data %>% 
  mutate(topic_name = paste0("topic ", topic, ": ", str_wrap(topic_name, 30))) %>%
  filter(central_bank %in% cb_focus) %>% 
  ggplot(aes(date, gamma, color = topic_name, linetype = central_bank)) +
  geom_smooth(alpha = 0.8, size = 0.8, se = FALSE) +
  geom_rect(data=rects, inherit.aes=FALSE, aes(xmin=start, xmax=end,), ymax = 1, ymin = 0, color="transparent", fill="grey", alpha=0.3) +
  facet_wrap(~ topic_name) +
  theme(strip.text = element_text (size = 9), legend.position = "bottom") +
  guides(color = "none")

ggsave(here("pictures", glue::glue("TM_LDA_topic_per_date_cb.png")), topic_per_date_cb,
       device = ragg::agg_png,
       width = 50, height = 45, units = "cm", res = 300)
>>>>>>> 94e4769367dc1858bff47b054eabd826cd2681e7

#' Evolution of specific topics
#' 
#' 

<<<<<<< HEAD
#topic_focus <- c(17, 37, 48, 63, 20, 56)

focus_topic_per_date <- lda_data %>% 
  mutate(topic_name = paste0("topic ", topic, ": ", str_wrap(topic_name, 30))) %>%
  filter(inflation_topic == TRUE) %>% 
  ggplot(aes(date, gamma, color = topic_name)) +
  geom_smooth(method = "loess", span = 0.2, show.legend = FALSE) +
  geom_vline(xintercept = date(c("2011-11-08", "2021-09-01"))) +
#  geom_rect(data=rects, inherit.aes=FALSE, aes(xmin=start, xmax=end,), ymax = 1, ymin = 0, color="transparent", fill="grey", alpha=0.3) +
  facet_wrap(~ topic_name, ncol = 5) +
=======
topic_focus <- c(17, 37, 48, 63, 20, 56)

focus_topic_per_date <- lda_data %>% 
  mutate(topic_name = paste0("topic ", topic, ": ", str_wrap(topic_name, 30))) %>%
  filter(topic %in% topic_focus) %>% 
  ggplot(aes(date, gamma, color = topic_name, linetype = central_bank)) +
  geom_smooth(method = "loess", span = 0.2, show.legend = FALSE) +
  geom_vline(xintercept = date(c("2011-11-08", "2021-09-01"))) +
#  geom_rect(data=rects, inherit.aes=FALSE, aes(xmin=start, xmax=end,), ymax = 1, ymin = 0, color="transparent", fill="grey", alpha=0.3) +
  facet_wrap(~ topic_name, ncol = 2) +
>>>>>>> 94e4769367dc1858bff47b054eabd826cd2681e7
  theme(strip.text = element_text (size = 15), legend.position = "bottom") +
  guides(color = "none")

ggsave(here("pictures", glue::glue("TM_LDA_main_topic_per_date.png")), focus_topic_per_date,
       device = ragg::agg_png,
       width = 50, height = 45, units = "cm", res = 300)


#' We create two subsamples of years: 2006-2008, 2021-2022
#' 
data_year_subset <- lda_data %>% 
  mutate(period = case_when(between(date, "1998-11-20", "2011-11-08") ~ "Period_1 (1998-2011)",
                            between(date, "2011-11-08", "2021-09-01") ~ "Period_2 (2011-2021)",
                            between(date, "2021-09-01", "2023-02-01") ~ "Period_3 (2021-)")) %>% 
<<<<<<< HEAD
  filter(! is.na(period),
         inflation_topic == TRUE) 
=======
  filter(! is.na(period)) 
>>>>>>> 94e4769367dc1858bff47b054eabd826cd2681e7


data <- data_year_subset %>% 
  group_by(topic, period) %>% 
  mutate(mean = mean(gamma),
         st_err = sd(gamma)/sqrt(length(gamma))) %>% 
  select(topic, topic_name, period, mean, st_err) %>% 
  unique()

data %>% 
  mutate(topic_name = paste0("topic ", topic, ": ", str_wrap(topic_name, 50))) %>% 
  ggplot(aes(mean, fct_reorder(topic_name, mean), color = period)) +
  geom_errorbar(aes(xmin = mean - st_err, xmax = mean + st_err)) +
  geom_point(show.legend = FALSE) +
  labs(x = "Prevalence of the topic")

ggsave(here("pictures", glue::glue("TM_mean_LDA_topic_period.png")),
       device = ragg::agg_png,
<<<<<<< HEAD
       width = 40, height = 35, units = "cm", res = 300)

#' Calculating correlation
#' 

similarity_data <- list()
inflation_topics <- lda_data %>% 
  filter(inflation_topic == TRUE) %>% 
  distinct(topic) %>% 
  pull(topic)

for(i in 1:3){
data_filtered <- lda_data %>%
  filter(between(date, rects$start[i], rects$end[i])) %>% 
  select(document_id, topic, gamma) %>% 
  as.data.table()

matrix <- dcast(data_filtered, document_id ~ topic, value.var = "gamma") %>% 
  .[, -"document_id"]
  
jaccard <- vegan::vegdist(t(matrix), method = "jaccard")

jaccard_similarity <- data.table(as.matrix(jaccard))
jaccard_similarity[, topic1 := rownames(jaccard_similarity)]
jaccard_similarity <- melt(jaccard_similarity, 
                id.vars = "topic1", 
                variable.name = "topic2", 
                value.name = "jaccard_similarity") %>% 
  .[, `:=`(topic1_clustered = ifelse(topic1 %in% inflation_topics, 0, topic1),
           topic2_clustered = ifelse(topic2 %in% inflation_topics, 0, topic2))] %>% 
  .[! (topic1_clustered == topic2_clustered) & topic1_clustered == 0,] %>% 
  .[, similarity := mean(jaccard_similarity), by = .(topic1_clustered, topic2_clustered)] %>% 
  select(topic = topic2_clustered, similarity) %>% 
  mutate(similarity_measure = "jaccard") %>% 
  unique()

matrix <- as.matrix(matrix)
cosine <- lsa::cosine(matrix)
cosine_similarity <- data.table(cosine)
cosine_similarity[, topic1 := rownames(cosine_similarity)]
cosine_similarity <- melt(cosine_similarity, 
                     id.vars = "topic1", 
                     variable.name = "topic2", 
                     value.name = "cosine_similarity") %>% 
  .[, `:=`(topic1_clustered = ifelse(topic1 %in% inflation_topics, 0, topic1),
           topic2_clustered = ifelse(topic2 %in% inflation_topics, 0, topic2))] %>% 
  .[! (topic1_clustered == topic2_clustered) & topic1_clustered == 0,] %>% 
  .[, similarity := 1 - mean(cosine_similarity), by = .(topic1_clustered, topic2_clustered)] %>% 
  select(topic = topic2_clustered, similarity) %>% 
  mutate(similarity_measure = "cosine") %>% 
  unique()

jsd_similarity <- list()
for(j in 1:K){
  print(j)
  divergence <- map((1:K)[-j], ~ data_filtered %>% 
                      filter(topic %in% c(j,.x)) %>% 
                      dcast(document_id ~ topic, value.var = "gamma") %>% 
                      select(-document_id)  %>% 
                      relocate(contains(paste0(.x)), .after = contains(paste0(j))))
  divergence <- map(divergence, ~ data.table(topic1 = names(.)[1],
                                         topic2 = names(.)[2],
                                         similarity = dlookr::jsd(pull(., 1), pull(., 2), base = "log10", margin = TRUE)))
  jsd_similarity[[paste0(j)]] <- bind_rows(divergence) %>% 
    mutate(similarity = 1 - scales::rescale(similarity))
}

jsd_similarity <- bind_rows(jsd_similarity)
jsd_similarity <- data.table(jsd_similarity) %>% 
  .[, `:=`(topic1_clustered = ifelse(topic1 %in% inflation_topics, 0, topic1),
           topic2_clustered = ifelse(topic2 %in% inflation_topics, 0, topic2))] %>% 
  .[! (topic1_clustered == topic2_clustered) & topic1_clustered == 0,] %>% 
  .[, similarity := mean(similarity), by = .(topic1_clustered, topic2_clustered)] %>% 
  select(topic = topic2_clustered, similarity) %>% 
  mutate(similarity_measure = "jsd") %>% 
  unique()
  
  similarity_data[[paste0("Period_", i)]] <- rbind(jaccard_similarity, 
                                                   cosine_similarity,
                                                   jsd_similarity)
}

similarity_data_binded <- bind_rows(similarity_data, .id = "period") %>% 
  group_by(period, similarity_measure) %>% 
  mutate(similarity = ifelse(similarity_measure == "jsd", 1 - similarity, similarity)) %>%
  arrange(period, similarity_measure, desc(similarity)) %>% 
  mutate(rank = row_number()) 

#' Testing correlation between topics
#' 

correlation_list <- list()
for(i in 1:3){
  data_filtered <- lda_data %>%
    filter(between(date, rects$start[i], rects$end[i])) %>% 
    select(document_id, topic, gamma) %>% 
    as.data.table() %>% 
    .[, topic_clustered := ifelse(topic %in% inflation_topics$topic, 0, topic)] %>% 
    .[, gamma_clustered := mean(gamma), by = .(topic_clustered, document_id)] %>%
    arrange(topic_clustered) %>% 
    distinct(document_id, topic_clustered, gamma_clustered) %>% 
    pivot_wider(values_from = "gamma_clustered", names_from = "topic_clustered",
                names_glue = "gamma_{topic_clustered}") %>% 
    as.data.table()
  
  data_subset <- data_filtered %>% 
    select(-gamma_0, -document_id)
  
  cor_values <- cor(data_filtered$gamma_0, data_subset, method = "spearman") %>% 
    as_tibble() %>% 
    pivot_longer(cols = everything(), names_to = "topic", values_to = "similarity") %>% 
    mutate(topic = str_extract(topic, "\\d+"))
  
  correlation_list[[paste0("Period_", i)]] <- cor_values
}

correlation_list_binded <- bind_rows(correlation_list, .id = "period") %>% 
  mutate(similarity_measure = "pearson_correlation") %>% 
  arrange(period, desc(similarity)) %>% 
  group_by(period) %>% 
  mutate(rank = row_number())

similarity_data <- similarity_data_binded %>% 
  ungroup() %>% 
  bind_rows(correlation_list_binded) %>% 
  mutate(topic = as.integer(topic)) %>% 
  left_join(distinct(lda_data, topic, topic_name))

saveRDS(similarity_data, here(data_path, 
                                     "topic_modelling",
                                     "similarities_LDA.rds"))
=======
       width = 50, height = 45, units = "cm", res = 300)

#data %>% 
#  pivot_wider(names_from = "period", values_from = c("mean", "st_err"),) %>% 
#  mutate(differential = mean_Period_2 - mean_Period_1,
 #        st_err = (st_err_Period_1 + st_err_Period_2)/2,
  #       topic_name = paste0("topic ", topic, ": ", str_wrap(topic_name, 50))) %>% 
  #ggplot(aes(differential, fct_reorder(topic_name, differential))) +
  #geom_errorbar(aes(xmin = differential - st_err, xmax = differential + st_err)) +
  #geom_point(aes(size = mean_Period_1 + mean_Period_2), show.legend = FALSE) +
  #geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6)

#ggsave(here("pictures", glue::glue("TM_differential_LDA_topic_period.png")),
 #      device = ragg::agg_png,
  #     width = 50, height = 45, units = "cm", res = 300)

#data_ecb <- data_year_subset %>% 
 # filter(central_bank %in% cb_focus) %>% 
  #group_by(topic, period, central_bank) %>% 
  #mutate(mean = mean(gamma),
  #       st_err = sd(gamma)/sqrt(length(gamma))) %>% 
  #select(topic, topic_name, period, central_bank, mean, st_err) %>% 
  #unique()

data_ecb %>% 
  pivot_wider(names_from = "period", values_from = c("mean", "st_err"),) %>% 
  mutate(differential = mean_Period_2 - mean_Period_1,
         st_err = (st_err_Period_1 + st_err_Period_2)/2,
         topic_name = paste0("topic ", topic, ": ", str_wrap(topic_name, 50))) %>% 
  ggplot(aes(differential, fct_reorder(topic_name, differential), color = central_bank)) +
  geom_errorbar(aes(xmin = differential - st_err, xmax = differential + st_err)) +
  geom_point(aes(size = mean_Period_1 + mean_Period_2), show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6)

ggsave(here("pictures", glue::glue("TM_differential_LDA_topic_period_cb.png")),
       device = ragg::agg_png,
       width = 50, height = 45, units = "cm", res = 300)

data_ecb %>% 
  pivot_wider(names_from = "period", values_from = c("mean", "st_err"),) %>% 
  mutate(differential = mean_Period_2 - mean_Period_1,
         st_err = (st_err_Period_1 + st_err_Period_2)/2,
         topic_name = paste0("topic ", topic, ": ", str_wrap(topic_name, 50))) %>% 
  ggplot(aes(differential, fct_reorder(topic_name, differential), color = central_bank)) +
  geom_errorbar(aes(xmin = differential - st_err, xmax = differential + st_err)) +
  geom_point(aes(size = mean_Period_1 + mean_Period_2), show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  facet_wrap(~central_bank, ncol = 3) +
  theme(legend.position = "bottom")
  

ggsave(here("pictures", glue::glue("TM_differential_LDA_topic_period_per_cb.png")),
       device = ragg::agg_png,
       width = 60, height = 45, units = "cm", res = 300)

>>>>>>> 94e4769367dc1858bff47b054eabd826cd2681e7
