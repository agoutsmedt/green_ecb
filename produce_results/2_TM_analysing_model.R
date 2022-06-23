source("packages_and_data.R")
source("function/functions_for_topic_modelling.R")
eurosystem_metadata <- readRDS(here(data_path, "eurosystem_metadata.rds"))
eurosystem_text <- readRDS(here(data_path, "eurosystem_text.rds"))
topic_model <- readRDS(here(data_path,"topic_modelling", "TM_50.rds"))
chosen_topic_model <- readRDS(here(data_path, "topic_modelling", "TM_data_set.rds")) %>% 
  filter(preprocessing_id == 2)
nb_topics <- topic_model$settings$dim$K

stm_data <- chosen_topic_model$stm[[1]]
doc_pages <- chosen_topic_model$data[[1]] %>% 
  select(document, page, file) %>% 
  unique

metadata <- data.table("document" = names(stm_data$documents)) %>% 
  left_join(doc_pages) %>% 
  left_join(rename(eurosystem_metadata, "file" = file_name)) %>% 
  mutate(date = dmy(date),
         quarter = quarter(date),
         semester = semester(date))

metadata_with_text <- metadata %>% 
  left_join(eurosystem_text)


#' We can use the stm package function to plot some descriptive visualisations.
#' For certain visualisations, we can extract the data to use ggplot/ggraph.
#' 
#' We can also extract the top terms for different measure (not just for beta).
#' This data.frame can also be used to give name to the topics. We will use it
#' for the nodes of the topic correlation network.

top_terms <- extract_top_terms(topic_model,
                               chosen_topic_model$data[[1]],
                               nb_terms = 30,
                               frexweight = 0.5)

#' We will use this table for exploration
#' `saveRDS(top_terms, here(data_path, "topic_modelling", "TM_top_terms.rds"))`

#' This function will take the four words with the highest frex value to name the 
#' topic.
topics <- name_topics(top_terms, method = "beta", nb_word = 5)

#' ## Correlation network 
#' 
#' We now plot the topic correlation network. The use is double: to see which topics are 
#' the closest, and to use this closeness to build a new order of topics. What we want
#' is to plot summary statistics of topics with more correlated topics being next
#' to each other
set.seed(1989)
topic_corr_network <- ggraph_topic_correlation(topic_model, 
                                               nodes = topics,
                                               method = "huge", 
                                               size_label = 2,
                                               nb_topics = nb_topics,
                                               resolution = 1.4) 



#' We observe the results and gives name to the identified communities:
communities <- topic_corr_network$graph %>% 
  activate(nodes) %>% 
  as.data.table %>% 
  select(topic, Com_ID)
topics <- merge(topics, communities, by = "topic") %>% 
  arrange(Com_ID)

#' We can look at the composition of the different community: `View(topics)`

community_name <- tribble(
  ~Com_ID, ~Com_name,
  "04", "Prudential Policy",
  "02", "ECB role, research & European Integration",
  "03", "Price Stability & Growth",
  "07", "Financial Supervision",
  "05", "Fiscal Policy & Structural Reforms",
  "06", "Varia",
  "08", "Credition conditions & Asset purchase"
)

#' #### Plotting network with communities

community_name$com_color <- c(scico(n = nrow(community_name), palette = "hawaii"))
topics_with_com <- merge(topics, community_name, by = "Com_ID")
network <- topic_corr_network$graph 
network <- network %>% 
  activate(nodes) %>% 
  left_join(unique(topics_with_com[, c("Com_ID", "Com_name", "com_color")]))
network <- network %>% # mix color
  activate(edges) %>%
  mutate(color_com_ID_to = .N()$com_color[to], color_com_ID_from = .N()$com_color[from]) %>%
  mutate(color_edges = DescTools::MixColor(color_com_ID_to, color_com_ID_from, amount1 = 0.5))

graph_plot <- ggraph(network, layout = "manual", x = x, y = y) +
  geom_edge_arc0(aes(color = color_edges, width = weight), strength = 0.3, alpha = 0.6, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.5,12)) +
  scale_edge_colour_identity() +
  scale_fill_identity() +
  geom_node_label(aes(label = topic_name, fill = com_color), size = 2.5, alpha = 0.7) +
  theme_void()

ragg::agg_png(here("pictures", "TM_topic_correlation.png"), 
              width = 40, 
              height = 30, 
              units = "cm", 
              res = 300)
graph_plot
invisible(dev.off())

#' ## Describing topics top terms and frequency
#' 
#' We use the order resulting of correlation to distribute colors for each topic. More generally,
#' we have the choice between giving a unique color per topic (depending of the correlation network),
#' or giving to topics the color of their community (identified in the correlation network with 
#' Leiden algorithm).

topics_with_com <- topics_with_com %>% 
  arrange(Com_ID, id) %>% 
  mutate(new_id = 1:n(),
         color = c(scico(n = (nb_topics)/2 - 1, begin = 0, end = 0.35, palette = "roma"),
                   scico(n = (nb_topics)/2 + 1, begin = 0.55, palette = "roma")))

#' We plot the terms with the highest FREX value for each topic:

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

ragg::agg_png(here("pictures", "TM_top_terms.png"),
              width = 50, height = 40, units = "cm", res = 300)
top_terms_graph
invisible(dev.off())

#' We now plot the frequency of each topics:
#' 

plotting_frequency <- plot_frequency(topics_with_com, topic_model, com_color)

ragg::agg_png(here("pictures", "TM_topic_prevalence.png"), 
              width = 40, 
              height = 40, 
              units = "cm", 
              res = 300)
plotting_frequency
invisible(dev.off())

#' We add the frequency value of each topic:
#' 

topics_complete <- topics_with_com %>% 
  arrange(id) %>% 
  mutate(topic_prevalence = colMeans(topic_model$theta))
saveRDS(topics_complete, here(data_path,
                              "topic_modelling",
                              "topics_complete.RDS"))

#' 
#' We also want to save some examples of the topics, by keeping the more representative abstracts.
#' Some topics are most represented just by title

topic_examples <- topics_complete %>% 
  select(id) %>% 
  mutate(examples = map(id, ~findThoughts(topic_model, 
                                          texts = paste0(metadata_with_text$title, " (page ",
                                                         metadata_with_text$page, ") - ",
                                                         metadata_with_text$central_bank, " (",
                                                         metadata_with_text$pdf_link, "):",
                                                         metadata_with_text$text),
                                          n = 4, 
                                          topics = .x)$docs[[1]])) %>% 
  unnest(examples)

saveRDS(topic_examples, here(data_path,
                             "topic_modelling",
                             "TM_Topic_examples.rds"))

#' # Topics according to our variables of interest
#' 
#' ## Extracting the data from the topic model
#' 
#' We extract the gamma table: the table that associates each document to each topic with
#' a certain gamma value (a kind of rate of belonging).

topic_gamma <- tidy(topic_model, matrix = "gamma") 
topic_gamma <- topic_gamma %>% 
  left_join(select(topics_complete, id, topic_name), by = c("topic" = "id")) %>% 
  select(-topic) %>% 
  mutate(topic_name = str_remove_all(str_replace(topic_name, "\\\n", " "), " \\/"))


topic_gamma_widered <- pivot_wider(topic_gamma,
                                   names_from = topic_name, 
                                   values_from = gamma) %>% 
  mutate(document = names(stm_data$documents)) %>% 
  inner_join(metadata) %>%
  select(document, page, title, speaker_cleaned, central_bank, year, quarter, semester, pdf_link, contains("Topic")) %>% 
  as.data.table()

topic_gamma_attributes <- pivot_longer(topic_gamma_widered, 
                                       cols = contains("Topic"),
                                       names_to = "topic_name",
                                       values_to = "gamma") %>% 
  as.data.table() %>% 
  mutate(topic = str_extract(topic_name, "Topic \\d+")) %>% 
  left_join(select(topics_complete, topic, new_id)) # we add the id depending of the correlation and the prevalence

#' We will use this table for exploration
#' `saveRDS(topic_gamma_attributes, here(data_path, "topic_modelling", "TM_gamma_values.rds"))`
#' `topic_gamma_attributes <- readRDS(here(data_path, "topic_modelling", "TM_gamma_values.rds"))`

#' ## Differences in terms of journals and affiliations
#' 
#' We will use covariates below to do that, but to have results easier to interpret, we also
#' observe the journal and affiliation effect on topics just by looking at the difference 
#' in the mean of each variable for each topic.
#' 

nb_speech <- metadata %>% 
  select(central_bank, file) %>% 
  unique %>% 
  count(central_bank)

topic_diff <- copy(topic_gamma_attributes) %>% 
  mutate(central_bank_filtered = ifelse(central_bank %in% filter(nb_speech, n >= 200)$central_bank, 
                                        central_bank,
                                        "other central banks"))


topic_diff <- topic_diff[, mean_cb := mean(gamma), by = c("topic_name", "central_bank_filtered")] %>% 
  .[, mean_year := mean(gamma), by = c("topic_name", "year")] %>% 
  .[, mean_semester := mean(gamma), by = c("topic_name", "semester")] %>% 
  .[, mean_quarter := mean(gamma), by = c("topic_name", "quarter")] %>% 
  .[, mean_year_cb := mean(gamma), by = c("topic_name", "central_bank_filtered", "year")] %>% 
  .[, mean_semester_cb := mean(gamma), by = c("topic_name", "central_bank_filtered", "semester")] %>% 
  .[, mean_quarter_cb := mean(gamma), by = c("topic_name", "central_bank_filtered", "quarter")] %>% 
  select(central_bank_filtered, year, semester, quarter, topic_name, topic, new_id, contains("mean")) %>% 
  unique %>% 
  left_join(select(topics_complete, new_id, Com_name, com_color, topic_prevalence)) %>% 
  mutate(order = str_extract(topic, "\\d+") %>% as.integer)

#' We will use this table for exploration
#' `saveRDS(topic_diff, here(data_path, "topic_modelling", "TM_topics_diff.rds"))`  
#' 

#' # Producing graphs
#' 
topic_of_interest <- c(17,
                    #   13, 
                    #   6, 
                       31, 
                       33)
topic_diff_filtered <- topic_diff %>% 
  filter(topic %in% paste0("Topic ", topic_of_interest)) %>% 
  mutate(month = ifelse(str_extract(semester, "\\d$") == "1", 1, 6),
         year = str_extract(semester, "^\\d+"),
         date = paste0(year, "-", month) %>% ym())

topic_year <- topic_diff_filtered %>% 
  select(topic_name, topic, order, date, mean_semester) %>% 
  unique

plot_topic_year <- ggplot(topic_year, aes(date, mean_semester, color = fct_reorder(topic_name, order))) + 
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE, span = 0.4, size = 2, alpha = 0.9) +
  scale_color_scico_d(palette = "roma",
                      name = NULL) +
  theme_bw(base_size = 14) + 
  theme(legend.position = "bottom") +
  labs(title = "Topic Prevalence",
       x = NULL,
       y = NULL) +
  guides(color = guide_legend (nrow=2, byrow=TRUE))

ragg::agg_png(here("pictures", "topic_prevalence_over_time.png"), 
              width = 35, 
              height = 30, 
              units = "cm", 
              res = 300)
plot_topic_year
invisible(dev.off())

topic_year_cb <- topic_diff_filtered %>% 
  filter(! central_bank_filtered %in% c("other central banks"),
         ! str_detect(central_bank_filtered, "ireland")) %>% 
  select(topic_name, topic, order, date, central_bank_filtered, mean_semester_cb) %>% 
  unique

plot_topic_year_cb <- ggplot(topic_year_cb, aes(date, mean_semester_cb, color = fct_reorder(topic_name, order))) + 
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE, span = 0.4, size = 2, alpha = 0.9) +
  facet_wrap(~central_bank_filtered, scales = "free") +
  scale_color_scico_d(palette = "roma",
                      name = NULL) +
  theme_bw(base_size = 14) + 
  theme(legend.position = "bottom") +
  labs(title = "Topic Prevalence per Central Banks",
       x = NULL,
       y = NULL) +
  guides(color = guide_legend (nrow=2))

ragg::agg_png(here("pictures", "topic_prevalence_per_cb.png"), 
              width = 35, 
              height = 30, 
              units = "cm", 
              res = 300)
plot_topic_year_cb
invisible(dev.off())

cb_year_topic <- topic_diff_filtered %>% 
  filter(! central_bank_filtered %in% c("other central banks"),
         ! str_detect(central_bank_filtered, "ireland")) %>% 
  select(topic_name, topic, date, central_bank_filtered, mean_semester_cb) %>% 
  unique

plot_cb_year_topic <- ggplot(cb_year_topic, aes(date, mean_semester_cb, color = central_bank_filtered)) + 
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE, span = 0.5, size = 2, alpha = 0.9) +
  scale_color_scico_d(palette = "hawaii",
                      name = NULL) +
  facet_wrap(~ topic_name, scales = "free") + 
  theme_bw(base_size = 13) + 
  theme(legend.position = "bottom") +
  labs(title = "Central Banks per Topics",
       x = NULL,
       y = NULL)

ragg::agg_png(here("pictures", "CB_per_topics.png"), 
              width = 35, 
              height = 30, 
              units = "cm", 
              res = 300)
plot_cb_year_topic
invisible(dev.off())
  
