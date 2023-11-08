source("packages_and_data.R")
source("function/functions_for_topic_modelling.R")
eurosystem_text <- readRDS(here(data_path, "eurosystem_text_cleaned.rds"))
eurosystem_metadata <- readRDS(here(data_path, "eurosystem_metadata.rds")) %>% 
  filter(file_name %in% eurosystem_text$file,
         central_bank == "european central bank") %>% 
  mutate(year = year(date))
eurosystem_text <- eurosystem_text %>% 
  filter(file %in% eurosystem_metadata$file_name)

#' We can imagine two types of methods to identify speeches about inflation in Eurosystem
#' CBs' speeches. Two methods are possible:
#' - a simple, fast and arbitrary one: keeping speeches with a certain frequency of
#' "inflation" and "price stability". 
#' - a more complex method: running topic modelling, then keeping only the speech with
#' a certain (arbitrary) value of gamma parameters for the topic of interests.
#' 
#' We can test the two methods in order of comparison. In this script, we will implement
#' the first method and calculate the share of corresponding speeches for each year.
#' 

inflation_voc <- c("inflation")

inflation_text <- eurosystem_text %>% 
  as.data.table %>% 
  .[, inflation_words := str_count(paragraphs, paste0(inflation_voc, collapse = "|"))] %>% 
  .[, sum_inflation_words := sum(inflation_words)/max(page), by = "file"] %>% 
  select(file, sum_inflation_words) %>% 
  unique()

stats_inflation_texts <- eurosystem_metadata %>% 
  select(file = file_name, year) %>% 
  inner_join(inflation_text) %>% 
  mutate(small_threshold = sum_inflation_words >= 1,
         medium_threshold = sum_inflation_words >= 1.5,
         big_threshold = sum_inflation_words >= 2) %>% 
  add_count(year, name = "number_files") %>% 
  group_by(year) %>% 
  mutate(across(ends_with("threshold"), ~sum(.))) %>% 
  select(year, number_files, contains("threshold")) %>% 
  unique() %>% 
  pivot_longer(cols = c(number_files, contains("threshold")), names_to = "type_of_speech", values_to = "count")

stats_inflation_texts$type_of_speech %>% unique()
stats_inflation_texts %>% 
  ggplot(aes(x = year, y = count, fill = fct_inorder(type_of_speech))) +
  geom_col(position = "identity", alpha = 0.9) +
  scico::scale_fill_scico_d(palette = "roma", 
                             labels = c("number of speeches", "> to 1", "> to 1.5", "> to 2")) +
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom") +
  labs(fill = NULL,
       y = NULL,
       x = NULL)

ggsave(here("pictures", glue::glue("threshold_corpus_absolute.png")),
       device = ragg::agg_png,
       width = 24, height = 20, units = "cm", res = 300)

stats_inflation_texts %>% 
  filter(type_of_speech != "number_files",
         year < 2023) %>% 
  inner_join(select(stats_inflation_texts, year, type_of_speech, number_speech = count) %>% 
               filter(type_of_speech == "number_files") %>% 
               select(-type_of_speech) %>% 
               unique()) %>% 
  mutate(share = count/number_speech * 100) %>% 
  ggplot(aes(x = year, y = share, fill = type_of_speech)) +
  geom_col(position = "dodge") +
  scale_fill_discrete(labels = c("> to 2", "> to 1.5", "> to 1"))


ggsave(here("pictures", glue::glue("threshold_corpus_share.png")),
       device = ragg::agg_png,
       width = 50, height = 40, units = "cm", res = 300)

inflation_text <- eurosystem_text %>% 
  filter(file %in% filter(inflation_text, sum_inflation_words >= 2)$file)

saveRDS(inflation_text, here(data_path, "eurosystem_text_inflation.rds"))

eurosystem_metadata %>% 
  filter(file_name %in% unique(inflation_text$file)) %>% 
saveRDS(here(data_path, "eurosystem_metadata_inflation.rds"))
