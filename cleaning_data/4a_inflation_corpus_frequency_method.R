source("packages_and_data.R")
source("function/functions_for_topic_modelling.R")
eurosystem_text <- readRDS(here(data_path, "eurosystem_text_cleaned.rds"))
eurosystem_metadata <- readRDS(here(data_path, "eurosystem_metadata.rds")) %>% 
  filter(file_name %in% eurosystem_text$file) %>% 
  mutate(year = year(date))


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
  .[, sum_inflation_words := sum(inflation_words), by = "file"] %>% 
  select(file, sum_inflation_words) %>% 
  unique()

stats_inflation_texts <- eurosystem_metadata %>% 
  select(file = file_name, year) %>% 
  inner_join(inflation_text) %>% 
  mutate(small_threshold = sum_inflation_words >= 5,
         medium_threshold = sum_inflation_words >= 10,
         big_threshold = sum_inflation_words >= 15) %>% 
  add_count(year, name = "number_files") %>% 
  group_by(year) %>% 
  mutate(across(ends_with("threshold"), ~sum(.))) %>% 
  select(year, number_files, contains("threshold")) %>% 
  unique() %>% 
  pivot_longer(cols = c(number_files, contains("threshold")), names_to = "type_of_speech", values_to = "count")

stats_inflation_texts %>% 
  ggplot(aes(x = year, y = count, fill = type_of_speech)) +
  geom_col(position = "identity")

ggsave(here("pictures", glue::glue("threshold_corpus_absolute.png")),
       device = ragg::agg_png,
       width = 50, height = 40, units = "cm", res = 300)

stats_inflation_texts %>% 
  filter(type_of_speech != "number_files") %>% 
  inner_join(select(stats_inflation_texts, year, type_of_speech, number_speech = count) %>% 
               filter(type_of_speech == "number_files") %>% 
               select(-type_of_speech) %>% 
               unique()) %>% 
  mutate(share = count/number_speech * 100) %>% 
  ggplot(aes(x = year, y = share, fill = type_of_speech)) +
  geom_col(position = "dodge")

ggsave(here("pictures", glue::glue("threshold_corpus_share.png")),
       device = ragg::agg_png,
       width = 50, height = 40, units = "cm", res = 300)

inflation_text <- eurosystem_text %>% 
  filter(file %in% filter(inflation_text, sum_inflation_words >= 15)$file)

saveRDS(inflation_text, here(data_path, "eurosystem_text_inflation.rds"))

eurosystem_metadata %>% 
  filter(file_name %in% unique(inflation_text$file)) %>% 
saveRDS(here(data_path, "eurosystem_metadata_inflation.rds"))
