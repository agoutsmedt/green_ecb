source("packages_and_data.R")
source("function/functions_for_topic_modelling.R")
library(rollinglda)
library(GGally)
eurosystem_metadata <- readRDS(here(data_path, "eurosystem_metadata_inflation.rds")) %>% 
  arrange(file_name)
#eurosystem_text <- readRDS(here(data_path, "eurosystem_text_inflation.rds")) %>% 
 # arrange(document_id)
term_list <- readRDS(here(data_path, "topic_modelling","TM_term_list_inflation.rds")) %>% 
  left_join(select(eurosystem_metadata, date, file = file_name)) %>% 
  arrange(date, document_id)
  
# for RollingLDA, we need
nb_docs <- length(unique(term_list$document_id))
term_list_filtered <- term_list %>%
  .[, total_freq := .N, by = "term"] %>% 
  unique() %>% 
  .[, share_of_doc := .N/nb_docs, by = "term"] %>% 
  filter(share_of_doc < 0.50,
         total_freq >= 20) 

texts <- term_list_filtered %>% 
  unstack(term  ~ document_id)

dates <- term_list_filtered %>% 
  distinct(document_id, date) %>% 
  pull(date)

list_roll <- list()
for(k in seq(30, 90, 20)){
roll <- RollingLDA(texts = texts,
                   dates = dates,
                   chunks = "quarter",
                   memory = "year",
                   init = "2007-01-01",
                   K = k,
                   seeds = 1989)

saveRDS(roll, here(data_path,"rollingLDA", paste0("roll_", k, ".rds")))
list_roll[[paste(k)]] <- roll
}

saveRDS(list_roll, here(data_path,"rollingLDA", "roll_full.rds"))



  
  