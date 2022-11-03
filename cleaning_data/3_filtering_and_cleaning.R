#' In this script, we do a bit of cleaning of the corpus, by removing what is not
#' in English, as well as removing some messy paragraphs, or paragraphs which are actually bibliography

source("packages_and_data.R")
eurosystem_text <- readRDS(here(data_path, "eurosystem_text_with_language.rds")) %>% 
  filter(language_1 == "en" & prob_language_1 > 0.60)

eurosystem_text_cleaned <- eurosystem_text %>% 
  mutate(bibliography = ifelse(str_detect(paragraphs, "^references|^bibliography"), TRUE, FALSE)) %>% 
  mutate(bibliography = ifelse(bibliography == TRUE & file == lead(file), TRUE, bibliography)) %>% # in case the biblio is on several paragraphs or pages (but not the case here)
  filter(bibliography == FALSE) %>% 
  distinct(file, paragraphs, .keep_all = TRUE)


#' Later on, we can imagine having a more fine-grained cleaning of the bibliographies, by
#' spotting patterns of reference.
#' 

saveRDS(eurosystem_text_cleaned, here(data_path,
                                            "eurosystem_text_cleaned.rds"))
