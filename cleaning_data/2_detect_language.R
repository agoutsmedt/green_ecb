source("packages_and_data.R")
library(fastText)
eurosystem_text <- readRDS(here(data_path, "eurosystem_text.rds"))

eurosystem_text_with_language <- eurosystem_text %>% 
  mutate(language_cld3 = detect_language(paragraphs))

file_ftz = system.file("language_identification/lid.176.ftz", package = "fastText")


#' We are using the algorithm from fastText. We have a weird problem: the line 334 generates
#' a stop, and no language is detected after this line. Thus, we detect the language for this line
#' separetely.
#' 

corpus_reduced <- eurosystem_text[-334,]
language_data <- tribble(
  ~iso_lang_1, ~prob_1, ~iso_lang_2, ~prob_2
)
for(i in seq(1, 59800, 100)){
dtbl_res_in <- language_identification(input_obj = corpus_reduced$paragraphs[i:(i+99)],
                                                pre_trained_language_model_path = file_ftz,
                                                k = 2,
                                                th = 0.0,
                                                threads = 1,
                                                verbose = TRUE)

message(i)
if(nrow(dtbl_res_in) < 100){
  break() # to spot problems
}
  
language_data <- language_data %>% 
  bind_rows(dtbl_res_in) 
}

dtbl_res_in_supp <- language_identification(input_obj = eurosystem_text$paragraphs[334],
                                                 pre_trained_language_model_path = file_ftz,
                                                 k = 2,
                                                 th = 0.0,
                                                 threads = 1,
                                                 verbose = TRUE)

language_data <- language_data[1:nrow(test),]
eurosystem_text_with_language <- eurosystem_text[-334,] %>% 
  mutate(language_1 = language_data$iso_lang_1,
         prob_language_1 = language_data$prob_1,
         language_2 = language_data$iso_lang_2,
         prob_language_2 = language_data$prob_2)

additional_line <- eurosystem_text[334,] %>% 
  mutate(language_1 = dtbl_res_in_supp$iso_lang_1,
         prob_language_1 = dtbl_res_in_supp$prob_1,
         language_2 = dtbl_res_in_supp$iso_lang_2,
         prob_language_2 = dtbl_res_in_supp$prob_2)

eurosystem_text_with_language <- eurosystem_text_with_language %>% 
  bind_rows(additional_line) %>% 
  arrange(document_id)

saveRDS(eurosystem_text_with_language, here(data_path,
                                            "eurosystem_text_with_language.rds"))
