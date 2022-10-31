source("packages_and_data.R")

########## Loading BIS data ###################

bis_data_path <- here(path.expand("~"),
                      "data",
                      "scrap_bis")

data_files <- list.files(bis_data_path)
bis_metadata <- readRDS(here(bis_data_path, 
                             data_files[str_which(data_files, "metadata_cleaned")]))
bis_text <- readRDS(here(bis_data_path, 
                         data_files[str_which(data_files, "text")])) %>% 
  mutate(file = str_remove(file, "\\.pdf$"))


########### Add variables on bis metadata ########################

eurosystem <- c("european central bank",
                "bundesbank",
                "austrian national bank",
                "bank of belgium",
                "central bank of cyprus",
                "bank of spain",
                "bank of estonia",
                "bank of finland",
                "bank of france",
                "bank of greece",
                "central bank of ireland",
                "bank of italy",
                "bank of latvia",
                "bank of lithuania",
                "central bank of luxembourg",
                "central bank of malta",
                "netherlands bank",
                "bank of portugal",
                "bank of slovakia",
                "bank of slovenia")

eurosystem_metadata <- bis_metadata %>% 
  mutate(year = str_extract(date, "\\d{4}$")) %>% 
  filter(central_bank %in% eurosystem,
         year >= 2000) 

eurosystem_text <- bis_text %>% 
  filter(file %in% eurosystem_metadata$file_name)

########## cleaning text ##################

to_remove_strictly <- c("^[\\d \\.]+$",
               "^Page \\d+ sur \\d+$",
               "^Figure \\d+$") %>% 
  tolower()

to_remove <- c("BIS Review",
               "BIS central bankers(’|')? speeches( \\d+)?$",
               "^Source(s)?: ",
               "^Chart \\d{1,2}",
               "^Figure \\d{1,2}",
               "(FOCUS|Question): ") %>% 
  tolower()


# Removing the presentation page of speeches
eurosystem_text_paragraph <- eurosystem_text %>% 
  unnest_tokens("paragraphs", text, "paragraphs") %>% 
  mutate(paragraphs = str_squish(paragraphs),
         row = 1:n()) %>% 
  mutate(stars = str_detect(paragraphs, "\\* \\* \\*") & page == 1,
         characters = str_count(paragraphs),
         words = str_count(paragraphs, "[[:alpha:]]+"), 
         paragraphs = str_remove_all(paragraphs, paste0(to_remove, collapse = "|"))) %>% 
  filter(! paragraphs == "",
         ! str_detect(paragraphs, paste0(to_remove_strictly, collapse = "|")))

first_line <- eurosystem_text_paragraph %>% 
  filter(stars == TRUE) %>% 
  select(file, row) %>% 
  rename(first_line = row)

eurosystem_text_paragraph_cleaned <- eurosystem_text_paragraph %>% 
  left_join(first_line) %>% 
  mutate(paragraphs = str_trim(paragraphs, "both")) %>% 
  filter(row >= first_line | is.na(first_line),
         words > 3 | (between(words, 1, 3) & str_detect(paragraphs, "^[[:alpha:]]+( [[:alpha:]]+( [[:alpha:]]+)?)?(\\.)?$")),
         characters > 11,
         ! (words == 2 & characters < 8),
         ! (words == 1 & between(characters, 6, 18))) %>% 
  select(-c(row, stars, first_line)) %>% 
  group_by(file, page) %>% 
  mutate(paragraph_number = 1:n())

eurosystem_text_cleaned <- eurosystem_text_paragraph_cleaned %>% 
  mutate(problems = str_detect(paragraphs, "")) # problem of ocrisation

# save the list of speeches with problem of ocrisation
eurosystem_text_cleaned %>% 
  filter(problems == TRUE) %>% 
  ungroup() %>% 
  select(file) %>% 
  unique %>% 
  write_csv2(paste0(bis_data_path, "/speech_in_need_of_OCR.csv"))

#' The strategy here is:
#' - to find the small paragraph that are alone in a page.
#' - to put them in the next page, as a first paragraph if they were in the first page
#' - if they were not in the first page, to put them on the former page, and to give them 
#' the number of the maximum paragraph by grouping per document and page
#' - to collapse the new paragraphs
#' - to recalculate the number of words
#' - to put the small paragraph with the next one if they are the first, with the former one if they are not
changing_page <- function(data){
  data <- data %>% 
    group_by(file) %>% 
    add_count(page, name = "n_paragraphs") %>% 
    ungroup() %>% 
    mutate(words = str_count(paragraphs, "[[:alpha:]]+"),
           alone_small_paragraph = (n_paragraphs == 1 & words < 50),
           original_page = page,
           page = ifelse(alone_small_paragraph == TRUE & page > 1 & file == lag(file),
                         page - 1,
                         page),
           page = ifelse(alone_small_paragraph == TRUE & page == 1 & file == lead(file), 
                         lead(page),
                         page)) %>% 
    group_by(file, page) %>% 
    mutate(highest_paragraph = max(paragraph_number),
           paragraph_number = ifelse(alone_small_paragraph == TRUE & original_page > page, highest_paragraph, paragraph_number))  %>% 
    group_by(file, page, paragraph_number) %>% 
    mutate(paragraphs = paste0(paragraphs, collapse = " ")) %>% 
    select(file, page, paragraph_number, paragraphs, alone_small_paragraph) %>% 
    unique()
}

final_text <- eurosystem_text_cleaned %>% 
  filter(problems == FALSE,
         words > 0) %>% 
  select(file, page, paragraph_number, paragraphs) %>%
  changing_page() %>% 
  changing_page() %>% 
  filter(alone_small_paragraph == FALSE) %>% 
  select(-alone_small_paragraph)

merging_paragraph <- function(data){
  data <- data %>% 
    group_by(file, page) %>% 
    mutate(paragraph_number = 1:n()) %>% 
    ungroup() %>% 
    mutate(words = str_count(paragraphs, "[[:alpha:]]+"),
           paragraph_number = ifelse(words < 50 & paragraph_number > 1 & page == lag(page) & file == lag(file), 
                                     lag(paragraph_number), 
                                     paragraph_number),
           paragraph_number = ifelse(words < 50 & paragraph_number == 1 & page == lead(page) & file == lead(file), 
                                     lead(paragraph_number), 
                                     paragraph_number)) %>% 
    group_by(file, page, paragraph_number) %>% 
    mutate(paragraphs = paste0(paragraphs, collapse = " ")) %>% 
    ungroup() %>% 
    select(file, page, paragraphs) %>% 
    unique()
}

final_text_cleaned <- merging_paragraph(final_text) %>% # repeting the process until we have no more paragraphs below 50 words
  merging_paragraph() %>% 
  merging_paragraph() %>% 
  merging_paragraph() %>% 
  merging_paragraph() %>% 
  merging_paragraph() %>% 
  merging_paragraph() %>% 
  merging_paragraph() %>% 
  group_by(file, page) %>% 
  mutate(paragraph_number = 1:n()) %>% 
  ungroup()

final_text_cleaned <- final_text_cleaned %>% 
  mutate(document_name = paste0(file, "_page", page),
         document_id = paste0(document_name, "_paragraph", paragraph_number)) %>% 
  arrange(document_id)

eurosystem_metadata <- eurosystem_metadata %>% 
  arrange(file_name)

saveRDS(eurosystem_metadata, here(data_path, "eurosystem_metadata.rds"))
saveRDS(final_text_cleaned, here(data_path, "eurosystem_text.rds"))
