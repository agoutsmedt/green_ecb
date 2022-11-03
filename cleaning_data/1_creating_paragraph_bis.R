source("packages_and_data.R")

########## Loading BIS data ###################

bis_data_path <- here(path.expand("~"),
                      "data",
                      "scrap_bis")

data_files <- list.files(bis_data_path)
bis_metadata <- readRDS(here(bis_data_path, 
                             data_files[str_which(data_files, "metadata_cleaned_updated")]))
bis_text <- readRDS(here(bis_data_path, 
                         data_files[str_which(data_files, "text_updated")])) %>% 
  mutate(file = str_remove(file, "\\.pdf$"))

# Complete by running OCR on problematic pdfs
problem_ocr <- read_csv2(here(bis_data_path,
                              "speech_in_need_of_OCR_corrected.csv"))

pdf_files <- paste0(problem_ocr$file, ".pdf")
path_pdf <- here(path.expand("~"),
                 "data",
                 "scrap_bis", 
                 "pdf")

new_text_ocr <- tribble(
  ~text, ~page, ~file
)
for(i in pdf_files){
text <- tesseract::ocr(here(path_pdf, i), engine = tesseract("eng"))
text <- tibble(text) %>% 
  mutate(text = str_remove(text, ".+(=?\\\n)"),
         page = 1:n(),
         file = str_remove(i, "\\.pdf"))
new_text_ocr <- new_text_ocr %>% 
  bind_rows(text)
}

bis_text_updated <- bis_text %>% 
  filter(! file %in% problem_ocr$file) %>% 
  bind_rows(new_text_ocr)

########### Add variables on bis metadata ########################

eurosystem <- tribble(
  ~ central_bank, ~ entry_date,
  "european central bank", "1997-01-06",
  "bundesbank", "1997-01-06",
  "austrian national bank", "1997-01-06",
  "bank of belgium", "1997-01-06",
  "central bank of cyprus", "2008-01-01",
  "bank of spain", "1997-01-06",
  "bank of estonia", "2011-01-01",
  "bank of finland", "1997-01-06",
  "bank of france", "1997-01-06",
  "bank of greece", "2001-01-01",
  "central bank of ireland", "1997-01-06",
  "bank of italy", "1997-01-06",
  "bank of latvia", "2014-01-01",
  "bank of lithuania", "2015-01-01",
  "central bank of luxembourg", "1997-01-06",
  "central bank of malta", "2008-01-01",
  "netherlands bank", "1997-01-06",
  "bank of portugal", "1997-01-06",
  "bank of slovakia", "2009-01-01",
  "bank of slovenia", "2007-01-01") %>% 
  mutate(entry_date = date(entry_date))

eurosystem_metadata <- bis_metadata %>% 
  mutate(year = str_extract(date, "\\d{4}$"),
         eurozone = FALSE)

for(j in 1:nrow(eurosystem)){
  eurosystem_metadata <- eurosystem_metadata %>% 
    mutate(eurozone = ifelse(central_bank == eurosystem$central_bank[j] & date > eurosystem$entry_date[j],
                             TRUE,
                             eurozone))
}

eurosystem_metadata <- eurosystem_metadata %>%
  filter(eurozone == TRUE)
eurosystem_text <- bis_text_updated %>% 
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

test_ocr = FALSE
if(test_ocr == TRUE) {
# save the list of speeches with problem of ocrisation (kind of loop with what has been
# already ocrise before)
test_ocr <- eurosystem_text_cleaned %>% 
  filter(problems == TRUE) %>% 
  ungroup() %>% 
  select(file) %>% 
  unique %>% 
  filter(! file %in% problem_ocr &
           ! file %in% c("r000323c", "r000725a", "r990928b")) # no problem with these ones

test_ocr %>% 
  write_csv2(paste0(bis_data_path, "/speech_in_need_of_OCR_updated.csv"))
}

#

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

supplementary_ocr = FALSE
if(supplementary_ocr == TRUE){

##################################################################################
###################### Supplement: ocr on problematic texts ######################

former_texts <- readRDS(here(data_path, "eurosystem_text.rds"))

# Complete by running OCR on problematic pdfs
problem_ocr <- read_csv2(here(bis_data_path,
                              "speech_in_need_of_OCR_updated.csv"))

pdf_files <- paste0(problem_ocr$file, ".pdf")
path_pdf <- here(path.expand("~"),
                 "data",
                 "scrap_bis", 
                 "pdf")

new_text_ocr <- tribble(
  ~text, ~page, ~file
)
for(i in pdf_files){
  text <- tesseract::ocr(here(path_pdf, i), engine = tesseract("eng"))
  text <- tibble(text) %>% 
    mutate(text = str_remove(text, ".+(=?\\\n)"),
           page = 1:n(),
           file = str_remove(i, "\\.pdf"))
  new_text_ocr <- new_text_ocr %>% 
    bind_rows(text)
}

# Removing the presentation page of speeches
new_text_ocr_paragraph <- new_text_ocr %>% 
  unnest_tokens("paragraphs", text, "paragraphs") %>% 
  mutate(paragraphs = str_squish(paragraphs),
         row = 1:n()) %>% 
  mutate(stars = str_detect(paragraphs, "\\* \\* \\*") & page == 1,
         characters = str_count(paragraphs),
         words = str_count(paragraphs, "[[:alpha:]]+"), 
         paragraphs = str_remove_all(paragraphs, paste0(to_remove, collapse = "|"))) %>% 
  filter(! paragraphs == "",
         ! str_detect(paragraphs, paste0(to_remove_strictly, collapse = "|")))

first_line <- new_text_ocr_paragraph %>% 
  filter(stars == TRUE) %>% 
  select(file, row) %>% 
  rename(first_line = row)

new_text_ocr_paragraph_cleaned <- new_text_ocr_paragraph %>% 
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


final_new_text_ocr <- new_text_ocr_paragraph_cleaned %>% 
  filter(words > 0) %>% 
  select(file, page, paragraph_number, paragraphs) %>%
  changing_page() %>% 
  changing_page() %>% 
  filter(alone_small_paragraph == FALSE) %>% 
  select(-alone_small_paragraph)

final_new_text_ocr_cleaned <- merging_paragraph(final_new_text_ocr) %>% # repeting the process until we have no more paragraphs below 50 words
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

final_new_text_ocr_cleaned <- final_new_text_ocr_cleaned %>% 
  mutate(document_name = paste0(file, "_page", page),
         document_id = paste0(document_name, "_paragraph", paragraph_number)) %>% 
  arrange(document_id)

updated_texts <- former_texts %>% 
  filter(! file %in% problem_ocr$file) %>% 
  bind_rows(final_new_text_ocr_cleaned)
saveRDS(updated_texts, here(data_path, "eurosystem_text.rds"))
}

