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
         year >= 2010) 

eurosystem_text <- bis_text %>% 
  filter(file %in% eurosystem_metadata$file_name)

########## cleaning text ##################

to_remove <- c("BIS Review",
               "BIS central bankers(’|')? speeches( \\d+)?$",
               "^[\\d \\.]+$",
               "^Page \\d+ sur \\d+$",
               "^Figure \\d+$")

eurosystem_text_paragraph <- eurosystem_text %>% 
  mutate(text = str_split(text, "\n\n")) %>% 
  unnest(text) %>% 
  mutate(text = str_squish(text),
         row = 1:n()) %>% 
  filter(! text == "",
         ! str_detect(text, paste0(to_remove, collapse = "|"))) %>% 
  mutate(stars = str_detect(text, "\\* \\* \\*") & page == 1,
         length = str_count(text),
         text = str_remove_all(text, "^Source(s)?: |^Chart \\d{1,2}|^Figure \\d{1,2}|(FOCUS|Question): "))

first_line <- eurosystem_text_paragraph %>% 
  filter(stars == TRUE) %>% 
  select(file, row) %>% 
  rename(first_line = row)

eurosystem_text_paragraph <- eurosystem_text_paragraph %>% 
  left_join(first_line) %>% 
  filter(row >= first_line | is.na(first_line),
         length > 12)

eurosystem_text_cleaned <- eurosystem_text_paragraph %>% 
  group_by(file, page) %>% 
  mutate(text = paste0(text, collapse = " ")) %>% 
  select(-c(row, stars, length, first_line)) %>% 
  unique

eurosystem_text_cleaned <- eurosystem_text_cleaned %>% 
  mutate(length = str_count(text, "\\w"),
         problems = str_detect(text, ""),
         en_tete = page == 1 & length < 400) %>% 
  filter(length > 40,
         ! str_detect(text, "^Designed (and|by)? .*Italy$"),
         problems == FALSE,
         en_tete == FALSE) %>% 
  mutate(page = ifelse(length < 100, page + 1, page)) %>% 
  group_by(file, page) %>% 
  mutate(text = paste0(text, collapse = " ")) %>% 
  select(-c(length, problems, en_tete)) %>% 
  unique

saveRDS(eurosystem_metadata, here(data_path, "eurosystem_metadata.rds"))
saveRDS(eurosystem_text_cleaned, here(data_path, "eurosystem_text.rds"))
