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

saveRDS(eurosystem_metadata, here(data_path, "eurosystem_metadata.rds"))
saveRDS(eurosystem_text, here(data_path, "eurosystem_text.rds"))
