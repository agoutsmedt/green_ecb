############## Loading packages and data #####################

package_list <- c("tidyverse",
 #                 "httr",
  #                "jsonlite",
                  "academictwitteR",
                  "glue",
                  "here",
                  "tidytext",
                  "data.table",
                  "furrr",
                  "stm")

for(package in package_list){
  if(package %in% installed.packages() == FALSE){
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}

data_path <- here(path.expand("~"),
                  "data",
                  "green_ecb_responsiveness")
