############## Loading packages and data #####################

package_list <- c("tidyverse",
                  "lubridate",
                  "stringi",
                  "academictwitteR",
                  "glue",
                  "here",
                  "tidytext",
                  "tm",
                  "quanteda",
                  "stopwords",
                  "textstem",
                  "data.table",
                  "furrr",
                  "stm",
                  "huge",
                  "igraph",
                  "tidygraph",
                  "ggraph",
                  "ggalluvial",
                  "scico",
                  "biblionetwork")

for(package in package_list){
  if(package %in% installed.packages() == FALSE){
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}


github_list <- c("agoutsmedt/networkflow")
for (p in github_list) {
  if (gsub(".*/", "", p) %in% installed.packages() == FALSE) {
    devtools::install_github(p)
  }
  library(gsub(".*/", "", p), character.only = TRUE)
}

data_path <- here(path.expand("~"),
                  "data",
                  "green_ecb_responsiveness")
