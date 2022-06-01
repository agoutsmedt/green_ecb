############## Loading packages and data #####################

package_list <- c("tidyverse",
                  "stringi",
                  "academictwitteR",
                  "glue",
                  "here",
                  "tidytext",
                  "tm",
                  "quanteda",
                  "data.table",
                  "furrr",
                  "stm",
                  "tidygraph",
                  "ggraph",
                  "ggalluvial",
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
