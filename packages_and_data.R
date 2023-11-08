############## Loading packages and data #####################

package_list <- c("tidyverse",
                  "lubridate",
                  "stringi",
        #          "academictwitteR",
                  "here",
                  "tidytext",
                  "tm",
                  "quanteda",
                  "topicmodels",
                  "ldatuning",
                  "tosca",
                  "stopwords",
                  "textstem",
                  "data.table",
                  "furrr",
                  "stm",
         #         "torch",
          #        "topicmodels.etm",
           #       "udpipe",
             #     "huge",
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


github_list <- c("agoutsmedt/networkflow", "mikajoh/tidystm")
for (p in github_list) {
  if (gsub(".*/", "", p) %in% installed.packages() == FALSE) {
    devtools::install_github(p)
  }
  library(gsub(".*/", "", p), character.only = TRUE)
}

if(str_detect(here::here(), "\\/home\\/aurelien")){
  data_path <- "/projects/data/aurelien/green_ecb_responsiveness"
} else {
  data_path <- here(path.expand("~"),
                    "data",
                    "green_ecb_responsiveness")
}
