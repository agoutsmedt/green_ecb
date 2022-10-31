source("packages_and_data.R")
source("function/functions_for_network_analysis.R")


tweets <- readRDS(here(data_path, "tweets", "ecb_tweets.RDS"))
retweets <- readRDS(here(data_path, "tweets", "ecb_retweets.RDS"))
retweets_users <- readRDS(here(data_path, "tweets", "ecb_retweets_user.RDS"))



set.seed(500) # set seed otherwise the name of communities change everytime the function is run


retweets <- retweets %>% 
  mutate(year = year(date),
         author_id = as.character(author_id),
         referenced_tweets_id = as.character(referenced_tweets_id))


users <- data.table(retweets_users) %>% 
  .[, list(id, username, name, description, public_metrics.followers_count)] %>% 
  unique()
doublons <- which(duplicated(users$id)) 
users <- users[-doublons] %>% 
  rename(author_id = id,
         followers = public_metrics.followers_count) %>% 
  filter(followers >= 100)

retweets <- data.table(retweets) %>% 
  .[, list(id, author_id, referenced_tweets_id, text, date, year)]
retweets_filtered <- retweets %>% 
  filter(author_id %in% users$author_id)

# Find the time_window
first_year <- as.integer(min(retweets$year))
last_year <- (as.integer(max(retweets$year)) - 2) # +1 to get the very last year in the window
all_years <- first_year:last_year

# Prepare our list
tbl_list <- list()
nodes <- users
direct_citation_dt <- retweets_filtered

for (Year in all_years) {

  edges_threshold <- 3
  message(paste0("- Creation of the network for the ", Year, "-", Year + 2, " window."))
  edges_of_the_year <- direct_citation_dt[between(year, Year, Year + 2)]
  nodes_of_the_year <- nodes[author_id %in% edges_of_the_year$author_id] # < for time_window being the number of years, note the value of the addition
  
  # size of nodes
  nb_retweets <- edges_of_the_year[, .N, referenced_tweets_id]
  colnames(nb_retweets)[colnames(nb_retweets) == "N"] <- "nb_retweets"

  edges <- biblionetwork::coupling_strength(dt = edges_of_the_year, 
                                          source = "author_id", 
                                          ref = "referenced_tweets_id", 
                                          weight_threshold = 2, 
                                          output_in_character = TRUE)
  
  # remove nodes with no edges
  nodes_of_the_year <- nodes_of_the_year[author_id %in% edges$from | author_id %in% edges$to]
  
  # make tbl
  tbl_list[[as.character(Year)]] <- networkflow::tbl_main_component(nodes = nodes_of_the_year, edges = edges, directed = FALSE, node_key = source)
}

tbl_coup_list <- lapply(tbl_list, leiden_workflow, res_1 = 1.1)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Intertemporal Naming ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' ## Intertemporal Naming + size
#tbl_coup_list <- readRDS(here(graph_data_path, paste0("list_graph_", first_year, "-", last_year, ".rds")))
tbl_coup_list <- lapply(tbl_coup_list, 
                        function(tbl){tbl %>% 
                            activate(nodes) %>% 
                            mutate(size = followers,
                                   ID_Art = as.character(Id))})

list_graph <- tbl_coup_list
intertemporal_naming <- intertemporal_naming_function(list_graph, 
                                                      community_column = "Com_ID", 
                                                      individual_ids = "ID_Art",
                                                      threshold_similarity = 0.52)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Into Alluvial ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

intertemporal_naming <- lapply(intertemporal_naming,
                               function(tbl){tbl %>% 
                                   activate(nodes) %>% 
                                   mutate(Titre = description)})

alluv_dt <- make_into_alluv_dt(intertemporal_naming)

alluv_dt$new_Id_com   <- as.factor(alluv_dt$new_Id_com)
alluv_dt$new_Id_com <- fct_reorder(alluv_dt$new_Id_com, alluv_dt$share_leiden_total,min, .desc = FALSE)
#alluv_dt$new_Id_com <- fct_reorder(alluv_dt$new_Id_com, alluv_dt$n_years,min, .desc = FALSE)

######################### Colors **********************
scico1 <- scico::scico(25, palette = "tokyo")
scico2 <- scico::scico(25, palette = "roma")
scico3 <- scico::scico(25, palette = "hawaii")
color_final <- append(scico1, scico2)
color_final <- append(color_final, scico3)

unique_ids_color <- data.table(
  Leiden1 = as.character(alluv_dt[n_years>0 & share_leiden>=0.04,.N,new_Id_com]$new_Id_com), 
  color = color_final[c(1:alluv_dt[n_years>0 & share_leiden>=0.04,.N,new_Id_com][,.N])])
alluv_dt<-merge(alluv_dt, unique_ids_color[,.(Leiden1,color)], by="Leiden1",all.x = TRUE)
alluv_dt[is.na(color)==TRUE,color:="grey"]

#' ## Projecting the alluvials and saving

######################### Label **********************
label <- copy(alluv_dt)
label <- label[,Window:=round(mean(as.numeric(Window))),new_Id_com][color!="grey", head(.SD, 1), .(new_Id_com)]
label[,Label:=new_Id_com]

alluv_dt<-merge(alluv_dt,label[,.(new_Id_com,Window,Label)], by = c("new_Id_com","Window"), all.x = TRUE) 

######################### Minimize Crossing **********************
alluv_dt[,Id:=ID_Art]
alluv_dt <- minimize_crossing(alluv_dt)


alluv_dt$new_Id_com <- fct_reorder(alluv_dt$new_Id_com, alluv_dt$order,min, .desc = TRUE)
alluv_dt[,order:=NULL]

plot_alluvial <- ggplot(alluv_dt, aes(x = Window, y=share, stratum = new_Id_com, alluvium = ID_Art, fill = color, label = new_Id_com)) +
  scale_fill_identity("Disciplines", guide = "legend") +
  geom_flow() +
  geom_stratum(alpha =1, size=1/10) +
  theme(legend.position = "none") +
  geom_label(stat = "stratum", size = 5, aes(label = Label)) +
  ggtitle("")

ggsave(here("pictures", "alluvial.png"), plot = plot_alluvial, width = 40, height = 30, units = "cm")

alluv_dt <- alluv_dt[, Com_ID := new_Id_com]
alluv_dt <- alluv_dt[, share_max := max(share_leiden), by = "Com_ID"]

# adding a more simple label for naming communities later
ID_bis <- unique(alluv_dt[color != "grey"][order(Window,-n_years), "Com_ID"])
ID_bis <- ID_bis[, ID_bis:= 1:.N]
alluv_dt <- merge(alluv_dt, ID_bis, by = "Com_ID", all.x = TRUE)

saveRDS(alluv_dt, here(data_path, "tweets", "alluv.rds"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### tf-idf ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' ## Search tf-idf values for each community

################## search for tf-idf ############################

alluv_dt_with_tweets <- alluv_dt %>% 
  left_join(select(retweets, author_id, id = referenced_tweets_id)) %>% 
  left_join(select(tweets, id, text))

tf_idf_data <- alluv_dt_with_tweets %>% 
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "[a-z]")) %>% 
  anti_join(stop_words) %>% 
  mutate(lemma = textstem::lemmatize_words(word)) %>% 
  add_count(new_Id_com, lemma) %>% 
  filter(n >= 3) %>% 
  select(new_Id_com, lemma, n) %>% 
  unique() %>% 
  bind_tf_idf(lemma, new_Id_com, n)
  
saveRDS(tf_idf_data, here(data_path, "tweets", "tf_idf.rds"))
