source("packages_and_data.R")

get_all_tweets(users = "ecb",
               start_tweets = "2010-01-01T00:00:00Z",
               end_tweets = "2022-05-20T00:00:00Z",
               data_path = here(data_path, "tweets", paste0("ecb_tweets_", Sys.Date())),
               n = Inf,
               bind_tweets = FALSE)
tweets <- bind_tweets(data_path = here(data_path, "tweets", paste0("ecb_tweets_", Sys.Date())))
saveRDS(tweets, here(data_path, "tweets", "ecb_tweets.RDS"))

get_all_tweets(retweets_of = "ecb",
               start_tweets = "2010-01-01T00:00:00Z",
               end_tweets = "2022-05-20T00:00:00Z",
               data_path = here(data_path, "tweets", paste0("ecb_retweets_", Sys.Date())),
               n = Inf,
               bind_tweets = FALSE)

retweets <- bind_tweets(data_path = here(data_path, "tweets", paste0("ecb_retweets_", Sys.Date()))) %>%
  unnest_wider(referenced_tweets, names_sep = "_")
saveRDS(retweets, here(data_path, "tweets", "ecb_retweets.RDS"))

retweets_user <- bind_tweets(data_path = here(data_path, "tweets", paste0("ecb_retweets_", Sys.Date())), user = TRUE)
saveRDS(retweets_user, here(data_path, "tweets", "ecb_retweets_user.RDS"))


# for later
nb_retweets <- retweets %>% 
  count(author_id)
top_retweeter <- retweets_user %>% 
  rename(author_id = id) %>% 
  left_join(nb_retweets) %>% 
  select(author_id, username, name, description, location, n) %>% 
  distinct(author_id, .keep_all = TRUE) %>% 
  arrange(-n) %>% 
  filter(n > 10)
  