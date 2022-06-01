source("packages_and_data.R")

get_all_tweets(users = "ecb",
               start_tweets = "2010-01-01T00:00:00Z",
               end_tweets = "2022-05-20T00:00:00Z",
               data_path = here(data_path, "tweets", paste0("ecb_tweets_", Sys.Date())),
               n = Inf,
               bind_tweets = FALSE)
tweets <- bind_tweets(data_path = here(data_path, "tweets", "ecb_tweets_2022-05-21"))
tweets <- tweets %>% 
  mutate(date = str_extract(created_at, ".*(?=T)"), 
         date = parse_date(date))
saveRDS(tweets, here(data_path, "tweets", "ecb_tweets.RDS"))

retweets <- bind_tweets(data_path = here(data_path, "tweets", "ecb_retweets_2022-05-21")) %>%
  unnest_wider(referenced_tweets, names_sep = "_")
retweets <- retweets %>% 
  mutate(date = str_extract(created_at, ".*(?=T)"), 
         date = parse_date(date))
saveRDS(retweets, here(data_path, "tweets", "ecb_retweets.RDS"))

retweets_user <- bind_tweets(data_path = here(data_path, "tweets", "ecb_retweets_2022-05-21"), user = TRUE) %>% 
  unnest_wider(public_metrics)
saveRDS(retweets_user, here(data_path, "tweets", "ecb_retweets_user.RDS"))

map(tweets$id[str_which(tweets$id, "999949330869039105") + 1:nrow(tweets)], ~get_all_tweets(query = .,
                               start_tweets = "2010-01-01T00:00:00Z",
                               end_tweets = "2022-05-20T00:00:00Z",
                               is_quote = TRUE,
                               data_path = here(data_path, "tweets", "ecb_quotes_2022-05-24"),
                               n = Inf,
                               bind_tweets = FALSE))
quotes <- bind_tweets(data_path = here(data_path, "tweets", "ecb_quotes_2022-05-24"))

quotes <- quotes %>% 
  unnest_wider(referenced_tweets, names_repair = "unique") %>% 
  rename(referenced_tweet = `id...11`,
         id = `id...8`) %>% 
  unnest_auto(type) %>% 
  unnest_auto(referenced_tweet) %>% 
  mutate(date = str_extract(created_at, ".*(?=T)"), 
         date = parse_date(date))
saveRDS(quotes, here(data_path, "tweets", "ecb_quotes.RDS"))

quotes_user <- bind_tweets(data_path = here(data_path, "tweets", "ecb_quotes_2022-05-24"), user = TRUE) 

saveRDS(quotes_user, here(data_path, "tweets", "ecb_quotes_user.RDS"))

# stop at 1045259970017669120
# 132470815

str_which(tweets$id, "999949330869039105")

#liking_users <- get_liking_users(x = tweets$id) too_long
#saveRDS(retweets_user, here(data_path, "tweets", "ecb_liking_user.RDS"))