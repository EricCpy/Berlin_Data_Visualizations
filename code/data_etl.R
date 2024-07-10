# march 2024 airbnb

df_listings_march2024 <- read.csv("./data/airbnb/March_2024/listings.csv")
df_sentiments_march2024 <- read.csv("./data/airbnb/March_2024/sentiments.csv") %>% 
  mutate(sentiment = ordered(sentiment, levels = c("negative", "neutral", "positive"))) %>% 
  rename_at(vars(-id), ~str_c(., "_for_name"))
df_sentiments_for_reviews_march2024 <- read.csv("./data/airbnb/review_sentiments.csv") %>% 
  as_tibble()
df_sentiments_for_reviews_march2024_aggregated <- df_sentiments_for_reviews_march2024 %>% 
  filter(language %in% c("de")) %>% 
  mutate(sentiment_score = positive_sentiment-negative_sentiment) %>% 
  group_by(listing_id) %>% 
  summarise(median_sentiment_score_for_reviews = median(sentiment_score), mean_sentiment_score_for_reviews = mean(sentiment_score))


df_airbnb_march2024 <- inner_join(
  df_listings_march2024, df_sentiments_march2024, by = "id"
  ) %>% left_join(
  df_sentiments_for_reviews_march2024_aggregated, by = c("id" = "listing_id")
  ) %>% as_tibble()

df_clean <- df_airbnb_march2024 %>%
  filter(!is.na(price))

