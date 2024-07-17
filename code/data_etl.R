# march 2024 airbnb, not really worth to analyse earlier reviews, because only unlisted airbnbs are not contained inside the march data
# In my opionion the only really interesting use case is to analyse what types of airbnbs are unlisted, during a time period. 

# --- LISTINGS ---
df_listings <- read.csv("./data/airbnb/March_2024/listings_detailed.csv")

df_title_sentiments <- read.csv("./data/airbnb/March_2024/sentiments.csv") %>% 
  mutate(sentiment = ordered(sentiment, levels = c("negative", "neutral", "positive"))) %>% 
  rename_at(vars(-id), ~str_c(., "_for_name"))

df_listings <- inner_join(df_listings, df_title_sentiments, by = "id") %>%
  as_tibble()  

df_listings_cleaned <- df_listings %>%
  filter(!is.na(price))

# --- REVIEWS ---

df_review_sentiments <- read.csv("./data/airbnb/review_sentiments.csv") %>% 
  select(-id) %>%
  as_tibble()

df_review_sentiments <- inner_join(df_listings_cleaned, df_review_sentiments, by = c("id" = "listing_id")) %>%
  as_tibble()

df_review_sentiments_aggregated <- df_review_sentiments %>% 
  mutate(sentiment_score = positive_sentiment-negative_sentiment) %>% 
  group_by(id) %>% 
  summarise(median_sentiment_score_for_reviews = median(sentiment_score), mean_sentiment_score_for_reviews = mean(sentiment_score))



