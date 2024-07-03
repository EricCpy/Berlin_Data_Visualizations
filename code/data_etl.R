# march 2024 airbnb

df_listings_march2024 <- read.csv("./data/airbnb/March_2024/listings.csv")
df_sentiments_march2024 <- read.csv("./data/airbnb/March_2024/sentiments.csv")

df_airbnb_march2024 <- inner_join(df_listings_march2024, df_sentiments_march2024, by = "id")