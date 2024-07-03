source("code/setup.R")

# Sentiment Analysis

# perhaps use piechart and compare to filtered listings 
ggplot(df_airbnb_march2024, aes(x = sentiment)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Sentiment Count", x = "Sentiment", y = "Count") +
  theme_minimal()


df_melted <- melt(df_airbnb_march2024, id.vars = c('id', 'sentiment'), 
                  measure.vars = c('positive_sentiment', 'neutral_sentiment', 'negative_sentiment'), 
                  variable.name = 'sentiment_type', value.name = 'percentage')

ggplot(df_melted, aes(x = percentage, fill = sentiment_type)) +
  geom_density(alpha = 0.6) +
  labs(title = "Density of sentiment percentages", x = "Percentage", y = "Density") +
  theme_minimal()


# Sentiment Price Analysis

df_clean <- df_airbnb_march2024 %>%
  filter(!is.na(price))

mean_price <- mean(df_clean$price, na.rm = TRUE)
mean_price
sd_price <- sd(df_clean$price, na.rm = TRUE)
sd_price
z99 <- qnorm(0.95) # maybe change interval
ci_low <- mean_price - z99 * sd_price / sqrt(nrow(df_clean))
ci_high <- mean_price + z99 * sd_price / sqrt(nrow(df_clean))

df_filtered <- df_clean %>%
  filter(price >= ci_low & price <= ci_high)


ggplot(df_filtered, aes(x = sentiment, y = price, fill = sentiment)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Distribution of prices by sentiment", x = "Sentiment", y = "Price") +
  theme_minimal()
