source("code/setup.R")

# Sentiment Analysis
test <- df_review_sentiments[df_review_sentiments$sentiment_for_name == "negative", c("name", "sentiment_for_name")]

# TOP 10 languages
df_review_languages <- read.csv("./data/airbnb/March_2024/review_sentiments_languages.csv")
language_counts <- df_review_languages %>%
  group_by(language) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

top_languages <- language_counts %>%
  top_n(10, count)

ggplot(top_languages, aes(x = reorder(language, count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 languages by number of reviews",
       x = "Language",
       y = "Number of reviews") +
  theme_minimal() +
  coord_flip()


# perhaps use piechart and compare to filtered listings 
ggplot(df_listings, aes(x = sentiment_for_name)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Sentiment Count", x = "Sentiment", y = "Count") +
  theme_minimal()


df_melted <- melt(df_listings, id.vars = c('id', 'sentiment_for_name'), 
                  measure.vars = c('positive_sentiment_for_name', 'neutral_sentiment_for_name', 'negative_sentiment_for_name'), 
                  variable.name = 'sentiment_type', value.name = 'percentage')

df_melted$percentage <- round(df_melted$percentage, 2)

ggplot(df_melted, aes(x = percentage, fill = sentiment_type)) +
  geom_density(alpha = 0.6) +
  labs(title = "Density of sentiment percentages", x = "Percentage", y = "Density") +
  theme_minimal()


# Sentiment Price Analysis

mean_price <- mean(df_listings_cleaned$price)
mean_price
sd_price <- sd(df_listings_cleaned$price)
sd_price
z99 <- qnorm(0.975) # maybe change interval
ci_low <- mean_price - z99 * sd_price / sqrt(nrow(df_listings_cleaned))
ci_high <- mean_price + z99 * sd_price / sqrt(nrow(df_listings_cleaned))

df_filtered <- df_listings_cleaned %>%
  filter(price >= ci_low & price <= ci_high)

ggplot(df_filtered, aes(x = sentiment_for_name, y = price, fill = sentiment_for_name)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Distribution of prices by sentiment", x = "Sentiment", y = "Price") +
  theme_minimal()

ggplot(df_listings_cleaned, aes(x = sentiment_for_name, y = log_price, fill = sentiment_for_name)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Distribution of prices by sentiment", x = "Sentiment", y = "Price", fill="Sentiment") +
  theme_minimal()

ggplot(df_listings_cleaned, aes(x = sentiment_for_name, y = review_scores_rating, fill = sentiment_for_name)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Distribution of rating by sentiment", x = "Sentiment", y = "Rating") +
  theme_minimal()

# same but for airbnb reviews

ggplot(df_review_sentiments, aes(x = sentiment)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Sentiment Count", x = "Sentiment", y = "Count") +
  theme_minimal()

ggplot(df_review_sentiments, aes(x = sentiment, y = log_price, fill = sentiment)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Distribution of prices by sentiment", x = "Sentiment", y = "Price") +
  theme_minimal()


df_filtered <- df_review_sentiments %>%
  filter(price >= ci_low & price <= ci_high)

ggplot(df_filtered, aes(x = sentiment, y = price, fill = sentiment)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Distribution of prices by sentiment", x = "Sentiment", y = "Price") +
  theme_minimal()

# ---- START OF VISUALIZATION: are there more english airbnbs in some districts, review ratings in these districts by english reviews ---- 
df_english_reviews <- df_review_sentiments %>%
  filter(language == "en")

df_german_reviews <- df_review_sentiments %>%
  filter(language == "de")

df_review_percentages <- df_review_sentiments %>%
  group_by(neighbourhood_group_cleansed) %>%
  summarize(
    total_reviews = n(),
    english_reviews = sum(language == "en"),
    german_reviews = sum(language == "de")
  ) %>%
  mutate(
    percent_english = english_reviews / nrow(df_english_reviews),
    percent_german = german_reviews / nrow(df_german_reviews)
  )

score_to_color <- function(score, min_score, max_score) {
  color_palette <- colorRampPalette(c("slateblue3", "lightblue", "limegreen"))(100)
  color_palette[as.numeric(cut(score, breaks = seq(min_score, max_score, length.out = 101), include.lowest = TRUE))]
}

bezirke_name_id <- tribble(
  ~BEZ_NAME, ~BEZ_ID,
  "Mitte", "01",
  "Friedrichshain-Kreuzberg", "02",
  "Pankow", "03",
  "Charlottenburg-Wilmersdorf", "04",
  "Spandau", "05",
  "Steglitz-Zehlendorf", "06",
  "Tempelhof-Schöneberg", "07",
  "Neukölln", "08",
  "Treptow-Köpenick", "09",
  "Marzahn-Hellersdorf", "10",
  "Lichtenberg", "11",
  "Reinickendorf", "12",
)

lor <- sf::st_read(dsn = "data/lor/Planung") %>% 
  st_make_valid(tol = 0.00001) %>% st_transform(4326) %>% 
  mutate(
    BEZ_ID = str_sub(PLR_ID, 1, 2),
    PGR_ID = str_sub(PLR_ID, 1, 4),
    BZR_ID = str_sub(PLR_ID, 1, 6)
  ) %>% 
  left_join(bezirke_name_id, by = c("BEZ_ID" = "BEZ_ID"))

lor <- left_join(lor, df_review_percentages, by = c("BEZ_NAME" = "neighbourhood_group_cleansed"))

min_percent <- min(df_review_percentages$percent_english, df_review_percentages$percent_german)
max_percent <- max(df_review_percentages$percent_english, df_review_percentages$percent_german)

lor_combined <- lor %>%
  group_by(BEZ_ID, BEZ_NAME) %>%
  summarize(
    geometry = st_union(geometry),
    percent_german = unique(percent_german),
    percent_english = unique(percent_english),
    .groups = 'drop'
  ) %>%
  mutate(
    color_german = score_to_color(percent_german, min_percent, max_percent),
    color_english = score_to_color(percent_english, min_percent, max_percent),
    label_combined = paste(BEZ_ID, BEZ_NAME, sep = ": ")
  )

plot_ger <- ggplot() +
  geom_sf(data = lor_combined, aes(geometry = geometry, fill = label_combined)) +
  geom_sf_text(data = lor_combined, aes(geometry = geometry, label = BEZ_ID), size = 3, color = "black") +
  scale_fill_manual(values = lor_combined$color_german, name = "Neighborhood (ID: Name)") +
  theme_void() +
  labs(title = "Percentage of German reviews") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))

# Plot for English reviews
plot_eng <- ggplot() +
  geom_sf(data = lor_combined, aes(geometry = geometry, fill = label_combined)) +
  geom_sf_text(data = lor_combined, aes(geometry = geometry, label = BEZ_ID), size = 3, color = "black") +
  scale_fill_manual(values = lor_combined$color_english, name = "Neighborhood (ID: Name)") +
  theme_void() +
  labs(title = "Percentage of English reviews") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))

grid.arrange(plot_ger, plot_eng, ncol=3)

# ---- END OF VISUALIZATION: are there more english airbnbs in some districts, review ratings in these districts by english reviews ----

# ---- START OF VISUALIZATION: in which months/years are more english/german reviews, where are the peaks in reviews ----
df_english_reviews <- df_review_sentiments %>%
  filter(language == "en")

df_german_reviews <- df_review_sentiments %>%
  filter(language == "de")

overall_yearly_sentiment_en <- df_english_reviews %>%
  group_by(year) %>%
  summarize(number_of_reviews = n() / nrow(df_english_reviews), .groups = 'drop')

overall_yearly_sentiment_de <- df_german_reviews %>%
  group_by(year) %>%
  summarize(number_of_reviews = n() / nrow(df_english_reviews), .groups = 'drop')

ggplot(overall_yearly_sentiment_en, aes(x = year, y = mean_sentiment)) +
  geom_line(group = 1, color = "blue") +
  geom_point(color = "blue") +
  scale_x_continuous(breaks = seq(min(overall_yearly_sentiment_en$year), 
                                  max(overall_yearly_sentiment_en$year), 
                                  by = 1)) + 
  labs(title = "Mean Sentiment Score Over the Years (English Reviews)",
       x = "Year", y = "Mean Sentiment Score") +
  theme_minimal()

# Plot for German reviews
ggplot(overall_yearly_sentiment_de, aes(x = year, y = mean_sentiment)) +
  geom_line(group = 1, color = "red") +
  geom_point(color = "red") +
  scale_x_continuous(breaks = seq(min(overall_yearly_sentiment_de$year), 
                                  max(overall_yearly_sentiment_de$year), 
                                  by = 1)) + 
  labs(title = "Mean Sentiment Score Over the Years (German Reviews)",
       x = "Year", y = "Mean Sentiment Score") +
  theme_minimal()

df_english_reviews$month <- as.numeric(format(df_english_reviews$date, "%m"))
df_german_reviews$month <- as.numeric(format(df_german_reviews$date, "%m"))

overall_monthly_sentiment_en <- df_english_reviews %>%
  group_by(month) %>%
  summarize(mean_sentiment = mean(sentiment_score), .groups = 'drop')

# For German reviews
overall_monthly_sentiment_de <- df_german_reviews %>%
  group_by(month) %>%
  summarize(mean_sentiment = mean(sentiment_score), .groups = 'drop')

month_labels <- setNames(month.abb, 1:12)

# Plot for English reviews
ggplot(overall_monthly_sentiment_en, aes(x = month, y = mean_sentiment)) +
  geom_line(group = 1, color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Mean Sentiment Score Over the Months (English Reviews)",
       x = "Month", y = "Mean Sentiment Score") +
  scale_x_continuous(breaks = 1:12, labels = month_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Plot for German reviews
ggplot(overall_monthly_sentiment_de, aes(x = month, y = mean_sentiment)) +
  geom_line(group = 1, color = "red") +
  geom_point(color = "red") +
  labs(title = "Mean Sentiment Score Over the Months (German Reviews)",
       x = "Month", y = "Mean Sentiment Score") +
  scale_x_continuous(breaks = 1:12, labels = month_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# ---- END OF VISUALIZATION: in which months are more english/german reviews, where are the peaks in reviews ----

# ---- START OF VISUALIZATION: english vs german reviews/ratings, are german reviews/expectations lower/higher ----
df_percentage <- df_review_sentiments %>%
  group_by(language, sentiment) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(language) %>%
  mutate(total = sum(count),
         percentage = count / total * 100) %>%
  select(language, sentiment, percentage)

ggplot(df_percentage, aes(x = language, y = percentage, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("negative" = "red", "positive" = "green", "neutral" = "blue")) +
  labs(title = "Percentage of Sentiments by Language",
       x = "Language",
       y = "Percentage") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# conclusion: most reviews are positive and germans have more negative reviews, 
# but we cant really compare these results because we used different sentiment analyizers for different languages

# ---- END OF VISUALIZATION: english vs german reviews, are german reviews/expectations lower/higher -----

# ---- START OF VISUALIZATION: review sentiments over time ----
library(RColorBrewer)
color_palette <- brewer.pal(n = length(unique(df_review_sentiments$neighbourhood_group_cleansed)), name = "Set3")
df_review_sentiments$date <- as.Date(df_review_sentiments$date, format = "%Y-%m-%d")
df_review_sentiments$year <- as.numeric(format(df_review_sentiments$date, "%Y"))

yearly_sentiment_by_neighbourhood <- df_review_sentiments %>%
  group_by(year, neighbourhood_group_cleansed) %>%
  summarize(mean_sentiment = mean(sentiment_score), .groups = 'drop')

overall_yearly_sentiment <- df_review_sentiments %>%
  group_by(year) %>%
  summarize(mean_sentiment = mean(sentiment_score), .groups = 'drop')

ggplot(overall_yearly_sentiment, aes(x = year, y = mean_sentiment)) +
  geom_line(group = 1) +
  geom_point() +
  scale_x_continuous(breaks = seq(min(overall_yearly_sentiment$year), 
                                  max(overall_yearly_sentiment$year), 
                                  by = 1)) + 
  labs(title = "Mean Sentiment Score Over the Years",
       x = "Year", y = "Mean Sentiment Score") +
  theme_minimal()

ggplot() +
  geom_line(data = yearly_sentiment_by_neighbourhood, aes(x = year, y = mean_sentiment, color = neighbourhood_group_cleansed, group = neighbourhood_group_cleansed), size = 1, alpha = 2) +
  geom_point(data = yearly_sentiment_by_neighbourhood, aes(x = year, y = mean_sentiment, color = neighbourhood_group_cleansed, group = neighbourhood_group_cleansed), size = 1.75, alpha = 2) +
  geom_line(data = overall_yearly_sentiment, aes(x = year, y = mean_sentiment), color = "black", size = 1.2, alpha = 0.5) +
  geom_point(data = overall_yearly_sentiment, aes(x = year, y = mean_sentiment), color = "black", size = 2, alpha = 0.5) +
  scale_x_continuous(breaks = seq(min(overall_yearly_sentiment$year), 
                                  max(overall_yearly_sentiment$year), 
                                  by = 1)) + 
  labs(title = "Mean Sentiment Score Over the Years by District",
       x = "Year", y = "Mean Sentiment Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_color_manual(values = color_palette, name = "District")

# Months

df_review_sentiments$month <- as.numeric(format(df_review_sentiments$date, "%m"))

monthly_sentiment_by_neighbourhood <- df_review_sentiments %>%
  group_by(month, neighbourhood_group_cleansed) %>%
  summarize(mean_sentiment = mean(sentiment_score), .groups = 'drop')

overall_monthly_sentiment <- df_review_sentiments %>%
  group_by(month) %>%
  summarize(mean_sentiment = mean(sentiment_score), .groups = 'drop')

month_labels <- setNames(month.abb, 1:12)

ggplot(overall_monthly_sentiment, aes(x = month, y = mean_sentiment)) +
  geom_line(group = 1) +
  geom_point() +
  labs(title = "Mean Sentiment Score Over the Months",
       x = "Month", y = "Mean Sentiment Score") +
  scale_x_continuous(breaks = 1:12, labels = month_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot() +
  geom_line(data = monthly_sentiment_by_neighbourhood, aes(x = month, y = mean_sentiment, color = neighbourhood_group_cleansed, group = neighbourhood_group_cleansed), size = 1, alpha = 2) +
  geom_point(data = monthly_sentiment_by_neighbourhood, aes(x = month, y = mean_sentiment, color = neighbourhood_group_cleansed, group = neighbourhood_group_cleansed), size = 1.75, alpha = 2) +
  geom_line(data = overall_monthly_sentiment, aes(x = month, y = mean_sentiment), color = "black", size = 1.2, alpha = 0.5) +
  geom_point(data = overall_monthly_sentiment, aes(x = month, y = mean_sentiment), color = "black", size = 2, alpha = 0.5) +
  labs(title = "Mean Sentiment Score Over the Months by District",
       x = "Month", y = "Mean Sentiment Score") +
  scale_x_continuous(breaks = 1:12, labels = month_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_color_manual(values = color_palette, name = "District")

# ---- END OF VISUALIZATION: review sentiments over time ----

# ---- START OF VISUALIZATION:  map price to review sentiment to rating in dataset ----
rating_plot <- ggplot(df_listings_cleaned_with_review_sentiment, aes(x = review_scores_rating, y = price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Correlation Plot: Price vs Rating Score",
    x = "Price",
    y = "Rating Score"
  ) +
  theme_minimal()

sentiment_plot <- ggplot(df_listings_cleaned_with_review_sentiment, aes(x = mean_sentiment_score_for_reviews, y = price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Correlation Plot: Price vs Review Sentiment Score",
    x = "Price",
    y = "Review Sentiment Score"
  ) +
  theme_minimal()

grid.arrange(rating_plot, sentiment_plot, ncol=2)


# correlation between price and rating
summary(aov(review_scores_rating ~ neighbourhood_group_cleansed, data=df_listings_cleaned_with_review_sentiment))
summary(aov(mean_sentiment_score_for_reviews ~ neighbourhood_group_cleansed, data=df_listings_cleaned_with_review_sentiment))

ggplot(df_listings_cleaned_with_review_sentiment, aes(x = price, y = review_scores_rating)) +
  geom_point(aes(color = neighbourhood_group_cleansed), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, aes(color = neighbourhood_group_cleansed)) +
  facet_wrap(~ neighbourhood_group_cleansed) + 
  labs(
    title = "Correlation Plot: Price vs Rating Score by Bezirk",
    x = "Price",
    y = "Rating Score"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),
    legend.position = "bottom"
  )

# correlation between price and review sentiment
ggplot(df_listings_cleaned_with_review_sentiment, aes(x = price, y = median_sentiment_score_for_reviews)) +
  geom_point(aes(color = neighbourhood_group_cleansed), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, aes(color = neighbourhood_group_cleansed)) +
  facet_wrap(~ neighbourhood_group_cleansed) + 
  labs(
    title = "Correlation Plot: Price vs Review Sentiment Score by Bezirk",
    x = "Price",
    y = "Rating Score"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),
    legend.position = "bottom" 
  )


# test to see which variable has stronger influence on price / is more significant
model_both <- lm(price ~ review_scores_rating + mean_sentiment_score_for_reviews, data = df_listings_cleaned_with_review_sentiment)
summary(model_both)

# ---- END OF VISUALIZATION:  map price to review sentiment to rating in dataset ----

# ---- START OF VISUALIZATION: map reviews in region by sentiment -----
region_sentiment_scores <- df_listings_cleaned_with_review_sentiment %>%
  group_by(neighbourhood_group_cleansed) %>%
  summarize(sentiment_region_score = mean(mean_sentiment_score_for_reviews))

min_score <- min(region_sentiment_scores$sentiment_region_score)
max_score <- max(region_sentiment_scores$sentiment_region_score)

score_to_color <- function(score, min_score, max_score) {
  color_palette <- colorRampPalette(c("slateblue3", "lightblue", "limegreen"))(100)
  color_palette[as.numeric(cut(score, breaks = seq(min_score, max_score, length.out = 101), include.lowest = TRUE))]
}

region_sentiment_scores <- region_sentiment_scores %>%
  mutate(color = score_to_color(sentiment_region_score, min_score, max_score))

bezirke_name_id <- tribble(
  ~BEZ_NAME, ~BEZ_ID,
  "Mitte", "01",
  "Friedrichshain-Kreuzberg", "02",
  "Pankow", "03",
  "Charlottenburg-Wilmersdorf", "04",
  "Spandau", "05",
  "Steglitz-Zehlendorf", "06",
  "Tempelhof-Schöneberg", "07",
  "Neukölln", "08",
  "Treptow-Köpenick", "09",
  "Marzahn-Hellersdorf", "10",
  "Lichtenberg", "11",
  "Reinickendorf", "12",
)

lor <- sf::st_read(dsn = "data/lor/Planung") %>% 
  st_make_valid(tol = 0.00001) %>% st_transform(4326) %>% 
  mutate(
    BEZ_ID = str_sub(PLR_ID, 1, 2),
    PGR_ID = str_sub(PLR_ID, 1, 4),
    BZR_ID = str_sub(PLR_ID, 1, 6)
  ) %>% 
  left_join(bezirke_name_id, by = c("BEZ_ID" = "BEZ_ID"))

lor <- left_join(lor, region_sentiment_scores, by = c("BEZ_NAME" = "neighbourhood_group_cleansed"))

lor_combined <- lor %>%
  group_by(BEZ_ID, BEZ_NAME) %>%
  summarize(geometry = st_union(geometry), color = unique(color), .groups = 'drop') %>%
  mutate(label_combined = paste(BEZ_ID, BEZ_NAME, sep = ": "))

ggplot() +
  geom_sf(data = lor_combined, aes(geometry = geometry, fill = label_combined)) +
  geom_sf_text(data = lor_combined, aes(geometry = geometry, label = BEZ_ID), size = 3, color = "black") +
  scale_fill_manual(values = unique(lor_combined$color), name = "Bezirke (ID: Name)") +
  theme_void() +
  labs(title = "Sentiment Map of Berlin") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))

ggplot(region_sentiment_scores, aes(x = reorder(neighbourhood_group_cleansed, sentiment_region_score), 
                                    y = sentiment_region_score, fill = color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() + 
  coord_flip() +
  labs(title = "Sentiment Scores by Neighborhood",
       x = "Neighborhood",
       y = "Sentiment Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# ---- END OF VISUALIZATION: map reviews in region by price segments -----

# ---- START OF VISUALIZATION: map reviews in region by number ----
listings_per_bezirk <- df_listings_cleaned_with_review_sentiment %>%
  group_by(neighbourhood_group_cleansed) %>%
  summarize(num_listings = n()) %>%
  ungroup()

total_listings <- sum(listings_per_bezirk$num_listings)
listings_per_bezirk <- listings_per_bezirk %>%
  mutate(percentage_listings = (num_listings / total_listings) * 100)

region_data <- region_sentiment_scores %>%
  left_join(listings_per_bezirk, by = "neighbourhood_group_cleansed")

min_percentage <- min(region_data$percentage_listings)
max_percentage <- max(region_data$percentage_listings)

score_to_color <- function(score, min_score, max_score) {
  color_palette <- colorRampPalette(c("slateblue3", "lightblue", "limegreen"))(100)
  color_palette[as.numeric(cut(score, breaks = seq(min_score, max_score, length.out = 101), include.lowest = TRUE))]
}

region_data <- region_data %>%
  mutate(color = score_to_color(percentage_listings, min_percentage, max_percentage))

lor <- sf::st_read(dsn = "data/lor/Planung") %>%
  st_make_valid(tol = 0.00001) %>%
  st_transform(4326) %>%
  mutate(
    BEZ_ID = str_sub(PLR_ID, 1, 2),
    PGR_ID = str_sub(PLR_ID, 1, 4),
    BZR_ID = str_sub(PLR_ID, 1, 6)
  ) %>%
  left_join(bezirke_name_id, by = c("BEZ_ID" = "BEZ_ID")) %>%
  left_join(region_data, by = c("BEZ_NAME" = "neighbourhood_group_cleansed"))

lor_combined <- lor %>%
  group_by(BEZ_ID, BEZ_NAME) %>%
  summarize(geometry = st_union(geometry), color = unique(color), .groups = 'drop') %>%
  mutate(label_combined = paste(BEZ_ID, BEZ_NAME, sep = ": "))

ggplot() +
  geom_sf(data = lor_combined, aes(geometry = geometry, fill = label_combined)) +
  geom_sf_text(data = lor_combined, aes(geometry = geometry, label = BEZ_ID), size = 3, color = "black") +
  scale_fill_manual(values = lor_combined$color, name = "Bezirke (ID: Name)") +
  theme_void() +
  labs(title = "Percentage of Airbnbs by Bezirk in Berlin") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))

ggplot(region_data, aes(x = reorder(neighbourhood_group_cleansed, num_listings), 
                        y = num_listings, fill = color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() + 
  coord_flip() +
  labs(title = "Number of Airbnbs by Neighborhood",
       x = "Neighborhood",
       y = "Number of Airbnbs") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# ---- END OF VISUALIZATION: map reviews in region by number -----

# --- START OF Visualization: Correlation between number of airbnbs and sentiment ---
plot_correlation <- function(data, x_var, y_var) {
  cor_spearman <- cor(data[[x_var]], data[[y_var]], method = "spearman")
  cor_pearson <- cor(data[[x_var]], data[[y_var]], method = "pearson")
  
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(
      title = paste0("Correlation between ", x_var, " and ", y_var, "\nPearson: ", round(cor_pearson, 2), " Spearman: ", round(cor_spearman, 2)),
      x = paste0(x_var, "-value"),
      y = paste0(y_var, "-value")
    ) +
    theme_minimal()
}

plot_correlation(region_data, "num_listings", "sentiment_region_score")

# ---- END OF VISUALIZATION: Correlation between number of airbnbs and sentiment -----




# ---- START OF INTERACTIVE TEST STUFF ----

str(df_review_sentiments)


points <- df_airbnb[, c("longitude", "latitude")] |> 
  as.matrix() |> 
  st_multipoint() |> 
  st_sfc(crs = 4326) |> 
  st_cast('POINT')

# Create the ggplot
p <- lor %>% 
  ggplot() + 
  geom_sf(
    mapping = aes(geometry = geometry, fill = BEZ_ID, text = BEZ_NAME) # Add text for region names
  ) +
  geom_sf(
    data = points,
    aes(geometry = geometry),
    size = 1,
    color = "black",
    alpha = .3
  ) +
  # geom_sf(
  #   data = opnv, mapping = aes(geometry = geometry), color = "red") +
  theme_bw()

# Convert the ggplot to a plotly object
ggplotly(p, tooltip = "text")

# Test shiny app

library(shiny)
library(plotly)
library(ggplot2)
library(sf)
library(dplyr)

ui <- fluidPage(
  titlePanel("Airbnb Prices in Berlin"),
  sidebarLayout(
    sidebarPanel(
      plotlyOutput("sampleplot")
    ),
    mainPanel(
      plotlyOutput("map")
    )
  )
)

server <- function(input, output, session) {
  
  # Prepare the points
  points <- airbnb[, c("longitude", "latitude")] |> 
    as.matrix() |> 
    st_multipoint() |> 
    st_sfc(crs = 4326) |> 
    st_cast('POINT') 
  
  # Create the map
  output$map <- renderPlotly({
    p <- lor %>%
      ggplot() + 
      geom_sf(
        mapping = aes(geometry = geometry, fill = BEZ_ID, text = BEZ_NAME) # Add text for region names
      ) +
      geom_sf(
        data = points,
        aes(geometry = geometry),
        size = 1,
        color = "black",
        alpha = .3
      ) +
      theme_bw()
    
    ggplotly(p, tooltip = "text") %>%
      event_register("plotly_click")
  })
  
  # Create the sample plot
  output$sampleplot <- renderPlotly({
    event_data <- event_data("plotly_click")
    
    if (is.null(event_data)) return(NULL)
    
    selected_bezirk <- event_data$text[1]
    
    # Generate a random sample plot
    set.seed(123)  # Ensure reproducibility
    sample_data <- data.frame(
      x = rnorm(100),
      y = rnorm(100)
    )
    
    sample_plot <- ggplot(sample_data, aes(x = x, y = y)) +
      geom_point() +
      theme_minimal() +
      labs(title = paste("Random Sample Plot for", selected_bezirk))
    
    ggplotly(sample_plot)
  })
}

shinyApp(ui, server)

