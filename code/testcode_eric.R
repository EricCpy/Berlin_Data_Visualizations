source("code/setup.R")

# Sentiment Analysis

# perhaps use piechart and compare to filtered listings 
ggplot(df_listings, aes(x = sentiment_for_name)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Sentiment Count", x = "Sentiment", y = "Count") +
  theme_minimal()


df_melted <- melt(df_listings, id.vars = c('id', 'sentiment_for_name'), 
                  measure.vars = c('positive_sentiment_for_name', 'neutral_sentiment_for_name', 'negative_sentiment_for_name'), 
                  variable.name = 'sentiment_type', value.name = 'percentage')

ggplot(df_melted, aes(x = percentage, fill = sentiment_type)) +
  geom_density(alpha = 0.6) +
  labs(title = "Density of sentiment percentages", x = "Percentage", y = "Density") +
  theme_minimal()


# Sentiment Price Analysis

mean_price <- mean(df_listings_cleaned$price, na.rm = TRUE)
mean_price
sd_price <- sd(df_listings_cleaned$price, na.rm = TRUE)
sd_price
z99 <- qnorm(0.95) # maybe change interval
ci_low <- mean_price - z99 * sd_price / sqrt(nrow(df_listings_cleaned))
ci_high <- mean_price + z99 * sd_price / sqrt(nrow(df_listings_cleaned))

df_filtered <- df_listings_cleaned %>%
  filter(price >= ci_low & price <= ci_high)

ggplot(df_filtered, aes(x = sentiment_for_name, y = price, fill = sentiment_for_name)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Distribution of prices by sentiment", x = "Sentiment", y = "Price") +
  theme_minimal()

ggplot(df_listings_cleaned, aes(x = sentiment_for_name, y = log(price), fill = sentiment_for_name)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Distribution of prices by sentiment", x = "Sentiment", y = "Price") +
  theme_minimal()

# same but for airbnb reviews
# ---- START OF VISUALIZATION: english vs german reviews, are german reviews/expectations lower/higher ----

df_percentage <- df_review_sentiments %>%
  group_by(language, sentiment) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(language) %>%
  mutate(total = sum(count),
         percentage = count / total * 100) %>%
  select(language, sentiment, percentage)

ggplot(df_percentage, aes(x = language, y = percentage, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Sentiments by Language",
       x = "Language",
       y = "Percentage") +
  theme_minimal()

# conclusion: most reviews are positive and germans have more negative reviews, 
# but we cant really compare these results because we used different sentiment analyizers for different languages

# ---- END OF VISUALIZATION: english vs german reviews, are german reviews/expectations lower/higher -----


# review sentiment time analysis, normal plots and animated map with slider for year or month selection
# interactive map price to review sentiment to rating metrics in dataset

# ---- START OF VISUALIZATION: map reviews in region by sentiment -----
library(scales)
library(sf)

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
  scale_fill_manual(values = unique(lor_combined$color), name = "Bezirke (ID: Name)") + # Adjust the name and colors here
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


# test stuff

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

