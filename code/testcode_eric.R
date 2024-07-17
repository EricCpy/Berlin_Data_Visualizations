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

mean_price <- mean(df_clean$price, na.rm = TRUE)
mean_price
sd_price <- sd(df_clean$price, na.rm = TRUE)
sd_price
z99 <- qnorm(0.95) # maybe change interval
ci_low <- mean_price - z99 * sd_price / sqrt(nrow(df_clean))
ci_high <- mean_price + z99 * sd_price / sqrt(nrow(df_clean))

df_filtered <- df_clean %>%
  filter(price >= ci_low & price <= ci_high)

ggplot(df_filtered, aes(x = sentiment_for_name, y = price, fill = sentiment_for_name)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Distribution of prices by sentiment", x = "Sentiment", y = "Price") +
  theme_minimal()

ggplot(df_clean, aes(x = sentiment_for_name, y = log(price), fill = sentiment_for_name)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Distribution of prices by sentiment", x = "Sentiment", y = "Price") +
  theme_minimal()

# same but for airbnb reviews
# english vs german reviews, are german reviews/expectations lower/higher
# review sentiment time analysis, normal plots and animated map with slider for year or month selection
# interactive map price to review sentiment to rating metrics in dataset
# map reviews in region by price segments

# test stuff

points <- airbnb[, c("longitude", "latitude")] |> 
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

