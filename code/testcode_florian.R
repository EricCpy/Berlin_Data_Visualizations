source("code/setup.R")

airbnb_count <- df_listings_cleaned %>% 
  count(neighbourhood_group_cleansed, sort = TRUE) %>% 
  rename(BEZ_NAME = neighbourhood_group_cleansed)

raw <- sf::st_read(dsn = "data/lor/Planung/lor_plr.shp") %>% 
  st_make_valid() %>% 
  st_transform(4326) %>% 
  mutate(
    BEZ_ID = str_sub(PLR_ID, 1, 2),
    PGR_ID = str_sub(PLR_ID, 1, 4),
    BZR_ID = str_sub(PLR_ID, 1, 6)
  )

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
  "Reinickendorf", "12"
)

raw <- raw %>% 
  left_join(bezirke_name_id, by = c("BEZ_ID" = "BEZ_ID")) %>% 
  left_join(airbnb_count, by = c("BEZ_NAME" = "BEZ_NAME"))

bezirk_colors <- c(
  "Mitte" = "#FF6347",
  "Friedrichshain-Kreuzberg" = "#FFD700",
  "Pankow" = "#ADFF2F",
  "Charlottenburg-Wilmersdorf" = "#1E90FF",
  "Spandau" = "#8A2BE2",
  "Steglitz-Zehlendorf" = "#FF69B4",
  "Tempelhof-Schöneberg" = "#7FFF00",
  "Neukölln" = "#00CED1",
  "Treptow-Köpenick" = "#D2691E",
  "Marzahn-Hellersdorf" = "#FF4500",
  "Lichtenberg" = "#32CD32",
  "Reinickendorf" = "#0000FF"
)

# map of bezirks
ggplot(raw) + 
  geom_sf(aes(fill = BEZ_NAME)) +
  scale_fill_manual(values = bezirk_colors, name = "BEZ_NAME") +
  theme_void() +
  labs(title = "Bezirk in Berlin",
       fill = "Bezirk")

#map with numbers
centroids <- raw %>% 
  group_by(BEZ_NAME) %>% 
  summarise(geometry = st_centroid(st_union(geometry)), n = first(n))

ggplot(raw) + 
  geom_sf(aes(fill = BEZ_NAME), color = NA) +
  geom_sf(data = raw, fill = NA) + 
  geom_text(data = centroids, aes(label = n, geometry = geometry), stat = "sf_coordinates", size = 5, color = "black") +
  scale_fill_manual(values = bezirk_colors, name = "BEZ_NAME") +
  theme_void() +
  labs(title = "Bezirk in Berlin with Number of Airbnbs",
       fill = "Bezirk")

#number of airbnbs on a scale (try log scale)
borough_borders <- raw %>%
  group_by(BEZ_ID) %>%
  summarise(geometry = st_union(geometry))

ggplot(raw) +
  geom_sf(aes(fill = n), color = "black", size = 0.2) +  # Smaller internal borders
  geom_sf(data = borough_borders, fill = NA, color = "black", size = 1) +  # Larger borders for main boroughs
  scale_fill_gradient(low = "lightyellow", high = "darkred", name = "Number of Airbnbs") +
  theme_void() +
  labs(title = "Number of Airbnbs by Bezirk in Berlin",
       fill = "Number of Airbnbs")

#barplot of number of airbnbs by bezirk
ggplot(airbnb_count, aes(x = reorder(BEZ_NAME, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Number of Airbnbs by Bezirk",
       x = "Bezirk",
       y = "Number of Airbnbs") +
  theme_minimal()

#boxplot of prices by bezirk
ggplot(df_listings_cleaned, aes(x = neighbourhood_group_cleansed, y = price)) +
  geom_boxplot(fill = "lightblue") +
  coord_flip() +
  labs(title = "Boxplot of Airbnb Prices by Bezirk",
       x = "Bezirk",
       y = "Price")

#barplot of airbnbs by roomtype
ggplot(df_listings_cleaned, aes(x = room_type)) +
  geom_bar(fill = "green", alpha = 0.7) +
  labs(title = "Number of Airbnbs by Room Type",
       x = "Room Type",
       y = "Number of Listings")

#boxplot of availability by bezirk
ggplot(df_listings_cleaned, aes(x = neighbourhood_group_cleansed, y = availability_365)) +
  geom_boxplot(fill = "lightblue") +
  coord_flip() +
  labs(title = "Boxplot of Availability by Bezirk",
       x = "Bezirk",
       y = "Availability (days per year)") +
  theme_minimal()

#barplot of mean availability by bezirk
availability_rate <- df_listings_cleaned %>%
  group_by(neighbourhood_group_cleansed) %>%
  summarise(avg_availability = mean(availability_365))

ggplot(availability_rate, aes(x = reorder(neighbourhood_group_cleansed, avg_availability), y = avg_availability)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Average Availability Rate by Bezirk",
       x = "Bezirk",
       y = "Average Availability Rate (days)")
