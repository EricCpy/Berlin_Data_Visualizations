# march 2024 airbnb, not really worth to analyse earlier reviews, because only unlisted airbnbs are not contained inside the march data
# In my opionion the only really interesting use case is to analyse what types of airbnbs are unlisted, during a time period. 

# ---- LISTINGS ----
df_listings <- read.csv("./data/airbnb/March_2024/listings_detailed.csv")

df_title_sentiments <- read.csv("./data/airbnb/March_2024/sentiments.csv") %>% 
  mutate(sentiment = ordered(sentiment, levels = c("negative", "neutral", "positive"))) %>% 
  rename_at(vars(-id), ~str_c(., "_for_name"))

df_listings <- inner_join(df_listings, df_title_sentiments, by = "id") %>%
  as_tibble() %>% mutate(
    price = as.numeric(str_remove(price, "\\$")),
    log_price = log(price),
    host_since = as.Date(host_since),
    first_review = as.Date(first_review), 
    last_review = as.Date(last_review),
    neighbourhood_group_cleansed = str_replace_all(neighbourhood_group_cleansed, " ", "") %>% 
      str_replace(., "Charlottenburg-Wilm.", "Charlottenburg-Wilmersdorf")
  )

df_listings_cleaned <- df_listings %>%
  filter(!is.na(price))

# ---- REVIEWS ----

df_review_sentiments <- read.csv("./data/airbnb/review_sentiments.csv") %>% 
  select(-id) %>%
  as_tibble()

df_review_sentiments <- inner_join(df_listings_cleaned, df_review_sentiments, by = c("id" = "listing_id")) %>%
  mutate(sentiment_score = positive_sentiment-negative_sentiment) %>%
  as_tibble()

df_review_sentiments_aggregated <- df_review_sentiments %>% 
  group_by(id) %>% 
  summarise(median_sentiment_score_for_reviews = median(sentiment_score), mean_sentiment_score_for_reviews = mean(sentiment_score))

df_listings_cleaned_with_review_sentiment <- df_listings_cleaned %>% inner_join(
  df_review_sentiments_aggregated, by = c("id" = "id")
)

# ---- REGRESSION ----

df_airbnb <- df_listings_cleaned_with_review_sentiment %>% 
  select(
    id, 
    # name, description, neighborhood_overview,
    host_id, host_since, host_response_time, host_is_superhost,
    price, log_price,
    neighbourhood_group_cleansed, neighbourhood_cleansed,
    latitude, longitude,
    room_type,
    minimum_nights, availability_365,
    accommodates, bedrooms, beds,
    bathrooms, bathrooms_text,
    amenities,
    number_of_reviews, number_of_reviews_l30d, reviews_per_month,
    first_review, last_review,
    review_scores_rating, review_scores_accuracy, 
    review_scores_cleanliness, review_scores_checkin,
    review_scores_communication, review_scores_location,
    review_scores_value,
    instant_bookable,
    sentiment_for_name, positive_sentiment_for_name, negative_sentiment_for_name, neutral_sentiment_for_name,
    median_sentiment_score_for_reviews, mean_sentiment_score_for_reviews
  ) %>% 
  mutate(
    id = as.factor(id),
    host_id = as.factor(host_id)
  ) %>% 
  drop_na()

airbnb_amenities <- df_airbnb %>% select(id, amenities) %>% mutate(
  amenities = amenities %>% str_remove_all(., '"|\\[|\\]') %>% str_split(", ")
) %>% rowwise() %>% mutate(
  id = as.character(id),
  kitchen = any(str_detect(amenities, "Kitchen")),
  dishwasher = any(str_detect(amenities, "Dishwasher")),
  oven = any(str_detect(amenities, "Oven")),
  stove = any(str_detect(amenities, "Stove")),
  microwave = any(str_detect(amenities, "Microwave")),
  wineglasses = any(str_detect(amenities, "Wine glasses")),
  freezer = any(str_detect(amenities, "Freezer")),
  refrigerator = any(str_detect(amenities, "Refrigerator")),
  washer = any(str_detect(amenities, "Washer")),
  dryer = any(str_detect(amenities, "Dryer")),
  wifi = any(str_detect(amenities, "(W|w)ifi")),
  TV = any(str_detect(amenities, "TV")),
  workspace = any(str_detect(amenities, "Dedicated workspace")),
  bathtub =  any(str_detect(amenities, "Bathtub")),
  boardgames = any(str_detect(amenities, "Board games")),
  piano = any(str_detect(amenities, "Piano")),
  sauna = any(str_detect(amenities, "(s|S)auna")),
  bedlinens = any(str_detect(amenities, "Bed linens")),
  privateentrance = any(str_detect(amenities, "Private entrance")),
  pets = any(str_detect(amenities, "Pets allowed")),
  balcony = any(str_detect(amenities, "(p|P)atio or balcony")),
  freeparking = any(str_detect(amenities, "Free parking")),
  smoking = any(str_detect(amenities, "Smoking allowed")),
  grill = any(str_detect(amenities, "(G|g)rill")),
) %>% select(-amenities)

df_airbnb <- df_airbnb %>% select(-amenities) %>% 
  left_join(
    airbnb_amenities,
    by = c("id" = "id")
  )

# ---- RENTS -----

df_rents <- read.csv("./data/immo_data.csv") %>%
  mutate(rent_per_sqm = rent/sqm)

# ---- AREAS -----

#### information ####

##### Bezirke #####

bezirk_name_id <- tribble(
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

##### LOR #####

lor_inhabitants <- read_csv2("./data/lor/einwohner_lor_2020-12.csv", col_types = "c") %>% 
  select(-c(ZEIT, BEZ, PGR, BZR, PLR, STADTRAUM))
lor_foreigner <- read_csv2("./data/lor/einwohner_foreign_lor_2020-12.csv", col_types = "c") %>% 
  select(-c(ZEIT, BEZ, PGR, BZR, PLR, STADTRAUM))
lor_migration_bg <- read_csv2("./data/lor/einwohner_migrationbackground_lor_2020-12.csv", col_types = "c") %>% 
  select(-c(ZEIT, BEZ, PGR, BZR, PLR, STADTRAUM))

###### crime rate ######

crime_rate <- readxl::read_excel("./data/Kriminalität Fallzahlen&HZ 2014-2023.xlsx", skip = 4, sheet = "Fallzahlen_2023")

###### gigabit internet ######

gigabit_supply <- rio::import("./data/Berlin_LOR_Versorgungsdaten_Stand_2402.xlsx", skip = 3)

###### traffic accidents ######
traffic_accidents <- rio::import("./data/AfSBBB_BE_LOR_Strasse_Strassenverkehrsunfaelle_2020_Datensatz.csv") %>% 
  mutate(LOR_ab_2021 = str_pad(LOR_ab_2021, 8, "0", side = "left")) %>% group_by(LOR_ab_2021) %>% 
  summarise(count = n()) %>% rename(PLR_ID = LOR_ab_2021)

##### election units #####

election_2016_party_vote <- read_csv2("./data/Wahlergebnisse Berlin 2016/Berlin_AH16_W2.csv")

##### public toilets #####

toilets <- rio::import("./data/berliner-toiletten-standorte.xlsx", skip = 10)
toilet_coordinates <- toilets[, c("Längengrad", "Breitengrad")] |> 
  mutate_all(~str_replace(., ",", ".")) %>% 
  drop_na() %>%
  mutate_all(~if_else(str_sub(., 3, 3) == ".", . , str_c(str_sub(., 1, 2), ".", str_sub(., 3, -1)))) %>% 
  mutate_all(~as.numeric(.)) %>% 
  as.matrix() |> 
  st_multipoint() |> 
  st_sfc(crs = 4326) |> 
  st_cast('POINT')

##### wohnlage #####

airbnb_with_rent_data_median_knn <- readRDS("./saved_objects/airbnb_with_rent_data_median_knn.rds") %>% 
  mutate(airbnb_id = as.character(airbnb_id)) %>% 
  right_join(
    df_airbnb %>% select(id),
    by = c("airbnb_id" = "id")
  )

#### geodata ####

##### airBNBs #####

airbnb_coordinates <- df_airbnb[, c("longitude", "latitude")] |> 
  as.matrix() |> 
  st_multipoint() |> 
  st_sfc(crs = 4326) |> 
  st_cast('POINT') 

##### election units #####

geo_elect_units <- sf::st_read(dsn = "./data/Wahlbezirke") %>% 
  st_make_valid(tol = 0.00001) %>% st_transform(4326) %>% 
  mutate(
    BEZNAME = str_replace_all(BEZNAME, "\xf6", "ö"),
    BWB = str_c(str_sub(BWB, 1, 2), "B", str_sub(BWB, 3, 4))
  )

airbnb_election_unit_mapping <- st_intersection(
  geo_elect_units %>% st_transform(9311),
  airbnb_coordinates %>% st_transform(9311)
) %>% st_transform(4326) %>% 
  select(FID_1) %>% 
  bind_cols("airbnb_id" = df_airbnb$id)

election_2016_party_vote_UWB_with_BWB <- election_2016_party_vote %>% filter(Wahlbezirksart == "Urnenwahlbezirk") %>% 
  mutate(Adresse = str_remove(Adresse, "W")) %>% 
  left_join(
    geo_elect_units %>% as.data.frame() %>% select(UWB, BWB),
    by = c("Adresse" = "UWB")
  )

election_results_summed_for_BWB <- election_2016_party_vote_UWB_with_BWB %>% group_by(BWB) %>% 
  select_if(is.numeric) %>% select(-Bundestagswahlkreis) %>% 
  summarise_all(sum) %>% bind_rows(
    election_2016_party_vote %>% filter(Wahlbezirksart == "Briefwahlbezirk") %>% 
      rename(BWB = Adresse) %>% group_by(BWB) %>% 
      select_if(is.numeric) %>% select(-Bundestagswahlkreis) 
  ) %>% group_by(BWB) %>% summarise_all(sum)

##### LOR #####

geo_lor <- sf::st_read(dsn = "./data/lor/Planung") %>% 
  st_make_valid(tol = 0.00001) %>% st_transform(4326) %>% 
  mutate(
    BEZ_ID = str_sub(PLR_ID, 1, 2),
    PGR_ID = str_sub(PLR_ID, 1, 4),
    BZR_ID = str_sub(PLR_ID, 1, 6)
  ) %>% 
  left_join(bezirk_name_id, by = c("BEZ_ID" = "BEZ_ID"))

airbnb_lor_mapping <- st_intersection(
  geo_lor %>% st_transform(9311),
  airbnb_coordinates %>% st_transform(9311)
) %>% st_transform(4326) %>% 
  select(PLR_ID) %>% 
  bind_cols("airbnb_id" = df_airbnb$id)

##### Bezirke #####

geo_bezirk <- geo_lor %>% aggregate(
  list(BEZ_NAME=geo_lor$BEZ_NAME), FUN=function(x) 1
  )

##### opnv #####

opnv_rails <- sf::st_read(dsn = "./data/OPNV/Strecken/") %>% 
  st_make_valid(tol = 0.00001) %>% st_transform(4326)
opnv_stations <- sf::st_read(dsn = "./data/OPNV/Stationen/") %>% 
  st_make_valid(tol = 0.00001) %>% st_transform(4326)

opnv_rails_no_tram_berlin <- opnv_rails %>% 
  filter(Bahn_Typ_k != "T") %>% 
  st_crop(
    xmin = 13.08, ymin = 52.32,
    xmax = 13.75, ymax = 52.69
  ) %>% st_sf() %>% st_cast()
opnv_stations_no_tram_berlin <- opnv_stations %>% 
  filter(Bahn_Typ_k != "T") %>% 
  st_crop(
    xmin = 13.08, ymin = 52.32,
    xmax = 13.75, ymax = 52.69
  )

# # ---- DISTANCES ----

#### OPNV ####

# airbnb_opnv_distances <- st_distance(opnv_stations_no_tram_berlin, airbnb_coordinates)
# saveRDS(airbnb_opnv_distances, "saved_objects/airbnb_opnv_distances.rds")

airbnb_opnv_distances <- readRDS("./saved_objects/airbnb_opnv_distances.rds")
df_airbnb <- df_airbnb %>% bind_cols(
  distance_opnv = apply(airbnb_opnv_distances, 2, min)
  )

#### toilets ####

# airbnb_toilet_distances <- st_distance(toilet_coordinates, airbnb_coordinates)
# saveRDS(airbnb_toilet_distances, "saved_objects/airbnb_toilet_distances.rds")

airbnb_toilet_distances <- readRDS("./saved_objects/airbnb_toilet_distances.rds")
df_airbnb <- df_airbnb %>% bind_cols(
  distance_toilet = apply(airbnb_toilet_distances, 2, min)
)

#### Hauptbahnhof ####

coords_hauptbahnhof <- tribble(
  ~name, ~lat, ~lon,
  "Hauptbahnhof", 52.52507464195593, 13.369127492553043
) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

df_airbnb <- df_airbnb %>% bind_cols(
  distance_hauptbahnhof = st_distance(coords_hauptbahnhof, airbnb_coordinates) %>% t()
)

#---- combining in df_airbnb2 ----

df_airbnb2 <- df_airbnb %>% left_join(
  airbnb_with_rent_data_median_knn,
  by = c("id"  = "airbnb_id")
) %>% left_join(
  airbnb_lor_mapping,
  by = c("id" = "airbnb_id")
) %>% left_join(
  airbnb_election_unit_mapping,
  by = c("id" = "airbnb_id")
) %>% mutate(BZR_ID = str_sub(PLR_ID, 1, 6)) %>% 
  left_join(
  crime_rate %>% rename(
    LOR_ID = `LOR-Schlüssel (Bezirksregion)`,
    Straftaten_total = `Straftaten \r\n-insgesamt-`
    ),
  by = c("BZR_ID" = "LOR_ID")
) %>% left_join(
  traffic_accidents %>% rename(n_traffic_accidents = count),
  by = c("PLR_ID" = "PLR_ID")
) %>% left_join(
  gigabit_supply %>% transmute(LOR_ID = `LOR\r\nPlanungsräume\r\nNummer`, gigabit_supply_2024 = `Anteil (%)...6`),
  by = c("PLR_ID" = "LOR_ID")
)

bezirk_levels <- df_airbnb2 %>% 
  transmute(bezirk = as.factor(neighbourhood_group_cleansed)) %>% 
  pull() %>% levels()

property_type_levels <- df_airbnb2 %>%
  transmute(property_type = as.factor(room_type)) %>%
  pull() %>% levels()

direct_effect_only <- c(
  "bE", "bAccommodates", "bBedrooms", "bKitchen", "bDistTrainstation", "bGigabit", 
  "bDishwasher", "bStove", "bMicrowave", "bWineglasses", "bFreezer", "bRefrigerator", "bWasher", "bDryer"
)

# ---- VISUALS ----

bezirk_colors <- c(
  "Charlottenburg-Wilmersdorf" = "#a6cee3",
  "Friedrichshain-Kreuzberg" = "#1f78b4",
  "Lichtenberg" = "#b2df8a",
  "Marzahn-Hellersdorf" = "#33a02c",
  "Mitte" = "#fb9a99",
  "Neukölln" = "#e31a1c",
  "Pankow" = "#fdbf6f",
  "Reinickendorf" = "#ff7f00",
  "Spandau" = "#cab2d6",
  "Steglitz-Zehlendorf" = "#6a3d9a",
  "Tempelhof-Schöneberg" = "#ffff99",
  "Treptow-Köpenick" = "#b15928"
)

# ---- DAGs ----

full_dag <- dagitty::dagitty('dag {
bb="0,0,1,1"
... [pos="0.810,0.447"]
TV [pos="0.827,0.497"]
accommodates [pos="0.206,0.350"]
amenities [pos="0.648,0.515"]
bathrooms [pos="0.513,0.407"]
bedrooms [pos="0.434,0.377"]
beds [pos="0.329,0.350"]
crime_rate [pos="0.137,0.819"]
distance_hauptbahnhof [pos="0.614,0.729"]
distance_opnv [pos="0.524,0.785"]
distance_toilets [pos="0.419,0.811"]
gigbit_supply [pos="0.753,0.631"]
kitchen [pos="0.600,0.455"]
oven [pos="0.738,0.433"]
price [outcome,pos="0.249,0.580"]
property_type [pos="0.119,0.410"]
reputation [exposure,pos="0.047,0.581"]
reviews [pos="0.137,0.523"]
traffic_accidents [pos="0.268,0.830"]
unobserved [latent,pos="0.858,0.594"]
wifi [pos="0.771,0.532"]
wohnlage [pos="0.147,0.628"]
... -> amenities
TV -> amenities
accommodates -> beds
accommodates -> price
amenities -> price
bathrooms -> price
bedrooms -> beds
bedrooms -> price
beds -> price
crime_rate -> reputation
distance_hauptbahnhof -> price
distance_opnv -> distance_hauptbahnhof
distance_opnv -> price
distance_toilets -> price
gigbit_supply -> price
gigbit_supply -> wifi
kitchen -> oven
kitchen -> price
oven -> amenities
price -> reviews
property_type -> price
reputation -> price
reputation -> reviews
traffic_accidents -> price
unobserved -> price
wifi -> amenities
wohnlage -> crime_rate
wohnlage -> price
wohnlage -> reputation
}')

full_dag_adjusted <- dagitty::dagitty('dag {
bb="0,0,1,1"
... [adjusted,pos="0.810,0.447"]
TV [adjusted,pos="0.827,0.497"]
accommodates [adjusted,pos="0.206,0.350"]
amenities [adjusted,pos="0.648,0.515"]
bathrooms [adjusted,pos="0.513,0.407"]
bedrooms [adjusted,pos="0.434,0.377"]
beds [adjusted,pos="0.329,0.350"]
crime_rate [pos="0.137,0.819"]
distance_hauptbahnhof [adjusted,pos="0.614,0.729"]
distance_opnv [adjusted,pos="0.524,0.785"]
distance_toilets [adjusted,pos="0.419,0.811"]
gigbit_supply [adjusted,pos="0.753,0.631"]
kitchen [adjusted,pos="0.600,0.455"]
oven [adjusted,pos="0.738,0.433"]
price [outcome,pos="0.249,0.580"]
property_type [adjusted,pos="0.119,0.410"]
reputation [exposure,pos="0.047,0.581"]
reviews [pos="0.137,0.523"]
traffic_accidents [adjusted,pos="0.268,0.830"]
unobserved [latent,pos="0.858,0.594"]
wifi [adjusted,pos="0.771,0.532"]
wohnlage [adjusted,pos="0.147,0.628"]
... -> amenities
TV -> amenities
accommodates -> beds
accommodates -> price
amenities -> price
bathrooms -> price
bedrooms -> beds
bedrooms -> price
beds -> price
crime_rate -> reputation
distance_hauptbahnhof -> price
distance_opnv -> distance_hauptbahnhof
distance_opnv -> price
distance_toilets -> price
gigbit_supply -> price
gigbit_supply -> wifi
kitchen -> oven
kitchen -> price
oven -> amenities
price -> reviews
property_type -> price
reputation -> price
reputation -> reviews
traffic_accidents -> price
unobserved -> price
wifi -> amenities
wohnlage -> crime_rate
wohnlage -> price
wohnlage -> reputation
}')

### Basic plots data

airbnb_count_by_neiborhood <- df_listings_cleaned %>% 
  count(neighbourhood_group_cleansed, sort = TRUE) %>% 
  rename(BEZ_NAME = neighbourhood_group_cleansed)

total_airbnbs <- sum(airbnb_count_by_neiborhood$n)

airbnb_count_by_neiborhood <- airbnb_count_by_neiborhood %>%
  mutate(proportion = n / total_airbnbs)

raw <- sf::st_read(dsn = "data/lor/Planung/lor_plr.shp") %>% 
  st_make_valid() %>% 
  st_transform(4326) %>% 
  mutate(
    BEZ_ID = str_sub(PLR_ID, 1, 2),
    PGR_ID = str_sub(PLR_ID, 1, 4),
    BZR_ID = str_sub(PLR_ID, 1, 6)
  )

airbnb_sf <- sf::st_as_sf(df_listings_cleaned, coords = c("longitude", "latitude"), crs = st_crs(raw))

# airbnb_count_by_LOR <- raw %>%
#   st_join(airbnb_sf, join = sf::st_intersects) %>%
#   group_by(PLR_ID) %>% # Replace 'region_id' with the actual column name of your region IDs
#   summarise(n = n()) %>% 
#   st_drop_geometry() %>%
#   select(PLR_ID, n)
# saveRDS(airbnb_count_by_LOR, "saved_objects/airbnb_count_by_LOR.rds")

airbnb_count_by_LOR <- readRDS("saved_objects/airbnb_count_by_LOR.rds")

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
  left_join(bezirke_name_id, by = c("BEZ_ID" = "BEZ_ID"))

sf_airbnb_by_neighborhood <- raw %>% 
  left_join(airbnb_count_by_neiborhood, by = c("BEZ_NAME" = "BEZ_NAME"))

sf_airbnb_by_LOR <- raw %>% 
  left_join(airbnb_count_by_LOR, by = c("PLR_ID" = "PLR_ID"))

# bezirk_colors <- c(
#   "Mitte" = "#FF6347",
#   "Friedrichshain-Kreuzberg" = "#FFD700",
#   "Pankow" = "#ADFF2F",
#   "Charlottenburg-Wilmersdorf" = "#1E90FF",
#   "Spandau" = "#8A2BE2",
#   "Steglitz-Zehlendorf" = "#FF69B4",
#   "Tempelhof-Schöneberg" = "#7FFF00",
#   "Neukölln" = "#00CED1",
#   "Treptow-Köpenick" = "#D2691E",
#   "Marzahn-Hellersdorf" = "#FF4500",
#   "Lichtenberg" = "#32CD32",
#   "Reinickendorf" = "#0000FF"
# )

#map with numbers
centroids <- sf_airbnb_by_neighborhood %>% 
  group_by(BEZ_NAME) %>% 
  summarise(geometry = st_centroid(st_union(geometry)), n = first(n))

#number of airbnbs on a scale (try log scale)
borough_borders <- sf_airbnb_by_neighborhood %>%
  group_by(BEZ_ID) %>%
  summarise(geometry = st_union(geometry))

#barplot of mean availability by bezirk
availability_rate <- df_listings_cleaned %>%
  group_by(neighbourhood_group_cleansed) %>%
  summarise(avg_availability = mean(availability_365))

#rents
rent_sf <- sf::st_as_sf(df_rents, coords = c("lng", "lat"), crs = st_crs(raw))

rents_by_LOR <- raw %>%
  st_join(rent_sf, join = sf::st_intersects) %>%
  group_by(PLR_ID) %>% # Replace 'region_id' with the actual column name of your region IDs
  summarise(n_offers = n(), mean_rent = mean(rent), mean_rent_per_sqm = mean(rent_per_sqm)) %>% 
  st_drop_geometry() %>%
  select(PLR_ID, n_offers, mean_rent, mean_rent_per_sqm)

sf_rent_by_LOR <- raw %>% 
  left_join(rents_by_LOR, by = c("PLR_ID" = "PLR_ID"))



rents_by_LOR_Q1 <- quantile(rents_by_LOR[['mean_rent_per_sqm']], 0.25, na.rm = TRUE)
rents_by_LOR_Q3 <- quantile(rents_by_LOR[['mean_rent_per_sqm']], 0.75, na.rm = TRUE)
rents_by_LOR_IQR <- rents_by_LOR_Q3 - rents_by_LOR_Q1
rents_by_LOR_lower_bound <- rents_by_LOR_Q1 - 1.5 * rents_by_LOR_IQR
rents_by_LOR_upper_bound <- rents_by_LOR_Q3 + 1.5 * rents_by_LOR_IQR
cleaned_rents_by_LOR = rents_by_LOR %>%
  filter(rents_by_LOR[['mean_rent_per_sqm']] >= rents_by_LOR_lower_bound & rents_by_LOR[['mean_rent_per_sqm']] <= rents_by_LOR_upper_bound)

sf_cleaned_rent_by_LOR <- raw %>% 
  left_join(cleaned_rents_by_LOR, by = c("PLR_ID" = "PLR_ID"))

rent_and_airbnb = airbnb_count_by_LOR %>% 
  left_join(cleaned_rents_by_LOR, by = c("PLR_ID" = "PLR_ID"))
