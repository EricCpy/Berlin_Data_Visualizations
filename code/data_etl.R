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
  as_tibble()

df_review_sentiments_aggregated <- df_review_sentiments %>% 
  mutate(sentiment_score = positive_sentiment-negative_sentiment) %>% 
  group_by(id) %>% 
  summarise(median_sentiment_score_for_reviews = median(sentiment_score), mean_sentiment_score_for_reviews = mean(sentiment_score))

df_listings_cleaned_with_review_sentiment <- df_listings_cleaned %>% inner_join(
  df_review_sentiments_aggregated, by = c("id" = "id")
)

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

lor_inhabitants <- read_csv2("data/lor/einwohner_lor_2020-12.csv", col_types = "c") %>% 
  select(-c(ZEIT, BEZ, PGR, BZR, PLR, STADTRAUM))
lor_foreigner <- read_csv2("data/lor/einwohner_foreign_lor_2020-12.csv", col_types = "c") %>% 
  select(-c(ZEIT, BEZ, PGR, BZR, PLR, STADTRAUM))
lor_migration_bg <- read_csv2("data/lor/einwohner_migrationbackground_lor_2020-12.csv", col_types = "c") %>% 
  select(-c(ZEIT, BEZ, PGR, BZR, PLR, STADTRAUM))

##### election units #####

party_votes <- read_csv2("data/Wahlergebnisse Berlin 2016/Berlin_AH16_W2.csv", col_types = "c")

#### geodata ####

##### air bnbs #####

airbnb_coordinates <- df_listings_cleaned_with_review_sentiment[, c("longitude", "latitude")] |> 
  as.matrix() |> 
  st_multipoint() |> 
  st_sfc(crs = 4326) |> 
  st_cast('POINT') 

##### election units #####

geo_elect_units <- sf::st_read(dsn = "data/Wahlbezirke") %>% 
  st_make_valid(tol = 0.00001) %>% st_transform(4326) %>% 
  mutate(
    BEZNAME = str_replace_all(BEZNAME, "\xf6", "ö"),
    BWB = str_c(str_sub(BWB, 1, 2), "B", str_sub(BWB, 3, 4))
  )

##### LOR #####

geo_lor <- sf::st_read(dsn = "data/lor/Planung") %>% 
  st_make_valid(tol = 0.00001) %>% st_transform(4326) %>% 
  mutate(
    BEZ_ID = str_sub(PLR_ID, 1, 2),
    PGR_ID = str_sub(PLR_ID, 1, 4),
    BZR_ID = str_sub(PLR_ID, 1, 6)
  ) %>% 
  left_join(bezirk_name_id, by = c("BEZ_ID" = "BEZ_ID"))

##### Bezirke #####

geo_bezirk <- geo_lor %>% aggregate(list(BEZ_NAME=geo_lor$BEZ_NAME), FUN=function(x) 1)

##### opnv #####

opnv_rails <- sf::st_read(dsn = "data/OPNV/Strecken/") %>% 
  st_make_valid(tol = 0.00001) %>% st_transform(4326)
opnv_stations <- sf::st_read(dsn = "data/OPNV/Stationen/") %>% 
  st_make_valid(tol = 0.00001) %>% st_transform(4326)

# ---- REGRESSION ----

df_airbnb <- df_listings_cleaned_with_review_sentiment %>% 
  select(
    id, 
    # name, description, neighborhood_overview,
    host_id, host_since, host_response_time, host_is_superhost,
    price, log_price,
    neighbourhood_group_cleansed, neighbourhood_cleansed,
    latitude, longitude,
    property_type,
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
