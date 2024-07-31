source("code/setup.R")

#### Data loading ####

elect_units <- sf::st_read(dsn = "data/Wahlbezirke") %>% 
  st_make_valid(tol = 0.00001) %>% st_transform(4326) %>% 
  mutate(
    BEZNAME = str_replace_all(BEZNAME, "\xf6", "ö"),
    BWB = str_c(str_sub(BWB, 1, 2), "B", str_sub(BWB, 3, 4))
    )
table(st_is_valid(elect_units))
crs <- st_crs(elect_units)

zweitstimme <- read_csv2("data/Wahlergebnisse Berlin 2016/Berlin_AH16_W2.csv", col_types = "c")

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

lor_inhabitants <- read_csv2("data/lor/einwohner_lor_2020-12.csv", col_types = "c") %>% 
  select(-c(ZEIT, BEZ, PGR, BZR, PLR, STADTRAUM))
lor_foreigner <- read_csv2("data/lor/einwohner_foreign_lor_2020-12.csv", col_types = "c") %>% 
  select(-c(ZEIT, BEZ, PGR, BZR, PLR, STADTRAUM))
lor_migration_bg <- read_csv2("data/lor/einwohner_migrationbackground_lor_2020-12.csv", col_types = "c") %>% 
  select(-c(ZEIT, BEZ, PGR, BZR, PLR, STADTRAUM))

lor <- sf::st_read(dsn = "data/lor/Planung") %>% 
  st_make_valid(tol = 0.00001) %>% st_transform(4326) %>% 
  mutate(
    BEZ_ID = str_sub(PLR_ID, 1, 2),
    PGR_ID = str_sub(PLR_ID, 1, 4),
    BZR_ID = str_sub(PLR_ID, 1, 6)
    ) %>% 
  left_join(bezirke_name_id, by = c("BEZ_ID" = "BEZ_ID"))

bezirke_geometry <- lor %>% aggregate(list(BEZ_NAME=lor$BEZ_NAME), FUN=function(x) 1)

airbnb <- rio::import("data/airbnb/March_2024/listings.csv") %>% 
  as_tibble() %>% mutate(
    neighbourhood_group = str_replace_all(neighbourhood_group, " ", "") %>% 
      str_replace(., "Charlottenburg-Wilm.", "Charlottenburg-Wilmersdorf")
  )

airbnb_complete <- rio::import("data/airbnb/March_2024/listings_detailed.csv") %>% 
  as_tibble()

airbnb_regression <- airbnb %>% select(
  id, host_id, neighbourhood_group, neighbourhood, room_type, price, minimum_nights, number_of_reviews, availability_365, reviews_per_month
  ) %>% left_join(
    airbnb_complete %>% select(
      id, accommodates, bathrooms, bedrooms, beds
      ), by = c("id" = "id")
  ) %>% mutate(
    id = as.factor(id),
    host_id = as.factor(host_id)
  ) %>% drop_na()

#### regression ####

airbnb_regression %>% group_by(neighbourhood) %>% summarise(count = n()) %>% arrange(count)

lm1 <- lm(data = airbnb_regression, price ~ accommodates)
summary(lm1)
plot(lm1)

lm2 <- lm(data = airbnb_regression, price ~ accommodates + neighbourhood_group)

lm3 <- lm(data = airbnb_regression, price ~ . - id - host_id - neighbourhood)
summary(lm3)

lm_full <- lm(data = airbnb_regression, price ~ . - id - host_id)
plot(lm_full)

anova(lm1, lm2, lm3, lm_full)

#### bayesian regression ####

library(brms)

blm1 <- brm(
  data = airbnb_regression, price ~ accommodates,
  file = "models/blm1"
  )

blm3 <- brm(
  data = airbnb_regression, price ~ . - id - host_id - neighbourhood,
  file = "models/blm3"
  )
summary(blm3)

loo(blm3, blm1)

blm3_student <- brm(
  data = airbnb_regression, price ~ . - id - host_id - neighbourhood, family = "student",
  file = "models/blm3_student"
  )
summary(blm3_student)

blm3_skew <- brm(
  data = airbnb_regression, price ~ . - id - host_id - neighbourhood, family = "skew_normal",
  file = "models/blm3_skew"
  )
summary(blm3_skew)

blm3_lognormal <- brm(
  data = airbnb_regression, price ~ . - id - host_id - neighbourhood, family = "lognormal",
  file = "models/blm3_lognormal"
  )
summary(blm3_lognormal)

conditional_effects(
  blm3_lognormal,
  effects = "accommodates",
  conditions = tribble(
    ~bedrooms, ~bathrooms,
    1, 1,
    1, 4,
    4, 1,
    4, 4,
  ))

plot(airbnb_regression$minimum_nights, airbnb_regression$price)
plot(airbnb_regression$accommodates, airbnb_regression$price)
hist(airbnb_regression$price)

loo(blm3, blm3_lognormal, blm3_skew, blm3_student)

#### extract flat features ####

airbnb_amenities <- airbnb_complete %>% select(id, amenities) %>% mutate(
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
  )

amenities_ranking <- do.call(c, airbnb_complete$amenities %>% str_remove_all(., '"|\\[|\\]') %>% str_split(", ")) %>% 
  table() %>% as_tibble() %>% setNames(c("amenity", "n")) %>% arrange(desc(n))

amenities_ranking %>% filter(str_detect(amenity, "(w|W)ifi")) %>% mutate(
  WifiSpeed = as.numeric(str_split_i(amenity, " ", -2))
) %>% drop_na() %>% pull(n) %>% sum()

amenities_ranking %>% filter(str_detect(amenity, "HDTV")) %>% pull(n)

airbnb_regression_amenities <-  airbnb_amenities %>% select(-amenities) %>% right_join(airbnb_regression)

lm(
  data = airbnb_regression_amenities,
  formula = price ~ . - id - host_id - neighbourhood
  ) %>% summary()

lm(
  data = airbnb_regression_amenities,
  formula = log(price) ~ . - id - host_id - neighbourhood
) %>% summary()

blm3_lognormal_amenities <- brm(
  data = airbnb_regression_amenities, price ~ . - id - host_id - neighbourhood, family = "lognormal",
  file = "models/blm3_lognormal_amenities"
)
summary(blm3_lognormal_amenities)

loo(blm3_lognormal, blm3_lognormal_amenities)

# p <- st_sfc(st_point(c(13.4181, 52.53471)), crs = 4326) %>%
#   st_transform(9311)

#### displaying all airbnbs ####

points <- airbnb[, c("longitude", "latitude")] |> 
  as.matrix() |> 
  st_multipoint() |> 
  st_sfc(crs = 4326) |> 
  st_cast('POINT') 

# plot(st_combine(elect_units))
# plot(st_combine(opnv))

elect_units %>% 
  ggplot() + 
  geom_sf(
    mapping = aes(geometry = geometry, fill = BEZNAME)
    ) +
  geom_sf(
    data = points,
    aes(geometry = geometry),
    size = 1,
    color = "black",
    alpha = .3) +
  theme_bw()

lor %>% 
  ggplot() + 
  geom_sf(
    mapping = aes(geometry = geometry, fill = BEZ_ID)
  ) +
  geom_sf(
    data = points,
    aes(geometry = geometry),
    size = 1,
    color = "black",
    alpha = .3) +
  # geom_sf(
  #   data = opnv, mapping = aes(geometry = geometry), color = "red") +
  theme_bw()

#### displaying opnv ####

opnv_rails <- sf::st_read(dsn = "data/OPNV/Strecken/") %>% 
  st_make_valid(tol = 0.00001) %>% st_transform(4326)
opnv_stations <- sf::st_read(dsn = "data/OPNV/Stationen/") %>% 
  st_make_valid(tol = 0.00001) %>% st_transform(4326)

bezirke_geometry %>% 
  ggplot() + 
  geom_sf(
    mapping = aes(geometry = geometry, fill = BEZ_NAME)
  ) +
  geom_sf(
    data = opnv_rails, mapping = aes(geometry = geometry, color = Bahn_Typ_k)) +
  geom_sf(
    data = opnv_stations, mapping = aes(geometry = geometry, color = Bahn_Typ_k)
  ) +
  coord_sf(xlim = c(13, 13.8), ylim = c(52.3, 52.7), expand = FALSE) +
  theme_bw()

#### plotly ####

library(plotly)

map_colors_pale <- c(
  "#8dd3c7",
  "#ffffb3",
  "#bebada",
  "#fb8072",
  "#80b1d3",
  "#fdb462",
  "#b3de69",
  "#fccde5",
  "#d9d9d9",
  "#bc80bd",
  "#ccebc5",
  "#ffed6f"
)

map_colors <- c(
  "#a6cee3",
  "#1f78b4",
  "#b2df8a",
  "#33a02c",
  "#fb9a99",
  "#e31a1c",
  "#fdbf6f",
  "#ff7f00",
  "#cab2d6",
  "#6a3d9a",
  "#ffff99",
  "#b15928"
)

price_color <- c(
  "#33a02c",
  "#e31a1c"
)

sf_use_s2(FALSE)

rails_cropped <- opnv_rails %>% 
  st_crop(
    xmin = 13, ymin = 52.3,
    xmax = 13.8, ymax = 52.7
  )

rails_cropped %>% plot()

plot_ly() %>% 
  add_sf(
    data = geo_bezirk, color = ~BEZ_NAME, 
    span = I(0.5), colors = map_colors,
    hoverinfo='skip'
  ) %>% 
  add_sf(
    data = opnv_rails %>% 
      st_crop(
        xmin = 13, ymin = 52.3,
        xmax = 13.8, ymax = 52.7
      ) %>% st_sf() %>% st_cast(),
    name = 'Rail'
  ) %>% 
  add_sf(
    data = opnv_stations %>% 
      st_crop(
        xmin = 13, ymin = 52.3,
        xmax = 13.8, ymax = 52.7
      ),
    name = 'Station'
  ) %>% 
  add_markers(
    data = df_airbnb,
    x = ~longitude,
    y = ~latitude,
    color = I("black"),
    text = ~str_c(price, " €"),
    hoverinfo='text',
    alpha = 0.3,
    name = 'airBNB'
  ) %>% layout(
    legend=list(
      x=0,
      xanchor='left',
      yanchor='bottom',
      orientation='h'
    )
  )

ggplotly(
  ggplot(geo_bezirk) + 
    geom_sf(aes(fill = BEZ_NAME)) +
    geom_sf(
      data = airbnb_coordinates,
      aes(geometry = geometry),
      size = 1,
      color = "black",
      alpha = .3) +
    theme_bw()
) %>% layout(
  legend=list(
    x=0, 
    xanchor='left',
    yanchor='bottom',
    orientation='h'
    )
  )

plot_ly() %>% 
  add_sf(
    data = geo_bezirk,
    span = I(0.5),
    hoverinfo='skip'
  ) %>% 
  add_sf(
    data = opnv_rails_no_tram_berlin,
    name = 'Rail',
    hoverinfo='skip'
  ) %>% 
  add_sf(
    data = opnv_stations_no_tram_berlin,
    name = 'Station',
    hoverinfo='skip'
  ) %>% 
  add_markers(
    data = df_airbnb,
    x = ~longitude,
    y = ~latitude,
    color = ~log(price),
    text = ~str_c(price, " €"),
    hoverinfo='text',
    alpha = 0.3,
    name = 'Airbnb'
  )

#### leaflet ####

library(leaflet)

basemap <- leaflet() %>%
  # add different provider tiles
  addProviderTiles(
    "OpenStreetMap",
    # give the layer a name
    group = "OpenStreetMap"
  )

basemap %>% addMarkers(
  data = airbnb_coordinates
)

#### visualize price of airbnb ####

airbnb_with_elect_units <- st_intersection(
  elect_units %>% st_transform(9311),
  points %>% st_transform(9311)
) %>% st_transform(4326) %>% 
  bind_cols(airbnb)

airbnb_bezirk <- airbnb_with_elect_units %>% as_tibble() %>% group_by(BEZ, BEZNAME) %>% 
  summarise(median = median(price, na.rm = TRUE), MAD = mad(price, na.rm = TRUE), count = n()) 

bezirke_geometry %>% left_join(
  airbnb_bezirk, by = c("BEZ_NAME" = "BEZNAME")
) %>% 
  ggplot() +
  geom_sf(
    mapping = aes(geometry = geometry, fill = median)
  ) +
  geom_sf(
    data = points,
    aes(geometry = geometry),
    size = 1,
    color = "black",
    alpha = .3)

airbnb_ubw <- airbnb_with_elect_units %>% as_tibble() %>% group_by(UWB) %>% 
  summarise(median = median(price, na.rm = TRUE), MAD = mad(price, na.rm = TRUE), count = n()) 

elect_units %>% left_join(
  airbnb_ubw, by = c("UWB" = "UWB")
) %>% 
  ggplot() +
  geom_sf(
    mapping = aes(geometry = geometry, fill = median)
  )

airbnb_lor_unit_mapping <- st_intersection(
  geo_lor %>% st_transform(9311),
  airbnb_coordinates %>% st_transform(9311)
) %>% st_transform(4326) %>% 
  pull(PLR_ID) %>% 
  bind_cols("airbnb_id" = df_airbnb$id)

airbnb_lor <- airbnb_with_lor_units %>% as_tibble() %>% group_by(PLR_ID, PLR_NAME) %>% 
  summarise(median = median(price, na.rm = TRUE), MAD = mad(price, na.rm = TRUE), count = n()) 

lor %>% left_join(
  airbnb_lor, by = c("PLR_ID" = "PLR_ID")
) %>% 
  ggplot() +
  geom_sf(
    mapping = aes(geometry = geometry, fill = median)
  )

lor %>% left_join(
  airbnb_lor, by = c("PLR_ID" = "PLR_ID")
) %>% 
  ggplot() +
  geom_sf(
    mapping = aes(geometry = geometry, fill = MAD)
  )

lor %>% left_join(
  airbnb_lor, by = c("PLR_ID" = "PLR_ID")
) %>% 
  ggplot() +
  geom_sf(
    mapping = aes(geometry = geometry, fill = count)
  )

airbnb_lor_bzr <- airbnb_with_lor_units %>% as_tibble() %>% group_by(BZR_ID) %>% 
  summarise(median = median(price, na.rm = TRUE), MAD = mad(price, na.rm = TRUE), count = n()) 

aggregate(lor, list(BZR_ID=lor$BZR_ID), FUN=function(x) 1) %>% 
  left_join(
    airbnb_lor_bzr, by = c("BZR_ID" = "BZR_ID")
  ) %>% 
  ggplot() +
  geom_sf(
    mapping = aes(geometry = geometry, fill = median)
  )

aggregate(lor, list(BZR_ID=lor$BZR_ID), FUN=function(x) 1) %>% 
  left_join(
    airbnb_lor_bzr, by = c("BZR_ID" = "BZR_ID")
  ) %>% 
  ggplot() +
  geom_sf(
    mapping = aes(geometry = geometry, fill = count)
  )

#### Match LOR and WB ######## Match LOR and Wcount_fields()B ####

# can't be mapped properly because they cut each others areas
ggplot() + 
  geom_sf(
    data = elect_units %>% filter(BEZ=="05") %>% head(20),
    mapping = aes(geometry = geometry, fill = BWB)
  ) +
  geom_sf(
    data = lor %>% filter(BEZ_ID=="05") %>% head(20),
    mapping = aes(geometry = geometry),
    fill = "#FFFFFF00",
    color = "red"
  ) +
  theme_bw()
# 
# lor_uwb_intersection <- st_intersection(
#   elect_units %>% filter(BEZ=="05") %>% st_transform(9311),
#   lor %>% filter(BEZ_ID=="05") %>% st_transform(9311)
# ) %>% st_transform(4326)
# 
# lor %>% filter(BEZ_ID=="05") %>% head(20) %>% 
#   ggplot() + 
#   geom_sf(
#     mapping = aes(geometry = geometry, fill = PLR_ID)
#   ) +
#   theme_bw()
# 
# lor_uwb_intersection %>% head(80) %>% 
#   ggplot() + 
#   geom_sf(
#     mapping = aes(geometry = geometry, fill = PLR_ID)
#   ) +
#   theme_bw()
# 
# lor_bwb_intersection <- st_intersection(
#   aggregate(elect_units, list(BWB=elect_units$BWB), FUN=function(x) 1) %>% st_transform(9311),
#   lor %>% st_transform(9311)
# ) %>% st_transform(4326)
# 
# lor_bwb_intersection %>% filter(BEZ_ID=="05") %>% head(40) %>% 
#   ggplot() + 
#   geom_sf(
#     mapping = aes(geometry = geometry, fill = BWB)
#   ) +
#   theme_bw()

#### Aggregation UWB into BWB works ####

elect_units %>% 
  filter(BEZ=="05") %>% 
  head(160) %>% 
  ggplot() + 
  geom_sf(
    mapping = aes(geometry = geometry, fill = BWB)
  ) +
  theme_bw()

aggregate(elect_units %>% filter(BEZ=="05"), list(BWB=elect_units %>% filter(BEZ=="05") %>% pull(BWB)), FUN=function(x) 1) %>% 
  head(40) %>% 
  ggplot() + 
  geom_sf(
    mapping = aes(geometry = geometry, fill = BWB)
  ) +
  theme_bw()

#### open maps and ggplot ####
# library(OpenStreetMap)
# 
# lat1 <- 52.25; lat2 <- 52.75; lon1 <- 13; lon2 <- 14
# sa_map <- openmap(c(lat2, lon1), c(lat1, lon2), zoom = 10,
#                   type = "osm-german", mergeTiles = TRUE)
# 
# sa_map2 <- openproj(sa_map)
# 
# OpenStreetMap::autoplot.OpenStreetMap(sa_map2) + 
#   xlab("Longitude (°E)") + ylab("Latitude (°N)")

#### sentiment reviews ####

airbnb_reviews <- rio::import("data/airbnb/March_2024/reviews.csv")
airbnb_reviews_2024 <- airbnb_reviews %>% filter(date > as.Date("2024-01-01"))

airbnb_reviews_2024 <- airbnb_reviews_2024 %>% mutate(language = cld3::detect_language(comments))
airbnb_reviews_2024 %>% group_by(language) %>% summarise(count = n()) %>% arrange(desc(count))
airbnb_reviews_2024_main_languages <- airbnb_reviews_2024 %>% filter(language %in% c("en", "de", "fr", "es"))

airbnb_reviews_lang_detection <- airbnb_reviews %>% mutate(language = cld3::detect_language(comments))
airbnb_reviews_lang_detection %>% group_by(language) %>% summarise(count = n()) %>% arrange(desc(count))

# airbnb_reviews_2024_main_languages %>% head() %>% mutate(
#   english_comment = polyglotr::google_translate(comment, target_language = "en", source_language = language)
#   )

comments_fr_to_de <- polyglotr::google_translate(
  airbnb_reviews_2024_main_languages %>% filter(language == "fr") %>% pull(comments), 
  target_language = "de", source_language = "fr"
  )
data.frame(text = comments_fr_to_de %>% unlist(), source_language = "fr", target_language = "de") %>% head(n = 10) %>% rio::export("data/airbnb/March_2024/review_comments_fr_2024.csv")
airbnb_reviews_2024_fr_to_de <- airbnb_reviews_2024_main_languages %>% filter(language == "fr") %>% bind_cols(unlist(comments_fr_to_de))

comments_en_to_de <- polyglotr::google_translate(
  airbnb_reviews_2024_main_languages %>% filter(language == "fr") %>% pull(comments), 
  target_language = "de", source_language = "en"
)
comments_es_to_de <- polyglotr::google_translate(
  airbnb_reviews_2024_main_languages %>% filter(language == "fr") %>% pull(comments), 
  target_language = "de", source_language = "es"
)


airbnb_reviews_2024_en <- airbnb_reviews_2024 %>% filter(language %in% c("en"))
airbnb_reviews_2024_de <- airbnb_reviews_2024 %>% filter(language %in% c("de"))
temp <- airbnb_reviews_lang_detection %>% filter(language %in% c("de"))
rio::export(temp, "data/airbnb/March_2024/reviews_de.csv")

# library(reticulate)
# py_run_file("python_code/sentiment.py")
# # virtualenv_create("nlp_airbnb")
# virtualenv_install("nlp_airbnb", "germansentiment")
# use_virtualenv("nlp_airbnb")
# 
# germansentiment <- import("germansentiment")
# model <- germansentiment$SentimentModel()
# model$predict_sentiment(temp$comments, output_probabilities = TRUE)

airbnb_sentiments_de_2024 <- airbnb_reviews_2024_de %>% left_join(
  rio::import("data/airbnb/March_2024/sentiments_de_2024.csv"),
  by = c("id" = "id")
)
airbnb_sentiments_de <- airbnb_reviews_2024_de %>% left_join(
  rio::import("data/airbnb/March_2024/sentiments_de.csv"),
  by = c("id" = "id")
)

airbnb_with_sentiments_de_2024 <- airbnb_sentiments_de_2024 %>% drop_na() %>% 
  mutate(listing_id = as.character(listing_id), sentiment_score = positive_sentiment - negative_sentiment) %>% 
  group_by(listing_id) %>% summarise(mean_sentiment_score = mean(sentiment_score), count = n()) %>% 
  arrange(desc(count), desc(mean_sentiment_score)) %>% 
  left_join(airbnb_regression, by = c("listing_id" = "id")) 
lm(data = airbnb_with_sentiments_de_2024, formula = log(price) ~ mean_sentiment_score) %>% summary()
airbnb_with_sentiments_de_2024 %>% ggplot() +
  geom_point(aes(x = mean_sentiment_score, y = price))

# todo:
# - translate to common language? GoogleAPI translation faster than HuggingFace translation but still not as fast as needed to handle whole dataset - translations seem to be identical

##### 5 results from cluster ######

sentiment_results <- rio::import("data/airbnb/review_sentiments.csv")
sentiment_results %>% group_by(language) %>% summarise(count = n()) %>% arrange(desc(count))
airbnb_reviews_with_sentiment <- airbnb_reviews %>% left_join(
  sentiment_results, by = c("id" = "id", "listing_id" = "listing_id", "date" = "date")
)

airbnb_with_agregated_sentiment_scores <- airbnb_reviews_with_sentiment %>% # filter(language %in% c("de", "en")) %>% 
  drop_na() %>% 
  mutate(listing_id = as.character(listing_id), sentiment_score = positive_sentiment - negative_sentiment) %>% 
  group_by(listing_id, language) %>% 
  summarise(mean_sentiment_score = mean(sentiment_score), count = n()) %>% 
  # arrange(desc(count), desc(mean_sentiment_score)) %>% 
  left_join(airbnb_regression, by = c("listing_id" = "id")) 

lm(data = airbnb_with_agregated_sentiment_scores %>% filter(language %in% c("de")), formula = log(price) ~ mean_sentiment_score) %>% summary()
lm(data = airbnb_with_agregated_sentiment_scores %>% filter(language %in% c("de", "en")), formula = log(price) ~ mean_sentiment_score + language) %>% summary()
lm(data = airbnb_with_agregated_sentiment_scores %>% filter(language %in% c("de", "en")), formula = log(price) ~ mean_sentiment_score:language) %>% summary()
lm(data = airbnb_with_agregated_sentiment_scores %>% filter(language %in% c("de", "en")), formula = log(price) ~ mean_sentiment_score*language) %>% summary()

lm_sentiment1 <- lm(data = airbnb_with_agregated_sentiment_scores %>% filter(language %in% c("de", "en", "fr", "es", "nl", "it", "ru")), formula = log(price) ~ mean_sentiment_score:language)
lm_sentiment2 <- lm(data = airbnb_with_agregated_sentiment_scores %>% filter(language %in% c("de", "en", "fr", "es", "nl", "it", "ru")), formula = log(price) ~ mean_sentiment_score*language)

summary(lm_sentiment1)
summary(lm_sentiment2)

anova(lm_sentiment1, lm_sentiment2)

#### crime rate ####

crime_rates <- readxl::read_excel("data/Kriminalität Fallzahlen&HZ 2014-2023.xlsx", skip = 4, sheet = "Fallzahlen_2023")
lor_with_crime <-lor %>% left_join(
  crime_rates, by = c("PLR_ID" = "LOR-Schlüssel (Bezirksregion)")
)

# airbnb_regression_with_crime <- airbnb_regression %>% left_join(
#   crime_rates, by = c("neighbourhood_group" = "Bezeichnung (Bezirksregion)")
# )

airbnb_with_lor_units_with_crime <- airbnb_with_lor_units %>% left_join(
  crime_rates, by = c("BZR_ID" = "LOR-Schlüssel (Bezirksregion)")
)

lm_region <- lm(log(price) ~ neighbourhood_group, data = airbnb_with_lor_units_with_crime)
# lm_crime1 <- lm(log(price) ~ Kieztaten, data = airbnb_regression_with_crime)
# lm_crime2 <- lm(log(price) ~ neighbourhood_group + Kieztaten, data = airbnb_regression_with_crime) # double specified crime and bezirk

lm_crime1 <- lm(log(price) ~ `Straftaten \r\n-insgesamt-`, data = airbnb_with_lor_units_with_crime)
lm_crime2 <- lm(log(price) ~ neighbourhood_group + Kieztaten, data = airbnb_with_lor_units_with_crime)
lm_crime3 <- lm(
  log(price) ~ ., 
  data = airbnb_with_lor_units_with_crime %>% as_tibble() %>% select(price, Raub:Kieztaten)
  )

summary(lm_region)
summary(lm_crime1)
summary(lm_crime2)
summary(lm_crime3)

anova(lm_region, lm_crime1, lm_crime2, lm_crime3)

#### öpnv station distance ####

airbnb_opnv_distances <- st_distance(opnv_stations, airbnb_with_lor_units)

airbnb_with_lor_units_with_opnv_dist <- airbnb_with_lor_units %>% bind_cols(min_dist_opnv = apply(airbnb_opnv_distances, 2, min))

lm_opnv1 <- lm(log(price) ~ min_dist_opnv, data = airbnb_with_lor_units_with_opnv_dist)
summary(lm_opnv1)

anova(lm_region, lm_opnv1)

#### public toilets distance ####

toilets <- rio::import("data/berliner-toiletten-standorte.xlsx", skip = 10)
toilet_coords <- toilets[, c("Längengrad", "Breitengrad")] |> 
  mutate_all(~str_replace(., ",", ".")) %>% 
  drop_na() %>%
  mutate_all(~if_else(str_sub(., 3, 3) == ".", . , str_c(str_sub(., 1, 2), ".", str_sub(., 3, -1)))) %>% 
  mutate_all(~as.numeric(.)) %>% 
  as.matrix() |> 
  st_multipoint() |> 
  st_sfc(crs = 4326) |> 
  st_cast('POINT') 

airbnb_toilet_distances <- st_distance(toilet_coords, airbnb_with_lor_units)

airbnb_with_lor_units_with_toilet_dist <- airbnb_with_lor_units %>% bind_cols(min_dist_wc = apply(airbnb_toilet_distances, 2, min))

lm_wc1 <- lm(log(price) ~ min_dist_wc, data = airbnb_with_lor_units_with_toilet_dist)
summary(lm_wc1)

lm_base <- lm(log(price) ~ 1, data = airbnb)

anova(lm_base, lm_region, lm_opnv1, lm_wc1)

#### gigabit internet ####

gigabit_supply <- rio::import("data/Berlin_LOR_Versorgungsdaten_Stand_2402.xlsx", skip = 3)

airbnb_with_lor_units_with_gigabit <- airbnb_with_lor_units %>% left_join(
  gigabit_supply, by = c("PLR_ID" = "LOR\r\nPlanungsräume\r\nNummer")
)

lm_gigabit1 <- lm(log(price) ~ `Anteil (%)...6`, data = airbnb_with_lor_units_with_gigabit)
summary(lm_gigabit1)

anova(lm_base, lm_region, lm_opnv1, lm_wc1, lm_gigabit1)

#### verkehrunfälle ####

traffic_accidents <- rio::import("data/AfSBBB_BE_LOR_Strasse_Strassenverkehrsunfaelle_2020_Datensatz.csv") %>% 
  mutate(LOR_ab_2021 = str_pad(LOR_ab_2021, 8, "0", side = "left")) %>% group_by(LOR_ab_2021) %>% 
  summarise(count = n()) %>% rename(PLR_ID = LOR_ab_2021)

airbnb_with_lor_units_with_traffic_accidents <- airbnb_with_lor_units %>% left_join(
  traffic_accidents, by = c("PLR_ID" = "LOR_ab_2021")
) %>% mutate(count = if_else(is.na(count), 0, count))

lm_accidents1 <- lm(log(price) ~ count, data = airbnb_with_lor_units_with_traffic_accidents)
summary(lm_accidents1)

anova(lm_base, lm_region, lm_opnv1, lm_wc1, lm_gigabit1, lm_accidents1)
anova(lm_base, lm_accidents1, lm_crime1)

#### election results 2016 ####

election_2016_party_vote <- read_csv2("data/Wahlergebnisse Berlin 2016/Berlin_AH16_W2.csv")

election_2016_party_vote_UWB_with_BWB <- election_2016_party_vote %>% filter(Wahlbezirksart == "Urnenwahlbezirk") %>% 
  mutate(Adresse = str_remove(Adresse, "W")) %>% 
  left_join(
  elect_units %>% as.data.frame() %>% select(UWB, BWB),
  by = c("Adresse" = "UWB")
)

election_results_summed_for_BWB <- election_2016_party_vote_UWB_with_BWB %>% group_by(BWB) %>% 
  select_if(is.numeric) %>% select(-Bundestagswahlkreis) %>% 
  summarise_all(sum) %>% bind_rows(
    election_2016_party_vote %>% filter(Wahlbezirksart == "Briefwahlbezirk") %>% 
      rename(BWB = Adresse) %>% group_by(BWB) %>% 
      select_if(is.numeric) %>% select(-Bundestagswahlkreis) 
  ) %>% group_by(BWB) %>% summarise_all(sum)

airbnb_with_elect_results <- airbnb_with_elect_units %>% left_join(
  election_results_summed_for_BWB %>% select(BWB, Wähler, SPD:Gesundheitsforschung) %>%
    mutate_at(vars(SPD:Gesundheitsforschung), ~./Wähler*100),
  by = c("BWB" = "BWB")
)

lm_spd1 <- lm(log(price) ~ SPD, data = airbnb_with_elect_results)
summary(lm_spd1)

lm_cdu1 <- lm(log(price) ~ CDU, data = airbnb_with_elect_results)
lm_fdp1 <- lm(log(price) ~ FDP, data = airbnb_with_elect_results)
lm_grüne1 <- lm(log(price) ~ GRÜNE, data = airbnb_with_elect_results)

anova(lm_base, lm_spd1, lm_cdu1, lm_fdp1, lm_grüne1)

temp <- airbnb_with_elect_results %>% as_tibble() %>% select(SPD:Gesundheitsforschung)
airbnb_with_elect_majority <- airbnb_with_elect_units %>% 
  bind_cols(
    vote_majority = colnames(temp)[apply(temp,1,which.max)],
    vote_majority2 = colnames(temp)[apply(temp,1,x_highest,2)]
    )

x_highest <- function(data, x) {
  order <- kit::topn(data, n = x)
  return(order[x])
}

lm_votes1 <- lm(log(price) ~ vote_majority, data = airbnb_with_elect_majority)
summary(lm_votes1)

lm_votes2 <- lm(log(price) ~ vote_majority + vote_majority2, data = airbnb_with_elect_majority)
summary(lm_votes2)

lm_votes3 <- lm(log(price) ~ vote_majority*vote_majority2, data = airbnb_with_elect_majority)
summary(lm_votes3)

anova(lm_base, lm_spd1, lm_cdu1, lm_fdp1, lm_grüne1, lm_votes1, lm_votes2, lm_votes3)

# #### rents ####
# 
# rent_data <- sf::st_read(dsn = "data/Mieten/") %>%
#   st_make_valid(tol = 0.00001) %>% st_transform(4326)
# 
# rent_data_with_elect_units <- st_intersection(
#   geo_elect_units %>% st_transform(9311),
#   rent_data %>% st_transform(9311)
# ) %>% st_transform(4326)
# 
# elect_units_with_aggregated_rent_data <- rent_data_with_elect_units %>% mutate(
#   wol = ordered(wol, levels = c("einfach", "mittel", "gut"))
# ) %>%
#   group_by(UWB) %>% summarise(
#   wohnlage = median(as.numeric(wol))
# )
# 
# airbnb_with_rent_data <- airbnb_election_unit_mapping %>% left_join(
#   geo_elect_units %>% as_tibble(),
#   by = c("FID_1" = "FID_1")
#   ) %>% left_join(
#     elect_units_with_aggregated_rent_data %>% as_tibble(),
#     by = c("UWB" = "UWB")
#   )
# 
# # airbnb_rent_distances <- st_distance(rent_data, airbnb_with_lor_units) # takes a long time
# 
# ##### try with knn #####
# 
# knn_df <- rent_data %>% mutate(
#   long = sf::st_coordinates(.)[,1],
#   lat = sf::st_coordinates(.)[,2]) %>%
#   as_tibble() %>%
#   select(wol, long, lat)
# 
# model_knn <- class::knn(
#   train = knn_df[,c("long", "lat")],
#   test = df_airbnb[, c("longitude", "latitude")],
#   cl = knn_df$wol,
#   k = 3
#   )
# 
# elect_units_with_aggregated_rent_data_median <- rent_data_with_elect_units %>% mutate(
#   wol = ordered(wol, levels = c("einfach", "mittel", "gut"))
# ) %>%
#   group_by(UWB) %>% summarise(
#     wohnlage = median(as.numeric(wol))
#   )
# 
# airbnb_with_rent_data_median_knn <- model_knn %>% as_tibble() %>% mutate(
#   wol = ordered(value, levels = c("einfach", "mittel", "gut")),
#   knn_wohnlage = as.numeric(wol),
#   median_wohnlage = airbnb_with_rent_data$wohnlage,
#   airbnb_id = airbnb_with_rent_data$airbnb_id
#   )
# 
# airbnb_with_rent_data_median_knn %>% select(knn_wohnlage, median_wohnlage) %>% table()
# 
# airbnb_with_rent_data_median_knn %>% saveRDS("saved_objects/airbnb_with_rent_data_median_knn.rds")
airbnb_with_rent_data_median_knn <- readRDS("saved_objects/airbnb_with_rent_data_median_knn.rds")

lm_rents1 <- lm(log(price) ~ wohnlage, data = airbnb_with_rent_data)
summary(lm_rents1)

anova(lm_base, lm_accidents1, lm_crime1, lm_rents1)

lm_rents2 <- lm(log(price) ~ knn_wohnlage, data = airbnb_with_rent_data_median_knn)
summary(lm_rents2)

anova(lm_rents1, lm_rents2)

#### Regression Part 2 ####

library(brms)

lm_sentiment_for_name <- lm(data = df_listings, log(price) ~ sentiment_for_name)
summary(lm_sentiment_for_name)

contrasts(df_listings$sentiment_for_name) = contr.treatment(3)
lm_sentiment_for_name2 <- lm(data = df_listings, log(price) ~ sentiment_for_name)
summary(lm_sentiment_for_name2)

df_listings_cleaned %>% group_by(sentiment_for_name) %>% 
  summarise(mean_price = mean(price), median_price = median(price))

blm_sentiment_for_name <- brm(
  data = df_listings_cleaned, 
  price ~ sentiment_for_name, 
  family = "lognormal"
)

# exp(4.603887)
# exp(4.603887+0.053228)
# exp(4.603887+0.053228+0.009651)

blm_sentiment_for_name2 <- brm(
  data = df_listings_cleaned, 
  price ~ mo(sentiment_for_name), 
  family = "lognormal"
)

fixef(blm_sentiment_for_name2)
# ranef(blm_sentiment_for_name2)

brms::variables(blm_sentiment_for_name2)
conditional_effects(blm_sentiment_for_name2)

tidybayes::get_variables(blm_sentiment_for_name2)
ordered_params <- tidybayes::spread_draws(
  blm_sentiment_for_name2, 
  b_Intercept, 
  bsp_mosentiment_for_name, 
  simo_mosentiment_for_name1[num]
  ) %>% pivot_wider(names_from = num, values_from = simo_mosentiment_for_name1)

#### figures ####

df_airbnb %>% ggplot(
  aes(
    y = neighbourhood_group_cleansed, 
    x = price, 
    fill = neighbourhood_group_cleansed)
  ) +
  geom_violin() +
  geom_boxplot(width = 0.1, fill = "white") +
  scale_fill_manual(values = bezirk_colors, name = "Bezirk") +
  ylab("Bezirk") +
  theme(
    legend.position = "bottom",
    legend.title.position = "top"
  )

#### rethinkng #####

library(rethinking)
library(tidybayes)
library(tidybayes.rethinking)
# library(modelr)

##### Simulations #####

###### base model ######

n <- 6306
mean <- log(100)
sd <- 0.5

sample_mu <- rnorm(n, mean, sd)
sample_sigma <- runif(n, 0, 1)
prior_h <- rnorm( n , sample_mu , sample_sigma )

sim_base_model <- tibble(
  sim_base_point_prior = rnorm(n, mean, sd),
  sim_base_distr_prior = prior_h,
  original = df_airbnb$log_price
)

sim_base_model %>% 
  pivot_longer(everything()) %>% 
  ggplot() +
  geom_density(aes(x = value), fill = "#AACCFF") +
  facet_wrap(
    ~name, nrow = 3
    )

sim_base_model %>% 
  pivot_longer(everything()) %>% 
  ggplot() +
  geom_density(aes(x = value), fill = "#AACCFF") +
  facet_wrap(
    ~name, nrow = 3
  )

###### Bezirk reputation ######

n_bezirk <- 12
bezirke <- sample(1:12, n, replace = TRUE)
b_bezirk_reputation <- rnorm(12, 0, 0.5)
b_bezirk_reputation_distr <- rnorm(n, b_bezirk_reputation[bezirke], 0.2)
prior_bezirke = rnorm(n, sample_mu+b_bezirk_reputation_distr, sample_sigma)

cbind(bezirke, b_bezirk_reputation_distr) %>% 
  ggplot(
    aes(
      x = b_bezirk_reputation_distr,
      y = bezirke,
      fill = factor(bezirke)
    )) +
  geom_violin()

sim_reputation_only_model <- tibble(
  bezirk = bezirke,
  sim_reputation = prior_bezirke,
  original = df_airbnb$log_price
)

sim_reputation_only_model %>% 
  pivot_longer(-bezirk) %>% 
  ggplot() +
  geom_density(aes(x = value), fill = "#AACCFF") +
  facet_wrap(
    ~name, nrow = 2
  )

sim_reputation_only_model %>% 
  select(-original) %>% 
  mutate(
    bezirk = str_c("Bezirk ", str_pad(bezirk, 2, "0", side = "left"))) %>% 
  ggplot(
    aes(
      x = sim_reputation,
      y = bezirk,
      fill = bezirk
    )) +
  geom_violin() +
  geom_boxplot(width = 0.2, fill = "white") +
  scale_fill_manual(values = bezirk_colors, name = "Bezirk") +
  ylab("Bezirk") +
  xlab("log(price)") +
  theme(
    legend.position = "bottom",
    legend.title.position = "top"
  )

##### modeling #####

###### base model ######
dat <- df_airbnb %>% select(log_price)

flist <- alist(
                log_price ~ dnorm(a, sigma),
                a <- mu,
                mu ~ dnorm(4.61, 0.5),
                sigma ~ dunif( 0 , 1 )
)

base_model <- ulam(
  flist, data=dat, 
  cores = 4, chains = 4,
  file = "saved_objects/base_model"
  )

summary(base_model)
# rm(base_model)
base_model
precis(base_model)
post <- extract.samples(base_model , n=1e4) %>% as_tibble() %>% setNames(c("mu", "sigma"))
post[[1]] %>% exp() %>% density() %>% plot()
post %>% rowwise() %>% transmute(price = exp(rnorm(1, mu, sigma))) %>% pull() %>% dens()

base_model %>% spread_draws(mu, ndraws = 1000) %>% 
  mutate(price = exp(mu)) %>% 
  ggplot(aes(x = price)) +
  stat_halfeye(.width = c(.90, .5)) +
  ylab("density")

base_model %>% spread_draws(mu, sigma, ndraws = 1000) %>% 
  mutate(price = exp(rnorm(1000, mu, sigma))) %>% 
  ggplot(aes(x = price)) +
  stat_halfeye(.width = c(.90, .5)) +
  ylab("density")

dat %>% 
  modelr::data_grid(1) %>%
  add_predicted_draws(base_model) %>%
  mutate(price = exp(.prediction)) %>% 
  ggplot(aes(x = price)) +
  stat_slab() +
  
  coord_cartesian(xlim = c(0, 250))

dat <- df_airbnb %>% select(log_price)

flist_studnet <- alist(
  log_price ~ dstudent(2, a, sigma),
  a <- mu,
  mu ~ dnorm(4.61, 0.5),
  sigma ~ dunif( 0 , 1 )
)

base_model_student <- ulam(
  flist_studnet, data=dat, 
  cores = 4, chains = 4,
  file = "saved_objects/base_model_student"
)

summary(base_model_student)

# plots 

set.seed(1337)
g_param_norm <- base_model %>% spread_draws(mu, ndraws = 1000) %>% 
  mutate(price = exp(mu)) %>% 
  ggplot(aes(x = price)) +
  stat_halfeye(.width = c(.90, .5)) +
  labs(x=NULL, y=NULL) +
  coord_cartesian(xlim = c(100, 108))

g_pred_norm <- base_model %>% spread_draws(mu, sigma, ndraws = 1000) %>% 
  mutate(price = exp(rnorm(1000, mu, sigma))) %>% 
  ggplot(aes(x = price)) +
  stat_halfeye(.width = c(.90, .5)) +
  labs(x=NULL, y=NULL) +
  xlim(0, 800)

g_param_student <- base_model_student %>% spread_draws(mu, ndraws = 1000) %>% 
  mutate(price = exp(mu)) %>% 
  ggplot(aes(x = price)) +
  stat_halfeye(.width = c(.90, .5)) +
  labs(x=NULL, y=NULL) +
  coord_cartesian(xlim = c(100, 108))

g_pred_student <- base_model_student %>% spread_draws(mu, sigma, ndraws = 1000) %>% 
  mutate(price = exp(rstudent(1000, 2, mu, sigma))) %>% 
  ggplot(aes(x = price)) +
  stat_halfeye(.width = c(.90, .5)) +
  labs(x=NULL, y=NULL) +
  xlim(0, 800)

base_model_student %>% spread_draws(mu, sigma, ndraws = 1000) %>% 
  mutate(price = exp(rstudent(1000, 2, mu, sigma))) %>% pull(price) %>% max()

p <- list(g_param_norm, g_pred_norm, g_param_student, g_pred_student)

yleft = gridtext::richtext_grob("density", rot=90)
bottom = gridtext::richtext_grob(
  text = 'price'
)

gridExtra::grid.arrange(
  grobs = p, 
  nrow = 2, 
  left = yleft, bottom = bottom
)

###### reputation only ######

dat <- df_airbnb %>% 
  mutate(bezirk = as.numeric(as.factor(neighbourhood_group_cleansed))) %>% 
  select(log_price, bezirk)

flist <- alist(
  log_price ~ dnorm(a, sigma),
  a <- reputation[bezirk],
  reputation[bezirk] ~ dnorm(4.61, 0.5),
  sigma ~ dexp(1)
)

base_model_reputation <- ulam(
  flist, data=dat, 
  cores = 4, chains = 4, iter = 2000
)

# precis(base_model_reputation, depth = 2)
summary(base_model_reputation)
coef(base_model_reputation) %>% names()

bezirk_levels <- df_airbnb %>% 
  transmute(bezirk = as.factor(neighbourhood_group_cleansed)) %>% 
  pull() %>% levels()

base_model_reputation %>% spread_draws(reputation[bezirk], ndraws = 1000) %>% 
  mutate(bezirk = bezirk_levels[bezirk], price = exp(reputation)) %>% 
  ggplot(aes(y = bezirk, x = price)) +
  stat_halfeye(.width = c(.90, .5))

base_model_reputation %>% spread_draws(reputation[bezirk], ndraws = 1000) %>% 
  mutate(bezirk = bezirk_levels[bezirk], price = exp(reputation)) %>% 
  compare_levels(price, by = bezirk) %>%
  ggplot(aes(y = bezirk, x = price, fill = stat(abs(x) < 5))) +
  stat_halfeye(.width = c(.90, .5)) +
  geom_vline(xintercept = c(-5, 5), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))

dat %>% 
  modelr::data_grid(bezirk) %>%
  add_predicted_draws(base_model_reputation) %>%
  mutate(bezirk = bezirk_levels[bezirk], price = exp(.prediction)) %>% 
  ggplot(aes(x = price, y = bezirk)) +
  stat_slab() +
  coord_cartesian(xlim = c(0, 250))

dat %>% 
  modelr::data_grid(bezirk) %>%
  add_predicted_draws(base_model_reputation) %>%
  mutate(bezirk = bezirk_levels[bezirk], price = exp(.prediction)) %>% 
  compare_levels(price, by = bezirk) %>%
  ggplot(aes(x = price, y = bezirk, fill = stat(abs(x) < 5))) +
  stat_slab() +
  # stat_halfeye(.width = c(.90, .5)) +
  geom_vline(xintercept = c(-5, 5), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  xlim(c(-100, 100))

###### With Wohnlage ######

dat <- df_airbnb %>% 
  left_join(
    airbnb_with_rent_data_median_knn,
    by = c("id"  = "airbnb_id")
  ) %>% 
  mutate(
    bezirk = as.numeric(as.factor(neighbourhood_group_cleansed)),
    median_wohnlage = round(median_wohnlage)
    ) %>% 
  select(log_price, bezirk, median_wohnlage)

# wohnlagen_levels <- airbnb_with_rent_data_median_knn %>% 
#   transmute(wohnlage = as.factor(median_wohnlage)) %>% 
#   pull() %>% levels()

flist <- alist(
  log_price ~ dnorm(a, sigma),
  a <- reputation[bezirk] + bE*sum(delta[1:median_wohnlage]),
  reputation[bezirk] ~ dnorm(4.61, 0.5),
  bE ~ dnorm(0, 0.5),
  # vector[3]: delta_j <<- append_row( 0 , delta ),
  simplex[3]: delta ~ dirichlet( alpha ),
  sigma ~ dexp(1)
)

dat2 <- list(
  alpha = rep(2, 3),
  median_wohnlage = as.integer(dat$median_wohnlage),
  bezirk = dat$bezirk,
  log_price = dat$log_price
)

base_model_reputation_wohnlage <- ulam(
  flist, data=dat2, 
  cores = 4, chains = 4, iter = 2000,
  file = "saved_objects/base_model_reputation_wohnlage"
)

summary(base_model_reputation_wohnlage)
# summary(base_model_reputation_wohnlage_broken)

flist_student <- alist(
  log_price ~ dstudent(2, a, sigma),
  a <- reputation[bezirk] + bE*sum(delta[1:median_wohnlage]),
  reputation[bezirk] ~ dnorm(4.61, 0.5),
  bE ~ dnorm(0, 0.5),
  # vector[3]: delta_j <<- append_row( 0 , delta ),
  simplex[3]: delta ~ dirichlet( alpha ),
  sigma ~ dexp(1)
)

base_model_reputation_wohnlage_student <- ulam(
  flist_student, data=dat2, 
  cores = 4, chains = 4, iter = 2000,
  file = "saved_objects/base_model_reputation_wohnlage_student"
)

summary(base_model_reputation_wohnlage_student)

get_variables(base_model_reputation_wohnlage)

base_model_reputation_wohnlage %>% spread_draws(reputation[bezirk], ndraws = 1000) %>% 
  mutate(bezirk = bezirk_levels[bezirk], price = exp(reputation)) %>% 
  ggplot(aes(y = bezirk, x = price)) +
  stat_halfeye(.width = c(.90, .5))

base_model_reputation_wohnlage %>% spread_draws(reputation[bezirk], bE, delta[wohnlage], sigma, ndraws = 1000) %>% 
  mutate(bezirk = bezirk_levels[bezirk]) %>% 
  pivot_wider(names_from = wohnlage, names_prefix = "wol_", values_from = delta) %>% 
  mutate(
    price_wol_1 = reputation+bE*(wol_1),
    price_wol_2 = reputation+bE*(wol_1+wol_2),
    price_wol_3 = reputation+bE*(wol_1+wol_2+wol_3)
  ) %>% pivot_longer(cols = starts_with("price_wol")) %>% 
  # mutate(price = exp(value)) %>% 
  rowwise() %>% 
  mutate(price = exp(rnorm(1, value, sigma))) %>% 
  ggplot(aes(x = price, y = bezirk)) +
  stat_slab() +
  facet_wrap(~name) +
  coord_cartesian(xlim = c(0, 250))

# base_model_reputation_wohnlage_broken %>% spread_draws(reputation[bezirk], bE, delta[wohnlage], ndraws = 1000) %>% 
#   mutate(bezirk = bezirk_levels[bezirk]) %>% 
#   pivot_wider(names_from = wohnlage, names_prefix = "wol_", values_from = delta) %>% 
#   mutate(
#     price_wol_1 = reputation,
#     price_wol_2 = reputation+bE*(wol_1),
#     price_wol_3 = reputation+bE*(wol_1+wol_2)
#   ) %>% pivot_longer(cols = starts_with("price_wol")) %>% 
#   mutate(price = exp(value)) %>% 
#   ggplot(aes(x = price, y = bezirk)) +
#   stat_slab() +
#   facet_wrap(~name)
# 
# base_model_reputation_wohnlage %>% spread_draws(reputation[bezirk], bE, delta[wohnlage], ndraws = 1000) %>% 
#   mutate(bezirk = bezirk_levels[bezirk]) %>% 
#   pivot_wider(names_from = wohnlage, names_prefix = "wol_", values_from = delta) %>% 
#   mutate(
#     price_wol_1 = reputation+bE*(wol_1),
#     price_wol_2 = reputation+bE*(wol_1+wol_2),
#     price_wol_3 = reputation+bE*(wol_1+wol_2+wol_3)
#   ) %>% pivot_longer(cols = starts_with("price_wol")) %>% 
#   mutate(price = exp(value), n_simplex = 3) %>% 
#   bind_rows(
#     base_model_reputation_wohnlage_broken %>% spread_draws(reputation[bezirk], bE, delta[wohnlage], ndraws = 1000) %>% 
#       mutate(bezirk = bezirk_levels[bezirk]) %>% 
#       pivot_wider(names_from = wohnlage, names_prefix = "wol_", values_from = delta) %>% 
#       mutate(
#         price_wol_1 = reputation,
#         price_wol_2 = reputation+bE*(wol_1),
#         price_wol_3 = reputation+bE*(wol_1+wol_2)
#       ) %>% pivot_longer(cols = starts_with("price_wol")) %>% 
#       mutate(price = exp(value), n_simplex = 2)
#   ) %>% 
#   ggplot(aes(x = price, y = bezirk, fill = factor(n_simplex))) +
#   stat_slab(alpha = .5) +
#   facet_wrap(~name)

base_model_reputation_wohnlage %>% spread_draws(reputation[bezirk], bE, delta[wohnlage], sigma, ndraws = 1000) %>% 
  mutate(bezirk = bezirk_levels[bezirk]) %>% 
  pivot_wider(names_from = wohnlage, names_prefix = "wol_", values_from = delta) %>% 
  mutate(
    price_wol_1 = reputation+bE*(wol_1),
    price_wol_2 = reputation+bE*(wol_1+wol_2),
    price_wol_3 = reputation+bE*(wol_1+wol_2+wol_3)
  ) %>% pivot_longer(cols = starts_with("price_wol")) %>% 
  mutate(price = exp(value)) %>% 
  # rowwise() %>% 
  # mutate(price = exp(rnorm(1, value, sigma))) %>% 
  compare_levels(price, by = name) %>% 
  ggplot(aes(x = price, y = name, fill = stat(abs(x) > 5))) +
  stat_slab() +
  # stat_halfeye(.width = c(.90, .5)) +
  geom_vline(xintercept = c(-5, 5), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue")) # +
  # coord_cartesian(xlim = c(-250, 250))

# broken
base_model_reputation_wohnlage %>% spread_draws(reputation[bezirk], bE, delta[wohnlage], ndraws = 1000) %>% 
  mutate(bezirk = bezirk_levels[bezirk]) %>% 
  pivot_wider(names_from = wohnlage, names_prefix = "wol_", values_from = delta) %>% 
  mutate(
    price_wol_1 = reputation+bE*(wol_1),
    price_wol_2 = reputation+bE*(wol_1+wol_2),
    price_wol_3 = reputation+bE*(wol_1+wol_2+wol_3)
  ) %>% pivot_longer(cols = starts_with("price_wol")) %>% 
  mutate(price = exp(value)) %>%
  select(bezirk, price, name, .draw, .chain, .iteration) %>% 
  pivot_wider(names_from = bezirk, values_from = price) # %>% 
  # compare_levels(price, by = bezirk)

# dat %>%
#   modelr::data_grid(bezirk, median_wohnlage) %>%
#   add_predicted_draws(base_model_reputation_wohnlage) %>%
#   mutate(
#     bezirk = bezirk_levels[bezirk], 
#     wohnlage = wohnlagen_levels[median_wohnlage],
#     price = .prediction
#     ) %>%
#   ggplot(aes(x = price, y = bezirk)) +
#   stat_slab() +
#   # coord_cartesian(xlim = c(0, 250)) +
#   facet_wrap(~median_wohnlage)

append_row <- function(a, b) {
  cbind(0, b)
}

sim(base_model_reputation_wohnlage) %>% dens()
link(base_model_reputation_wohnlage) %>% dens()
rethinking::sim(base_model_reputation_wohnlage) %>% as.numeric() %>% dens()
extract.samples(base_model_reputation_wohnlage) %>% as_tibble()

# fixed
bdat <- base_model_reputation_wohnlage %>% spread_draws(reputation[bezirk], bE, delta[wohnlage], sigma, ndraws = 1000) %>% 
  mutate(bezirk = bezirk_levels[bezirk]) %>% 
  pivot_wider(names_from = wohnlage, names_prefix = "wol_", values_from = delta) %>% 
  mutate(
    price_wol_1 = reputation+bE*(wol_1),
    price_wol_2 = reputation+bE*(wol_1+wol_2),
    price_wol_3 = reputation+bE*(wol_1+wol_2+wol_3)
  ) %>% pivot_longer(cols = starts_with("price_wol")) %>% 
  # mutate(price = exp(value)) %>%
  rowwise() %>% mutate(price = exp(rnorm(1, value, sigma))) %>%
  select(bezirk, price, name, .draw, .chain, .iteration) %>% 
  pivot_wider(names_from = bezirk, values_from = price, names_prefix = "price_")

adat <- bdat %>% 
  select(starts_with("price_"))

# Create a sample dataframe (you can replace this with your actual data)
# set.seed(24)
# adat <- as.data.frame(matrix(sample(1:20, 26*14, replace=TRUE), ncol=14))

# Get column names
nm1 <- outer(colnames(adat), colnames(adat), paste, sep="_-_")

# Indices for lower triangular elements (excluding diagonal)
indx1 <- which(lower.tri(nm1, diag=TRUE))

# Calculate pairwise differences
res <- outer(1:ncol(adat), 1:ncol(adat), function(x, y) adat[, x] - adat[, y])

# Set column names for the resulting dataframe
colnames(res) <- nm1
res1 <- res[-indx1]

# Dimensions of the resulting dataframe
# dim(res1)  # [1] 26 91

res1 %>% bind_cols(
  bdat %>% select(-starts_with("price_")) %>% rename(wohnlage = name)
) %>% 
  pivot_longer(starts_with("price_"), values_to = "price_diff") %>% 
  mutate(
    name = str_replace(name, "_-_", " - ") %>% str_remove_all("price_")
  ) %>% 
  ggplot(aes(x = price_diff, y = name, fill = wohnlage)) +
  stat_slab(alpha = .5) +
  coord_cartesian(xlim = c(-250, 250))

# plots

get_variables(base_model_reputation_wohnlage)

set.seed(1337)
g_param_norm <- base_model_reputation_wohnlage %>% 
  spread_draws(reputation[bezirk], bE, delta[wohnlage], ndraws = 1000) %>% 
  pivot_wider(names_from = "wohnlage", values_from = "delta", names_prefix = "wol_") %>% 
  mutate(
    price_wol_1 = exp(reputation+bE*(wol_1)),
    price_wol_2 = exp(reputation+bE*(wol_1+wol_2)),
    price_wol_3 = exp(reputation+bE*(wol_1+wol_2+wol_3))
    ) %>% pivot_longer(cols = starts_with("price_"), values_to = "price", names_prefix = "price_wol_", names_to = "wohnlage") %>% 
  mutate(bezirk = bezirk_levels[bezirk]) %>% 
  ggplot(aes(x = price, y = bezirk, fill = wohnlage)) +
  stat_halfeye(.width = c(.90, .5), alpha = .5) +
  labs(x=NULL, y=NULL)

g_pred_norm <- base_model_reputation_wohnlage %>% 
  spread_draws(reputation[bezirk], bE, delta[wohnlage], sigma, ndraws = 1000) %>% 
  pivot_wider(names_from = "wohnlage", values_from = "delta", names_prefix = "wol_") %>% 
  rowwise() %>% 
  mutate(
    price_wol_1 = exp(rnorm(1, reputation+bE*(wol_1), sigma)),
    price_wol_2 = exp(rnorm(1, reputation+bE*(wol_1+wol_2), sigma)),
    price_wol_3 = exp(rnorm(1, reputation+bE*(wol_1+wol_2+wol_3), sigma))
  ) %>% pivot_longer(cols = starts_with("price_"), values_to = "price", names_prefix = "price_wol_", names_to = "wohnlage") %>% 
  mutate(bezirk = bezirk_levels[bezirk]) %>% 
  ggplot(aes(x = price, y = bezirk, fill = wohnlage)) +
  stat_halfeye(.width = c(.90, .5), alpha = .5) +
  labs(x=NULL, y=NULL) +
  xlim(0, 500)

g_param_student <- base_model_reputation_wohnlage_student %>% 
  spread_draws(reputation[bezirk], bE, delta[wohnlage], ndraws = 1000) %>% 
  pivot_wider(names_from = "wohnlage", values_from = "delta", names_prefix = "wol_") %>% 
  mutate(
    price_wol_1 = exp(reputation+bE*(wol_1)),
    price_wol_2 = exp(reputation+bE*(wol_1+wol_2)),
    price_wol_3 = exp(reputation+bE*(wol_1+wol_2+wol_3))
  ) %>% pivot_longer(cols = starts_with("price_"), values_to = "price", names_prefix = "price_wol_", names_to = "wohnlage") %>% 
  mutate(bezirk = bezirk_levels[bezirk]) %>% 
  ggplot(aes(x = price, y = bezirk, fill = wohnlage)) +
  stat_halfeye(.width = c(.90, .5), alpha = .5) +
  labs(x=NULL, y=NULL)

g_pred_student <- base_model_reputation_wohnlage_student %>%
  spread_draws(reputation[bezirk], bE, delta[wohnlage], sigma, ndraws = 1000) %>% 
  pivot_wider(names_from = "wohnlage", values_from = "delta", names_prefix = "wol_") %>% 
  rowwise() %>% 
  mutate(
    price_wol_1 = exp(rstudent(1, 2, reputation+bE*(wol_1), sigma)),
    price_wol_2 = exp(rstudent(1, 2, reputation+bE*(wol_1+wol_2), sigma)),
    price_wol_3 = exp(rstudent(1, 2, reputation+bE*(wol_1+wol_2+wol_3), sigma))
  ) %>% pivot_longer(cols = starts_with("price_"), values_to = "price", names_prefix = "price_wol_", names_to = "wohnlage") %>% 
  mutate(bezirk = bezirk_levels[bezirk]) %>% 
  ggplot(aes(x = price, y = bezirk, fill = wohnlage)) +
  stat_halfeye(.width = c(.90, .5), alpha = .5) +
  labs(x=NULL, y=NULL) +
  xlim(0, 500)

base_model_student %>% spread_draws(mu, sigma, ndraws = 1000) %>% 
  mutate(price = exp(rstudent(1000, 2, mu, sigma))) %>% pull(price) %>% max()

p <- list(g_param_norm, g_pred_norm, g_param_student, g_pred_student)

yleft = gridtext::richtext_grob("density", rot=90)
bottom = gridtext::richtext_grob(
  text = 'price'
)

ggpubr::ggarrange(
  plotlist = p, 
  nrow = 2,
  common.legend = TRUE, legend = "bottom"
)

bdat <- base_model_reputation_wohnlage %>% 
  spread_draws(reputation[bezirk], bE, delta[wohnlage], ndraws = 1000) %>% 
  pivot_wider(names_from = "wohnlage", values_from = "delta", names_prefix = "wol_") %>% 
  mutate(
    price_wol_1 = exp(reputation+bE*(wol_1)),
    price_wol_2 = exp(reputation+bE*(wol_1+wol_2)),
    price_wol_3 = exp(reputation+bE*(wol_1+wol_2+wol_3))
  ) %>% pivot_longer(cols = starts_with("price_"), values_to = "price", names_prefix = "price_wol_", names_to = "wohnlage") %>% 
  mutate(bezirk = bezirk_levels[bezirk]) %>% 
  filter(bezirk %in% c("Mitte", "Tempelhof-Schöneberg", "Neukölln", "Reinickendorf")) %>% 
  select(bezirk, price, wohnlage, .draw, .chain, .iteration) %>% 
  pivot_wider(names_from = bezirk, values_from = price, names_prefix = "price_")
  
adat <- bdat %>% 
  select(starts_with("price_"))

# Get column names
nm1 <- outer(colnames(adat), colnames(adat), paste, sep="_-_")

# Indices for lower triangular elements (excluding diagonal)
indx1 <- which(lower.tri(nm1, diag=TRUE))

# Calculate pairwise differences
res <- outer(1:ncol(adat), 1:ncol(adat), function(x, y) adat[, x] - adat[, y])

# Set column names for the resulting dataframe
colnames(res) <- nm1
res1 <- res[-indx1]

area <- 10
res1 %>% bind_cols(
  bdat %>% select(-starts_with("price_")) # %>% rename(wohnlage = name)
) %>% 
  pivot_longer(starts_with("price_"), values_to = "price_diff") %>% 
  mutate(
    name = str_replace(name, "_-_", " - ") %>% str_remove_all("price_")
  ) %>% 
  ggplot(aes(x = price_diff, y = name, fill = stat(abs(x) > area))) +
  stat_slab() +
  geom_vline(xintercept = c(-area, area), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))

bdat <- base_model_reputation_wohnlage %>% 
  spread_draws(reputation[bezirk], bE, delta[wohnlage], sigma, ndraws = 1000) %>% 
  pivot_wider(names_from = "wohnlage", values_from = "delta", names_prefix = "wol_") %>%
  rowwise() %>% 
  mutate(
    price_wol_1 = exp(rnorm(1, reputation+bE*(wol_1), sigma)),
    price_wol_2 = exp(rnorm(1, reputation+bE*(wol_1+wol_2), sigma)),
    price_wol_3 = exp(rnorm(1, reputation+bE*(wol_1+wol_2+wol_3), sigma))
  ) %>% pivot_longer(cols = starts_with("price_"), values_to = "price", names_prefix = "price_wol_", names_to = "wohnlage") %>% 
  mutate(bezirk = bezirk_levels[bezirk]) %>% 
  filter(bezirk %in% c("Mitte", "Tempelhof-Schöneberg", "Neukölln", "Reinickendorf")) %>% 
  select(bezirk, price, wohnlage, .draw, .chain, .iteration) %>% 
  pivot_wider(names_from = bezirk, values_from = price, names_prefix = "price_")

adat <- bdat %>% 
  select(starts_with("price_"))

# Get column names
nm1 <- outer(colnames(adat), colnames(adat), paste, sep="_-_")

# Indices for lower triangular elements (excluding diagonal)
indx1 <- which(lower.tri(nm1, diag=TRUE))

# Calculate pairwise differences
res <- outer(1:ncol(adat), 1:ncol(adat), function(x, y) adat[, x] - adat[, y])

# Set column names for the resulting dataframe
colnames(res) <- nm1
res1 <- res[-indx1]

area <- 10
df_temp <- res1 %>% bind_cols(
  bdat %>% select(-starts_with("price_")) # %>% rename(wohnlage = name)
) %>% 
  pivot_longer(starts_with("price_"), values_to = "price_diff") %>% 
  mutate(
    name = str_replace(name, "_-_", " - ") %>% str_remove_all("price_")
  )

df_temp %>% left_join(
  df_temp %>% group_by(name) %>% summarise(p_safe = round(100*mean(price_diff > 0),1)) %>% 
    mutate(p_safe = str_c(p_safe, " %"))
) %>% 
  ggplot(aes(x = price_diff, y = name, fill = stat(x > 0))) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  labs(x="difference of mean price in €", y=NULL) +
  geom_text(aes(x = 80, y = as.integer(as.factor(name))+0.15, label = p_safe)) +
  coord_cartesian(xlim = c(-350, 350))

df_temp %>% group_by(name) %>% summarise(p_safe_10 = round(100*mean(price_diff > 10), 2))
  
##### trolley example #####

data(Trolley)
d <- Trolley %>% sample_n(1000)
edu_levels <- c( 6 , 1 , 8 , 4 , 7 , 2 , 5 , 3 )
d$edu_new <- edu_levels[ d$edu ]

dat <- list(
             R = d$response ,
             action = d$action,
             intention = d$intention,
             contact = d$contact,
             E = as.integer( d$edu_new ), # edu_new as an index
             alpha = rep( 2 , 8 ) ) # delta prior

m12.6 <- ulam(
  alist(
    R ~ ordered_logistic( phi , kappa ),
    phi <- bE*sum( delta[1:E] ) + bA*action + bI*intention + bC*contact,
    kappa ~ normal( 0 , 1.5 ),
    c(bA,bI,bC,bE) ~ normal( 0 , 1 ),
    simplex[8]: delta ~ dirichlet( alpha )
  ), data=dat , chains=4 , cores=4 )
summary(m12.6)
temp <- sim(m12.6) %>% as_tibble()

temp %>% pivot_longer(everything()) %>% pull(value) %>% hist()
d$response %>% hist()

get_variables(m12.6)
spread_draws(m12.6, kappa[cutpoint], bE, bC, bI, bA, delta[response_level]) %>% 
  pivot_wider(names_from = "response_level", names_prefix = "resp_", values_from = delta) %>% 
  mutate(
    val1 = bAbE*(resp_1)
    )

dat$edu_norm <- normalize( d$edu_new )
m12.7 <- ulam(
  alist(
    R ~ ordered_logistic( mu , cutpoints ),
    mu <- bE*edu_norm + bA*action + bI*intention + bC*contact,
    c(bA,bI,bC,bE) ~ normal( 0 , 1 ),
    cutpoints ~ normal( 0 , 1.5 )
  ), data=dat , chains=4 , cores=4 )
temp2 <- sim(m12.7) %>% as_tibble()

temp2 %>% pivot_longer(everything()) %>% pull(value) %>% hist()

##### full model #####

property_type_levels <- df_airbnb2 %>% transmute(property_type = as.factor(room_type)) %>% pull() %>% levels()

dat <- df_airbnb2 %>%  
  transmute(
    log_price = log_price,
    bezirk = as.integer(as.factor(neighbourhood_group_cleansed)),
    median_wohnlage = round(median_wohnlage),
    n_traffic_accidents = scale(n_traffic_accidents),
    n_crimes_total = scale(Straftaten_total),
    distance_opnv = scale(as.numeric(distance_opnv)),
    distance_hauptbahnhof = scale(as.numeric(distance_hauptbahnhof)),
    distance_toilet = scale(as.numeric(distance_toilet)),
    gigabit_supply = scale(gigabit_supply_2024),
    property_type = as.integer(as.factor(room_type)),
    accommodates = scale(accommodates),
    beds = scale(beds),
    bedrooms = scale(bedrooms),
    bathrooms = scale(bathrooms),
    kitchen = as.integer(kitchen),
    tv = as.integer(TV),
    dishwasher = as.integer(dishwasher),
    stove = as.integer(stove),
    microwave = as.integer(microwave),
    wineglasses = as.integer(wineglasses),
    freezer = as.integer(freezer),
    refrigerator = as.integer(refrigerator),
    washer = as.integer(washer),
    dryer = as.integer(dryer),
    wifi = as.integer(wifi),
    workspace = as.integer(workspace),
    bathtub = as.integer(bathtub),
    boardgames = as.integer(boardgames),
    piano = as.integer(piano),
    sauna = as.integer(sauna),
    bedlinens = as.integer(bedlinens),
    privateentrance = as.integer(privateentrance),
    pets = as.integer(pets),
    balcony = as.integer(balcony),
    freeparking = as.integer(freeparking),
    smoking = as.integer(smoking),
    grill = as.integer(grill)
  ) %>% 
  drop_na() %>% 
  as.list() %>% c(list(alpha = rep(2, 3)))

flist_only_flat_features <- alist(
  log_price ~ dnorm(a, sigma),
  a <- reputation[bezirk] + bE*sum(delta[1:median_wohnlage]) + flat_features,
  flat_features <- bPT[property_type] + bA*accommodates + bB*beds + bBR*bedrooms + 
    bBaR*bathrooms + bK*kitchen,
  reputation[bezirk] ~ dnorm(4.61, 0.5),
  c(bE, bA, bB, bBR, bBaR, bK) ~ dnorm(0, 0.3),
  bPT[property_type] ~ dnorm(0, 0.5),
  simplex[3]: delta ~ dirichlet( alpha ),
  sigma ~ dexp(1)
)

full_model_only_flat_features <- ulam(
  flist_only_flat_features, data=dat, 
  cores = 4, chains = 4, iter = 2000,
  file = "saved_objects/full_model_only_flat_features"
)

summary(full_model_only_flat_features)

flist_no_amenities <- alist(
  log_price ~ dnorm(a, sigma),
  a <- reputation[bezirk] + bE*sum(delta[1:median_wohnlage]) + flat_features + region_effects,
  flat_features <- bPT[property_type] + bA*accommodates + bB*beds + bBR*bedrooms + 
    bBaR*bathrooms + bK*kitchen,
  region_effects <- bTA*n_traffic_accidents + bDT*distance_toilet + 
    bDH*distance_hauptbahnhof + bDO*distance_opnv + bGB*gigabit_supply,
  reputation[bezirk] ~ dnorm(4.61, 0.5),
  c(bE, bA, bB, bBR, bBaR, bK,
    bTA, bDO, bDH, bDT, bGB) ~ dnorm(0, 0.3),
  bPT[property_type] ~ dnorm(0, 0.5),
  simplex[3]: delta ~ dirichlet( alpha ),
  sigma ~ dexp(1)
)

full_model_no_amenities <- ulam(
  flist_no_amenities, data=dat, 
  cores = 4, chains = 4, iter = 2000,
  file = "saved_objects/full_model_no_amenities"
)

summary(full_model_no_amenities)

flist_full <- alist(
  log_price ~ dnorm(a, sigma),
  a <- reputation[bezirk] + bE*sum(delta[1:median_wohnlage]) + flat_features + region_effects + amenities,
  region_effects <- bTA*n_traffic_accidents + bDT*distance_toilet + 
    bDH*distance_hauptbahnhof + bDO*distance_opnv + bGB*gigabit_supply,
  flat_features <- bPT[property_type] + bA*accommodates + bB*beds + bBR*bedrooms + 
    bBaR*bathrooms + bK*kitchen,
  amenities <- bTV*tv + bDW*dishwasher + bS*stove + bMW*microwave + bWG*wineglasses + 
    bF*freezer + bRF*refrigerator + bW*washer + bD*dryer + bWifi*wifi + bWS*workspace +
    bBT*bathtub + bBG*boardgames + bP*piano + bSauna*sauna + bBL*bedlinens + 
    bPE*privateentrance + bPet*pets + bBalc*balcony + bFP*freeparking + bSmoke*smoking +
    bG*grill,
  reputation[bezirk] ~ dnorm(4.61, 0.5),
  c(bE, bA, bB, bBR, bBaR, bK,
    bTA, bDO, bDH, bDT, bGB, 
    bTV, bDW, bS, bMW, bWG, bF, bRF, bW, bD, bWifi, bWS, bBT,
    bBG, bP, bSauna, bBL, bPE, bPet, bBalc, bFP, bSmoke, bG) ~ dnorm(0, 0.3),
  bPT[property_type] ~ dnorm(0, 0.5),
  simplex[3]: delta ~ dirichlet( alpha ),
  sigma ~ dexp(1)
)

flist_full_verbose <- alist(
  log_price ~ dnorm(a, sigma),
  a <- reputation[bezirk] + bE*sum(delta[1:median_wohnlage]) + flat_features + region_effects + amenities + kitchen_amenities,
  region_effects <- bTrafficAccidents*n_traffic_accidents + bDistToilet*distance_toilet + 
    bDistHbf*distance_hauptbahnhof + bDistTrainstation*distance_opnv + bGigabit*gigabit_supply,
  flat_features <- bPropType[property_type] + bAccommodates*accommodates + bBeds*beds + bBedrooms*bedrooms + 
    bBathrooms*bathrooms + bKitchen*kitchen,
  amenities <- bTV*tv + bWifi*wifi + bWorkspace*workspace +
  bBathtub*bathtub + bBoardGames*boardgames + bPiano*piano + bSauna*sauna + bBedlinens*bedlinens +
  bPrivateEntrance*privateentrance + bPet*pets + bBalcony*balcony + bFreeParking*freeparking + bSmoke*smoking +
  bGrill*grill,
  kitchen_amenities <- bDishwasher*dishwasher + bStove*stove + bMicrowave*microwave + bWineglasses*wineglasses +
    bFreezer*freezer + bRefrigerator*refrigerator + bWasher*washer + bDryer*dryer,
  reputation[bezirk] ~ dnorm(4.61, 0.5),
  c(bE, bAccommodates, bBeds, bBedrooms, bBathrooms, bKitchen,
    bTrafficAccidents, bDistTrainstation, bDistHbf, bDistToilet, bGigabit, 
    bTV, bDishwasher, bStove, bMicrowave, bWineglasses, bFreezer, bRefrigerator, bWasher, bDryer, 
    bWifi, bWorkspace, bBathtub, bBoardGames, bPiano, bSauna, bBedlinens, 
    bPrivateEntrance, bPet, bBalcony, bFreeParking, bSmoke, 
    bGrill
    ) ~ dnorm(0, 0.3),
  bPropType[property_type] ~ dnorm(0, 0.5),
  simplex[3]: delta ~ dirichlet( alpha ),
  sigma ~ dexp(1)
)

full_model_verbose <- ulam(
  flist_full_verbose, data=dat, 
  cores = 4, chains = 4, iter = 2000
)

full_model_verbose %>% 
  spread_draws(
    reputation[bezirk], bE, delta[wohnlage], bPropType[property_type], sigma,
    bAccommodates, bBeds, bBedrooms, bBathrooms, bKitchen,
    bTrafficAccidents, bDistTrainstation, bDistHbf, bDistToilet, bGigabit, 
    bTV, bDishwasher, bStove, bMicrowave, bWineglasses, bFreezer, bRefrigerator, bWasher, bDryer, 
    bWifi, bWorkspace, bBathtub, bBoardGames, bPiano, bSauna, bBedlinens, 
    bPrivateEntrance, bPet, bBalcony, bFreeParking, bSmoke, 
    bGrill,
    ndraws = 1000) %>% 
  saveRDS("saved_objects/full_model_verbose_post.rds")


full_model_verbose_post <- readRDS("saved_objects/full_model_verbose_post.rds")

flist_full_student <- alist(
  log_price ~ dstudent(2, a, sigma),
  a <- reputation[bezirk] + bE*sum(delta[1:median_wohnlage]) + flat_features + region_effects + amenities,
  region_effects <- bTA*n_traffic_accidents + bDT*distance_toilet + 
    bDH*distance_hauptbahnhof + bDO*distance_opnv + bGB*gigabit_supply,
  flat_features <- bPT[property_type] + bA*accommodates + bB*beds + bBR*bedrooms + 
    bBaR*bathrooms + bK*kitchen,
  amenities <- bTV*tv + bDW*dishwasher + bS*stove + bMW*microwave + bWG*wineglasses + 
    bF*freezer + bRF*refrigerator + bW*washer + bD*dryer + bWifi*wifi + bWS*workspace +
    bBT*bathtub + bBG*boardgames + bP*piano + bSauna*sauna + bBL*bedlinens + 
    bPE*privateentrance + bPet*pets + bBalc*balcony + bFP*freeparking + bSmoke*smoking +
    bG*grill,
  reputation[bezirk] ~ dnorm(4.61, 0.5),
  c(bE, bA, bB, bBR, bBaR, bK,
    bTA, bDO, bDH, bDT, bGB, 
    bTV, bDW, bS, bMW, bWG, bF, bRF, bW, bD, bWifi, bWS, bBT,
    bBG, bP, bSauna, bBL, bPE, bPet, bBalc, bFP, bSmoke, bG) ~ dnorm(0, 0.3),
  bPT[property_type] ~ dnorm(0, 0.5),
  simplex[3]: delta ~ dirichlet( alpha ),
  sigma ~ dexp(1)
)

full_model_student <- ulam(
  flist_full_student, data=dat, 
  cores = 4, chains = 4, iter = 2000,
  file = "saved_objects/full_model_student"
)

summary(full_model_student)

# plots
get_variables(full_model)

full_model_post <- readRDS("saved_objects/full_model_post.rds") 
full_model_post %>%
  pivot_wider(names_from = "wohnlage", values_from = "delta", names_prefix = "wol_") %>% 
  mutate(
    price_wol_1 = exp(reputation+bE*(wol_1)+bPT),
    price_wol_2 = exp(reputation+bE*(wol_1+wol_2)+bPT),
    price_wol_3 = exp(reputation+bE*(wol_1+wol_2+wol_3)+bPT)
  ) %>% pivot_longer(cols = starts_with("price_"), values_to = "price", names_prefix = "price_wol_", names_to = "wohnlage") %>% 
  mutate(bezirk = bezirk_levels[bezirk], property_type = property_type_levels[property_type]) %>% 
  ggplot(aes(x = price, y = bezirk, fill = wohnlage, color = property_type)) +
  stat_slab(.width = c(.90, .5), alpha = .5) +
  labs(x=NULL, y=NULL)

full_model_post %>%
  pivot_wider(names_from = "wohnlage", values_from = "delta", names_prefix = "wol_") %>% 
  rowwise() %>% 
  mutate(
    price_wol_1 = exp(rnorm(1, reputation+bE*(wol_1)+bPT, sigma)),
    price_wol_2 = exp(rnorm(1, reputation+bE*(wol_1+wol_2)+bPT, sigma)),
    price_wol_3 = exp(rnorm(1, reputation+bE*(wol_1+wol_2+wol_3)+bPT, sigma))
  ) %>% pivot_longer(cols = starts_with("price_"), values_to = "price", names_prefix = "price_wol_", names_to = "wohnlage") %>% 
  mutate(bezirk = bezirk_levels[bezirk], property_type = property_type_levels[property_type]) %>% 
  ggplot(aes(x = price, y = bezirk, fill = wohnlage, color = property_type)) +
  stat_slab(.width = c(.90, .5), alpha = .5) +
  labs(x=NULL, y=NULL) +
  coord_cartesian(xlim = c(0, 500))

bdat <- full_model_post %>% 
  pivot_wider(names_from = "wohnlage", values_from = "delta", names_prefix = "wol_") %>% 
  mutate(
    price_wol_1 = exp(reputation+bE*(wol_1)+bPT),
    price_wol_2 = exp(reputation+bE*(wol_1+wol_2)+bPT),
    price_wol_3 = exp(reputation+bE*(wol_1+wol_2+wol_3)+bPT)
  ) %>% pivot_longer(cols = starts_with("price_"), values_to = "price", names_prefix = "price_wol_", names_to = "wohnlage") %>% 
  mutate(bezirk = bezirk_levels[bezirk], property_type = property_type_levels[property_type]) %>% 
  filter(bezirk %in% c("Mitte", "Tempelhof-Schöneberg", "Neukölln", "Reinickendorf")) %>% 
  select(bezirk, price, wohnlage, .draw, .chain, .iteration, property_type) %>% 
  pivot_wider(names_from = bezirk, values_from = price, names_prefix = "price_")

adat <- bdat %>% ungroup() %>% 
  select(starts_with("price_"))

# Get column names
nm1 <- outer(colnames(adat), colnames(adat), paste, sep="_-_")

# Indices for lower triangular elements (excluding diagonal)
indx1 <- which(lower.tri(nm1, diag=TRUE))

# Calculate pairwise differences
res <- outer(1:ncol(adat), 1:ncol(adat), function(x, y) adat[, x] - adat[, y])

# Set column names for the resulting dataframe
colnames(res) <- nm1
res1 <- res[-indx1]

area <- 10
res1 %>% bind_cols(
  bdat %>% select(-starts_with("price_")) # %>% rename(wohnlage = name)
) %>% 
  pivot_longer(starts_with("price_"), values_to = "price_diff") %>% 
  mutate(
    name = str_replace(name, "_-_", " - ") %>% str_remove_all("price_")
  ) %>% 
  ggplot(aes(x = price_diff, y = name, fill = stat(abs(x) > area))) +
  stat_halfeye() +
  geom_vline(xintercept = c(-area, area), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  labs(x="difference of mean price in €", y=NULL) +
  facet_wrap(~property_type)

bdat <- full_model_post %>% 
  pivot_wider(names_from = "wohnlage", values_from = "delta", names_prefix = "wol_") %>% 
  rowwise() %>% 
  mutate(
    price_wol_1 = exp(rnorm(1, reputation+bE*(wol_1)+bPT, sigma)),
    price_wol_2 = exp(rnorm(1, reputation+bE*(wol_1+wol_2)+bPT, sigma)),
    price_wol_3 = exp(rnorm(1, reputation+bE*(wol_1+wol_2+wol_3)+bPT, sigma))
  ) %>% pivot_longer(cols = starts_with("price_"), values_to = "price", names_prefix = "price_wol_", names_to = "wohnlage") %>% 
  mutate(bezirk = bezirk_levels[bezirk], property_type = property_type_levels[property_type]) %>% 
  filter(bezirk %in% c("Mitte", "Tempelhof-Schöneberg", "Neukölln", "Reinickendorf")) %>% 
  select(bezirk, price, wohnlage, .draw, .chain, .iteration, property_type) %>% 
  pivot_wider(names_from = bezirk, values_from = price, names_prefix = "price_")

adat <- bdat %>% ungroup() %>% 
  select(starts_with("price_"))

# Get column names
nm1 <- outer(colnames(adat), colnames(adat), paste, sep="_-_")

# Indices for lower triangular elements (excluding diagonal)
indx1 <- which(lower.tri(nm1, diag=TRUE))

# Calculate pairwise differences
res <- outer(1:ncol(adat), 1:ncol(adat), function(x, y) adat[, x] - adat[, y])

# Set column names for the resulting dataframe
colnames(res) <- nm1
res1 <- res[-indx1]

df_temp <- res1 %>% bind_cols(
  bdat %>% select(-starts_with("price_")) # %>% rename(wohnlage = name)
) %>% 
  pivot_longer(starts_with("price_"), values_to = "price_diff") %>% 
  mutate(
    name = str_replace(name, "_-_", " - ") %>% str_remove_all("price_")
  )

df_temp %>% left_join(
  df_temp %>% group_by(name, property_type) %>% summarise(p_safe = round(100*mean(price_diff > 0),1)) %>% 
    mutate(p_safe = str_c(p_safe, " %"))
) %>% 
  ggplot(aes(x = price_diff, y = name, fill = stat(x > 0))) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  labs(x="difference of mean price in €", y=NULL) +
  geom_text(aes(x = 150, y = as.integer(as.factor(name))+0.35, label = p_safe)) +
  coord_cartesian(xlim = c(-250, 250)) +
  facet_wrap(~property_type) +
  theme(
    legend.position = "bottom"
  )

p_safe_10 <- df_temp %>% group_by(name, property_type) %>% 
  summarise(p_safe_10 = round(100*mean(price_diff > 10), 2)) %>% 
  # mutate(p_safe_10 = abs(50 - p_safe_10)) %>% 
  arrange(p_safe_10) %>% print(n = 24)

contrast_by <- "bSauna"
full_model_verbose_post %>% 
  pivot_wider(names_from = "wohnlage", values_from = "delta", names_prefix = "wol_") %>% 
  mutate(
    log_price_wol_1 = reputation+bE*(wol_1)+bPropType,
    log_price_wol_2 = reputation+bE*(wol_1+wol_2)+bPropType,
    log_price_wol_3 = reputation+bE*(wol_1+wol_2+wol_3)+bPropType
  ) %>% pivot_longer(cols = starts_with("log_price_"), values_to = "log_price", names_prefix = "log_price_wol_", names_to = "wohnlage") %>% 
  mutate(bezirk = bezirk_levels[bezirk], property_type = property_type_levels[property_type]) %>% 
  mutate(price_diff = exp(log_price+!!rlang::sym(contrast_by))-exp(log_price)) %>% 
  ungroup() %>% 
  mutate(perc = str_c(round(100*mean(price_diff>0), 2), " %"), .before = 1) %>% 
  sample_frac(0.1) %>% 
  ggplot(aes(x = price_diff, fill = stat(x > 0))) +
  stat_halfeye() +
  geom_text(aes(x = 10, 0.05, label = perc)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  labs(x="difference of mean price in €", y=NULL) +
  theme(
    legend.position = "bottom"
  )

full_model_verbose_post %>% 
  pivot_wider(names_from = "wohnlage", values_from = "delta", names_prefix = "wol_") %>% 
  mutate(
    log_price_wol_1 = reputation+bE*(wol_1)+bPropType,
    log_price_wol_2 = reputation+bE*(wol_1+wol_2)+bPropType,
    log_price_wol_3 = reputation+bE*(wol_1+wol_2+wol_3)+bPropType
  ) %>% pivot_longer(cols = starts_with("log_price_"), values_to = "log_price", names_prefix = "log_price_wol_", names_to = "wohnlage") %>% 
  mutate(bezirk = bezirk_levels[bezirk], property_type = property_type_levels[property_type]) %>% 
  rowwise() %>% 
  mutate(price_diff = exp(rnorm(1, log_price+!!rlang::sym(contrast_by), sigma))-exp(rnorm(1, log_price, sigma))) %>% 
  ungroup() %>% 
  mutate(perc = str_c(round(100*mean(price_diff>0), 2), " %"), .before = 1) %>% 
  sample_frac(0.1) %>% 
  ggplot(aes(x = price_diff, fill = stat(x > 0))) +
  stat_halfeye() +
  geom_text(aes(x = 50, 0.05, label = perc)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  labs(x="difference of predicted price in €", y=NULL) +
  theme(
    legend.position = "bottom"
  ) +
  coord_cartesian(xlim = c(-250, 250))

#### ggdag #####

dag <- dagify(
  price ~ u + reputation + wohnlage,
  reputation ~ wohnlage,
  latent = c("u"),
  exposure = "reputation",
  outcome = "price",
  labels = c(price = "price", u = "unobserved", reputation = "reputation", wohnlage = "Wohnlage")
) %>% tidy_dagitty()

ggdag_status(dag, use_labels = "label", text = FALSE) + 
  theme_dag() +
  theme(
    legend.position = "bottom"
  ) +
  geom_dag_edges()

tidy_ggdag <- dagify(
  y ~ x + z2 + w2 + w1,
  x ~ z1 + w1 + w2,
  z1 ~ w1 + v,
  z2 ~ w2 + v,
  w1 ~ ~w2, # bidirected path
  exposure = "x",
  outcome = "y"
) %>%
  tidy_dagitty()

g <- ggdag_adjustment_set(tidy_ggdag)

g

tidy_ggdag <- dagify(
  y ~ z + x + u,
  x ~ z,
  u ~ x + m,
  exposure = "x",
  outcome = "y"
) %>%
  tidy_dagitty()

ggdag_adjustment_set(tidy_ggdag)

dag <- dagify(
  price ~ u + reputation + wohnlage,
  reputation ~ wohnlage,
  review ~ reputation + price,
  latent = c("u"),
  exposure = "reputation",
  outcome = "price",
  labels = c(price = "price", u = "unobserved", reputation = "reputation", wohnlage = "Wohnlage", review = "review")
) %>% tidy_dagitty()

(ggdag_adjustment_set(dag, use_labels = "label", text = FALSE) + 
  theme_dag() +
  theme(
    legend.position = "bottom"
  )) %>% plot_arrows_on_top()

ggdag_collider(dag)
ggdag_paths_fan(dag, adjust_for = "review", spread = 1)

ggdag_adjust(dag, var = c("wohnlage", "review"))

library(dagitty)
dag <- dagitty('dag {
price [outcome,pos="0.638,0.735"]
rep [exposure,pos="-1.843,0.743"]
review [selected,pos="-0.300,-0.082"]
price -> review
rep -> price
rep -> review
}
')

x <- dag %>% ggdag::tidy_dagitty()

impliedConditionalIndependencies(dag)
ggdag::ggdag_status(dag)

mosquito_dag <- dagify(
  malaria_risk ~ net + income + health + temperature + resistance,
  net ~ income + health + temperature + eligible + household,
  eligible ~ income + household,
  health ~ income,
  exposure = "net",
  outcome = "malaria_risk",
  coords = list(x = c(malaria_risk = 7, net = 3, income = 4, health = 5,
                      temperature = 6, resistance = 8.5, eligible = 2, household = 1),
                y = c(malaria_risk = 2, net = 2, income = 3, health = 1,
                      temperature = 3, resistance = 2, eligible = 3, household = 2)),
  labels = c(malaria_risk = "Risk of malaria", net = "Mosquito net", income = "Income",
             health = "Health", temperature = "Nighttime temperatures", 
             resistance = "Insecticide resistance",
             eligible = "Eligible for program", household = "Number in household")
)

ggdag_status(mosquito_dag, use_labels = "label", text = FALSE) + 
  guides(fill = FALSE, color = FALSE) +  # Disable the legend
  theme_dag()

(ggdag_adjustment_set(mosquito_dag, shadow = T,
                     use_labels = "label", text = FALSE) +
  theme_dag()) %>% 
  plot_arrows_on_top()

dag <- dagify(
  price ~ reputation + wohnlage,
  reputation ~ wohnlage,
  review ~ reputation + price,
  # latent = c("u"),
  exposure = "reputation",
  outcome = "price",
  labels = c(price = "price", #u = "unobserved", 
             reputation = "reputation", wohnlage = "Wohnlage", review = "review")
) %>% tidy_dagitty()

(ggdag_adjust(dag, use_labels = "label", text = FALSE, var = c("wohnlage", "review")) + 
    theme_dag() +
    theme(
      legend.position = "bottom"
    )) %>% plot_arrows_on_top()

##### full model #####

library(dagitty)

tidy_dag_adjusted <- full_dag_adjusted %>% ggdag::tidy_dagitty()

(ggdag_status(tidy_dag_adjusted, use_labels = "name", text = FALSE) + 
  guides(fill = FALSE, color = FALSE) +  # Disable the legend
  theme_dag()) %>% 
  plot_arrows_on_top()

m <- ggdag_adjustment_set(
  tidy_dag_adjusted, shadow = T, use_labels = "name", text = FALSE
)

m + theme(
  strip.background = element_blank(),
  strip.text.x = element_blank()
)

(ggdag_adjustment_set(
  tidy_dag_adjusted, shadow = T, use_labels = "name", text = FALSE
  ) +
    theme_dag()) %>% 
  plot_arrows_on_top()

tidy_dag <- full_dag %>% ggdag::tidy_dagitty()

(ggdag_adjustment_set(
  tidy_dag, shadow = T, use_labels = "name", text = FALSE
) +
    theme_dag()) %>% 
  plot_arrows_on_top()

((ggdag_status(full_dag, use_labels = "name", text = FALSE) + 
    scale_fill_manual(
      labels = c("exposure", "outcome", "latent"), 
      values = c("#e41a1c", "#377eb8", "#4daf4a"),
      limits = c("exposure", "outcome", "latent")
    ) +
    scale_color_manual(
      labels = c("exposure", "outcome", "latent"), 
      values = c("#e41a1c", "#377eb8", "#4daf4a"),
      limits = c("exposure", "outcome", "latent")
    ) +
    theme_dag() +
    theme(
      legend.position = "bottom"
    )) %>% plot_arrows_on_top() +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0.6, ymax = 0.875), fill = NA, color = "purple", size = 2) +
  geom_rect(aes(xmin = 0, xmax = 0.67, ymin = 0.3, ymax = 0.55), fill = NA, color = "green", size = 2) +
  geom_rect(aes(xmin = 0.68, xmax = 1, ymin = 0.3, ymax = 0.575), fill = NA, color = "orange", size = 2)
  ) %>% layer_on_bottom(5) %>% layer_on_bottom(6) %>% layer_on_bottom(7)

#### point #####

coords_hauptbahnhof <- tribble(
  ~name, ~lat, ~lon,
  "Hauptbahnhof", 52.52507464195593, 13.369127492553043
) %>% 
sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

geo_elect_units %>% 
  ggplot() + 
  geom_sf(
    mapping = aes(geometry = geometry, fill = BEZNAME)
  ) + geom_sf(
    data = coords_hauptbahnhof,
    mapping = aes(geometry = geometry)
  )

#####

base_model <- readRDS("saved_objects/base_model.rds")
base_model_student <- readRDS("saved_objects/base_model_student.rds")

base_model_reputation_wohnlage <- readRDS("saved_objects/base_model_reputation_wohnlage.rds")
base_model_reputation_wohnlage_student <- readRDS("saved_objects/base_model_reputation_wohnlage_student.rds")

bezirk_levels <- df_airbnb %>% 
  transmute(bezirk = as.factor(neighbourhood_group_cleansed)) %>% 
  pull() %>% levels()

property_type_levels <- df_airbnb %>%
  transmute(property_type = as.factor(room_type)) %>%
  pull() %>% levels()

(ggdag_adjustment_set(full_dag_adjusted, use_labels = "name", text = FALSE, shadow = T) + 
    theme_dag() +
    theme(
      legend.position = "bottom",
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )) %>% plot_arrows_on_top()

#### interactive ####

full_model_verbose_post <- readRDS("saved_objects/full_model_verbose_post.rds")
full_model_verbose_post_summary <- readRDS("saved_objects/full_model_verbose_post_summary.rds") %>% 
  select(-rhat, -ess_bulk) %>% 
  filter(!str_starts(parameter, "delta")) %>% 
  filter(!str_starts(parameter, "reputation")) %>% 
  filter(!str_starts(parameter, "bPropType")) %>% 
  filter(parameter != "sigma") %>% 
  mutate("total effect" = parameter %in% direct_effect_only)

full_model_verbose_post %>% 
  pivot_wider(names_from = "wohnlage", values_from = "delta", names_prefix = "wol_") %>% 
  mutate(
    log_price_wol_1 = reputation+bE*(wol_1)+bPropType,
    log_price_wol_2 = reputation+bE*(wol_1+wol_2)+bPropType,
    log_price_wol_3 = reputation+bE*(wol_1+wol_2+wol_3)+bPropType
  ) %>% pivot_longer(cols = starts_with("log_price_"), values_to = "log_price", names_prefix = "log_price_wol_", names_to = "wohnlage") %>% 
  mutate(bezirk = bezirk_levels[bezirk], property_type = property_type_levels[property_type]) %>% 
  pivot_longer(full_model_verbose_post_summary$parameter) %>% 
  sample_frac(0.01) %>% 
  filter(name == "bSauna") %>% 
  rowwise() %>% 
  mutate(price_diff = exp(rnorm(1, log_price+value, sigma))-exp(rnorm(1, log_price, sigma))) %>% 
  group_by(name) %>% 
  mutate(perc = str_c(round(100*mean(price_diff>0), 2), " %"), .before = 1) %>% 
  ggplot(aes(x = price_diff, fill = property_type)) +
  geom_density(alpha = .5) +
  geom_text(aes(x = 10, 0.05, label = perc)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  # scale_fill_manual(values = c("gray80", "skyblue")) +
  labs(x="difference of mean price in €", y=NULL) +
  theme(
    legend.position = "bottom"
  )

contrast_by <- "bSauna"
full_model_verbose_post %>% 
  pivot_wider(names_from = "wohnlage", values_from = "delta", names_prefix = "wol_") %>% 
  mutate(
    log_price_wol_1 = reputation+bE*(wol_1)+bPropType,
    log_price_wol_2 = reputation+bE*(wol_1+wol_2)+bPropType,
    log_price_wol_3 = reputation+bE*(wol_1+wol_2+wol_3)+bPropType
  ) %>% pivot_longer(cols = starts_with("log_price_"), values_to = "log_price", names_prefix = "log_price_wol_", names_to = "wohnlage") %>% 
  mutate(bezirk = bezirk_levels[bezirk], property_type = property_type_levels[property_type]) %>% 
  mutate(price_diff = exp(log_price+!!rlang::sym(contrast_by))-exp(log_price)) %>% 
  ungroup() %>% 
  mutate(perc = str_c(round(100*mean(price_diff>0), 2), " %"), .before = 1) %>% 
  sample_frac(0.1) %>% 
  ggplot(aes(x = price_diff, fill = stat(x > 0))) +
  stat_halfeye() +
  geom_text(aes(x = 10, 0.05, label = perc)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  labs(x="difference of mean price in €", y=NULL) +
  theme(
    legend.position = "bottom"
  )

full_model_verbose_post %>% 
  pivot_wider(names_from = "wohnlage", values_from = "delta", names_prefix = "wol_") %>% 
  mutate(
    log_price_wol_1 = reputation+bE*(wol_1)+bPropType,
    log_price_wol_2 = reputation+bE*(wol_1+wol_2)+bPropType,
    log_price_wol_3 = reputation+bE*(wol_1+wol_2+wol_3)+bPropType
  ) %>% pivot_longer(cols = starts_with("log_price_"), values_to = "log_price", names_prefix = "log_price_wol_", names_to = "wohnlage") %>% 
  mutate(bezirk = bezirk_levels[bezirk], property_type = property_type_levels[property_type]) %>% 
  rowwise() %>% 
  mutate(price_diff = exp(rnorm(1, log_price+!!rlang::sym(contrast_by), sigma))-exp(rnorm(1, log_price, sigma))) %>% 
  ungroup() %>% 
  mutate(perc = str_c(round(100*mean(price_diff>0), 2), " %"), .before = 1) %>% 
  sample_frac(0.1) %>% 
  ggplot(aes(x = price_diff, fill = stat(x > 0))) +
  stat_halfeye() +
  geom_text(aes(x = 50, 0.05, label = perc)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  labs(x="difference of predicted price in €", y=NULL) +
  theme(
    legend.position = "bottom"
  ) +
  coord_cartesian(xlim = c(-250, 250))

full_model_post <- readRDS("saved_objects/full_model_post.rds")
full_model_post %>%
  pivot_wider(names_from = "wohnlage", values_from = "delta", names_prefix = "wol_") %>% 
  rowwise() %>% 
  mutate(
    price_wol_1 = rnorm(1, reputation+bE*(wol_1)+bPT, sigma),
    price_wol_2 = rnorm(1, reputation+bE*(wol_1+wol_2)+bPT, sigma),
    price_wol_3 = rnorm(1, reputation+bE*(wol_1+wol_2+wol_3)+bPT, sigma)
  ) %>% pivot_longer(cols = starts_with("price_"), values_to = "price", names_prefix = "price_wol_", names_to = "wohnlage") %>% 
  mutate(bezirk = bezirk_levels[bezirk], property_type = property_type_levels[property_type]) %>% 
  group_by(bezirk) %>% summarise(median_price = exp(median(price))) %>% 
  arrange(median_price)

#### final plot ####

df_temp <- full_model_post %>%
  pivot_wider(names_from = "wohnlage", values_from = "delta", names_prefix = "wol_") %>% 
  rowwise() %>% 
  mutate(
    price_wol_1 = exp(rnorm(1, reputation+bE*(wol_1)+bPT, sigma)),
    price_wol_2 = exp(rnorm(1, reputation+bE*(wol_1+wol_2)+bPT, sigma)),
    price_wol_3 = exp(rnorm(1, reputation+bE*(wol_1+wol_2+wol_3)+bPT, sigma))
  ) %>% pivot_longer(cols = starts_with("price_"), values_to = "price", names_prefix = "price_wol_", names_to = "wohnlage") %>% 
  mutate(bezirk = bezirk_levels[bezirk], property_type = property_type_levels[property_type]) %>% 
  group_by(bezirk) %>% summarise(median_price = median(price), sd_price = sd(price)) %>% 
  arrange(median_price) %>% rowid_to_column() %>% 
  mutate(type = "full model") %>% 
  bind_rows(
    df_airbnb %>% group_by(neighbourhood_group_cleansed) %>%
      summarise(median_price = median(price), sd_price = sd(price)) %>% 
      arrange(median_price) %>% rowid_to_column() %>% 
      mutate(type = "raw data") %>% rename(bezirk = neighbourhood_group_cleansed)
  ) %>% mutate(
    median_price = round(median_price),
    type = fct_rev(ordered(type))
    )

df_temp %>% 
  ggplot() +
  geom_col(aes(x = median_price, y = bezirk, group = type, fill = as.ordered(rowid), color = type), 
           position = "dodge2", size = 1)

df_temp2 <- full_model_post %>%
  pivot_wider(names_from = "wohnlage", values_from = "delta", names_prefix = "wol_") %>% 
  rowwise() %>% 
  mutate(
    price_wol_1 = exp(rnorm(1, reputation+bE*(wol_1)+bPT, sigma)),
    price_wol_2 = exp(rnorm(1, reputation+bE*(wol_1+wol_2)+bPT, sigma)),
    price_wol_3 = exp(rnorm(1, reputation+bE*(wol_1+wol_2+wol_3)+bPT, sigma))
  ) %>% pivot_longer(cols = starts_with("price_"), values_to = "price", names_prefix = "price_wol_", names_to = "wohnlage") %>% 
  mutate(bezirk = bezirk_levels[bezirk], property_type = property_type_levels[property_type]) %>% 
  select(bezirk, price) %>% mutate(type = "full model") %>% 
  group_by(bezirk) %>% 
  mutate(median_price = median(price))

df_temp3 <- df_airbnb %>% rename(Bezirk = neighbourhood_group_cleansed) %>% 
  group_by(Bezirk) %>% 
  summarise("mean price" = mean(price), "median price" = median(price), "#Airbnbs" = n()) %>% 
  mutate(size = -scale(`#Airbnbs`)) %>% rename(bezirk = Bezirk)

# install.packages("CGPfunctions")
# install.packages("ggthemes")
library(CGPfunctions)

CGPfunctions::newggslopegraph(
  dataframe = df_temp,
  Times = type,
  Measurement = median_price,
  Grouping = bezirk,
  Title = NULL,
  SubTitle = NULL,
  Caption = NULL,
  ThemeChoice = "gdocs",
  LineColor = bezirk_colors
  ) +
  geom_point(data = df_temp %>% filter(type == "full model") %>% mutate(size = scale(sd_price)), aes(size = size, color = bezirk), shape = 1) +
  geom_point(data = df_temp3, aes(size = size, color = bezirk, fill = bezirk, y = `median price`, x = "raw data"), shape = 1) +
  scale_size_continuous(range = c(7, 14))

ggplot() +
  geom_violin(data = df_temp2, aes(y = bezirk, x = price, fill = bezirk)) +
  geom_boxplot(data = df_temp2, aes(y = bezirk, x = price), width = .2, fill = I("white")) +
  coord_cartesian(xlim = c(0, 250))

gg1 <- df_temp2 %>% ungroup() %>% mutate(bezirk = fct_reorder(bezirk, median_price)) %>% 
  ggplot(aes(y = bezirk, x = price, fill = bezirk)) +
  stat_halfeye() +
  # stat_halfeye(data = df_airbnb %>% rename(bezirk = neighbourhood_group_cleansed) %>% mutate(type = "raw data"), aes(y = bezirk, x = price, fill = bezirk)) +
  coord_cartesian(xlim = c(0, 250)) +
  scale_fill_manual(
    values = bezirk_colors
  ) +
  facet_wrap(~type) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title.position = "top",
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "null"),
    panel.margin = unit(c(0, 0, 0, 0), "null")
  ) +
  ylab(NULL)

gg2 <- df_airbnb %>% 
  group_by(neighbourhood_group_cleansed) %>% mutate(median_price = median(price)) %>% 
  ungroup() %>% 
  mutate(bezirk = fct_reorder(neighbourhood_group_cleansed, median_price)) %>% 
  mutate(type = "raw data") %>% 
  ggplot(aes(y = bezirk, x = price, fill = bezirk)) +
  # stat_halfeye(data = df_temp2 %>% ungroup() %>% mutate(bezirk = fct_reorder(bezirk, median_price)), aes(y = bezirk, x = price, fill = bezirk)) +
  stat_halfeye() +
  coord_cartesian(xlim = c(0, 200)) +
  scale_fill_manual(
    values = bezirk_colors
  ) +
  facet_wrap(~type) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title.position = "top",
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "null"),
    panel.margin = unit(c(0, 0, 0, 0), "null")
  ) +
  ylab(NULL)

ggpubr::ggarrange(gg2, gg1, common.legend = TRUE, legend = "bottom")

df_temp3$size %>% hist()

df_airbnb %>% rename(bezirk = neighbourhood_group_cleansed) %>% mutate(type = "raw data")


gg3 <- df_airbnb %>% 
  group_by(neighbourhood_group_cleansed) %>% mutate(median_price = median(price)) %>% 
  ungroup() %>% 
  mutate(bezirk = fct_reorder(neighbourhood_group_cleansed, median_price)) %>% 
  pull(bezirk) %>% levels() %>% data.frame(bezirk = ., rank = 1:12) %>% 
  mutate(type = "raw data") %>% bind_rows(
    df_temp2 %>% ungroup() %>% mutate(bezirk = fct_reorder(bezirk, median_price)) %>% 
      pull(bezirk) %>% levels() %>% data.frame(bezirk = ., rank = 1:12) %>% 
      mutate(type = "full model")
  ) %>% 
  mutate(rank = ordered(rank)) %>% 
  ungroup() %>% as_tibble() %>% 
  ggplot(aes(color = bezirk, x = type, y = rank, group = bezirk)) +
  geom_line(size = 2,
            alpha = 0.5) +
  geom_point(size = 3) +
  scale_color_manual(
    values = bezirk_colors,
    guide = "none"
  ) +
  theme_void() +
  scale_x_discrete(limits=rev) +
  coord_cartesian(ylim = c(-.5,13.5)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "null"),
    panel.margin = unit(c(0, 0, 0, 0), "null")
  )

ggpubr::ggarrange(
  gg2, gg3, gg1, 
  common.legend = TRUE, legend = "bottom", 
  nrow = 1, widths = c(1, 0.5, 1))

#### residual map ####

# predicting price for any given Airbnb and calculate the difference to actual price
# show difference on map with colored scatter plot

#### max price difference ####

max_diff <- readRDS("saved_objects/full_model_verbose_post_summary.rds") %>% 
  filter(!str_starts(parameter, "reputation")) %>% 
  filter(!str_starts(parameter, "sigma")) %>% 
  filter(!str_starts(parameter, "delta")) %>% 
  filter(!str_starts(parameter, "bProp")) %>% 
  pull(mean) %>% abs() %>% sum()

full_model_post <- readRDS("./saved_objects/full_model_post.rds")
full_model_post %>% filter(wohnlage == 1) %>% mutate(
  price = reputation+bE*delta,
  price_max = price+max_diff/2
  ) %>% # pivot_longer(cols = c(price, price_max)) %>% 
  rowwise() %>% 
  mutate(
    price = exp(rnorm(1, price, sigma)),
    price_max = exp(rnorm(1, price_max, sigma)),
    price_diff = price_max - price
    ) %>% 
  ungroup() %>% 
  mutate(p_safe = str_c(round(100*mean(price_diff > 0), 1), " %")) %>% 
  ggplot(aes(x = price_diff, fill = stat(x > 0))) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"), guide = "none") +
  labs(x="difference of predicted price in €", y=NULL) +
  geom_text(aes(x = 250, 0.125, label = p_safe))
  coord_cartesian(xlim = c(-150, 850))
  
