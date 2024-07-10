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

airbnb_with_lor_units <- st_intersection(
  lor %>% st_transform(9311),
  points %>% st_transform(9311)
) %>% st_transform(4326) %>% 
  bind_cols(airbnb)

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
  summarise(count = n())

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

#### rents ####

# rent_data <- sf::st_read(dsn = "data/Mieten/") %>% 
#   st_make_valid(tol = 0.00001) %>% st_transform(4326)
# 
# rent_data_with_elect_units <- st_intersection(
#   elect_units %>% st_transform(9311),
#   rent_data %>% st_transform(9311)
# ) %>% st_transform(4326)
# 
# elect_units_with_aggregated_rent_data <- rent_data_with_elect_units %>% mutate(
#   wol = ordered(wol, levels = c("einfach", "mittel", "gut"))
# ) %>% 
#   group_by(UWB) %>% summarise(
#   wohnlage = mean(as.numeric(wol))
# )
# 
# airbnb_with_rent_data <- airbnb_with_elect_units %>% left_join(
#   elect_units_with_aggregated_rent_data %>% as_tibble(),
#   by = c("UWB" = "UWB")
# )
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
#   test = airbnb[, c("longitude", "latitude")],
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
# airbnb_with_rent_data_median <- airbnb_with_elect_units %>% left_join(
#   elect_units_with_aggregated_rent_data_median %>% as_tibble(),
#   by = c("UWB" = "UWB")
# )
# 
# airbnb_with_rent_data_median_knn <- model_knn %>% as_tibble() %>% mutate(
#   wol = ordered(value, levels = c("einfach", "mittel", "gut")),
#   knn_wohnlage = as.numeric(wol),
#   median_wohnlage = airbnb_with_rent_data_median$wohnlage,
#   price = airbnb_with_rent_data_median$price,
#   airbnb_id = airbnb_with_rent_data_median$id
#   )
# airbnb_with_rent_data_median_knn %>% select(knn_wohnlage, median_wohnlage) %>% table()

airbnb_with_rent_data_median_knn %>% saveRDS("saved_objects/airbnb_with_rent_data_median_knn.rds")
airbnb_with_rent_data_median_knn <- readRDS("saved_objects/airbnb_with_rent_data_median_knn.rds")

lm_rents1 <- lm(log(price) ~ wohnlage, data = airbnb_with_rent_data)
summary(lm_rents1)

anova(lm_base, lm_accidents1, lm_crime1, lm_rents1)

lm_rents2 <- lm(log(price) ~ knn_wohnlage, data = airbnb_with_rent_data_median_knn)
summary(lm_rents2)

anova(lm_rents1, lm_rents2)

#### Regression Part 2 ####

library(brms)

lm_sentiment_for_name <- lm(data = df_airbnb_march2024, log(price) ~ sentiment_for_name)
summary(lm_sentiment_for_name)

contrasts(df_airbnb_march2024$sentiment_for_name) = contr.treatment(3)
lm_sentiment_for_name2 <- lm(data = df_airbnb_march2024, log(price) ~ sentiment_for_name)
summary(lm_sentiment_for_name2)

df_clean %>% group_by(sentiment_for_name) %>% 
  summarise(mean_price = mean(price), median_price = median(price))

blm_sentiment_for_name <- brm(
  data = df_clean, 
  price ~ sentiment_for_name, 
  family = "lognormal"
)

# exp(4.603887)
# exp(4.603887+0.053228)
# exp(4.603887+0.053228+0.009651)

blm_sentiment_for_name2 <- brm(
  data = df_clean, 
  price ~ mo(sentiment_for_name), 
  family = "lognormal"
)

fixef(blm_sentiment_for_name2)
ranef(blm_sentiment_for_name2)
brms::
  conditional_effects(blm_sentiment_for_name2)
