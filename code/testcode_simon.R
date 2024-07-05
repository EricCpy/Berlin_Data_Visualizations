source("code/setup.R")

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
  as_tibble()

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
    data = opnv_rails, mapping = aes(geometry = geometry), color = "red") +
  geom_sf(
    data = opnv_stations, mapping = aes(geometry = geometry), color = "green"
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

#### OPNV and WFS ####

library(ows4R)
library(httr)

opnv_api <- "https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_sbu_oepnv_l?REQUEST=GetCapabilities&SERVICE=wfs"
bwk_client <- WFSClient$new(opnv_api, 
                            serviceVersion = "2.0.0")
bwk_client$getFeatureTypes(pretty = TRUE)

bwk_client$
  getCapabilities()$
  findFeatureTypeByName("fis:s_sbu_oepnv_l")$
  getDescription() %>%
  map_chr(function(x){x$getName()})

url <- parse_url("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_sbu_oepnv_l")
url$query <- list(
  service = "WFS",
  #version = "2.0.0", # optional
  request = "GetFeature"
  #bbox = "142600,153800,146000,156900"
)
request <- build_url(url)

trains <- read_sf(request) # gives an error; can't be downloaded completly in qgis as well (only net not stations)
