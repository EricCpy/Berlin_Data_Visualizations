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

airbnb <- rio::import("data/airbnb/March_2024/listings.csv") %>% 
  as_tibble()

# p <- st_sfc(st_point(c(13.4181, 52.53471)), crs = 4326) %>%
#   st_transform(9311)

points <- airbnb[, c("longitude", "latitude")] |> 
  as.matrix() |> 
  st_multipoint() |> 
  st_sfc(crs = 4326) |> 
  st_cast('POINT') 

airbnb_with_elect_units <- st_intersection(
  elect_units %>% st_transform(9311),
  points %>% st_transform(9311)
  ) %>% st_transform(4326) %>% 
  bind_cols(airbnb)

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

#### Match LOR and WB ####

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
