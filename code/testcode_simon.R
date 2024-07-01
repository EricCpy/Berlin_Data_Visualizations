source("code/setup.R")

elect_units <- sf::st_read(dsn = "data/Wahlbezirke") %>% 
  st_make_valid(tol = 0.00001) %>% st_transform(4326)
table(st_is_valid(elect_units))
crs <- st_crs(elect_units)

# opnv <- sf::st_read(dsn = "data/opnv/") %>% 
#   st_make_valid(tol = 0.00001) %>% st_transform(4326)
# table(st_is_valid(opnv))

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
  )

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
  # geom_sf(
  #   data = opnv, mapping = aes(geometry = geometry), color = "red") +
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
