source("code/setup.R")

elect_units <- sf::st_read(dsn = "data/Wahlbezirke") %>% 
  st_make_valid(ElectUnits, tol = 0.00001)
table(st_is_valid(elect_units))
crs <- st_crs(elect_units)

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
