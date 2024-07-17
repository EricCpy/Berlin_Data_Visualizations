source("code/setup.R")

listings = read.csv("./data/airbnb/March_2024/listings.csv")

bezirk_stats = listings %>% count(neighbourhood_group, sort=TRUE); bezirk_stats

raw<- sf::read_sf('data/lor/Bezirke/lor_bzr.shp')
raw<- sf::read_sf('data/lor/Planung/lor_plr.shp')
raw<- sf::read_sf('data/lor/Prognose/lor_pgr.shp')

ggplot2::ggplot(raw) + 
  ggplot2::geom_sf() + 
  ggplot2::theme_void()
