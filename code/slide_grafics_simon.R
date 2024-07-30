df_airbnb %>% rename(Bezirk = neighbourhood_group_cleansed) %>% 
  group_by(Bezirk) %>% 
  summarise("mean price" = mean(price), "median price" = median(price), "#Airbnbs" = n()) %>%
  pivot_longer()

