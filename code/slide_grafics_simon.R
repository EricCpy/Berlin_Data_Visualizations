df_airbnb %>% rename(Bezirk = neighbourhood_group_cleansed) %>% 
  group_by(Bezirk) %>% 
  summarise("mean price" = mean(price), "median price" = median(price), "#Airbnbs" = n()) %>%
  mutate(Bezirk = fct_reorder(Bezirk, `median price`)) %>% 
  pivot_longer(cols = ends_with("price")) %>% 
  filter(name == "median price") %>% 
  ggplot(aes(x = value, y = Bezirk, fill = Bezirk, group = name#, color = name
             )) +
  geom_col(position = "dodge2", size = 1) +
  scale_fill_manual(
    values = bezirk_colors
  ) +
  scale_color_manual(values = c("median price" = "black","mean price" = "white")) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title.position = "top"
  )

