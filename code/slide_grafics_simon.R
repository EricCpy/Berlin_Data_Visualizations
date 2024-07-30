#### raw data ####

gg1 <- df_airbnb %>% rename(Bezirk = neighbourhood_group_cleansed) %>% 
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
  labs(
    x = "price in €",
    y = NULL,
    title = "Median Airbnb prices",
    subtitle = "Broken down according to districts"
  ) +
  theme(
    legend.position = "bottom",
    legend.title.position = "top",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
  )

q25_mitte <- df_airbnb %>% filter(neighbourhood_group_cleansed == "Mitte") %>% 
  pull(price) %>% quantile(.25)
q75_reinickendorf <- df_airbnb %>% filter(neighbourhood_group_cleansed == "Reinickendorf") %>% 
  pull(price) %>% quantile(.75)
  
gg2 <- df_airbnb %>% 
  group_by(neighbourhood_group_cleansed) %>% 
  mutate(median_price = median(price)) %>% 
  ungroup() %>% 
  mutate(Bezirk = fct_reorder(neighbourhood_group_cleansed, `median_price`)) %>% 
  ggplot(
    aes(
    y = Bezirk, 
    x = price, 
    fill = Bezirk)
  ) +
  geom_violin() +
  geom_boxplot(width = 0.1, fill = "white") +
  scale_fill_manual(values = bezirk_colors) +
  theme(
    legend.position = "bottom",
    legend.title.position = "top"
  ) +
  labs(
    x = "price in €",
    y = NULL,
    title = "Airbnb price distribution",
    subtitle = "Broken down according to districts"
  ) +
  # coord_cartesian(xlim = c(0, 250)) +
  geom_vline(xintercept = q25_mitte, lty = 2, color = "black") +
  geom_vline(xintercept = q75_reinickendorf, lty = 2, color = "black")

gg_raw_data <- ggpubr::ggarrange(
  gg2, gg1,
  common.legend = TRUE, legend = "right",
  widths = c(3,2)
  )

ggsave(plot = gg_raw_data, units = "px", filename = "images_presentation/price_by_district.png", width = 4000, height = 1700, dpi = 300)

#### simple dag ####

dag <- dagify(
  price ~ u + reputation,
  latent = c("u"),
  exposure = "reputation",
  outcome = "price",
  labels = c(price = "price", u = "unobserved", reputation = "reputation")
) %>% tidy_dagitty()

gg_simple_dag <- (ggdag_status(dag, use_labels = "label", text = FALSE) + 
    scale_fill_manual(
      labels = c("exposure", "latent", "outcome"), 
      values = c("#e41a1c", "#4daf4a", "#377eb8")
    ) +
    scale_color_manual(
      labels = c("exposure", "latent", "outcome"), 
      values = c("#e41a1c", "#4daf4a", "#377eb8")
    ) +
    theme_dag() +
    labs(
      title = "Basic model for Airbnb prices"
    ) +
    theme(
      legend.position = "bottom"
    )) %>% plot_arrows_on_top()

ggsave(plot = gg_simple_dag, units = "px", filename = "images_presentation/simple_dag.png", width = 1500, height = 1500, dpi = 300)

#### simulation ####

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

n_bezirk <- 12
bezirke <- sample(1:12, n, replace = TRUE)
b_bezirk_reputation <- rnorm(12, 0, 0.5)
b_bezirk_reputation_distr <- rnorm(n, b_bezirk_reputation[bezirke], 0.2)
prior_bezirke = rnorm(n, sample_mu+b_bezirk_reputation_distr, sample_sigma)

sim_reputation_only_model <- tibble(
  bezirk = bezirke,
  sim_reputation = prior_bezirke,
  original = df_airbnb$log_price
)

gg_simulation <- sim_reputation_only_model %>% 
  select(-original) %>% 
  mutate(price = exp(sim_reputation)) %>% 
  mutate(bezirk = str_c("Bezirk ", str_pad(bezirk, 2, "0", side = "left"))) %>% 
  ggplot(
    aes(
      x = price,
      y = bezirk,
      fill = bezirk
    )) +
  geom_violin() +
  geom_boxplot(width = 0.2, fill = "white") +
  scale_fill_discrete(guide="none") +
  labs(
    y= NULL,
    x ="price in €",
    title = "Simulated prices for Airbnbs",
    subtitle = "Districts do not correspond to any real districts"
    ) +
  coord_cartesian(xlim = c(0, 1000))

ggsave(plot = gg_simulation, units = "px", filename = "images_presentation/simulation.png", width = 1500, height = 1500, dpi = 300)

#### full dag ####

gg_dag_full <- ((ggdag_status(full_dag, use_labels = "name", text = FALSE) + 
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
              geom_rect(aes(xmin = 0, xmax = 0.65, ymin = 0.3, ymax = 0.55), fill = NA, color = "green", size = 2) +
              geom_rect(aes(xmin = 0.68, xmax = 1, ymin = 0.3, ymax = 0.575), fill = NA, color = "orange", size = 2)
) %>% layer_on_bottom(5) %>% layer_on_bottom(6) %>% layer_on_bottom(7)

ggsave(plot = gg_dag_full, units = "px", filename = "images_presentation/full_dag.png", width = 2500, height = 1700, dpi = 300)

#### 

base_model_post <- readRDS("saved_objects/base_model_post.rds")

g_param_norm <- base_model_post %>% 
  mutate(price = exp(mu)) %>% 
  ggplot(aes(x = price)) +
  stat_halfeye(.width = c(.90, .5)) +
  labs(x=NULL, y=NULL) +
  coord_cartesian(xlim = c(100, 108))

g_pred_norm <- base_model_post %>% 
  mutate(price = exp(rnorm(1000, mu, sigma))) %>% 
  ggplot(aes(x = price)) +
  stat_halfeye(.width = c(.90, .5)) +
  labs(x=NULL, y=NULL) +
  xlim(0, 800)

p <- list(g_param_norm, g_pred_norm)

yleft = gridtext::richtext_grob("density", rot=90)
bottom = gridtext::richtext_grob(
  text = 'price in €'
)
title = gridtext::richtext_grob("Parameter estimation and posterior predictions")

gg_param_pred <- gridExtra::grid.arrange(
  grobs = p, 
  nrow = 2, 
  left = yleft, bottom = bottom, top = title
)

ggsave(plot = gg_param_pred, units = "px", filename = "images_presentation/param_pred.png", width = 4000/3, height = 1700, dpi = 300)

####

base_model_reputation_wohnlage_post <- readRDS("saved_objects/base_model_reputation_wohnlage_post.rds")

gg_param_norm <- base_model_reputation_wohnlage_post %>% 
  pivot_wider(names_from = "wohnlage", values_from = "delta", names_prefix = "wol_") %>% 
  mutate(
    price_wol_1 = exp(reputation+bE*(wol_1)),
    price_wol_2 = exp(reputation+bE*(wol_1+wol_2)),
    price_wol_3 = exp(reputation+bE*(wol_1+wol_2+wol_3))
  ) %>% pivot_longer(cols = starts_with("price_"), values_to = "price", names_prefix = "price_wol_", names_to = "wohnlage") %>% 
  mutate(bezirk = bezirk_levels[bezirk]) %>% 
  ggplot(aes(x = price, y = bezirk, fill = wohnlage)) +
  stat_slab(.width = c(.90, .5), alpha = .5) +
  labs(x="price in €", y=NULL) +
  theme(
    legend.position = "left"
  )

bdat <- base_model_reputation_wohnlage_post %>% 
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
gg_param_contrast <- res1 %>% bind_cols(
  bdat %>% select(-starts_with("price_")) # %>% rename(wohnlage = name)
) %>% 
  pivot_longer(starts_with("price_"), values_to = "price_diff") %>% 
  mutate(
    name = str_replace(name, "_-_", " - ") %>% str_remove_all("price_")
  ) %>% 
  ggplot(aes(x = price_diff, y = name, fill = stat(abs(x) > area))) +
  stat_halfeye() +
  geom_vline(xintercept = c(-area, area), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"), guide="none") +
  labs(x="difference of mean price in €", y=NULL) +
  theme(
    legend.position = "bottom"
  )

title = gridtext::richtext_grob("Parameter estimations and conrasts of parameter pairs")

gg_param <- gridExtra::grid.arrange(
  grobs = list(gg_param_norm, gg_param_contrast), 
  nrow = 2,
  top = title
)

ggsave(plot = gg_param, units = "px", filename = "images_presentation/param.png", width = 4000/3, height = 1700, dpi = 300)

####

gg_pred_norm <- base_model_reputation_wohnlage_post %>% 
  pivot_wider(names_from = "wohnlage", values_from = "delta", names_prefix = "wol_") %>% 
  rowwise() %>% 
  mutate(
    price_wol_1 = exp(rnorm(1, reputation+bE*(wol_1), sigma)),
    price_wol_2 = exp(rnorm(1, reputation+bE*(wol_1+wol_2), sigma)),
    price_wol_3 = exp(rnorm(1, reputation+bE*(wol_1+wol_2+wol_3), sigma))
  ) %>% pivot_longer(cols = starts_with("price_"), values_to = "price", names_prefix = "price_wol_", names_to = "wohnlage") %>% 
  mutate(bezirk = bezirk_levels[bezirk]) %>% 
  ggplot(aes(x = price, y = bezirk, fill = wohnlage)) +
  stat_slab(.width = c(.90, .5), alpha = .5) +
  labs(x="price in €", y=NULL) +
  coord_cartesian(xlim = c(0, 300)) +
  theme(
    legend.position = "left"
  )

bdat <- base_model_reputation_wohnlage_post %>% 
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

df_temp <- res1 %>% bind_cols(
  bdat %>% select(-starts_with("price_")) # %>% rename(wohnlage = name)
) %>% 
  pivot_longer(starts_with("price_"), values_to = "price_diff") %>% 
  mutate(
    name = str_replace(name, "_-_", " - ") %>% str_remove_all("price_")
  )

gg_pred_contrast <- df_temp %>% left_join(
  df_temp %>% group_by(name) %>% summarise(p_safe = round(100*mean(price_diff > 0),1)) %>% 
    mutate(p_safe = str_c(p_safe, " %"))
) %>% 
  ggplot(aes(x = price_diff, y = name, fill = stat(x > 0))) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"), guide = "none") +
  labs(x="difference of predicted price in €", y=NULL) +
  geom_text(aes(x = 80, y = as.integer(as.factor(name))+0.3, label = p_safe)) +
  coord_cartesian(xlim = c(-350, 350)) +
  theme(
    legend.position = "bottom"
  )

title = gridtext::richtext_grob("Posterior predictions and conrasts of parameter pairs")

gg_pred <- gridExtra::grid.arrange(
  grobs = list(gg_pred_norm, gg_pred_contrast), 
  nrow = 2,
  top = title
)

ggsave(plot = gg_pred, units = "px", filename = "images_presentation/pred.png", width = 4000/3, height = 1700, dpi = 300)

####

full_model_post <- readRDS("saved_objects/full_model_post.rds")

ggtheme <- theme_get()

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

df_temp3 <- df_airbnb %>% rename(Bezirk = neighbourhood_group_cleansed) %>% 
  group_by(Bezirk) %>% 
  summarise("mean price" = mean(price), "median price" = median(price), "#Airbnbs" = n()) %>% 
  mutate(size = -scale(`#Airbnbs`)) %>% rename(bezirk = Bezirk)

gg_news <- CGPfunctions::newggslopegraph(
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

theme_set(ggtheme)

ggsave(plot = gg_news, units = "px", filename = "images_presentation/news_rank.png", width = 4000/2, height = 1700, dpi = 300)

####

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
  mutate(median_price = median(price)) %>% 
  ungroup() %>% mutate(bezirk = fct_reorder(bezirk, median_price))

gg1 <- df_temp2 %>% 
  ggplot(aes(y = bezirk, x = price, fill = bezirk)) +
  stat_halfeye() +
  # stat_halfeye(data = df_airbnb %>% rename(bezirk = neighbourhood_group_cleansed) %>% mutate(type = "raw data"), aes(y = bezirk, x = price, fill = bezirk)) +
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
  ylab(NULL) +
  scale_y_discrete(position = "right")

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
  coord_cartesian(ylim = c(-.5,13)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "null"),
    panel.margin = unit(c(0, 0, 0, 0), "null")
  )

gg_halfeye_rank <- ggpubr::ggarrange(
  gg2, gg3, gg1, 
  common.legend = TRUE, legend = "bottom", 
  nrow = 1, widths = c(1, 0.5, 1))

ggsave(plot = gg_halfeye_rank, units = "px", filename = "images_presentation/halfexe_rank.png", width = 2300, height = 1700, dpi = 300)
