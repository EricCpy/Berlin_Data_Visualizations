source("code/setup.R")

#### Base model ####

dat <- df_airbnb %>% select(log_price)

flist <- alist(
  log_price ~ dnorm(a, sigma),
  a <- mu,
  mu ~ dnorm(4.61, 0.5),
  sigma ~ dunif( 0 , 1 )
)

base_model <- ulam(
  flist, data=dat, 
  cores = 4, chains = 4
)

flist_studnet <- alist(
  log_price ~ dstudent(2, a, sigma),
  a <- mu,
  mu ~ dnorm(4.61, 0.5),
  sigma ~ dunif( 0 , 1 )
)

base_model_student <- ulam(
  flist_studnet, data=dat, 
  cores = 4, chains = 4
)

#### mit wohnlage ####

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
  cores = 4, chains = 4, iter = 2000
)

flist_student <- alist(
  log_price ~ dstudent(2, a, sigma),
  a <- reputation[bezirk] + bE*sum(delta[1:median_wohnlage]),
  reputation[bezirk] ~ dnorm(4.61, 0.5),
  bE ~ dnorm(0, 0.5),
  simplex[3]: delta ~ dirichlet( alpha ),
  sigma ~ dexp(1)
)

base_model_reputation_wohnlage_student <- ulam(
  flist_student, data=dat2, 
  cores = 4, chains = 4, iter = 2000
)

#### full model ####

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

full_model <- ulam(
  flist_full, data=dat, 
  cores = 4, chains = 4, iter = 2000
)

#### saving posterior draws ####

set.seed(1337)

base_model %>% spread_draws(mu, sigma, ndraws = 1000) %>% 
  saveRDS("saved_objects/base_model_post.rds")
base_model_student %>% spread_draws(mu, sigma, ndraws = 1000) %>% 
  saveRDS("saved_objects/base_model_studnet_post.rds")

base_model_reputation_wohnlage %>% 
  spread_draws(reputation[bezirk], bE, delta[wohnlage], sigma, ndraws = 1000) %>% 
  saveRDS("saved_objects/base_model_reputation_wohnlage_post.rds")
base_model_reputation_wohnlage_student %>% 
  spread_draws(reputation[bezirk], bE, delta[wohnlage], sigma, ndraws = 1000) %>% 
  saveRDS("saved_objects/base_model_reputation_wohnlage_studnet_post.rds")

full_model %>% 
  spread_draws(reputation[bezirk], bE, delta[wohnlage], bPT[property_type], sigma, ndraws = 1000) %>% 
  saveRDS("saved_objects/full_model_post.rds")