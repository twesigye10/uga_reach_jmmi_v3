# Uganda Market Monitoring - Food MEB and MEB Calculations

# load reference meb data
ref_mebs <- read_excel("./inputs/wfp_march_mebs.xlsx") %>% 
  mutate(yrmo = yrmo_to_include[1])

# median calculations on item prices

meb_items <- item_prices %>% 
  select(-c(uuid, market_final, price_maize_g, price_underwear, price_charcoal,
            price_pads, price_DAP, price_NKP, price_malathion, price_millet_f,
            -contains("_price"), month)
         ) %>%
  group_by(regions, district, settlement, yrmo) %>% 
  summarise(across(everything(), ~median(.x, na.rm = TRUE)), .groups = "drop_last")


# Calculate proximity: if a price is missing take the mean of settlement, or district, otherwise, regions

# imputation of NAs based on means of the current month(yrmo)
meb_items <- meb_items %>% 
  group_by(settlement, yrmo) %>% 
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>% 
  ungroup() %>% 
  group_by(district, yrmo) %>% 
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>% 
  ungroup() %>% 
  group_by(regions, yrmo) %>% 
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) 

prior_2_rounds <- c(yrmo_to_include[length(yrmo_to_include) - 1], yrmo_to_include[length(yrmo_to_include) - 2])
current_and_prior_round <- c(yrmo_to_include[length(yrmo_to_include)], yrmo_to_include[length(yrmo_to_include) - 1])

# if NA, put price from last round
meb_items_for_this_month_imputation <- meb_items %>% 
  filter(yrmo %in% current_and_prior_round)

meb_items_for_prev_month_imputation <- meb_items %>% 
  filter(yrmo %in% prior_2_rounds)

# this improvement should yield better values since we start by aggregating at settlement level
meb_items_this_round <- meb_items_for_this_month_imputation %>% 
  group_by(settlement) %>% 
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>% 
  ungroup() %>% 
  group_by(district) %>% 
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>% 
  ungroup() %>% 
  group_by(regions) %>% 
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>% 
  filter(yrmo %in% yrmo_constructed)

meb_items_last_round <- meb_items_for_prev_month_imputation %>% 
  group_by(settlement) %>% 
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>% 
  ungroup() %>% 
  group_by(district) %>% 
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>% 
  ungroup() %>% 
  group_by(regions) %>% 
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>% 
  filter(yrmo %in% prior_2_rounds[1])

# meb items for this and last rounds
meb_items <- bind_rows(meb_items_this_round, meb_items_last_round)