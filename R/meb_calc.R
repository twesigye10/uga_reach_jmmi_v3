# Uganda Market Monitoring - Food MEB and MEB Calculations

meb_cal_func <- function(input_item_prices, input_ref_mebs, input_yrmo_constructed, input_yrmo_to_include ) {
  # median calculations on item prices
  meb_items <- input_item_prices %>% 
    select(-c(uuid, market_final, price_maize_g, price_underwear, price_charcoal,
              price_pads, price_DAP, price_NKP, price_malathion, price_millet_f, month),
           -contains("_price")
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
  
  prior_2_rounds <- c(input_yrmo_to_include[length(input_yrmo_to_include) - 1], input_yrmo_to_include[length(input_yrmo_to_include) - 2])
  current_and_prior_round <- c(input_yrmo_to_include[length(input_yrmo_to_include)], input_yrmo_to_include[length(input_yrmo_to_include) - 1])
  
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
    filter(yrmo %in% input_yrmo_constructed)
  
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
  
  # MEB calculation
  # one-off Items - once a year
  blanket <- 45000
  pans <- 13125
  plates <- 4885
  spoon <- 3538
  cups <- 3985
  mingle <- 1000
  
  meb_items <- meb_items %>% 
    mutate(
      # food items
      meb_maize_f = price_maize_f * 8.7 * 5,
      meb_beans = price_beans * 5.4 * 5,
      meb_sorghum = price_sorghum * 1.5 * 5,
      meb_oil = price_oil * 0.75 * 5,
      meb_salt = price_salt * 0.15 * 5,
      meb_milk = price_milk * 0.3 * 5,
      meb_dodo = price_dodo * 3 * 5,
      meb_fish = price_fish * 0.6 * 5,
      meb_cassava = price_cassava * 0.6 *5,
      # non-food items
      meb_soap = price_soap * 0.45 * 5,
      meb_firewood = price_firewood * 1.1 * 30 * 5,
      # Hygiene items
      meb1_reusable_pads = 4667,
      meb1_jerry_can = 1090,
      meb1_bucket = 632,
      meb1_hand_washing = 208,
      meb_hygiene = meb1_reusable_pads + meb1_jerry_can + meb1_bucket + meb1_hand_washing + meb_soap,
      # extra items
      meb_clothing = 3806, 
      meb_water = 3750,
      meb_livelihoods = 37705,
      meb_education = 28667,
      meb_transport = 11001,
      meb_health = 2669,
      meb_communication = 4256,
      meb1_lighting = 5000,
      # one-off items - once a year
      meb_other_hdd = sum(blanket, pans, plates, spoon, cups, mingle)/12,
      # MEB Energy
      meb_energy = meb1_lighting + meb_firewood,
      # Food MEB Calcuations
      meb_food = meb_maize_f + meb_beans + meb_sorghum + meb_oil + meb_milk + meb_cassava + meb_salt + meb_dodo + meb_fish,
      # Full MEB Calcuations
      meb_full = meb_food + meb_clothing + meb_water + meb_livelihoods + meb_education + meb_transport +
        meb_health + meb_communication + meb_hygiene + meb_other_hdd + meb_energy
    ) %>% 
    # clean the table
    select(-starts_with("price_"), -starts_with("meb1")) %>% 
    # round values
    mutate(across(where(is.numeric), round, 0)) 
  
  # combine with reference mebs
  meb_items <- bind_rows(input_ref_mebs, meb_items) %>% 
    mutate( collection_order = case_when( yrmo == input_yrmo_to_include[length(input_yrmo_to_include)] ~ 4,
                                          yrmo == input_yrmo_to_include[length(input_yrmo_to_include) - 1] ~ 3,
                                          TRUE ~ 1)
    ) %>% select(yrmo, month, everything())
  
  # regional
  meb_items_regional <- meb_items %>% 
    select(-c(district, settlement)) %>% 
    group_by(regions, yrmo) %>% 
    summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>% 
    mutate(across(where(is.numeric), round, 0)) %>%
    rename(regional_meb_food = meb_food, regional_meb_full = meb_full)
  
  # national
  meb_items_national <- meb_items %>% 
    select(-c(district, settlement, regions)) %>% 
    group_by(yrmo) %>% 
    summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>% 
    mutate(across(where(is.numeric), round, 0), regions = "nationwide") %>%
    rename(national_meb_food = meb_food, national_meb_full = meb_full)
  
  # data for ranking
  # top three most expensive settlements
  top_settlements <- meb_items %>%
    filter(yrmo == input_yrmo_constructed) %>% 
    ungroup() %>% 
    select(settlement, meb_full) %>% 
    arrange(desc(meb_full)) %>% 
    mutate(rank = row_number()) %>% 
    filter(rank <= 3)
  
  # bottom three least expensive settlements
  bottom_settlements <- meb_items %>%
    filter(yrmo == input_yrmo_constructed) %>% 
    ungroup() %>% 
    select(settlement, meb_full) %>% 
    arrange(desc(meb_full)) %>% 
    mutate(rank = row_number()) %>% 
    filter(rank >= 11)
  
  rank_settlements <- bind_rows(top_settlements, bottom_settlements)
  
  meb_data <- list(meb_items = meb_items,
                   meb_items_regional = meb_items_regional,
                   meb_items_national = meb_items_national,
                   rank_settlements = rank_settlements)
}