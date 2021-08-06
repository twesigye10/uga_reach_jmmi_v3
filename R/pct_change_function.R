# Uganda Market Monitoring - MEB Calculations and Percentage Change Analysis

################################
# Percentage Change Settlement #
################################

pct_change<- function(x){(x/lag(x)-1)*100}

pct_change_by_groups_all_numerics<-function(df, group_var, time_id){
  
  res<-df %>%
    group_by(!!sym(group_var))  %>%
    arrange(!!sym(group_var), !!sym(time_id)) %>%
    summarise(across(where(is.numeric), pct_change)) %>%
    ungroup() %>%
    filter(rowAny(
      filter(across(c(-group_var), ~!is.na(.))))
    )
  return(res)
}

rowAny <- function(x) rowSums(x) > 0


# percentage_change_calculations ------------------------------------------


percentage_change_calculations <- function(input_df, input_yrmo_to_include, 
                                           input_settlement_items, input_region_items, 
                                           input_national_items, input_meb_items, input_meb_items_regional, 
                                           input_meb_items_national ) {
  # getting unique settlements in each of the relevant rounds
  settlements_this_round <- input_df %>% 
    filter(yrmo == input_yrmo_to_include[length(input_yrmo_to_include)]) %>% 
    pull(settlement) %>% 
    unique()
  
  # we need to consider settlements that match the current round in order calculate % changes in current round
  settlement_items <- input_settlement_items %>% 
    filter(settlement %in% settlements_this_round)
  
  # extract yrmo combinations of interest
  yrmo_current_and_last <- c(input_yrmo_to_include[length(input_yrmo_to_include) - 1], input_yrmo_to_include[length(input_yrmo_to_include)])
  yrmo_current_and_baseline <- c(input_yrmo_to_include[1], input_yrmo_to_include[length(input_yrmo_to_include)])
  
  # Calculate % change at settlement, regional and national -----------------------------
  
  # the process is the same, put them in a list so we can purrr through
  
  pct_change_items_list_all <- list(settlement_items, input_region_items, input_national_items) %>% 
    set_names(c("settlement", "regions", "national"))
  
  analysis_level <- c("settlement", "regions", "national")
  
  # calculate % change between this round and base and this round and last round
  
  pct_change_all <- pct_change_items_list_all %>% 
    map2(analysis_level, function(x, y) {
      if(y == "national"){
        x <- x %>% 
          mutate(national = "national")
      }
      
      current_and_last <- x %>% 
        filter(yrmo %in% yrmo_current_and_last)
      
      current_and_base <- x %>% 
        filter(yrmo %in% yrmo_current_and_baseline)
      
      pct_change_current_to_last <- current_and_last %>% 
        pct_change_by_groups_all_numerics(group_var = y, time_id = "yrmo") %>% 
        rename_cols_for_factsheet("last_round")
      
      pct_change_current_to_base <- current_and_base %>% 
        pct_change_by_groups_all_numerics(group_var = y, time_id = "yrmo") %>% 
        rename_cols_for_factsheet("march")
      
      pct_change_current_to_last %>% 
        left_join(pct_change_current_to_base)
    })
  
  # Calc % change in mebs at settlement, regional and national  level -------
  
  meb_items_for_pct_change_list <- list(input_meb_items, input_meb_items_regional, input_meb_items_national) %>% 
    set_names(c("settlement", "regions", "national"))
  
  meb_analysis_level <- c("settlement", "regions", "national")
  
  meb_pct_change_all_levels <- meb_items_for_pct_change_list %>% 
    map2(meb_analysis_level, function(x, y) {
      
      print(meb_analysis_level)
      
      if(y == "national"){
        x <- x %>% 
          mutate(national = "national")
      }
      
      current_and_last <- x %>% 
        filter(yrmo %in% yrmo_current_and_last)
      
      current_and_base <- x %>% 
        filter(yrmo %in% yrmo_current_and_baseline)
      
      pct_change_current_to_last <- current_and_last %>% 
        pct_change_by_groups_all_numerics(group_var = y, time_id = "yrmo") %>% 
        rename_cols_for_factsheet("last_round")
      
      pct_change_current_to_base <- current_and_base %>% 
        pct_change_by_groups_all_numerics(group_var = y, time_id = "yrmo") %>% 
        rename_cols_for_factsheet("march")
      
      pct_change_current_to_last %>% 
        left_join(pct_change_current_to_base) %>% 
        select(y, contains("food"), contains("full") )
    })
  
  # regional extract --------------------------------------------------------
  
  # extract regional results in format/schema needed for downstream processes
  # combine different results, get price changes with meb price changes  (this round to last round)
  change_region_last_round<- left_join(pct_change_all$regions, meb_pct_change_all_levels$regions) %>%
    select(-ends_with("march"))
  # combine different results, get price changes with meb price changes  (this round to reference round)
  change_region_march<- left_join(pct_change_all$regions, meb_pct_change_all_levels$regions) %>%
    select(-ends_with("last_round"))
  # combine results
  regional_list <- list(pct_change_all$regions,
                        change_region_last_round,
                        change_region_march
  )
  percent_change_region <- purrr::reduce(regional_list, left_join)
  
  # national extract --------------------------------------------------------
  
  change_national_last_round <- left_join(pct_change_all$national,  meb_pct_change_all_levels$national) %>% 
    select(-ends_with("march"))
  
  change_national_last_march <- left_join(pct_change_all$national,  meb_pct_change_all_levels$national) %>% 
    select(-ends_with("last_round"))
  
  percent_change_national <- left_join(change_national_last_round, change_national_last_march)
  
  # settlement extract ------------------------------------------------------
  
  change_settlement_last_round <- left_join(pct_change_all$settlement, meb_pct_change_all_levels$settlement) %>% 
    select(-ends_with("march"))
  
  change_settlement_march <- left_join(pct_change_all$settlement, meb_pct_change_all_levels$settlement) %>% 
    select(-ends_with("last_round"))
  
  settlement_list <- list(change_settlement_last_round, change_settlement_march)
  
  percent_change_settlement <- purrr::reduce(settlement_list, left_join)
  
  price_and_meb_pct_changes <- list(
    settlement_items = settlement_items,
    percent_change_settlement = percent_change_settlement,
    percent_change_region = percent_change_region,
    percent_change_national = percent_change_national,
    meb_percent_change_settlement = meb_pct_change_all_levels$settlement,
    meb_percent_change_region = meb_pct_change_all_levels$regions,
    meb_percent_change_national = meb_pct_change_all_levels$national
  )
  
}
