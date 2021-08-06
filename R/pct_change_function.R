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

# getting unique settlements in each of the relevant rounds
settlements_last_round <- df %>% 
  filter(yrmo == yrmo_to_include[length(yrmo_to_include) - 1]) %>% 
  pull(settlement) %>% 
  unique()

settlements_baseline <- df %>% 
  filter(yrmo == yrmo_to_include[1]) %>% 
  pull(settlement) %>% 
  unique()

settlements_this_round <- df %>% 
  filter(yrmo == yrmo_to_include[length(yrmo_to_include)]) %>% 
  pull(settlement) %>% 
  unique()

# we need to consider settlements that match the current round in order calculate % changes in current round
settlement_items <- settlement_items %>% 
  filter(settlement %in% settlements_this_round)

# extract yrmo combinations of interest
yrmo_current_and_last <- c(yrmo_to_include[length(yrmo_to_include) - 1], yrmo_to_include[length(yrmo_to_include)])
yrmo_current_and_baseline <- c(yrmo_to_include[1], yrmo_to_include[length(yrmo_to_include)])

# create the two datasets necessary for % change calculations
current_and_last_settlement_items <- settlement_items %>% 
  filter(yrmo %in% yrmo_current_and_last)
current_and_baseline_settlement_items <- settlement_items %>% 
  filter(yrmo %in% yrmo_current_and_baseline)


# calculate % change for each data set ------------------------------------

# current to last month
pct_change_current_to_last <- current_and_last_settlement_items %>% 
  pct_change_by_groups_all_numerics(group_var = "settlement", time_id = "yrmo") %>% 
  # rename columns according to schema from previous rounds
  rename_cols_for_factsheet(suffix = "last_round")
# current to baseline (reference data)
pct_change_current_to_baseline <- current_and_baseline_settlement_items %>% 
  pct_change_by_groups_all_numerics(group_var = "settlement", time_id = "yrmo") %>% 
  # rename columns according to schema from previous rounds
  rename_cols_for_factsheet(suffix = "march")

# merge the two datasets
pct_change_settlement <- pct_change_current_to_last %>% 
  left_join(pct_change_current_to_baseline, by = "settlement")


# Calculate % change at regional and national -----------------------------

# since the process is the same, put them in a list so we can purrr through

pct_change_items_list_regional_national <- list(region_items, national_items) %>% 
  set_names(c("regional", "national"))

analysis_level <- c("regional", "national")
  
# calculate % change between this round and base and this round and last round

pct_change_regional_national <- pct_change_items_list_regional_national %>% 
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

meb_items_for_pct_change_list <- list(region_items, national_items) %>% 
  set_names(c("settlement", "regional", "national"))

meb_analysis_level <- c("settlement", "regional", "national")
  
meb_pct_change_all_levels <- meb_items_for_pct_change_list %>% 
  map2(meb_analysis_level, function(x, y) {
    
    print(analysis_level)
    
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
change_region_last_round<- left_join(pct_change_regional_national$regional, meb_pct_change_all_levels$regional) %>%
  select(-ends_with("march"))
# combine different results, get price changes with meb price changes  (this round to reference round)
change_region_march<- left_join(pct_change_regional_national$regional, meb_pct_change_all_levels$regional) %>%
  select(-ends_with("last_round"))
# combine results
regional_list <- list(pct_change_regional_national$regional,
                      change_region_last_round,
                      change_region_march
                      )
percent_change_region <- purrr::reduce(regional_list, left_join)


# national extract --------------------------------------------------------

change_national_last_round <- left_join(pct_change_regional_national$national,
                                        meb_pct_change_all_levels$national) %>% 
  select(-ends_with("march"))

change_national_last_march <- left_join(pct_change_regional_national$national,
                                        meb_pct_change_all_levels$national) %>% 
  select(-ends_with("last_round"))

percent_change_national <- left_join(change_national_last_round, change_national_last_march)


# settlement extract ------------------------------------------------------

change_settlement_last_round <- left_join(pct_change_settlement, meb_pct_change_all_levels$settlement) %>% 
  select(-ends_with("march"))

change_settlement_march <- left_join(pct_change_settlement, meb_pct_change_all_levels$settlement) %>% 
  select(-ends_with("last_round"))

settlement_list <- list(change_settlement_last_round, change_settlement_march)

percent_change_settlement <- purrr::reduce(settlement_list, left_join)
