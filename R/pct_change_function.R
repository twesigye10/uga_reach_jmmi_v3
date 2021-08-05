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

# calculate % change for each data set
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


