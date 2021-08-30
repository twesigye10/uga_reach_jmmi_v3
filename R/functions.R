# process location data (settlements and districts)

settlement_district <- function(input_setlement, input_districts) {
  # settlement data with settlement names in lower case
  settlement_data <- input_setlement %>% 
    rename(settlement = NAME0) %>% 
    mutate(settlement = str_to_lower(settlement))
  
  # district data with district names in lower case
  district_data <- input_districts %>%  
    mutate(district = str_to_lower(DName2019))
  
  # Aggregate coordinates for Adjumani and add the aggregated value to the settlements coordinates list
  adjumani_coordinates <- settlement_data %>% 
    filter(DISTRICT == "adjumani") %>% 
    select(DISTRICT,Longitude,Latitude) %>% 
    group_by(DISTRICT) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
    mutate(settlement = "adjumani")
  
  settlement_data <- bind_rows(settlement_data, adjumani_coordinates) %>% 
    janitor::clean_names()
  
  settlement_district_data <- list(settlements = settlement_data, districts = district_data)
}

# regroup regions
regroup_regions<-function(input_df){
  south_west<-c("kyaka", "kyangwali", "nakivale", "oruchinga", "rwamwanja")
  west_nile<- c("adjumani", "rhino camp", "bidibidi", "imvepi", "kiryandongo", 
                "lobule", "palabek", "palorinya")
  input_df %>% 
    mutate(
      regions = case_when(
        yrmo > 202012 & settlement %in% south_west ~ "south west",
        yrmo > 202012 & settlement %in% west_nile ~ "west nile",
        TRUE ~ regions
      )
    )
}

# specific data cleaning for nov 2020 - round 11 (in case of re calculating past values)

month_specific_cleaning<-function(input_df){
  
  input_df %>% 
    mutate(vendors_change= case_when(yrmo == 202011 & vendors_change=="Less" ~ NA_character_,
                                     TRUE ~ vendors_change),
           weight_firewood= case_when(yrmo == 202011 & 
                                        settlement == "kiryandongo" & 
                                        weight_firewood == 2 ~ 6,
                                      TRUE ~ weight_firewood)
    )
}

# get files names and full paths into a list
get_files_metadata<-function(folder_path){
  fps<-list()
  fps$fullpath<-list.files(folder_path, full.names = T,pattern = ".csv$")
  fps$file_name<-list.files(folder_path,pattern = ".csv$")
  fps$num_files<- length(fps$file_name)
  fps
}

# replace Inf and NaN with NAs for a cleaner output
change_nan_and_inf_to_na <- function(x) {
  y <- replace(x, is.infinite(x), NA) 
  z <- replace(y, is.nan(y), NA)
  z
} 

# summaries for percentage changes
prices_for_pct_change_summary <- function(input_df, input_select, input_groups) {
  input_df <- item_prices_for_pct_change %>% 
    select(-{{input_select}}) %>% 
    group_by(across({{input_groups}})) %>%
    summarise(across(everything(), ~mean(., na.rm = TRUE)), .groups = "keep") %>% 
    mutate(across(everything(), ~change_nan_and_inf_to_na(.)))
}

# custom function to rename columns in the same way that has been used for previous FS
rename_cols_for_factsheet<- function(input_df, input_suffix){
  df <- input_df %>%
    rename_with(.cols = !1, .fn = ~str_replace_all(.x, "price_", "")) %>% 
    rename_with(.cols = !1, .fn = ~paste0(.x, "_perct_", input_suffix))
}

# combine stats
resultlist_summary_statistics_as_one_table<-function(results){
  results %>% 
    lapply(function(x){x$summary.statistic}) %>% do.call(rbind,.)
}

# top n from summary statistics
top_n_analysis <- function(input_summary_stats, input_n, input_dependent_vars,
                           input_independent_var) {
  input_summary_stats %>% 
    filter(dependent.var %in% {{input_dependent_vars}},
           independent.var.value == {{input_independent_var}}) %>% 
    arrange(desc(numbers)) %>%
    group_by(dependent.var) %>%
    slice(1:{{input_n}}) %>% 
    mutate(rank = row_number(),
           new_var = paste0(independent.var.value, "_", dependent.var, "_", rank)
    ) %>% 
    ungroup() %>% 
    select(new_var, numbers, dependent.var.value) %>% 
    pivot_wider(names_from = new_var, values_from = c(numbers, dependent.var.value) )
}

# non percentage vars analysis
non_perct_vars_analysis <- function(input_summary_stats, input_dependent_vars,
                                    input_independent_var) {
  input_summary_stats %>% 
    filter(dependent.var %in% {{input_dependent_vars}},
           independent.var.value == {{input_independent_var}}) %>% 
    mutate(new_var = paste0(independent.var.value, "_", dependent.var)) %>% 
    ungroup() %>% 
    select(new_var, numbers) %>% 
    pivot_wider(names_from = new_var, values_from = c(numbers) )
}

# percentage vars analysis
perct_vars_analysis <- function(input_summary_stats, input_dependent_vars,
                                input_independent_var) {
  input_summary_stats %>% 
    filter(dependent.var %in% {{input_dependent_vars}},
           independent.var.value == {{input_independent_var}}) %>% 
    mutate(new_var = paste0(independent.var.value, "_", dependent.var, "_", dependent.var.value)) %>% 
    ungroup() %>% 
    select(dependent.var.value, new_var, numbers) %>% 
    pivot_wider(names_from = new_var, values_from = c(numbers, dependent.var.value) )
}

# jmmi data merge filter_ungroup_select_rename
jmmi_datamerge_filter_rename <- function(input_df, input_yrmo_constructed, input_unselection, input_level) {
  
  if(input_level == "national"){
    out_df <- input_df %>% 
      filter(yrmo == {{input_yrmo_constructed}}) %>% 
      ungroup() %>% 
      select(-any_of({{input_unselection}})) %>%
      rename_with(.cols = everything(), .fn = ~paste0(input_level, "_", .x))
  }
  if(input_level == "southwest"){
    out_df <- input_df %>% 
      filter(yrmo == {{input_yrmo_constructed}} & regions == "south west") %>% 
      ungroup() %>% 
      select(-any_of({{input_unselection}})) %>% 
      rename_with(.cols = everything(), .fn = ~paste0(input_level, "_", .x))
  }
  if(input_level == "westnile"){
    out_df <- input_df %>% 
      filter(yrmo == {{input_yrmo_constructed}} & regions == "west nile") %>% 
      ungroup() %>% 
      select(-any_of({{input_unselection}})) %>%
      rename_with(.cols = everything(), .fn = ~paste0(input_level, "_", .x))
  }
  
  out_df
}
