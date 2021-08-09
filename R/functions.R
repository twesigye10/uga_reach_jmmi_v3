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
    summarise(across(where(is.numeric),~mean(.,na.rm=T)), .groups = "keep") %>% 
    mutate(across(everything(),~change_nan_and_inf_to_na(.)))
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
    input_df %>% 
      filter(yrmo == {{input_yrmo_constructed}}) %>% 
      ungroup() %>% 
      select(-any_of({{input_unselection}})) %>%
      rename_with(.cols = everything(), .fn = ~paste0(input_level, "_", .x))
  }
  if(input_level == "southwest"){
    input_df %>% 
      filter(yrmo == {{input_yrmo_constructed}} & regions == "south west") %>% 
      ungroup() %>% 
      select(-any_of({{input_unselection}})) %>% 
      rename_with(.cols = everything(), .fn = ~paste0(input_level, "_", .x))
  }
  if(input_level == "westnile"){
    input_df %>% 
      filter(yrmo == {{input_yrmo_constructed}} & regions == "west nile") %>% 
      ungroup() %>% 
      select(-any_of({{input_unselection}})) %>%
      rename_with(.cols = everything(), .fn = ~paste0(input_level, "_", .x))
  }
  
}
