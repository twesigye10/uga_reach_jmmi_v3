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