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
