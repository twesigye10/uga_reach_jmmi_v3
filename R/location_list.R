# location data -----------------------------------------------------------

# Load locations data
settlement_data <- read_csv("inputs/settlement_list.csv", na = c(""," ", "NA"))
district_data <- read_csv("inputs/Districts_list.csv", na = c(""," ","NA"))

# settlement data with settlement names in lower case
settlement_data <- settlement_data %>% 
  rename(settlement = NAME0) %>% 
  mutate(settlement = str_to_lower(settlement))

# district data with district names in lower case
district_data <- district_data %>%  mutate(district = str_to_lower(DName2019))

# Aggregate coordinates for Adjumani and add the aggregated value to the settlements coordinates list
adjumani_coordinates <- settlement_data %>% 
  filter(DISTRICT == "adjumani") %>% 
  select(DISTRICT,Longitude,Latitude) %>% 
  group_by(DISTRICT) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(settlement = "adjumani")

settlement_data <- bind_rows(settlement_data, adjumani_coordinates)  

rm(Adjumani_coordinates)