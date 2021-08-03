# Uganda Market Monitoring R Script - V3

today <- Sys.Date()

# Download and install hypegrammaR from IMPACT GitHub
# devtools::install_github("impact-initiatives/hypegrammaR", build_opts = c())

# Load required packaged
library(tidyverse)
library(readxl)
library(data.table)
library(hypegrammaR)
library(lubridate)
library(butteR)

# source scripts
source("./R/location_list.R")
source("./R/functions.R")
source("./R/extra_r11_cleaning.R")
source("./R/pct_change_function.R")

# location data -----------------------------------------------------------

# Load locations data
df_settlements <- read_csv("inputs/settlement_list.csv", na = c(""," ", "NA"))
df_districts <- read_csv("inputs/Districts_list.csv", na = c(""," ","NA"))

location_data <- settlement_district(df_settlements, df_districts)

settlement_data <- location_data$settlements
district_data <- location_data$districts

# create year, months, rounds and folder variables
fps <- get_files_metadata(folder_path = "inputs/clean_data/")
year_of_assessment <- as.numeric(str_sub(fps$file_name[length(fps$file_name)], start = 1, end = 4))
month_number <- as.numeric(str_sub(fps$file_name[length(fps$file_name)], start = 5, end = 6))
month_chr <- if_else(condition = month_number < 10, true = paste0("0",month_number), false = as.character(month_number) )
this_round_vec <- month(month_number, label = TRUE, abbr = FALSE)
output_folder <- paste0(year_of_assessment, month_chr, "_reach_uga_jimmi_outputs")
# create output folder for the analysis if it doesn't exist
if(!dir.exists(paste0("outputs/",output_folder))){
  dir.create(paste0("outputs/",output_folder))
}

yrmo_constructed <- glue::glue("{year_of_assessment}{month_chr}")
date_contstructed <- as_date(glue::glue("{year_of_assessment}-{month_chr}-01"))

yrmo <- str_sub(string = fps$file_name, start = 1, end = 6)
yrmo_to_include <- c(yrmo[1], yrmo[length(yrmo)], yrmo[length(yrmo) - 1], yrmo[length(yrmo) - 2]) %>% 
  sort()

# read in all the data
df_all_data <- fps$fullpath %>% 
  set_names(fps$file_name) %>% 
  map2_dfr(.y = yrmo,
           function(.x, .y){
             print(.x)
             x <- read_csv(.x) %>% 
               mutate(
                 settlement = str_replace(settlement, "rhino", "rhino camp"),
                 yrmo = as.numeric(.y),
                 yr = as.numeric(str_sub(string = yrmo, start = 1, end = 4)),
                 mo = as.numeric(str_sub(string = yrmo, start = 5, end = 6))
               ) %>% 
               select(-any_of("today"), yrmo)
             
             colnames(x)<- ifelse(str_detect(colnames(x), "uuid"), "uuid", colnames(x))
             x
           }
  )

# filter only required data for analysis, add settlement and district data
df<-df_all_data %>% 
  filter(yrmo %in% yrmo_to_include) %>% 
  left_join( settlement_data, by = "settlement" ) %>% 
  left_join( district_data, by = "district") %>% 
  select(month:district, F15Regions, DName2019, uuid) %>% 
  select(-contains("X_"), -objectid) %>% 
  rename(country = name, sub_regions = F15Regions) %>% 
  mutate(country = str_to_lower(country), 
         sub_regions = str_to_sentence(sub_regions),
         # regions grouping(initial implementation), This is changes in regrouping since Dec 2020
         regions = case_when(
           sub_regions == "Acholi" | df$sub_regions == "West nile" ~ "west nile",
           district == "Bunyoro" ~ "west nile",
           TRUE ~ "south west"
         )
         ) %>% 
  # correction from Dec 2020
  regroup_regions() %>%
  mutate(month_lab = lubridate::month(mo, label = TRUE, abbr = FALSE),
         # Add new market column that includes other markets
         market_final = ifelse(market == "Other", market_other, market),
         market = NULL,
         market_other = NULL
         ) %>% 
  filter(!is.na(month_lab)) %>% 
  # reorganise columns and remove other columns
  select(-c("day", "DName2019", "sub_regions"),
         c("month","country", "district", "regions", "settlement", "market_final"), 
         everything()
         )
# WFP/REACH decided to remove 'Less' from vendors_change data as it should not have been an option 
if("202011" %in% yrmo_to_include){
  df<- month_specific_cleaning(df)
}
