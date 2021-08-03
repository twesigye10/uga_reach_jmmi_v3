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
