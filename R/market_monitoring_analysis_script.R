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

# location data -----------------------------------------------------------

# Load locations data
settlement_data <- read_csv("inputs/settlement_list.csv", na = c(""," ", "NA"))
district_data <- read_csv("inputs/Districts_list.csv", na = c(""," ","NA"))

location_data <- settlement_district(settlement_data, district_data)

settlement_data <- location_data$settlements
district_data <- location_data$districts