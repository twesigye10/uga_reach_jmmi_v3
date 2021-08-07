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
source("./R/meb_calc.R")
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
           sub_regions == "Acholi" | sub_regions == "West nile" ~ "west nile",
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


# means and percentage change calculations -------------------------------------------------------

item_prices <- df %>% 
  select(uuid, yrmo, month, regions, district, settlement, market_final,
         contains("price"), starts_with("weight_"), -starts_with("price_increase"),
         -starts_with("price_decrease"), -ends_with(".prices"), -starts_with("challenge.")
  ) %>% 
  ungroup() %>% 
  mutate(
    price_dodo = price_dodo/weight_dodo,
    price_cassava = price_cassava/weight_cassava,
    price_fish = price_fish/weight_fish,
    price_firewood = price_firewood/weight_firewood,
    price_charcoal = price_charcoal/weight_charcoal
  ) %>% 
  select(-contains("weight_"), - contains("Observed")) %>% 
  mutate(across(starts_with("price_"), ~ as.double(.)))

# could not identify its usage
# item_prices[item_prices == 99] <- NA
# item_prices[item_prices == "yes"] <- 1

# for pct change we only want this month, last month, and reference month
item_prices_for_pct_change <- item_prices %>% 
  filter(yrmo %in% c(yrmo_to_include[1], yrmo_to_include[length(yrmo_to_include)], yrmo_to_include[length(yrmo_to_include) - 1])) %>% 
  mutate(collection_order = case_when( yrmo == yrmo_to_include[length(yrmo_to_include)] ~ 4,
                                       yrmo == yrmo_to_include[length(yrmo_to_include) - 1] ~ 3,
                                       TRUE ~ 1  ),
         month = month(month, label = TRUE, abbr = FALSE)
  )


national_items <- prices_for_pct_change_summary(item_prices_for_pct_change,
                                                c("uuid", "regions", "district", "settlement", "market_final"),
                                                c(yrmo, collection_order))

markets_items <- prices_for_pct_change_summary(item_prices_for_pct_change,
                                               c("uuid", "regions", "district"),
                                               c(settlement, market_final, yrmo))

settlement_items <- prices_for_pct_change_summary(item_prices_for_pct_change,
                                                  c("uuid", "market_final"),
                                                  c(regions, district, settlement, yrmo))

district_items <- prices_for_pct_change_summary(item_prices_for_pct_change,
                                                c("uuid", "settlement", "market_final"),
                                                c(regions, district, yrmo))

region_items <- prices_for_pct_change_summary(item_prices_for_pct_change,
                                              c("uuid", "district", "settlement", "market_final"),
                                              c(regions, yrmo))
## for data merge
# counts per area: region and settlements
markets_per_region <- item_prices_for_pct_change %>% 
  select(regions, yrmo, market_final) %>% 
  group_by(regions, yrmo) %>% 
  summarise(num_market_assessed = n_distinct(market_final),
            num_assessed = length(yrmo), .groups = "drop_last") %>% 
  rename(level = regions) %>% 
  filter(yrmo == yrmo_constructed) %>% 
  select(level, num_market_assessed, num_assessed)

# counts per area: nation wide
markets_nationwide <- item_prices_for_pct_change %>% 
  select(regions, yrmo, market_final) %>% 
  group_by(yrmo) %>% 
  summarise(num_market_assessed = n_distinct(market_final),
            num_assessed = length(yrmo), .groups = "drop_last") %>% 
  mutate(level = "national") %>%
  filter(yrmo == yrmo_constructed) %>% 
  select(level, num_market_assessed, num_assessed)

data_merge_summary <- bind_rows(markets_nationwide, markets_per_region)


# load reference meb data
ref_mebs <- read_excel("./inputs/wfp_march_mebs.xlsx") %>% 
  mutate(yrmo = as.numeric(yrmo_to_include[1]))

# calculating MEBs
meb_data <- meb_cal_func(input_item_prices = item_prices,
                         input_ref_mebs = ref_mebs, 
                         input_yrmo_constructed = yrmo_constructed,
                         input_yrmo_to_include = yrmo_to_include)

# calculating pecentage changes
pct_change_data <- percentage_change_calculations(input_df = df,
                                                  input_yrmo_to_include = yrmo_to_include,
                                                  input_settlement_items = settlement_items,
                                                  input_region_items = region_items,
                                                  input_national_items = national_items,
                                                  input_meb_items = meb_data$meb_items,
                                                  input_meb_items_regional = meb_data$meb_items_regional,
                                                  input_meb_items_national = meb_data$meb_items_national)
## Data exports for percentage change and MEB
list_of_datasets_med <- list("Market mean price" = markets_items,
                             "Settlement mean price" = pct_change_data$settlement_items,
                             "District Mean" = district_items,
                             "Region mean" = region_items,
                             "National level mean" = national_items,
                             "Percent change Settlement" = pct_change_data$percent_change_settlement,
                             "Percent change Region" = pct_change_data$percent_change_region,
                             "Percent change National" = pct_change_data$percent_change_national,
                             "Rank settlements" = meb_data$rank_settlements)

openxlsx::write.xlsx(list_of_datasets_med, 
                     paste0("./outputs/",
                            output_folder,"/",
                            butteR::date_file_prefix(),"_",
                            yrmo_constructed,
                            "_UGA_JMMI_Means and percentage change.xlsx")
)

list_of_datasets_meb <- list("Settlement MEB" = meb_data$meb_items,
                             "Regional MEB" = meb_data$meb_items_regional,
                             "National MEB" = meb_data$meb_items_national,
                             "Percent change MEB Settlment" = pct_change_data$meb_percent_change_settlement,
                             "Percent change MEB Regional" = pct_change_data$meb_percent_change_region,
                             "Percent change MEB National" = pct_change_data$meb_percent_change_national)

openxlsx::write.xlsx(list_of_datasets_meb, 
                     paste0("./outputs/",
                            output_folder,"/",
                            butteR::date_file_prefix(),"_",
                            yrmo_constructed,
                            "_UGA_JMMI_MEB and percentage change.xlsx")
)


# Market Functionality Page -----------------------------------------------

# load analysis plan
dap <- load_analysisplan("./inputs/dap/jmmi_dap_v1.csv")

# prepare dataset for analysis
df_analysis <- df %>% 
  filter(yrmo == yrmo_constructed) %>% 
  mutate(mobile_accepted = ifelse(grepl("mobile_money", payment_type), "yes", "no"),
         customer_number = as.numeric(customer_number),
         agents_number = as.numeric(agents_number))


# load kobo tool
kobo_tool <- load_questionnaire(df_analysis,
                                questions = read.csv("./inputs/kobo/questions.csv"),
                                choices = read.csv("./inputs/kobo/choices.csv"),
                                choices.label.column.to.use = "label")

# launch analysis and isolate analysis results
analysis <- from_analysisplan_map_to_output(data = df_analysis, 
                                            analysisplan = dap, 
                                            weighting = NULL, 
                                            questionnaire = kobo_tool )


# summary statistics list -------------------------------------------------

summary.stats.list <- analysis$results %>% 
  map(function(x) { map_to_labeled(result = x, questionnaire = kobo_tool) }) %>% 
  resultlist_summary_statistics_as_one_table() %>% 
  select(-c(se, min, max)) %>% 
  map_to_file(paste0("./outputs/",
                     output_folder,"/",
                     butteR::date_file_prefix(),"_",
                     yrmo_constructed, "_jmmi_analysis.csv"))


# save analysis to html ---------------------------------------------------

hypegrammaR::map_to_generic_hierarchical_html(resultlist = analysis,
                                              render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                              by_analysisplan_columns = c("research.question","indicator"),
                                              by_prefix = c("RQ:", "Indicator: "),
                                              level = 2,
                                              questionnaire = kobo_tool,
                                              label_varnames = TRUE,
                                              dir = paste0("./outputs/", output_folder),
                                              filename = paste0(butteR::date_file_prefix(),
                                                                "_html_analysis_jmmi",
                                                                ".html")
)

# top 3 analysis ----------------------------------------------------------

# Slice result list by areas
summary.stats.list <- analysis$results %>% 
  resultlist_summary_statistics_as_one_table() %>% 
  select(-c(se, min, max, repeat.var, repeat.var.value))

vec1 <- rep(c(1,2,3), 7)
vec2 <- rep(c(1,2), 10)

# Rename based on choices from kobo
choices <- read.csv("./inputs/kobo/choices.csv")
summary.stats.list$dependent.var.value <- choices$label[match(summary.stats.list$dependent.var.value, choices$name)]

# All markets 
top3_uganda <- summary.stats.list %>% 
  filter(dependent.var %in% c("payment_type", "safety_reason_less_secure",
                              "safety_reason_more_secure", "item_scarcity_reason",
                              "price_increase_item", "price_decrease_item", "challenge"),
         independent.var.value == "uganda") %>% 
  arrange(desc(numbers)) %>%
  group_by(dependent.var) %>%
  slice_head(3) %>% 
  mutate(rank = row_number(),
         new_var = paste0(independent.var.value, "_", dependent.var, "_", rank)
         ) %>% 
  ungroup() %>% 
  select(new_var, numbers, dependent.var.value) %>% 
  pivot_wider(names_from = new_var, values_from = c(numbers, dependent.var.value) )

top3_dependent_vars <- c("payment_type", "safety_reason_less_secure",
                         "safety_reason_more_secure", "item_scarcity_reason",
                         "price_increase_item", "price_decrease_item", "challenge")

top3_uganda <- top_n_analysis(input_summary_stats = summary.stats.list,
                              input_n = 3, 
                              input_dependent_vars = top3_dependent_vars, 
                              input_independent_var = "uganda"
                              )