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
                            "_UGA_JMMI_Means and percentage change.xlsx"), overwrite = TRUE
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
                            "_UGA_JMMI_MEB and percentage change.xlsx"), overwrite = TRUE
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

# slice result list by areas
summary.stats.list <- analysis$results %>% 
  resultlist_summary_statistics_as_one_table() %>% 
  select(-c(se, min, max, repeat.var, repeat.var.value))

# rename based on choices from kobo
choices <- read.csv("./inputs/kobo/choices.csv")
summary.stats.list$dependent.var.value <- choices$label[match(summary.stats.list$dependent.var.value, choices$name)]

# all markets 
top3_dependent_vars <- c("payment_type", "safety_reason_less_secure",
                         "safety_reason_more_secure", "item_scarcity_reason",
                         "price_increase_item", "price_decrease_item", "challenge")

top3_uganda <- top_n_analysis(input_summary_stats = summary.stats.list,
                              input_n = 3, 
                              input_dependent_vars = top3_dependent_vars, 
                              input_independent_var = "uganda" )
# South West region
top3_southwest <- top_n_analysis(input_summary_stats = summary.stats.list,
                                 input_n = 3, 
                                 input_dependent_vars = top3_dependent_vars, 
                                 input_independent_var = "south west" )
# West Nile region
top3_westnile <- top_n_analysis(input_summary_stats = summary.stats.list,
                                input_n = 3, 
                                input_dependent_vars = top3_dependent_vars, 
                                input_independent_var = "west nile" )

# top 2 analysis - increase in price --------------------------------------

# all markets
top2_dependent_vars <- c("cereal_increase_reason", "cassava_increase_reason",
                         "beans_increase_reason", "vegetables_increase_reason",
                         "milk_increase_reason", "fish_increase_reason",
                         "oil_increase_reason", "salt_increase_reason",
                         "wash_increase_reason", "energy_increase_reason")

top2_uganda <- top_n_analysis(input_summary_stats = summary.stats.list,
                              input_n = 2, 
                              input_dependent_vars = top2_dependent_vars, 
                              input_independent_var = "uganda" )
# South West region
top2_southwest <- top_n_analysis(input_summary_stats = summary.stats.list,
                                 input_n = 2, 
                                 input_dependent_vars = top2_dependent_vars, 
                                 input_independent_var = "south west" )
# West Nile region
top2_westnile <- top_n_analysis(input_summary_stats = summary.stats.list,
                                input_n = 2, 
                                input_dependent_vars = top2_dependent_vars, 
                                input_independent_var = "west nile" )

# top 2 analysis - decrease in price --------------------------------------

# all markets
top2_price_decrease_dependent_vars <- c("cereal_decrease_reason", "cassava_decrease_reason",
                                        "beans_decrease_reason", "vegetables_decrease_reason",
                                        "milk_decrease_reason", "fish_decrease_reason",
                                        "oil_decrease_reason", "salt_decrease_reason",
                                        "wash_decrease_reason", "energy_decrease_reason")

top2_uganda_dec <- top_n_analysis(input_summary_stats = summary.stats.list,
                                  input_n = 2, 
                                  input_dependent_vars = top2_price_decrease_dependent_vars, 
                                  input_independent_var = "uganda" )
# South West region
top2_southwest_dec <- top_n_analysis(input_summary_stats = summary.stats.list,
                                     input_n = 2, 
                                     input_dependent_vars = top2_price_decrease_dependent_vars, 
                                     input_independent_var = "south west" )
# West Nile region
top2_westnile_dec <- top_n_analysis(input_summary_stats = summary.stats.list,
                                    input_n = 2, 
                                    input_dependent_vars = top2_price_decrease_dependent_vars, 
                                    input_independent_var = "west nile" )


# bind all together in one data merge-ready file --------------------------

# bind all together , multiply by 100 and round up

top_analysis <- bind_cols(top3_uganda, top3_southwest, top3_westnile, 
                          top2_uganda, top2_uganda_dec, top2_southwest,
                          top2_southwest_dec, top2_westnile, top2_westnile_dec) %>% 
  mutate(across(where(is.numeric), ~.*100))


# select one and other analysis spread ------------------------------------

# non percentage vars analysis
non_perct_vars_dependent_vars = c("agents_number", "customer_number")

# all markets
non_perct_vars <- non_perct_vars_analysis(input_summary_stats = summary.stats.list, 
                                          input_dependent_vars = non_perct_vars_dependent_vars,
                                          input_independent_var = "uganda")

# South West
non_perct_vars_southwest <- non_perct_vars_analysis(input_summary_stats = summary.stats.list, 
                                                    input_dependent_vars = non_perct_vars_dependent_vars,
                                                    input_independent_var = "south west")

# West Nile
non_perct_vars_westnile <- non_perct_vars_analysis(input_summary_stats = summary.stats.list, 
                                                   input_dependent_vars = non_perct_vars_dependent_vars,
                                                   input_independent_var = "west nile")
# combine non-percent analysis
non_perct_vars_fin <- bind_cols(non_perct_vars, non_perct_vars_southwest, non_perct_vars_westnile)

# percentage vars analysis
perct_vars_dependent_vars = c("mobile_accepted", "vendor_number", "vendors_change",
                              "safety", "item_scarcity", "stock_runout")
# all markets
perct_vars <- perct_vars_analysis(input_summary_stats = summary.stats.list, 
                                  input_dependent_vars = perct_vars_dependent_vars,
                                  input_independent_var = "uganda")
# South West
perct_vars_southwest <- perct_vars_analysis(input_summary_stats = summary.stats.list, 
                                                input_dependent_vars = perct_vars_dependent_vars,
                                                input_independent_var = "south west")
# West Nile
perct_vars_westnile <- perct_vars_analysis(input_summary_stats = summary.stats.list, 
                                               input_dependent_vars = perct_vars_dependent_vars,
                                               input_independent_var = "west nile")
# combine percent analysis and multiply by 100
perct_vars_fin <- bind_cols(perct_vars, perct_vars_southwest, perct_vars_westnile) %>% 
  mutate(across(where(is.numeric), ~.*100))


# Data Merge Wrangling ----------------------------------------------------

# number of things assessed
num_assessed_merge <- data_merge_summary %>% 
  pivot_longer(cols = starts_with("num_"), names_to = "num_market_assessed", values_to = "num_assessed") %>% 
  mutate(new_var = paste0(num_market_assessed, "_", level)) %>% 
  ungroup() %>% 
  select(new_var, num_assessed) %>% 
  pivot_wider(names_from = new_var, values_from = c(num_assessed))

#       Items Prices         #collection_order

item_prices_cols_to_remove <- c("collection_order", "price_nails", "month")
# extracting relevant data - national
national <- jmmi_datamerge_filter_rename(input_df = national_items, input_yrmo_constructed = yrmo_constructed,
                                         input_unselection = item_prices_cols_to_remove, input_level = "national")
# extracting relevant data - regional
regional_sw <- jmmi_datamerge_filter_rename(input_df = region_items, input_yrmo_constructed = yrmo_constructed,
                                            input_unselection = item_prices_cols_to_remove, input_level = "southwest")
regional_wn <- jmmi_datamerge_filter_rename(input_df = region_items, input_yrmo_constructed = yrmo_constructed,
                                            input_unselection = item_prices_cols_to_remove, input_level = "westnile")
# extracting relevant data - settlement
settlement_dm <- settlement_items %>% 
  filter(yrmo == yrmo_constructed) %>% 
  ungroup() %>% 
  select(-c(collection_order, district, regions, price_nails, yrmo)) %>% 
  pivot_longer(cols = -settlement, names_to = "var_name", values_to = "var_value") %>% 
  mutate(new_var = paste0(settlement, "_", var_name)) %>% 
  select(new_var, var_value) %>% 
  pivot_wider(names_from = new_var, values_from = var_value)

# Percentage Change National #

change_national_march <- pct_change_data$change_national_march %>% 
  mutate(collection_order_perct_march = NULL) %>% 
  rename_with(.cols = everything(), .fn = ~paste0("national_", .x))

change_national_last_round <- pct_change_data$change_national_march %>% 
  mutate(collection_order_perct_last_round = NULL) %>% 
  rename_with(.cols = everything(), .fn = ~paste0("national_", .x))

# Percentage Change Regional #

# extracting relevant data - regional

percent_change_region_sw <- pct_change_data$percent_change_region %>% 
  filter(regions == "south west") %>% 
  ungroup() %>% 
  select(-c(collection_order_perct_last_round, collection_order_perct_march, regions) ) %>% 
  rename_with(.cols = everything(), .fn = ~paste0("southwest_", .x))
percent_change_region_wn <- pct_change_data$percent_change_region %>% 
  filter(regions == "west nile") %>% 
  ungroup() %>% 
  select(-c(collection_order_perct_last_round, collection_order_perct_march, regions) ) %>% 
  rename_with(.cols = everything(), .fn = ~paste0("westnile_", .x))

# Percentage Change Settlement #
percent_change_set <- pct_change_data$percent_change_settlement %>% 
  select(-contains("collection_")) %>% 
  ungroup() %>% 
  pivot_longer(cols = -settlement, names_to = "var_name", values_to = "var_value") %>% 
  mutate(new_var = paste0(settlement, "_", var_name)) %>% 
  select(new_var, var_value) %>% 
  pivot_wider(names_from = new_var, values_from = var_value)

#            MEBs            #

# Settlement data merge
meb_set <- meb_data$meb_items %>% 
  filter(yrmo == yrmo_constructed) %>% 
  ungroup() %>% 
  select(-c(collection_order, month, regions, district, yrmo)) %>% 
  pivot_longer(cols = -settlement, names_to = "var_name", values_to = "var_value") %>% 
  mutate(new_var = paste0(settlement, "_", var_name)) %>% 
  select(new_var, var_value) %>% 
  pivot_wider(names_from = new_var, values_from = var_value)

# Regional data merge
meb_cols_to_remove <- c("regions", "month", "collection_order", "yrmo")
meb_reg_sw <- jmmi_datamerge_filter_rename(input_df = meb_data$meb_items_regional, input_yrmo_constructed = yrmo_constructed,
                                           input_unselection = meb_cols_to_remove, input_level = "southwest")
meb_reg_wn <- jmmi_datamerge_filter_rename(input_df = meb_data$meb_items_regional, input_yrmo_constructed = yrmo_constructed,
                                           input_unselection = meb_cols_to_remove, input_level = "westnile")
# National data merge

meb_nat <- jmmi_datamerge_filter_rename(input_df = meb_data$meb_items_national, input_yrmo_constructed = yrmo_constructed,
                                        input_unselection = c("month", "collection_order"), input_level = "national")

#         MEB Ranks          #

rank_dm <- meb_data$rank_settlements %>% 
  mutate(new_var = paste0("expansive_rank_", rank)) %>% 
  ungroup() %>% 
  select(-c(rank, meb_full)) %>% 
  pivot_wider(names_from = new_var, values_from = settlement)

#         bind everything   #

data_merge <- bind_cols(top_analysis,
                        non_perct_vars_fin,
                        perct_vars_fin,
                        national,
                        regional_sw,
                        regional_wn,
                        settlement_dm,
                        change_national_march,
                        change_national_last_round,
                        percent_change_region_sw,
                        percent_change_region_wn,
                        percent_change_set,
                        meb_set,
                        meb_reg_sw,
                        meb_reg_wn,
                        meb_nat,
                        num_assessed_merge,
                        rank_dm
) %>% 
  mutate(across(where(is.numeric), ~round(., 0)))

# save the file
write_csv(x = data_merge,
          file = paste0("outputs/",
                 output_folder,
                 "/",
                 butteR::date_file_prefix(), "_",
                 yrmo_constructed,
                 "_jmmi_data_merge.csv"), na = "n/a" )
