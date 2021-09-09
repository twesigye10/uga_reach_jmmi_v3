library(tidyverse)

# combining MEB values for the 2021 reference -----------------------------------

meb_ref_item_references <- readxl::read_excel(path = "support_docs/Reference_2021_MEB_Items_source_18_08_2021.xlsx", 
                                              sheet = "Item references", range = "A1:AU14")
meb_ref_quantities <- readxl::read_excel(path = "support_docs/Reference_2021_MEB_Items_source_18_08_2021.xlsx", 
                                         sheet = "Item source price table", range = "B119:C130") %>% 
  janitor::clean_names() %>% 
  mutate(across(!where(is.numeric), .fns = ~paste0("q_", .x)))

meb_quantities_lookup <- setNames(object = meb_ref_quantities$meb_quantity_kg, nm = meb_ref_quantities$meb_quantities)

meb_ref_price_items <- meb_ref_item_references %>% 
  mutate(
    meb_maize_f =    price_maize_f * meb_quantities_lookup["q_meb_maize_f"],   
    meb_beans = price_beans * meb_quantities_lookup["q_meb_beans"],
    meb_sorghum = price_sorghum * meb_quantities_lookup["q_meb_sorghum"],
    meb_oil = price_oil * meb_quantities_lookup["q_meb_oil"],
    meb_salt = price_salt * meb_quantities_lookup["q_meb_salt"],
    meb_milk = price_milk * meb_quantities_lookup["q_meb_milk"],
    meb_dodo = price_dodo * meb_quantities_lookup["q_meb_dodo"],
    meb_fish = price_fish * meb_quantities_lookup["q_meb_fish"],
    meb_cassava = price_cassava * meb_quantities_lookup["q_meb_cassava"],
    meb_soap = price_soap * meb_quantities_lookup["q_meb_soap"],
    meb_firewood = price_firewood * meb_quantities_lookup["q_meb_firewood"],
    
  ) %>%
  select(starts_with("meb"))

# meb components
meb_components <- readxl::read_excel(path = "support_docs/Reference_2021_MEB_components_source.xlsx", sheet = "Reference 2021", range = "A1:Q14")

# combine the two data frames
combined_meb_components <- bind_cols(meb_components, meb_ref_price_items) %>% 
  select(c(settlement, district, regions, month, collection_order,
           meb_maize_f, meb_beans, meb_sorghum, meb_oil, meb_salt, meb_milk, meb_dodo,
           meb_fish, meb_cassava, meb_soap, meb_firewood),everything()) %>% 
  mutate(across(any_of(c("settlement",  "district", "regions")), .fns = str_to_lower )) %>% 
  rowwise() %>% 
  mutate(meb_full = sum(meb_food, meb_clothing, meb_water, meb_livelihoods, meb_education, meb_transport,
                            meb_health, meb_communication, meb_hygiene, meb_other_hdd, meb_energy, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(across(where(is.numeric), .fns = ~round(., 0)))

openxlsx::write.xlsx(x = combined_meb_components, file = "outputs/wfp_march_mebs_2021_new.xlsx", overwrite = TRUE)

# Item prices -------------------------------------------------------------
item_prices_reference <- readxl::read_excel(path = "support_docs/Reference_2021_MEB_Items_source_18_08_2021.xlsx", sheet = "Item references", range = "A1:AU14") %>% 
  mutate(across(any_of(c("settlement",  "district", "regions")), .fns = str_to_lower ),
         year = as.numeric(str_replace(string = year, pattern = "Ref ", replacement =  "")),
         month = 3) %>% 
  relocate(weight_cassava, .after = price_cassava) %>% 
  relocate(weight_dodo, .after = price_dodo) %>% 
  relocate(weight_fish, .after = price_fish) %>% 
  relocate(weight_firewood, .after = price_firewood) %>% 
  relocate(weight_charcoal, .after = price_charcoal) %>% 
  mutate(across(where(is.numeric), .fns = ~round(., 0)))

write_csv(x = item_prices_reference, file = "outputs/202103_market_monitoring_cleaned.csv")
