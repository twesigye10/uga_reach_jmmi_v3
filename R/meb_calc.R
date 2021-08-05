# Uganda Market Monitoring - Food MEB and MEB Calculations

# load reference meb data
ref_mebs <- read_excel("./inputs/wfp_march_mebs.xlsx") %>% 
  mutate(yrmo = yrmo_to_include[1])

# median calculations on item prices

meb_items <- item_prices %>% 
  select(-c(uuid, market_final, price_maize_g, price_underwear, price_charcoal,
            price_pads, price_DAP, price_NKP, price_malathion, price_millet_f,
            -contains("_price"), month)
         ) %>%
  group_by(regions, district, settlement, yrmo) %>% 
  summarise(across(everything(), ~median(.x, na.rm = TRUE)), .groups = "drop_last")



