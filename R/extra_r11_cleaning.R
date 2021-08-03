# specific data cleaning for nov 2020 - round 11 (in case of re calculating past values)

month_specific_cleaning<-function(input_df){

  df %>% 
    mutate(vendors_change= case_when(yrmo == 202011 & vendors_change=="Less" ~ NA_character_,
                                     TRUE ~ vendors_change),
           weight_firewood= case_when(yrmo == 202011 & 
                                        settlement == "kiryandongo" & 
                                        weight_firewood == 2 ~ 6,
                                      TRUE ~ weight_firewood)
    )
}