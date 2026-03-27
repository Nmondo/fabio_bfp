###########################################################
########### LOADING PACKAGES #########
###########################################################

library(tidyr)

###########################################################
########### LOADING DATA #########
###########################################################

########### FAO data #########

setwd("/home/mmondolfo/fabio_data_local/")
sua <- readRDS("data/tidy/sua_tidy.rds")
sua_btd <- readRDS("data/tidy/btd_sua_tidy.rds")
cbs_full <- readRDS("data/cbs_full.rds")

setwd("/home/mmondolfo/")




###########################################################
########### SELECTING RELEVANT TRADE DATA #########
###########################################################

sua_btd_disaggregation <- sua_btd %>%
  filter(
    grepl("^Animal or vegetable fats and oils and their fractions", item) |
      item == "Molasses" |
      item == "Oil of castor beans" |
      item == "Triticale"
  ) %>% 
  filter(unit == "tonnes")

rm(sua_btd)




######################################################################################################################
########### UPDATING THE CBS "Other" CATEGORIES #########
######################################################################################################################

#################################################################################################
#1. Selection of the rows for extension from SUAs #########
#################################################################################################

sua_extension <- sua %>%
  filter(grepl("^Animal or vegetable fats and oils and their fractions|Oil of castor beans|Molasses|Triticale", item)) %>%
  mutate(cbs_match = case_when(grepl("Animal or vegetable|Oil of castor beans", item) ~ "Oilcrops Oil, Other",
                               item == "Molasses" ~ "Sweeteners, Other",
                               item == "Triticale" ~ "Cereals, Other"))

sua_total_subtract <- sua_extension %>% group_by(area_code,year,cbs_match) %>%
  summarize(across(exports:use, ~ sum(.x,na.rm=TRUE)))

cbs_extension <- cbs_full %>% filter(item %in% c("Oilcrops Oil, Other","Sweeteners, Other","Cereals, Other"),
                                     year >= 2010)


#################################################################################################
#2. Subtracting the values from products included in the extension from their corresponding CBS category #########
#################################################################################################

#### Visibly an issue: some values are negative. Maybe cbs_full and sua_tidy do not come exactly from the same step and there is rebalancing upstream? 
cbs_extension <- cbs_extension %>%
  left_join(sua_total_subtract, 
            by = c("area_code", "year", "item" = "cbs_match"),
            suffix = c("", "_sua")) %>%
  mutate(across(exports:use, 
                ~ .x - coalesce(get(paste0(cur_column(), "_sua")), 0))) %>%
  select(-ends_with("_sua"))


#################################################################################################
#3. Updating the full CBS #########
#################################################################################################

cbs_full_after_extension <- cbs_full %>% rows_update(cbs_extension, by = c("area_code","year","item"))




######################################################################################################################
########### CLEANING THE EXTENSION #########
######################################################################################################################

sua_extension_clean <- sua_extension %>% 
  mutate(item = case_when(grepl("Animal or vegetable", item) ~ "Used cooking oil",
                          grepl("Oil of castor beans", item) ~ "Castor oil",
                          TRUE ~ item)) %>%
  select(-cbs_match) %>% 
  # Correcting UCO production, adding residuals
  mutate(production = case_when(item_code == 1274 & residuals < 0 ~ production - residuals,
                                TRUE ~ production),
         other = case_when(item_code == 1274 & residuals > 0 ~ other + residuals,
                           TRUE ~ other),
         residuals = case_when(item_code == 1274 ~ 0,
                                TRUE ~ residuals)
         )

sua_extension_btd_clean <- sua_btd_disaggregation %>% 
  mutate(item = case_when(grepl("Animal or vegetable", item) ~ "Used cooking oil",
                          grepl("Oil of castor beans", item) ~ "Castor oil",
                          TRUE ~ item))         




######################################################################################################################
########### SAVING TABLES #########
######################################################################################################################

saveRDS(sua_extension_clean, "sua_extension_clean.rds")
saveRDS(sua_extension_btd_clean, "sua_extension_btd_clean.rds")
saveRDS(cbs_full_after_extension, "cbs_extension_full.rds")

