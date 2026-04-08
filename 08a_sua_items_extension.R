###########################################################
########### LOADING PACKAGES #########
###########################################################

library(tidyr)

###########################################################
########### MAKING VECTORS #########
###########################################################

area_code_exclusion <- c(18,45,127,145,148,196,219,227)

###########################################################
########### LOADING DATA #########
###########################################################

########### FAO data #########

setwd("/home/mmondolfo/fabio_bfp/fabio_data_local")

sua <- readRDS("data/tidy/sua_tidy.rds") %>% filter(! area_code %in% area_code_exclusion)
sua_btd <- readRDS("data/tidy/btd_sua_tidy.rds") %>% filter(! from_code %in% area_code_exclusion,
                                                            ! to_code   %in% area_code_exclusion)
cbs_full <- readRDS("data/cbs_full.rds") %>% filter(! area_code %in% area_code_exclusion)

setwd("/home/bruckner2/fabio")
cbs_btd <- readRDS("data/btd_final.rds") ### !!!!


###########################################################
########### SELECTING RELEVANT TRADE DATA #########
###########################################################

cbs_btd_disaggregation <- cbs_btd %>% filter(item_code %in% c(2520, 2570,2543, 2586),
                                             year %in% 2012:2022) #2520: Cereals; 2570: Oilcrops; 2543 : Sweeteners; 2586: Oilcrops Oil 

sua_btd_disaggregation <- sua_btd %>%
  filter(
    grepl("^Animal or vegetable fats and oils and their fractions", item) |
      item == "Molasses" |
      item == "Oil of castor beans" |
      item == "Castor oil seeds" |
      item == "Triticale"
  ) %>% 
  filter(unit == "tonnes") %>%
  mutate(cbs_match = case_when(grepl("^Animal or vegetable fats and oils|Oil of castor beans", item) ~ 2586,
                               item == "Castor oil seeds" ~ 2570,
                               item == "Molasses" ~ 2543,
                               item == "Triticale" ~ 2520))


###########################################################
########### UPDATING TRADE DATA FOR CBS "Other" CATEGORIES #########
###########################################################

###########################################################
#1. Updating bilateral trade for "Other" categories, lower bound at 0 #########
###########################################################

values_cbs_btd_update <- sua_btd_disaggregation %>%
  group_by(cbs_match, from_code, to_code, year) %>%
  summarise(to_subtract = sum(value, na.rm = TRUE),
            .groups = "drop")

cbs_btd_update <- cbs_btd_disaggregation %>%
  left_join(values_cbs_btd_update, by = c("item_code" = "cbs_match", "from_code", "to_code", "year")) %>%
  mutate(value = ifelse(is.na(to_subtract), value, pmax(value - to_subtract, 0))) %>%
  select(-to_subtract)

btd_final_cbs <- cbs_btd %>% 
  rows_update(cbs_btd_update, by = c("item_code", "from_code", "to_code", "year"))

rm(sua_btd,cbs_btd)

###########################################################
#2. Calculating totals for later join to cbs_extension #########
###########################################################
# 
# btd_imports <- cbs_btd_update %>%
#   summarise(
#     imports = sum(value, na.rm = TRUE),
#     .by = c(to_code, year, item_code)
#   ) %>%
#   rename(area_code = to_code)
# 
# btd_exports <- cbs_btd_update %>%
#   summarise(
#     exports = sum(value, na.rm = TRUE),
#     .by = c(from_code, year, item_code)
#   ) %>%
#   rename(area_code = from_code)
# 
# btd_update_wide <- btd_imports %>%
#   full_join(btd_exports, by = c("area_code", "year", "item_code"))
# 
# 


######################################################################################################################
########### UPDATING THE CBS "Other" CATEGORIES #########
######################################################################################################################

#################################################################################################
#1. Selection of the rows for extension from SUAs #########
#################################################################################################

sua_extension <- sua %>%
  filter(grepl("^Animal or vegetable fats and oils and their fractions|Oil of castor beans|Castor oil seeds|Molasses|Triticale", item)) %>%
  mutate(cbs_match = case_when(grepl("Animal or vegetable|Oil of castor beans", item) ~ "Oilcrops Oil, Other",
                               item == "Castor oil seeds" ~ "Oilcrops, Other",
                               item == "Molasses" ~ "Sweeteners, Other",
                               item == "Triticale" ~ "Cereals, Other"))

sua_total_subtract <- sua_extension %>% group_by(area_code,year,cbs_match) %>%
  summarize(across(exports:use, ~ sum(.x,na.rm=TRUE)))

cbs_extension <- cbs_full %>% filter(item %in% c("Oilcrops Oil, Other","Oilcrops, Other","Sweeteners, Other","Cereals, Other"),
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
  select(-ends_with("_sua")) %>% 
  # Update total imports and exports from updated btd
  rows_update(
    btd_update_wide,
    by = c("item_code", "year", "area_code"),
    unmatched = "ignore"
  )



#################################################################################################
#3. Updating the full CBS #########
#################################################################################################

cbs_full_after_extension <- cbs_full %>% rows_update(cbs_extension, by = c("area_code","year","item")) %>%
  filter(year %in% 2012:2022)


#################################################################################################
#4. Lower bound at 0 for values in CBS extension #########
#################################################################################################

cbs_full_after_extension <- cbs_full_after_extension %>%
  mutate(across(supply:use, ~ ifelse(.x < 0, 0, .x)),
         domestic_supply = rowSums(cbind(production, stock_withdrawal), na.rm = TRUE),
         supply          = rowSums(cbind(production, stock_withdrawal ,imports), na.rm = TRUE),
         domestic_use    = rowSums(cbind(food, feed, seed, processing, losses,
                                         other, tourist, stock_addition), na.rm = TRUE),
         use             = rowSums(cbind(food, feed, seed, processing, losses,
                                         other, tourist, stock_addition,
                                         exports), na.rm = TRUE),
         imbalance = supply-use
       ) # Here we have some imbalances 


View(subset(cbs_full_after_extension, item %in% c("Oilcrops, Other", "Cereals, Other", "Oilcrops Oil, Other", "Sweeteners, Other")))

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
  select(-cbs_match) %>%
  mutate(item = case_when(grepl("Animal or vegetable", item) ~ "Used cooking oil",
                          grepl("Oil of castor beans", item) ~ "Castor oil",
                          TRUE ~ item))         




######################################################################################################################
########### SAVING TABLES #########
######################################################################################################################

setwd("/home/mmondolfo/fabio_bfp/")

saveRDS(sua_extension_clean, "intermediate_data/sua_extension_clean.rds")
saveRDS(sua_extension_btd_clean, "intermediate_data/sua_extension_btd_clean.rds")
saveRDS(cbs_full_after_extension, "intermediate_data/cbs_extension_full.rds")
saveRDS(btd_final_cbs, "inputs_for_final_data/btd_final_cbs.rds")

rm(list = ls())




