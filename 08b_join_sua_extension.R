###########################################################
########### LOADING PACKAGES #########
###########################################################

library(tidyr)

###########################################################
########### LOADING DATA #########
###########################################################

########### FABIO regions & updated items list #########

setwd("/home/mmondolfo/fabio_bfp/")

regions <- read.csv("inst/regions.csv") 
items_full_bcp <- read_csv("inst/items_full_bcp.csv")
items_supply_bcp <- read_csv("inst/items_supply_bcp.csv")

############## Final tables for biofuels ############## 


btd_final_bf <- readRDS("inputs_for_final_data/btd_final_bf.rds")
supply_final_bf <- readRDS("inputs_for_final_data/supply_final_bf.rds")

############## Pre-cleaned SUA extension ############## 

setwd("/home/mmondolfo/fabio_bfp/intermediate_data/")

sua_extension_clean <- readRDS("sua_extension_clean.rds")
sua_extension_btd_clean <- readRDS("sua_extension_btd_clean.rds")
cbs_full_after_extension <- readRDS("cbs_extension_full.rds")




###########################################################
########### JOINING SUA EXTENSION #########
###########################################################

###########################################################
#1. To supply table #########
###########################################################

sua_extension_supply <- sua_extension_clean %>%
  left_join(regions %>% select(code, iso3c), by = c("area_code"="code")) %>%
  select(iso3c, year, item, production) %>%
  rename(supply = production,
         product = item) %>%
  mutate(unit = "tonnes") %>%
  #Joining process name
  left_join(items_supply_bcp  %>% select(proc, item), by = c("product" = "item"))

#joining
supply_final_bf_sua <- bind_rows(supply_final_bf,
                          sua_extension_supply)




###########################################################
#2. To BTD table #########
###########################################################

btd_final_sua <- sua_extension_btd_clean %>%
  #joining exporter ISO3c
  left_join(regions %>% select(code, iso3c), by = c("from_code" = "code")) %>%
  rename(exporter_iso3 = iso3c) %>%
  #joining importer ISO3c
  left_join(regions %>% select(code, iso3c), by = c("to_code" = "code")) %>%
  rename(importer_iso3 = iso3c) %>%
  #formatting
  select(-c(to, to_code, from, from_code, item_code)) %>%
  rename(product = item)



######################################################################################################################
########### USE TABLE EXTENSION #########
######################################################################################################################

use_sua_final <- sua_extension_clean %>%
  filter(item == "Castor oil seeds") %>%
  left_join(regions %>% select(code, iso3c), by = c("area_code" = "code"))

use_sua_final <- use_sua_final %>%
  select(iso3c, year, processing) %>%
  right_join(
    use_sua_final %>% distinct(iso3c, year),
    by = c("iso3c", "year")
  ) %>%
  rename(use = processing) %>%
  mutate(
    proc = "Castor oil production",
    item = "Castor oil seeds",
    unit = "tonnes"
  ) %>%
  select(proc, iso3c, year, item, use, unit)




###########################################################
########### CLEANING Y EXTENSION #########
###########################################################

sua_extension_clean <- sua_extension_clean %>% select(area_code, area, item_code, item, year, food, losses, other, stock_addition, stock_withdrawal, tourist) %>%
  mutate(unit = "tonnes")




###########################################################
########### SAVING TABLES #########
###########################################################

setwd("/home/mmondolfo/fabio_bfp/")

saveRDS(use_sua_final, "inputs_for_final_data/use_final_sua.rds")
saveRDS(supply_final_bf_sua, "inputs_for_final_data/supply_final_bf_sua.rds")
saveRDS(btd_final_sua, "inputs_for_final_data/btd_final_sua.rds")
saveRDS(sua_extension_clean, "inputs_for_final_data/y_final_sua.rds")
saveRDS(cbs_full_after_extension, "inputs_for_final_data/y_final_cbs.rds")

rm(list = ls())
