###########################################################
########### LOADING PACKAGES #########
###########################################################

library(tidyr)

###########################################################
########### LOADING DATA #########
###########################################################

########### FABIO regions #########

setwd("/home/mmondolfo/fabio_data_local/")

regions <- read.csv("inst/regions.csv") 

############## Final tables for biofuels ############## 

setwd("/home/mmondolfo/final_data/")

btd_final_biofuels <- readRDS("btd_final_biofuels.rds")
supply_final_biofuels <- readRDS("supply_final_biofuels.rds")

############## Pre-cleaned SUA extension ############## 

setwd("/home/mmondolfo/")

sua_extension_clean <- readRDS("sua_extension_clean.rds")
sua_extension_btd_clean <- readRDS("sua_extension_btd_clean.rds")
cbs_full_after_extension <- readRDS("cbs_extension_full.rds")

items_full_nonfood <- readRDS("items_full_nonfood.rds")
items_supply_nonfood <- readRDS("items_supply_nonfood.rds")




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
  left_join(items_supply_nonfood %>% select(proc, item), by = c("product" = "item"))

#joining
supply_final_biofuels_sua <- bind_rows(supply_final_biofuels,
                          sua_extension_supply)




###########################################################
#2. To BTD table #########
###########################################################

sua_extension_btd <- sua_extension_btd_clean %>%
  #joining exporter ISO3c
  left_join(regions %>% select(code, iso3c), by = c("from_code" = "code")) %>%
  rename(exporter_iso3 = iso3c) %>%
  #joining importer ISO3c
  left_join(regions %>% select(code, iso3c), by = c("to_code" = "code")) %>%
  rename(importer_iso3 = iso3c) %>%
  #formatting
  select(-c(to, to_code, from, from_code, item_code)) %>%
  rename(product = item)

#joining
btd_final_biofuels_sua <- bind_rows(btd_final_biofuels, sua_extension_btd)
  
