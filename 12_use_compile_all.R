###########################################################
########### LOADING PACKAGES #########
###########################################################

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

###########################################################
########### LOADING DATA #########
###########################################################

############# Loading all the data necessary for final compilation ##################

setwd("/home/mmondolfo/fabio_bfp/inputs_for_final_data")

files <- c(
  "btd_final_bf.rds",
  "btd_final_bp_bf_coproducts.rds",
  "btd_final_sua.rds",
  "supply_final_bf_sua.rds",
  "supply_final_bp.rds",
  "use_final_bf.rds",
  "use_final_bp.rds",
  "use_final_sua.rds",
  "y_bp_complete_rows.rds",
  "y_bp_incomplete_rows.rds",
  "y_final_bf.rds",
  "y_final_bf_coproducts.rds",
  "y_final_cbs.rds",
  "y_final_sua.rds"
)

invisible(lapply(files, function(f) {
  assign(tools::file_path_sans_ext(f), readRDS(f), envir = .GlobalEnv)
}))

############# Loading lists of items, processes, regions ##################

setwd("/home/mmondolfo/fabio_bfp/inst")
regions <- read_csv("regions.csv")
items_supply_bcp <- read_csv("items_supply_bcp.csv")
items_full_bcp <- read_csv("items_full_bcp.csv")
items_use_bcp <- read_csv("items_use_bcp.csv")




# use_final_existing <- readRDS("/home/bruckner2/fabio/data/use_final.rds")
# use_final_existing
# sup_final_existing <- readRDS("/home/bruckner2/fabio/data/sup_final.rds")
# sup_final_existing
# use_fd_final_existing <- readRDS("/home/bruckner2/fabio/data/use_fd_final.rds")
# use_fd_final_existing 
#btd_final_existing <- readRDS("/home/bruckner2/fabio/data/btd_final.rds")
# btd_final_existing

###########################################################
########### FORMATTING USE TABLES #########
###########################################################

use_final_bcp <- bind_rows(use_final_bf, use_final_bp, use_final_sua) %>%
  # converting all liquid fuels to liters
  mutate(use = case_when(item == "Bionaphtha" ~ (1/0.71)*use,
                         item == "Biopropane" ~ (1/0.51)*use,
                         TRUE ~ use),
         unit = ifelse(item %in% c("Biopropane", "Bionaphtha"), "Ml", unit)) %>%
  # Standardizing units to tonnes or 1000 liters
  mutate(use = case_when(unit %in% c("kt","Ml") ~ use*1000,
                         TRUE ~ use),
         unit = case_when(unit == "kt" ~ "tonnes",
                          unit == "Ml" ~ "1000 liters", 
                          TRUE ~ unit)) %>%
  left_join(items_use_bcp, by = c("proc", "item")) %>%
  left_join(regions %>% select(iso3c, area_code = code, area = name), by = c("iso3c")) %>%
  filter(year %in% 2012:2022,
         !is.na(area)) %>%
  select(-iso3c)




###########################################################
########### FORMATTING SUPPLY TABLES #########
###########################################################

supply_final_bcp <- bind_rows(supply_final_bf_sua, supply_final_bp) %>%
  rename(item = product) %>%
  # converting all liquid fuels to liters
  mutate(supply = case_when(item == "Bionaphtha" ~ (1/0.71)*supply,
                         item == "Biopropane" ~ (1/0.51)*supply,
                         TRUE ~ supply),
         unit = ifelse(item %in% c("Biopropane", "Bionaphtha"), "Ml", unit)) %>%
  # Standardizing units to tonnes or 1000 liters
  mutate(supply = case_when(unit %in% c("kt","Ml") ~ supply*1000,
                         TRUE ~ supply),
         unit = case_when(unit == "kt" ~ "tonnes",
                          unit == "Ml" ~ "1000 liters", 
                          TRUE ~ unit)) %>%
  left_join(items_supply_bcp, by = c("proc", "item")) %>%
  left_join(regions %>% select(iso3c, area_code = code, area = name), by = c("iso3c")) %>%
  filter(year %in% 2012:2022,
         !is.na(area)) %>%
  select(-iso3c)

unique(regions$name[!(regions$name %in% supply_final_bcp$area)]) # checking the completeness of supply
unique(supply_final_bcp$area[!( supply_final_bcp$area %in% regions$name)]) # checking whether there is any region that does not find a match in regions.  




###########################################################
########### FORMATTING FINAL DEMAND TABLES #########
###########################################################

use_fd_final_bcp <- bind_rows(y_final_bf, y_final_bf_coproducts, y_final_sua, y_bp_complete_rows) %>%
  mutate(across(c(fuel, non_fuel, unknown_use, food, losses, other, stock_addition, stock_withdrawal, tourist, other_industry_use), ~ case_when(item == "Bionaphtha" ~ (1/0.71)*.x,
                                                                                                                                                item == "Biopropane" ~ (1/0.51)*.x,
                                                                                                                                                TRUE ~ .x)),
         unit = ifelse(item %in% c("Biopropane", "Bionaphtha"), "Ml", unit)) %>%
  mutate(across(c(fuel, non_fuel, unknown_use, food, losses, other, stock_addition, stock_withdrawal, tourist, other_industry_use), ~ case_when(unit %in% c("kt","Ml") ~ .x*1000,
                            TRUE ~ .x)),
         unit = case_when(unit == "kt" ~ "tonnes",
                          unit == "Ml" ~ "1000 liters", 
                          TRUE ~ unit))  %>%
  select(-item_code) %>%
  left_join(items_full_bcp %>% select(comm_code, item_code, item), by = c("item")) %>%
  rows_update(regions %>% select(iso3c, area_code = code, area = name), by = "iso3c") %>%
  rows_update(regions %>% select(iso3c, area_code = code, area = name), by = "area_code")

###########################################################
#1. Subtracting Bioethanol use as a BP feedstock from "Non-fuel use" #########
###########################################################

use_fd_final_bcp <- use_fd_final_bcp %>% 
  left_join(use_final_bcp %>% 
              filter(item=="Biogasoline") %>%
              select(item, year, to_subtract = use, area), by = c("item", "year", "area")) %>%
  mutate(non_fuel = ifelse(!is.na(to_subtract), non_fuel - to_subtract, non_fuel)) %>%
  relocate(c(fuel, non_fuel, unknown_use), .after = other_industry_use) %>%
  relocate(c(comm_code, item_code), .after = item) %>%
  select(-to_subtract)

View(use_fd_final_bcp %>% filter(item=="Biogasoline")) ## HERE WE GET NEGATIVE VALUES FOR NON FUEL USE FOR EGYPT => where do we correct?



# Then there is y_bp_incomplete_rows which needs monetary MRIO linking


###########################################################
########### FORMATTING BTD TABLES #########
###########################################################

btd_final_bcp <- bind_rows(btd_final_bf, btd_final_bp_bf_coproducts, btd_final_sua) %>%
  rename(item = product) %>%
  mutate(value = case_when(item == "Bionaphtha" ~ (1/0.71)*value,
                           item == "Biopropane" ~ (1/0.51)*value,
                           TRUE ~ value),
         unit = ifelse(item %in% c("Biopropane", "Bionaphtha"), "Ml", unit)) %>%
  mutate(value = case_when(unit %in% c("kt","Ml") ~ value*1000,
                           TRUE ~ value),
         unit = case_when(unit == "kt" ~ "tonnes",
                          unit == "Ml" ~ "1000 liters", 
                          TRUE ~ unit)) %>%
  select(-type) %>%
  left_join(regions %>% select(from_code = code, iso3c), by = c("exporter_iso3" = "iso3c")) %>%
  left_join(regions %>% select(to_code = code, iso3c), by = c("importer_iso3" = "iso3c")) %>%
  left_join(items_full_bcp %>% select(item, item_code, comm_code), by = "item")


print(btd_final_bcp[is.na(btd_final_bcp$from_code),]) # want no NA exporter
View(btd_final_bcp[is.na(btd_final_bcp$to_code),]) # want no NA importer
print(btd_final_bcp[is.na(btd_final_bcp$comm_code),]) # want no NA on comm_code (OK)

# rm(list = ls())

