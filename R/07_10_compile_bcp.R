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
  "supply_final_bf.rds",
  "supply_final_bp.rds",
  "use_final_bf.rds",
  "use_final_bp.rds",
  "y_bp_complete_rows.rds",
  "y_bp_incomplete_rows.rds",
  "y_final_bf.rds",
  "y_final_bf_coproducts.rds"
)

invisible(lapply(files, function(f) {
  assign(tools::file_path_sans_ext(f), readRDS(f), envir = .GlobalEnv)
}))

############# Loading lists of items, processes, regions ##################

setwd("/home/mmondolfo/fabio_bfp/")
regions <- read.csv("inst/regions_full.csv", fileEncoding = "latin1") %>% filter(current == TRUE)
items_supply_bcp <- read.csv("inst/items_supply_bcp.csv")
items_full_bcp <- read.csv("inst/items_full_bcp.csv")
items_use_bcp <- read.csv("inst/items_use_bcp.csv")


###########################################################
########### FORMATTING USE TABLES #########
###########################################################

use_compiled_bcp <- bind_rows(use_final_bf, use_final_bp) %>%
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
                          TRUE ~ unit),
         item = case_when(item == "Castor oil" ~ "Oil of castor beans",
                          item == "Used cooking oil" ~ "Animal or vegetable fats and oils and their fractions, chemically modified, except those hydrogenated, inter-esterified, re-esterified or elaidinized; inedible mixtures or preparations of animal or vegetable fats or oils",
                          TRUE ~ item)) %>%
  left_join(items_use_bcp, by = c("proc", "item")) %>%
  left_join(regions %>% select(iso3c, area_code = code, area = name), by = c("iso3c")) %>%
  filter(year %in% 2012:2022,
         !is.na(area)) %>%
  select(-iso3c) 

subset(use_compiled_bcp, area == "Italy" & proc == "Biogasoline production")




###########################################################
########### FORMATTING FINAL DEMAND TABLES #########
###########################################################

use_fd_final_bcp <- bind_rows(y_final_bf, y_final_bf_coproducts, y_bp_complete_rows) %>%
  mutate(across(c(fuel, non_fuel, unknown_use, other_industry_use), 
                ~ case_when(item == "Bionaphtha" ~ (1/0.71)*.x,
                            item == "Biopropane" ~ (1/0.51)*.x,
                            TRUE ~ .x)),
         unit = ifelse(item %in% c("Biopropane", "Bionaphtha"), "Ml", unit)) %>%
  mutate(across(c(fuel, non_fuel, unknown_use, other_industry_use), 
                ~ case_when(unit %in% c("kt","Ml") ~ .x*1000,
                            TRUE ~ .x)),
         unit = case_when(unit == "kt" ~ "tonnes",
                          unit == "Ml" ~ "1000 liters", 
                          TRUE ~ unit),
         item = case_when(item == "Castor oil" ~ "Oil of castor beans",
                          item == "Used cooking oil" ~ "Animal or vegetable fats and oils and their fractions, chemically modified, except those hydrogenated, inter-esterified, re-esterified or elaidinized; inedible mixtures or preparations of animal or vegetable fats or oils",
                          TRUE ~ item),
         other_industrial = coalesce(non_fuel, other_industry_use)) %>%
  select(-non_fuel, -other_industry_use) %>%
  left_join(items_full_bcp %>% select(comm_code, item_code, item), by = c("item")) %>%
  left_join(regions %>% select(iso3c, area_code = code, area = name), by = "iso3c")



###########################################################
#1. Subtracting Bioethanol use as a BP feedstock from "Non-fuel use" #########
###########################################################

use_fd_final_bcp <- use_fd_final_bcp %>% 
  left_join(use_compiled_bcp %>% 
              filter(item=="Biogasoline") %>%
              select(item, year, to_subtract = use, area), by = c("item", "year", "area")) %>%
  mutate(other_industrial = ifelse(!is.na(to_subtract), other_industrial - to_subtract, other_industrial)) %>%
  relocate(c(other_industrial, unknown_use), .after = fuel) %>%
  relocate(c(comm_code, item_code), .after = item) %>%
  select(-to_subtract)


# Then there is y_bp_incomplete_rows which needs monetary MRIO linking




###########################################################
########### FORMATTING SUPPLY TABLES #########
###########################################################

supply_final_bcp <- bind_rows(supply_final_bf, supply_final_bp) %>%
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
                          TRUE ~ unit),
         item = case_when(item == "Castor oil" ~ "Oil of castor beans",
                          item == "Used cooking oil" ~ "Animal or vegetable fats and oils and their fractions, chemically modified, except those hydrogenated, inter-esterified, re-esterified or elaidinized; inedible mixtures or preparations of animal or vegetable fats or oils",
                          TRUE ~ item)) %>%
  left_join(items_supply_bcp, by = c("proc", "item")) %>%
  left_join(regions %>% select(iso3c, area_code = code, area = name), by = c("iso3c")) %>%
  filter(year %in% 2012:2022,
         !is.na(area)) %>%
  select(-iso3c)


# Adding supply of "Other, Unknown" and "Other, Waste" (own supply)

# 1. Aggregate supply from use_compiled_bcp
supply_sums <- use_compiled_bcp %>%
  filter(comm_code %in% c("c901", "c999")) %>%
  group_by(year, comm_code, area_code) %>%
  summarise(supply = sum(use, na.rm = TRUE), .groups = "drop")

# 2. Build scaffold
new_rows <- expand_grid(
  comm_code = c("c901", "c999"),
  area_code = unique(supply_final_bcp$area_code),
  year      = 2012:2022
) %>%
  mutate(
    unit      = "tonnes"
  ) %>%
  # 3. Attach aggregated supply
  left_join(supply_sums, by = c("comm_code", "area_code", "year")) %>%
  mutate(supply = ifelse(is.na(supply), 0, supply)) %>%
  # 4. Attach proc_code and proc
  left_join(
    items_supply_bcp %>% select(comm_code, item, item_code, proc_code, proc),
    by = "comm_code"
  ) %>%
  left_join(regions %>% select(area_code = code, area = name), by = "area_code")

# 6. Push into supply_final_bcp
supply_final_bcp <- supply_final_bcp %>%
  bind_rows(new_rows)




###########################################################
########### FORMATTING BTD TABLES #########
###########################################################

btd_final_bcp <- bind_rows(btd_final_bf, btd_final_bp_bf_coproducts) %>%
  rename(item = product) %>%
  mutate(value = case_when(item == "Bionaphtha" ~ (1/0.71)*value,
                           item == "Biopropane" ~ (1/0.51)*value,
                           TRUE ~ value),
         unit = ifelse(item %in% c("Biopropane", "Bionaphtha"), "Ml", unit)) %>%
  mutate(value = case_when(unit %in% c("kt","Ml") ~ value*1000,
                           TRUE ~ value),
         unit = case_when(unit == "kt" ~ "tonnes",
                          unit == "Ml" ~ "1000 liters", 
                          TRUE ~ unit),
         item = case_when(item == "Castor oil" ~ "Oil of castor beans",
                          item == "Used cooking oil" ~ "Animal or vegetable fats and oils and their fractions, chemically modified, except those hydrogenated, inter-esterified, re-esterified or elaidinized; inedible mixtures or preparations of animal or vegetable fats or oils",
                          TRUE ~ item)) %>%
  select(-type) %>%
  left_join(regions %>% select(from_code = code, iso3c), by = c("exporter_iso3" = "iso3c")) %>%
  left_join(regions %>% select(to_code = code, iso3c), by = c("importer_iso3" = "iso3c")) %>%
  left_join(items_full_bcp %>% select(item, item_code, comm_code), by = "item") %>%
  select(-exporter_iso3, -importer_iso3)




###########################################################
########### WRITING TABLES #########
###########################################################

setwd("/home/mmondolfo/fabio_bfp/")

write_rds(use_compiled_bcp, "intermediate_data/use_compiled_bcp.rds")

write_rds(use_fd_final_bcp, "data/use_fd_final_bcp.rds")
write_rds(supply_final_bcp, "data/sup_final_bcp.rds")
write_rds(btd_final_bcp, "data/btd_final_bcp.rds")

rm(list = ls())