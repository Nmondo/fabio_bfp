rm(list = ls())

###########################################################
########### LOADING PACKAGES #########
###########################################################

library("tidyverse")
library("data.table")
library(dplyr)

###########################################################
########### LOADING DATA #########
###########################################################

setwd("/home/mmondolfo/fabio_bfp/data/")

files <- c(
  "use_final.rds",
  "use_fd_final.rds",
  "sup_final.rds",
  "btd_final.rds",
  "use_final_bcp.rds",
  "use_fd_final_bcp.rds",
  "sup_final_bcp.rds",
  "btd_final_bcp.rds"
)

invisible(lapply(files, function(f) {
  assign(
    tools::file_path_sans_ext(f),
    readRDS(f) %>% filter(year %in% 2012:2022),
    envir = .GlobalEnv
  )
}))


setwd("/home/mmondolfo/fabio_bfp/")

y_bp_incomplete_rows <- readRDS("inputs_for_final_data/y_bp_incomplete_rows.rds") 
items_use_bcp <- read_csv("inst/items_use_bcp.csv")
items_full_bcp <- read_csv("inst/items_full_bcp.csv")
items_supply_bcp <- read_csv("inst/items_supply_bcp.csv")
regions <- read_csv("inst/regions_full.csv") %>% filter(current == TRUE)


###########################################################
########### MAKING VECTORS#########
###########################################################

extension_items <- unique(use_fd_final_bcp$item)



###########################################################
########### BINDING USE #########
###########################################################

use_full <- bind_rows(use_final, use_final_bcp) %>%
  select(-unit)




###########################################################
########### BINDING USE IN FINAL DEMAND #########
###########################################################

use_fd_full <- bind_rows(use_fd_final, use_fd_final_bcp) %>% 
  select(-iso3c, -unit) %>%
  mutate(across(c(food, losses, other, stock_addition, stock_withdrawal, tourist), 
                ~ if_else(item %in% extension_items, 0, .x)),
         across(c(fuel, other_industrial, unknown_use), 
                ~ case_when(item %in% extension_items & is.na(.x) ~ 0, 
                            ! item %in% extension_items           ~ 0,
                            TRUE                                  ~ .x))
         )


# Item info for the two comm_codes
new_items <- items_full_bcp %>%
  filter(comm_code %in% c("c901", "c999")) %>%
  select(comm_code, item_code, item) %>%
  distinct()

# All combinations of area_code × comm_code × year
new_fd_rows <- expand.grid(
  area_code = unique(use_fd_full$area_code),
  comm_code = c("c901", "c999"),
  year      = 2012:2022,
  stringsAsFactors = FALSE
) %>%
  left_join(new_items, by = "comm_code") %>%
  left_join(regions %>% select(area_code = code, area = name), by = "area_code")

# Add all remaining columns from use_fd_full as 0
missing_cols <- setdiff(names(use_fd_full), names(new_fd_rows))
for (col in missing_cols) {
  new_fd_rows[[col]] <- 0
}

# Align column order and bind
new_fd_rows <- new_fd_rows %>% select(any_of(names(use_fd_full)))
use_fd_full <- bind_rows(use_fd_full, new_fd_rows)

###########################################################
########### BINDING SUPPLY #########
###########################################################

sup_full <- bind_rows(sup_final, sup_final_bcp) %>%
  select(-production, -unit)




###########################################################
########### BINDING BTD #########
###########################################################

btd_full <- bind_rows(btd_final, btd_final_bcp) %>%
  select(-importer_iso3, -exporter_iso3, -item, -unit)

rm(btd_final)
gc()




###########################################################
########### UPDATING OR CREATING, AND JOINING "SELF-CONSUMPTION" FLOWS #########
###########################################################

supply_summary <- sup_full %>%
  group_by(comm_code, year, area_code) %>%
  summarise(prod = sum(supply, na.rm = TRUE), .groups = "drop")

use_summary <- use_full %>%
  group_by(comm_code, year, area_code) %>%
  summarise(use = sum(use, na.rm = TRUE), .groups = "drop")

exports_summary <- btd_full %>%
  filter(from_code != to_code) %>%
  group_by(comm_code, year, from_code) %>%
  summarise(exports = sum(value, na.rm = TRUE), .groups = "drop") %>%
  rename(area_code = from_code)

imports_summary <- btd_full %>%
  filter(from_code != to_code) %>%
  group_by(comm_code, year, to_code) %>%
  summarise(imports = sum(value, na.rm = TRUE), .groups = "drop") %>%
  rename(area_code = to_code)

self_flows <- use_fd_full %>%
  left_join(supply_summary, by = c("comm_code", "year", "area_code")) %>%
  left_join(use_summary, by = c("comm_code", "year", "area_code")) %>%
  left_join(exports_summary, by = c("comm_code", "year", "area_code")) %>%
  left_join(imports_summary, by = c("comm_code", "year", "area_code")) 

#Checking imbalances and joining their quantities as a use in fd. 

self_flows <- self_flows %>% 
  mutate(tot_supply = prod + imports + stock_withdrawal, 
         tot_use = use + food + losses + other + stock_addition + tourist + fuel + other_industrial + unknown_use + exports,
         imbalance = tot_supply - tot_use)

use_fd_full <- use_fd_full %>%
  left_join(self_flows %>% select(comm_code, area_code, year, imbalance), by = c("comm_code", "area_code", "year"))


self_flows <- self_flows %>%
  mutate(
    across(c(prod, imports, stock_withdrawal, use, food, losses, other, 
             stock_addition, tourist, fuel, other_industrial, unknown_use, exports, imbalance),
           ~ replace_na(.x, 0)),
    domestic_use = food + losses + other + stock_addition + tourist + 
      fuel + other_industrial + unknown_use + use + imbalance,
    prod_share = if_else(tot_supply > 0, prod / tot_supply, 0),
    self_consumption = prod_share * domestic_use
  )

