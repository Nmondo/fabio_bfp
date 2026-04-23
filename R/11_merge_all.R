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
source("R/00_system_variables.R")

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
########### REALLOCATING IMBALANCES #######################
###########################################################

###########################################################
#1. Calculating imbalances
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

balance_check <- use_fd_full %>%
  left_join(supply_summary, by = c("comm_code", "year", "area_code")) %>%
  left_join(use_summary, by = c("comm_code", "year", "area_code")) %>%
  left_join(exports_summary, by = c("comm_code", "year", "area_code")) %>%
  left_join(imports_summary, by = c("comm_code", "year", "area_code")) 

balance_check <- balance_check %>%
  mutate(across(c(imports, exports), ~ ifelse(comm_code %in% c("c901", "c999"), 0, .x)))

#Checking imbalances and joining their quantities as a use in fd. 

balance_check <- balance_check %>% 
  mutate(tot_supply = na_sum(prod, imports, stock_withdrawal, - exports), 
         tot_use    = na_sum(use, food, losses, other, stock_addition, 
           tourist, fuel, other_industrial, unknown_use),
         imbalance  = tot_supply - tot_use)




###########################################################
#2. Redistribute imbalances
###########################################################

fd_cols <- c("food", "losses", "other", "stock_addition", "tourist",
             "fuel", "other_industrial", "unknown_use")

# 1) Redistribute imbalances in balance_check
balance_check_adj <- balance_check %>%
  mutate(fd_sum = rowSums(across(all_of(fd_cols)), na.rm = TRUE)) %>%
  # Positive imbalance -> distribute proportionally across FD items
  mutate(across(all_of(fd_cols), ~ case_when(
    imbalance > 0 & fd_sum > 0 ~ .x + imbalance * (.x / fd_sum),
    TRUE                       ~ .x
  ))) %>%
  # Fallback: positive imbalance with fd_sum == 0 -> park in stock_addition
  mutate(stock_addition = if_else(imbalance > 0 & fd_sum == 0,
                                  stock_addition + imbalance,
                                  stock_addition),
         # Negative imbalance -> bump up production
         prod_new = if_else(imbalance < 0, prod + abs(imbalance), prod)) %>%
  # Recompute totals to verify zero imbalance
  mutate(tot_use_new    = na_sum(use, food, losses, other, stock_addition,
           tourist, fuel, other_industrial, unknown_use),
         tot_supply_new = na_sum(prod_new, imports, stock_withdrawal, exports),
         imbalance_new  = tot_supply_new - tot_use_new)


###########################################################
#2. Updating use in final demand with readjusted values
###########################################################

use_fd_full <- use_fd_full %>%
  select(-all_of(fd_cols)) %>%
  left_join(
    balance_check_adj %>% select(comm_code, area_code, year, all_of(fd_cols)),
    by = c("comm_code", "area_code", "year")
  )


###########################################################
#3. Updating supply with readjusted values
###########################################################

prod_update <- balance_check_adj %>%
  select(comm_code, area_code, year, prod_new)

sup_full <- sup_full %>%
  left_join(prod_update, by = c("comm_code", "area_code", "year")) %>%
  mutate(supply = if_else(!is.na(prod_new), prod_new, supply)) %>%
  select(-prod_new)




###########################################################
########### CALCULATE 'SELF-FLOWS' AND UPDATE BTD #######################
###########################################################

self_flows <- balance_check_adj %>%
  mutate(value = pmax(na_sum(tot_supply_new, - imports), 0), # 'self-consumption' is the part of use that is neither imported nor exported 
         from_code = area_code,
         to_code = area_code) %>%   
  select(comm_code, item_code, from_code, to_code, year, value)

## Sanity check 

print(subset(self_flows, is.na(value))) # no NA

btd_full <- rows_upsert(btd_full, self_flows, by = c("from_code", "to_code", "comm_code", "year"))
