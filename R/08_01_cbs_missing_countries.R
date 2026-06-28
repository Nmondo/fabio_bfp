###########################################################
########### LOADING PACKAGES #########
###########################################################

setwd("/home/mmondolfo/fabio_bfp/")

library("data.table")
library(dplyr)
source("R/00_system_variables.R")


###########################################################
########### LOADING DATA #########
###########################################################

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full_bcp.csv")

# FABIO tables ------------------------------------------------------------------

btd <- readRDS("data/btd_final.rds")
cbs <- readRDS("data/cbs_full.rds")

# Use for BCP production -----------------------------------------------------------------

use_bcp <- readRDS("intermediate_data/use_compiled_bcp.rds")

# SUA for shares in use ------------------------------------------------------------------

sua <- readRDS("data/tidy/sua_tidy.rds") 

###########################################################
########### CREATING MISSING ROWS #########
###########################################################

# Collecting the list of countries to add
cbs_sua_ext <- subset(cbs, item_code %in% c(97, 165, 265, 266, 1274))
to_add <- setdiff(unique(regions$code), unique(cbs_sua_ext$area_code))


# Build the full combination of missing areas x years x items
missing_rows <- CJ(
  area_code  = to_add,
  year       = 2012:2022,
  item_code  = unique(cbs_sua_ext$item_code)
)

# Add all other columns from cbs_sua_ext, filled with NA
other_cols <- setdiff(names(cbs_sua_ext), c("area_code", "year", "item_code"))
missing_rows[, (other_cols) := NA_real_]

# Make sure column order matches cbs_sua_ext
setcolorder(missing_rows, names(cbs_sua_ext))




###########################################################
########### CALCULATING TOTAL EXPORTS AND IMPORTS #########
###########################################################

btd <- subset(btd, item_code %in% unique(missing_rows$item_code) & 
                (from_code %in% unique(missing_rows$area_code) | to_code %in% unique(missing_rows$area_code)) &
                to_code != from_code
)

ex_join <- btd %>%
  group_by(from_code, year, item_code) %>%
  summarise(exports = sum(ifelse(value == "NaN", 0, value)),
            .groups = "drop") %>%
  rename(area_code = from_code)


im_join <- btd %>%
  group_by(to_code, year, item_code) %>%
  summarise(imports = sum(ifelse(value == "NaN", 0, value)),
            .groups = "drop") %>%
  rename(area_code = to_code)

missing_rows <- missing_rows %>% 
  rows_patch(ex_join, by = c("area_code", "item_code", "year"), unmatched = "ignore") %>% 
  rows_patch(im_join, by = c("area_code", "item_code", "year"), unmatched = "ignore") %>%
  mutate(item = as.character(item),
         area = as.character(area))

missing_rows <- missing_rows %>%
  left_join(items %>% select(item, item_code) %>% distinct(item_code, .keep_all = TRUE),
            by = "item_code", suffix = c("", "_new")) %>%
  mutate(item = coalesce(item, item_new)) %>%
  select(-item_new) %>%
  rows_update(regions %>% select(area = name, area_code = code), by = "area_code", unmatched = "ignore")




###########################################################
########### JOINING ESTIMATED USE #########
###########################################################

# Calculating world shares by use for missing items
sua_shares <- sua %>%
  filter(item_code %in% c(97, 165, 265, 266, 1274)) %>%
  group_by(item_code, year) %>%
  summarise(
    across(c(feed, food, losses, other, processing, seed, tourist),
           ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    total = feed + food + losses + other + processing + seed + tourist,
    across(c(feed, food, losses, other, processing, seed, tourist),
           ~ .x / total)
  ) %>%
  mutate(across(c(feed, food, losses, processing, seed, tourist), 
                ~ ifelse(item_code == 1274, 0, .x)),
         other = ifelse(item_code == 1274, 1, other)) %>%
  select(-total)

# Computing use
join_missing_use <- use_bcp %>%
  filter(item_code %in% c(97, 165, 265, 266, 1274) & area_code %in% unique(missing_rows$area_code)) %>%
  group_by(area_code, year, item_code) %>%
  summarise(input_use = sum(use, na.rm = TRUE),
            .groups = "drop")

# Estimating production (input_use + exports - imports > 0) or missing use (< 0)
missing_rows <- missing_rows %>% 
  left_join(join_missing_use, by = c("item_code", "area_code", "year")) %>%
  mutate(
    production_raw = ifelse(is.na(input_use), 0, input_use) + exports - imports,
    residual = abs(pmin(production_raw, 0)),   # negative residual (≤ 0), else 0
    production     = pmax(production_raw, 0)    # floored at 0
  ) %>%
  select(-production_raw)


# Estimating the distribution of uses from world shares
use_shares <- missing_rows %>%
  select(item_code, area_code, year, residual) %>%
  left_join(sua_shares, by = c("item_code", "year")) %>%
  mutate(
    across(c(feed, food, losses, other, processing, seed, tourist),
           ~ residual * .x)
  ) %>%
  select(item_code, area_code, year, feed, food, losses, other, processing, seed, tourist)

missing_rows <- missing_rows %>%
  rows_update(use_shares, by = c("item_code", "area_code", "year")) %>%
  select(-input_use, -residual)


cbs <- bind_rows(cbs, missing_rows)
cbs[, `:=`(domestic_supply = production)]
cbs[, `:=`(supply = na_sum(domestic_supply, imports))]
cbs[, `:=`(domestic_use = na_sum(food, feed, other, tourist, seed, losses, processing, stock_addition, -stock_withdrawal))]
cbs[, `:=`(use = na_sum(domestic_use, exports))]



###########################################################
########### SAVING FULL CBS WITH SUA EXTENSION #########
###########################################################

setwd("/home/mmondolfo/fabio_bfp/")

saveRDS(cbs, "data/cbs_sua_full.rds")

rm(list = ls())