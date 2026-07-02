rm(list = ls())

###########################################################
########### LOADING PACKAGES #########
###########################################################
setwd("/home/mmondolfo/fabio_bfp/")

library("tidyverse")
library("data.table")
library(dplyr)
source("R/00_system_variables.R")   # needed for na_sum()
source("R/00_run_config.R")          # RUN_MODE / BYPASS_RESCALE / tag()

###########################################################
########### LOADING DATA #########
###########################################################

cbs_sua_full <- readRDS("data/cbs_sua_full.rds")

if (BYPASS_RESCALE) {
  # ---- non-rescaled run: take the 08_02 pre-rescale inputs (08_03 ignored) ----
  cbs_sua_adjusted <- readRDS("intermediate_data/cbs_sua_adjusted.rds")
  use_final_bcp    <- readRDS("intermediate_data/use_rebal_bcp.rds")
} else {
  # ---- rescaled run: take the 08_03 RED-aligned inputs ----
  cbs_sua_adjusted <- readRDS("intermediate_data/cbs_sua_adjusted_resc.rds")
  use_final_bcp    <- readRDS("data/use_final_bcp.rds")
}
setDT(use_final_bcp)

###########################################################
####### Updating adjusted input use after rescaling in 08_03 ######
###########################################################

input_use_resc <- use_final_bcp[
  proc %in% c("Biodiesel production", "Renewable diesel production", "Biogasoline production"),
  .(input_use_adj_new = sum(use, na.rm = TRUE)),
  by = .(area_code, year, comm_code)
]
# align key types to cbs_sua_adjusted (area_code/year numeric, comm_code character)
input_use_resc[, `:=`(area_code = as.numeric(area_code),
                      year      = as.numeric(year),
                      comm_code = as.character(comm_code))]

cbs_sua_adjusted[, input_use_adj := 0]                       # wipe stale 08_02 values
cbs_sua_adjusted[input_use_resc, input_use_adj := i.input_use_adj_new,
                 on = .(area_code, year, comm_code)]         # set rescaled values on matches






###########################################################
####### ADJUSTING USES IN FINAL DEMAND FROM INPUT USE ######
###########################################################

###########################################################
#1. Sequence of adjustments from the best-corresponding use to the worst-corresponding use, then increase production if necessary (last resort) ######
###########################################################

cbs_sua_adjusted <- cbs_sua_adjusted %>%
  mutate(
    # Safe copies (NA -> 0) to work with
    other_f          = replace_na(other, 0),
    stock_addition_f = replace_na(stock_addition, 0),
    tourist_f        = replace_na(tourist, 0),
    processing_f     = replace_na(processing, 0),
    feed_f           = replace_na(feed, 0),
    food_f           = replace_na(food, 0),
    production_f     = replace_na(production, 0),
    # --- Step 1: draw from "other" ---
    to_remove_1   = replace_na(input_use_adj, 0),
    red_other     = pmin(to_remove_1, other_f),
    other_adj     = other_f - red_other,
    to_remove_2   = to_remove_1 - red_other,
    # --- Step 2: draw from "stock_addition" ---
    red_stock          = pmin(to_remove_2, stock_addition_f),
    stock_addition_adj = stock_addition_f - red_stock,
    to_remove_3        = to_remove_2 - red_stock,
    # --- Step 3: draw from "tourist" ---
    red_tourist   = pmin(to_remove_3, tourist_f),
    tourist_adj   = tourist_f - red_tourist,
    to_remove_4   = to_remove_3 - red_tourist,
    # --- Step 4: draw from "processing" ---
    red_processing = pmin(to_remove_4, processing_f),
    processing_adj = processing_f - red_processing,
    to_remove_5    = to_remove_4 - red_processing,
    # --- Step 5: draw from "feed" ---
    red_feed      = pmin(to_remove_5, feed_f),
    feed_adj      = feed_f - red_feed,
    to_remove_6   = to_remove_5 - red_feed,
    # --- Step 6: draw from "food" ---
    red_food      = pmin(to_remove_6, food_f),
    food_adj      = food_f - red_food,
    to_remove_7   = to_remove_6 - red_food,
    # --- Step 7: top up "production" with the residual ---
    production_adj = production_f + to_remove_7
  ) %>%
  # Overwrite originals with adjusted values, then drop helpers
  mutate(
    other          = other_adj,
    stock_addition = stock_addition_adj,
    tourist        = tourist_adj,
    processing     = processing_adj,
    feed           = feed_adj,
    food           = food_adj,
    production     = production_adj,
    input_use = input_use_adj
  ) %>%
  select(
    -ends_with("_f"),
    -ends_with("_adj"),
    -starts_with("to_remove_"),
    -starts_with("red_")
  ) %>% select(year:use, input_use) 

###########################################################
#2. Recalculate supply & use ######
###########################################################

cbs_sua_adjusted[, `:=`(domestic_supply = production)]
cbs_sua_adjusted[, `:=`(supply = na_sum(domestic_supply, imports))]
cbs_sua_adjusted[, `:=`(domestic_use = na_sum(food, feed, other, tourist, seed, losses, processing, stock_addition, -stock_withdrawal))]
cbs_sua_adjusted[, `:=`(use = na_sum(domestic_use, exports))]

cbs_sua_full <- cbs_sua_full %>% mutate(input_use = 0)
cbs_sua_bal <- rows_update(cbs_sua_full, cbs_sua_adjusted, by = c("item_code", "year", "area_code") ) %>%
  relocate(input_use, .after = balancing)



###########################################################
########### SAVING TABLES #########
###########################################################

setwd("/home/mmondolfo/fabio_bfp/")

saveRDS(cbs_sua_bal,   tag("data/cbs_sua_bal.rds"))


rm(list = ls())