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

setwd("/home/mmondolfo/fabio_bfp/")

cbs_sua_full <- readRDS("data/cbs_sua_full.rds")
use_final_bcp <- readRDS("data/use_final_bcp.rds")


###########################################################
########### REBALANCING USE IN FINAL DEMAND BASED ON USE AS BIOCHEMICALS FEEDSTOCK #########
###########################################################

to_subtract <- use_final_bcp %>%
  group_by(area_code, year, item_code) %>%
  summarise(input_use = sum(use, na.rm = TRUE),
            .groups = "drop")

cbs_sua_adjusted <- cbs_sua_full %>%
  left_join(to_subtract, by = c("area_code","year","item_code")) %>%
  mutate(gap_bcp_use_to_all_uses      = pmax(0, input_use - (food + feed + processing + losses + other + stock_addition + tourist)),
         gap_bcp_use_to_credible_uses = pmax(0, input_use - (feed + processing + other + stock_addition + tourist))) %>%
  mutate(production = case_when(gap_bcp_use_to_credible_uses > 0 & item_code == 1274 ~ production + gap_bcp_use_to_credible_uses,
                                TRUE ~ production))

View(subset(cbs_sua_adjusted, gap_bcp_use_to_all_uses  > 0))
View(subset(cbs_sua_adjusted, gap_bcp_use_to_credible_uses > 0 & gap_bcp_use_to_all_uses  == 0))

