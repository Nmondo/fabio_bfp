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


###########################################################
########### REBALANCING USE IN FINAL DEMAND BASED ON USE AS BIOCHEMICALS FEEDSTOCK #########
###########################################################

to_subtract <- use_final_bcp %>%
  group_by(area_code, year, item_code) %>%
  summarise(input_use = sum(use, na.rm = TRUE),
            .groups = "drop")

use_fd_final_test <- use_fd_final %>%
  left_join(to_subtract, by = c("area_code","year","item_code"))

View(subset(use_fd_final_test, input_use > food + losses + other + stock_addition + tourist))
View(subset(use_fd_final_test, input_use > other + stock_addition + tourist & (input_use < food + losses + other + stock_addition + tourist)))

