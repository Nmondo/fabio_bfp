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

setwd("/home/mmondolfo/inputs_for_final_data")

files <- c(
  "btd_final_bf_sua.rds",
  "btd_final_bf.rds",
  "btd_final_bp_bf_coproducts.rds",
  "supply_final_bf_sua.rds",
  "use_final_bf_sua.rds",
  "use_final_bp.rds",
  "y_bp_complete_rows.rds",
  "y_bp_incomplete_rows.rds",
  "y_final_bf.rds",
  "y_final_sua.rds",
  "y_final_bf_coproducts.rds"
)

invisible(lapply(files, function(f) {
  assign(tools::file_path_sans_ext(f), readRDS(f), envir = .GlobalEnv)
}))




###########################################################
########### FORMATTING USE TABLES #########
###########################################################
View(use_final_bf_sua)
View(use_final_bp)
