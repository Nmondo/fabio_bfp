###########################################################
########### LOADING PACKAGES #########
###########################################################

library(readr)
library(readxl)

###########################################################
########### LOADING DATA #########
###########################################################

setwd("/home/bruckner2/fabio/")

########### FABIO regions #########

regions <- read.csv("inst/regions.csv") 

########### FAO data #########

cbs_full <- readRDS("data/cbs_full.rds")
sua <- readRDS("data/tidy/sua_tidy.rds")
tcf_sua <- readRDS("data/tidy/tcf_sua_tidy.rds")
sua_btd <- readRDS("data/tidy/btd_sua_tidy.rds")

# Create directory structure
setwd("/home/mmondolfo/")
dir.create("fabio_data_local/inst",      recursive = TRUE, showWarnings = FALSE)
dir.create("fabio_data_local/data/tidy", recursive = TRUE, showWarnings = FALSE)

# inst/ — CSV (mirrors original read.csv)
write.csv(regions, "fabio_data_local/inst/regions.csv", row.names = FALSE)

# data/ — top-level RDS files
saveRDS(cbs_full,   "fabio_data_local/data/cbs_full.rds")
saveRDS(sup_final,  "fabio_data_local/data/sup_final.rds")
saveRDS(sup, "fabio_data_local/data/sup.rds")
saveRDS(use_final,  "fabio_data_local/data/use_final.rds")

# data/tidy/ — tidy RDS files
saveRDS(sua,     "fabio_data_local/data/tidy/sua_tidy.rds")
saveRDS(tcf_sua, "fabio_data_local/data/tidy/tcf_sua_tidy.rds")
saveRDS(sua_btd, "fabio_data_local/data/tidy/btd_sua_tidy.rds")

rm(regions, cbs_full, sua, tcf_sua, sua_btd, sup_final, use_final,sup)
