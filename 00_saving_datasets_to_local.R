###########################################################
########### LOADING PACKAGES #########
###########################################################

library(readr)
library(readxl)

###########################################################
########### LOADING DATA #########
###########################################################

setwd("/home/bruckner2/fabio/")

########### FAO data #########

cbs_full <- readRDS("data/cbs_full.rds")
sua <- readRDS("data/tidy/sua_tidy.rds")
sua_btd <- readRDS("data/tidy/btd_sua_tidy.rds")
tcf_sua_final <- readRDS("data/sua/tcf_sua_final.rds")

# Create directory structure
setwd("/home/mmondolfo/fabio_bfp/")
dir.create("fabio_data_local/data/tidy", recursive = TRUE, showWarnings = FALSE)
dir.create("fabio_data_local/data/sua", recursive = TRUE, showWarnings = FALSE)
dir.create("intermediate_data/", recursive = TRUE, showWarnings = FALSE)


# data/
saveRDS(cbs_full,   "fabio_data_local/data/cbs_full.rds")

# data/tidy/
saveRDS(sua,     "fabio_data_local/data/tidy/sua_tidy.rds")
saveRDS(sua_btd, "fabio_data_local/data/tidy/btd_sua_tidy.rds")

# data/sua/
saveRDS(tcf_sua_final, "fabio_data_local/data/sua/tcf_sua_final.rds")


rm(regions, cbs_full, sua, tcf_sua_final, sua_btd)
