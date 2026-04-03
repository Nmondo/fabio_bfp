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

setwd("/home/bruckner2/fabio/")
use_final_existing <- readRDS("data/use_final.rds") %>% filter(year %in% 2012:2022)
sup_final_existing <- readRDS("data/sup_final.rds") %>% filter(year %in% 2012:2022)
use_fd_final_existing <- readRDS("data/use_fd_final.rds") %>% filter(year %in% 2012:2022)


#probably should use "sup" and "use" rather than "...final"; and integrate it in the "07" and "08" files.

######################################################################################################################
########### DELETING USE IN ETHANOL PRODUCTION PROCESS AND ADDING THIS AS AN "OTHER" USE FINAL DEMAND COMPONENT #########
######################################################################################################################

use_final_update <- use_final_existing %>% 
  filter(! proc == "Alcohol production, Non-Food") 

use_fd_final_update <- use_fd_final_existing %>%
  left_join(
    use_final_existing %>% 
      filter(proc_code=="p084") 
    %>% select(area_code, item_code, year, use),
    by = c("area_code", "item_code", "year")
  ) %>%
  mutate(other = rowSums(cbind(other, use), na.rm = TRUE)) %>%
  select(-use)



######################################################################################################################
########### DELETING ETHANOL FROM SUPPLY TABLE #########
######################################################################################################################

sup_final_update <- sup_final_existing %>%
  filter(! proc == "Alcohol production, Non-Food")




######################################################################################################################
########### SAVING TABLES #########
######################################################################################################################

setwd("/home/mmondolfo/fabio_bfp/inputs_for_final_data/")

saveRDS(sup_final_update, "sup_final_cbs.rds")
saveRDS(use_final_update, "use_final_cbs.rds")
saveRDS(use_fd_final_update, "y_final_cbs.rds")

rm(list = ls())