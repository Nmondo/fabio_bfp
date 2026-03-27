###########################################################
########### LOADING PACKAGES #########
###########################################################

library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(stringr)
library(magrittr)
library(zoo)
library(broom)
library(purrr)


###########################################################
########### LOADING DATA #########
###########################################################

setwd("/home/bruckner2/fabio/")

########### FABIO regions #########

regions <- read.csv("inst/regions.csv") 

########### FAO Supply utilization accounts #########

sua <- readRDS("data/tidy/sua_tidy.rds")
tcf_sua <- readRDS("data/tidy/tcf_sua_tidy.rds")
View(subset(tcf_sua, item_sua %like% "Starch"))

########### Pre-cleaned supply & use data #########

setwd("/home/mmondolfo/")

use_bb_bp <- readRDS("use_bb_bp_base.rds")


########### Technical conversion factors #########

tcf_table <- readRDS("tcf_table_clean.rds")



###########################################################
########### JOINING TCFs #########
###########################################################

use_bb_bp %<>% left_join(tcf_table %>% select(input,output,input_unit,output_qty,output_unit), by = c("input","product" = "output")) %>%
  left_join(tcf_table %>% select(input,output,input_unit,output_qty,output_unit), by = c("feedstock_details" = "input","product" = "output")) %>%
  left_join(tcf_table %>% select(input,output,input_unit,output_qty,output_unit), by = c("feedstock_category" = "input","product" = "output")) %>%
  mutate(
    output_qty = coalesce(output_qty.x, output_qty.y, output_qty),
    output_unit = coalesce(output_unit.x, output_unit.y, output_unit),
    input_unit = coalesce(input_unit.x, input_unit.y, input_unit)
  ) %>%
  select(-ends_with(c(".X",".y")))


#### Sugars ####

sugars_use <- subset(sua, item %in% c("Raw cane or beet sugar (centrifugal only)","Refined sugar") | item %like% "Starch" ) # Would need to use the relative shares of Cane and Beet in the production of sugars (processing) to weight these two first categories. 

#### Starch ####

starch_use <- subset(sua, item %like% "Starch")
starch_use %<>% 
  group_by(area,year) %>%
  summarize(share_in_other = other/sum(other),
            item = item) %>%
  mutate(crop_match = case_when(item == "Starch of potatoes" ~ "Potatoes and products",
                                item == "Starch of cassava" ~ "Cassava and products",
                                item == "Starch of maize" ~ "Maize and products",
                                item == "Starch of wheat" ~ "Wheat and products",
                                item == "Starch of rice" ~ "Rice and products"))

castor_oil_use <- subset(sua, item %like% "castor")
