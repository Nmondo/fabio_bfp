##########################################################################################################
################ FINAL USE TABLES #####################################################
##########################################################################################################

options(scipen = 999)
options(digits = 5)

###########################################################
########### LOADING PACKAGES #########
###########################################################

library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(stringr)
library(magrittr)

###########################################################
########### LOADING DATA #########
###########################################################

setwd("/home/mmondolfo/fabio_bfp/")

########### Regions #########

regions <- read.csv("inst/regions_full.csv", fileEncoding = "latin1") %>% filter(current == TRUE)

########### Biofuels supply table #########

supply_final_bf <- readRDS("intermediate_data/supply_intermediate_bf.rds") %>%
  filter(product %in% c("Biodiesel","Renewable diesel","Biogasoline"))

########### Intermediate use table #########

setwd("/home/mmondolfo/fabio_bfp/intermediate_data/")

use_fame_hvo_full <- readRDS("use_fame_hvo_full.rds")
use_biogasoline_full <- readRDS("use_biogasoline_full.rds") %>% mutate(proc = "Biogasoline production")
use_table <- bind_rows(use_fame_hvo_full, use_biogasoline_full)

########### FAOSTAT non-food uses #########

faostat_non_food <- readRDS("fao_nonfood.rds")

########### TCFs #########

tcf_table <- readRDS("tcf_table_clean.rds")




###########################################################################################
########### FORMATTING #########
###########################################################################################

###########################################################################################
#1. FAOSTAT non-food #########
###########################################################################################

faostat_non_food <- faostat_non_food %>%
  rename(proc = use) %>%
  mutate(proc = case_when(proc == "Bioethanol" ~ "Biogasoline production",
                          proc == "Biodiesel & Renewable diesel" ~ "Biodiesel production")) %>%
  select(iso3c, proc, year, item, other)


###########################################################################################
#2. TCFs #########
###########################################################################################

tcf_table_clean <- tcf_table %>%
  filter(Type=="tcf",
         output %in% c("Biodiesel","Renewable diesel","Bioethanol"),
         is.na(input_subtype) | input_subtype %in% c("Molasses","Used cooking oil","Triticale")) %>%
  rename(proc = output) %>%
  mutate(proc = case_when(proc == "Biodiesel" ~ "Biodiesel production",
                          proc == "Renewable diesel" ~ "Renewable diesel production",
                          proc == "Bioethanol" ~ "Biogasoline production")) %>%
  select(-Type,-input_subtype,-output_qty_max,-input_unit,-output_unit,-input_qty,-(source:process))
  



###########################################################################################
########### READJUSTING FEEDSTOCK USE FROM READJUSTED TOTAL SUPPLY #########
###########################################################################################

###########################################################################################
#1. Collect the non-zero rows in use_table #########
###########################################################################################

use_table_nonzero <- use_table %>%
  filter(use > 0, year >= 2012) %>%
  #joining TCFs
  left_join(tcf_table_clean, by=c("proc","item"= "input")) %>%
  #joining total supply of biofuels by process, year, country
  left_join(supply_final_bf, by = c("proc","year","iso3c")) %>%
  
  rename(unit_use = unit.x,
         unit_supply = unit.y)
  
# check for failed TCF joins. 
# print(subset(use_table_nonzero, is.na(output_qty))) 


###########################################################################################
#2. Compute the adjusted use, based on initial feedstock-specific contributions to supply #########
###########################################################################################

use_table_supply_adjust <- use_table_nonzero %>% 
  group_by(proc,iso3c,year) %>%
  mutate(supply_contrib = use*output_qty,
         share_in_supply = supply_contrib/sum(supply_contrib),
         supply_contrib_adjust = share_in_supply*supply,
         use_adjust = supply_contrib_adjust*multiplier_output_kl_to_input_t)


###########################################################################################
#3. Final table #########
###########################################################################################

use_table_adjusted <- use_table_supply_adjust %>%
  select(proc,iso3c,year,item,use_adjust,unit_use) %>%
  rename(use = use_adjust,
         unit = unit_use)

## Collecting the differences caused by adjustment, for sensitivity analysis: 
balancing_use_diff_for_sensitivity <- use_table_supply_adjust %>%
  select(proc,iso3c,year,item,use,use_adjust)




###########################################################################################
########### GAP-FILLING FOR BIOFUEL PRODUCTION TECHNOLOGIES (ITERATIVE) #########
###########################################################################################

###########################################################################################
#0. Collecting the (country-year-process) that are currently missing feedstock use estimates
###########################################################################################

missing_use <- supply_final_bf %>%
  filter(supply > 0) %>%
  select(iso3c, year, proc, supply) %>%
  anti_join(
    use_table_nonzero %>% distinct(iso3c, year, proc),
    by = c("iso3c", "year", "proc")
  ) %>%
  arrange(proc, iso3c, year)


###########################################################################################
#1. Extracting the rows in FAOSTAT non food that will allow to fill some of the gaps
###########################################################################################

faostat_gapfill <- faostat_non_food %>%
  filter(item != "Sugar (Raw Equivalent)") %>%
  right_join(missing_use, by = c("iso3c", "year", "proc")) %>%
  mutate(item = case_when(proc %in% c("Biodiesel production", "Renewable diesel production") & item == "Oilcrops Oil, Other" ~ "Inedible animal or vegetable fats and oils",
                          TRUE ~ item)) %>%
  group_by(iso3c, year, proc) %>%
  mutate(other = other/sum(other)) %>%
  ungroup()


###########################################################################################
#2. Correcting feedstock use assumptions for selected countries (petrol-based or likely statistical discrepancies)
###########################################################################################

faostat_gapfill <- faostat_gapfill %>%
  filter(!(iso3c %in% c("IRN", "SAU", "ARE","QAT") & proc == "Biogasoline production"))

new_rows <- expand.grid(
  year  = unique(faostat_gapfill$year),
  iso3c = c("IRN", "SAU", "ARE","QAT"),
  stringsAsFactors = FALSE
) %>%
  mutate(
    proc  = "Biogasoline production",
    item  = "Other, Unknown",
    other = 1L
  )

faostat_gapfill <- bind_rows(faostat_gapfill, new_rows) 


###########################################################################################
#3. World averages for Renewable diesel production technologies (remaining non-zero values correspond to small statistical discrepancies, except for PRT)
###########################################################################################

rd_shares <- use_table_nonzero %>%
  filter(proc == "Renewable diesel production") %>%
  group_by(item, year) %>%
  summarize(total_use = sum(use, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  mutate(share = total_use / sum(total_use, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-total_use)

rd_combos <- faostat_gapfill %>%
  filter(proc == "Renewable diesel production") %>%
  distinct(iso3c, year)

# Step 2: Drop existing RD rows from faostat_gapfill
faostat_gapfill <- faostat_gapfill %>%
  filter(proc != "Renewable diesel production")

# Step 3: Build expanded rows — one per (iso3c, year, item) from rd_shares
new_rd_rows <- rd_combos %>%
  left_join(rd_shares, by = "year", relationship = "many-to-many") %>%   # joins item + share for each year
  rename(other = share) %>%
  mutate(proc = "Renewable diesel production")

# Step 4: Bind back
faostat_gapfill <- bind_rows(faostat_gapfill, new_rd_rows) %>%
  rows_upsert(missing_use %>% select(iso3c,year,proc,supply), by = c("iso3c", "year", "proc"))


###########################################################################################
#4. Joining TCFs
###########################################################################################

faostat_gapfill <- faostat_gapfill %>%
  left_join(tcf_table_clean, by=c("proc","item"="input"))


###########################################################################################
#5. Calculating shares in supply, contribution to supply and from that estimated use
###########################################################################################

faostat_gapfill <- faostat_gapfill %>%
  group_by(iso3c, year) %>%
  mutate(
    share_supply = (other / multiplier_output_kl_to_input_t) /
      sum(other / multiplier_output_kl_to_input_t, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(supply_contrib = share_supply*supply,
         use = supply_contrib*multiplier_output_kl_to_input_t,
         unit = "kt") %>%
  select(iso3c,proc,year,item,use,unit)

# Collecting final use rows for the gaps successfully filled

use_table_intermediate <- bind_rows(use_table_adjusted,faostat_gapfill %>% filter(!is.na(item)))


###########################################################################################
#6. Filling gaps from regional averages for remaining countries
###########################################################################################

#6a. Collecting regional averages (by year-process)
use_table_continent <- use_table_intermediate %>%
  left_join(regions %>% select(iso3c,continent), by = "iso3c") %>%
  mutate(continent = ifelse(continent %in% c("EU","EUR"), "EUR", continent))

use_table_continent <- use_table_continent %>%
  group_by(proc, year, item, continent) %>%
  summarize(use = sum(use, na.rm = TRUE), .groups = "drop") %>%
  group_by(proc, year, continent) %>%
  mutate(share = use / sum(use, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(continent))

# World averages: aggregate across all continents, assign to "ROW"
use_table_world <- use_table_intermediate %>%
  group_by(proc, year, item) %>%
  summarize(use = sum(use, na.rm = TRUE), .groups = "drop") %>%
  group_by(proc, year) %>%
  mutate(
    share     = use / sum(use, na.rm = TRUE),
    continent = "ROW"
  ) %>%
  ungroup()

use_table_continent <- bind_rows(use_table_continent, use_table_world)

#6b. Collecting country-year-process combinations for which no "other" use was found in FAOSTAT. 
last_missing_use <- subset(faostat_gapfill, (is.na(item))) %>% 
  select(-item,-use) %>%
  left_join(regions %>% select(iso3c,continent), by = "iso3c") %>%
  mutate(continent = ifelse(continent %in% c("EU","EUR"), "EUR", continent)) %>%
  
  #Joining the continent avgs
  left_join(use_table_continent %>%select(-use), by = c("continent", "proc", "year"), relationship = "many-to-many") %>%
 
  #Joining the TCFs
  left_join(tcf_table_clean, by=c("proc","item"="input")) %>%
  
  #Joining the estimated supply by process
  left_join(supply_final_bf, by = c("proc","year","iso3c"))

#6c. Calculating shares in supply, contribution to supply and from that estimated use
last_missing_use <- last_missing_use %>%
  group_by(iso3c, year) %>%
  mutate(
    share_supply = (share / multiplier_output_kl_to_input_t) /
      sum(share / multiplier_output_kl_to_input_t, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(supply_contrib = share_supply*supply,
         use = supply_contrib*multiplier_output_kl_to_input_t,
         unit = "kt") %>%
  select(iso3c,proc,year,item,use,unit)


###########################################################################################
#7. Joining to use table
###########################################################################################

use_table_final <- bind_rows(use_table_intermediate, last_missing_use) %>%
  arrange(iso3c, year, proc) %>%
  mutate(item = case_when(item == "Sweeteners, Other" ~ "Molasses",
                          item == "Inedible animal or vegetable fats and oils" ~ "Used cooking oil",
                          item == "Oilcrops Oil, Other" ~ "Used cooking oil",
                          item == "Cereals, Other" ~ "Triticale",
                          TRUE ~ item),
         unit = "kt") %>%
  ungroup() %>%
  summarise(use = sum(use, na.rm = TRUE),
            unit = first(unit), 
            .by = c(item, iso3c, year, proc))




###########################################################################################
########### SAVING FINAL USE DATASET #########
###########################################################################################

setwd("/home/mmondolfo/fabio_bfp/")

saveRDS(use_table_final, "inputs_for_final_data/use_final_bf.rds")


rm(list = ls())
