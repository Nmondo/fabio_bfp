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

setwd("/home/mmondolfo/fabio_bfp/")
########### FABIO regions #########

regions <- read.csv("inst/regions_full.csv", fileEncoding = "latin1") %>% filter(current == TRUE) 
EU27_countries <- subset(regions, EU27==TRUE)
EU27_countries <- EU27_countries$iso3c
years <- as.character(2012:2022)

########### Pre-cleaned supply & use data #########

setwd("/home/mmondolfo/fabio_bfp/intermediate_data/")

supply_feedstock <- readRDS("supply_feedstock.rds")
supply_fame_hvo <- readRDS("sup_fame_hvo_initial.rds")
use_long <- readRDS("use_long.rds")
oecd_fao_use <- readRDS("oecd_fao_use_clean.rds")


########### Technical conversion factors #########

tcf_table <- readRDS("tcf_table_clean.rds")

########### FAO non-food use #########

biodiesel_nonfood <- readRDS("fao_nonfood.rds") %>% filter(use=="Biodiesel & Renewable diesel")



###########################################################
########### FORMATTING SUPPLY TABLE FOR LATER JOIN #########
###########################################################

supply_fame_hvo %<>%
  mutate(year = as.integer(year))




###########################################################
########### CONVERSION FROM SUPPLY-FROM-FEEDSTOCK TO FEEDSTOCK USE #########
###########################################################


supply_feedstock_biodiesel <- subset(supply_feedstock, output %in% c("Biodiesel","Renewable diesel","Biodiesel & Renewable diesel"))

supply_feedstock_biodiesel %<>% rename(iso3c = country_iso3)


###########################################################
#1. Interpolation and extrapolation of missing years #########
###########################################################

## Gaps to estimate : 
# ESP ; Renewable diesel ; interpolation for 2012-2013 from 2011 and 2014 ; and for 2021 from 2020 and 2022.
# DEU ; Biodiesel ; extrapolation 2010-2013 from 2014 shares
# AUT ; Biodiesel ; extrapolation 2010-2012 from 2013 shares
# CZE : Biodiesel ; extrapolation 2019-2022 from 2018 shares
# FRA : Biodiesel ; extrapolation 2010-2018 from 2019-2022 shares. 


supply_feedstock_biodiesel %<>%
  pivot_longer(cols = all_of(years),    
               names_to = "year",
               values_to = "value") %>%
  left_join(biodiesel_nonfood %>% 
              select(year, iso3c, item, other) %>%
              rename(input = item) %>%
              mutate(year = as.character(year)),
            by = c("year", "iso3c", "input")
  ) %>%
  group_by(input, iso3c,output) %>%
  arrange(iso3c, year, input) %>%
  mutate(
    year = as.integer(year),
    is_extrapolated = case_when(
      iso3c == "ESP" ~ FALSE,
      iso3c %in% c("DEU", "AUT","FRA") & is.na(value) ~ TRUE,
      iso3c == "CZE" & is.na(value) ~ TRUE,
      TRUE ~ FALSE
    ),
    value = if_else(
      iso3c == "ESP",
      na.approx(value, x = year, na.rm = FALSE),
      value
    ),
    value = case_when(
      iso3c %in% c("DEU", "AUT", "FRA") & is.na(value) ~ first(na.omit(value)),
      iso3c == "CZE" & is.na(value) ~ last(na.omit(value)),
      TRUE ~ value
    )
  ) %>%
  ungroup() %>%
  left_join(supply_fame_hvo %>% 
              select(iso3c, year, output, value) %>%
              rename(total_supply = value),
            by = c("iso3c", "year", "output")) %>%
  group_by(iso3c, year, output) %>%
  mutate(
    palm_share_extrap = if_else(
      any(input == "Palm Oil" & is_extrapolated),
      other[input == "Palm Oil" & is_extrapolated][1],
      0
    ),
    sum_observed_values = sum(value[!is_extrapolated], na.rm = TRUE)
  ) %>%
  mutate(
    share_in_supply = case_when(
      total_supply == 0 ~ 0,
      is_extrapolated & input == "Palm Oil" ~ 
        other,
      is_extrapolated & input != "Palm Oil" ~ 
        (value / sum(value[input != "Palm Oil" & is_extrapolated], na.rm = TRUE)) * 
        ((total_supply - sum_observed_values - palm_share_extrap * total_supply) / total_supply),
      !is_extrapolated ~ 
        value / total_supply,
      TRUE ~ 0
    ),
    value = share_in_supply * total_supply
  ) %>%
  select(-share_in_supply, -is_extrapolated, -palm_share_extrap, -sum_observed_values, -other) %>%
  ungroup()


# Completing the conversion factors
supply_feedstock_biodiesel %<>% mutate(multiplier_output_kl_to_input_t = case_when(output == "Biodiesel" & input == "Other, Waste" ~ 1,
                                                                                   output == "Renewable diesel" & input == "Oilcrops Oil, Other" ~ 1.053,
                                                                                   TRUE ~ multiplier_output_kl_to_input_t)
                                       )


###########################################################
#2. Conversion to feedstock use #########
###########################################################

supply_to_use_biodiesel <- supply_feedstock_biodiesel %>% 
  mutate(value = value*multiplier_output_kl_to_input_t,
         unit = "kt",
         data_type = "Use") %>%
  select(-c(conversion_to_Ml,multiplier_output_kl_to_input_t,output_subtype)) %>%
  mutate(`Source(s)`="Estimate")


###########################################################
#3. Join to use dataset #########
###########################################################

use_biodiesel <- subset(use_long, process %in% c("FAME","HVO"))

use_biodiesel %<>% 
  rename(iso3c = country_iso3) %>%
  bind_rows(supply_to_use_biodiesel) %>%
  arrange(iso3c,process,input,year) %>%
  select(-data_type,-always_keep)



###########################################################################################
########### CONSTRUCTION OF A FULL USE TABLE FOR BIODIESEL & RENEWABLE DIESEL #########
###########################################################################################

###########################################################
#1. Formatting the table #########
###########################################################

intermediate_table_biodiesel <- use_biodiesel %>%
  distinct(iso3c,process,input,input_subtype) %>%
  crossing(year = 2010:2022) %>% 
  mutate(
    unit = "kt",
    value = NA_real_
  ) %>%
  left_join(regions,by="iso3c") 

existing_keys <- use_biodiesel %>%
  distinct(process, iso3c, input, input_subtype, year)

to_add <- intermediate_table_biodiesel %>%
  anti_join(
    existing_keys,
    by = c("process", "iso3c", "input", "input_subtype", "year")
  )

use_biodiesel_complete <-  use_biodiesel %>%
  bind_rows(to_add) %>%
  arrange(process, iso3c, input, input_subtype, year)


###########################################################
#2. Joining output by product-year-country #########
###########################################################

use_biodiesel_complete %<>% 
  rows_update(supply_fame_hvo %>% 
              select(process,year,iso3c,value,output) %>% 
              rename(total_supply = value),
            by = c("iso3c","process","year"),
            unmatched = "ignore") 

use_biodiesel_complete %<>%
  group_by(process, iso3c, input, year) %>%
  summarise(
    input_subtype = if (n() == 1) first(input_subtype) else NA_character_,
    value         = if (iso3c[1] == "BRA" && input[1] == "Fats, Animals, Raw") {
      sum(value, na.rm = TRUE)
    } else {
      sum(value, na.rm = FALSE)
    },
    unit          = "kt",
    output = first(output),
    total_supply = first(total_supply),
    .groups = "drop"
  ) %>% 
  mutate(input_subtype = case_when(input %in% c("Other, Waste","Fats, Animals, Raw") ~ NA_character_,
                                   TRUE ~input_subtype))


###########################################################
#3. Joining conversion factors #########
###########################################################

use_biodiesel_complete %<>% left_join(tcf_table %>% 
                                          select(input,input_subtype,output,output_qty,output_unit,multiplier_output_kl_to_input_t), 
                                        by = c("input", "input_subtype", "output")
                                      )



###########################################################################################
########### ALLOCATING FEEDSTOCK USE ACROSS RENEWABLE DIESEL PRODUCERS #########
###########################################################################################

setdiff(unique(subset(supply_fame_hvo, process == "HVO"  & value > 0)$iso3c),
        unique(subset(use_biodiesel_complete, process == "HVO")$iso3c))

# HVO producers with feedstock to estimate : FIN, NLD, SGP, SWE. 
# SGP : Palm Oil, PFAD, UCO. 
# SWE : Other, Waste (tall oil)
# FIN :  ; Other, Waste (tall oil) in Lappeenranta plant


###########################################################################################
#1. Neste plants: Singapore, The Netherlands and Finland #########
###########################################################################################

setwd("/home/mmondolfo/fabio_bfp/")

Neste_specific_data <- read_excel("own_data/RD_specific_data.xlsx")
Neste_total_use <- Neste_specific_data[1:6,2:16]
Neste_total_use %<>% mutate(across(`2010`:`2022`, ~ as.numeric(.x)))
Neste_country_assumptions <- Neste_specific_data[10:20,1:16]


### Step-by-step allocation

### SGP
SGP_output <- unlist(as.numeric(subset(supply_fame_hvo, process == "HVO" & iso3c == "SGP")$value))

SGP_palm_oil_shares <- unlist(as.numeric(Neste_country_assumptions[1, 4:16]))
SGP_palm_oil_use <- SGP_palm_oil_shares*(SGP_output/1.1538)
SGP_palm_oil_supply <- SGP_palm_oil_shares*SGP_output

## The following distribution of waste feedstocks use is unrealistic for SGP because POME & PFAD were most likely overwhelmingly used until recently.
## However, this will not impact estimated footprints because we assume an impact = 0 for waste products. 
SGP_residues_supply <- 0.4*SGP_palm_oil_supply
SGP_residues_supply[1] <- 0
SGP_residues_use <- (SGP_residues_supply/1.053)

SGP_animal_fats_supply <- SGP_output - SGP_palm_oil_supply - SGP_residues_supply
SGP_animal_fats_supply[4:6] <- (3/4)*(SGP_output - SGP_palm_oil_supply - SGP_residues_supply)[4:6]
SGP_animal_fats_supply[7:9] <- (1/2)*(SGP_output - SGP_palm_oil_supply - SGP_residues_supply)[7:9]
SGP_animal_fats_supply[10:13] <- (3/7)*(SGP_output - SGP_palm_oil_supply - SGP_residues_supply)[10:13]
SGP_animal_fats_use <- (SGP_animal_fats_supply/1.043)

SGP_uco_supply <- SGP_output - SGP_palm_oil_supply - SGP_residues_supply
SGP_uco_supply[1:3] <- 0
SGP_uco_supply[4:6] <- (1/4)*(SGP_output - SGP_palm_oil_supply - SGP_residues_supply)[4:6]
SGP_uco_supply[7:9] <- (1/2)*(SGP_output - SGP_palm_oil_supply - SGP_residues_supply)[7:9]
SGP_uco_supply[10:13] <- (4/7)*(SGP_output - SGP_palm_oil_supply - SGP_residues_supply)[10:13]
SGP_uco_use <- (SGP_uco_supply/1.053)

### NLD 
NLD_output <- unlist(as.numeric(subset(supply_fame_hvo, process == "HVO" & iso3c == "NLD")$value))
NLD_palm_oil_use <- as.numeric(unlist(Neste_total_use[1, 3:15]))
NLD_palm_oil_use <- NLD_palm_oil_use - SGP_palm_oil_use
NLD_palm_oil_use[1] <- 0
NLD_palm_oil_supply <- 1.1538*NLD_palm_oil_use

NLD_rapeseed_use <- unlist(as.numeric(Neste_country_assumptions[10, 4:16]))
NLD_rapeseed_supply <- NLD_rapeseed_use*1.16662

NLD_animal_fats_supply <- NLD_output - NLD_palm_oil_supply - NLD_rapeseed_supply
NLD_animal_fats_supply[2:6] <- (1.3/2.3)*(NLD_output - NLD_palm_oil_supply - NLD_rapeseed_supply)[2:6]
NLD_animal_fats_supply[7:13] <- (1.8/2.8)*(NLD_output - NLD_palm_oil_supply - NLD_rapeseed_supply)[7:13]
NLD_animal_fats_use <- (NLD_animal_fats_supply/1.043)

NLD_uco_supply <- NLD_output - NLD_palm_oil_supply - NLD_rapeseed_supply
NLD_uco_supply[2:6] <- (1/2.3)*(NLD_output - NLD_palm_oil_supply - NLD_rapeseed_supply)[2:6]
NLD_uco_supply[7:13] <- (1/2.8)*(NLD_output - NLD_palm_oil_supply - NLD_rapeseed_supply)[7:13]
NLD_uco_use <- (NLD_uco_supply/1.053)

### FIN
FIN_output <- unlist(as.numeric(subset(supply_fame_hvo, process == "HVO" & iso3c == "FIN")$value))

FIN_rapeseed_use <- c(0,unlist(as.numeric(Neste_country_assumptions[6, 5:16])))
FIN_rapeseed_supply <- FIN_rapeseed_use*1.16662
FIN_rapeseed_supply[1] <- (FIN_rapeseed_supply/FIN_output)[2]
FIN_rapeseed_supply[1] <- (FIN_rapeseed_supply/FIN_output)[2] * FIN_output[1]
FIN_rapeseed_use[1] <- FIN_rapeseed_supply[1]/1.16662

FIN_palm_oil_supply <- rep(0,13)
FIN_palm_oil_supply[1] <- (FIN_output - FIN_rapeseed_supply)[1]
FIN_palm_oil_use <- FIN_palm_oil_supply/1.1538

FIN_tall_oil_supply <- c(rep(0,5),rep(120,8))
FIN_tall_oil_use <- FIN_tall_oil_supply/1.053

FIN_animal_fats_supply <- FIN_output - FIN_rapeseed_supply - FIN_palm_oil_supply - FIN_tall_oil_supply
FIN_animal_fats_use <- FIN_animal_fats_supply/1.043


###########################################################################################
#2. Sweden, based on Preem's Sustainability reports and extrapolation #########
###########################################################################################

SWE_output <- unlist(as.numeric(subset(supply_fame_hvo, process == "HVO" & iso3c == "SWE")$value))

# Best we can do is to assume avg shares from Preem's 2023 and 2024 Sustainability reports and extrapolate them to all other years. 
print(subset(biodiesel_nonfood, iso3c=="SWE")) #We assume that the vegetable oil being used is the one that's the biggest non-food use, i.e. rape and mustard oil (consistent across all years)
SWE_s_animal_fats <- (0.55+0.66)/2
SWE_s_rapeseed_oil <- (0.07+0.1)/2
SWE_s_tall_oil <- (0.13+0.18)/2
SWE_s_uco <- (0.06+0.17)/2
SWE_s_other_waste <- (0.05+0.04)/2

SWE_supply_animal_fats <- SWE_s_animal_fats*SWE_output
SWE_supply_rapeseed_oil <- SWE_s_rapeseed_oil*SWE_output
SWE_supply_tall_oil <- SWE_s_tall_oil*SWE_output
SWE_supply_uco <- SWE_s_uco*SWE_output
SWE_supply_other_waste <- SWE_s_other_waste*SWE_output

SWE_use_animal_fats <- SWE_supply_animal_fats/1.043
SWE_use_rapeseed_oil <- SWE_supply_rapeseed_oil/1.16662
SWE_use_tall_oil <- SWE_supply_tall_oil/1.053
SWE_use_uco <- SWE_supply_uco/1.053
SWE_use_other_waste <- SWE_supply_other_waste/1.053

rd_feedstocks <- expand_grid(
  iso3c = c("FIN","SGP","NLD","SWE"),
  input = c("Other, Waste","Rape and Mustard Oil","Palm Oil","Fats, Animals, Raw","Inedible animal or vegetable fats and oils"),
  year = 2010:2022,
  value = 0,
  unit = "kt"
)

rd_feedstocks %<>% mutate(input_subtype = case_when(input=="Inedible animal or vegetable fats and oils" ~ "Used cooking oil",
                                                          input=="Other, Waste" & iso3c %in% c("FIN","SWE") ~ "Tall oil",
                                                          input=="Other, Waste" & iso3c == "SGP" ~ "Palm oil by-products",
                                                          TRUE ~ NA)
                                )

SWE_rows_other <- rd_feedstocks %>%
  filter(iso3c == "SWE", input == "Other, Waste") %>%
  mutate(input_subtype = NA_character_)

rd_feedstocks %<>%
  bind_rows(SWE_rows_other)


###########################################################################################
#3. Compiling into a single table #########
###########################################################################################

rd_feedstocks %<>%
  select(iso3c, input, input_subtype, year, value, unit) %>%
  pivot_wider(
    names_from  = year,
    values_from = value,
    names_prefix = "y"
  )

cols_years <- which(names(rd_feedstocks) %in% paste0("y", 2010:2022))

rd_feedstocks[rd_feedstocks$iso3c=="SGP" & rd_feedstocks$input=="Palm Oil",cols_years] <- t(SGP_palm_oil_use)
rd_feedstocks[rd_feedstocks$iso3c=="SGP" & rd_feedstocks$input=="Other, Waste" ,cols_years] <- t(SGP_residues_use)
rd_feedstocks[rd_feedstocks$iso3c=="SGP" & rd_feedstocks$input=="Fats, Animals, Raw" ,cols_years] <- t(SGP_animal_fats_use)
rd_feedstocks[rd_feedstocks$iso3c=="SGP" & rd_feedstocks$input=="Inedible animal or vegetable fats and oils" ,cols_years] <- t(SGP_uco_use)

rd_feedstocks[rd_feedstocks$iso3c=="NLD" & rd_feedstocks$input=="Palm Oil" ,cols_years] <- t(NLD_palm_oil_use)
rd_feedstocks[rd_feedstocks$iso3c=="NLD" & rd_feedstocks$input=="Fats, Animals, Raw" ,cols_years] <- t(NLD_animal_fats_use)
rd_feedstocks[rd_feedstocks$iso3c=="NLD" & rd_feedstocks$input=="Inedible animal or vegetable fats and oils",cols_years] <- t(NLD_uco_use)
rd_feedstocks[rd_feedstocks$iso3c=="NLD" & rd_feedstocks$input=="Rape and Mustard Oil" ,cols_years] <- t(NLD_rapeseed_use)

rd_feedstocks[rd_feedstocks$iso3c=="FIN" & rd_feedstocks$input=="Palm Oil" ,cols_years] <- t(FIN_palm_oil_use)
rd_feedstocks[rd_feedstocks$iso3c=="FIN" & rd_feedstocks$input=="Other, Waste" ,cols_years] <- t(FIN_tall_oil_use)
rd_feedstocks[rd_feedstocks$iso3c=="FIN" & rd_feedstocks$input=="Fats, Animals, Raw" ,cols_years] <- t(FIN_animal_fats_use)
rd_feedstocks[rd_feedstocks$iso3c=="FIN" & rd_feedstocks$input=="Rape and Mustard Oil" ,cols_years] <- t(FIN_rapeseed_use)

rd_feedstocks[rd_feedstocks$iso3c=="SWE" & rd_feedstocks$input=="Inedible animal or vegetable fats and oils" ,cols_years] <- t(SWE_use_uco)
rd_feedstocks[rd_feedstocks$iso3c=="SWE" & rd_feedstocks$input=="Fats, Animals, Raw" ,cols_years] <- t(SWE_use_animal_fats)
rd_feedstocks[rd_feedstocks$iso3c=="SWE" & rd_feedstocks$input=="Rape and Mustard Oil" ,cols_years] <- t(SWE_use_rapeseed_oil)
rd_feedstocks[rd_feedstocks$iso3c=="SWE" & rd_feedstocks$input=="Other, Waste" & is.na(rd_feedstocks$input_subtype) ,cols_years] <- t(SWE_use_other_waste)
rd_feedstocks[rd_feedstocks$iso3c=="SWE" & rd_feedstocks$input=="Other, Waste" & 
                      rd_feedstocks$input_subtype=="Tall oil" &
                      !is.na(rd_feedstocks$input_subtype),cols_years] <- t(SWE_use_tall_oil)

rd_feedstocks %<>%
  pivot_longer(
    cols = starts_with("y20"),
    names_to = "year",
    values_to = "value",
    names_prefix = "y"
  ) %>%
  mutate(year = as.integer(year),
         process = "HVO",
         output = "Renewable diesel") %>%
  select(iso3c, input, input_subtype, year, value, unit, process, output) %>% 
  group_by(iso3c,input) %>%
  filter(!all(value==0))


###########################################################################################
#4. Joining to the use table #########
###########################################################################################

use_biodiesel_complete %<>% bind_rows(rd_feedstocks)

rm(list = ls(pattern = "^(SGP|NLD|FIN|SWE)"), rd_feedstocks)




###########################################################
########### ALLOCATING FEEDSTOCK USE VALUES FROM OECD-FAO Agricultural outlook #########
###########################################################

## Identifying the countries which are in the OECD-FAO dataset and not in the existing use table.

countries_to_add_biodiesel <- setdiff(unique(oecd_fao_use$iso3c),unique(use_biodiesel_complete$iso3c))

## Computing the shares in supply and harmonizing the products, including qualitative information. 

oecd_fao_use_biodiesel <- oecd_fao_use %>%
  filter(process=="Biodiesel & Renewable diesel") %>%
  group_by(iso3c) %>%
  filter(iso3c %in% countries_to_add_biodiesel) %>%
  ungroup() %>%
  group_by(iso3c,input) %>%
  filter(!all(value<=1))


###########################################################
#1. Paraguay from Soyabean Oil #########
###########################################################

PRY_biodiesel_feedstocks <- subset(oecd_fao_use_biodiesel, iso3c == "PRY") 
PRY_biodiesel_feedstocks %<>% 
  select(-total_supply) %>%
  filter(year <= 2022) %>%
  mutate(input = "Soyabean Oil",
         input_subtype = NA,
         process = "FAME",
         output = "Biodiesel",
         unit = "kt") %>%
  left_join(supply_fame_hvo %>% select(output,iso3c,year,value) %>% rename(total_supply=value), by = c("output","iso3c","year")) %>%
  left_join(tcf_table %>% select(output,input,input_subtype,multiplier_output_kl_to_input_t,output_qty) %>% filter(output == "Biodiesel"),
            by = c("input","input_subtype","output")) %>%
  mutate(value = value * multiplier_output_kl_to_input_t)


###########################################################
#2. United Kingdom from UCO and animal fats #########
###########################################################

GBR_biodiesel_feedstocks <- subset(biodiesel_nonfood,
                                   iso3c=="GBR" &
                                   item %in% c("Oilcrops Oil, Other", "Fats, Animals, Raw"))

GBR_biodiesel_feedstocks %<>% group_by(iso3c,year) %>%
  mutate(share_in_supply = other/sum(other),
         input = ifelse(item == "Oilcrops Oil, Other","Inedible animal or vegetable fats and oils" , item),
         input_subtype = ifelse(input == "Inedible animal or vegetable fats and oils", "Used cooking oil", NA),
         output = "Biodiesel",
         process = "FAME",
         unit = "kt") %>%
  ungroup() %>%
  left_join(supply_fame_hvo %>% select(output,iso3c,year,value) %>% rename(total_supply=value), by = c("output","iso3c","year")) %>%
  left_join(tcf_table %>% select(output,input,input_subtype,multiplier_output_kl_to_input_t,output_qty) %>% filter(output == "Biodiesel"),
            by = c("input","input_subtype","output"))

GBR_biodiesel_feedstocks %<>% mutate(value = share_in_supply * total_supply * multiplier_output_kl_to_input_t)


###########################################################
#3. Joining to use table #########
###########################################################

use_biodiesel_complete %<>% bind_rows(PRY_biodiesel_feedstocks,GBR_biodiesel_feedstocks) %>%
  select(-(data_type:share_in_supply))




###########################################################
########### ALLOCATING FEEDSTOCK USE FOR BIODIESEL ACROSS EUROPEAN COUNTRIES #########
###########################################################

###########################################################
#1. Listing countries missing feedstock use estimates  #########
###########################################################

eu_use <- use_biodiesel_complete %>% filter(iso3c %in% EU27_countries & process == "FAME")
estimated <- unique(eu_use$iso3c)
to_estimate <- setdiff(EU27_countries,estimated)


###########################################################
#2. Listing assumed potential feedstocks by country (based on qualitative info)  #########
###########################################################


nonfood_use_eu <- subset(biodiesel_nonfood, 
                           (item == "Rape and Mustard Oil" & iso3c %in% c("BEL","BGR","GRC","HRV","HUN","NLD","SVK") ) |
                           (item == "Palm Oil" & iso3c %in% c("BEL","ESP","HUN","ITA")) |
                           (item == "Oilcrops Oil, Other" & iso3c %in% c("BEL","GRC","HUN","IRL","NLD","SVK")) |
                           (item == "Fats, Animals, Raw" & iso3c %in% c("BEL","ESP","GRC","HUN","IRL","SVK")) |
                           (item == "Soyabean Oil" & iso3c %in% c("BEL","ESP")) |
                           (item == "Sunflowerseed Oil" & iso3c %in% c("BGR","GRC","HUN","SVK"))
                         )


#SWE,LTU,LVA,ROU : 100% Rape and Mustard Oil
nonfood_use_eu_additions <- expand.grid(
  iso3c = c("SWE", "LTU", "LVA", "ROU"),
  year = 2010:2022,
  output = "Biodiesel",
  item = "Rape and Mustard Oil"
) %>%
  mutate(share_in_use = 1)


###########################################################
#3. Estimating feedstock use from relative shares in non-food use (FAOSTAT)  #########
###########################################################

nonfood_use_eu %<>% 
  group_by(iso3c,year,use) %>%
  mutate(share_in_use = other/sum(other),
            item = item,
            output = "Biodiesel") %>%
  ungroup() %>%
  select(iso3c,year,output,item,share_in_use)

nonfood_use_eu <- rbind(nonfood_use_eu, nonfood_use_eu_additions)


###########################################################
#4. Joining supply data and TCFs, converting from share in use to share in supply  #########
###########################################################

nonfood_use_eu %<>% 
  rename(input = item) %>%
  mutate(input = if_else(input == "Oilcrops Oil, Other", "Inedible animal or vegetable fats and oils", input),
         input_subtype = if_else(input == "Inedible animal or vegetable fats and oils", "Used cooking oil", NA_character_)) %>%
  left_join(supply_fame_hvo %>% 
                                select(iso3c,year,output,value) %>%
                                rename(total_supply = value), 
                              by = c("iso3c","year","output")) %>%
  left_join(tcf_table %>%
              select(input,input_subtype,output,multiplier_output_kl_to_input_t,output_qty),
            by = c("input","input_subtype","output"))


# From share in use to share in supply

nonfood_use_eu %<>% mutate(weighted_share = share_in_use / multiplier_output_kl_to_input_t) %>%
  group_by(iso3c,year,output) %>% mutate(share_in_estimated_supply = weighted_share/sum(weighted_share)) %>%
  ungroup() %>%
  select(-c(share_in_use,weighted_share))


###########################################################
#5. Final estimate of feedstock use  #########
###########################################################

nonfood_use_eu %<>%
  mutate(value = share_in_estimated_supply * total_supply * multiplier_output_kl_to_input_t,
         process = "FAME") %>%
  select(-multiplier_output_kl_to_input_t)


###########################################################
#6. Joining to use table  #########
###########################################################

use_biodiesel_complete %<>%
  bind_rows(nonfood_use_eu)




###########################################################
########### ALLOCATING FEEDSTOCK USE FOR REMAINING COUNTRIES #########
###########################################################

###########################################################
#1. Listing countries missing feedstock use estimates  #########
###########################################################

biodiesel_producers <- supply_fame_hvo %>% 
  group_by(process,iso3c) %>% 
  filter(any(value>0)) %>%
  ungroup() %>%
  select(iso3c)
to_estimate <- setdiff(unique(unlist(as.vector(biodiesel_producers))),unique(use_biodiesel_complete$iso3c))


###########################################################
#2. Listing assumed potential feedstocks by country (based on qualitative info)  #########
###########################################################

share_in_use <- biodiesel_nonfood %>% 
  filter(item == "Oilcrops Oil, Other" & iso3c %in% c("CHE") |
         item == "Fats, Animals, Raw" & iso3c %in% c("CHE") |
         iso3c %in% c("TUR","TWN","URY") & item != "Cottonseed Oil"
         ) %>%
  rename(input = item)

nonfood_inputs_world <- tribble(
  ~iso3c, ~input,
  "BLR", "Rape and Mustard Oil",
  "HKG", "Inedible animal or vegetable fats and oils"
  )

nonfood_inputs_world %<>%
  expand_grid(year = 2010:2022) %>%
  mutate(other = 1,
         use = "Biodiesel")


###########################################################
#3. Estimating feedstock use from relative shares in non-food use (FAOSTAT)  #########
###########################################################

## Joining TCFs and estimating share in supply 

share_in_use %<>% 
  bind_rows(nonfood_inputs_world) %>%
  mutate(output = "Biodiesel",
         input = ifelse(input == "Oilcrops Oil, Other", "Inedible animal or vegetable fats and oils", input),
         input_subtype = case_when(
           input == "Inedible animal or vegetable fats and oils" ~ "Used cooking oil",
           TRUE ~ NA_character_)
         ) %>%
  select(-use) %>%
  left_join(tcf_table %>% select(output,input,input_subtype,output_qty,multiplier_output_kl_to_input_t),by=c("output","input","input_subtype")) %>%
  mutate(weighted_share = other*output_qty) %>%
  group_by(iso3c,year,output) %>%
  mutate(share_in_estimated_supply = weighted_share/sum(weighted_share)) %>%
  ungroup() %>%
  select(-weighted_share)

## Joining total supply and estimating use per feedstock type. 

share_in_use %<>% 
  left_join(supply_fame_hvo %>% select(output,year,iso3c,value) %>% rename(total_supply = value),
            by = c("iso3c","year","output")) %>%
  mutate(value = share_in_estimated_supply * total_supply * multiplier_output_kl_to_input_t,
         process = "FAME")


###########################################################
#4. Joining to use table  #########
###########################################################

use_biodiesel_complete %<>% bind_rows(share_in_use)




###########################################################################################
########### RESCALING USE FROM USDA BIOFUELS ANNUAL #########
###########################################################################################

## Compute the estimated supply, the estimated share of each feedstock in supply, the gap between estimated supply from feedstcks and total supply quantity

use_biodiesel_noNA <- use_biodiesel_complete %>% 
  group_by(process, iso3c, year) %>%
  filter(all(!is.na(value))) %>%
  ungroup()

## Estimating current output levels with feedstock use data, for comparison with total output.
use_biodiesel_est <- use_biodiesel_noNA %>% 
  group_by(process, iso3c, year) %>% 
  summarise(
    estimated_supply = sum(value * output_qty, na.rm = TRUE),
    .groups = "drop"
  )

use_biodiesel_est %<>%
  left_join(
    use_biodiesel_complete %>% 
      select(process, iso3c, year, total_supply) %>% 
      distinct(),
    by = c("process", "iso3c", "year")
  ) %>%
  mutate(left_to_estimate = total_supply - estimated_supply)

use_biodiesel_with_share <- use_biodiesel_noNA %>% select(process,iso3c,input,input_subtype,year,value,output_qty,output) %>%
  group_by(process, output, iso3c, year) %>%
  mutate(
    share_in_estimated_supply = value * output_qty / sum(value * output_qty, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  left_join(use_biodiesel_est %>% select(process,iso3c,year,left_to_estimate) %>%  filter(!(iso3c %in% c("SWE","NLD") & is.na(left_to_estimate))), 
            by = c("process","iso3c","year")) %>%
  select(-c(value,output_qty))

use_biodiesel_complete %<>% left_join(use_biodiesel_with_share %>%
                                        select(process,input,input_subtype,iso3c,year,share_in_estimated_supply,left_to_estimate), 
                                      by = c("process","input","input_subtype","iso3c","year"))


## Adjust feedstock use estimates to keep input-output ratios consistent. 

use_biodiesel_complete %<>% 
  mutate(share_in_estimated_supply = coalesce(share_in_estimated_supply.x,share_in_estimated_supply.y),
         value = ifelse(is.na(share_in_estimated_supply)==FALSE,
                        value + share_in_estimated_supply * left_to_estimate * (1/output_qty),
                        value)) %>%
  select(-share_in_estimated_supply.x,-share_in_estimated_supply.y) %>% 
  mutate(share_in_estimated_supply = ifelse(share_in_estimated_supply=="NaN",NA,share_in_estimated_supply))




###########################################################################################
########### GAP FILLING (MISSING YEARS ONLY) #########
###########################################################################################

###########################################################################################
#1. Interpolation / Extrapolation of shares of contribution to total supply, by feedstock #########
########################################################################################### 

use_biodiesel_complete %<>%
  arrange(process, iso3c, input, input_subtype, year)


use_biodiesel_complete %<>%
  group_by(process, iso3c, input, input_subtype) %>%
  mutate(
    share_interp = {
      s <- share_in_estimated_supply
      y <- as.numeric(year)
      
      if (all(is.na(s))) {
        rep(NA_real_, length(s))
      } else {
        idx_valid <- !is.na(s)
        if (sum(idx_valid) < 2) {
          s
        } else {
          f <- approxfun(y[idx_valid], s[idx_valid], rule = 2, method = "linear")
          f(y)
        }
      }
    }
  ) %>%
  ungroup() %>%
  # Remplir les NA restants en avant avec la première valeur valide
  group_by(process, iso3c, input, input_subtype) %>%
  mutate(
    share_interp = {
      idx_na <- is.na(share_interp)
      if (any(idx_na)) {
        first_valid <- which(!is.na(share_interp))[1]
        if (!is.na(first_valid)) {
          share_interp[1:first_valid][is.na(share_interp[1:first_valid])] <- share_interp[first_valid]
        }
      }
      share_interp
    }
  ) %>%
  ungroup()


###########################################################################################
#2. Making specific cases for calibrating shares, dependent on whether we have use data for all inputs (case 0), some inputs (case 1) or none (case 2) #########
########################################################################################### 

compute_shares_one_year <- function(df_year) {
  n <- nrow(df_year)
  n_na_value <- sum(is.na(df_year$value))
  
  case <- dplyr::case_when(
    n_na_value == 0 ~ "case0",                 # no NA
    n_na_value == n ~ "case2",                 # all NA
    TRUE ~ "case1"                             # between 1 and n-1 NAs. 
  )
  
  df_year$case <- case
  
  share_filled <- df_year$share_in_estimated_supply
  
  if (case[1] == "case0") { # keep existing shares
    share_filled <- df_year$share_in_estimated_supply 
    
  } else if (case[1] == "case2") { # normalize shares from interpolation/extrapolation
    s <- df_year$share_interp
    
    if (all(is.na(s))) {
      share_filled <- rep(NA_real_, n)
    } else {
      s[is.na(s)] <- 0
      sum_s <- sum(s)
      if (sum_s > 0) {
        share_filled <- s / sum_s
      } else {
        share_filled <- rep(NA_real_, n)
      }
    }
    
  } else {
    idx_na <- is.na(df_year$value)
    
    # extrapolation of the shares of rows with missing feedstock use
    s_na <- df_year$share_interp[idx_na]
    s_na[is.na(s_na)] <- 0
    S_na <- sum(s_na)
    
    # extrapolation of the shares of rows with available feedstock use, for the remaining part
    R <- 1 - S_na
    if (R < 0) R <- 0
    
    contrib <- df_year$value * df_year$output_qty
    contrib[is.na(contrib)] <- 0
    sum_contrib <- sum(contrib[!idx_na])
    
    share_filled <- numeric(n)
    share_filled[idx_na] <- s_na
    
    if (sum_contrib > 0 && R > 0) {
      share_filled[!idx_na] <- R * contrib[!idx_na] / sum_contrib
    } else {
      k <- sum(!idx_na)
      share_filled[!idx_na] <- if (k > 0) R / k else 0
    }
  }
  
  df_year$share_filled <- share_filled
  df_year
}

use_biodiesel_complete %<>%
  group_by(process, iso3c, year) %>%
  group_modify(~ compute_shares_one_year(.x)) %>%
  ungroup()

use_biodiesel_complete %<>%
  mutate(
    share_in_estimated_supply_raw = share_in_estimated_supply,
    share_in_estimated_supply     = share_filled
  )


###########################################################################################
#3. Estimating feedstock use from extrapolated or interpolated shares #########
########################################################################################### 

use_biodiesel_complete %<>%
  mutate(
    value_raw = value, 
    value = dplyr::if_else(
      case == "case0" | is.na(share_in_estimated_supply) |
        is.na(total_supply) | is.na(output_qty),
      value,
      share_in_estimated_supply * total_supply * (1 / output_qty)
    ),
    unit = "kt"
  ) %>%
  relocate(value, .after = input_subtype) %>%
  select(-value_raw)




#########################################################################
################## FORMATTING USE TABLE ####################
#########################################################################

use_biodiesel_complete %<>% 
  select(process,iso3c,year,input,value,unit) %>%
  rename(proc = process,
         item = input,
         use = value
  ) %>%
  mutate(proc = case_when(proc == "FAME" ~ "Biodiesel production",
                          proc == "HVO" ~ "Renewable diesel production")) %>%
  arrange(iso3c,year,proc,item)




#########################################################################
################## FORMATTING SUPPLY TABLE ####################
#########################################################################

supply_fame_hvo %<>%
mutate(process = case_when(process == "FAME" ~ "Biodiesel production",
                             process == "HVO" ~ "Renewable diesel production")) %>%
  rows_update(regions, by ="iso3c", unmatched = "ignore")




#########################################################################
################## SAVING SUPPLY AND USE TABLE ####################
#########################################################################

setwd("/home/mmondolfo/fabio_bfp/intermediate_data/")

saveRDS(object = supply_fame_hvo,
        file = "sup_fame_hvo_full.rds")

saveRDS(object = use_biodiesel_complete,
        file = "use_fame_hvo_full.rds")

rm(list = ls())