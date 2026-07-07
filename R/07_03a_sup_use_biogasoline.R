
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

## --- portable repo root: FABIO_BFP_ROOT override, else walk up to the repo marker ---
fabio_root <- Sys.getenv("FABIO_BFP_ROOT", unset = "")
if (!nzchar(fabio_root)) {
  fabio_root <- getwd()
  while (!file.exists(file.path(fabio_root, "R", "00_system_variables.R")) &&
         dirname(fabio_root) != fabio_root) fabio_root <- dirname(fabio_root)
  if (!file.exists(file.path(fabio_root, "R", "00_system_variables.R")))
    stop("Repo root not found above ", getwd(), " - set FABIO_BFP_ROOT or run from inside the repo.")
}
setwd(fabio_root)
setwd(fabio_root)
########### FABIO regions #########

regions <- read.csv("inst/regions_full.csv", fileEncoding = "latin1") %>% filter(current == TRUE) 
EU27_countries <- subset(regions, EU27==TRUE)
EU27_countries <- EU27_countries$iso3c
years <- as.character(2012:2022)

########### Pre-cleaned supply & use data #########

setwd(file.path(fabio_root, "intermediate_data"))

supply_feedstock <- readRDS("supply_feedstock.rds")
supply_biogasoline <- readRDS("sup_biogasoline_initial.rds")
use_long <- readRDS("use_long.rds")
oecd_fao_use <- readRDS("oecd_fao_use_clean.rds")

########### Technical conversion factors #########

tcf_table <- readRDS("tcf_table_clean.rds")

########### FAO non-food use #########

bioethanol_nonfood <- readRDS("fao_nonfood.rds") %>% filter(use=="Bioethanol")




###########################################################
########### FORMATTING SUPPLY TABLE FOR LATER JOIN #########
###########################################################

supply_biogasoline %<>%
  mutate(year = as.integer(year)) %>%
  rename(total_supply = value)




###########################################################
########### CONVERSION FROM SUPPLY-FROM-FEEDSTOCK TO FEEDSTOCK USE #########
###########################################################

supply_feedstock_bioethanol <- subset(supply_feedstock, output%in% c("Bioethanol","Biogasoline"))

###########################################################
#1. Interpolation and extrapolation of missing years #########
###########################################################

## Linear interpolation for France, year 2018

supply_feedstock_bioethanol <- supply_feedstock_bioethanol %>% 
  mutate(
    `2018` = dplyr::if_else(
      country_iso3 == "FRA" & is.na(`2018`),
      (`2017` + `2019`) / 2,
      `2018`
    )
  ) %>%
  rename(iso3c=country_iso3)

## Extrapolation based on shares, calibrated on total supply

supply_feedstock_bioethanol <- supply_feedstock_bioethanol %>%
  pivot_longer(cols = all_of(years),    
               names_to = "year",
               values_to = "value") %>%
  mutate(year = as.integer(year),
         output = "Biogasoline",
         process = "Biogasoline") %>%
  arrange(iso3c, output, year) %>%
  group_by(iso3c, output) %>%
  mutate(first_year_no_na = if (all(is.na(value))) NA_integer_
         else min(year[!is.na(value)])) %>%
  ungroup() %>%
  
  group_by(iso3c, output, year) %>%
  mutate(
    denom = sum(value, na.rm = TRUE),
    share = ifelse(denom == 0 | is.na(value), NA_real_, value / denom)
  ) %>%
  select(-denom) %>%
  ungroup()

supply_feedstock_bioethanol %<>%
  left_join(
    supply_biogasoline %>% select(output,iso3c,year,total_supply),
    by = c("output", "iso3c", "year")
  ) %>%
  group_by(iso3c, output, input) %>%
  mutate(
    share_ref = share[year == first_year_no_na][1],      
    supply_per_feedstock = total_supply * share_ref      
  ) %>%
  ungroup()

supply_feedstock_bioethanol %<>%
  mutate(`Source(s)` = ifelse(is.na(value==TRUE), "Estimate", `Source(s)`),
         value = ifelse(is.na(value==TRUE),supply_per_feedstock,value)) %>%
  select(-(first_year_no_na:supply_per_feedstock))


###########################################################
#2. Conversion to feedstock use #########
###########################################################

supply_to_use_bioethanol <- supply_feedstock_bioethanol %>% 
  mutate(value = value*multiplier_output_kl_to_input_t,
         unit = "kt",
         data_type = "Use") %>%
  select(-c(conversion_to_Ml,multiplier_output_kl_to_input_t,output,output_subtype)) %>%
  mutate(`Source(s)`="Estimate")


###########################################################
#3. Join to use dataset #########
###########################################################

use_biogasoline <- subset(use_long, process %in% c("Bioethanol","Biogasoline"))

use_biogasoline %<>% mutate(process = "Biogasoline") %>%
  rename(iso3c = country_iso3) %>%
  bind_rows(supply_to_use_bioethanol) %>%
  arrange(iso3c,process,input,year) %>%
  select(-data_type)



###########################################################################################
########### CONSTRUCTION OF A FULL USE TABLE FOR BIODIESEL & RENEWABLE DIESEL #########
###########################################################################################

###########################################################
#1. Formatting the table #########
###########################################################

intermediate_table_biogasoline <- use_biogasoline %>%
  distinct(iso3c, input,input_subtype) %>%
  crossing(year = 2010:2022) %>% 
  mutate(
    process = "Biogasoline",
    unit = "kt",
    value = NA_real_
  ) %>%
  left_join(regions,by="iso3c") %>%
  mutate(EU28 = ifelse(EU27==TRUE|iso3c=="GBR",TRUE,FALSE))

existing_keys <- use_biogasoline %>%
  distinct(process, iso3c, input, input_subtype, year)

to_add <- intermediate_table_biogasoline %>%
  anti_join(
    existing_keys,
    by = c("process", "iso3c", "input", "input_subtype", "year")
  )

use_biogasoline_complete <- use_biogasoline %>%
  bind_rows(to_add) %>%
  arrange(process, iso3c, input, input_subtype, year)


###########################################################
#2. Joining output by product-year-country #########
###########################################################

eu_supply <- supply_biogasoline %>% 
  filter(year<=2014 & (EU27==TRUE|iso3c=="GBR") | year >= 2015 & EU27==TRUE) %>%
  group_by(year) %>%
  summarize(total_supply = sum(total_supply, na.rm=TRUE),
            iso3c = "EU27",
            process = first(process),
            supply_unit = first(unit)) %>%
  ungroup()


use_biogasoline_complete %<>% 
  left_join(supply_biogasoline %>% 
              select(process,year,iso3c,total_supply,unit) %>% 
              rename(supply_unit = unit),
            by = c("iso3c","process","year")) %>%
  left_join(eu_supply,
            by = c("iso3c", "process", "year")
  ) %>%
  mutate(iso3c = ifelse(iso3c=="EU27"&year<=2014,"EU28",iso3c),
         total_supply = coalesce(total_supply.x, total_supply.y),
         supply_unit = coalesce(supply_unit.x, supply_unit.y)) %>%
  select(-c(supply_unit.x, supply_unit.y, total_supply.x, total_supply.y))


###########################################################
#3. Joining conversion factors #########
###########################################################

use_biogasoline_complete %<>% left_join(tcf_table %>% 
                                          select(input,input_subtype,output,output_qty,output_unit) %>% 
                                          rename(process = output) %>% 
                                          mutate(process=ifelse(process=="Bioethanol","Biogasoline",process)), 
                                        by = c("input", "input_subtype", "process"))




###########################################################
########### ALLOCATING FEEDSTOCK USE VALUES FROM OECD-FAO Agricultural outlook #########
###########################################################

## Identifying the countries which are in the OECD-FAO dataset and not in the existing use table.

countries_to_add_biogasoline <- setdiff(unique(oecd_fao_use$iso3c),unique(use_biogasoline_complete$iso3c))

###########################################################
#1. Computing relative shares of feedstocks in supply #########
###########################################################

oecd_fao_use_biogasoline <- oecd_fao_use %>%
  filter(process=="Biogasoline") %>%
  group_by(iso3c) %>%
  filter(iso3c %in% countries_to_add_biogasoline &
           !all(total_supply<=5)) %>%
  ungroup() %>%
  group_by(iso3c,input) %>%
  filter(!all(value<=1))

oecd_fao_use_biogasoline %<>% 
  group_by(process,iso3c,year) %>%
  mutate(share_in_supply = value/sum(value),
            input = input,
            total_supply = first(total_supply),
            unit = first(unit)) %>%
  filter(year <= 2022) %>%
  ungroup()


###########################################################
#2. Listing assumed feedstocks and harmonizing names #########
###########################################################

oecd_fao_use_biogasoline %<>%
  filter(!(iso3c %in% c("GBR","TUR") & input == "Other, Unknown"),
         !(iso3c %in% c("PAK","IDN","EGY") & input != "Sweeteners, Other"),
         !(iso3c=="KAZ" & input != "Wheat and products"),
         !(iso3c %in% c("NGA","VNM") & input != "Cassava and products"),
         !(iso3c %in% c("UKR","ETH") & input == "Cassava and products"),
         !(iso3c %in% c("GBR","ZAF","UKR") & input == "Other coarse grains")) %>%
  mutate(input = case_when(iso3c=="ETH" & input == "Other, Unknown" ~ "Sweeteners, Other",
                           iso3c=="RUS" & input == "Other coarse grains" ~ "Barley and products",
                           iso3c=="TUR" & input == "Sweeteners, Other" ~ "Sugar beet",
                           TRUE ~ input),
         share_in_supply = case_when(iso3c=="ETH" & input == "Sweeteners, Other" ~ 1,
                                     TRUE ~ share_in_supply)
  ) %>%
  group_by(process,iso3c,year) %>%
  mutate(share_in_supply = share_in_supply/sum(share_in_supply),
            input = input,
            total_supply = first(total_supply),
            unit = first(unit)) %>%
  ungroup() %>%
  mutate(input_subtype = case_when(input == "Sweeteners, Other" ~"Molasses",
                                   input == "Other, Unknown" & iso3c == "ZAF" ~ "Synthetic",
                                   TRUE ~ NA))


###########################################################
#3. Joining TCFs #########
###########################################################

oecd_fao_use_biogasoline %<>% left_join(tcf_table %>% 
                                          select(output,input,input_subtype,multiplier_output_kl_to_input_t,output_qty) %>% 
                                          filter(output == "Bioethanol") %>% 
                                          mutate(process = "Biogasoline"), 
                                        by = c("process","input","input_subtype"))


###########################################################
#4. Modifying total supply in ZAF to account for the high proportion of synthetic ethanol ("Other, Unknown" share) #########
###########################################################

zaf_correction <- subset(oecd_fao_use_biogasoline,input_subtype=="Synthetic")%>%
  mutate(synthetic = share_in_supply*total_supply)

supply_biogasoline %<>% left_join(zaf_correction %>% 
                                    select(iso3c,year,process,synthetic),
                                  by = c("iso3c","year","process")) %>%
  mutate(total_supply = total_supply - coalesce(synthetic,0)) %>%
  select(-synthetic)


###########################################################
#5. Estimating the feedstock use depending on shares in supply and total supply #########
###########################################################

oecd_fao_use_biogasoline %<>% rows_update(supply_biogasoline %>% select(iso3c,year,process,total_supply),by=c("iso3c","year","process"), unmatched = "ignore") %>%
  filter(!(iso3c == "ZAF" & input == "Other, Unknown")) %>%
  group_by(process, iso3c, year) %>%
  mutate(share_in_supply = share_in_supply / sum(share_in_supply, na.rm = TRUE)) %>%
  ungroup()


oecd_fao_use_biogasoline %<>% mutate(value = share_in_supply*total_supply*multiplier_output_kl_to_input_t,
                                     unit = "kt") %>%
  select(-c(share_in_supply,multiplier_output_kl_to_input_t,output)) 

oecd_fao_use_biogasoline %<>% left_join(regions, by = "iso3c") %>% mutate(EU28 = ifelse(iso3c %in% c(EU27_countries,"GBR"),TRUE,FALSE))


###########################################################
#6. Joining to use table #########
###########################################################

use_biogasoline_complete %<>% bind_rows(oecd_fao_use_biogasoline)




###########################################################
########### ALLOCATING FEEDSTOCK USE FOR BIOETHANOL ACROSS EUROPEAN COUNTRIES #########
###########################################################

###########################################################
#1. Listing countries missing feedstock use estimates  #########
###########################################################
eu_use <- use_biogasoline_complete %>% filter(EU28==TRUE|iso3c %in% c("EU27","EU28"))
estimated <- unique(eu_use$iso3c[!(eu_use$iso3c %in% c("EU27","EU28"))])


###########################################################
#2. Listing assumed potential feedstocks by country (based on qualitative info)  #########
###########################################################

#CYP;DNK;EST;HRV;LUX;MLT;SVN;FIN;GRC;LVA: : output = 0 across all years.
#BGR;NLD : Wheat and products ; Maize and products. Use FAOSTAT shares.
#BEL : FAOSTAT products for non-food use match the qualitative information collected, so we use FAOSTAT shares. 
#IRL : very low output across all years. Use FAOSTAT. 
#ITA : 100% from "Other, Waste".  
#HUN;ROU;SVK;SWE : 100% from Maize and products.
#LTU : 100% from Rye and products


nonfood_use_eu <- subset(bioethanol_nonfood, (iso3c %in% c("BGR","NLD") & item %in% c("Wheat and products","Maize and products")) |
                           (iso3c == "IRL" & !(item %in% c("Rice and products","Cassava and products")) |
                              (iso3c == "BEL"))
)


countries_join <- c("ITA", "HUN", "ROU", "SVK", "SWE", "LTU")
commodities <- tibble(
  iso3c = c("ITA", "HUN", "ROU", "SVK", "SWE", "LTU"),
  item = c("Other, Waste", "Maize and products", "Maize and products", 
           "Maize and products", "Maize and products", "Rye and products")
)

extra_grid <- expand_grid(
  iso3c = countries_join,
  year = 2010:2022
) %>%
  left_join(commodities, by = "iso3c")

###########################################################
#3. Computing relative shares in non-food use (FAOSTAT)  #########
###########################################################

nonfood_use_eu %<>% 
  group_by(iso3c,year,use) %>%
  mutate(share_in_use = other/sum(other),
            item = item) %>%
  ungroup()


nonfood_use_eu %<>% bind_rows(extra_grid) %>%
  mutate(use = "Biogasoline",
         share_in_use = case_when(iso3c %in% countries_join ~ 1,
                                  TRUE  ~ share_in_use),
         input_subtype = case_when(item == "Sweeteners, Other" ~ "Molasses",
                                   item == "Cereals, Other" ~ "Triticale",
                                   TRUE ~ NA)) %>%
  rename(process = use,
         input = item)


###########################################################
#4. Joining supply data and TCFs, converting from share in use to share in supply  #########
###########################################################

nonfood_use_eu %<>% left_join(supply_biogasoline %>% 
                                select(iso3c,year,process,total_supply), 
                              by = c("iso3c","year","process")) %>%
  left_join(tcf_table %>%
              select(input,input_subtype,output,multiplier_output_kl_to_input_t,output_qty) %>%
              mutate(output = case_when(output == "Bioethanol" ~ "Biogasoline",
                                        TRUE ~ output)) %>%
              rename(process = output), 
            by = c("input","input_subtype","process"))


## From share in use to share in supply

nonfood_use_eu %<>% mutate(weighted_share = share_in_use / multiplier_output_kl_to_input_t) %>%
  group_by(iso3c,year,process) %>% mutate(share_in_estimated_supply = weighted_share/sum(weighted_share)) %>%
  ungroup() %>%
  select(-c(share_in_use,weighted_share))


###########################################################
#5. Final estimate of feedstock use  #########
###########################################################

nonfood_use_eu %<>%
  mutate(value = share_in_estimated_supply * total_supply * multiplier_output_kl_to_input_t) %>%
  select(-multiplier_output_kl_to_input_t)


###########################################################
#6. Joining to use table  #########
###########################################################

use_biogasoline_complete %<>%
  bind_rows(nonfood_use_eu)




###########################################################
########### ALLOCATING FEEDSTOCK USE FOR REMAINING COUNTRIES #########
###########################################################

###########################################################
#1. Listing countries missing feedstock use estimates  #########
###########################################################

biogasoline_producers <- supply_biogasoline %>% 
  group_by(process,iso3c) %>% 
  filter(any(total_supply>0)) %>%
  ungroup() %>%
  select(iso3c)

to_estimate <- setdiff(unique(unlist(as.vector(biogasoline_producers))),unique(use_biogasoline_complete$iso3c))


###########################################################
#2. Listing assumed potential feedstocks by country (based on qualitative info)  #########
###########################################################

share_in_use <- bioethanol_nonfood %>% 
  filter(iso3c=="URY" & item %in% c("Sorghum and products","Maize and products","Wheat and products","Sugar cane")) %>%
  rename(input = item)

nonfood_inputs_world <- tribble(
  ~iso3c, ~input,
  "BOL", "Sugar cane",
  "CRI", "Sugar cane",
  "CUB", "Sugar cane",
  "ECU", "Sugar cane",
  "GTM", "Sugar cane",
  "KEN", "Sweeteners, Other",
  "MOZ", "Cassava and products",
  "MWI", "Sweeteners, Other",
  "PAN", "Sugar cane",
  "SDN", "Sweeteners, Other",
  "SWZ", "Sweeteners, Other",
  "ZWE", "Sugar cane"
)

nonfood_inputs_world %<>%
  mutate(
    input_subtype = case_when(
      input == "Sweeteners, Other" ~ "Molasses",
      TRUE ~ NA_character_
    )
  ) %>%
  expand_grid(year = 2010:2022) %>%
  mutate(other = 1,
         use = "Bioethanol")


###########################################################
#3. Estimating feedstock use from relative shares in non-food use (FAOSTAT)  #########
###########################################################

## Joining TCFs and estimating share in supply 

share_in_use %<>% bind_rows(nonfood_inputs_world) %>%
  rename(output = use) %>%
  left_join(tcf_table %>% select(output,input,input_subtype,output_qty,multiplier_output_kl_to_input_t),by=c("output","input","input_subtype")) %>%
  mutate(weighted_share = other*output_qty) %>%
  group_by(iso3c,year,output) %>%
  mutate(share_in_estimated_supply = weighted_share/sum(weighted_share)) %>%
  ungroup() %>%
  select(-weighted_share)

## Joining total supply and estimating use per feedstock type. 

share_in_use %<>% 
  mutate(output = "Biogasoline") %>%
  left_join(supply_biogasoline %>% select(output,year,iso3c,total_supply),
            by = c("iso3c","year","output")) %>%
  mutate(value = share_in_estimated_supply * total_supply * multiplier_output_kl_to_input_t)


###########################################################
#4. Joining to use table  #########
###########################################################

use_biogasoline_complete %<>% bind_rows(share_in_use)




###########################################################################################
########### GAP FILLING -- USDA BIOFUELS ANNUAL COUNTRIES (MISSING YEARS ONLY) #########
###########################################################################################

###### 1) Compute the estimated supply, the estimated share of each feedstock in supply, the gap between estimated supply from feedstcks and total supply quantity

use_biogasoline_noNA <- use_biogasoline_complete %>% 
  group_by(process, iso3c, year) %>%
  filter(all(!is.na(value))) %>%
  ungroup()

pry_2022_data <- tribble(
  ~process, ~iso3c, ~year, ~input, ~value, ~output_qty,
  "Biogasoline", "PRY", 2022, "Maize and products", 501059577, 1.0,
  "Biogasoline", "PRY", 2022, "Sugar cane", 70734668, 1.0,
  "Biogasoline", "PRY", 2022, "Sweeteners, Other", 9769833, 1.0
)

use_biogasoline_noNA <- bind_rows(use_biogasoline_noNA, pry_2022_data)

## Estimating current output levels with feedstock use data, for comparison with total output.
use_biogasoline_est <- use_biogasoline_noNA %>% 
  group_by(process, iso3c, year) %>% 
  summarise(
    estimated_supply = sum(value * output_qty, na.rm = TRUE),
    .groups = "drop"
  )

use_biogasoline_est %<>%
  left_join(
    use_biogasoline_complete %>% 
      select(process, iso3c, year, total_supply) %>% 
      distinct(),
    by = c("process", "iso3c", "year")
  ) %>%
  mutate(left_to_estimate = total_supply - estimated_supply)

use_biogasoline_with_share <- use_biogasoline_noNA %>% select(process:iso3c,year,value,output_qty) %>%
  group_by(process, iso3c, year) %>%
  mutate(
    share_in_estimated_supply = value * output_qty / sum(value * output_qty, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(share_in_estimated_supply = case_when(iso3c == "PRY" & year == 2022 & input == "Maize and products" ~ 0.86,
                                               iso3c == "PRY" & year == 2022 & input == "Sugar cane" ~ 0.12,
                                               iso3c == "PRY" & year == 2022 & input == "Sweeteners, Other" ~ 0.02,
                                               TRUE ~ share_in_estimated_supply)) %>%
  left_join(use_biogasoline_est %>% select(process,iso3c,year,left_to_estimate), by = c("process","iso3c","year")) %>%
  select(-c(value,output_qty))

use_biogasoline_complete %<>% left_join(use_biogasoline_with_share, by = c("process","input","input_subtype","iso3c","year"))


###### 2) From this, we can adjust feedstock use estimates to keep input-output ratios consistent. 

use_biogasoline_complete %<>% 
  mutate(share_in_estimated_supply = coalesce(share_in_estimated_supply.x,share_in_estimated_supply.y),
         value = ifelse(is.na(share_in_estimated_supply)==FALSE,
                        value + share_in_estimated_supply * left_to_estimate * (1/output_qty),
                        value))


###### 3) Adjusting the TCFs (uniformly) for countries which combine feedstock use and total supply, and where gaps in the estimates remain. 

use_biogasoline_complete %<>% mutate(output_qty = case_when(iso3c %in% c("USA","BRA") ~ (1 + (left_to_estimate/total_supply))*output_qty,
                                                            TRUE ~ output_qty)) %>% 
  group_by(process, iso3c, input, input_subtype) %>%
  mutate(
    output_qty = if_else(
      is.na(output_qty),
      mean(output_qty, na.rm = TRUE),  # moyenne sur le groupe
      output_qty
    )
  ) %>%
  ungroup() %>% 
  mutate(share_in_estimated_supply = ifelse(share_in_estimated_supply=="NaN",NA,share_in_estimated_supply))




###########################################################################################
########### GAP FILLING - OTHER (MISSING YEARS ONLY) #########
###########################################################################################

###########################################################################################
#1. Interpolation / Extrapolation of shares of contribution to total supply, by feedstock #########
########################################################################################### 

use_biogasoline_complete %<>%
  arrange(process, iso3c, input, input_subtype, year)


use_biogasoline_complete %<>%
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
  ungroup() %>%
  mutate(share_interp = ifelse(iso3c=="BRA"&input=="Other, Unknown"&year<=2017, 
                               0.01,
                               share_interp))

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

use_biogasoline_complete %<>%
  group_by(process, iso3c, year) %>%
  group_modify(~ compute_shares_one_year(.x)) %>%
  ungroup()

use_biogasoline_complete %<>%
  mutate(
    share_in_estimated_supply_raw = share_in_estimated_supply,
    share_in_estimated_supply     = share_filled
  )

use_biogasoline_complete %<>%
  left_join(
    use_biogasoline_complete %>%
      filter(year == 2011 & iso3c == "EU28" & process == "Biogasoline") %>%
      select(input, input_subtype, share_in_estimated_supply_2011 = share_in_estimated_supply),
    by = c("input", "input_subtype")
  ) %>%
  mutate(
    share_in_estimated_supply = if_else(
      year == 2010 & iso3c == "EU28" & process == "Biogasoline",
      share_in_estimated_supply_2011,
      share_in_estimated_supply
    )
  ) %>%
  select(-share_in_estimated_supply_2011)

###########################################################################################
#3. Estimating feedstock use from extrapolated or interpolated shares #########
########################################################################################### 

use_biogasoline_complete %<>%
  mutate(
    value_raw = value, 
    value = dplyr::if_else(
      case == "case0" | is.na(share_in_estimated_supply) |
        is.na(total_supply) | is.na(output_qty),
      value,
      share_in_estimated_supply * total_supply * (1 / output_qty)
    )
  ) %>%
  relocate(value, .after = input_subtype) %>%
  select(-value_raw)




#########################################################################
################## FORMATTING USE TABLE ####################
#########################################################################

use_biogasoline_complete %<>% 
  rows_update(regions, by ="iso3c", unmatched = "ignore") %>%
  select(process,iso3c,year,input,value,unit) %>%
  rename(proc = process,
         item = input,
         use = value
  ) %>%
  mutate(proc = "Bioethanol production") %>%
  arrange(iso3c,year,proc,item)




#########################################################################
################## FORMATTING SUPPLY TABLE ####################
#########################################################################

supply_biogasoline %<>%
  mutate(output = "Bioethanol",
         process = "Bioethanol production")

etbe_suppliers <- subset(regions, EU27==TRUE | iso3c %in% c("BRA","USA","JPN","CHN"))$iso3c
etbe_zeros <- supply_biogasoline %>%
  filter(!(iso3c %in% etbe_suppliers)) %>%
  distinct(iso3c) %>%
  crossing(year = 2010:2022) %>%
  mutate(
    output       = "ETBE",
    process      = "ETBE production",
    total_supply = 0,
    unit         = "Ml"
  )

supply_biogasoline <- bind_rows(supply_biogasoline, etbe_zeros) %>%
  rows_update(regions, by ="iso3c", unmatched = "ignore")




#########################################################################
################## WRITING BIOGASOLINE SUPPLY AND USE TABLE ####################
#########################################################################

setwd(fabio_root)

saveRDS(object = supply_biogasoline,
        file = "intermediate_data/sup_biogasoline_full.rds")

saveRDS(object = use_biogasoline_complete,
        file = "intermediate_data/use_biogasoline_full.rds")



## Removing temporary objects
rm(list = ls())

