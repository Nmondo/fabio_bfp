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

setwd("/home/mmondolfo/fabio_data_local/")

########### FABIO regions #########

regions <- read.csv("inst/regions.csv") 

  ############# Other ##################

setwd("/home/mmondolfo/")

y_other_bf <- readRDS("y_other_bf.rds")
supply_final_bf_sua <- readRDS("inputs_for_final_data/supply_final_bf_sua.rds")
use_bf_coproducts <- readRDS("use_bf_coproducts_initial.rds")
btd_intermediate_other <- readRDS("btd_intermediate_other.rds")




###########################################################
########### STANDARDIZING UNITS TO KT #########
###########################################################

y_other_bf <- y_other_bf %>% 
  mutate(value = case_when(product == "Bionaphtha" & unit == "Ml" ~ 0.71*value,
                           product == "Biopropane" & unit == "Ml" ~ 0.51*value),
         unit = "kt")




###########################################################
########### JOINING TOTAL TRADE FOR GLYCEROL #########
###########################################################

total_trade_im <- btd_intermediate_other %>%
  filter(product == "Glycerol, crude") %>%
  group_by(importer_iso3, product, year) %>%
  summarise(value = sum(value),
            .groups = "drop")

total_trade_ex <- btd_intermediate_other %>%
  filter(product == "Glycerol, crude") %>%
  group_by(exporter_iso3, product, year) %>%
  summarise(value = sum(value),
            .groups = "drop")



###########################################################
########### COMPILING SUPPLY AND USE OF COPRODUCTS TO ESTIMATE NECESSARY TRADE FLOWS #########
###########################################################

compile_bf_coproducts <- supply_final_bf_sua %>%
  filter(product %in% c("Glycerol, crude", "Bionaphtha", "Biopropane")) %>%
  select(-proc) %>%
  left_join(use_bf_coproducts %>% rename(bp_feedstock_use = domestic_use_as_input), by = c("product", "iso3c", "year")) %>%
  left_join(total_trade_im %>% select(iso3c = importer_iso3, product, year, imports = value),
            by = c("product","iso3c","year")) %>%
  left_join(total_trade_ex %>% select(iso3c = exporter_iso3, product, year, exports = value),
            by = c("product","iso3c","year")) %>%
  left_join(y_other_bf %>% select(product,iso3c,year,Fuel_use=value), by = c("iso3c","product","year")) %>%
  mutate(across(c(imports, exports), ~ ifelse(product == "Glycerol, crude", coalesce(.x, 0), .x)),
         bp_feedstock_use = coalesce(bp_feedstock_use, 0),
         Fuel_use = ifelse(iso3c=="USA" & product %in% c("Biopropane","Bionaphtha"), #assume self-consumption of HVO co-products (values are close to "Other biofuels" consumption reported by EIA)
                           supply,
                           coalesce(Fuel_use, 0)),
         required_imports = case_when(product == "Glycerol, crude" ~ 0, 
                                      bp_feedstock_use + Fuel_use > supply ~ bp_feedstock_use + Fuel_use - supply,
                                      TRUE ~ 0),
         available_exports =  case_when(product == "Glycerol, crude" ~ 0, 
                                        ((bp_feedstock_use + Fuel_use) < supply) & iso3c %in% c("SGP","NLD","FIN") ~ supply - (bp_feedstock_use + Fuel_use),
                                        TRUE ~ 0),
         unknown_use = case_when(product == "Glycerol, crude" ~ coalesce(unknown_use, pmax(supply + imports - exports, 0)), 
                                 TRUE ~ unknown_use)
         )




###########################################################
########### ESTIMATING BILATERAL TRADE FLOWS OF BIONAPHTHA AND BIOPROPANE FROM BALANCING #########
###########################################################

###########################################################
#1. Estimating flows (proportional to availabilities and requirements) #########
###########################################################

btd_from_balancing <- compile_bf_coproducts %>%
  filter(product %in% c("Bionaphtha", "Biopropane")) %>%
  filter(available_exports > 0 | required_imports > 0) %>%
  group_by(product, year) %>%
  filter(any(required_imports > 0) & any(available_exports > 0)) %>%
  group_modify(~ {
    imp <- .x %>% filter(required_imports > 0)  %>% select(iso3c, required_imports)
    exp <- .x %>% filter(available_exports > 0) %>% select(iso3c, available_exports)
    
    sum_req   <- sum(imp$required_imports)
    sum_avail <- sum(exp$available_exports)
    total     <- min(sum_req, sum_avail)
    
    crossing(
      rename(imp, importer_iso3 = iso3c),
      rename(exp, exporter_iso3 = iso3c)
    ) %>%
      mutate(
        value = (required_imports  / sum_req) *
          (available_exports / sum_avail) *
          total,
        unit  = "kt"
      ) %>%
      select(importer_iso3, exporter_iso3, value, unit)
  }) %>%
  ungroup()

###########################################################
#2. Updating the bilateral trade flows dataset with these estimates #########
###########################################################

btd_final_other <- bind_rows(btd_intermediate_other, btd_from_balancing)


###########################################################
#3. Patch imports and exports into compile_bf_coproducts #########
###########################################################

btd_imports <- btd_from_balancing %>%
  group_by(iso3c = importer_iso3, product, year) %>%
  summarise(imports = sum(value), .groups = "drop")

btd_exports <- btd_from_balancing %>%
  group_by(iso3c = exporter_iso3, product, year) %>%
  summarise(exports = sum(value), .groups = "drop")

compile_bf_coproducts <- compile_bf_coproducts %>%
  rows_patch(btd_imports, by = c("iso3c", "product", "year"), unmatched = "ignore") %>%
  rows_patch(btd_exports, by = c("iso3c", "product", "year"), unmatched = "ignore") %>%
  mutate(
    imports = coalesce(imports, 0),
    exports = coalesce(exports, 0)
  )


###########################################################
#4. Update estimate of unknown_use of biofuel coproducts #########
###########################################################

compile_bf_coproducts <- compile_bf_coproducts %>%
  mutate(
    unknown_use = case_when(
      product %in% c("Biopropane", "Bionaphtha") & iso3c %in% c("SGP", "NLD", "FIN")
      ~ available_exports - coalesce(exports, 0),
      product %in% c("Biopropane", "Bionaphtha") & !(iso3c %in% c("SGP", "NLD", "FIN"))
      ~ pmax(supply - Fuel_use - bp_feedstock_use, 0),
      TRUE ~ unknown_use
    )
  ) %>%
  select(-required_imports, -available_exports)




###########################################################
########### SAVING TABLES #########
###########################################################

saveRDS(btd_final_other, "inputs_for_final_data/btd_final_bp_bf_coproducts.rds")
saveRDS(compile_bf_coproducts,"inputs_for_final_data/y_final_bf_coproducts.rds")