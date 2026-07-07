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

########### Consumption data (to collect consumption from the same source) #########

y_own <- read_excel("own_data/Compilation_data_sources.xlsx",sheet="y")

########### Pre-cleaned supply data (to collect consumption from the same source) #########

setwd(file.path(fabio_root, "intermediate_data"))

supply_biogasoline <- readRDS("sup_biogasoline_full.rds")
supply_fame_hvo <- readRDS("sup_fame_hvo_full.rds")
supply_fame_hvo %<>% rename(total_supply=value)

########### Consumption data (to collect consumption from the same source) #########

y_eia <- readRDS("y_eia.rds")
y_iea <- readRDS("y_iea.rds")
y_faostat <- readRDS("y_faostat.rds")
y_oecd_fao <- readRDS("y_oecd_fao.rds")

###########################################################
########### MAKING VECTORS #########
###########################################################

years   <- as.character(2010:2022)




################################################################################################################
########### CLEANING CONSUMPTION DATA FROM OWN COLLECTION ###############################
################################################################################################################

y_own %<>% select(-c(`2023`,`2024`)) %>%
  mutate(across(`2010`:`2022`, ~ as.numeric(as.character(.x))))

y_own %<>%
  pivot_longer(
    cols = all_of(years),
    names_to = "year",
    names_transform = list(year = as.integer),
    values_to = "value"
  ) %>%
  filter(Select == "1") %>%
  select(-product_subtype,-Select,-data_type) %>%
  rename(use = Use,
         iso3c = country_iso3,
         source = `Source(s)`) %>% 
  mutate(value = case_when(unit == "1000 barrels" ~ value*0.1589873,
                           TRUE ~ value),
         unit = if_else(unit == "1000 barrels", "Ml", unit)
  )


#Saving for later the rows for Biopropane and Bionaphtha consumption
y_other_bf <- subset(y_own, product %in% c("Biopropane", "Bionaphtha")) %>%
  mutate(value = ifelse(is.na(value), 0, value))

################################################################################################################
########### COLLECTING CONSUMPTION DATA FROM THE SAME SOURCES AS PRODUCTION DATA, WHENEVER POSSIBLE ###############################
################################################################################################################

tocollect_iea_renewables <- bind_rows(subset(supply_biogasoline %>% mutate(output="Biogasoline"), source=="IEA Renewables"), subset(supply_fame_hvo, source=="IEA Renewables"))
tocollect_eia_international <- bind_rows(subset(supply_biogasoline %>% mutate(output="Biogasoline"), source=="EIA international"), subset(supply_fame_hvo, source=="EIA international"))
tocollect_faostat <- bind_rows(subset(supply_biogasoline %>% mutate(output="Biogasoline"), source=="FAOSTAT Bioenergy"), subset(supply_fame_hvo, source=="FAOSTAT Bioenergy"))
tocollect_fao_oecd <- bind_rows(subset(supply_biogasoline %>% mutate(output="Biogasoline"), source=="OECD-FAO Agricultural outlook"), subset(supply_fame_hvo, source=="OECD-FAO Agricultural outlook"))

keys_bind_iea <- tocollect_iea_renewables %>%
  distinct(output, iso3c, year)

keys_bind_eia <- tocollect_eia_international %>%
  distinct(output, iso3c, year)

keys_bind_faostat <- tocollect_faostat %>%
  distinct(output, iso3c, year)

keys_bind_oecd_fao <- tocollect_fao_oecd %>%
  distinct(output, iso3c, year)

y_iea %<>%
  semi_join(keys_bind_iea, by = c("output", "iso3c", "year"))
y_iea %<>%
  arrange(iso3c, output, year) %>%
  distinct(iso3c, output, year, .keep_all = TRUE)

y_eia %<>%
  semi_join(keys_bind_eia, by = c("output", "iso3c", "year"))

y_faostat %<>%
  semi_join(keys_bind_faostat, by = c("output", "iso3c", "year"))

y_oecd_fao %<>%
  semi_join(keys_bind_oecd_fao, by = c("output", "iso3c", "year"))

y_table <- bind_rows(y_iea, y_eia, y_faostat, y_oecd_fao) %>% 
  select(-process,-Frequency) %>%
  rename(product = output) %>%
  mutate(product = ifelse(product == "Biodiesel", "Biodiesel & Renewable diesel", product),
         use = "Total")




################################################################################################################
########### MERGING CONSUMPTION DATA FROM ALL SOURCES, AND MAKING SELECTIONS ###############################
################################################################################################################

################################################################################################################
#1. Binding y from all sources ###############################
################################################################################################################

y_table <- bind_rows(y_own, y_table) %>%
  arrange(iso3c, product, year)


################################################################################################################
#2. Making three uses in final demand: Fuel, Non fuel and Total (the latter is temporary) ###############################
################################################################################################################

y_table %<>%
  group_by(year, iso3c, product) %>%
  mutate(
    # difference Total - Fuel in each group (only if both exist)
    diff_total_fuel = {
      val_total <- value[use == "Total"]
      val_fuel  <- value[use == "Fuel"]
      if (length(val_total) == 1 && length(val_fuel) == 1) val_total - val_fuel else NA_real_
    },
    # change value ONLY when diff_total_fuel is known
    value = case_when(
      product %in% c("Biogasoline", "Bioethanol") &
        use == "Total" &
        !is.na(diff_total_fuel) ~ diff_total_fuel,
      TRUE ~ value
    ),
    # change use ONLY when diff_total_fuel is known; otherwise keep "Total"
    use = case_when(
      product %in% c("Biogasoline", "Bioethanol") &
        use == "Total" &
        !is.na(diff_total_fuel) ~ "Non-fuel",
      product %in% c("Bio jet kerosene", "Biodiesel", "Renewable diesel",
                     "Biodiesel & Renewable diesel", "ETBE") ~ "Fuel",
      TRUE ~ use
    )
  ) %>%
  ungroup() %>%
  select(-diff_total_fuel)


################################################################################################################
#3. Correct some consumption values / make assumptions on national biobased diesels mix based on qualitative information ###############################
################################################################################################################

keys_change <- y_table %>%
  filter(product == "Biodiesel & Renewable diesel") %>%
  filter(
    (iso3c == "SWE" & year <= 2010) |
    (iso3c == "IRL" & year <= 2015) |
      (iso3c == "NOR" & year <= 2013) |
      (iso3c == "AUS" & year == 2022) |
      (iso3c == "LUX" & year <= 2010) |
      (iso3c == "ISL") |
      (iso3c %in% c("SVK","SVN","LVA","EST") & year <= 2018) | 
      (iso3c %in% c("ALB","BGR","BLR","CYP","DOM","GRC","HKG","HRV",
                    "KOR","LTU","MLT","PRY","ROU","TUR","TWN","URY"))
  ) %>%
  distinct(iso3c, year) %>%
  ungroup()

y_table <- y_table %>%
  mutate(
  value = if_else(
    (product == "Biodiesel & Renewable diesel" & iso3c %in% c("EGY","ETH","GEO","GTM","IRN","KAZ","MKD","NGA","NZL","PAK","SAU","SRB","VNM","ZAF")) |
    (product == "Biogasoline" & iso3c %in% c("HRV","HTI")),
    0, value),
  product = case_when(
    (iso3c == "SWE" & product == "Biodiesel & Renewable diesel" & year <= 2010) | 
    (iso3c == "IRL" & product == "Biodiesel & Renewable diesel" & year <= 2015) | 
      (iso3c == "NOR" & product == "Biodiesel & Renewable diesel" & year <= 2013) |
      (iso3c == "AUS" & product == "Biodiesel & Renewable diesel" & year == 2022) |
      (iso3c == "ISL" & product == "Biodiesel & Renewable diesel" & year <= 2016) |
      (iso3c == "LUX" & product == "Biodiesel & Renewable diesel" & year <= 2010) |
      (iso3c %in% c("SVK","SVN","LVA","EST") & product == "Biodiesel & Renewable diesel" & year <= 2018) | 
      (iso3c %in% c("ALB","BGR","BLR","CYP","DOM","GRC","HKG","HRV","KOR","LTU","MLT","PRY","ROU","TUR","TWN","URY") & product == "Biodiesel & Renewable diesel") ~ "Biodiesel",
    (iso3c == "ISL" & product == "Biodiesel & Renewable diesel" & year >= 2017) ~ "Renewable diesel",
    TRUE ~ product)) %>%
  
  group_by(iso3c) %>%
  do({
    df <- .
    fixed <- df %>%
      filter(iso3c %in% c("FIN","JPN")) %>%
      arrange(source) %>%
      distinct(product, use, year, .keep_all = TRUE)
    
    others <- df %>% filter(! iso3c %in% c("FIN","JPN"))
    
    bind_rows(others, fixed)
  }) %>%
  ungroup() %>%

  filter(!(iso3c %in% c("HUN","NLD","POL","FRA") & product == "Biodiesel & Renewable diesel"),
         !(iso3c == "NOR" & product == "Biodiesel & Renewable diesel" & year >= 2014),
         !(iso3c == "NOR" & source == "Norwegian Miljodirektoratet" & year <= 2013),
         !(iso3c == "SWE" & source == "Energimyndigheten dynamic panel" & year <= 2010),
         !(iso3c == "SWE" & source == "IEA Renewables" & year >= 2011),
         !(iso3c == "FRA" & product == "Biogasoline" & source == "IEA Renewables"),
         !(iso3c == "FRA" & product == "Biogasoline" & year >= 2014 & source == "IEA Renewable Energy Progress Tracker"),
         !(iso3c == "FRA" & year <= 2013 & source == "CarbuRe"),
         !(iso3c == "IRL" & product == "Biodiesel & Renewable diesel" & year >= 2016),
         !(iso3c == "IRL" & product == "Biogasoline" & source == "IEA Renewables" & year >= 2016),
         !(iso3c == "IRL" & product %in% c("Bioethanol","Biodiesel") & source == "NORA BOS/RTFO Annual reports" & year <= 2015),
         !(iso3c == "ITA" & product == "Biogasoline"),
         !(iso3c == "ITA" & source == "GSE Rapporto attivita" & year <= 2011),
         !(iso3c == "ITA" & source != "GSE Rapporto attivita" & year >= 2012),
         !(iso3c == "NOR" & product == "Biogasoline"),
         !(iso3c == "PER" & source == "IEA Renewables"),
         !(iso3c == "PER" & product == "Biogasoline" & source == "USDA Biofuels annual" & year <= 2017 & use == "Total"),
         !(iso3c == "PHL" & product == "Biogasoline" & source == "OECD-FAO Agricultural outlook"),
         !(iso3c == "POL" & source == "IEA Renewables"),
         !(iso3c == "PRT" & product == "Biodiesel & Renewable diesel" & source != "Laboratorio Nacional de Energia e Geologia; DGEG Estatisticas rapidas das renovaveis" & year %in% 2014:2017),
         !(iso3c == "PRT" & product == "Biodiesel & Renewable diesel" & source == "IEA Renewables" ),
         !(iso3c == "SWE" & product == "Biodiesel & Renewable diesel" & year >= 2011),
         !(iso3c == "DNK" & product == "Bioethanol" & source == "Eurostat"),
         !(iso3c == "CZE" & product == "Biodiesel & Renewable diesel" & year <= 2019),
         !(iso3c == "AUS" & source == "USDA Biofuels annual" & year == 2022),
         !(iso3c == "NLD" & is.na(source) == TRUE),
         !(iso3c == "CAN" & product == "Biogasoline" & source == "IEA Renewables"),
         !(iso3c %in% c("BEL","DEU","HUN") & source == "IEA Renewables"))

# Adding the rows 

extra_rows <- keys_change %>%
  select(iso3c,year) %>%
  crossing(product = c("Biodiesel", "Renewable diesel")) %>%
  anti_join(y_table %>% distinct(iso3c, year, product), 
            by = c("iso3c", "year", "product")) %>%
  mutate(value = 0)

y_table %<>% 
  bind_rows(extra_rows) %>%
  arrange(iso3c,product,year)


################################################################################################################
#4. Disaggregating rows where "Biodiesel & Renewable diesel" consumption is 0 into two rows (Biodiesel and Renewable diesel) ###############################
################################################################################################################

y_table %<>% bind_rows(
    y_table %>%
      filter(product == "Biodiesel & Renewable diesel" & value == 0) %>%
      mutate(product = "Renewable diesel",
             unit = "kt",
             use = "Fuel")
  ) %>%
  bind_rows(
    y_table %>%
      filter(product == "Biodiesel & Renewable diesel" & value == 0) %>%
      mutate(product = "Biodiesel",
             unit = "kt",
             use = "Fuel")
  ) %>%
  filter(!(product == "Biodiesel & Renewable diesel" & value == 0)) %>%
  
  select(product,use,iso3c,unit,source,year,value) 


################################################################################################################
#5. Disaggregating rows where Biogasoline 0 into two rows (Bioethanol and ETBE) ###############################
################################################################################################################

extra_rows <- y_table %>%
  filter(
    use == "Total",
    value == 0,
    product %in% c("Biogasoline", "Bioethanol")) %>%
  select(-use) %>%                         
  crossing(use = c("Non-fuel", "Fuel"))

y_table %<>%
  filter(!(use == "Total" & product %in% c("Biogasoline", "Bioethanol") & value == 0)) %>%  
  bind_rows(extra_rows) 

# Country specifications of ETBE/bioethanol consumption 
y_table %<>% left_join(regions %>% select(iso3c, continent, region), by = "iso3c")

keys_change <- y_table %>%
  filter(product == "Biogasoline",
         use == "Fuel",
         (!(continent %in% c("EU", "EUR")) | region == "East Asia" | iso3c %in% c("DNK","IRL","JPN")))

extra_rows <- keys_change %>%
  mutate(product = "ETBE",
         value   = 0,
         use     = "Fuel")

y_table %<>%
  mutate(product = case_when(
    product == "Biogasoline" & (!(continent %in% c("EU", "EUR")) | region == "East Asia" | iso3c %in% c("DNK","IRL","JPN")) ~ "Bioethanol",
    TRUE ~ product
  )) %>%
  bind_rows(extra_rows)


################################################################################################################
#6. Assigning non-fuel use only to Bioethanol (ETBE is a fuel additive) ###############################
################################################################################################################

y_table %<>%
  mutate(product = case_when(product == "Biogasoline" & use == "Non-fuel" ~ "Bioethanol",
                             TRUE ~ product)) 


################################################################################################################
#7. Assigning 0 fuel use both to Bioethanol and ETBE when Biogasoline fuel use is 0 ###############################
################################################################################################################

candidates <- y_table %>%
  filter(
    product == "Biogasoline",
    use == "Fuel",
    value == 0
  )

key_cols <- c("iso3c", "year", "use") 

potential_new <- bind_rows(
  candidates %>% mutate(product = "Bioethanol"),
  candidates %>% mutate(product = "ETBE")
)

missing_new <- anti_join(
  potential_new,
  y_table,
  by = c(key_cols, "product")
)

y_table %<>%
  filter(!(product == "Biogasoline" & use == "Fuel" & value == 0)) %>%
  bind_rows(missing_new) %>%
  arrange(iso3c, year, product, use)



rm(keys_change,extra_rows,candidates,key_cols,potential_new,missing_new)

################################################################################################################
########### LINEAR INTERPOLATION OF THE RELATIVE SHARES OF BIODIESEL VS. RENEWABLE DIESEL CONSUMPTION FOR PORTUGAL, 2014-2017 ###############################
################################################################################################################

################################################################################################################
#1. Extract 2013/2018 shares for PRT Biodiesel & Renewable diesel ###############################
################################################################################################################

prt_shares <- y_table %>%
  filter(iso3c == "PRT", 
         year %in% c(2013, 2018),
         product %in% c("Biodiesel", "Renewable diesel")) %>%
  group_by(year) %>%
  summarise(
    total_detailed = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Cross to get per-product shares
  crossing(product = c("Biodiesel", "Renewable diesel")) %>%
  left_join(y_table %>% 
              filter(iso3c == "PRT", year %in% c(2013, 2018), 
                     product %in% c("Biodiesel", "Renewable diesel")) %>%
              select(year, product, value),
            by = c("year", "product")) %>%
  group_by(year) %>%
  mutate(share = value / total_detailed) %>%
  select(year, product, share) %>%
  ungroup()


################################################################################################################
#2. Get total "Biodiesel & Renewale diesel" consumption in PRT for 2014-2017 ###############################
################################################################################################################ 

prt_totals <- y_table %>%
  filter(iso3c == "PRT", product == "Biodiesel & Renewable diesel", 
         year %in% 2014:2017) %>%
  select(year, value)


################################################################################################################
#3. Linearly interpolate shares 2014-2017 ###############################
################################################################################################################ 

years_fill <- tibble(year = 2014:2017)
interp_shares <- prt_shares %>%
  complete(year = c(2013, 2018, 2014:2017), product, fill = list(share = NA)) %>%
  group_by(product) %>%
  arrange(year) %>%
  mutate(share = zoo::na.approx(share, na.rm = FALSE)) %>%  # linear interp
  ungroup() %>%
  filter(year %in% 2014:2017)


################################################################################################################
#4. Estimate "Biodiesel" and "Renewable diesel" consumption from total consumption and interpolated shares  ###############################
################################################################################################################ 

new_rows <- prt_totals %>%
  crossing(product = c("Biodiesel", "Renewable diesel")) %>%
  left_join(interp_shares, by = c("year", "product")) %>%
  mutate(value = value * share,
         iso3c = "PRT",
         source = "Estimate",
         unit = "kt",
         use = "Fuel") %>%  # total * interpolated share
  select(-share)  # drop temp col; retain other cols from y_table as needed


################################################################################################################
#5. Updating y_table with PRT 2014-2017  ###############################
################################################################################################################ 

y_table <- y_table %>%
  rows_upsert(new_rows, by = c("iso3c", "year", "product", "use")) %>%
  filter(!(iso3c == "PRT" & product == "Biodiesel & Renewable diesel" & year %in% 2014:2017))




################################################################################################################
########### SAVING CONSUMPTION TABLE ###############################
################################################################################################################

setwd(fabio_root)

saveRDS(y_table,
        "intermediate_data/y_table_initial.rds")

saveRDS(y_other_bf,
        "intermediate_data/y_other_bf.rds")

rm(list = ls())