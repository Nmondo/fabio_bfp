###########################################################
########### LOADING PACKAGES #########
###########################################################

library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(stringr)
library(magrittr)
library(janitor)

###########################################################
########### LOADING DATA #########
###########################################################

setwd("/home/mmondolfo/fabio_bfp/")

########### FABIO regions #########

regions <- read.csv("inst/regions_full.csv", fileEncoding = "latin1") %>% filter(current == TRUE)

############# Supply data from own collection ##################

supply <- read_excel("own_data/Compilation_data_sources.xlsx",sheet="supply")
supply_selection <- read_excel("own_data/Compilation_data_sources.xlsx",sheet="supply_selection")
supply_pretreatment <- read_excel("own_data/Compilation_data_sources.xlsx",sheet="supply_pretreatment")
neste_hvo <- read_excel("own_data/Neste_financial_statements_share_of_sales_renewables.xlsx")

############# Supply and consumption data from various sources ##################
eia_prod_and_cons <- read.csv("own_data/eia_biofuels_prod_and_cons.csv",sep=",",header=FALSE,skip=1)
supply_consumption_iea <- read.csv("own_data/IEA_Renewables_supply_consumption.csv")
oecd_fao_supply <- read.csv("own_data/oecd_fao_use_complete.csv")
faostat_bioenergy <- read.csv("own_data/FAOSTAT_bioenergy.csv",sep=";",header=TRUE)

y_oecd_fao <- read.csv("own_data/oecd_fao_y_complete.csv")

setwd("/home/mmondolfo/fabio_bfp/intermediate_data/")




###########################################################
########### MAKING FUNCTION #########
###########################################################

pivot_df <- function(df) {
  df %>%
    pivot_longer(
      cols = matches("^(20)\\d{2}$"),
      names_to = "year",
      names_transform = list(year = as.integer),
      values_to = "value"
    )
}

###########################################################
########### MAKING VECTORS #########
###########################################################

years   <- as.character(2010:2022)




###########################################################
########### CLEANING DATA FROM OWN COLLECTION #########
###########################################################

supply %<>%
  mutate(across(`2010`:`2022`, ~ as.numeric(as.character(.x))))

# Selecting variables of interest and removing rows out of the scope of this study
supply %<>% select(-c(TCF,...26,`2023`,`2024`)) %>% filter(!(process %in% c("Biogas","Biomethane")))
supply_selection <- supply_selection %>% select(-c(TCF,...26,`2023`,`2024`)) %>% filter(!(process %in% c("Biogas","Biomethane")))

id_cols  <- setdiff(names(supply_selection), years) 

rows_to_drop <- supply_selection %>%
  filter(if_all(all_of(years), ~ str_detect(as.character(.x), "F"))) %>%
  distinct(across(all_of(id_cols)))

supply %<>%
  anti_join(rows_to_drop, by = id_cols)

supply_selection %<>%   anti_join(rows_to_drop, by = id_cols)
  
  supply %<>%
  filter(is.na(output_subtype) | output_subtype != "Fuel",
         process != "Inedible animal or vegetable fats and oils") %>%
    group_by(country_iso3,process,input,output,`Source(s)`) %>% summarise(
      across(`2010`:`2022`, ~ sum(.x, na.rm = FALSE)),
      unit = first(unit)
      ) %>% ungroup()
      
  supply_selection %<>%
    filter(is.na(output_subtype) | output_subtype != "Fuel",
           process != "Inedible animal or vegetable fats and oils") %>%
    group_by(country_iso3,process,input,output,`Source(s)`) %>%
    summarise(
      across(`2010`:`2022`, ~ first(.x)),
      unit = first(unit)
    ) %>% ungroup()
  

### Pivot longer to filter the rows we select.
  
  supply_long <- supply %>%
    pivot_df()
  
  supply_selection_long <- supply_selection %>%
    pivot_longer(
      cols = all_of(years),
      names_to = "year",
      names_transform = list(year = as.integer),
      values_to = "selection"
    )  
  
  supply_long %<>%   inner_join(
    supply_selection_long,
    by = c(
      "year",
      "process",
      "input",
      "output",
      "country_iso3",
      "Source(s)",
      "unit"
    )
  )
  
supply_long %<>% filter(selection=="T",country_iso3!="Other") %>% select(-selection) %>%
  mutate(output = case_when(output=="Bioethanol" ~ "Biogasoline",
                            TRUE ~ output),
         process = case_when(process=="Bioethanol" ~ "Biogasoline",
                             TRUE ~ process)
         )

supply_long %<>%  
  group_by(country_iso3, output, process, year, `Source(s)`) %>%
  summarise(value = sum(value, na.rm = TRUE),
    unit  = first(unit),
    .groups = "drop"
  ) %>%
  rename(
    iso3c = country_iso3,
    source = `Source(s)`
  )

supply_long %<>% left_join(regions, by="iso3c")




###########################################################
########### CLEANING DATA FROM NESTE REPORTS FOR RENEWABLE DIESEL PRODUCTION AND SALES #########
###########################################################

# Production capacity by plant-year

neste_capacities <- neste_hvo[c(58,60:62),1:17]  %>% 
  row_to_names(1) %>%
  mutate(across(2:16, ~1.282*as.numeric(.x)), # Values were in kt and must be converted to Ml (*1.282)
         unit = "Ml") %>%
  rename(iso3c = capacity) %>%
  pivot_df()


# Total HVO output by year 

neste_total_output <- neste_hvo[24,1:16] %>% 
  mutate(across(2:16, ~1.282*as.numeric(.x)),
         unit = "Ml") %>% 
  rename_with(~ c("product", .x[2:16], "unit")) %>% 
  mutate(product = "Renewable diesel") %>%
  pivot_df()


# Total HVO sales by region (North America VS EU)

neste_regional <- neste_hvo[28:31,1:16] %>% 
  mutate(across(2:16, ~1.282*as.numeric(.x)),
         unit = "Ml") %>%
  rename_with(~ c("region", .x[2:16], "unit")) %>%
  pivot_df()


# Total HVO sales by group of countries within Europe 

neste_eu <- neste_hvo[34:37,1:16] %>% 
  mutate(across(2:16, ~1.282*as.numeric(.x)),
         unit = "Ml") %>%
  rename_with(~ c("region", .x[2:16], "unit")) %>%
  pivot_df()


# Correspondence between Neste country categories and their iso codes

neste_eu_corresp <- neste_hvo[43:51,1:2] 
colnames(neste_eu_corresp) <- neste_hvo[42,1:2]


## Estimating output by plant, from the yearly share of each plant's capacity in total capacity; and annual output

neste_output_by_plant <- neste_capacities %>%
  group_by(year) %>%
  mutate(
    share = value / sum(value, na.rm = FALSE)
  ) %>%
  ungroup() %>%
  left_join(
    neste_total_output %>%
      select(-unit) %>%
      rename(total_output = value),
    by = "year"
  ) %>%
  mutate(value = share * total_output) %>%
  select(-share, -total_output)

## Saving these tables

rm(neste_hvo,neste_capacities,neste_total_output)
nms <-  ls(envir = .GlobalEnv)[startsWith( ls(envir = .GlobalEnv), "neste")]
neste_list <- mget(nms, envir = .GlobalEnv)

saveRDS(neste_list, file = "neste_data_clean.rds")




###########################################################
########### CLEANING IEA RENEWABLES DATA ########
###########################################################

supply_consumption_iea <- supply_consumption_iea %>% 
  select(-c(STRUCTURE:ACTION,`Country.Region`,ENERGY_BALANCE_FLOW,ENERGY_PRODUCT,FREQUENCY,`Time.Period`,`Observation.value`,QUALIFIER,UNIT,CONF_STATUS,DECIMALS,Decimals))
supply_consumption_iea <- supply_consumption_iea %>% arrange(COUNTRY,Product,Flow,TIME_PERIOD) %>% filter(TIME_PERIOD!="2024",Product!="Wood pellets")

supply_consumption_iea <- supply_consumption_iea %>% filter(Product %in% c("Biogasoline","Biodiesels","Bio Jet Kerosene","Other liquid biofuels","Other Biogases from anaerobic processes")) %>% 
  mutate(Product = case_when(Product == "Biodiesels" & COUNTRY %in% c("FRANCE","ITALY","CHINA","NETHERLANDS","SPAIN","UK","USA","FINLAND")  ~ "Biodiesel & Renewable diesel",
                             Product == "Biodiesels" & !(COUNTRY %in% c("FRANCE","ITALY","CHINA","NETHERLANDS","SPAIN","UK","USA","FINLAND")) ~ "Biodiesel",
                             Product == "Bio Jet Kerosene" ~"Bio jet kerosene",
                             Product == "Other Biogases from anaerobic processes" ~ "Biogases",
                             TRUE ~ Product ))

regions_lookup <- regions %>%
  mutate(area_lower = tolower(name)) %>%
  select(area_lower, iso3c)

supply_consumption_iea <- supply_consumption_iea %>% 
  mutate(country = tolower(COUNTRY)) %>% 
  left_join(regions_lookup, by = c("country" = "area_lower")) %>% 
  mutate(
    iso3c = case_when(
      COUNTRY == "BOSNIAHERZ" & is.na(iso3c) ~ "BIH",
      COUNTRY == "COSTARICA"  & is.na(iso3c) ~ "CRI",
      COUNTRY == "CZECH"      & is.na(iso3c) ~ "CZE",
      COUNTRY == "KOREA"      & is.na(iso3c) ~ "KOR",
      COUNTRY == "KOSOVO"     & is.na(iso3c) ~ "XKX",
      COUNTRY == "MOLDOVA"    & is.na(iso3c) ~ "MDA",
      COUNTRY == "NEWZEALAND" & is.na(iso3c) ~ "NZL",
      COUNTRY == "NORTHMACED" & is.na(iso3c) ~ "MKD",
      COUNTRY == "TURKIYE"    & is.na(iso3c) ~ "TUR",
      COUNTRY == "UK"         & is.na(iso3c) ~ "GBR",
      COUNTRY == "USA"        & is.na(iso3c) ~ "USA",
      TRUE ~ iso3c
    )
  )

supply_consumption_iea <- supply_consumption_iea %>%
  left_join(regions, by = "iso3c")


supply_consumption_iea <- supply_consumption_iea %>% rename(year = TIME_PERIOD,
                                                              value = OBS_VALUE,
                                                              output = Product,
                                                              unit = Unit
                                                              ) %>%
  mutate(source = "IEA Renewables") %>%
  select(-c(COUNTRY,Qualifier,Confidential.Status,country))

x_iea <- supply_consumption_iea %>% filter(Flow=="Indigenous production") %>% select(-Flow)
x_iea %<>% mutate(process = case_when(output == "Biodiesel & Renewable diesel" ~ "FAME & HVO",
                                      output == "Biodiesel" ~ "FAME",
                                      output %in% c("Other liquid biofuels","Bio jet kerosene") ~ "HVO",
                                      output == "Biogasoline" ~ "Biogasoline",
                                      output == "Biogases" ~ "Biogases",
                                      TRUE ~ NA))




###########################################################
########### CLEANING OECD-FAO AGRICULTURAL OUTLOOK DATA ########
###########################################################

oecd_fao_supply %<>% select(REF_AREA,Commodity,Measure,Unit.of.measure,TIME_PERIOD,OBS_VALUE) %>%
  rename(iso3c = REF_AREA,
         data_type = Measure,
         output = Commodity,
         unit = Unit.of.measure,
         year = TIME_PERIOD,
         value = OBS_VALUE) %>%
  filter(data_type == "Production", year <= 2022) %>%
  select(-data_type) %>%
  mutate(unit = "Ml",
         output = case_when(output == "Ethanol" ~ "Biogasoline",
                            TRUE ~ output),
         process = case_when(output == "Biogasoline" ~ "Biogasoline",
                             output == "Biodiesel" ~ "FAME",
                             TRUE ~ NA),
         iso3c = ifelse(iso3c=="EU","EU27",iso3c),
         source = "OECD-FAO Agricultural outlook") %>%
  filter(output %in% c("Biogasoline","Biodiesel")) %>%
  relocate(output, .after = "iso3c") %>%
  arrange(iso3c,output,year)

oecd_fao_supply %<>% left_join(regions, by = "iso3c")




###########################################################
########### CLEANING FAOSTAT BIOENERGY DATA ########
###########################################################

faostat_bioenergy %<>% 
  mutate(Value = case_when(is.na(Value)==TRUE ~ 0,
                          Item=="Biodiesel" ~ (1/36.8)*Value, # assumed energy content in FAOSTAT Bioenergy
                          Item=="Bio jet kerosene" ~ (1/40)*Value, # assumed energy content in FAOSTAT Bioenergy
                          Item=="Biogasoline" ~(1/26.8)*Value, # assumed energy content in FAOSTAT Bioenergy
                          Item=="Other liquid biofuels" ~ (1/27.4)*Value, # assumed energy content in FAOSTAT Bioenergy
                          TRUE ~ Value),
        Unit = case_when(Item %in% c("Biodiesel","Bio jet kerosene","Biogasoline","Other liquid biofuels") ~ "kt",
                         TRUE ~ Unit),
        process = case_when(Item == "Biodiesel" ~ "FAME",
                            Item %in% c("Other liquid biofuels","Bio jet kerosene") ~ "HVO",
                            Item == "Biogasoline" ~ "Biogasoline",
                            Item == "Biogases" ~ "Biogases",
                            TRUE ~ NA),
        Area = case_when(Area == "Czechia" ~ "Czech Republic",
                         Area == "Liechtenstein" ~ "RoW",
                         Area == "Netherlands (Kingdom of the)" ~ "Netherlands",
                         Area == "Türkiye" ~ "Turkey",
                         Area == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
                         TRUE ~ Area)
)

faostat_bioenergy %<>% left_join(regions, by=c("Area"="name")) %>%
  rename(output = Item,
         year = Year,
         unit = Unit,
         value = Value,
         name = Area) %>% 
  select(-c(Domain)) %>% 
  filter(is.na(iso3c)==FALSE)

x_faostat <- subset(faostat_bioenergy, Element== "Energy production") %>% select(-Element) 




###########################################################
########### CLEANING EIA INTERNATIONAL DATA ########
###########################################################

colnames(eia_prod_and_cons) <- c("","country_and_type",as.character(eia_prod_and_cons[1,3:ncol(eia_prod_and_cons)]))
eia_prod_and_cons <- eia_prod_and_cons[2:nrow(eia_prod_and_cons),c(2:ncol(eia_prod_and_cons))]
eia_prod_and_cons <- eia_prod_and_cons %>% left_join(regions,by=c("country_and_type"="eia"))
iso_vec <- regions$iso3c

eia_prod_and_cons %<>%
  rowwise() %>%
  mutate(
    iso3c = case_when(
      !is.na(iso3c) ~ iso3c,
      TRUE ~ {
        hits <- iso_vec[str_detect(iso_vec, fixed(country_and_type))]
        if (length(hits) == 0) NA_character_ else hits[1]
      }
    )
  ) %>%
  ungroup() %>%
  mutate(iso3c = case_when(country_and_type == "Bolivia" ~ "BOL",
                           country_and_type == "Brunei" ~ "BRN",
                           country_and_type == "Burma" ~ "MMR",
                           country_and_type == "Congo-Brazzaville" ~ "COG",
                           country_and_type == "Congo-Kinshasa" ~ "COD",
                           country_and_type == "Cote d'Ivoire" ~ "CIV",
                           country_and_type == "Eswatini" ~ "SWZ",
                           country_and_type == "Gambia, The" ~ "GMB",
                           country_and_type == "Hong Kong" ~ "HKG",
                           country_and_type == "Iran" ~ "IRN",
                           country_and_type == "Laos" ~ "LAO",
                           country_and_type == "Macau" ~ "MAC",
                           country_and_type == "Russia" ~ "RUS",
                           country_and_type == "Syria" ~ "SYR",
                           country_and_type == "Taiwan" ~ "TWN",
                           country_and_type == "Tanzania" ~ "TZA",
                           country_and_type == "The Bahamas" ~ "BHS",
                           country_and_type == "Venezuela" ~ "VEN",
                           country_and_type == "Vietnam" ~ "VNM",
                           country_and_type == "Sudan" ~ "SDN",
                           TRUE ~ iso3c
  ))



eia_prod_and_cons %<>%
  mutate(
    block_id     = (row_number() - 1) %/% 7,
    pos_in_block = (row_number() - 1) %% 7 + 1       
  ) %>%
  group_by(block_id) %>%
  mutate(
    first_iso3c  = as.character(first(iso3c)),
    iso3c = case_when(
      pos_in_block == 1                 ~ NA_character_, 
      is.na(first_iso3c)                ~ "ROW",            
      TRUE                              ~ first_iso3c
    ),
    flow = case_when(pos_in_block %in% c(3,4) ~ "production",
                     pos_in_block %in% c(6,7) ~ "consumption",
                     TRUE ~ NA)
  ) %>%
  ungroup() %>%
  filter(!(pos_in_block %in% c(1,2,5))) %>%
  select(-block_id, -pos_in_block, -first_iso3c) %>%
  rename(product = country_and_type) %>%
  mutate(product = case_when(product == "        Fuel ethanol (Mmt)" ~ "Biogasoline",
                             product == "        Biomass-based diesel (Mmt)" ~ "Biodiesel"))

eia_prod_and_cons %<>% mutate(across(`2010`:`2019`, ~ as.numeric(.)))

eia_prod_and_cons %<>%
  group_by(product, iso3c, flow) %>%
  summarise(
    across(`2010`:`2022`,
           ~ sum(.x, na.rm = cur_group()$iso3c == "ROW")),
    .groups = "drop"
  ) %>%
  mutate(across(`2010`:`2022`, ~ ifelse(is.na(.x)==TRUE,0,.x)))

eia_prod_and_cons %<>%
  pivot_longer(
    cols = `2010`:`2022`,
    names_to = "year",
    values_to = "value",
    names_transform = list(year = as.integer)
  ) %>%
  arrange(product, iso3c, flow, year)

eia_prod_and_cons %<>% 
  left_join(regions, by = "iso3c") %>%
  rename(output = product) %>%
  mutate(source = "EIA international",
         unit = "kt",
         process = case_when(output == "Biodiesel" ~ "FAME",
                             output == "Biogasoline" ~ "Biogasoline")
  )

x_eia <- subset(eia_prod_and_cons, flow=="production") %>% select(-flow)




###########################################################
########### ITERATIVE ADDITIONS OF SUPPLY DATA BY ORDER OF PREFERENCE OF DATA SOURCES ########
###########################################################

###########################################################
#1. Merging IEA Renewables ########
###########################################################

keys_bind_1 <- supply_long %>%
  distinct(output, iso3c, year)

x_iea %<>%
  left_join(
    keys_bind_1 %>% mutate(match_supply = TRUE),
    by = c("output", "iso3c", "year")
  ) %>%
  mutate(selection = is.na(match_supply)) %>%
  select(-c(match_supply,Frequency))

x_iea %<>% filter(selection==TRUE) %>% select(-selection)

supply_intermediate1 <- rbind(supply_long,x_iea)


###########################################################
#2. Merging OECD-FAO Agricultural Outlook ########
###########################################################

keys_bind_2 <- supply_intermediate1 %>%
  distinct(output, iso3c, year)

oecd_fao_supply %<>%
  left_join(
    keys_bind_2 %>% mutate(match_supply = TRUE),
    by = c("output", "iso3c", "year")
  ) %>%
  mutate(selection = is.na(match_supply)) %>%
  select(-match_supply)

oecd_fao_supply %<>% filter(selection==TRUE) %>% 
  select(-selection) 

supply_intermediate2 <- rbind(supply_intermediate1,oecd_fao_supply)


###########################################################
#3. Merging FAOSTAT Bioenergy ########
###########################################################

keys_bind_3 <- supply_intermediate2 %>%
  distinct(output, iso3c, year)

x_faostat %<>%
  left_join(
    keys_bind_3 %>% mutate(match_supply = TRUE),
    by = c("output", "iso3c", "year")
  ) %>%
  mutate(selection = is.na(match_supply),
         source = "FAOSTAT Bioenergy") %>%
  select(-c(match_supply))

x_faostat %<>% filter(selection==TRUE) %>% 
  select(-selection) 

supply_intermediate3 <- rbind(supply_intermediate2,x_faostat)


###########################################################
#4. Merging EIA International ########
###########################################################

keys_bind_4 <- supply_intermediate3 %>%
  distinct(output, iso3c, year)

x_eia %<>%
  left_join(
    keys_bind_4 %>% mutate(match_supply = TRUE),
    by = c("output", "iso3c", "year")
  ) %>%
  mutate(selection = is.na(match_supply)) %>%
  select(-c(match_supply))

x_eia %<>% filter(selection==TRUE) %>% 
  select(-selection) 

supply_intermediate4 <- rbind(supply_intermediate3,x_eia) %>% 
  filter(!(iso3c %in% c("EU27","EU28")))




###########################################################
########### MAKING SUPPLY TABLE FOR BIOETHANOL/BIOGASOLINE #########
###########################################################

###########################################################
#1. Formatting the table #########
###########################################################

supply_biogasoline <- subset(supply_intermediate4, output=="Biogasoline")

full_table_biogasoline <- expand.grid(
  output  = "Biogasoline",
  process = "Biogasoline",
  iso3c   = unique(regions$iso3c),
  year    = 2010:2022,
  source = NA,
  value = NA,
  unit = NA,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
) 
full_table_biogasoline %<>% left_join(regions, by="iso3c")

missing_keys <- full_table_biogasoline %>%
  anti_join(supply_biogasoline, by = c("output", "process", "iso3c", "year"))

supply_biogasoline_full <- bind_rows(supply_biogasoline, missing_keys)
supply_biogasoline_full %<>% arrange(iso3c,output,process,year) %>%
  mutate(value = case_when(unit=="kt" ~ 1.267*value,
                           TRUE ~ value),
         unit = case_when(unit=="kt" ~ "Ml",
                          TRUE ~ unit))


###########################################################
#2. Manual corrections based on qualitative information #########
###########################################################

## Correct SAU;NIC;JAM;HTI;MUS;TWN;TZA;IRN;FIN;GRC;LVA supply data : output = 0 across all years.

supply_biogasoline_full %<>% mutate(value = case_when(output == "Biogasoline" & iso3c %in% c("GRC","LVA","SAU","NIC","JAM","HTI","MUS","TWN","TZA","IRN") ~ 0,
                                                      output == "Biogasoline" & iso3c == "JPN" & year >= 2016 ~ 0,
                                                        TRUE ~ value))

to_complete <- supply_biogasoline_full %>%
  group_by(iso3c) %>%
  filter(any(is.na(value)) & !all(is.na(value)))
to_complete # only CRI with missing values for some years.


to_complete %<>%
  arrange(iso3c, year) %>%    
  group_by(iso3c) %>%
  fill(value, .direction = "up") %>%
  ungroup()

supply_biogasoline_full %<>% 
  rows_update(to_complete, by = c("iso3c","year","output"), unmatched = "ignore") %>%
  mutate(value = ifelse(is.na(value)==TRUE,0,value)) %>%
  filter(iso3c %in% regions$iso3c)
  
rm(to_complete)


###########################################################
#3. Writing the table #########
###########################################################

saveRDS(object = supply_biogasoline_full,
        file = "sup_biogasoline_initial.rds")




###########################################################
########### MAKING SUPPLY TABLE FOR BIODIESEL & RENEWABLE DIESEL #########
###########################################################

supply_fame_hvo <- subset(supply_intermediate4, output %in% c("Biodiesel","Renewable diesel","Biodiesel & Renewable diesel")) %>% 
  arrange(iso3c,output,year) %>% 
  filter(!(iso3c %in% c("ESP","FRA","GBR","ITA","NLD","USA") & output == "Biodiesel & Renewable diesel")) %>%
  ## Standardizing units to liters using density
  mutate(value = case_when(output == "Biodiesel" & unit == "kt" ~ 1.136 * value,
                           output == "Renewable diesel" & unit == "kt" ~ 1.282 * value,
                           TRUE ~ value),
         unit = case_when(output %in% c("Biodiesel","Renewable diesel") & unit == "kt" ~ "Ml",
                          TRUE ~unit)) %>%
  filter(iso3c %in% regions$iso3c)


###########################################################
#1. Assigning 0 output of Renewable diesel for all non-producing countries #########
###########################################################

years_countries_to_join <- expand_grid(
  iso3c = unique(regions$iso3c),
  output  = "Renewable diesel",
  process = "HVO",
  year = 2010:2022,
  value = 0,
  unit = "Ml"
)

years_countries_to_join %<>%
  anti_join(supply_fame_hvo %>% distinct(iso3c,output,year),
            by = c("iso3c", "output","year"))

supply_fame_hvo %<>% 
  bind_rows(years_countries_to_join)


###########################################################
#2. Assigning 0 output of Biodiesel for all non-producing countries #########
###########################################################

years_countries_to_join <- expand_grid(
  iso3c = unique(regions$iso3c),
  output  = "Biodiesel",
  process = "FAME",
  year = 2010:2022,
  value = 0,
  unit = "Ml"
)

years_countries_to_join %<>%
  anti_join(supply_fame_hvo %>% distinct(iso3c,output,year),
            by = c("iso3c", "output","year"))

supply_fame_hvo %<>% 
  bind_rows(years_countries_to_join)


###########################################################
#3. Correcting Biodiesel and Renewable diesel production values #########
###########################################################

# Set of countries for which we assume 0 Biodiesel production (before balancing)
supply_fame_hvo %<>% 
  mutate(value = if_else(iso3c %in% c("CYP","FIN","MLT","SVN","EGY","ETH","GEO","GTM","IRN","KAZ","MKD","NGA","NOR","NZL","PAK","SAU","VNM","ZAF") & output == "Biodiesel",
                         0,
                         value))

#Sweden
biodiesel_adj <- supply_fame_hvo %>%
  filter(
    iso3c == "SWE",
    output %in% c("Biodiesel", "Renewable diesel")
  ) %>%
  group_by(year, output) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = output,
    values_from = value,
    values_fill = 0
  ) %>%
  mutate(
    value_new = if_else(
      year <= 2017,
      Biodiesel,
      pmax(Biodiesel - `Renewable diesel`, 0)
    )
  ) %>%
  select(year, value_new)

supply_fame_hvo <- supply_fame_hvo %>%
  left_join(
    biodiesel_adj,
    by = "year"
  ) %>%
  mutate(
    value = if_else(
      iso3c == "SWE" & output == "Biodiesel" & !is.na(value_new),
      value_new,
      value
    )
  ) %>%
  select(-value_new)

#### Updating Renewable diesel supply by country for Finland, Netherlands and Singapore. 

neste_output_by_plant %<>%
  mutate(value = ifelse(iso3c == "FIN" & year >= 2015, 
                        value + 120, 
                        value)) %>%
  mutate(source = "Neste annual and sustainability reports")

supply_fame_hvo %<>% rows_update(neste_output_by_plant %>% rename(output = product),
                                 by = c("year","iso3c","output"),
                                 unmatched = "ignore") %>%
  filter(!(iso3c == "FIN" & output == "Biodiesel & Renewable diesel"))




############################################################
########### WRITING SUPPLY TABLE FOR BIODIESEL & RENEWABLE DIESEL #########
###########################################################

saveRDS(object = supply_fame_hvo,
        file = "sup_fame_hvo_initial.rds")




##############################################################################################
########### WRITING THE CLEANED DATA FOR CONSUMPTION VECTORS FOR BIOFUELS #########
##############################################################################################


##############################################################################################
#1. Final cleaning #########
##############################################################################################

y_iea <- supply_consumption_iea %>% filter(Flow %in% c("Total final consumption","Transport")) %>% select(-Flow)

y_oecd_fao %<>% select(REF_AREA,Commodity,Measure,Unit.of.measure,TIME_PERIOD,OBS_VALUE) %>%
  rename(iso3c = REF_AREA,
         data_type = Measure,
         output = Commodity,
         unit = Unit.of.measure,
         year = TIME_PERIOD,
         value = OBS_VALUE) %>%
  filter(year <= 2022) %>%
  select(-data_type) %>%
  mutate(unit = "Ml",
         output = case_when(output == "Ethanol" ~ "Biogasoline",
                            TRUE ~ output),
         iso3c = ifelse(iso3c=="EU","EU27",iso3c),
         source = "OECD-FAO Agricultural outlook") %>%
  filter(output %in% c("Biogasoline","Biodiesel")) %>%
  relocate(output, .after = "iso3c") %>%
  arrange(iso3c,output,year)

y_oecd_fao %<>% left_join(regions, by = "iso3c")

y_faostat <- subset(faostat_bioenergy, Element== "Energy consumption") %>% select(-Element)
y_eia <- subset(eia_prod_and_cons, flow=="consumption") %>% select(-flow)


##############################################################################################
#2. Saving #########
##############################################################################################

saveRDS(object = y_iea,
        file = "y_iea.rds")

saveRDS(object = y_oecd_fao,
        file = "y_oecd_fao.rds")

saveRDS(object = y_faostat,
        file = "y_faostat.rds")

saveRDS(object = y_eia,
        file = "y_eia.rds")





###### Remove temporary objects

rm(list = ls())

  