##########################################################################################################
################ Supply and use data collection #####################################################
##########################################################################################################


###########################################################
########### Loading packages #########
###########################################################
install.packages("rlang")
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

regions <- read.csv("inst/regions.csv") 

########### FAO data (non-food use) #########

setwd("/home/mmondolfo/fabio_bfp/fabio_data_local")

cbs_full <- readRDS("data/cbs_full.rds")

############# Supply data from own collection ##################

setwd("/home/mmondolfo/fabio_bfp/")

supply <- read_excel("own_data/Compilation_data_sources.xlsx",sheet="supply")
supply_selection <- read_excel("own_data/Compilation_data_sources.xlsx",sheet="supply_selection")
supply_pretreatment <- read_excel("own_data/Compilation_data_sources.xlsx",sheet="supply_pretreatment")

########### Use data from own collection #########

use <- read_excel("own_data/Compilation_data_sources.xlsx",sheet="use")
use_selection <- read_excel("own_data/Compilation_data_sources.xlsx",sheet="use_selection")
use_pretreatment <- read_excel("own_data/Compilation_data_sources.xlsx",sheet="use_pretreatment")
oecd_fao_use <- read.csv("own_data/oecd_fao_use_complete.csv")

############# TCFs ##################

tcf_table <- read_excel("own_data/tcf_table.xlsx")

############# Supply tables ##################

setwd("/home/mmondolfo/fabio_bfp/intermediate_data/")

supply_biogasoline <- readRDS("sup_biogasoline_initial.rds")
supply_fame_hvo <- readRDS("sup_fame_hvo_initial.rds")



###########################################################
########### MAKING FUNCTIONS #########
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
EU27_countries <- unique(regions$iso3c[regions$EU27==TRUE])
EU28_countries <- c(EU27_countries,"GBR")




###########################################################
########### CLEANING #########
###########################################################

###########################################################
#1. Supply from own collection #########
###########################################################

supply %<>%
  mutate(across(years, ~ as.numeric(as.character(.x))))

# Selecting variables of interest and removing rows out of the scope of this study
supply %<>% select(-c(TCF,...26,`2023`,`2024`)) %>% filter(!(process %in% c("Biogas","Biomethane")))
supply_selection <- supply_selection %>% select(-c(TCF,...26,`2023`,`2024`)) %>% filter(!(process %in% c("Biogas","Biomethane")))

id_cols  <- setdiff(names(supply_selection), years) 

rows_to_drop <- supply_selection %>%
  filter(if_all(all_of(years), ~ .x == "F")) %>%
  distinct(across(all_of(id_cols)))

supply %<>%
  anti_join(rows_to_drop, by = id_cols)

supply %<>% left_join(regions, by = c("country_iso3"="iso3c")) %>%
            mutate(EU28 = ifelse(EU27==TRUE|country_iso3=="GBR",TRUE,FALSE))

supply %<>% mutate(conversion_to_Ml = case_when(output=="Bioethanol" & unit=="kt" ~ 1.267,
                                          output=="Biodiesel" & unit=="kt" ~ 1.136,
                                          output=="Renewable diesel" & unit=="kt" ~ 1.282,
                                          output_subtype=="Used cooking oil" & unit=="kt" ~ 1.087),
                   across(`2010`:`2022`, ~ ifelse(is.na(conversion_to_Ml), .x, .x*conversion_to_Ml)),
                   unit=case_when((output %in% c("Bioethanol","Biodiesel","Renewable diesel") | output_subtype=="Used cooking oil") & unit=="kt" ~ "Ml",
                                  TRUE ~ unit))

supply_selection %<>%
  filter(!if_all(`2010`:`2022`, ~ .x == "F"))


###########################################################
#2. Use from own collection #########
###########################################################

use %<>%
  mutate(across(`2010`:`2022`, ~ as.numeric(as.character(.x)))) %>%
  select(-c(`2023`,`2024`))

use_selection %<>% select(-c(`2023`,`2024`)) %>% filter(!(process %in% c("Biogas","Biomethane")))

id_cols  <- setdiff(names(use_selection), years) 

rows_to_drop <- use_selection %>%
  filter(if_all(all_of(years), ~ .x == "F")) %>%
  distinct(across(all_of(id_cols)))

use %<>%
  anti_join(rows_to_drop, by = id_cols)

use %<>% left_join(regions, by = c("country_iso3"="iso3c")) %>%
  mutate(EU28 = ifelse(EU27==TRUE|country_iso3=="GBR",TRUE,FALSE))

use_selection %<>%
  filter(!if_all(`2010`:`2022`, ~ .x == "F"))


###########################################################
#3. OECD-FAO use data #########
###########################################################

oecd_fao_use %<>% select(REF_AREA,Commodity,Measure,Unit.of.measure,TIME_PERIOD,OBS_VALUE) %>%
  rename(iso3c = REF_AREA,
         input = Commodity,
         data_type = Measure,
         unit = Unit.of.measure,
         year = TIME_PERIOD,
         value = OBS_VALUE) %>%
  mutate(data_type = case_when(data_type=="Production" ~ "Supply",
                               data_type %in% c("Production d’éthanol à partir du produit","Production biodiesel à partir du produit") ~ "Supply from feedstock",
                               TRUE ~ NA),
         unit = case_when(data_type == "Supply" & input %in% c("Ethanol","Biodiesel") ~ "Ml",
                          data_type == "Supply from feedstock" ~ "Ml",
                          TRUE ~ NA),
         process = case_when(input %in% c("Biodiesel","Vegetable oils") ~ "Biodiesel & Renewable diesel",
                             TRUE ~ "Biogasoline"),
         input = case_when(input=="Ethanol" ~ "Biogasoline",
                           input=="Biodiesel" ~ "Biodiesel & Renewable diesel",
                           TRUE ~ input),
         iso3c = ifelse(iso3c=="EU","EU27",iso3c)) %>%
  filter(!(data_type == "Supply" & !(input%in% c("Biogasoline","Biodiesel & Renewable diesel"))),
         !is.na(data_type)) %>%
  relocate(process, .after = "input") %>%
  arrange(iso3c,year,process,data_type)

#### Harmonizing the items names ####
oecd_fao_use %<>% mutate(input = case_when(input == "Rice" ~ "Rice and products",
                                           input == "Wheat" ~ "Wheat and products",
                                           input == "Maize (corn)" ~ "Maize and products",
                                           input == "Molasses" ~ "Sweeteners, Other",
                                           input == "Roots and tubers" ~ "Cassava and products",
                                           TRUE ~ input))


## Calculating waste feedstock use as the residual

waste_rows <- oecd_fao_use %>%
  group_by(iso3c, process, year) %>%
  summarise(
    value = max(
      sum(value[data_type == "Supply"], na.rm = TRUE) - 
        sum(value[data_type == "Supply from feedstock"], na.rm = TRUE), 
      0
    ),
    .groups = "drop"
  ) %>%
  mutate(
    data_type = "Supply from feedstock",
    input = "Other, Unknown",
    unit = "Ml"
  )

oecd_fao_use <- bind_rows(oecd_fao_use, waste_rows) %>%
  arrange(iso3c, year, process, data_type) %>%
  filter(data_type=="Supply from feedstock") %>%
  left_join(regions, by="iso3c") %>%
  left_join(supply_biogasoline %>% select(iso3c,process,year,value) %>% rename(total_supply=value), by=c("iso3c","process","year"))

## Remove duplicate rows
oecd_fao_use %<>% 
  group_by(iso3c, year, process, data_type, input) %>%
  filter(!(n() > 1 & value == 0)) %>%  
  ungroup()


###########################################################
#4. FAO non-food use #########
###########################################################

fao_nonfood <- subset(cbs_full, !is.na(other) &
                        year >= 2010 &
                        (item %in% c("Wheat and products","Rice and products","Barley and products","Maize and products","Rye and products","Sorghum and products","Cereals, Other","Cassava and products","Sugar cane","Sugar beet","Sweeteners, Other",
                                     "Soyabean Oil","Sunflowerseed Oil","Rape and Mustard Oil","Cottonseed Oil","Palmkernel Oil","Palm Oil","Coconut Oil","Maize Germ Oil","Oilcrops Oil, Other","Fats, Animals, Raw",
                                     "Potatoes and products")))

fao_nonfood$use <- case_when(fao_nonfood$item %in% c("Wheat and products","Rice and products","Barley and products","Maize and products","Rye and products","Sorghum and products","Cereals, Other","Cassava and products","Sugar cane","Sugar beet","Sweeteners, Other") ~ "Bioethanol", 
                             fao_nonfood$item %in% c("Soyabean Oil","Sunflowerseed Oil","Rape and Mustard Oil","Cottonseed Oil","Palmkernel Oil","Palm Oil","Coconut Oil","Maize Germ Oil","Oilcrops Oil, Other","Fats, Animals, Raw") ~ "Biodiesel & Renewable diesel",
                             TRUE ~ NA)

fao_nonfood %<>% 
  group_by(item, area) %>%
  filter(other > 0) %>%
  ungroup() %>%
  group_by(year, area, use) %>%
  mutate(
    other = other / sum(other)
  ) %>%
  ungroup() %>%
  left_join(regions %>% select(name, iso3c), by = c("area" = "name"))

bioethanol_nonfood <- subset(fao_nonfood, use=="Bioethanol")
biodiesel_nonfood <- subset(fao_nonfood, use=="Biodiesel & Renewable diesel")


###########################################################
#5. TCFs #########
###########################################################

tcf_table <- tcf_table %>% mutate(across(c(input_qty, output_qty_max, output_qty), as.numeric)) %>% 
  mutate(output_qty = case_when(output_unit=="kl" ~ output_qty,
                                output_unit=="t"&input_unit=="t"&output=="Biodiesel" ~ 1.136*output_qty,
                                output_unit=="t"&input_unit=="t"&output=="Bioethanol" ~ 1.267*output_qty,
                                output_unit=="t"&input_unit=="t"&output=="Renewable diesel" ~ 1.282*output_qty,
                                TRUE ~ output_qty),
         output_qty_max = case_when(output_unit=="t" ~ output_qty_max,
                                    output_unit=="t"&input_unit=="t"&output=="Biodiesel" ~ 1.136*output_qty_max,
                                    output_unit=="t"&input_unit=="t"&output=="Bioethanol" ~ 1.267*output_qty_max,
                                    output_unit=="t"&input_unit=="t"&output=="Renewable diesel" ~ 1.282*output_qty_max,
                                    TRUE ~ output_qty_max),
         output_unit = case_when(output_unit=="t"&input_unit=="t"&(output %in% c("Biodiesel","Bioethanol","Renewable diesel")) ~ "kl",
                                 TRUE ~ output_unit),
         multiplier_output_kl_to_input_t = case_when(output_unit=="kl"&input_unit=="t" ~ input_qty/output_qty,
                                                     TRUE ~ NA))





###########################################################
########### PRETREATMENT OF SUPPLY ROWS (those not matching required format) #########
###########################################################

supply_match <- supply %>%
  semi_join(
    supply_pretreatment,
    by = c("process", "input", "output", "country_iso3")
  )

########### Case 1 (row 1) ########### 

case1_solution <- subset(bioethanol_nonfood, year %in% 2018:2022 & area=="Germany" & !(item %in% c("Wheat and products","Maize and products","Sweeteners, Other","Sugar (Raw Equivalent)","Cassava and products")))
## For Germany we can assume "Cereals, Other" is Triticale (looking at SUA)

case1_solution %<>% 
  group_by(year) %>%
  mutate(other = other/sum(other),
         item = item) %>%
  ungroup()

row1 <- supply_match[1, as.character(2018:2022), drop = FALSE]

row1_long <- row1 %>%
  pivot_longer(
    cols = everything(),
    names_to  = "year",
    values_to = "other"
  ) %>%
  mutate(year = as.numeric(year))

case1_solution %<>%
  left_join(row1_long, by = "year", suffix = c("", "_coef")) %>%
  mutate(other = other * other_coef) %>%
  select(-other_coef) 

## Pivot wider to match the structure of our supply dataset ##

case1_solution %<>%
  pivot_wider(
    id_cols   = item,
    names_from  = year,
    values_from = other
  ) %>%
  mutate(across(`2018`:`2022`, ~ ifelse(is.na(.x), 0, .x)))

## Matching the structure of our supply dataset
row1 <- supply_match[1, ]

cols_to_copy <- setdiff(
  names(row1),
  c("input", as.character(2018:2022))
)

case1_solution[cols_to_copy] <- row1[rep(1, nrow(case1_solution)), cols_to_copy]
case1_solution <- case1_solution %>% mutate(`Source(s)`="Estimation derived from VDB Politikinformation Biokraftstoffe",
                          input_subtype=NA) %>% rename(input = item)

## Remowing the row that was modified and adding the new rows ##

row1_keys <- supply_match[1, c("process", "input", "output", "country_iso3")]

supply %<>%
  anti_join(row1_keys,
            by = c("process", "input", "output", "country_iso3")) %>%
  bind_rows(case1_solution)



########### Case 2 (row 2) ########### 

row2 <- supply_match[2, as.character(2011:2017), drop = FALSE]
row2_long <- row2 %>%
  pivot_longer(
    cols = everything(),
    names_to  = "year",
    values_to = "other"
  ) %>%
  mutate(year = as.numeric(year))

case2_solution <- subset(bioethanol_nonfood, year %in% 2011:2017 & area=="Germany" & !(item %in% c("Sweeteners, Other","Sugar (Raw Equivalent)","Cassava and products")))
case2_solution %<>% group_by(year) %>% mutate(other = other/sum(other),
                                                 item = item) %>% ungroup()

case2_solution %<>%
  complete(year = 2011:2017, item) %>%
  mutate(other = case_when(
    year == 2016 ~ other,
    is.na(other) ~ 0,
    TRUE ~ other
  )) %>%
  group_by(item) %>%
  arrange(year) %>%
  mutate(other = na.approx(other, x = year, na.rm = FALSE)) %>% 
  ungroup() %>%
  left_join(row2_long, by = "year", suffix = c("", "_coef")) %>%
  mutate(other = other * other_coef) %>%
  select(-other_coef)

case2_solution %<>%
  pivot_wider(
    id_cols   = item,
    names_from  = year,
    values_from = other
  )

case2_complete <- subset(supply, process=="Bioethanol"&country_iso3=="DEU")[c(1,2,5:8),]
case2_complete %<>%
  select(-(`2011`:`2017`)) %>%
  left_join(case2_solution, by = c("input"="item")) %>%
  relocate((`2011`:`2017`), .before = `2018`) %>%
  mutate(across(as.character(2011:2017), ~ ifelse(is.na(.x), 0, .x)))

row2_keys <- supply_match[2, c("process", "input", "output", "country_iso3")]

supply %<>%
  anti_join(row2_keys,
            by = c("process", "input", "output", "country_iso3")) %>%
  rows_update(
    case2_complete,
    by = c("process", "input", "country_iso3")
  ) %>%
  mutate(input_subtype = ifelse(country_iso3=="DEU"&input=="Cereals, Other", "Triticale",input_subtype))




###########################################################
########### PRETREATMENT OF USE ROWS (those not matching required format) #########
###########################################################

use_match <- use %>%
  semi_join(
    use_pretreatment,
    by = c("process", "input", "input_subtype", "country_iso3")
  )


############### Case 1 (rows 1-2) ################

case1a <- use_match[1,]
case1b <- use_match[2,]
case1b %<>% pivot_longer(cols = all_of(years),
                         names_to  = "year",
                         values_to = "total_use") %>%
            mutate(year = as.integer(year))

case1a_solution <- subset(bioethanol_nonfood, area=="Canada"&!(item%in% c("Maize and products","Sweeteners, Other","Cassava and products","Sugar cane")))
# Given these inconsistent results and the fact that wheat is listed as the primary cereal in this case, we assume 100% of this use category is Wheat and products.
case1a %<>% mutate(input = "Wheat and products")

case1b_solution <- subset(biodiesel_nonfood, area=="Canada"&item%in%c("Rape and Mustard Oil","Soyabean Oil"))
case1b_solution %<>% group_by(year) %>% mutate(other = other/sum(other)) 
case1b_solution %<>% left_join(case1b %>% select(year,total_use), by="year") %>%
                     mutate(use=other*total_use)

case1b_solution %<>%
  select(year, item, use) %>%        
  mutate(year = as.character(year)) %>%        
  pivot_wider(
    names_from  = year,
    values_from = use                
  )

cols_to_copy <- setdiff(
  names(case1b),
  c("input", years)
)

case1b_solution[cols_to_copy] <- case1b[rep(1, nrow(case1b_solution)), cols_to_copy]
case1b_solution %<>% mutate(`Source(s)`="Estimation derived from USDA Biofuels annual") %>% rename(input = item) %>%select(-c(total_use,year))

## Merging 

case1a_keys <- use_match[1, c("process", "input", "country_iso3")]
case1b_keys <- use_match[2, c("process", "input", "country_iso3")]

use %<>% anti_join(case1a_keys, by = c("process", "input", "country_iso3")) %>%
         anti_join(case1b_keys, by = c("process", "input", "country_iso3")) %>%
         bind_rows(case1a) %>%
         bind_rows(case1b_solution)

############### Case 2 ################

case2 <- use_match[8:14,]
case2_support <- subset(use, country_iso3=="PRT"&process=="FAME")
case2_reference_supply <- subset(supply, country_iso3=="PRT"&process%in%c("FAME","HVO"))

# Assumption : HVO is 100% made from UCO. Thus all feedstocks other than UCO are 100% used for FAME. 

case2$process[2:7] <- rep("FAME",6)

case2_total_supply_PRT <- subset(supply_fame_hvo, iso3c == "PRT" & year >= 2017)
case2_total_supply_PRT %<>% mutate(multiplier_output_kl_to_input_t = ifelse(output=="Renewable diesel", (1/1.053), NA),
                             uco_use = ifelse(output== "Renewable diesel", multiplier_output_kl_to_input_t*value, NA))

case2_hvo_use <- case2_total_supply_PRT %>% filter(process == "HVO") %>% select(process,uco_use,year)

case2_hvo_use %<>% 
  pivot_wider(
  id_cols   = process,
  names_from  = year,
  values_from = uco_use
) 

case2 %<>% bind_rows(case2_hvo_use)
cols <- paste0(2017:2022)
case2[1, cols] <- case2[1, cols] - case2[7, cols]

case2 %<>% mutate(
  process = ifelse(process == "FAME & HVO", "FAME", process),
  input = ifelse(process == "HVO", "Inedible animal or vegetable fats and oils", input),
  input_subtype = ifelse(process == "HVO", "Used cooking oil", input_subtype),
  data_type = "Use",
  country_iso3 = "PRT",
  unit = "kt"
  )


## Deleting redundant rows and adding new ones

case2_keys <- use_match[8:14,c("process", "input","country_iso3")]

use %<>% anti_join(case2_keys, by = c("process", "input", "country_iso3")) %>%
  bind_rows(case2) 

use <- bind_rows(
  use %>% 
    filter(country_iso3 != "PRT"),
    use %>% 
    filter(country_iso3 == "PRT") %>%
    group_by(process, input, input_subtype, country_iso3) %>%
    summarise(
      across(all_of(years), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  )

use %<>% 
  mutate(
    data_type = if_else(
      country_iso3 == "PRT" & is.na(data_type),
      "Use",
      data_type),
    unit = if_else(
      country_iso3 == "PRT" & is.na(unit),
      "kt",
      unit),
    `Source(s)` = if_else(
      country_iso3 == "PRT" & is.na(`Source(s)`),
      "Laboratorio Nacional de Energia e Geologia",
      `Source(s)`),
    across(`2010`:`2011`, ~ if_else(
      country_iso3 == "PRT" & process == "FAME",
      NA, .x))
  )


always_keep_keys <- bind_rows(
  case1a %>% select(process, input, input_subtype, country_iso3, data_type),
  case1b_solution %>% select(process, input, input_subtype, country_iso3, data_type),
  case2 %>% select(process, input, input_subtype, country_iso3, data_type)
) %>%
  distinct()

use %<>%  left_join(
  always_keep_keys %>% mutate(always_keep = TRUE),
  by = c("process", "input", "input_subtype", "country_iso3", "data_type")
) %>%
  mutate(always_keep = coalesce(always_keep, FALSE))

## Removing temporary objects from the environment ##

rm(list = ls(pattern = "^(case|row)"), intermediate_gap, uco_to_biodiesel,use_match,use_pretreatment,supply_match,supply_pretreatment)




###########################################################
########### PIVOT TO LONG FORMAT AND JOIN THE SELECTION BOOLEAN #########
###########################################################

use_long <- use %>% 
  pivot_df()

use_selection_long <- use_selection %>%
  pivot_longer(
    cols = all_of(years),
    names_to = "year",
    names_transform = list(year = as.integer),
    values_to = "selection"
  )  

use_long %<>%
  left_join(
    use_selection_long,
    by = c(
      "data_type",
      "year",
      "process",
      "input",
      "input_subtype",
      "country_iso3",
      "Source(s)",
      "unit"
    )
  ) %>%
  filter(always_keep | is.na(selection) | selection == "T") %>%
  select(-selection) %>%
  mutate(value = case_when(unit == "million lbs" ~ 0.45359237*value,
                           unit == "Ml" & process == "FAME" ~ (1/1.087)*value,
                           input == "Cassava and products" & input_subtype == "Dried chips" ~ (1/0.45)*value, # assuming a 45% yield of dried chips from the root. Source : Howeler, R. H. (2004). Cassava in Asia: present situation and its future potential in agro-industry.
                           TRUE ~ value), # using USDA assumption on vegetable oil density. 
         unit = case_when(unit == "million lbs" | (unit == "Ml" & process == "FAME") ~ "kt",
                          TRUE ~ unit),
         input_subtype = case_when(input_subtype == "Dried chips" ~ NA,
                                   TRUE ~ input_subtype)
         )

rm(use_selection,use_selection_long,id_cols,rows_to_drop,supply_pretreatment,use_pretreatment)




###########################################################
########### CONVERSION OF SUPPLY-PER-FEEDSTOCK TO FEEDSTOCK USE #########
###########################################################

supply_feedstock <- supply %>% filter(input!="Total"&(!(output %in% c("Inedible animal or vegetable fats and oils")&(!country_iso3=="EU27"))))

###########################################################
#1. Joining TCFs #########
###########################################################


triplets_tcf <- tcf_table %>%
  distinct(input, input_subtype, output)

triplets_supply <- supply_feedstock %>%
  distinct(input, input_subtype, output)


## Checking duplicates and missing factors

missing_triplets <- triplets_supply %>%
  anti_join(triplets_tcf,
            by = c("input", "input_subtype", "output"))

duplicate_conversion_factors <- tcf_table %>%
  group_by(input, input_subtype, output) %>%
  filter(n() > 1) %>%
  ungroup()

supply_feedstock <- supply_feedstock %>% left_join(tcf_table %>% select(input,input_subtype,output,multiplier_output_kl_to_input_t), 
                                               by = c("input", "input_subtype", "output"))




###########################################################
########### SAVING CLEANED TABLES #########
###########################################################

setwd("/home/mmondolfo/fabio_bfp/intermediate_data/")

saveRDS(supply_feedstock,
        "supply_feedstock.rds")

saveRDS(use_long,
        "use_long.rds")

saveRDS(tcf_table,
        "tcf_table_clean.rds")

saveRDS(oecd_fao_use,
        "oecd_fao_use_clean.rds")

saveRDS(fao_nonfood,
        "fao_nonfood.rds")

rm(list = ls())