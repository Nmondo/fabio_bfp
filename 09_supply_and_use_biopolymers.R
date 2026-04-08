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

setwd("/home/mmondolfo/fabio_bfp/")

########### FABIO regions #########

regions <- read.csv("inst/regions.csv") 
items_supply_bcp <- read.csv("inst/items_supply_bcp.csv") 

########### FAO data (non-food use) #########

setwd("/home/mmondolfo/fabio_bfp/fabio_data_local")

cbs_full <- readRDS("data/cbs_full.rds")
tcf_sua_final <- readRDS("data/sua/tcf_sua_final.rds")


############# Supply data from own collection ##################

#### Here unit is always kilotonnes unless specified. 

setwd("/home/mmondolfo/fabio_bfp/")

capacities_bb_bp <- read_excel("own_data/Supply_BB_BP_report.xlsx",sheet="filter")
output_bp <- read_excel("own_data/Supply_BB_BP_report.xlsx",sheet="capacities_and_production")
supply_final_bf_sua <- readRDS("inputs_for_final_data/supply_final_bf_sua.rds")

btd_intermediate_other <- readRDS("intermediate_data/btd_intermediate_other_step1.rds")

########### Technical conversion factors #########

tcf_table <- readRDS("intermediate_data/tcf_table_clean.rds")  



######################################################################################################################
######### CLEANING AND CALCULATING TCFs STARCH / SUGAR EXTRACTION FROM CROPS #########
######################################################################################################################

######################################################################################################################
#1. Cleaning TCFs (crop from starch and starch to sugar) #########
######################################################################################################################

tcf_starch <- tcf_sua_final %>% 
  filter(grepl("Starch", child),
         !grepl("Flour|Sweet potatoes",parent)) %>%
  rename(tcf = extraction_rate) %>%
  mutate(parent = case_when(parent == "Cassava, fresh"          ~ "Cassava and products",
                            parent == "Maize (corn)"            ~ "Maize and products",
                            parent == "Potatoes"                ~ "Potatoes and products",
                            parent == "Rice, broken"            ~ "Rice and products",
                            parent == "Wheat and meslin flour"  ~ "Wheat and products",
                            TRUE                                ~ parent))

tcf_sugar <- tcf_sua_final %>% 
  filter(grepl("Raw cane or beet sugar|Glucose", child),
         !grepl("Malt",parent)) %>%
  mutate(moisture = case_when(grepl("Glucose",child) ~ 0.12,
                              TRUE ~ 0),
         child = "Sugar",
         tcf = (1-moisture)*extraction_rate) %>%
  select(-moisture, -extraction_rate)


## Making the TCF table
tcf_crop_bp <- bind_rows(tcf_starch, tcf_sugar)


######################################################################################################################
#2. Computing world averages for imputation #########
######################################################################################################################

world_avg_tcf <- tcf_crop_bp %>%
  group_by(parent, child) %>%
  summarise(tcf = mean(tcf, na.rm = TRUE), .groups = "drop")


######################################################################################################################
#3. Computation of direct Crop-to-sugar conversion factor #########
######################################################################################################################

tcf_starch_crop_to_sugar <- tcf_starch %>%
  left_join(
    tcf_sugar %>%
      select(parent, area, starch_to_sugar_tcf = tcf),
    by = c("child" = "parent", "area")
  ) %>%
  # imputation from world average for missing joins
  rows_patch(
    world_avg_tcf %>%
      rename(starch_to_sugar_tcf = tcf),
    by = c("parent", "child"),
    unmatched = "ignore"
  ) %>%
  mutate(tcf = tcf*starch_to_sugar_tcf,
         child = "Sugar") %>%
  select(-starch_to_sugar_tcf)

## Updating the TCF table

tcf_crop_bp <- bind_rows(tcf_crop_bp, tcf_starch_crop_to_sugar) %>%
  select(-parent_code, -proc, -proc_code, -child_code, -min, -max) %>%
  #Removing starch-to-sugar because we have crop-to-sugar
  filter(
    !((grepl("^Starch", parent) & grepl("Sugar", child)))
  )


######################################################################################################################
#4. Adding world average crop-to-sugar conversion factors #########
######################################################################################################################

world_avg_tcf <- bind_rows(world_avg_tcf,
                           tcf_starch_crop_to_sugar %>% 
                             group_by(parent, child) %>%
                             summarise(tcf = mean(tcf, na.rm = TRUE), .groups = "drop")) %>%
  #Removing starch-to-sugar because we have crop-to-sugar
  filter(
    !((grepl("^Starch", parent) & grepl("Sugar", child)))
  )


######################################################################################################################
#5. Adding world average crop-to-sugar conversion factors #########
######################################################################################################################

## Making a full table 
tcf_crop_bp <- tcf_crop_bp %>%
  distinct(parent, child) %>%
  crossing(area_code = regions$code) %>%
  left_join(tcf_crop_bp, by = c("parent", "child", "area_code")) %>%
  rows_update(regions %>% select(area = name, area_code = code), by = "area_code") %>%
  left_join(regions %>% select(area_code = code, iso3c), by = "area_code") %>%
  select(-area_code, -area)

## Joining missing values
tcf_crop_bp <- tcf_crop_bp %>% rows_patch(world_avg_tcf, by = c("parent","child"))

## Names to "Starch" when contain "Starch"
tcf_crop_bp <- tcf_crop_bp %>%
  mutate(child = ifelse(grepl("^Starch", child), "Starch", child),
         crop_requirement_multiplier = ifelse(is.na(tcf), NA, 1/tcf)) # Multiplier on qty of sugar or starch used that gives the crop qty required to meet this, in each country

# This is not applicable directly because we don't know the origins of the feedstock mix without taking into account trade flows.
# We will take world averages by default.




######################################################################################################################
######### CALCULATING SHARES IN NON-FOOD USE OF STARCH / SUGAR CROPS #########
######################################################################################################################

########### List of "Starch" and "Sugar" (incl. sugar from starch) crops, and corresponding subsets of fao_nonfood #########

sugar_crops <- c("Wheat and products","Rice and products","Maize and products","Cassava and products","Potatoes and products","Sugar cane","Sugar beet")
starch_crops <- c("Wheat and products","Rice and products","Maize and products","Cassava and products","Potatoes and products")

fao_nonfood <- subset(cbs_full, !is.na(other) &
                        year >= 2010 &
                        (item %in% c("Wheat and products","Rice and products","Maize and products","Cassava and products","Sugar cane","Sugar beet","Potatoes and products")))

nonfood_sugar <- subset(fao_nonfood, item %in% sugar_crops)
nonfood_starch <- subset(fao_nonfood, item %in% starch_crops)

########### Cleaning function #########

fao_other_share <- function(df) {
  df <- df %>%
    group_by(item, area) %>%
    filter(other > 0) %>%
    ungroup() %>%
    group_by(year,area) %>%
    mutate(other = other / sum(other)) %>%
    ungroup() %>%
    left_join(regions %>% select(name, iso3c), by = c("area" = "name")) %>%
    select(iso3c,year,item,other) %>%
    arrange(iso3c,year,item)
}

nonfood_sugar <- fao_other_share(nonfood_sugar)
nonfood_starch <- fao_other_share(nonfood_starch)




###########################################################
########### CLEANING #########
###########################################################

###########################################################
#1. Cleaning production and capacities time series #########
###########################################################

clean_output_bp <- function(df) {
  
  first_col <- names(df)[1]
  all_range  <- names(df %>% select(`...17`:`2024...31`))
  year_cols  <- all_range[-1]   # drop ...17, keep only actual year columns
  
  seg1 <- df[2:45,  ] %>% mutate(seq_row = row_number())
  seg2 <- df[47:74, ] %>% mutate(seq_row = row_number())
  
  assign_position <- function(x) {
    x %>% mutate(
      position = ((seq_row - 1) %% 4) + 1,
      block = ceiling(seq_row / 4) + if_else(
        identical(x, seg2), nrow(seg1) / 4, 0
      )
    )
  }
  
  sub_df <- bind_rows(seg1, seg2) %>%
    mutate(
      position = ((seq_row - 1) %% 4) + 1,
      block    = cumsum(position == 1)   # increment block counter at every position-1 row
    ) %>%
    mutate(across(all_of(year_cols), as.numeric))
  
  products <- sub_df %>%
    filter(position == 1) %>%
    select(block, product = !!sym(first_col))
  
  data_rows <- sub_df %>%
    filter(position %in% c(3, 4)) %>%
    mutate(variable = case_when(
      position == 3 ~ "ratio_updated_to_initial_cap",
      position == 4 ~ "ratio_prod_to_updated_cap"
    )) %>%
    select(block, variable, all_of(year_cols))
  
  result <- data_rows %>%
    left_join(products, by = "block") %>%
    pivot_longer(
      cols      = all_of(year_cols),
      names_to  = "period",
      values_to = "value"
    ) %>%
    mutate(year = as.integer(str_extract(period, "^\\d{4}"))) %>%
    select(product, variable, period, year, value) %>%
    arrange(product, variable, year)
  
  return(result)
}

long_df <- clean_output_bp(output_bp) %>%
  select(-period) %>%
  pivot_wider(
    names_from  = variable,
    values_from = value
  )


###########################################################
#2. Cleaning production time series with regional distribution #########
###########################################################

clean_output_bp_regions <- function(df) {
  
  label_col <- "...33"
  all_range  <- names(df %>% select(`...33`:`...47`))
  year_cols  <- all_range[-1]   # drop ...33
  
  year_map <- setNames(2011:2024, year_cols)
  
  subtables <- list(
    c(79, 86),
    c(90, 98),
    c(102, 110)
  )
  
  result <- purrr::map_dfr(subtables, function(rng) {
    
    rows  <- rng[1]:rng[2]
    block <- df[rows, ]
    
    product <- as.character(block[[label_col]][1])
    
    data_block <- block[3:nrow(block), ] %>%
      mutate(region = as.character(.data[[label_col]]),
             across(all_of(year_cols), as.numeric)) %>%
      select(region, all_of(year_cols))
    
    data_block %>%
      pivot_longer(
        cols      = all_of(year_cols),
        names_to  = "col",
        values_to = "value"
      ) %>%
      mutate(
        product = product,
        year    = year_map[col]
      ) %>%
      select(product, region, year, value)
  })
  
  result %>% arrange(product, region, year)
}

long_df_regions <- clean_output_bp_regions(output_bp)


###########################################################
#3. Cleaning capacities-by-country time series #########
###########################################################

capacities_bb_bp %<>%
  select(-`2029_forecast`, -`...25`) %>%
  mutate(across(`2011`:`2024`, ~ as.numeric(ifelse(.x=="NA", 0, .x)))) %>% 
  pivot_longer(
    cols            = `2011`:`2024`,
    names_to        = "year",
    names_transform = list(year = as.integer),
    values_to       = "capacity"
  ) %>%
  rename(product = product_name,
         iso3c = country) %>%
  mutate(across(c(product,input), ~ 
                  case_when(grepl("PBS", .x) ~ "PBS",
                            grepl("^SA$", .x) ~ "Succinic acid", 
                            TRUE ~ .x)),
         across(c(feedstock_details,feedstock_category), ~ ifelse(.x=="Sugars", "Sugar", .x))
  ) %>%
  filter(product != "CP")


###########################################################
#4. Vector for Braskem PE supply and use #########
###########################################################

pe_supply_braskem <- data.frame(
  year    = rep(2016:2024, each = 2),
  rows    = rep(c("domestic", "export"), times = 9),
  product = "PE",
  unit    = "kt",
  iso3c   = "BRA",
  value   = c(
    71575, 48406,
    66401, 67843,
    58719, 74439,
    57616, 103808,
    38007, 129585,
    26378, 138603,
    27529, 151009,
    16906, 138067,
    19481, 171203
  ) / 1000
) 

pe_supply_braskem <- pe_supply_braskem %>% 
  bind_rows(
    pe_supply_braskem %>% 
      group_by(year, product, unit) %>% 
      summarise(value = sum(value), .groups = "drop") %>% 
      mutate(rows = "output",
             iso3c = "BRA")
  ) %>% 
  arrange(year, rows) 




###########################################################
#4. Computing relative shares of PA11 capacities for 11-AA use #########
###########################################################

share_11aa <- data.frame(
  year  = rep(2011:2024, each = 2),
  iso3c = rep(c("SGP", "FRA"), times = 14),
  share = c(
    0,  18,
    0,  18,
    0,  18,
    0,  18,
    0,  18,
    0,  18,
    0,  18,
    0,  18,
    0,  18,
    0,  18,
    0,  18,
    15, 18,
    15, 18,
    15, 18
  ) / rep(c(0,0,0,0,0,0,0,0,0,0,0,15,15,15) +
            c(18,18,18,18,18,18,18,18,18,18,18,18,18,18), each = 2)
)




###########################################################
########### MAKING THE SUPPLY TABLE #########
###########################################################

###########################################################
#1. Joining production estimates and updated capacities ratios #########
###########################################################

prod_bb_bp <- capacities_bb_bp %>%
  left_join(long_df, by = c("product","year")) %>%
  mutate(est_prod = capacity * ratio_updated_to_initial_cap * ratio_prod_to_updated_cap) %>%
  filter(product != "SCPC",
         feedstock_category != "industrial monocarboxylic fatty acids; acid oils from refining" | is.na(feedstock_category))


###########################################################
#2. Making the supply table #########
###########################################################

supply_bb_bp <- prod_bb_bp %>%
  select(product, iso3c, year, est_prod) %>%
  rename(supply = est_prod) %>%
  filter(!(product %in% c("PTT","PA")))


###########################################################
#3. Formatting : country-year-product sums #########
###########################################################

supply_bb_bp <- supply_bb_bp %>%
  group_by(iso3c,product,year) %>%
  summarize(supply = sum(supply, na.rm=TRUE))



###########################################################
#4. Updating output for Brazil & Germany (Ethylene and PE)  #########
###########################################################

# Joining empirical production for Braskem PE (2016-2024)
supply_bb_bp <- supply_bb_bp %>%
  rows_update(
    pe_supply_braskem %>%
      filter(rows == "output") %>%
      select(product, iso3c, year, supply = value),
    by = c("product", "iso3c", "year")
  )

# Update estimated ethylene production (based on TCF for BRA/DEU, MEG feedstock for IND)
supply_bb_bp <- supply_bb_bp %>%
  left_join(
    supply_bb_bp %>%
      ungroup() %>%
      filter(iso3c %in% c("BRA", "DEU"), product == "PE") %>%
      select(year, iso3c, supply_PE = supply),
    by = c("year", "iso3c")
  ) %>%
  left_join(
    supply_bb_bp %>%
      ungroup() %>%
      filter(iso3c == "IND", product == "MEG") %>%
      select(year, supply_MEG = supply),
    by = "year"
  ) %>%
  mutate(
    supply = case_when(
      iso3c %in% c("BRA", "DEU") & product == "Ethylene" ~ 1.02 * supply_PE,
      iso3c == "IND"             & product == "Ethylene" ~ supply_MEG / 1.54,
      TRUE                                               ~ supply
    )
  ) %>%
  select(-supply_PE, -supply_MEG)

# Update prod to match
prod_bb_bp <- prod_bb_bp %>%
  left_join(
    supply_bb_bp %>%
      ungroup() %>%
      filter(iso3c == "BRA", product == "PE") %>%
      select(year, supply_PE = supply),
    by = "year"
  ) %>%
  left_join(
    supply_bb_bp %>%
      ungroup() %>%
      filter(iso3c %in% c("BRA", "DEU", "IND"), product == "Ethylene") %>%
      select(year, iso3c, supply_Ethylene = supply),
    by = c("year", "iso3c")
  ) %>%
  mutate(
    est_prod = case_when(
      iso3c == "BRA" & product == "PE"                         ~ supply_PE,
      iso3c %in% c("BRA", "DEU", "IND") & product == "Ethylene" ~ supply_Ethylene,
      TRUE                                                      ~ est_prod
    )
  ) %>%
  select(-supply_PE, -supply_Ethylene)



###########################################################
########### INITIATING THE USE TABLE #########
###########################################################

###########################################################
#1. Joining TCF #########
###########################################################

tcf_lookup <- tcf_table %>%
  select(input, output, output_qty) %>%
  rename(product = output)

# Three joins at decreasing levels of granularity
join1 <- prod_bb_bp %>%
  left_join(tcf_lookup %>% rename(specific_tcf_1 = output_qty),
            by = c("product", "input"))

join2 <- join1 %>%
  left_join(tcf_lookup %>% rename(feedstock_details = input,
                                  specific_tcf_2    = output_qty),
            by = c("product", "feedstock_details"))

join3 <- join2 %>%
  left_join(tcf_lookup %>% rename(feedstock_category = input,
                                  specific_tcf_3     = output_qty),
            by = c("product", "feedstock_category"))

# Take first non-NA across the three attempts
use_bb_bp <- join3 %>%
  mutate(specific_tcf = coalesce(specific_tcf, specific_tcf_1, specific_tcf_2, specific_tcf_3)) %>%
  select(-specific_tcf_1, -specific_tcf_2, -specific_tcf_3)


###########################################################
#2. Estimating feedstock use #########
###########################################################

use_bb_bp <- use_bb_bp %>%
  mutate(use = est_prod*(1/specific_tcf))




###########################################################
########### COLLECTING SUPPLY CHAIN INFO FOR BILATERAL TRADE FLOWS #########
###########################################################

supplychain_bb_bp <- use_bb_bp %>%
  select(product,chain,step, input, iso3c,year, est_prod, use) %>%
  filter(!is.na(chain)) %>%
  arrange(chain,year,step)

###########################################################
#1. Disaggregating rows when multiple chains involved #########
###########################################################

supplychain_bb_bp <- supplychain_bb_bp %>%
  rowwise() %>%
  mutate(
    chain_split = list(str_trim(str_split(chain, ";")[[1]])),
    step_split  = list(str_trim(str_split(step,  ";")[[1]]))
  ) %>%
  mutate(
    chain_out = list(if (length(chain_split) == 1) rep(chain_split, length(step_split)) else chain_split),
    step_out  = list(if (length(step_split)  == 1) rep(step_split,  length(chain_split)) else step_split)
  ) %>%
  ungroup() %>%
  select(-chain, -step, -chain_split, -step_split) %>%
  unnest(cols = c(chain_out, step_out)) %>%   # paired positional unnest
  rename(chain = chain_out, step = step_out) %>%
  arrange(chain, year, step)

supplychain_bb_bp <- supplychain_bb_bp %>%
  # Step 2: base rows (imports/use side)
  filter(step == 2) %>%
  select(chain, product, input, iso3c, year, use) %>%
  rename(importer_iso3 = iso3c) %>%
  # Join step 1 (exports/supply side) by chain + year
  left_join(
    supplychain_bb_bp %>%
      filter(step == 1) %>%
      select(chain, iso3c, year, est_prod) %>%
      rename(exporter_iso3 = iso3c,
             qty_available  = est_prod),
    by = c("chain", "year")
  ) %>%
  mutate(exporter_iso3 = ifelse(input %in% c("Bionaphtha", "Biopropane"), "NLD", exporter_iso3)) %>%
  rows_update(supply_final_bf_sua %>% 
                select(iso3c, year, supply, product) %>%
                rename(input = product,
                       qty_available = supply, 
                       exporter_iso3 = iso3c),
              by = c("exporter_iso3", "input", "year"),
              unmatched = "ignore") %>%
  mutate(value = pmin(use, qty_available)) %>%
  select(chain, product, input, exporter_iso3, importer_iso3, year, qty_available, use, value) %>%
  arrange(chain, year, exporter_iso3, importer_iso3)


###########################################################
#2. Extracting the set of flows #########
###########################################################

# Flows to keep
flows <- supplychain_bb_bp %>% filter(!is.na(value))

# Flows that still have to be estimated (output unknown)
flows_to_estimate <- supplychain_bb_bp %>% 
  filter(is.na(value),
         year %in% 2012:2022) %>% 
  filter(input!="11-AA")

flows_to_estimate <- flows_to_estimate %>%
  # Making sure to avoid duplicates
  group_by(product, input, exporter_iso3, importer_iso3, year, qty_available) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  #Sum by year of estimated flows
  group_by(product, input, exporter_iso3, importer_iso3, year) %>%
  summarise(value = sum(qty_available), .groups = "drop")

flows <- bind_rows(flows, flows_to_estimate) %>%
  select(-qty_available, -use, -chain) %>% 
  group_by(product, input, exporter_iso3, importer_iso3, year) %>%
  summarise(value = sum(value), .groups = "drop")



###########################################################
#3. Joining estimates for 11-AA -to- PA11 supply chain #########
###########################################################

supplychain_11aa <- share_11aa %>%
  left_join(
    supply_bb_bp %>%
      ungroup() %>%
      filter(iso3c == "FRA", product == "11-AA") %>%
      select(year, supply),
    by = "year"
  ) %>%
  mutate(
    exporter_iso3 = "FRA",
    importer_iso3 = iso3c,
    value         = share * supply,
    input       = "11-AA",
    product     = "PA",
  ) %>%
  select(year, exporter_iso3, importer_iso3, input, product, value) %>%
  filter(year <= 2022)

# Joining
flows <- bind_rows(flows, supplychain_11aa) %>%
  mutate(unit = "kt")

#Calculating total uses (to patch when missing info in "use_bb_bp_intermediate")
total_use_est <- flows %>% 
  group_by(importer_iso3, product, input, year) %>%
  summarise(value = sum(value),
            unit = "kt", 
            .groups = "drop")

#Separating trade flows VS "self-consumption"
btd_bb_bp <- flows %>% filter(exporter_iso3 != importer_iso3)
self_flows_bb_bp <- flows %>% filter(exporter_iso3 == importer_iso3) %>%
  select(-exporter_iso3) %>% 
  rename(iso3c = importer_iso3)




###########################################################
########### COMPLETING USE TABLE #########
###########################################################

use_bb_bp <- use_bb_bp %>%
  select(product, input, feedstock_details, feedstock_category, iso3c, year, use)




######################################################################################################################
#1. Joining shares in nonfood use when no feedstock detail #########
######################################################################################################################

# Selecting rows for which we need to assume feedstocks (based on "Sugar" or "Starch" category)

use_to_estimate <- use_bb_bp %>% filter(!is.na(feedstock_category))




use_to_estimate <- bind_rows(
  left_join(use_to_estimate %>% filter(feedstock_category=="Sugar"), nonfood_sugar, by = c("year","iso3c"), relationship = "many-to-many"),
  left_join(use_to_estimate %>% filter(feedstock_category=="Starch"), nonfood_starch, by = c("year","iso3c"), relationship = "many-to-many")
) %>%
  rename(share_crop_in_use = other) %>%
  mutate(input = coalesce(item, input),
         feedstock_details = feedstock_category) %>%
  select(-item, -feedstock_category)


######################################################################################################################
#2. Joining shares in nonfood use for the subset of feedstocks when there is a feedstock detail #########
######################################################################################################################

use_to_estimate2 <- use_bb_bp %>% 
  filter(!is.na(feedstock_details)) %>%
  mutate(feedstock_details = ifelse(feedstock_details=="Glucose", "Sugar", feedstock_details))


# ── Step 1: Expand input by ";" → long format of unique (iso3c, year, feedstock_details, input_item)
input_long <- use_to_estimate2 %>%
  select(iso3c, year, feedstock_details, input) %>%
  distinct() %>%
  mutate(input_item = str_split(input, ";")) %>%
  unnest(input_item) %>%
  mutate(input_item = str_trim(input_item))

# ── Step 2a: Sugar → look up nonfood_sugar, compute shares within (iso3c, year)
sugar_shares <- input_long %>%
  filter(feedstock_details == "Sugar") %>%
  left_join(
    nonfood_sugar,
    by = c("iso3c", "year", "input_item" = "item")
  ) %>%
  group_by(iso3c, year) %>%
  mutate(share_crop_in_use = other / sum(other, na.rm = TRUE)) %>%
  ungroup() %>%
  select(iso3c, year, feedstock_details, input_item, share_crop_in_use)

# ── Step 2b: Starch → look up nonfood_starch, compute shares within (iso3c, year)
starch_shares <- input_long %>%
  filter(feedstock_details == "Starch") %>%
  left_join(
    nonfood_starch,
    by = c("iso3c", "year", "input_item" = "item")
  ) %>%
  group_by(iso3c, year) %>%
  mutate(share_crop_in_use = other / sum(other, na.rm = TRUE)) %>%
  ungroup() %>%
  select(iso3c, year, feedstock_details, input_item, share_crop_in_use)

# ── Step 3: Combine shares
shares_all <- bind_rows(sugar_shares, starch_shares) %>%
  group_by(iso3c, year, feedstock_details) %>%
  mutate(share_crop_in_use = case_when(
    year == 2024 ~ share_crop_in_use,
    is.na(share_crop_in_use) & n() == 1 ~ 1,
    is.na(share_crop_in_use) & n() >  1 ~ 0,
    TRUE                                ~ share_crop_in_use
  )) %>%
  ungroup() %>%
  # Force ESP sugar beet share to 1 across all years (absent from nonfood_sugar due to zero non-food use)
  mutate(share_crop_in_use = ifelse(iso3c == "ESP" & input_item == "Sugar beet", 1, share_crop_in_use))


# ── Step 4: Join back to use_to_estimate2
# Expand input in use_to_estimate2 first so input_item is available as join key
use_to_estimate2 <- use_to_estimate2 %>%
  mutate(input_item = str_split(input, ";")) %>%
  unnest(input_item) %>%
  mutate(input_item = str_trim(input_item)) %>%
  left_join(
    shares_all,
    by = c("iso3c", "year", "feedstock_details", "input_item")
  ) %>%
  select(-input) %>%
  rename(input = input_item) %>%
  arrange(iso3c, year, feedstock_details, input)


######################################################################################################################
#3. Intermediate use dataset (joining) #########
######################################################################################################################

use_bb_bp_intermediate <- bind_rows(
  use_bb_bp %>% filter(is.na(feedstock_category) & is.na(feedstock_details)),
  use_to_estimate,
  use_to_estimate2
) %>%
  select(-feedstock_category) %>%
  filter(year <= 2023)


######################################################################################################################
#4. Estimating feedstock use from crops  #########
######################################################################################################################

use_bb_bp_intermediate <- use_bb_bp_intermediate %>%
  left_join(tcf_crop_bp %>% select(-tcf), by = c("input" = "parent", "feedstock_details" = "child", "iso3c"))

use_bb_bp_intermediate <- use_bb_bp_intermediate %>%
  group_by(iso3c, product, feedstock_details, year) %>%
  mutate(share_sugar_or_starch_from_crop = (share_crop_in_use / crop_requirement_multiplier) /
           sum(share_crop_in_use / crop_requirement_multiplier)) %>%
  ungroup() %>%
  #compute total sugar or starch use from each crop; and total qty of each crop as a result
  mutate(sugar_or_starch_from_crop = share_sugar_or_starch_from_crop * use,
         crop_use  = sugar_or_starch_from_crop * crop_requirement_multiplier)

# Updating the use row where inputs are crops (sugar/starch)
use_bb_bp_intermediate <- use_bb_bp_intermediate %>%
  mutate(use = case_when(!is.na(feedstock_details) ~ crop_use,
                         TRUE ~ use),
         input = case_when(input == "Bioethanol" ~ "Biogasoline",
                           TRUE ~ input)) %>%
  group_by(product, input, iso3c, year) %>%
  summarize(use = sum(use, na.rm = FALSE),
            unit = "kt")


######################################################################################################################
#5. Updating use from supply chain flows whenever no other info is available  #########
######################################################################################################################

use_bb_bp_intermediate <- use_bb_bp_intermediate %>% 
  rows_patch(total_use_est %>% rename(use = value,
                                      iso3c = importer_iso3), 
             by = c("product","input","year","iso3c"),
             unmatched = "ignore")





###########################################################
########### BTD FOR BIOPOLYMERS #########
###########################################################

###########################################################
#1. Initial table #########
###########################################################

btd_intermediate_other <- btd_intermediate_other %>%
  filter(!(product =="MEG"),
         !(product == "Epichlorohydrin" & !(exporter_iso3 %in% c("CHN","THA","CZE")))) %>%
  mutate(product = case_when(product == "1,4-butanediol" ~ "1,4-BDO",
                             product == "Lactic acid" ~ "L-LA",
                             product == "Polylactic acid" ~ "PLA",
                             product == "Epichlorohydrin" ~ "ECH",
                             TRUE ~ product)) %>%
  select(-FLOW, -source)

# Multipliers for ECH exports from CHN, based on the share of bio-based ECH in total ECH supply in China by year
mult_ech_trade <- data.frame(
  year = 2011:2024,
  mult = c(0.117820, 0.171990, 0.270400, 0.376176, 0.395137, 0.435190, 0.470829, 0.471980, 0.466720, 0.506280, 0.511860, 0.488970, 0.488970, 0.488970)
)

btd_intermediate_other <- btd_intermediate_other %>%
  left_join(mult_ech_trade, by = "year") %>%
  mutate(value = if_else(exporter_iso3 == "CHN" & product == "ECH", value * mult, value)) %>%
  select(-mult)

btd_intermediate_other <- btd_intermediate_other %>% bind_rows(btd_bb_bp %>% select(-product) %>% filter(! input %in% c("Bionaphtha","Biopropane")) %>% rename(product = input))

total_trade_im <- btd_intermediate_other %>%
  group_by(importer_iso3, product, year) %>%
  summarise(value = sum(value),
            .groups = "drop")

total_trade_ex <- btd_intermediate_other %>%
  group_by(exporter_iso3, product, year) %>%
  summarise(value = sum(value),
            .groups = "drop")



###########################################################
########### COMPILING FOR ELICITATION AND BALANCING  #########
###########################################################

###########################################################
#1. Calculating domestic use, estimating some missing flows  #########
###########################################################

full_compile_bp <-
  use_bb_bp_intermediate %>% filter(!grepl("Sugar|products|Biogasoline|Castor", input),
                                    year %in% 2012:2022) %>% #excluding crops as this is straightforward and requires FAOSTAT's trade data. Same for Biogasoline.
  # joining "self use" (from qualitative supply chain info)
  # left_join(self_flows_bb_bp %>% select(product, input, iso3c, year, self = value), 
  #           by = c("product", "input", "iso3c", "year")) %>%
  # joining total imports of feedstock by year-country
  left_join(total_trade_im %>% select(iso3c = importer_iso3, input = product, year, imports = value),
            by = c("input","iso3c","year")) %>%
  # joining total exports of feedstock by year-country
  left_join(total_trade_ex %>% select(iso3c = exporter_iso3, input = product, year, exports = value),
            by = c("input","iso3c","year")) %>%
  # joining production of feedstock by year-country (domestic availability before imports/exports)
  left_join(supply_bb_bp %>% select(iso3c, input = product, year, output = supply),
            by = c("input","iso3c","year")) %>%
  rows_patch(supply_final_bf_sua %>% select(iso3c, input = product, year, output = supply),
             by = c("input","iso3c","year"),
             unmatched = "ignore") 

# Replacing with 0s for imports and exports where NA for products for which we have full trade data
full_compile_bp <- full_compile_bp %>%
  mutate(across(c(imports, exports), ~ case_when(input %in% c("Succinic acid", "1,4-BDO", "ECH", "MEG", "11-AA", "L-LA", "PLA", "Sebacic acid") & is.na(.x) ~ 0,
                                                 TRUE ~ .x)),
         output = case_when(input %in% c("L-LA", "MEG", "Sebacic acid", "Succinic acid") & is.na(output) ~ 0,
                            TRUE ~ output))

# Elicitation of net trade when production and consumption are knwon
full_compile_bp <- full_compile_bp %>%
  mutate(
    use = case_when(input == "Sebacic acid" & is.na(use) ~ pmax(output + imports - exports,0),
                    TRUE ~ use))

# Compiling year sums of use
full_compile_bp <- full_compile_bp %>%
  group_by(input, iso3c, year) %>%
  summarise(use = sum(use),
            imports = first(imports),
            exports = first(exports),
            output = first(output),
            .groups = "drop") 


###########################################################
#2. Selecting the subset of rows that needs to be modified for inputs #########
###########################################################

full_compile_for_join <- subset(full_compile_bp %>% filter(input %in% c("Glycerol, crude", "Biopropane", "Bionaphtha"))) %>%
  rename(domestic_prod = output,
         domestic_use_as_input = use) %>%
  mutate(
    required_remaining = case_when(
      is.na(imports) & is.na(exports) & domestic_prod >= domestic_use_as_input ~ 0,
      is.na(imports) & is.na(exports) & domestic_prod <= domestic_use_as_input ~ domestic_use_as_input - domestic_prod,
      domestic_prod + imports - exports - domestic_use_as_input <= 0 ~ abs(domestic_prod + imports - exports - domestic_use_as_input),
      domestic_prod + imports - exports - domestic_use_as_input > 0 ~ 0,
      TRUE ~ NA
    ),
    available_remaining = case_when(
      is.na(imports) & is.na(exports) & domestic_prod >= domestic_use_as_input ~ domestic_prod - domestic_use_as_input,
      is.na(imports) & is.na(exports) & domestic_prod <= domestic_use_as_input ~ 0,
      domestic_prod + imports - exports - domestic_use_as_input > 0 ~ abs(domestic_prod + imports - exports - domestic_use_as_input),
      domestic_prod + imports - exports - domestic_use_as_input <= 0 ~ 0,
      TRUE ~ NA
    ),
    unknown_use = ifelse(available_remaining >= 0, available_remaining, 0),
    required_imports = ifelse(required_remaining >= 0 & input %in% c("Biopropane", "Bionaphtha"), required_remaining, 0),
    output_balancing = ifelse(required_remaining >= 0 & input == "Glycerol, crude", required_remaining, 0)
  ) %>%
  mutate(domestic_prod = domestic_prod + output_balancing,
         unit = "kt") %>%
  select(-available_remaining, -required_remaining) %>%
  rename(product = input)


###########################################################
#2. Bringing together supply and use info #########
###########################################################

full_compile_supply <- supply_bb_bp %>%
  filter(year %in% 2012:2022) %>%
  rename(domestic_prod = supply) %>%
  left_join(total_trade_im %>% select(iso3c = importer_iso3, product, year, imports = value),
            by = c("product","iso3c","year")) %>%
  left_join(total_trade_ex %>% select(iso3c = exporter_iso3, product, year, exports = value),
            by = c("product","iso3c","year")) %>%
  mutate(across(c(imports, exports), ~ case_when(product %in% c("Succinic acid", "1,4-BDO", "ECH", "MEG", "11-AA", "L-LA", "PLA", "Sebacic acid", "Biopropane", "Bionaphtha") & is.na(.x) ~ 0,
                                                 TRUE ~ .x)),
         domestic_prod = case_when(product %in% c("L-LA", "MEG", "Sebacic acid", "Succinic acid") & is.na(domestic_prod) ~ 0,
                                   TRUE ~ domestic_prod))

full_compile_supply <- left_join(full_compile_supply, full_compile_bp %>% select(product = input, iso3c, year, domestic_use_as_input = use),
                                 by = c("product", "iso3c", "year")) %>%
  mutate(domestic_use_as_input = case_when(
    product == "Sebacic acid" ~ 0,
    product == "11-AA" ~ 0,
    is.na(domestic_use_as_input) ~ 0,
    TRUE ~ domestic_use_as_input))



###########################################################
#3. Modeling ECH use for Epoxy resins #########
###########################################################

#Joining ECH to the table 
ech <- total_trade_im %>%
  filter(product == "ECH", value != 0) %>%
  rename(iso3c = importer_iso3) %>%
  distinct(iso3c, year, .keep_all = TRUE) %>%
  anti_join(
    full_compile_supply %>% filter(product == "ECH"),
    by = c("iso3c", "year")
  ) %>%
  transmute(
    iso3c,
    product        = "ECH",
    year,
    exports        = 0,
    imports        = value,
    domestic_prod  = 0
  )

full_compile_supply <- bind_rows(full_compile_supply, ech)
rm(ech)

# Step 1: Update domestic_use_as_input for ECH in full_compile_supply
full_compile_supply <- full_compile_supply %>%
  mutate(
    domestic_use_as_input = if_else(
      product == "ECH",
      pmax(domestic_prod + imports - exports, 0),
      domestic_use_as_input
    )
  )

# Step 2: Create new Epoxy resins rows from ECH
epoxy <- full_compile_supply %>%
  filter(product == "ECH") %>%
  transmute(
    iso3c,
    year,
    product       = "Epoxy resins",
    domestic_prod = 1.9 * domestic_use_as_input,
    domestic_use_as_input = 0
  )

full_compile_supply <- bind_rows(full_compile_supply, epoxy)
rm(epoxy)

###########################################################
#4. Adding ECH use for Epoxy resins to Use table #########
###########################################################

new_use_rows <- full_compile_supply %>%
  filter(product == "ECH") %>%
  transmute(
    product      = "Epoxy resins",
    iso3c,
    year,
    input        = "ECH",
    use          = domestic_use_as_input,
    unit         = "kt"
  )

use_bb_bp_intermediate <- bind_rows(use_bb_bp_intermediate, new_use_rows)
rm(new_use_rows)



# Estimating remaining requirements / excedent
full_compile_supply <- full_compile_supply %>%
  mutate(
    required_remaining = case_when(
      is.na(imports) & is.na(exports) & domestic_prod >= domestic_use_as_input ~ 0,
      is.na(imports) & is.na(exports) & domestic_prod <= domestic_use_as_input ~ domestic_use_as_input - domestic_prod,
      is.na(imports) & !is.na(exports) ~ 0,
      domestic_prod + imports - exports - domestic_use_as_input <= 0 ~ abs(domestic_prod + imports - exports - domestic_use_as_input),
      domestic_prod + imports - exports - domestic_use_as_input > 0 ~ 0,
      TRUE ~ NA
    ),
    available_remaining = case_when(
      is.na(imports) & is.na(exports) & domestic_prod >= domestic_use_as_input ~ domestic_prod - domestic_use_as_input,
      is.na(imports) & is.na(exports) & domestic_prod <= domestic_use_as_input ~ 0,
      !is.na(imports) & is.na(exports) ~ 0,
      domestic_prod + imports - exports - domestic_use_as_input > 0 ~ abs(domestic_prod + imports - exports - domestic_use_as_input),
      domestic_prod + imports - exports - domestic_use_as_input <= 0 ~ 0,
      TRUE ~ NA
    )
  )



###########################################################
########### MODELING TRADE FLOWS THAT CONTRIBUTE TO REBALANCING  #########
###########################################################


###########################################################
#1. Collecting couples that can be rebalanced and making trade flows  #########
###########################################################

# Step 1: Identify (product, year) pairs with both required and available remaining
eligible_py <- full_compile_supply %>%
  group_by(product, year) %>%
  filter(any(required_remaining > 0) & any(available_remaining > 0)) %>%
  ungroup()

# Step 2: Compute trade flows
btd_from_balancing <- eligible_py %>%
  group_by(product, year) %>%
  group_modify(~ {
    imp <- .x %>% filter(required_remaining > 0) %>% select(iso3c, required_remaining)
    exp <- .x %>% filter(available_remaining > 0) %>% select(iso3c, available_remaining)
    
    sum_req   <- sum(imp$required_remaining)
    sum_avail <- sum(exp$available_remaining)
    total     <- min(sum_req, sum_avail)
    
    crossing(
      rename(imp, importer_iso3 = iso3c),
      rename(exp, exporter_iso3 = iso3c)
    ) %>%
      mutate(
        value = (required_remaining / sum_req) *
          (available_remaining / sum_avail) *
          total,
        unit  = "kt"
      ) %>%
      select(importer_iso3, exporter_iso3, value, unit)
  }) %>%
  ungroup()

# Step 3: Compute btd_balancing per iso3c-product-year
# Positive = added to imports (importer side), Negative = added to exports (exporter side)
btd_balancing_import <- btd_from_balancing %>%
  group_by(iso3c = importer_iso3, product, year) %>%
  summarise(btd_balancing = sum(value), .groups = "drop")

btd_balancing_export <- btd_from_balancing %>%
  group_by(iso3c = exporter_iso3, product, year) %>%
  summarise(btd_balancing = -sum(value), .groups = "drop")

btd_balancing_all <- bind_rows(btd_balancing_import, btd_balancing_export) %>%
  group_by(iso3c, product, year) %>%
  summarise(btd_balancing = sum(btd_balancing), .groups = "drop")

rm(btd_balancing_export, btd_balancing_import)

###########################################################
#3. Join to the table and modify total imports, total exports, and "remaining" values  #########
###########################################################

full_compile_supply <- full_compile_supply %>%
  left_join(btd_balancing_all, by = c("iso3c", "product", "year")) %>%
  mutate(
    btd_balancing = replace_na(btd_balancing, 0),
    # Update imports: positive btd_balancing = added to imports
    imports = case_when(
      btd_balancing > 0 & is.na(imports)  ~ btd_balancing,
      btd_balancing > 0 & !is.na(imports) ~ imports + btd_balancing,
      TRUE                                 ~ imports
    ),
    # Update exports: negative btd_balancing = added to exports (store as positive)
    exports = case_when(
      btd_balancing < 0 & is.na(exports)  ~ -btd_balancing,
      btd_balancing < 0 & !is.na(exports) ~ exports - btd_balancing,
      TRUE                                 ~ exports
    ),
    # Update remaining columns
    required_remaining  = if_else(btd_balancing > 0,
                                  required_remaining - btd_balancing,
                                  required_remaining),
    available_remaining = if_else(btd_balancing < 0,
                                  available_remaining + btd_balancing,
                                  available_remaining)
  )


###########################################################
#4. Join bilateral trade flows to the dataset #########
###########################################################

btd_intermediate_other <- bind_rows(btd_intermediate_other %>% mutate(type = "official trade data"), btd_from_balancing %>% mutate(type = "Estimate")) %>%
  group_by(exporter_iso3, importer_iso3, product, year) %>%
  summarise(value = sum(value),
            unit = first(unit),
            type  = case_when(
              n_distinct(type) > 1  ~ "official trade data adjusted",
              TRUE ~ first(type)),
            .groups = "drop")




###########################################################
########### ELICITATION OF "OTHER INDUSTRIAL USES" WHEN POSSIBLE  #########
###########################################################

###########################################################
#1. Elicitation of other domestic industrial use when "available_remaining" > 0  #########
###########################################################

full_compile_supply <- full_compile_supply %>%
  mutate(other_industry_use = case_when(product %in% c("L-LA","PLA","1,4-BDO","Succinic acid", "Sebacic acid", "ECH", "MEG", "11-AA") & available_remaining >= 0 ~ available_remaining, # products for which we have full trade data
                                        required_remaining == 0 & available_remaining == 0 ~ 0,
                                        TRUE ~ NA),  
         required_exports = case_when(product %in% c("L-LA","PLA","1,4-BDO","Succinic acid", "Sebacic acid", "ECH", "MEG", "11-AA") & available_remaining >= 0 ~ 0,                    # products for which we have full trade data
                                      required_remaining == 0 & available_remaining == 0 ~ 0,
                                      TRUE ~ NA),
         required_imports = 0,
         output_balancing = ifelse(required_remaining > 0, required_remaining, 0)
  ) %>%
  mutate(domestic_prod = domestic_prod + output_balancing) %>%
  rows_update(pe_supply_braskem %>% filter(rows == "domestic") %>% select(year, product, iso3c, other_industry_use = value), by = c("year","product","iso3c"), unmatched = "ignore") %>%
  rows_update(pe_supply_braskem %>% filter(rows == "export") %>% select(year, product, iso3c, required_exports = value), by = c("year","product","iso3c"), unmatched = "ignore")

# Joining products that we know are used in PA production but PA production is not a separate process, so we include it as domestic industry uses. 
full_compile_supply <- full_compile_supply %>%
  bind_rows(
    full_compile_bp %>%
      filter(input == "Sebacic acid") %>%
      anti_join(full_compile_supply, by = c("iso3c", "year", "input" = "product")) %>%
      transmute(
        iso3c,
        year,
        product              = input,
        domestic_prod        = output,
        domestic_use_as_input = 0,
        other_industry_use = use,
        required_remaining = 0,
        available_remaining = 0, 
        btd_balancing = 0, 
        required_exports = 0, 
        required_imports = 0, 
        output_balancing = 0
      )
  )

###########################################################
#2. Extrapolate backwards for Brazil - PE  #########
###########################################################

shares_2016 <- full_compile_supply %>%
  filter(year == 2016, available_remaining > 0) %>%
  mutate(
    share_other_industry  = other_industry_use / available_remaining,
    share_required_exports = required_exports  / available_remaining
  ) %>%
  select(iso3c, product, share_other_industry, share_required_exports)

# Apply shares backwards to 2012-2015
full_compile_supply <- full_compile_supply %>%
  left_join(shares_2016, by = c("iso3c", "product")) %>%
  mutate(
    other_industry_use = case_when(
      year < 2016 & !is.na(share_other_industry) & available_remaining > 0
      ~ share_other_industry   * available_remaining,
      TRUE ~ other_industry_use
    ),
    required_exports  = case_when(
      year < 2016 & !is.na(share_required_exports) & available_remaining > 0
      ~ share_required_exports * available_remaining,
      TRUE ~ required_exports
    )
  ) %>%
  select(-share_other_industry, -share_required_exports) %>%
  rename(bp_feedstock_use = domestic_use_as_input) %>%
  mutate(unit = "kt")




###########################################################
########### MAKING TWO SUBTABLES: ONE WHERE ESTIMATES ARE COMPLETE, ONE WHICH REQUIRES TRADE LINKAGE  #########
###########################################################

###########################################################
#1. Full subtables  #########
###########################################################

# Complete rows for which do not require Monetary MRIO allocation
complete_rows <- full_compile_supply %>% filter(!is.na(other_industry_use)) %>% select(-available_remaining, -required_remaining, -output_balancing, -btd_balancing) %>%
  mutate(exports = coalesce(exports, required_exports),
         imports = coalesce(imports, required_imports)) %>%
  select(-required_exports, -required_imports) 
  

# Incomplete rows for which we will use Monetary MRIO allocation
incomplete_rows <- full_compile_supply %>% filter(is.na(other_industry_use)) %>% select(-imports, -exports, -btd_balancing, -required_remaining, -output_balancing) 

# And the complete rows for feedstocks 
use_bf_coproducts <- full_compile_for_join %>% select(product, iso3c, year, domestic_use_as_input, unknown_use, required_imports)


###########################################################
#2. Selecting only the variables required for the use in final demand  #########
###########################################################

y_bp_complete_rows <- complete_rows %>%
  rename(item = product) %>%
  select(-domestic_prod, -exports, -imports, -bp_feedstock_use) # This drops total exports of PE from BRA, which we haven't allocated yet. 





###########################################################
########### UPDATING SUPPLY TABLES FROM ADJUSTED TOTAL OUTPUT AFTER BALANCING  ##########
###########################################################

supply_final_bp <- supply_bb_bp %>% 
  mutate(unit = "kt") %>%
  rows_upsert(complete_rows %>% select(iso3c, product, year, supply = domestic_prod, unit), by = c("iso3c","product","year"))

supply_final_bf_sua <- supply_final_bf_sua %>% rows_update(full_compile_for_join %>% select(iso3c, product, year, supply = domestic_prod, unit), by = c("iso3c","product","year"), unmatched = "ignore")




###########################################################
########### FORMATTING USE TABLE  ##########
###########################################################

###########################################################
#1. Updating use for full consistency  ##########
###########################################################

use_bb_bp_intermediate <- use_bb_bp_intermediate %>% 
  filter(year %in% 2012:2022) %>%
  group_by(product, input, iso3c, year) %>%
  summarise(use = sum(use, na.rm = FALSE),
            unit = first(unit),
            .groups = "drop") %>%
  rows_patch(
    full_compile_supply %>%
      select(iso3c, year, input = product, use = bp_feedstock_use),
    by = c("iso3c", "year", "input"),
    unmatched = "ignore"
    ) %>%
  filter(!(input == "Sebacic acid"),
         !(input == "11-AA"),
         year %in% 2012:2022)

  
###########################################################
#2. Joining processes  ##########
###########################################################

use_bb_bp_intermediate <- use_bb_bp_intermediate %>%   
  left_join(items_supply_bcp %>% select(proc, item), 
            by = c("product" = "item")) %>%
  rename(item = input) %>%
  select(-product)

use_bb_bp_intermediate <- use_bb_bp_intermediate %>% 
  mutate(use = ifelse(item == "Biogasoline", 1.267*use, use),
         unit = ifelse(item == "Biogasoline", "Ml", unit))




###########################################################
########### SELECTING BTD ROWS  ##########
###########################################################

btd_intermediate_other <- btd_intermediate_other %>% 
  filter(product %in% c("L-LA","Glycerol, crude", "PLA", "1,4-BDO", "Sebacic acid", "Succinic acid", "ECH", "11-AA", "MEG"))




###########################################################
########### SAVING TABLES  ##########
###########################################################

setwd("/home/mmondolfo/fabio_bfp/")

saveRDS(y_bp_complete_rows, "inputs_for_final_data/y_bp_complete_rows.rds")
saveRDS(incomplete_rows, "inputs_for_final_data/y_bp_incomplete_rows.rds")
saveRDS(supply_final_bp, "inputs_for_final_data/supply_final_bp.rds")
saveRDS(btd_intermediate_other, "intermediate_data/btd_intermediate_other_step2.rds")
saveRDS(supply_final_bf_sua, "inputs_for_final_data/supply_final_bf_sua.rds")
saveRDS(use_bf_coproducts, "intermediate_data/y_bf_coproducts_initial.rds")
saveRDS(use_bb_bp_intermediate, "inputs_for_final_data/use_final_bp.rds")

rm(list = ls())