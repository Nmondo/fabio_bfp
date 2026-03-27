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

########### FAO data (non-food use) #########

cbs_full <- readRDS("data/cbs_full.rds")

############# Supply data from own collection ##################

#### Here unit is always kilotonnes unless specified. 

setwd("/home/mmondolfo/")

capacities_bb_bp <- read_excel("own_data/Supply_BB_BP_report.xlsx",sheet="filter")
output_bp <- read_excel("own_data/Supply_BB_BP_report.xlsx",sheet="capacities_and_production")

########### Technical conversion factors #########

tcf_table <- readRDS("tcf_table_clean.rds")  




######################################################################################################################
######### CALCULATING SHARES IN NON-FOOD USE OF STARCH / SUGAR CROPS #########
######################################################################################################################

########### List of "Starch" and "Sugar" (incl. sugar from starch) crops, and corresponding subsets of fao_nonfood #########

sugar_crops <- c("Wheat and products","Rice and products","Barley and products","Maize and products","Rye and products","Sorghum and products","Cassava and products","Potatoes and products","Sugar cane","Sugar beet")
starch_crops <- c("Wheat and products","Rice and products","Barley and products","Maize and products","Rye and products","Sorghum and products","Cassava and products","Potatoes and products")

fao_nonfood <- subset(cbs_full, !is.na(other) &
                        year >= 2010 &
                        (item %in% c("Wheat and products","Rice and products","Barley and products","Maize and products","Rye and products","Sorghum and products","Cereals, Other","Cassava and products","Sugar cane","Sugar beet","Sweeteners, Other",
                                     "Soyabean Oil","Sunflowerseed Oil","Rape and Mustard Oil","Cottonseed Oil","Palmkernel Oil","Palm Oil","Coconut Oil","Maize Germ Oil","Oilcrops Oil, Other","Fats, Animals, Raw",
                                     "Potatoes and products")))

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

supply_bb_bp <-supply_bb_bp %>%
  group_by(iso3c,product,year) %>%
  summarize(supply = sum(supply, na.rm=TRUE))




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
  mutate(specific_tcf = coalesce(specific_tcf_1, specific_tcf_2, specific_tcf_3)) %>%
  select(-specific_tcf_1, -specific_tcf_2, -specific_tcf_3)


###########################################################
#2. Estimating feedstock use #########
###########################################################

use_bb_bp <- use_bb_bp %>%
  mutate(use = est_prod*(1/specific_tcf))



###########################################################
########### COLLECTING SUPPLY CHAIN INFO FOR BILATERAL TRADE FLOWS #########
###########################################################

btd_bb_bp <- use_bb_bp %>%
  select(product,chain,step, input, iso3c,year, est_prod, use) %>%
  filter(!is.na(chain)) %>%
  arrange(chain,year,step)

###########################################################
#1. Disaggregating rows when multiple chains involved #########
###########################################################

btd_bb_bp <- btd_bb_bp %>%
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

btd_bb_bp <- btd_bb_bp %>%
  # Step 2: base rows (imports/use side)
  filter(step == 2) %>%
  select(chain, product, input, iso3c, year, use) %>%
  rename(importer_iso3 = iso3c) %>%
  # Join step 1 (exports/supply side) by chain + year
  left_join(
    btd_bb_bp %>%
      filter(step == 1) %>%
      select(chain, iso3c, year, est_prod) %>%
      rename(exporter_iso3 = iso3c,
             qty_available  = est_prod),
    by = c("chain", "year")
  ) %>%
  mutate(exporter_iso3 = ifelse(input %in% c("Bionaphtha", "Biopropane"), "NLD", exporter_iso3),
         flow = pmin(use, qty_available)) %>%
  select(chain, product, input, exporter_iso3, importer_iso3, year, qty_available, use, flow) %>%
  arrange(chain, year, exporter_iso3, importer_iso3)



###########################################################
#2. Extracting the set of flows #########
###########################################################

btd_bb_bp_flows <- btd_bb_bp %>% filter(!is.na(flow))

## Note : some of these flows are "self consumption". 




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
  rename(share_in_use = other) %>%
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
  mutate(share_in_use = other / sum(other, na.rm = TRUE)) %>%
  ungroup() %>%
  select(iso3c, year, feedstock_details, input_item, share_in_use)

# ── Step 2b: Starch → look up nonfood_starch, compute shares within (iso3c, year)
starch_shares <- input_long %>%
  filter(feedstock_details == "Starch") %>%
  left_join(
    nonfood_starch,
    by = c("iso3c", "year", "input_item" = "item")
  ) %>%
  group_by(iso3c, year) %>%
  mutate(share_in_use = other / sum(other, na.rm = TRUE)) %>%
  ungroup() %>%
  select(iso3c, year, feedstock_details, input_item, share_in_use)

# ── Step 3: Combine shares
shares_all <- bind_rows(sugar_shares, starch_shares) %>%
  group_by(iso3c, year, feedstock_details) %>%
  mutate(share_in_use = case_when(
    year == 2024 ~ share_in_use,
    is.na(share_in_use) & n() == 1 ~ 1,
    is.na(share_in_use) & n() >  1 ~ 0,
    TRUE                            ~ share_in_use
  )) %>%
  ungroup()


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
  filter(year <= 2023) %>%
  mutate(share_in_use = coalesce(share_in_use, 1))

unique(use_bb_bp_intermediate$input)
