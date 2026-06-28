###########################################################
########### LOADING PACKAGES #########
###########################################################
setwd("/home/mmondolfo/fabio_bfp/")

library("tidyverse")
library("data.table")
library(dplyr)
source("R/00_system_variables.R")

###########################################################
########### LOADING DATA #########
###########################################################


items_full_bcp <- read_csv("inst/items_full_bcp.csv")
cbs_sua_full <- readRDS("data/cbs_sua_full.rds")
use_rebal_bcp <- readRDS("intermediate_data/use_compiled_bcp.rds")
sup_final_bcp <- readRDS("data/sup_final_bcp.rds")
tcf_table <- readRDS("intermediate_data/tcf_table_final.rds")


###########################################################
########### MAKING VECTORS/IDs #########
###########################################################

bf_feedstocks <- c("Wheat and products","Rice and products","Barley and products",
                   "Maize and products","Rye and products","Sorghum and products",
                   "Cassava and products","Sugar cane","Sugar beet","Molasses", 
                   "Triticale","Soyabean Oil","Sunflowerseed Oil","Rape and Mustard Oil",
                   "Cottonseed Oil","Palmkernel Oil","Palm Oil","Coconut Oil","Maize Germ Oil","Fats, Animals, Raw",
                   "Animal or vegetable fats and oils and their fractions, chemically modified, except those hydrogenated, inter-esterified, re-esterified or elaidinized; inedible mixtures or preparations of animal or vegetable fats or oils")

biogasoline_proc_code <- use_rebal_bcp %>%
  filter(proc == "Biogasoline production") %>%
  distinct(proc, proc_code)

###########################################################
########### CORRECTING SOME FEEDSTOCK ASSUMPTIONS #########
###########################################################

swaps <- tibble::tribble(
  ~area,      ~new_item,
  "Uruguay",  "Wheat and products",
  "Iceland",  "Other, Waste",
  "Finland",  "Other, Waste",
  "Mauritius", "Molasses",
  "Sudan", "Molasses",
  "Bolivia (Plurinational State of)", "Sugar cane",
  "Uganda", "Molasses"
)

swap_tcf <- tcf_table %>%
  filter(proc == "Biogasoline production", item %in% swaps$new_item) %>%
  select(new_item = item, tcf = output_qty)

swap_lookup <- items_full_bcp %>%
  filter(item %in% swaps$new_item) %>%
  distinct() %>%
  rename(new_item = item)

new_rows <- sup_final_bcp %>%
  filter(area %in% swaps$area, proc == "Biogasoline production", item == "Biogasoline") %>%
  select(-item) %>%
  inner_join(swaps, by = "area") %>%
  inner_join(swap_tcf, by = "new_item") %>%
  inner_join(swap_lookup, by = "new_item") %>%
  mutate(
    proc = "Biogasoline production",
    item = new_item,
    unit = "tonnes",
    use  = supply / tcf
  ) %>%
  left_join(biogasoline_proc_code, by = "proc") %>%
  select(any_of(names(use_rebal_bcp)))

use_rebal_bcp <- use_rebal_bcp %>%
  filter(!(area %in% swaps$area & proc == "Biogasoline production")) %>%
  bind_rows(new_rows)


###########################################################
# Creating new supply rows for "Other, Waste" to match feedstock requirements.
###########################################################

waste_sup_rows <- use_rebal_bcp %>%
  filter(area %in% c("Iceland", "Finland"),
         proc == "Biogasoline production",
         item == "Other, Waste") %>%
  mutate(
    supply = use * (tcf_table %>%
                      filter(proc == "Biogasoline production",
                             item == "Other, Waste") %>%
                      pull(output_qty))
  ) %>%
  select(any_of(names(sup_final_bcp))) %>%
  mutate(proc = "Other, Waste collection") %>%
  group_by(area_code, year, proc, item) %>%
  summarise(add_supply = sum(supply, na.rm = TRUE), .groups = "drop")

sup_final_bcp <- sup_final_bcp %>%
  left_join(waste_sup_rows, by = c("area_code", "year", "proc", "item")) %>%
  mutate(supply = supply + replace_na(add_supply, 0)) %>%
  select(-add_supply)

###########################################################
########### REBALANCING USE IN FINAL DEMAND BASED ON USE AS BIOCHEMICALS FEEDSTOCK #########
###########################################################

to_subtract <- use_rebal_bcp %>%
  filter(proc %in% c("Biodiesel production", "Renewable diesel production", "Biogasoline production")) %>%
  mutate(proc_label = case_when(
    proc == "Biodiesel production"        ~ "biodiesel",
    proc == "Renewable diesel production" ~ "ren_diesel",
    proc == "Biogasoline production"      ~ "biogasoline"
  )) %>%
  group_by(area_code, year, comm_code, proc_label) %>%
  summarise(use = sum(use, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = proc_label, values_from = use, values_fill = 0)

###########################################################
# Step 2: Pivot TCFs to wide format (one col per proc)
###########################################################

tcf_wide <- tcf_table %>%
  mutate(proc_label = case_when(
    proc == "Biodiesel production"        ~ "biodiesel_tcf",
    proc == "Renewable diesel production" ~ "ren_diesel_tcf",
    proc == "Biogasoline production"      ~ "biogasoline_tcf"
  )) %>%
  select(item, proc_label, output_qty) %>%
  pivot_wider(
    names_from  = proc_label,
    values_from = c(output_qty),
    values_fill = 0
  )

###########################################################
# Step 3: Join both into cbs_sua_adjusted
###########################################################

cbs_sua_adjusted <- cbs_sua_full %>%
  left_join(items_full_bcp %>% select(item, comm_code), by = "item") %>%
  left_join(to_subtract, by = c("area_code", "year", "comm_code")) %>%
  mutate(across(c(biodiesel, ren_diesel, biogasoline), ~ replace_na(.x, 0))) %>%
  left_join(tcf_wide, by = "item") %>%
  filter(item %in% bf_feedstocks)

###########################################################
# Step 3b: Adjust production (and other) for comm_code 1274
#          so that input_use is fully absorbed by credible uses
###########################################################

cbs_sua_adjusted <- cbs_sua_adjusted %>%
  mutate(
    prelim_gap = pmax(0, biogasoline + biodiesel + ren_diesel - (replace_na(feed, 0) +
                                                                   replace_na(processing, 0) +
                                                                   replace_na(other, 0) +
                                                                   replace_na(stock_addition, 0) +
                                                                   replace_na(tourist, 0))),
    production = if_else(item_code == 1274 & prelim_gap > 0,
                         production + prelim_gap, production),
    other      = if_else(item_code == 1274 & prelim_gap > 0,
                         replace_na(other, 0) + prelim_gap, other)
  ) %>%
  select(-prelim_gap)

###########################################################
# Step 4: Compute input_use, gap to credible uses, shares, and gap_supply per product
###########################################################

cbs_sua_adjusted <- cbs_sua_adjusted %>%
  mutate(
    input_use = biodiesel + ren_diesel + biogasoline,
    
    # Gap between estimated feedstock use and credible SUA uses
    gap_bcp_use_to_credible_uses = pmax(0, input_use - (replace_na(feed, 0) + 
                                                          replace_na(processing, 0) + 
                                                          replace_na(other, 0) + 
                                                          replace_na(stock_addition, 0) + 
                                                          replace_na(tourist, 0))),
    gap_bcp_use_to_other_uses    = pmax(replace_na(other, 0) + replace_na(stock_addition, 0) + replace_na(tourist, 0) - input_use, 0),
    
    # Shares of each product in this item's total use (country-year)
    share_biodiesel  = if_else(input_use > 0, biodiesel  / input_use, 0),
    share_ren_diesel = if_else(input_use > 0, ren_diesel / input_use, 0),
    share_biogasoline = if_else(input_use > 0, biogasoline / input_use, 0),
    
    # Allocate the gap proportionally
    gap_biodiesel  = gap_bcp_use_to_credible_uses * share_biodiesel,
    gap_ren_diesel = gap_bcp_use_to_credible_uses * share_ren_diesel,
    gap_biogasoline = gap_bcp_use_to_credible_uses * share_biogasoline,
    
    # Convert feedstock gap to biofuel supply gap via TCFs
    gap_supply_biodiesel  = gap_biodiesel  * biodiesel_tcf,
    gap_supply_ren_diesel = gap_ren_diesel * ren_diesel_tcf,
    gap_supply_biogasoline = gap_biogasoline * biogasoline_tcf,
    
    potential_supply_biodiesel   = gap_bcp_use_to_other_uses * biodiesel_tcf,
    potential_supply_ren_diesel  = gap_bcp_use_to_other_uses * ren_diesel_tcf,
    potential_supply_biogasoline = gap_bcp_use_to_other_uses * biogasoline_tcf
  ) %>%
  select(-starts_with("share"))

supply_gap_summary <- cbs_sua_adjusted %>%
  group_by(area_code, area, year) %>%
  summarise(
    gap_supply_biodiesel        = sum(gap_supply_biodiesel, na.rm = TRUE),
    gap_supply_ren_diesel       = sum(gap_supply_ren_diesel, na.rm = TRUE),
    gap_supply_biogasoline      = sum(gap_supply_biogasoline, na.rm = TRUE),
    potential_supply_biodiesel   = sum(potential_supply_biodiesel, na.rm = TRUE),
    potential_supply_ren_diesel  = sum(potential_supply_ren_diesel, na.rm = TRUE),
    potential_supply_biogasoline = sum(potential_supply_biogasoline, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!(gap_supply_biodiesel == 0 & gap_supply_ren_diesel == 0 & gap_supply_biogasoline == 0))



###########################################################
############# REALLOCATION ALGORITHM ################
###########################################################

# --- Step 0: Subset to (area_code, year) with nonzero gap supply ---

realloc <- cbs_sua_adjusted %>%
  group_by(area_code, year) %>%
  filter(sum(gap_supply_biodiesel + gap_supply_ren_diesel + gap_supply_biogasoline, na.rm = TRUE) > 0) %>%
  ungroup()

# --- Step 1: Compute country-year multipliers ---

cy_M <- realloc %>%
  group_by(area_code, year) %>%
  summarise(
    # BD + RD: feedstock units (single pool for oils)
    sum_gap_bd    = sum(gap_biodiesel, na.rm = TRUE),
    sum_gap_rd    = sum(gap_ren_diesel, na.rm = TRUE),
    sum_pot_oil   = sum(
      if_else(biodiesel_tcf > 0 | ren_diesel_tcf > 0, gap_bcp_use_to_other_uses, 0),
      na.rm = TRUE
    ),
    # Biogasoline: supply units (separate feedstock pool)
    sum_gap_bg    = sum(gap_supply_biogasoline, na.rm = TRUE),
    sum_pot_bg    = sum(potential_supply_biogasoline, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    sum_gap_bd_rd = sum_gap_bd + sum_gap_rd,
    
    # BD+RD: fraction of gap filled / fraction of potential consumed
    M_gap_bd_rd = if_else(sum_gap_bd_rd > 0, pmin(1, sum_pot_oil / sum_gap_bd_rd), NA_real_),
    M_pot_bd_rd = if_else(sum_pot_oil   > 0, pmin(1, sum_gap_bd_rd / sum_pot_oil), NA_real_),
    
    # Biogasoline
    M_gap_bg    = if_else(sum_gap_bg > 0, pmin(1, sum_pot_bg / sum_gap_bg), NA_real_),
    M_pot_bg    = if_else(sum_pot_bg > 0, pmin(1, sum_gap_bg / sum_pot_bg), NA_real_),
    
    # Split potential feedstock between BD and RD proportional to their gaps
    share_bd = if_else(sum_gap_bd_rd > 0, sum_gap_bd / sum_gap_bd_rd, 0),
    share_rd = if_else(sum_gap_bd_rd > 0, sum_gap_rd / sum_gap_bd_rd, 0)
  )

# --- Step 2: Apply adjustments at item level ---

realloc <- realloc %>%
  left_join(
    cy_M %>% select(area_code, year, M_gap_bd_rd, M_pot_bd_rd,
                    M_gap_bg, M_pot_bg, share_bd, share_rd),
    by = c("area_code", "year")
  ) %>%
  mutate(
    is_oil = (biodiesel_tcf > 0 | ren_diesel_tcf > 0),
    
    # Biodiesel: decrease on gap items, increase on potential items
    biodiesel_adj = case_when(
      gap_biodiesel > 0 & !is.na(M_gap_bd_rd) ~
        biodiesel - M_gap_bd_rd * gap_biodiesel,
      gap_bcp_use_to_other_uses > 0 & is_oil & !is.na(M_pot_bd_rd) ~
        biodiesel + M_pot_bd_rd * gap_bcp_use_to_other_uses * share_bd,
      TRUE ~ biodiesel
    ),
    
    # Renewable diesel
    ren_diesel_adj = case_when(
      gap_ren_diesel > 0 & !is.na(M_gap_bd_rd) ~
        ren_diesel - M_gap_bd_rd * gap_ren_diesel,
      gap_bcp_use_to_other_uses > 0 & is_oil & !is.na(M_pot_bd_rd) ~
        ren_diesel + M_pot_bd_rd * gap_bcp_use_to_other_uses * share_rd,
      TRUE ~ ren_diesel
    ),
    
    # Biogasoline (independent feedstock pool)
    biogasoline_adj = case_when(
      gap_biogasoline > 0 & !is.na(M_gap_bg) ~
        biogasoline - M_gap_bg * gap_biogasoline,
      gap_bcp_use_to_other_uses > 0 & biogasoline_tcf > 0 & !is.na(M_pot_bg) ~
        biogasoline + M_pot_bg * gap_bcp_use_to_other_uses,
      TRUE ~ biogasoline
    ),
    
    input_use_adj = biodiesel_adj + ren_diesel_adj + biogasoline_adj
  )

# --- Step 3: Remaining gaps in feedstock (use) terms ---

realloc <- realloc %>%
  mutate(
    gap_use_bd_remaining = replace_na(if_else(!is.na(M_gap_bd_rd) & biodiesel_tcf > 0,
                                              (1 - M_gap_bd_rd) * gap_biodiesel,
                                              gap_biodiesel), 0),
    gap_use_rd_remaining = replace_na(if_else(!is.na(M_gap_bd_rd) & ren_diesel_tcf > 0,
                                              (1 - M_gap_bd_rd) * gap_ren_diesel,
                                              gap_ren_diesel), 0),
    gap_use_bg_remaining = replace_na(if_else(!is.na(M_gap_bg) & biogasoline_tcf > 0,
                                              (1 - M_gap_bg) * gap_biogasoline,
                                              gap_biogasoline), 0)
  )
# --- Step 4: Final output ---

realloc_output <- realloc %>%
  select(area_code, area, year, item, comm_code,
         biodiesel, ren_diesel, biogasoline, input_use,
         biodiesel_adj, ren_diesel_adj, biogasoline_adj, input_use_adj,
         gap_use_bd_remaining, gap_use_rd_remaining, gap_use_bg_remaining) %>%
  mutate(diff = input_use_adj - input_use)





###########################################################
############# JOINING REMAINING GAPS FOR ADJUSTMENTS OF USE IN FINAL DEMAND ################
###########################################################

realloc_summary <- realloc %>%
  group_by(area_code, year, item_code) %>%
  summarise(
    gap_use_remaining_total = sum(
      gap_use_bd_remaining + gap_use_rd_remaining + gap_use_bg_remaining,
      na.rm = TRUE
    ),
    input_use_adj = input_use_adj,
    .groups = "drop"
  )

cbs_sua_adjusted <- cbs_sua_adjusted %>%
  left_join(realloc_summary, by = c("area_code", "year", "item_code")) %>%
  mutate(gap_use_remaining_total = replace_na(gap_use_remaining_total, 0),
         input_use_adj = coalesce(input_use_adj, input_use, 0))

###########################################################
# Step A: Collect Maize and products rows for Sweden
###########################################################

maize_sweden_gaps <- cbs_sua_adjusted %>%
  filter(item == "Maize and products", area == "Sweden") %>%
  select(area_code, area, year, item, item_code, gap_use_remaining_total) %>%
  filter(gap_use_remaining_total > 0)

###########################################################
# Step B: Build new rows for sup_final_bcp
#   proc == "Biogasoline production", item == "Other, Waste"
#   supply == gap_use_remaining_total from Maize/Sweden
###########################################################

# Look up item_code (and comm_code if present) for "Other, Waste"
other_waste_lookup <- items_full_bcp %>%
  filter(item == "Other, Waste") %>%
  select(any_of(c("item", "item_code", "comm_code"))) %>%
  distinct()

new_sup_rows <- maize_sweden_gaps %>%
  transmute(
    area_code,
    area,
    year,
    proc = "Biogasoline production",
    item = "Other, Waste",
    supply = gap_use_remaining_total
  ) %>%
  left_join(other_waste_lookup, by = "item")

# Align to sup_final_bcp's columns (fills missing cols with NA, drops extras)
new_sup_rows <- new_sup_rows %>%
  select(any_of(names(sup_final_bcp))) %>%
  mutate(proc = "Other, Waste collection") %>%
  group_by(area_code, year, proc, item) %>%
  summarise(add_supply = sum(supply, na.rm = TRUE), .groups = "drop")

sup_final_bcp <- sup_final_bcp %>%
  left_join(new_sup_rows, by = c("area_code", "year", "proc", "item")) %>%
  mutate(supply = supply + replace_na(add_supply, 0)) %>%
  select(-add_supply)

###########################################################
# Step C: Build matching new rows for use_rebal_bcp
###########################################################

new_use_rows <- maize_sweden_gaps %>%
  transmute(
    area_code,
    area,
    year,
    proc = "Biogasoline production",
    item = "Other, Waste",
    unit = "tonnes",
    use = gap_use_remaining_total
  ) %>%
  left_join(other_waste_lookup, by = "item") %>%
  left_join(biogasoline_proc_code, by = "proc") %>%
  select(any_of(names(use_rebal_bcp)))

use_rebal_bcp <- bind_rows(use_rebal_bcp, new_use_rows)

###########################################################
# Step D: Reset gap_use_remaining_total to 0 for the
#         collected Maize/Sweden rows in cbs_sua_adjusted
###########################################################

cbs_sua_adjusted <- cbs_sua_adjusted %>%
  mutate(
    gap_use_remaining_total = if_else(
      item == "Maize and products" & area == "Sweden",
      0,
      gap_use_remaining_total
    )
  ) 


###########################################################
############# JOINING ADJUSTED FEEDSTOCK USE TO USE TABLE ################
###########################################################

# Pivot adjusted uses back to long format
adj_long <- realloc_output %>%
  select(area_code, year, item, comm_code,
         biodiesel = biodiesel_adj,
         ren_diesel = ren_diesel_adj,
         biogasoline = biogasoline_adj) %>%
  pivot_longer(
    cols = c(biodiesel, ren_diesel, biogasoline),
    names_to = "proc_label",
    values_to = "use_adj"
  ) %>%
  mutate(proc = case_when(
    proc_label == "biodiesel"   ~ "Biodiesel production",
    proc_label == "ren_diesel"  ~ "Renewable diesel production",
    proc_label == "biogasoline" ~ "Biogasoline production"
  )) %>%
  select(area_code, year, item, proc, use_adj)

# Update use_rebal_bcp
use_rebal_bcp <- use_rebal_bcp %>%
  left_join(adj_long, by = c("area_code", "year", "item", "proc")) %>%
  mutate(use = if_else(!is.na(use_adj), use_adj, use)) %>%
  select(-use_adj)


###########################################################
########### SAVING TABLES #########
###########################################################

setwd("/home/mmondolfo/fabio_bfp/")

# Finished
saveRDS(sup_final_bcp,  "data/sup_final_bcp.rds")

# Intermediate
saveRDS(use_rebal_bcp,  "intermediate_data/use_rebal_bcp.rds")
saveRDS(cbs_sua_adjusted, "intermediate_data/cbs_sua_adjusted.rds")

rm(list = ls())