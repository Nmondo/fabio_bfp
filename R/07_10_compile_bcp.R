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

############# Loading all the data necessary for final compilation ##################

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
setwd(file.path(fabio_root, "inputs_for_final_data"))

files <- c(
  "btd_final_bf.rds",
  "btd_final_bp_bf_coproducts.rds",
  "supply_final_bf.rds",
  "supply_final_bp.rds",
  "use_final_bf.rds",
  "use_final_bp.rds",
  "y_bp_complete_rows.rds",
  "y_bp_incomplete_rows.rds",
  "y_final_bf.rds",
  "y_final_bf_coproducts.rds"
)

invisible(lapply(files, function(f) {
  assign(tools::file_path_sans_ext(f), readRDS(f), envir = .GlobalEnv)
}))

############# Loading lists of items, processes, regions ##################

setwd(fabio_root)
regions <- read.csv("inst/regions_full.csv", fileEncoding = "latin1") %>% filter(current == TRUE)
items_supply_bcp <- read.csv("inst/items_supply_bcp.csv")
items_full_bcp <- read.csv("inst/items_full_bcp.csv")
items_use_bcp <- read.csv("inst/items_use_bcp.csv")


###########################################################
########### BCP ITEM-NAME ALIASES  [FIX] ##################
###########################################################
# EVERY table below joins to items_*_bcp BY NAME. That makes the item string a hard join
# key with no fallback, and a single mismatch silently produces rows with item_code = NA
# and comm_code = NA — a flow of *nothing*, which nothing downstream ever noticed.
#
# The known offender: the trade data calls item 654 "Dried distillers grains with solubles"
# (coined in 07_04:606 from HS 230330); items_full_bcp calls it "Brewing or distilling
# dregs and waste". The join MISSES, and 148.8 Mt of DDGS trade across 5,196 rows arrives
# in btd_final_bcp with a blank comm_code — 18 Mt of countries "shipping" an unnamed
# commodity, the single largest entry in 11's ghost check.
#
# Aliased HERE and not in 07_04, because 07_04's name is LOAD-BEARING: prices_bcp.rds
# carries it and script 11 filters on that exact string to price c171 for the co-product
# value allocation. Rename it at source and you break that silently.
#
# The two existing renames (Castor oil, Used cooking oil) are folded in, so all name
# normalisation now happens in ONE place, applied identically to all four tables.
BCP_ITEM_ALIAS <- c(
  "Dried distillers grains with solubles" = "Brewing or distilling dregs and waste",
  "Castor oil"                            = "Oil of castor beans",
  "Used cooking oil"                      = paste0(
    "Animal or vegetable fats and oils and their fractions, chemically modified, except ",
    "those hydrogenated, inter-esterified, re-esterified or elaidinized; inedible ",
    "mixtures or preparations of animal or vegetable fats or oils")
)

alias_item <- function(x) {
  x <- trimws(x)                                   # guard the " Triticale" class of bug
  unname(ifelse(x %in% names(BCP_ITEM_ALIAS), BCP_ITEM_ALIAS[x], x))
}

# Fail loudly if a name-join left a row without a commodity. Called before every write.
check_comm <- function(tbl, tbl_name, value_col) {
  bad <- tbl %>%
    filter(is.na(comm_code) | comm_code == "") %>%
    group_by(item, item_code) %>%
    summarise(t = sum(.data[[value_col]], na.rm = TRUE), n = dplyr::n(), .groups = "drop")
  if (nrow(bad)) {
    print(as.data.frame(bad))
    stop("07_10: ", nrow(bad), " item(s) in `", tbl_name, "` have NO comm_code after the ",
         "name-join (", format(round(sum(bad$t)), big.mark = ","), " units, ", sum(bad$n),
         " rows). They would become flows of nothing. Add them to BCP_ITEM_ALIAS, or fix ",
         "the name in inst/items_*_bcp.csv.")
  }
  message(sprintf(">>> 07_10: %-18s OK — every row has a comm_code.", tbl_name))
  invisible(TRUE)
}


###########################################################
########### FORMATTING USE TABLES #########
###########################################################

use_compiled_bcp <- bind_rows(use_final_bf, use_final_bp) %>%
  # converting all liquid fuels to liters
  mutate(use = case_when(item == "Bionaphtha" ~ (1/0.71)*use,
                         item == "Biopropane" ~ (1/0.51)*use,
                         TRUE ~ use),
         unit = ifelse(item %in% c("Biopropane", "Bionaphtha"), "Ml", unit)) %>%
  # Standardizing units to tonnes or 1000 liters
  mutate(use = case_when(unit %in% c("kt","Ml") ~ use*1000,
                         TRUE ~ use),
         unit = case_when(unit == "kt" ~ "tonnes",
                          unit == "Ml" ~ "1000 liters",
                          TRUE ~ unit),
         item = alias_item(item)) %>%                          # [FIX] one place, all names
  left_join(items_use_bcp, by = c("proc", "item")) %>%
  left_join(regions %>% select(iso3c, area_code = code, area = name), by = c("iso3c")) %>%
  filter(year %in% 2012:2022,
         !is.na(area)) %>%
  select(-iso3c)

check_comm(use_compiled_bcp, "use_compiled_bcp", "use")

subset(use_compiled_bcp, area == "Italy" & proc == "Biogasoline production")


###########################################################
########### FORMATTING FINAL DEMAND TABLES #########
###########################################################

use_fd_final_bcp <- bind_rows(y_final_bf, y_final_bf_coproducts, y_bp_complete_rows) %>%
  mutate(across(c(fuel, non_fuel, unknown_use, other_industry_use),
                ~ case_when(item == "Bionaphtha" ~ (1/0.71)*.x,
                            item == "Biopropane" ~ (1/0.51)*.x,
                            TRUE ~ .x)),
         unit = ifelse(item %in% c("Biopropane", "Bionaphtha"), "Ml", unit)) %>%
  mutate(across(c(fuel, non_fuel, unknown_use, other_industry_use),
                ~ case_when(unit %in% c("kt","Ml") ~ .x*1000,
                            TRUE ~ .x)),
         unit = case_when(unit == "kt" ~ "tonnes",
                          unit == "Ml" ~ "1000 liters",
                          TRUE ~ unit),
         item = alias_item(item),                              # [FIX]
         other_industrial = coalesce(non_fuel, other_industry_use)) %>%
  select(-non_fuel, -other_industry_use) %>%
  left_join(items_full_bcp %>% select(comm_code, item_code, item), by = c("item")) %>%
  left_join(regions %>% select(iso3c, area_code = code, area = name), by = "iso3c")


###########################################################
#1. Subtracting Bioethanol use as a BP feedstock from "Non-fuel use" #########
###########################################################

use_fd_final_bcp <- use_fd_final_bcp %>%
  left_join(use_compiled_bcp %>%
              filter(item=="Biogasoline") %>%
              select(item, year, to_subtract = use, area), by = c("item", "year", "area")) %>%
  mutate(other_industrial = ifelse(!is.na(to_subtract), other_industrial - to_subtract, other_industrial)) %>%
  relocate(c(other_industrial, unknown_use), .after = fuel) %>%
  relocate(c(comm_code, item_code), .after = item) %>%
  select(-to_subtract)

check_comm(use_fd_final_bcp, "use_fd_final_bcp", "fuel")

# Then there is y_bp_incomplete_rows which needs monetary MRIO linking


###########################################################
########### FORMATTING SUPPLY TABLES #########
###########################################################

supply_final_bcp <- bind_rows(supply_final_bf, supply_final_bp) %>%
  rename(item = product) %>%
  mutate(supply = case_when(item == "Bionaphtha" ~ (1/0.71)*supply,
                            item == "Biopropane" ~ (1/0.51)*supply,
                            TRUE ~ supply),
         unit = ifelse(item %in% c("Biopropane", "Bionaphtha"), "Ml", unit)) %>%
  mutate(supply = case_when(unit %in% c("kt","Ml") ~ supply*1000,
                            TRUE ~ supply),
         unit = case_when(unit == "kt" ~ "tonnes",
                          unit == "Ml" ~ "1000 liters",
                          TRUE ~ unit),
         item = alias_item(item))

# --- attach proc / comm_code  [FIX] ---------------------------------------------------
# supply_final_bp (07_08) has NO `proc` column — only iso3c/product/year/supply/unit. The
# original join was `by = c("proc", "item")`, so every biopolymer row joined on a key that
# bind_rows had filled with NA: no match, comm_code = NA, and 23.6 Mt of BP supply across
# 1,129 rows was silently discarded. Downstream that is fatal, not cosmetic: 12_a builds
# mr_sup from comm_code, so a NULL comm_code = NO SUPPLY COLUMN = an empty Z column = a
# ZERO footprint for the entire biopolymer chain. It is the same failure mode as the palm
# oil phantom, arriving from the supply side.
#
# The BP building blocks are single-process commodities (p128 -> c152 L-LA, p136 -> c160
# MEG, p138 -> c162 ECH, p139 -> c163 PLA, ...), so `item` alone is a sufficient key and
# items_supply_bcp supplies the proc. Join on (proc, item) where proc is present (the
# biofuel rows, which are multi-process), and on item alone where it is not.
sup_lu_pi <- items_supply_bcp %>% select(proc, item, proc_code, comm_code, item_code)
sup_lu_i  <- items_supply_bcp %>%
  add_count(item, name = "n_proc") %>%
  filter(n_proc == 1) %>%                      # unambiguous: one process makes it
  select(item, proc_i = proc, proc_code_i = proc_code,
         comm_code_i = comm_code, item_code_i = item_code)

supply_final_bcp <- supply_final_bcp %>%
  left_join(sup_lu_pi, by = c("proc", "item")) %>%
  left_join(sup_lu_i,  by = "item") %>%
  mutate(proc      = coalesce(proc,      proc_i),
         proc_code = coalesce(proc_code, proc_code_i),
         comm_code = coalesce(comm_code, comm_code_i),
         item_code = coalesce(item_code, item_code_i)) %>%
  select(-ends_with("_i")) %>%
  left_join(regions %>% select(iso3c, area_code = code, area = name), by = c("iso3c")) %>%
  filter(year %in% 2012:2022, !is.na(area)) %>%
  select(-iso3c)


# Adding supply of "Other, Unknown" and "Other, Waste" (own supply)

# 1. Aggregate supply from use_compiled_bcp
supply_sums <- use_compiled_bcp %>%
  filter(comm_code %in% c("c901", "c999")) %>%
  group_by(year, comm_code, area_code) %>%
  summarise(supply = sum(use, na.rm = TRUE), .groups = "drop")

# 2. Build scaffold
new_rows <- expand_grid(
  comm_code = c("c901", "c999"),
  area_code = unique(supply_final_bcp$area_code),
  year      = 2012:2022
) %>%
  mutate(
    unit      = "tonnes"
  ) %>%
  # 3. Attach aggregated supply
  left_join(supply_sums, by = c("comm_code", "area_code", "year")) %>%
  mutate(supply = ifelse(is.na(supply), 0, supply)) %>%
  # 4. Attach proc_code and proc
  left_join(
    items_supply_bcp %>% select(comm_code, item, item_code, proc_code, proc),
    by = "comm_code"
  ) %>%
  left_join(regions %>% select(area_code = code, area = name), by = "area_code")

# 6. Push into supply_final_bcp
supply_final_bcp <- supply_final_bcp %>%
  bind_rows(new_rows)

check_comm(supply_final_bcp, "supply_final_bcp", "supply")


###########################################################
########### FORMATTING BTD TABLES #########
###########################################################

btd_final_bcp <- bind_rows(btd_final_bf, btd_final_bp_bf_coproducts) %>%
  rename(item = product) %>%
  mutate(value = case_when(item == "Bionaphtha" ~ (1/0.71)*value,
                           item == "Biopropane" ~ (1/0.51)*value,
                           TRUE ~ value),
         unit = ifelse(item %in% c("Biopropane", "Bionaphtha"), "Ml", unit)) %>%
  mutate(value = case_when(unit %in% c("kt","Ml") ~ value*1000,
                           TRUE ~ value),
         unit = case_when(unit == "kt" ~ "tonnes",
                          unit == "Ml" ~ "1000 liters",
                          TRUE ~ unit),
         item = alias_item(item)) %>%                          # [FIX] the 148.8 Mt of DDGS
  select(-type) %>%
  left_join(regions %>% select(from_code = code, iso3c), by = c("exporter_iso3" = "iso3c")) %>%
  left_join(regions %>% select(to_code = code, iso3c), by = c("importer_iso3" = "iso3c")) %>%
  left_join(items_full_bcp %>% select(item, item_code, comm_code), by = "item") %>%
  select(-exporter_iso3, -importer_iso3)

check_comm(btd_final_bcp, "btd_final_bcp", "value")


###########################################################
########### WRITING TABLES #########
###########################################################

setwd(fabio_root)

write_rds(use_compiled_bcp, "intermediate_data/use_compiled_bcp.rds")

write_rds(use_fd_final_bcp, "data/use_fd_final_bcp.rds")
write_rds(supply_final_bcp, "data/sup_final_bcp.rds")
write_rds(btd_final_bcp, "data/btd_final_bcp.rds")

rm(list = ls())