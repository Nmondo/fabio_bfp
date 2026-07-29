###########################################################
########### LOADING PACKAGES #########
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

library("data.table")
library(dplyr)
source("R/00_system_variables.R")


###########################################################
########### LOADING DATA #########
###########################################################

regions <- fread("inst/regions_full.csv")[current==TRUE]
items <- fread("inst/items_full_bcp.csv")

# FABIO tables ------------------------------------------------------------------

btd <- readRDS("data/btd_final.rds")
cbs <- readRDS("data/cbs_full.rds")

# Use for BCP production -----------------------------------------------------------------

use_bcp <- readRDS("intermediate_data/use_compiled_bcp.rds")

# SUA for shares in use ------------------------------------------------------------------

sua <- readRDS("data/tidy/sua_tidy.rds") 

###########################################################
########### CREATING MISSING ROWS #########
###########################################################

# Collecting the list of countries to add
cbs_sua_ext <- subset(cbs, item_code %in% c(97, 165, 265, 266, 1274))
to_add <- setdiff(unique(regions$code), unique(cbs_sua_ext$area_code))


# Build the full combination of missing areas x years x items
missing_rows <- CJ(
  area_code  = to_add,
  year       = 2012:2022,
  item_code  = unique(cbs_sua_ext$item_code)
)

# Add all other columns from cbs_sua_ext, filled with NA
other_cols <- setdiff(names(cbs_sua_ext), c("area_code", "year", "item_code"))
missing_rows[, (other_cols) := NA_real_]

# Make sure column order matches cbs_sua_ext
setcolorder(missing_rows, names(cbs_sua_ext))




###########################################################
########### CALCULATING TOTAL EXPORTS AND IMPORTS #########
###########################################################

btd <- subset(btd, item_code %in% unique(missing_rows$item_code) & 
                (from_code %in% unique(missing_rows$area_code) | to_code %in% unique(missing_rows$area_code)) &
                to_code != from_code
)

ex_join <- btd %>%
  group_by(from_code, year, item_code) %>%
  summarise(exports = sum(ifelse(value == "NaN", 0, value)),
            .groups = "drop") %>%
  rename(area_code = from_code)


im_join <- btd %>%
  group_by(to_code, year, item_code) %>%
  summarise(imports = sum(ifelse(value == "NaN", 0, value)),
            .groups = "drop") %>%
  rename(area_code = to_code)

missing_rows <- missing_rows %>% 
  rows_patch(ex_join, by = c("area_code", "item_code", "year"), unmatched = "ignore") %>% 
  rows_patch(im_join, by = c("area_code", "item_code", "year"), unmatched = "ignore") %>%
  mutate(item = as.character(item),
         area = as.character(area))

missing_rows <- missing_rows %>%
  left_join(items %>% select(item, item_code) %>% distinct(item_code, .keep_all = TRUE),
            by = "item_code", suffix = c("", "_new")) %>%
  mutate(item = coalesce(item, item_new)) %>%
  select(-item_new) %>%
  rows_update(regions %>% select(area = name, area_code = code), by = "area_code", unmatched = "ignore")




###########################################################
########### JOINING ESTIMATED USE #########
###########################################################

# Calculating world shares by use for missing items
sua_shares <- sua %>%
  filter(item_code %in% c(97, 165, 265, 266, 1274)) %>%
  group_by(item_code, year) %>%
  summarise(
    across(c(feed, food, losses, other, processing, seed, tourist),
           ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    total = feed + food + losses + other + processing + seed + tourist,
    across(c(feed, food, losses, other, processing, seed, tourist),
           ~ .x / total)
  ) %>%
  mutate(across(c(feed, food, losses, processing, seed, tourist), 
                ~ ifelse(item_code == 1274, 0, .x)),
         other = ifelse(item_code == 1274, 1, other)) %>%
  select(-total)

# Computing use
join_missing_use <- use_bcp %>%
  filter(item_code %in% c(97, 165, 265, 266, 1274) & area_code %in% unique(missing_rows$area_code)) %>%
  group_by(area_code, year, item_code) %>%
  summarise(input_use = sum(use, na.rm = TRUE),
            .groups = "drop")

# Estimating production (input_use + exports - imports > 0) or missing use (< 0)
missing_rows <- missing_rows %>% 
  left_join(join_missing_use, by = c("item_code", "area_code", "year")) %>%
  mutate(
    production_raw = ifelse(is.na(input_use), 0, input_use) + exports - imports,
    residual = abs(pmin(production_raw, 0)),   # negative residual (≤ 0), else 0
    production     = pmax(production_raw, 0)    # floored at 0
  ) %>%
  select(-production_raw)


# Estimating the distribution of uses from world shares
use_shares <- missing_rows %>%
  select(item_code, area_code, year, residual) %>%
  left_join(sua_shares, by = c("item_code", "year")) %>%
  mutate(
    across(c(feed, food, losses, other, processing, seed, tourist),
           ~ residual * .x)
  ) %>%
  select(item_code, area_code, year, feed, food, losses, other, processing, seed, tourist)

missing_rows <- missing_rows %>%
  rows_update(use_shares, by = c("item_code", "area_code", "year")) %>%
  select(-input_use, -residual)


cbs <- bind_rows(cbs, missing_rows)


###########################################################
########### CREATING MISSING BF-FEEDSTOCK ROWS ############
###########################################################
# The block above mints only the five SUA items for WHOLLY-absent countries. Feedstocks a
# country burns as biodiesel/HVO/biogasoline INPUT but has no cbs row for are still missing,
# so their input_use is dropped at 08_02's left_join and 08_04's update-join. Mint them here.
#
# TRADE FROM btd_full, NOT btd (=btd_final): 05/06 zeroed these cells in every balanced btd;
# only btd_full keeps their real trade. This is load-bearing for production below — with real
# imports, pmax(input_use + exports - imports, 0) is ~0 for a pure importer; with zeroed
# imports it mints the whole requirement as phantom production (Bug 1).

setDT(cbs); setDT(use_bcp); setDT(sua)
bf_procs <- c("Biodiesel production", "Renewable diesel production", "Biogasoline production")

## (1) feedstock demand = the bf-feedstock universe, per (area, year, item) ----------------
feed_need <- use_bcp[proc %in% bf_procs & !is.na(item_code) & year %in% 2012:2022,
                     .(input_use = sum(use, na.rm = TRUE)),
                     by = .(area_code, year, item_code)][input_use > 0]
feed_need[, `:=`(area_code = as.numeric(area_code), year = as.numeric(year),
                 item_code = as.integer(item_code))]

## (2) drop cells cbs already has (incl. the SUA-5 rows just bound) ------------------------
have <- unique(cbs[, .(area_code = as.numeric(area_code), year = as.numeric(year),
                       item_code = as.integer(item_code))])
feed_missing <- feed_need[!have, on = .(area_code, year, item_code)]

## (3) imports/exports from btd_full — CONFIRM UNIT FIRST, then set UNIT_DIV ---------------
##   as.data.table(readRDS("data/btd_full.rds"))[item_code %in% unique(feed_missing$item_code),
##       .N, by = .(item_code, unit)]      # single-unit kg -> 1000 ; genuine tonnes -> 1
UNIT_DIV <- 1000
btd_raw <- as.data.table(readRDS("data/btd_full.rds"))[
  from_code != to_code & item_code %in% unique(feed_missing$item_code) & year %in% 2012:2022]
btd_raw[, value := as.numeric(value) / UNIT_DIV][!is.finite(value), value := 0]
ex_feed <- btd_raw[, .(exports = sum(value)),
                   by = .(area_code = as.numeric(from_code), year = as.numeric(year),
                          item_code = as.integer(item_code))]
im_feed <- btd_raw[, .(imports = sum(value)),
                   by = .(area_code = as.numeric(to_code), year = as.numeric(year),
                          item_code = as.integer(item_code))]
feed_missing[ex_feed, exports := i.exports, on = .(area_code, year, item_code)]
feed_missing[im_feed, imports := i.imports, on = .(area_code, year, item_code)]
feed_missing[is.na(exports), exports := 0][is.na(imports), imports := 0]

## (4) production = input_use + exports - imports, floored; over-import -> residual --------
feed_missing[, praw := input_use + exports - imports]
feed_missing[, `:=`(residual = abs(pmin(praw, 0)), production = pmax(praw, 0), praw = NULL)]

## (5) spread the over-import residual across uses by world shares
use_c  <- c("feed","food","losses","other","processing","seed","tourist")
sua_sh <- sua[item_code %in% unique(feed_missing$item_code),
              lapply(.SD, sum, na.rm = TRUE), by = .(item_code, year), .SDcols = use_c]
sua_sh[, tot := rowSums(.SD), .SDcols = use_c]
sua_sh[tot > 0,  (use_c) := lapply(.SD, function(x) x / tot), .SDcols = use_c]
sua_sh[tot == 0, (use_c) := 0][tot == 0, other := 1]
sua_sh[, tot := NULL]
setnames(sua_sh, use_c, paste0(use_c, "_sh"))          # <- rename here, unconditionally

feed_missing <- merge(feed_missing, sua_sh, by = c("item_code","year"), all.x = TRUE)

for (uc in use_c)
  feed_missing[, (uc) := residual * fcoalesce(get(paste0(uc, "_sh")),
                                              if (uc == "other") 1 else 0)]
feed_missing[, c(paste0(use_c, "_sh"), "residual", "input_use") := NULL]

## (6) labels + zero the residual/balancing scaffolding (comm_code is NOT a cbs column) ----
feed_missing[items,   item := i.item, on = "item_code"]
feed_missing[regions, area := i.name, on = c(area_code = "code")]
feed_missing[, `:=`(residuals = 0, balancing = 0)]

## (7) append — 163-166 recompute domestic_supply/supply/domestic_use/use for these too ----
cbs <- rbindlist(list(cbs, feed_missing), use.names = TRUE, fill = TRUE)

message(sprintf(">>> 08_01: minted %d bf-feedstock rows / %d items / %d areas; %.0f t imports, %.0f t exports (btd_full/%d); %.0f t production floored.",
                nrow(feed_missing), uniqueN(feed_missing$item_code), uniqueN(feed_missing$area_code),
                sum(feed_missing$imports), sum(feed_missing$exports), UNIT_DIV, sum(feed_missing$production)))



cbs[, `:=`(domestic_supply = production)]
cbs[, `:=`(supply = na_sum(domestic_supply, imports))]
cbs[, `:=`(domestic_use = na_sum(food, feed, other, tourist, seed, losses, processing, stock_addition, -stock_withdrawal))]
cbs[, `:=`(use = na_sum(domestic_use, exports))]



###########################################################
########### SAVING FULL CBS WITH SUA EXTENSION #########
###########################################################

setwd(fabio_root)

saveRDS(cbs, "data/cbs_sua_full.rds")

rm(list = ls())