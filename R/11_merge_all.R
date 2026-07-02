rm(list = ls())

###########################################################
########### LOADING PACKAGES #########
###########################################################

library("tidyverse")
library("data.table")
library(Matrix)
library(dplyr)

source("/home/mmondolfo/fabio_bfp/R/00_run_config.R")  # RUN_MODE / BYPASS_RESCALE / tag()

###########################################################
########### LOADING DATA #########
###########################################################

setwd("/home/mmondolfo/fabio_bfp/data/")

yr_keep <- function(x) dplyr::filter(x, year %in% 2012:2022)

# --- mode-INDEPENDENT BCP-extension tables (never rescaled) -------------------
use_fd_final_bcp <- yr_keep(readRDS("use_fd_final_bcp.rds"))
sup_final_bcp    <- yr_keep(readRDS("sup_final_bcp.rds"))
btd_final_bcp    <- yr_keep(readRDS("btd_final_bcp.rds"))

# --- mode-DEPENDENT CBS-side tables from 10_1a (tag() -> *_noresc in bypass) ---
use_final    <- yr_keep(readRDS(tag("use_final.rds")))
use_fd_final <- yr_keep(readRDS(tag("use_fd_final.rds")))
sup_final    <- yr_keep(readRDS(tag("sup_final.rds")))

# --- mode-DEPENDENT biofuel use + btd from 08_03 (bypass -> pre-rescale sources)
#     object names are kept identical so the binding code below is untouched.
if (BYPASS_RESCALE) {
  btd_final_resc <- yr_keep(readRDS("btd_final.rds"))                            # un-topped-up c145
  use_final_bcp  <- yr_keep(readRDS("../intermediate_data/use_rebal_bcp.rds"))   # pre-rescale use
  message(">>> [BYPASS] 11 using non-rescaled btd_final + use_rebal_bcp (08_03 ignored)")
} else {
  btd_final_resc <- yr_keep(readRDS("btd_final_resc.rds"))
  use_final_bcp  <- yr_keep(readRDS("use_final_bcp.rds"))
}


setwd("/home/mmondolfo/fabio_bfp/")

y_bp_incomplete_rows <- readRDS("inputs_for_final_data/y_bp_incomplete_rows.rds") 
items_use_bcp <- read_csv("inst/items_use_bcp.csv")
items_full_bcp <- read_csv("inst/items_full_bcp.csv")
items_supply_bcp <- read_csv("inst/items_supply_bcp.csv")
regions <- read_csv("inst/regions_full.csv") %>% filter(current == TRUE)
source("R/00_system_variables.R")

# ---- c901 waste trade flows from 08_03 (source for btd + supply of c901) ----
waste_flows <- as.data.frame(readRDS("data/waste_flows.rds"))
waste_flows <- waste_flows[waste_flows$year %in% 2012:2022, ]
# 08_03 may store item_code = NA for c901; use the items_full_bcp code (as the FD rows do)
c901_item_code <- items_full_bcp %>% filter(comm_code == "c901") %>% pull(item_code) %>% unique()
stopifnot(length(c901_item_code) == 1)
waste_flows$item_code <- c901_item_code

# In the no-rescale counterfactual the RED-driven c901 channel does not exist:
# drop the waste_flows rows (schema preserved) so c901 is self-sourced from the
# pre-rescale use table only. Flip BYPASS_KEEP_WASTE in 00_run_config.R to retain.
if (BYPASS_RESCALE && !BYPASS_KEEP_WASTE) {
  waste_flows <- waste_flows[0, , drop = FALSE]
  message(">>> [BYPASS] 11 dropped RED waste_flows; c901 self-sourced from pre-rescale use only")
}

###########################################################
########### MAKING VECTORS#########
###########################################################

extension_items <- unique(use_fd_final_bcp$item)
current_codes <- regions$code

###########################################################
########### BINDING USE #########
###########################################################

use_full <- bind_rows(use_final, use_final_bcp) %>%
  select(-unit) %>%
  filter(area_code %in% current_codes)


# self-source c901 use that waste_flows doesn't cover (placed right after edit 1)
c901_use <- as.data.table(as.data.frame(use_full))[comm_code == "c901",
                                                   .(use = sum(use)), by = .(area_code, year)]
c901_in   <- as.data.table(as.data.frame(waste_flows))[, .(inflow = sum(value)),
                                                       by = .(area_code = to_code, year)]
short <- merge(c901_use, c901_in, by = c("area_code", "year"), all.x = TRUE)
short[is.na(inflow), inflow := 0][, short := use - inflow]

# sanity: confirm there are no inflow > use cells (self-flows can't fix those)
if (nrow(short[short < -1])) warning("c901: ", nrow(short[short < -1]),
                                     " cells have inflow > use (over-allocated) — inspect before trusting the close")

self_add <- short[short > 1, .(from_code = area_code, to_code = area_code,
                               value = short, year,
                               item_code = c901_item_code, comm_code = "c901")]
waste_flows <- rbind(as.data.frame(waste_flows), as.data.frame(self_add))

# collapse any duplicate diagonal (a partial-inflow country getting a 2nd self-row)
waste_flows <- as.data.frame(as.data.table(waste_flows)[,
                                                        .(value = sum(value)), by = .(from_code, to_code, year, item_code, comm_code)])




###########################################################
########### BINDING USE IN FINAL DEMAND #########
###########################################################

use_fd_full <- bind_rows(use_fd_final, use_fd_final_bcp) %>% 
  select(-iso3c, -unit) %>%
  mutate(across(c(food, losses, other, stock_addition, stock_withdrawal, tourist), 
                ~ if_else(item %in% extension_items, 0, .x)),
         across(c(fuel, other_industrial, unknown_use), 
                ~ case_when(item %in% extension_items & is.na(.x) ~ 0, 
                            ! item %in% extension_items           ~ 0,
                            TRUE                                  ~ .x))
         )  %>%
  filter(area_code %in% current_codes)


# Item info for the two comm_codes
new_items <- items_full_bcp %>%
  filter(comm_code %in% c("c901", "c999")) %>%
  select(comm_code, item_code, item) %>%
  distinct()

# All combinations of area_code × comm_code × year
new_fd_rows <- expand.grid(
  area_code = unique(use_fd_full$area_code),
  comm_code = c("c901", "c999"),
  year      = 2012:2022,
  stringsAsFactors = FALSE
) %>%
  left_join(new_items, by = "comm_code") %>%
  left_join(regions %>% select(area_code = code, area = name), by = "area_code")

# Add all remaining columns from use_fd_full as 0
missing_cols <- setdiff(names(use_fd_full), names(new_fd_rows))
for (col in missing_cols) {
  new_fd_rows[[col]] <- 0
}

# Align column order and bind
new_fd_rows <- new_fd_rows %>% select(any_of(names(use_fd_full)))
use_fd_full <- bind_rows(use_fd_full, new_fd_rows)




###########################################################
########### BINDING SUPPLY #########
###########################################################

sup_full <- bind_rows(sup_final, sup_final_bcp) %>%
  select(-production, -unit)  %>%
  filter(area_code %in% current_codes)




###########################################################
########### BINDING BTD #########
###########################################################

btd_full <- bind_rows(btd_final_resc, btd_final_bcp) %>%
  select(-importer_iso3, -exporter_iso3, -item, -unit)

rm(btd_final_resc)
gc()



###########################################################
########### REALLOCATING IMBALANCES #######################
###########################################################

###########################################################
#1. Calculating imbalances
###########################################################

supply_summary <- sup_full %>%
  group_by(comm_code, year, area_code) %>%
  summarise(prod = sum(supply, na.rm = TRUE), .groups = "drop")

use_summary <- use_full %>%
  group_by(comm_code, year, area_code) %>%
  summarise(use = sum(use, na.rm = TRUE), .groups = "drop")

exports_summary <- btd_full %>%
  filter(from_code != to_code) %>%
  group_by(comm_code, year, from_code) %>%
  summarise(exports = sum(value, na.rm = TRUE), .groups = "drop") %>%
  rename(area_code = from_code)

imports_summary <- btd_full %>%
  filter(from_code != to_code) %>%
  group_by(comm_code, year, to_code) %>%
  summarise(imports = sum(value, na.rm = TRUE), .groups = "drop") %>%
  rename(area_code = to_code)

balance_check <- use_fd_full %>%
  left_join(supply_summary, by = c("comm_code", "year", "area_code")) %>%
  left_join(use_summary, by = c("comm_code", "year", "area_code")) %>%
  left_join(exports_summary, by = c("comm_code", "year", "area_code")) %>%
  left_join(imports_summary, by = c("comm_code", "year", "area_code")) 

balance_check <- balance_check %>%
  mutate(across(c(imports, exports), ~ ifelse(comm_code %in% c("c901", "c999"), 0, .x)))

#Checking imbalances and joining their quantities as a use in fd. 

balance_check <- balance_check %>% 
  mutate(tot_supply = na_sum(prod, imports, stock_withdrawal, - exports), 
         tot_use    = na_sum(use, food, losses, other, stock_addition, 
           tourist, fuel, other_industrial, unknown_use),
         imbalance  = tot_supply - tot_use)

# ---- c901 is pinned by waste_flows (trade) + biofuel use; keep it OUT of the
#      imbalance redistribution AND the self-flow loop (commodities_vec/self_flows
#      derive from balance_check_adj). Its tables are set explicitly below. ----
balance_check <- balance_check %>% filter(comm_code != "c901")


###########################################################
#2. Redistribute imbalances
###########################################################

fd_cols <- c("food", "losses", "other", "stock_addition", "tourist",
             "fuel", "other_industrial", "unknown_use")

# 1) Redistribute imbalances in balance_check
balance_check_adj <- balance_check %>%
  mutate(fd_sum = rowSums(across(all_of(fd_cols)), na.rm = TRUE)) %>%
  # Positive imbalance -> distribute proportionally across FD items
  mutate(across(all_of(fd_cols), ~ case_when(
    imbalance > 0 & fd_sum > 0 ~ .x + imbalance * (.x / fd_sum),
    TRUE                       ~ .x
  ))) %>%
  # Fallback: positive imbalance with fd_sum == 0 -> park in stock_addition
  mutate(stock_addition = if_else(imbalance > 0 & fd_sum == 0,
                                  stock_addition + imbalance,
                                  stock_addition),
         # Negative imbalance -> bump up production
         prod_new = if_else(imbalance < 0, prod + abs(imbalance), prod)) %>%
  # Recompute totals to verify zero imbalance
  mutate(tot_use_new    = na_sum(use, food, losses, other, stock_addition,
           tourist, fuel, other_industrial, unknown_use),
         tot_supply_new = na_sum(prod_new, imports, stock_withdrawal, exports),
         imbalance_new  = tot_supply_new - tot_use_new)


###########################################################
#2. Updating use in final demand with readjusted values
###########################################################

use_fd_full <- use_fd_full %>%
  select(-all_of(fd_cols)) %>%
  left_join(
    balance_check_adj %>% select(comm_code, area_code, year, all_of(fd_cols)),
    by = c("comm_code", "area_code", "year")
  )

# ---- c901 carries NO final demand (input to biofuels only) ----
fd_zero_cols <- c("food", "losses", "other", "stock_addition", "stock_withdrawal",
                  "tourist", "fuel", "other_industrial", "unknown_use")
use_fd_full <- use_fd_full %>%
  mutate(across(any_of(fd_zero_cols), ~ if_else(comm_code == "c901", 0, .x)))

###########################################################
#3. Updating supply with readjusted values
###########################################################

prod_update <- balance_check_adj %>%
  select(comm_code, area_code, year, prod_new)

sup_full <- sup_full %>%
  # 1. compute supply shares within group
  group_by(comm_code, area_code, year) %>%
  mutate(
    share = if (sum(supply, na.rm = TRUE) > 0) {
      supply / sum(supply, na.rm = TRUE)
    } else {
      0
    }
  ) %>%
  ungroup() %>%
  # 2. join updated production
  left_join(prod_update, by = c("comm_code", "area_code", "year")) %>%
  # 3. update supply using shares
  mutate(
    supply = if_else(
      !is.na(prod_new),
      share * prod_new,
      supply
    )
  ) %>%
  select(-prod_new, -share)

### Completing the proc_code and proc info
patch_tbl <- items_supply_bcp %>%
  select(item, proc, comm_code, proc_code) %>%
  semi_join(sup_full, by = "item") %>%     
  add_count(item) %>%
  filter(n == 1) %>%
  select(-n)

sup_full <- sup_full %>% 
  rows_patch(patch_tbl, by = "item") %>%
  mutate(price = ifelse(grepl("^p(12[5-9]|13[0-9]|14[0-6]|901|999)$", proc_code), 1, price))

# ---- c901 production by country = sum of its outgoing trade flows (self-flows
#      included). Reuse existing c901 supply metadata, rebuild for every producer. ----
c901_meta <- sup_full %>%
  filter(comm_code == "c901") %>%
  distinct(across(any_of(c("item", "item_code", "comm_code", "proc", "proc_code"))))
stopifnot(nrow(c901_meta) == 1)
c901_meta$item_code <- c901_item_code          # keep item_code consistent across tables

c901_sup <- waste_flows %>%
  group_by(area_code = from_code, year) %>%
  summarise(supply = sum(value, na.rm = TRUE), .groups = "drop") %>%
  filter(supply > 0) %>%
  mutate(!!!as.list(c901_meta[1, ]), price = 1) %>%
  left_join(regions %>% select(area_code = code, area = name), by = "area_code")

sup_full <- sup_full %>%
  filter(comm_code != "c901") %>%
  bind_rows(c901_sup %>% select(any_of(names(sup_full))))




###########################################################
########### CALCULATE 'SELF-FLOWS' AND UPDATE BTD #######################
###########################################################

# self_flows <- balance_check_adj %>%
#   mutate(value = pmax(na_sum(tot_supply_new, - imports), 0), # 'self-consumption' is the part of use that is neither imported nor exported 
#          from_code = area_code,
#          to_code = area_code) %>%   
#   select(comm_code, item_code, from_code, to_code, year, value)
# 
# 
# btd_full <- rows_upsert(btd_full, self_flows, by = c("from_code", "to_code", "comm_code", "year"))
# 
# 




areas <- sort(unique(balance_check_adj$area_code))
n <- length(areas)
commodities_vec <- sort(unique(balance_check_adj$comm_code))

# (from_code × to_code) template to force full n × n matrices
mapping_templ <- data.table(
  from_code = rep(areas, each = n),
  to_code   = rep(areas, times = n))

btd_dt <- as.data.table(btd_full)

# DS / TS / DU per (comm, area, year), derived from the adjusted balance
bal_dt <- as.data.table(as.data.frame(balance_check_adj))[, .(
  comm_code, item_code, area_code, year,
  DS = pmax(prod_new, 0, na.rm = TRUE), # Domestic supply 
  TS = pmax(na_sum(prod_new, imports), 0), # Total supply 
  DU = pmax(tot_use_new, 0, na.rm = TRUE))] # Domestic use

stopifnot(
  nrow(bal_dt) == nrow(balance_check_adj),
  nrow(bal_dt[, .N, by = .(comm_code, area_code, year)][N > 1]) == 0
)            # domestic use

self_flows_list <- vector("list", length(years))

for (i in seq_along(years)) {
  y <- years[i]
  cat("Calculating self-flows for year ", y, ".\n", sep = "")
  
  bal_y <- bal_dt[year == y]
  setkey(bal_y, comm_code, area_code)
  btd_y <- btd_dt[year == y, .(from_code, to_code, comm_code, value)]
  
  self_j <- lapply(commodities_vec, function(j) {
    # Build bilateral trade matrix T (n × n) for this commodity
    x <- btd_y[comm_code == j, .(from_code, to_code, value)]
    if (nrow(x) == 0) {
      T_mat <- Matrix::Matrix(0, n, n, sparse = TRUE,
                              dimnames = list(areas, areas))
    } else {
      tmp <- merge(mapping_templ, x, by = c("from_code", "to_code"), all.x = TRUE)
      tmp[is.na(value), value := 0]
      mat_wide <- data.table::dcast(tmp, from_code ~ to_code,
                                    fun.aggregate = sum, value.var = "value")[, -"from_code"]
      T_mat <- as(as.matrix(mat_wide), "CsparseMatrix")
    }
    
    # DS, TS, DU aligned with 'areas'
    b <- bal_y[.(j, areas), on = c("comm_code", "area_code")]
    DS <- b$DS; DS[is.na(DS)] <- 0
    TS <- b$TS; TS[is.na(TS)] <- 0
    DU <- b$DU; DU[is.na(DU)] <- 0
    item_j <- b$item_code[!is.na(b$item_code)][1]
    
    T_offdiag_sum <- sum(T_mat) - sum(Matrix::diag(T_mat))
    
    if (T_offdiag_sum == 0) {
      # No bilateral trade -> entire domestic use is self-sourced
      self_vals <- DU
    } else {
      A <- sweep(T_mat, 1, TS, FUN = "/")
      A[is.na(A)] <- 0
      
      L <- tryCatch(solve(Diagonal(n) - A),
                    error = function(e) {
                      m <- as.matrix(Diagonal(n) - A); m[!is.finite(m)] <- 0
                      MASS::ginv(m)
                    })
      
      F <- L * DS                       # row-scale: F[i,j] = L[i,j] * DS[i]
      F[F < 0] <- 0
      col_sums <- colSums(F)
      S <- as.matrix(t(t(F) / pmax(col_sums, 1)))
      S[, col_sums == 0] <- 0
      S[!is.finite(S)] <- 0
      
      # Only the diagonal of final_result is needed: S[i,i] * DU[i]
      self_vals <- diag(S) * DU
    }
    
    data.table(comm_code = j, item_code = item_j,
               area_code = areas, value = round(self_vals))
  })
  
  self_flows_list[[i]] <- rbindlist(self_j)[, year := y]
}

self_flows <- rbindlist(self_flows_list)
self_flows[, `:=`(from_code = area_code, to_code = area_code)]
self_flows <- self_flows[, .(comm_code, item_code, from_code, to_code, year, value)]


# Updating btd_full with the updated flows. 
btd_full <- rows_upsert(btd_full, self_flows,
                        by = c("from_code", "to_code", "comm_code", "year"))

# ---- c901 bilateral flows = waste_flows (off-diagonal trade + self-flows). c901
#      was excluded from the self-flow loop, so nothing else here touches it. ----
btd_full <- btd_full %>%
  filter(comm_code != "c901") %>%
  bind_rows(waste_flows %>% select(any_of(names(btd_full))))





###########################################################
########### WRITING TABLES #######################
###########################################################

setwd("/home/mmondolfo/fabio_bfp/")

saveRDS(sup_full, tag("data/sup_final_merged.rds"))
saveRDS(use_full, tag("data/use_final_merged.rds"))
saveRDS(use_fd_full, tag("data/use_fd_final_merged.rds"))
saveRDS(btd_full, tag("data/btd_final_merged.rds"))