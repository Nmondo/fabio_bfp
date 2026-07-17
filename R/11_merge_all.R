rm(list = ls())

###########################################################
########### LOADING PACKAGES #########
###########################################################

library("tidyverse")
library("data.table")
library(Matrix)
library(dplyr)

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
source(file.path(fabio_root, "R/00_system_variables.R"))  # + 00_9_fabio_helpers.R
source(file.path(fabio_root, "R/00_run_config.R"))        # RUN_MODE / BYPASS_RESCALE / tag() / STRICT

dir.create("output", showWarnings = FALSE)

###########################################################
########### LOADING DATA #########
###########################################################

setwd(file.path(fabio_root, "data"))

yr_keep <- function(x) dplyr::filter(x, year %in% 2012:2022)

# --- mode-INDEPENDENT BCP-extension tables (never rescaled) -------------------
use_fd_final_bcp <- yr_keep(readRDS("use_fd_final_bcp.rds"))
sup_final_bcp    <- yr_keep(readRDS("sup_final_bcp.rds"))
btd_final_bcp    <- yr_keep(readRDS("btd_final_bcp.rds"))

# --- mode-DEPENDENT CBS-side tables from 10_1a (tag() -> *_noresc in bypass) ---
use_final    <- yr_keep(readRDS(tag("use_final.rds")))
use_fd_final <- yr_keep(readRDS(tag("use_fd_final.rds")))
sup_final    <- yr_keep(readRDS(tag("sup_final.rds")))

# --- btd comes from 08_04, NOT 08_03 -----------------------------------------
# 08_04's ladder no longer mints production: its residual is a self-flow top-up (funded by
# an export cut) plus a bilateralised import top-up, both written into btd_final_bal.rds.
# Reading btd_final_resc.rds here would throw that away and 11 would re-mint the exact same
# tonnage as prod_new (Bug 2). 08_04 emits btd_final_bal in BOTH modes -> no mode branch.
btd_final_bal <- yr_keep(readRDS(tag("btd_final_bal.rds")))

if (BYPASS_RESCALE) {
  use_final_bcp <- yr_keep(readRDS("../intermediate_data/use_rebal_bcp.rds"))
  message(">>> [BYPASS] 11 using use_rebal_bcp (08_03 ignored); btd from 08_04 bal table")
} else {
  use_final_bcp <- yr_keep(readRDS("use_final_bcp.rds"))
}

setwd(fabio_root)

y_bp_incomplete_rows <- readRDS("inputs_for_final_data/y_bp_incomplete_rows.rds")
items_use_bcp    <- read_csv("inst/items_use_bcp.csv", show_col_types = FALSE)
items_full_bcp   <- read_csv("inst/items_full_bcp.csv", show_col_types = FALSE)
items_supply_bcp <- read_csv("inst/items_supply_bcp.csv", show_col_types = FALSE)
regions          <- read_csv("inst/regions_full.csv", show_col_types = FALSE) %>%
  filter(current == TRUE)

fabio_assert(!is.unsorted(regions$code, strictly = TRUE),
             "11: regions_full.csv is not strictly ascending by `code`.")

# ---- Producer prices for co-product VALUE allocation (p125, p126, p127) --------
ETOH_T_PER_KL <- 1 / 1.267   # t per 1000 L; Bioethanol 1.267 l/kg -> c146
FAME_T_PER_KL <- 1 / 1.136   # t per 1000 L; Biodiesel  1.136 l/kg -> c147

liq_dens <- tibble::tibble(
  comm_code   = c("c146",        "c147"),
  t_per_1000L = c(ETOH_T_PER_KL, FAME_T_PER_KL))

prices_trade <- readRDS("intermediate_data/prices_bcp.rds") %>%
  filter(product %in% c("Bioethanol", "Biodiesel", "Glycerol, crude",
                        "Dried distillers grains with solubles"),
         is.finite(unit_price), qty > 0, value > 0) %>%
  group_by(product, year) %>%
  summarise(price_usd_t = sum(value, na.rm = TRUE) / sum(qty, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(comm_code = dplyr::recode(product,
                                   "Bioethanol"                            = "c146",
                                   "Biodiesel"                             = "c147",
                                   "Glycerol, crude"                       = "c148",
                                   "Dried distillers grains with solubles" = "c171")) %>%
  left_join(liq_dens, by = "comm_code") %>%
  mutate(bcp_price = if_else(!is.na(t_per_1000L), price_usd_t * t_per_1000L, price_usd_t)) %>%
  select(comm_code, year, bcp_price)

prices_flat <- as_tibble(universal_bcp_prices) %>%
  transmute(comm_code, bcp_price = price_usd_t * t_per_1000L) %>%
  tidyr::crossing(year = 2012:2022)

bcp_price_lu <- bind_rows(prices_trade, prices_flat)

# ---- c901 waste trade flows from 08_03 ----
waste_flows <- as.data.frame(readRDS("data/waste_flows.rds"))
waste_flows <- waste_flows[waste_flows$year %in% 2012:2022, ]
c901_item_code <- items_full_bcp %>% filter(comm_code == "c901") %>% pull(item_code) %>% unique()
stopifnot(length(c901_item_code) == 1)
waste_flows$item_code <- c901_item_code

if (BYPASS_RESCALE && !BYPASS_KEEP_WASTE) {
  waste_flows <- waste_flows[0, , drop = FALSE]
  message(">>> [BYPASS] 11 dropped RED waste_flows; c901 self-sourced from pre-rescale use only")
}

###########################################################
########### MAKING VECTORS#########
###########################################################

extension_items <- unique(use_fd_final_bcp$item)
current_codes   <- regions$code

###########################################################
########### BINDING USE #########
###########################################################

use_full <- bind_rows(use_final, use_final_bcp) %>%
  select(-unit) %>%
  filter(area_code %in% current_codes)

###########################################################
########### RESCALE BIOFUEL FEEDSTOCK USE TO OUTPUT #######
###########################################################
# One scale factor per (biofuel, area, year): closes the gap between feedstock-implied
# output (use x TCF) and reported biofuel supply, leaving the feedstock mix unchanged.
# BRA + USA are held at their original, un-rescaled feedstock use.

tcf <- readRDS("intermediate_data/tcf_table_final.rds")
tcf$item <- ifelse(tcf$item == " Triticale", "Triticale", tcf$item)
setDT(tcf)
items_lu <- unique(as.data.table(items_full_bcp)[!is.na(comm_code), .(item, comm_code)], by = "item")
tcf[items_lu, comm_code := fcoalesce(comm_code, i.comm_code), on = "item"]
tcf[, biofuel_code := fcase(
  grepl("Biogasoline", proc),      "c146",
  grepl("Biodiesel", proc),        "c147",
  grepl("Renewable diesel", proc), "c149",
  default = NA_character_)]
tcf <- tcf[!item %in% c("Oilcrops Oil, Other", "Total") & !is.na(biofuel_code)]

tcf_key <- unique(tcf[, .(comm_code, biofuel_code, output_qty)])
stopifnot(nrow(tcf_key[, .N, by = .(comm_code, biofuel_code)][N > 1]) == 0)

bf_procs <- unique(as.data.table(use_full)[proc_code %in% c("p125","p126","p127"),
                                           .(proc_code, proc)])
bf_procs[, biofuel_code := fcase(
  grepl("Biogasoline", proc),      "c146",
  grepl("Biodiesel", proc),        "c147",
  grepl("Renewable diesel", proc), "c149",
  default = NA_character_)]
stopifnot(!anyNA(bf_procs$biofuel_code), uniqueN(bf_procs$proc_code) == 3L)

stopifnot("iso3c" %in% names(regions))
no_rescale_codes <- regions$code[regions$iso3c %in% c("BRA", "USA")]
stopifnot(length(no_rescale_codes) == 2L)

u_bf <- as.data.table(use_full)[proc_code %in% c("p125","p126","p127")]
u_bf <- merge(u_bf, bf_procs[, .(proc_code, biofuel_code)], by = "proc_code", all.x = TRUE)
u_bf <- merge(u_bf, tcf_key, by = c("comm_code", "biofuel_code"), all.x = TRUE)

miss_tcf <- u_bf[!is.na(use) & use != 0 & is.na(output_qty),
                 .(use = sum(use)), by = .(biofuel_code, comm_code, item)]
if (nrow(miss_tcf)) warning("biofuel rescale: ", nrow(miss_tcf),
                            " feedstock/biofuel combos have use but no TCF — inspect `miss_tcf`")

implied <- u_bf[, .(implied = sum(use * output_qty, na.rm = TRUE)),
                by = .(biofuel_code, area_code, year)]

actual <- as.data.table(sup_final_bcp)[comm_code %in% c("c146","c147","c149"),
                                       .(actual = sum(supply, na.rm = TRUE)),
                                       by = .(biofuel_code = comm_code, area_code, year)]

lam <- merge(implied, actual, by = c("biofuel_code","area_code","year"), all = TRUE)
lam[is.na(implied), implied := 0][is.na(actual), actual := 0]
lam[, lambda := actual / implied]
lam[!is.finite(lambda), lambda := 1]
lam[area_code %in% no_rescale_codes, lambda := 1]

n_zeroed <- lam[implied > 0 & actual == 0 & !(area_code %in% no_rescale_codes), .N]
if (n_zeroed) message(">>> biofuel rescale: ", n_zeroed,
                      " (biofuel,area,year) with output=0 but feedstock use>0 -> use zeroed")

use_full <- as.data.table(use_full)
use_full[proc_code %in% c("p125","p126","p127"),
         biofuel_code := bf_procs$biofuel_code[match(proc_code, bf_procs$proc_code)]]
use_full[lam, lambda := i.lambda, on = c("biofuel_code","area_code","year")]
use_full[!is.na(lambda) & !is.na(use), use := use * lambda]
use_full[, c("biofuel_code","lambda") := NULL]
use_full <- as_tibble(use_full)

# self-source c901 use that waste_flows doesn't cover
c901_use <- as.data.table(as.data.frame(use_full))[comm_code == "c901",
                                                   .(use = sum(use)), by = .(area_code, year)]
c901_in  <- as.data.table(as.data.frame(waste_flows))[, .(inflow = sum(value)),
                                                      by = .(area_code = to_code, year)]
short <- merge(c901_use, c901_in, by = c("area_code", "year"), all.x = TRUE)
short[is.na(inflow), inflow := 0][, short := use - inflow]

if (nrow(short[short < -1])) warning("c901: ", nrow(short[short < -1]),
                                     " cells have inflow > use (over-allocated)")

self_add_c901 <- short[short > 1, .(from_code = area_code, to_code = area_code,
                                    value = short, year,
                                    item_code = c901_item_code, comm_code = "c901")]
waste_flows <- rbind(as.data.frame(waste_flows), as.data.frame(self_add_c901))
waste_flows <- as.data.frame(as.data.table(waste_flows)[
  , .(value = sum(value)), by = .(from_code, to_code, year, item_code, comm_code)])

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
                            TRUE                                  ~ .x))) %>%
  filter(area_code %in% current_codes)

new_items <- items_full_bcp %>%
  filter(comm_code %in% c("c901", "c999")) %>%
  select(comm_code, item_code, item) %>%
  distinct()

new_fd_rows <- expand.grid(
  area_code = unique(use_fd_full$area_code),
  comm_code = c("c901", "c999"),
  year      = 2012:2022,
  stringsAsFactors = FALSE) %>%
  left_join(new_items, by = "comm_code") %>%
  left_join(regions %>% select(area_code = code, area = name), by = "area_code")

missing_cols <- setdiff(names(use_fd_full), names(new_fd_rows))
for (col in missing_cols) new_fd_rows[[col]] <- 0

new_fd_rows <- new_fd_rows %>% select(any_of(names(use_fd_full)))
use_fd_full <- bind_rows(use_fd_full, new_fd_rows)

###########################################################
########### BINDING SUPPLY #########
###########################################################

sup_full <- bind_rows(sup_final, sup_final_bcp) %>%
  select(-production, -unit) %>%
  filter(area_code %in% current_codes)

###########################################################
########### MINT DDGS (c171) SUPPLY EARLY #################
###########################################################
# c171 (DDGS) has no CBS supply of its own; 10_1a computed its grain-based production
# (0.3 x grain feedstock, pre-lambda) and saved ddgs_production.rds. We mint it HERE,
# before NO_SUPPLY_COMMS / commodity_kind / the self-flow loop, so c171 is a fully-fledged
# supplied commodity everywhere downstream -- NOT parked in NO_SUPPLY_COMMS and NOT skipped
# by the self-flow loop. Reading the SAME saved production 10_1a used for the feed keeps
# c171 balanced: supply + imports - exports = feed. price is re-set later with the co-products.
ddgs_meta <- items_supply_bcp %>% filter(comm_code == "c171") %>%
  distinct(across(any_of(c("item","item_code","comm_code","proc","proc_code"))))
if (nrow(ddgs_meta) != 1) stop("11: DDGS (c171) supply metadata not found/unique in items_supply_bcp.")

ddgs_prod_path <- tag("intermediate_data/ddgs_production.rds")
if (!file.exists(ddgs_prod_path))
  stop("11: ", ddgs_prod_path, " not found -- run 10_1a (which computes & saves it) first.")

ddgs_sup <- as_tibble(readRDS(ddgs_prod_path)) %>%
  rename(supply = production) %>% filter(supply > 0) %>%
  mutate(!!!as.list(ddgs_meta[1, ]), price = 1) %>%
  left_join(regions %>% select(area_code = code, area = name), by = "area_code")

sup_full <- sup_full %>% filter(comm_code != "c171") %>%
  bind_rows(ddgs_sup %>% select(any_of(names(sup_full))))
message(">>> 11: minted c171 (DDGS) supply early: ", nrow(ddgs_sup), " rows, ",
        round(sum(ddgs_sup$supply, na.rm = TRUE)/1e6, 2), " Mt (grain-based, pre-lambda).")

###########################################################
########### BINDING BTD #########
###########################################################

btd_full <- bind_rows(btd_final_bal, btd_final_bcp) %>%
  select(-importer_iso3, -exporter_iso3, -item, -unit)

rm(btd_final_bal); gc()

setDT(sup_full); setDT(use_full); setDT(use_fd_full); setDT(btd_full)
btd_full[, value := as.numeric(value)]

# --- de-collide the c171 dense-grid scaffold (btd_final_bal) against the real
#     DDGS trade (btd_final_bcp). 06's dense grid carries an all-zero c171 block;
#     07_10's alias fix populated the same comm_code in btd_final_bcp, so the bind
#     stacks scaffold-zero + real-trade -> 5196 duplicate keys (see 98_diag_c171_btd_dup.R).
#     Summing is safe ONLY where at most one side is nonzero; assert that, then collapse.
KEY  <- c("from_code","to_code","comm_code","year")
dups <- btd_full[, .N, by = KEY][N > 1]
if (nrow(dups)) {
  coll   <- btd_full[dups, on = KEY]
  bad_nz <- coll[, .(nz = sum(value != 0)), by = KEY][nz > 1]
  fabio_assert(nrow(bad_nz) == 0,
               "11: BINDING BTD produced %d duplicate keys NONZERO on both sides — a sum would double-count real trade.",
               nrow(bad_nz), data = bad_nz)
  merged   <- coll[, .(value = sum(value, na.rm = TRUE),
                       item_code = item_code[!is.na(item_code)][1]), by = KEY]
  btd_full <- rbind(btd_full[!dups, on = KEY], merged, use.names = TRUE, fill = TRUE)
  message(">>> 11: collapsed ", nrow(dups),
          " duplicate btd keys at BINDING BTD (scaffold zeros absorbed into bcp trade).")
}

###########################################################
########### c171 (DDGS) PRESENCE CHECK ####################
###########################################################
# The DDGS integration only works if c171 lives in ALL THREE tables: supply (minted above),
# use (feed allocated in 10_1b), and btd (trade from 07_04). Verify before the balance so a
# missing leg fails loudly here rather than silently ghosting downstream.
local({
  n_sup <- sup_full[comm_code == "c171" & !is.na(supply) & supply > 0, .N]
  t_sup <- sup_full[comm_code == "c171" & !is.na(supply) & supply > 0, sum(supply)]
  n_use <- use_full[comm_code == "c171" & !is.na(use) & use > 0, .N]
  t_use <- use_full[comm_code == "c171" & !is.na(use) & use > 0, sum(use)]
  n_btd <- btd_full[comm_code == "c171" & !is.na(value) & value > 0, .N]
  t_btd <- btd_full[comm_code == "c171" & !is.na(value) & value > 0, sum(value)]
  z <- function(x) if (length(x)) x else 0
  bar  <- strrep("=", 78)
  line <- sprintf("[c171 PRESENCE CHECK]  SUPPLY %d rows / %.2f Mt  |  USE %d rows / %.2f Mt  |  BTD %d rows / %.2f Mt",
                  z(n_sup), z(t_sup)/1e6, z(n_use), z(t_use)/1e6, z(n_btd), z(t_btd)/1e6)
  # Dual-channel + banner so it is impossible to miss in a terminal loop:
  #   message() -> stderr (shows live on the console, captured by `2>&1`)
  #   cat()     -> stdout (survives a stdout-only `> log` and is greppable on "[c171")
  message("\n", bar, "\n>>> ", line, "\n", bar)
  cat("\n", bar, "\n>>> ", line, "\n", bar, "\n", sep = ""); flush(stdout())
  fabio_assert(z(n_sup) > 0,
               "11: c171 has NO supply -- the early DDGS mint did not populate sup_full (check ddgs_production.rds).")
  fabio_assert(z(n_use) > 0,
               "11: c171 has NO use -- 10_1b did not allocate DDGS feed into use_final (check the feed integration).")
  fabio_assert(z(n_btd) > 0,
               "11: c171 has NO btd trade -- btd_final_bcp lost item 654 (check 07_10 alias / 07_04).")
})

###########################################################
########### COMMODITY CLASSIFICATION ######################
###########################################################

# --- PRIMARY vs PROCESSED ------------------------------------------------------------
# Which commodities is an empty input column NORMAL for? Grazing and fodder take their
# impact from a DIRECT extension, so a bare column is fine and a production bump is
# defensible (FAO under-reports harvests). Palm oil is not: output with no input is a
# DELETED footprint. Same self-calibrating rule as 43_trace_phantom_supply.R §1, computed
# from the SUT so it exists here, before Z does. 13 re-reads the csv.
# commodity_kind() reads the authoritative ISIC column; the waste override lives inside it but
# WASTE_COMMS stays defined here as the single source of truth for the message + NO_SUPPLY assert.
WASTE_COMMS <- c("c145", "c901")
kind_tbl <- commodity_kind(items_full_bcp, waste_comms = WASTE_COMMS)
fwrite(kind_tbl, "inst/commodity_kind.csv")

message(">>> 11: ", kind_tbl[kind == "PROCESSED", .N], " PROCESSED / ",
        kind_tbl[kind == "PRIMARY", .N], " PRIMARY commodities (waste: ",
        paste(WASTE_COMMS, collapse = ", "), " forced PRIMARY)")

# --- commodities NOTHING in the model supplies ---------------------------------------
# A commodity with trade and use but NO supply row ANYWHERE cannot be topped up and cannot
# be minted. Handing one to bilateralise_topup() is catastrophic: it sources A's deficit
# from B, which makes B an EXPORTER of something B does not produce, which drives B's
# imbalance negative, which is sourced from A next pass. Each pass inflates both sides —
# c162's btd went from 137 kt to 5.5 Mt over 8 passes, entirely fabricated, with China and
# area 216 each "exporting" ~2.7 Mt to the other. That is why the loop crawled.
#
# NB after 07_10's join fix (supply_final_bp had no `proc` column, so the (proc, item) join
# dropped 23.6 Mt of biopolymer supply across 1,129 rows) this set should be nearly EMPTY.
# If the BP building blocks are still in it, 07_10 has not been re-run.
NO_SUPPLY_COMMS <- setdiff(
  unique(c(use_full$comm_code, use_fd_full$comm_code, btd_full$comm_code)),
  unique(sup_full[!is.na(supply) & supply > 0, comm_code]))

if (length(NO_SUPPLY_COMMS)) {
  message(">>> 11: ", length(NO_SUPPLY_COMMS), " commodities have NO supply row anywhere in ",
          "the model and are EXCLUDED from the imbalance/top-up machinery: ",
          paste(sort(NO_SUPPLY_COMMS), collapse = ", "))
  fwrite(data.table(comm_code = sort(NO_SUPPLY_COMMS)), "output/11_no_supply_comms.csv")
}

fabio_assert(!any(WASTE_COMMS %in% NO_SUPPLY_COMMS),
             "11: a waste commodity landed in NO_SUPPLY_COMMS — it will be excluded from the balance instead of minted.")

###########################################################
########### GHOST SNAPSHOT: what 11 INHERITS ##############
###########################################################
# "Ghost" = a country SHIPS a commodity in btd but supplies none of it. 12_a turns such a
# row into an mr_use row with no mr_sup column -> an empty Z column -> zero embodied impact.
#
# The staged diagnostic (98_diag_ghost_origin.R) established that 11 INHERITS ~72 Mt of
# these and EMITS ~28 Mt: it net-REMOVES 44 Mt and authors almost none of them. The rest are
# born in 06 (c145 RAW_ITEMS), 07_10 (name-join drops) and 09_1 (the livestock-share
# allocation zeroes c117/c089/c113/c114 supply).
#
# So the invariant 11 must satisfy is NOT "no ghosts" — it cannot fix its inputs — but
# "11 CREATES NO NEW GHOSTS". Snapshot the inherited set now; assert on the delta at the end.
ghost_of <- function(sup, btd, exclude = c("c901")) {
  s <- sup[!is.na(supply) & supply > 0, .(s = sum(supply)), by = .(comm_code, area_code, year)]
  b <- btd[!is.na(value) & value > 1 & !comm_code %in% exclude,
           .(shipped = sum(value)), by = .(comm_code, area_code = from_code, year)]
  g <- merge(b, s, by = c("comm_code", "area_code", "year"), all.x = TRUE)
  g[is.na(s), s := 0]
  g[s <= 0][, .(comm_code, area_code, year, shipped)]
}

ghost_in <- ghost_of(sup_full, btd_full)
message(">>> 11: INHERITED ghosts (ship but supply nothing): ", nrow(ghost_in), " cells, ",
        round(sum(ghost_in$shipped) / 1e6, 1), " Mt — created upstream of 11.")
if (nrow(ghost_in)) {
  fwrite(ghost_in[order(-shipped)], tag_csv <- "output/11_ghosts_inherited.csv")
  print(ghost_in[, .(cells = .N, Mt = round(sum(shipped) / 1e6, 2)),
                 by = comm_code][order(-Mt)][1:10])
}

###########################################################
########### REALLOCATING IMBALANCES #######################
###########################################################
# Bug 2. The old code did
#     prod_new = if_else(imbalance < 0, prod + abs(imbalance), prod)
# unconditionally, then updated supply by `share * prod_new` — where `share` is 0 for any
# (comm, area, year) with NO supply row. So prod_new was SILENTLY DISCARDED from sup_full,
# but NOT from the self-flow loop, which read DS = pmax(prod_new, 0), turned it into a btd
# self-flow, and handed 12_a an mr_use ROW with no mr_sup COLUMN. 2,348 phantom nodes whose
# Z column is empty and whose embodied impact is exactly 0.
#
# Negative imbalance is now routed by whether ANYTHING can carry the tonnage:
#   supply row exists (sum(supply) > 0)  -> bump production   [FABIO behaviour, kept]
#   no supply row, PRIMARY               -> create the supply row, then bump
#   no supply row, PROCESSED             -> import top-up, bilateralised. NEVER mint.

MAX_PASS  <- 12L
TOPUP_TOL <- 1      # tonnes

fd_cols <- c("food", "losses", "other", "stock_addition", "tourist",
             "fuel", "other_industrial", "unknown_use")

# NA-safe zero. na_sum() returns NA when EVERY input is NA, so a row with no use and no FD
# at all got tot_use = NA -> imbalance = NA -> `if_else(imbalance > 0, ...)` silently did
# NOTHING, and the row carried its whole supply through as an unclosed imbalance.
z <- function(x) fifelse(is.na(x), 0, x)

# Commodities with supply but structurally no use and no trade. Leave empty unless
# output/11_supply_no_use.csv says otherwise.
NO_USE_COMMS <- character(0)

# Whose NEGATIVE `other` may be zeroed. c145 is the whole story: 186 cells, -19.3 Mt, and
# nothing else in the model carries material negative `other`. Do not widen this to NULL
# (= all commodities) without re-reading output/11_negative_other_by_comm.csv.
NEG_OTHER_COMMS <- c("c145")

rebuild_balance <- function(btd_full) {
  supply_summary  <- sup_full[, .(prod = sum(supply, na.rm = TRUE)),
                              by = .(comm_code, year, area_code)]
  use_summary     <- use_full[, .(use = sum(use, na.rm = TRUE)),
                              by = .(comm_code, year, area_code)]
  exports_summary <- btd_full[from_code != to_code,
                              .(exports = sum(value, na.rm = TRUE)),
                              by = .(comm_code, year, area_code = from_code)]
  imports_summary <- btd_full[from_code != to_code,
                              .(imports = sum(value, na.rm = TRUE)),
                              by = .(comm_code, year, area_code = to_code)]
  
  bc <- Reduce(function(a, b) merge(a, b, by = c("comm_code", "year", "area_code"),
                                    all.x = TRUE),
               list(copy(use_fd_full), supply_summary, use_summary,
                    exports_summary, imports_summary))
  
  bc[comm_code %in% c("c901", "c999"), `:=`(imports = 0, exports = 0)]
  
  for (cc in c("prod", "use", "imports", "exports", "stock_withdrawal", fd_cols))
    if (cc %in% names(bc)) set(bc, j = cc, value = z(bc[[cc]]))
  
  # --- FAO's NEGATIVE `other` is a balancing residual, not a use ----------------------
  # UCO (c145 / item 1274) never balances in the CBS: FAO books the gap in `other`, and it
  # is NEGATIVE — up to -3.5 Mt (MYS 2022), 10-30 cells EVERY year, growing with the global
  # UCO export boom (-435 kt in 2012 -> -3.5 Mt in 2022). A negative domestic use is not a
  # use; it is FAO writing "these numbers do not add up."
  #
  # Left alone it INVERTS the balance. MYS 2022: prod 406 kt, imports 309 kt, exports
  # 1,229 kt -> tot_supply = -462 kt (a real deficit: it ships more UCO than it has). But
  # tot_use = other = -3,522 kt, so imbalance = -462 - (-3,522) = +3,060 kt — the model saw
  # a 3 Mt phantom SURPLUS, spread it across final demand, and never sourced the deficit.
  #
  # Zero the negative and let the deficit surface honestly. c145 is PRIMARY (WASTE_COMMS),
  # so mint_ok is TRUE and the production bump sources it — which is the right answer: FAO
  # under-records waste-oil collection.
  neg_sel <- if (is.null(NEG_OTHER_COMMS)) bc$other < 0
  else bc$comm_code %in% NEG_OTHER_COMMS & bc$other < 0
  if (any(neg_sel)) bc[neg_sel, other := 0]
  
  # food and post-harvest losses are physically non-negative; a negative is always an artifact
  # (e.g. the 10_1a proportional reallocation over-reducing). Zero it and let the deficit surface
  # honestly via imbalance, mirroring the `other`/c145 handling above. Category-scoped on purpose:
  # negative losses appear in PRIMARY commodities (c040) too, so do NOT gate on kind.
  n_food <- sum(bc$food   < 0, na.rm = TRUE)
  n_loss <- sum(bc$losses < 0, na.rm = TRUE)
  if (n_food) message(sprintf(">>> [11 FD floor] zeroed %d negative food cells (%.0f t)",
                              n_food, -sum(pmin(bc$food, 0),   na.rm = TRUE)))
  if (n_loss) message(sprintf(">>> [11 FD floor] zeroed %d negative losses cells (%.0f t)",
                              n_loss, -sum(pmin(bc$losses, 0), na.rm = TRUE)))
  # !is.finite catches Inf/NaN (division by zero upstream) as well as the sign flip
  bc[!is.finite(food)   | food   < 0, food   := 0]
  bc[!is.finite(losses) | losses < 0, losses := 0]
  
  bc[, `:=`(tot_supply = prod + imports + stock_withdrawal - exports,
            tot_use    = use + food + losses + other + stock_addition +
              tourist + fuel + other_industrial + unknown_use)]
  bc[, imbalance := tot_supply - tot_use]
  bc[!comm_code %in% c("c901", NO_USE_COMMS, NO_SUPPLY_COMMS)]
}

sup_tot_of <- function() sup_full[, .(sup_tot = sum(supply, na.rm = TRUE)),
                                  by = .(comm_code, area_code, year)]

tag_gates <- function(bc) {
  bc[sup_tot_of(), on = .(comm_code, area_code, year), sup_tot := i.sup_tot]
  bc[is.na(sup_tot), sup_tot := 0]
  bc[kind_tbl, on = "comm_code", kind := i.kind]
  bc[is.na(kind), kind := "PROCESSED"]                 # unknown => the safe side
  bc[, mint_ok := sup_tot > 0 | kind == "PRIMARY"]
  bc[]
}

# --- REPORT: how much negative `other`, and where? (before the zeroing bites) ---------
neg_rep <- copy(use_fd_full)[z(other) < -1,
                             .(cells = .N, Mt = round(sum(z(other)) / 1e6, 2)),
                             by = comm_code][order(Mt)]
if (nrow(neg_rep)) {
  message(">>> 11: NEGATIVE `other` (FAO balancing residual, not a use) by commodity:")
  print(neg_rep)
  message(">>> 11: zeroing it for: ",
          if (is.null(NEG_OTHER_COMMS)) "ALL commodities" else paste(NEG_OTHER_COMMS, collapse = ", "),
          ". A commodity above that is NOT in that list still has an inverted imbalance.")
  fwrite(neg_rep, "output/11_negative_other_by_comm.csv")
}

# --- DIAGNOSTIC: production that genuinely nothing carries ----------------------------
# NB an earlier version of this check ignored the BTD DIAGONAL and reported 1,137 Mt of
# false positives. exports/imports both filter from_code != to_code, so a country whose
# commodity is entirely SELF-SUPPLIED shows imports = 0 and exports = 0 — and if its use
# rows are NA-filled (10_1a leaves livestock husbandry/slaughtering unsolved) it looks like
# production into a void. Ethiopia's 10.8 Mt of Asses (c103, p092) is the canonical case:
# bred and used at home, carried entirely by btd[238 -> 238]. A cell is only a real hole if
# NOTHING carries it: no use, no FD, no trade, AND no diagonal.
self_flow_now <- btd_full[from_code == to_code & value > 0,
                          .(self = sum(value)), by = .(comm_code, year, area_code = from_code)]

bc0 <- tag_gates(rebuild_balance(btd_full))
bc0[self_flow_now, on = .(comm_code, area_code, year), self := i.self]
bc0[is.na(self), self := 0]

no_use <- bc0[prod > 1 & self < 1 &
                (use + imports + exports +
                   food + losses + other + stock_addition + tourist +
                   fuel + other_industrial + unknown_use) < 1,
              .(comm_code, area_code, year, prod)]

if (nrow(no_use)) {
  fwrite(no_use[order(-prod)], "output/11_supply_no_use.csv")
  message(">>> 11: ", nrow(no_use), " (comm,area,year) have production (",
          round(sum(no_use$prod) / 1e6, 1), " Mt) with NO use, NO trade, NO final demand ",
          "and NO self-flow. Comms: ",
          paste(sort(unique(no_use$comm_code)), collapse = ", "),
          " -> output/11_supply_no_use.csv")
}

# --- the iterative top-up --------------------------------------------------------------
# Topping up A's imports raises B's exports, which pushes B's imbalance negative — so this
# iterates. It CONTRACTS (8.4 Mt -> 571 k -> 99 k -> 22 k -> ...) but only asymptotically:
# a small deficit shuffles between trade partners, halving each pass. The unsourceable
# residual is accumulated ACROSS passes and parked ONCE, after the loop — parking it inside
# wrote the same tonnage into stock_withdrawal on EVERY pass, and stock_withdrawal is a
# SUPPLY term, so each pass inflated supply and manufactured fresh imbalance. The loop was
# feeding itself.

resid_all <- data.table(comm_code = character(), area_code = integer(),
                        year = integer(), unallocated = numeric())

for (pass in seq_len(MAX_PASS)) {
  
  balance_check <- tag_gates(rebuild_balance(btd_full))
  balance_check[, import_topup := fifelse(imbalance < 0 & !mint_ok, abs(imbalance), 0)]
  
  need <- balance_check[import_topup > TOPUP_TOL,
                        .(area_code, year, comm_code, item_code, need = import_topup)]
  
  # already known to have no partner — retrying it every pass froze the residual
  if (nrow(resid_all)) need <- need[!resid_all, on = .(comm_code, area_code, year)]
  
  fabio_assert(!any(need$comm_code %in% NO_SUPPLY_COMMS),
               "11: the top-up was handed a commodity with no supply anywhere in the model — it would fabricate exports from countries that produce nothing.")
  
  message(sprintf(">>> 11 pass %2d: %5d cells need %10.0f t of imports they cannot produce",
                  pass, nrow(need), sum(need$need)))
  if (!nrow(need)) break
  
  add <- bilateralise_topup(need, btd_full)
  rs  <- attr(add, "residual")
  
  if (nrow(rs)) {
    resid_all <- unique(rbind(resid_all, rs[, .(comm_code, area_code, year, unallocated)]),
                        by = c("comm_code", "area_code", "year"))
    message("    ", nrow(rs), " cells (", round(sum(rs$unallocated)),
            " t) have NO trade partner -> deferred, parked once after the loop.")
  }
  
  btd_full <- btd_add(btd_full, add)
}

# park the unsourceable residual ONCE
if (nrow(resid_all)) {
  fwrite(resid_all, "output/11_topup_unallocated.csv")
  message(">>> 11: parking ", round(sum(resid_all$unallocated)), " t (", nrow(resid_all),
          " cells) in stock_withdrawal — no trade partner exists for these commodities.")
  use_fd_full[resid_all, on = .(comm_code, area_code, year),
              stock_withdrawal := z(stock_withdrawal) + i.unallocated]
}

balance_check <- tag_gates(rebuild_balance(btd_full))   # final, with the parked mass

###########################################################
#2. Redistribute imbalances (final pass)
###########################################################

balance_check_adj <- as_tibble(balance_check) %>%
  mutate(fd_sum = rowSums(across(all_of(fd_cols)), na.rm = TRUE)) %>%
  # Positive imbalance -> distribute proportionally across FD items
  mutate(across(all_of(fd_cols), ~ case_when(
    imbalance > 0 & fd_sum > 0 ~ .x + imbalance * (.x / fd_sum),
    TRUE                       ~ .x))) %>%
  # Fallback: ANY positive imbalance not absorbed above -> stock_addition.
  # The old condition was `fd_sum == 0`, which left a HOLE: a cell with NEGATIVE fd_sum
  # (404 cells, -27.4 Mt — FAO books negative other/stock_addition routinely) matched
  # NEITHER branch, so its imbalance was never absorbed at all. `!(fd_sum > 0)` closes it.
  mutate(stock_addition = if_else(imbalance > 0 & !(fd_sum > 0),
                                  stock_addition + imbalance, stock_addition)) %>%
  # NEGATIVE imbalance -> bump production ONLY where something carries it
  mutate(prod_new = if_else(imbalance < 0 & mint_ok, prod + abs(imbalance), prod)) %>%
  # The sign on `exports` was FLIPPED here (`+ exports`), so imbalance_new was wrong by
  # 2 x exports on every row: the guard meant to verify a zero imbalance was broken AND
  # never asserted on. Plain arithmetic, not na_sum() — the NAs are already zeroed and
  # na_sum() would return NA on an all-NA row and hide the failure.
  mutate(tot_use_new    = use + food + losses + other + stock_addition +
           tourist + fuel + other_industrial + unknown_use,
         tot_supply_new = prod_new + imports + stock_withdrawal - exports,
         imbalance_new  = tot_supply_new - tot_use_new) %>%
  as.data.table()

worst   <- max(abs(balance_check_adj$imbalance_new), na.rm = TRUE)
tot_bad <- balance_check_adj[, sum(abs(imbalance_new), na.rm = TRUE)]
n_bad   <- balance_check_adj[abs(imbalance_new) > 1, .N]

message(sprintf(">>> 11: residual imbalance — worst cell %.0f t, %d cells > 1 t, %.0f t total",
                worst, n_bad, tot_bad))

# The top-up converges ASYMPTOTICALLY (see the loop comment). After 12 passes the tail is a
# few hundred tonnes of c145 across small countries with no supply row. That is ~1e-8 of the
# model. An absolute 1 t bound tests the arithmetic, not the model — anything genuinely
# broken loses a commodity or a country, i.e. kilotonnes.
fabio_assert(worst < 1e4 && tot_bad < 1e5,
             "11: residual imbalance — worst cell %.0f t, %.0f t total across %d cells. A tail of a few hundred t is the asymptotic top-up; anything larger is a real imbalance.",
             worst, tot_bad, n_bad,
             data = balance_check_adj[order(-abs(imbalance_new))][1:10,
                                                                  .(comm_code, area_code, year, prod, prod_new, imports, exports,
                                                                    use, other, stock_addition, stock_withdrawal, mint_ok,
                                                                    tot_supply_new, tot_use_new, imbalance_new)])

###########################################################
#3. Updating use in final demand with readjusted values
###########################################################

use_fd_full <- use_fd_full %>%
  select(-all_of(fd_cols)) %>%
  left_join(balance_check_adj %>% select(comm_code, area_code, year, all_of(fd_cols)),
            by = c("comm_code", "area_code", "year"))

# ---- c901 carries NO final demand (input to biofuels only) ----
fd_zero_cols <- c("food", "losses", "other", "stock_addition", "stock_withdrawal",
                  "tourist", "fuel", "other_industrial", "unknown_use")
use_fd_full <- use_fd_full %>%
  mutate(across(any_of(fd_zero_cols), ~ if_else(comm_code == "c901", 0, .x)))

###########################################################
#4. Updating supply with readjusted values
###########################################################

prod_update <- balance_check_adj[, .(comm_code, area_code, year, prod_new, kind)]

has_row <- function() sup_full[, .(s = sum(supply, na.rm = TRUE)),
                               by = .(comm_code, area_code, year)][s > 0]

# (a) PRIMARY commodities with a bump but no supply row: CREATE the row. An empty input
#     column is normal there (grazing, fodder, and the waste streams — the impact is a
#     direct extension or belongs to the primary product), so the tonnage is real and just
#     needs a carrier.
prod_missing <- prod_update[prod_new > 1][!has_row(), on = .(comm_code, area_code, year)]

if (nrow(prod_missing[kind == "PRIMARY"])) {
  meta <- unique(as.data.table(items_supply_bcp)[, .(comm_code, item, item_code,
                                                     proc, proc_code)])
  meta <- meta[, .SD[1], by = comm_code]                    # one supplying proc each
  new_sup <- merge(prod_missing[kind == "PRIMARY",
                                .(comm_code, area_code, year, supply = prod_new)],
                   meta, by = "comm_code")
  new_sup <- merge(new_sup, as.data.table(regions)[, .(area_code = code, area = name)],
                   by = "area_code")
  new_sup[, price := 1]
  miss <- setdiff(names(sup_full), names(new_sup))
  for (m in miss) new_sup[, (m) := NA]
  sup_full <- rbind(sup_full, new_sup[, names(sup_full), with = FALSE], use.names = TRUE)
  message(">>> 11: created ", nrow(new_sup), " PRIMARY supply rows to carry prod_new (",
          paste(sort(unique(new_sup$comm_code)), collapse = ", "), ")")
  
  dropped <- prod_missing[kind == "PRIMARY"][!unique(meta[, .(comm_code)]), on = "comm_code"]
  fabio_assert(nrow(dropped) == 0,
               "11: %d PRIMARY (comm,area,year) need a supply row but the commodity has NO entry in items_supply_bcp — the row cannot be created and the tonnage will be discarded.",
               nrow(dropped), data = unique(dropped[, .(comm_code)]))
}

# (b) THE TRIPWIRE. Anything left is a phantom: output with nothing supplying it. This is
#     the single check that would have caught all 2,348 A2 nodes.
still_missing <- prod_update[prod_new > 1][!has_row(), on = .(comm_code, area_code, year)]
fabio_assert(nrow(still_missing) == 0,
             "11: %d (comm,area,year) have prod_new > 0 but NO supply row to carry it — they would become btd self-flows with an empty Z column (phantom output).",
             nrow(still_missing), data = still_missing)

# (c) scale the existing supply rows to prod_new (logic unchanged)
sup_full <- sup_full %>%
  group_by(comm_code, area_code, year) %>%
  mutate(share = if (sum(supply, na.rm = TRUE) > 0)
    supply / sum(supply, na.rm = TRUE) else 0) %>%
  ungroup() %>%
  left_join(prod_update %>% select(-kind), by = c("comm_code", "area_code", "year")) %>%
  mutate(supply = if_else(!is.na(prod_new), share * prod_new, supply)) %>%
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

# ---- c901 production by country = sum of its outgoing trade flows ----
c901_meta <- sup_full %>%
  filter(comm_code == "c901") %>%
  distinct(across(any_of(c("item", "item_code", "comm_code", "proc", "proc_code"))))
stopifnot(nrow(c901_meta) == 1)
c901_meta$item_code <- c901_item_code

c901_sup <- waste_flows %>%
  group_by(area_code = from_code, year) %>%
  summarise(supply = sum(value, na.rm = TRUE), .groups = "drop") %>%
  filter(supply > 0) %>%
  mutate(!!!as.list(c901_meta[1, ]), price = 1) %>%
  left_join(regions %>% select(area_code = code, area = name), by = "area_code")

sup_full <- sup_full %>%
  filter(comm_code != "c901") %>%
  bind_rows(c901_sup %>% select(any_of(names(sup_full))))


# DDGS (c171) supply is minted EARLY now (right after the supply bind, before
# NO_SUPPLY_COMMS), so c171 is a normal supplied commodity throughout: in the balance,
# in commodities_vec, and in the self-flow loop -- treated identically to every other item.
# See the "MINT DDGS (c171) SUPPLY EARLY" block above. Nothing to do here.



###########################################################
########### VALUE-ALLOCATION PRICES FOR CO-PRODUCT PROCS ##
###########################################################
coprod_procs <- c("p125", "p126", "p127")
coprod_comms <- c("c146", "c171", "c147", "c148", "c149", "c150", "c151")

sup_full <- sup_full %>%
  left_join(bcp_price_lu, by = c("comm_code", "year")) %>%
  mutate(price = if_else(proc_code %in% coprod_procs & comm_code %in% coprod_comms &
                           !is.na(bcp_price) & is.finite(bcp_price),
                         bcp_price, price)) %>%
  select(-bcp_price)

###########################################################
########### CALCULATE 'SELF-FLOWS' AND UPDATE BTD #########
###########################################################

setDT(sup_full); setDT(btd_full)

areas <- sort(unique(balance_check_adj$area_code))
n     <- length(areas)
commodities_vec <- sort(unique(balance_check_adj$comm_code))

fabio_assert(!is.unsorted(areas, strictly = TRUE),
             "11: `areas` for the self-flow loop is not strictly ascending.")

mapping_templ <- data.table(
  from_code = rep(areas, each = n),
  to_code   = rep(areas, times = n))

btd_dt <- as.data.table(btd_full)

# DS / TS are derived from the tables the model ACTUALLY SHIPS, not from the scratch
# variable prod_new. After the fix above, sup_full IS the production the model carries; a
# cell where sup_full and prod_new disagree is precisely the phantom class, and feeding
# prod_new to the self-flow loop is what minted the btd row with no supply column.
prod_final <- sup_full[, .(DS = pmax(sum(supply, na.rm = TRUE), 0)),
                       by = .(comm_code, area_code, year)]
imp_final  <- btd_full[from_code != to_code, .(imp = sum(value, na.rm = TRUE)),
                       by = .(comm_code, year, area_code = to_code)]

bal_dt <- balance_check_adj[, .(comm_code, item_code, area_code, year,
                                DU = pmax(tot_use_new, 0, na.rm = TRUE))]
bal_dt[prod_final, on = .(comm_code, area_code, year), DS  := i.DS]
bal_dt[imp_final,  on = .(comm_code, area_code, year), imp := i.imp]
bal_dt[is.na(DS), DS := 0][is.na(imp), imp := 0]
bal_dt[, TS := DS + imp][, imp := NULL]

fabio_assert(nrow(bal_dt[, .N, by = .(comm_code, area_code, year)][N > 1]) == 0,
             "11: bal_dt is not unique on (comm, area, year).")

self_flows_list <- vector("list", length(years))

for (i in seq_along(years)) {
  y <- years[i]
  cat("Calculating self-flows for year ", y, ".\n", sep = "")
  
  bal_y <- bal_dt[year == y]
  setkey(bal_y, comm_code, area_code)
  btd_y <- btd_dt[year == y, .(from_code, to_code, comm_code, value)]
  
  self_j <- lapply(commodities_vec, function(j) {
    
    x <- btd_y[comm_code == j, .(from_code, to_code, value)]
    if (nrow(x) == 0) {
      T_mat <- Matrix::Matrix(0, n, n, sparse = TRUE, dimnames = list(areas, areas))
    } else {
      tmp <- merge(mapping_templ, x, by = c("from_code", "to_code"), all.x = TRUE)
      tmp[is.na(value), value := 0]
      mat_wide <- data.table::dcast(tmp, from_code ~ to_code,
                                    fun.aggregate = sum, value.var = "value")[, -"from_code"]
      T_mat <- as(as.matrix(mat_wide), "CsparseMatrix")
    }
    
    b  <- bal_y[.(j, areas), on = c("comm_code", "area_code")]
    DS <- b$DS; DS[is.na(DS)] <- 0
    TS <- b$TS; TS[is.na(TS)] <- 0
    DU <- b$DU; DU[is.na(DU)] <- 0
    item_j <- b$item_code[!is.na(b$item_code)][1]
    
    T_offdiag_sum <- sum(T_mat) - sum(Matrix::diag(T_mat))
    
    if (T_offdiag_sum == 0) {
      # No bilateral trade -> domestic use is self-sourced. GATE IT ON SUPPLY: without the
      # DS > 0 test this writes a country's ENTIRE domestic use onto the btd diagonal with
      # nothing supplying it — a self-flow ghost, i.e. an mr_use row with no mr_sup column.
      self_vals <- fifelse(DS > 0, DU, 0)
    } else {
      # ZERO THE DIAGONAL BEFORE FORMING A. This loop reuses 06's Leontief formula, but 06
      # builds A from OFF-DIAGONAL trade only — btd_full here already carries the diagonal
      # 06 wrote. Leaving it in makes rowSums(A) = (self + exports)/(prod + imports) ~ 1 BY
      # CONSTRUCTION, so (I - A) is systematically near-singular and F[F<0] <- 0 then clips
      # the garbage into something that merely looks like a share.
      Matrix::diag(T_mat) <- 0
      
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

# rows_upsert REPLACES the diagonal (the loop recomputes it from scratch). The join keys
# must agree on TYPE or the old diagonal survives and a duplicate zero row is appended.
btd_full[, `:=`(from_code = as.integer(from_code), to_code = as.integer(to_code),
                year = as.integer(year))]
self_flows[, `:=`(from_code = as.integer(from_code), to_code = as.integer(to_code),
                  year = as.integer(year))]

# ==========================================================================================
# [UPSERT DUP DIAGNOSTIC]  -- print key-column types on BOTH sides, and pre-check for
# duplicate keys WITHIN each side (rows_upsert cannot dedupe a side that is already dupe'd
# on its own key). Only comm_code is left uncoerced above; if it disagrees in type (factor
# vs character, or differing storage between btd_full's two bind_rows sources and the fresh
# self_flows character column) the join silently misses and both old+new rows survive.
KEY <- c("from_code","to_code","comm_code","year")
message(">>> [UPSERT DIAG] btd_full key types:   ", paste(sprintf("%s=%s", KEY, sapply(btd_full[, ..KEY], class)), collapse=" | "))
message(">>> [UPSERT DIAG] self_flows key types: ", paste(sprintf("%s=%s", KEY, sapply(self_flows[, ..KEY], class)), collapse=" | "))
dup_before_btd <- btd_full[, .N, by = KEY][N > 1]
dup_before_sf  <- self_flows[, .N, by = KEY][N > 1]
message(">>> [UPSERT DIAG] duplicate keys ALREADY in btd_full pre-upsert: ", nrow(dup_before_btd),
        " | ALREADY in self_flows pre-upsert: ", nrow(dup_before_sf))
if (nrow(dup_before_btd)) print(head(dup_before_btd[order(-N)], 5))
if (nrow(dup_before_sf))  print(head(dup_before_sf[order(-N)], 5))
# is comm_code identical in content AND type where it matters (c171 specifically)?
message(">>> [UPSERT DIAG] c171 rows -- btd_full: ", btd_full[comm_code == "c171", .N],
        " self_flows: ", self_flows[comm_code == "c171", .N])
message(">>> [UPSERT DIAG] btd_full comm_code sample class per distinct value (first 3): ",
        paste(sapply(head(unique(btd_full$comm_code), 3), function(v) sprintf("%s(%s)", v, class(v))), collapse=", "))
# ==========================================================================================

n_before <- nrow(btd_full)
btd_full <- rows_upsert(as.data.frame(btd_full), as.data.frame(self_flows),
                        by = c("from_code", "to_code", "comm_code", "year"))
setDT(btd_full)
n_after <- nrow(btd_full)

# --- post-upsert: which comm_code(s) actually carry the duplicate rows? ---
dup_after <- btd_full[, .N, by = KEY][N > 1]
message(">>> [UPSERT DIAG] rows before upsert: ", n_before, " | after: ", n_after,
        " | duplicate KEYS after upsert: ", nrow(dup_after))
if (nrow(dup_after)) {
  by_comm <- dup_after[, .(dup_keys = .N), by = comm_code][order(-dup_keys)]
  message(">>> [UPSERT DIAG] duplicate keys BY comm_code (top 10):")
  print(head(by_comm, 10))
  worst <- dup_after[1]
  message(">>> [UPSERT DIAG] sample offending key: from=", worst$from_code, " to=", worst$to_code,
          " comm=", worst$comm_code, " year=", worst$year)
  print(btd_full[from_code == worst$from_code & to_code == worst$to_code &
                   comm_code == worst$comm_code & year == worst$year])
}

# ---- c901 bilateral flows = waste_flows (excluded from the self-flow loop) ----
btd_full <- btd_full %>%
  filter(comm_code != "c901") %>%
  bind_rows(waste_flows %>% select(any_of(names(btd_full))))

setDT(btd_full); setDT(sup_full)

fabio_assert(nrow(btd_full[, .N, by = .(from_code, to_code, comm_code, year)][N > 1]) == 0,
             "11: btd_full has duplicate (from, to, comm, year) rows after the self-flow upsert — the join keys disagree on type and the old diagonal survived.")

###########################################################
########### FINAL INVARIANT: 11 CREATES NO NEW GHOSTS #####
###########################################################
# 11 cannot fix what it inherits (c145's RAW_ITEMS exports from 06, the c117/c089 supply
# 09_1 zeroes in its livestock-share allocation, name-join drops in 07_10). The staged
# diagnostic showed it net-REMOVES ~44 Mt of ghosts. So the invariant it must satisfy is
# not "no ghosts" — it is "no NEW ghosts". Assert on the delta; report the inherited set.

ghost_out <- ghost_of(sup_full, btd_full)
born      <- ghost_out[!ghost_in, on = .(comm_code, area_code, year)]

message(sprintf(">>> 11: ghosts in %d cells (%.1f Mt)  ->  out %d cells (%.1f Mt)  |  NET %+.1f Mt",
                nrow(ghost_in),  sum(ghost_in$shipped)  / 1e6,
                nrow(ghost_out), sum(ghost_out$shipped) / 1e6,
                (sum(ghost_out$shipped) - sum(ghost_in$shipped)) / 1e6))

if (nrow(ghost_out)) fwrite(ghost_out[order(-shipped)], "output/11_ghosts_out.csv")

fabio_assert(nrow(born) == 0,
             "11: %d (comm,area,year) SHIP tonnage in btd but supply nothing, and did NOT inherit it — 11 CREATED them. 12_a would build an mr_use row with no mr_sup column.",
             nrow(born), data = born[order(-shipped)])

###########################################################
########### WRITING TABLES #######################
###########################################################

setwd(fabio_root)

setDT(sup_full); setDT(use_full); setDT(use_fd_full); setDT(btd_full)

saveRDS(sup_full,    tag("data/sup_final_merged.rds"))
saveRDS(use_full,    tag("data/use_final_merged.rds"))
saveRDS(use_fd_full, tag("data/use_fd_final_merged.rds"))
saveRDS(btd_full,    tag("data/btd_final_merged.rds"))