rm(list = ls())

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

library("tidyverse")
library("data.table")
library(dplyr)
source("R/00_system_variables.R")   # na_sum() + 00_9_fabio_helpers.R (fabio_assert,
# bilateralise_topup, btd_add)
source("R/00_run_config.R")         # RUN_MODE / BYPASS_RESCALE / tag() / STRICT

###########################################################
########### LOADING DATA #########
###########################################################

cbs_sua_full <- readRDS("data/cbs_sua_full.rds")
setDT(cbs_sua_full)

if (BYPASS_RESCALE) {
  # ---- non-rescaled run: take the 08_02 pre-rescale inputs (08_03 ignored) ----
  cbs_sua_adjusted <- readRDS("intermediate_data/cbs_sua_adjusted.rds")
  use_final_bcp    <- readRDS("intermediate_data/use_rebal_bcp.rds")
} else {
  # ---- rescaled run: take the 08_03 RED-aligned inputs ----
  cbs_sua_adjusted <- readRDS("intermediate_data/cbs_sua_adjusted_resc.rds")
  use_final_bcp    <- readRDS("data/use_final_bcp.rds")
}
setDT(use_final_bcp); setDT(cbs_sua_adjusted)

items_lu <- fread("inst/items_full_bcp.csv")[!is.na(item_code), .(item_code, comm_code)]

# base btd for the origin routing: 08_03's c145-topped-up table in a rescaled run, the
# untouched 06 output in bypass. 08_04 re-emits it as btd_final_bal.rds in BOTH modes, so
# 11 has a single, mode-symmetric source and btd_final*.rds is never mutated in place.
btd_base <- as.data.table(readRDS(
  if (BYPASS_RESCALE) "data/btd_final.rds" else "data/btd_final_resc.rds"))
btd_base[, value := as.numeric(value)]

###########################################################
####### Updating adjusted input use after rescaling in 08_03 ######
###########################################################

input_use_resc <- use_final_bcp[
  proc %in% c("Biodiesel production", "Renewable diesel production", "Biogasoline production"),
  .(input_use_adj_new = sum(use, na.rm = TRUE)),
  by = .(area_code, year, comm_code)
]
input_use_resc[, `:=`(area_code = as.numeric(area_code),
                      year      = as.numeric(year),
                      comm_code = as.character(comm_code))]

cbs_sua_adjusted[, input_use_adj := 0]                       # wipe stale 08_02 values
cbs_sua_adjusted[input_use_resc, input_use_adj := i.input_use_adj_new,
                 on = .(area_code, year, comm_code)]

# snapshot production BEFORE the ladder, so the "never minted" tripwire in 1c has a baseline
cbs_sua_adjusted[, production_pre := replace_na(production, 0)]

# Snapshot the PRE-EXISTING export violations. FAO's own data breaks
# exports <= production + imports + stock_withdrawal (03_1b warns and carries them through).
# The 1c invariant must fire only on violations WE create, so it is differenced against this.
viol_pre <- cbs_sua_adjusted[
  na_sum(exports, -production, -imports, -stock_withdrawal) > 1,
  .(area_code, item_code, year)]
message(">>> 08_04: ", nrow(viol_pre), " (area,item,year) already violate exports <= supply ",
        "in the input CBS (FAO's own data; carried through).")

###########################################################
####### BTD-SIDE EXPORT CEILING FOR THE STEP-7 DRAWDOWN ######
###########################################################
# The CBS `exports` column is NOT a valid ceiling for an export drawdown, and assuming it
# was is what tripped the shortfall assert on 78 cells (area 256 wanting to redirect 11 kt
# of maize when btd records it shipping 269 t).
#
# WHY THEY DISAGREE: 06_re-exports.R builds `final_result <- t(t(S) * DU)` — it pins btd
# COLUMNS to each importer's domestic_use. NOTHING constrains its ROW sums to equal
# cbs$exports, and round() (06:154) plus the pmax(0, .) clip (06:207) push them further
# apart. FABIO never asserted the identity because nothing needed it: 11 reads exports FROM
# BTD, not from the CBS.
#
# So btd is the binding constraint. A country can only stop exporting mass that btd records
# it actually shipping. Cap the rung there and `shortfall` becomes structurally impossible.

btd_exp_comm <- btd_base[from_code != to_code,
                         .(btd_exports = sum(value, na.rm = TRUE)),
                         by = .(area_code = from_code, year, comm_code)]

# comm -> item is one-to-MANY. Split each commodity's btd export total across its items pro
# rata to their CBS exports (falling back to production for an item never exported), so two
# items of the same commodity cannot each claim the same btd tonnage.
w_it <- cbs_sua_adjusted[, .(area_code = as.integer(area_code), year = as.integer(year),
                             item_code = as.integer(item_code),
                             base = pmax(na_sum(exports, 0), 0),
                             prod = pmax(na_sum(production, 0), 0))]
w_it <- merge(w_it, items_lu, by = "item_code", all.x = TRUE)[!is.na(comm_code)]
w_it[, wt := base]
w_it[, s := sum(wt), by = .(area_code, year, comm_code)]
w_it[s == 0, wt := prod]
w_it[, s := sum(wt), by = .(area_code, year, comm_code)]
w_it <- w_it[s > 0][, wt := wt / s][, .(area_code, year, item_code, comm_code, wt)]

btd_exp <- merge(btd_exp_comm, w_it, by = c("area_code", "year", "comm_code"))
btd_exp[, btd_exports := btd_exports * wt]
btd_exp <- btd_exp[, .(area_code = as.numeric(area_code), year = as.numeric(year),
                       item_code = as.integer(item_code), btd_exports)]

cbs_sua_adjusted[btd_exp, on = .(area_code, year, item_code), btd_exports := i.btd_exports]
cbs_sua_adjusted[is.na(btd_exports), btd_exports := 0]

gap <- cbs_sua_adjusted[na_sum(exports, 0) > btd_exports + 1]
message(">>> 08_04: ", nrow(gap), " (area,item,year) have cbs$exports > their btd row sum. ",
        "btd is the binding ceiling for the step-7 rung.")

###########################################################
####### ADJUSTING USES IN FINAL DEMAND FROM INPUT USE ######
###########################################################

###########################################################
#1. Drawdown ladder: best-corresponding use -> worst.
#
#   PRODUCTION IS NEVER TOPPED UP. It used to be — the old step 7 was
#       production_adj = production_f + to_remove_7
#   which is Bug 1: a country with a feedstock requirement and nothing left to reallocate
#   from had the ENTIRE residual minted as domestic production. The Netherlands "produces"
#   104 kt of palm oil in 2019 with no palm fruit and no palm oil going in; Finland
#   "produces" 385 kt in 2013. Their Z column is empty, so every tonne the HVO chain draws
#   from them carries ZERO land use: the Indonesian deforestation behind it is not
#   reallocated, it is DELETED.
#
#   The ladder now ends in two new rungs instead:
#     step 7  exports  -> the country stops SHIPPING mass it already produced, and consumes
#                         it at home. Production untouched; nothing invented anywhere on
#                         earth. This is the right answer for a country with a real
#                         feedstock base (FRA/DEU rapeseed) — which is exactly where the
#                         RAS scale-up bites. Capped at the BTD row sum (see above).
#     step 8  imports  -> the true residual. Nothing domestic covers it, so it is bought,
#                         and 1b gives it real bilateral origins.
###########################################################

cbs_sua_adjusted <- cbs_sua_adjusted %>%
  mutate(
    # Safe copies (NA -> 0) to work with
    other_f          = replace_na(other, 0),
    stock_addition_f = replace_na(stock_addition, 0),
    tourist_f        = replace_na(tourist, 0),
    processing_f     = replace_na(processing, 0),
    feed_f           = replace_na(feed, 0),
    food_f           = replace_na(food, 0),
    production_f     = replace_na(production, 0),
    imports_f        = replace_na(imports, 0),
    exports_cbs_f    = replace_na(exports, 0),
    # --- Step 1: draw from "other" ---
    to_remove_1   = replace_na(input_use_adj, 0),
    red_other     = pmin(to_remove_1, other_f),
    other_adj     = other_f - red_other,
    to_remove_2   = to_remove_1 - red_other,
    # --- Step 2: draw from "stock_addition" ---
    red_stock          = pmin(to_remove_2, stock_addition_f),
    stock_addition_adj = stock_addition_f - red_stock,
    to_remove_3        = to_remove_2 - red_stock,
    # --- Step 3: draw from "tourist" ---
    red_tourist   = pmin(to_remove_3, tourist_f),
    tourist_adj   = tourist_f - red_tourist,
    to_remove_4   = to_remove_3 - red_tourist,
    # --- Step 4: draw from "processing" ---
    red_processing = pmin(to_remove_4, processing_f),
    processing_adj = processing_f - red_processing,
    to_remove_5    = to_remove_4 - red_processing,
    # --- Step 5: draw from "feed" ---
    red_feed      = pmin(to_remove_5, feed_f),
    feed_adj      = feed_f - red_feed,
    to_remove_6   = to_remove_5 - red_feed,
    # --- Step 6: draw from "food" ---
    red_food      = pmin(to_remove_6, food_f),
    food_adj      = food_f - red_food,
    to_remove_7   = to_remove_6 - red_food,
    # --- Step 7: draw from "exports", capped at what BTD records being shipped ---
    exports_f     = pmin(exports_cbs_f, btd_exports),   # the BINDING ceiling
    red_exports   = pmin(to_remove_7, exports_f),
    exports_adj   = exports_cbs_f - red_exports,        # subtract from the FULL cbs value
    to_remove_8   = to_remove_7 - red_exports,
    # --- Step 8: the true residual is an IMPORT. Never production. ---
    self_topup     = red_exports,       # -> btd DIAGONAL (funded by the export cut)
    import_topup   = to_remove_8,       # -> btd OFF-DIAGONAL, bilateralised in 1b
    imports_adj    = imports_f + to_remove_8,
    production_adj = production_f        # <- the Bug 1 fix: production is never minted
  ) %>%
  # Overwrite originals with adjusted values, then drop helpers
  mutate(
    other          = other_adj,
    stock_addition = stock_addition_adj,
    tourist        = tourist_adj,
    processing     = processing_adj,
    feed           = feed_adj,
    food           = food_adj,
    production     = production_adj,
    exports        = exports_adj,
    imports        = imports_adj,
    input_use      = input_use_adj
  ) %>%
  select(
    -ends_with("_f"),
    -ends_with("_adj"),
    -starts_with("to_remove_"),
    -starts_with("red_")
  ) %>%
  # self_topup / import_topup / production_pre must survive: 1b and 1c need them
  select(year:use, input_use, self_topup, import_topup, production_pre)

setDT(cbs_sua_adjusted)     # the dplyr pipe returns a tbl; everything below is data.table


###########################################################
#1b. ORIGIN ROUTING FOR THE RESIDUAL FEEDSTOCK REQUIREMENT
###########################################################
# Both halves of the residual need a bilateral home in btd. 06_re-exports.R ran BEFORE 08
# and never saw this demand, so btd carries no flow for it — and 11 would re-mint the exact
# same tonnage as prod_new (Bug 2), putting the phantom straight back.
#
#   self_topup   -> raise the btd DIAGONAL, and cut the same country's OUTGOING flows by
#                   exactly as much. Mass conserved; production untouched.
#   import_topup -> route to real ORIGINS via the commodity's existing import mix for that
#                   (area, year), falling back to world export shares. For NLD/FIN palm oil
#                   that mix is IDN/MYS, so the Indonesian land use lands where it belongs.
#
# Order matters: the export cut lands on btd FIRST, so the headroom the import routing sees
# already reflects it.

# ---------------------------------------------------------------------------
# (a) SELF-FLOW TOP-UP — raise the diagonal
# ---------------------------------------------------------------------------
self_add <- cbs_sua_adjusted[!is.na(self_topup) & self_topup > 1,
                             .(area_code = as.integer(area_code),
                               year      = as.integer(year),
                               item_code = as.integer(item_code),
                               value     = as.numeric(self_topup))]
self_add <- merge(self_add, items_lu, by = "item_code", all.x = TRUE)[!is.na(comm_code)]
self_add <- self_add[, .(value = sum(value)),
                     by = .(from_code = area_code, to_code = area_code,
                            year, comm_code, item_code)]

# c145 (UCO) has NO diagonal in btd at this stage — 06 keeps item 1274 as a RAW_ITEM, so
# only its cross-border flows are present. btd_add() will therefore APPEND a diagonal row
# rather than raise one. That is defensible (domestic UCO collection feeding domestic
# biodiesel) but it is a NEW STRUCTURAL ROW, and in a RESCALED run 08_03's
# allocate_c145_origin() already estimated the same quantity via its btd path. If both
# fire you are adding the same domestic UCO twice.
new_diag <- self_add[!btd_base, on = .(from_code, to_code, year, comm_code)]
if (nrow(new_diag))
  message(">>> 08_04: ", nrow(new_diag), " NEW btd diagonal rows created (",
          round(sum(new_diag$value)), " t) — comms: ",
          paste(sort(unique(new_diag$comm_code)), collapse = ", "),
          ".  CHECK c145 against 08_03's c145_origin before trusting it.")

# ---------------------------------------------------------------------------
# (b) MIRROR IT — scale the same country's OFF-DIAGONAL outgoing flows down pro rata.
#     This is what FUNDS the diagonal: mass stops being exported, it does not appear.
# ---------------------------------------------------------------------------
cut  <- self_add[, .(cut = sum(value)), by = .(from_code, year, comm_code)]
otot <- btd_base[from_code != to_code, .(tot = sum(value, na.rm = TRUE)),
                 by = .(from_code, year, comm_code)]
cut  <- merge(cut, otot, by = c("from_code", "year", "comm_code"), all.x = TRUE)
cut[is.na(tot), tot := 0]

# Step 7 is now capped at the BTD row sum, so this is structurally impossible. It remains
# as a guard against the item<->comm split going wrong, NOT against a modelling gap.
shortfall <- cut[cut > tot + 1]
fabio_assert(nrow(shortfall) == 0,
             "08_04: %d (area,comm,year) want to redirect more exports into self-supply than btd records shipping — the btd_exports ceiling or its item split is wrong.",
             nrow(shortfall),
             data = shortfall[order(-(cut - tot))][, .(from_code, year, comm_code, cut, tot)])

cut[, f := pmax(0, 1 - cut / pmax(tot, 1))]
btd_base[cut, on = .(from_code, year, comm_code), f := i.f]
btd_base[from_code != to_code & !is.na(f), value := value * f]
btd_base[, f := NULL]

btd_bal <- btd_add(btd_base, self_add)

message(">>> 08_04: self-flow top-up ", round(sum(self_add$value)), " t across ",
        nrow(self_add), " cells, funded by an equal cut to those countries' exports.")

# ---------------------------------------------------------------------------
# (c) IMPORT TOP-UP — the residual nothing domestic can cover
# ---------------------------------------------------------------------------
need <- cbs_sua_adjusted[!is.na(import_topup) & import_topup > 1,
                         .(area_code = as.integer(area_code),
                           year      = as.integer(year),
                           item_code = as.integer(item_code),
                           need      = as.numeric(import_topup))]
need <- merge(need, items_lu, by = "item_code", all.x = TRUE)[!is.na(comm_code)]

# Headroom must be read from the FULL CBS, not from cbs_sua_adjusted: the latter is a
# SUBSET (it is rows_update'd into cbs_sua_full below), so an origin like IDN may not
# appear in it at all — and a missing cap silently becomes Inf, i.e. uncapped. Build the
# post-ladder view of the whole CBS and take headroom from that.
cbs_now  <- copy(cbs_sua_full)
upd_cols <- intersect(c("production", "imports", "exports", "stock_withdrawal"),
                      names(cbs_sua_adjusted))
cbs_now[cbs_sua_adjusted, on = .(item_code, year, area_code),
        (upd_cols) := mget(paste0("i.", upd_cols))]

# How much MORE each origin can ship before we are inventing ITS production too: its own
# total supply not already committed to exports. Computed AFTER (b), so the export cut has
# already freed up (or not) the relevant room. Origins that would blow through the cap are
# frozen and the remainder is water-filled onto the others; what nothing can cover is
# LOGGED, never silently absorbed.
hr <- cbs_now[, .(cap = pmax(na_sum(production, imports, stock_withdrawal, -exports), 0)),
              by = .(area_code = as.integer(area_code),
                     year      = as.integer(year),
                     item_code = as.integer(item_code))]
hr <- merge(hr, items_lu, by = "item_code", all.x = TRUE)[!is.na(comm_code)]
hr <- hr[, .(cap = sum(cap)), by = .(area_code, year, comm_code)]   # caps add across items

topup_flows <- bilateralise_topup(need, btd_bal, headroom = hr)
resid <- attr(topup_flows, "residual")

message(">>> 08_04: import top-up ", round(sum(need$need)), " t needed, ",
        round(sum(topup_flows$value)), " t routed to origins, ",
        round(sum(resid$unallocated)), " t unallocated (", nrow(resid), " cells)")
if (nrow(resid)) data.table::fwrite(resid, "output/08_04_topup_unallocated.csv")

btd_bal <- btd_add(btd_bal, topup_flows)

# ---------------------------------------------------------------------------
# (d) KEEP THE CBS IN STEP WITH BTD
# ---------------------------------------------------------------------------
# The consumer's `exports` (down by self_topup) and `imports` (up by import_topup) were
# already written by the ladder. What remains is the ORIGINS' side: their exports rise by
# what they now ship. Their production is NOT bumped here — 11 does that, and only where a
# supply row exists to carry it. The headroom cap in (c) is what keeps the 1c export
# invariant true for them.
dexp <- topup_flows[from_code != to_code, .(d_exports = sum(value)),
                    by = .(area_code = from_code, year, comm_code)]

# Same one-to-many trap as the btd_exports ceiling: joining items_lu straight onto dexp
# would fan the delta out and add the FULL amount to EVERY item of the commodity. Reuse the
# w_it weights to split it pro rata.
dexp <- merge(dexp, w_it, by = c("area_code", "year", "comm_code"))
dexp[, d_exports := d_exports * wt]
dexp[, `:=`(year = as.numeric(year), area_code = as.numeric(area_code),
            item_code = as.integer(item_code))]

lost <- sum(topup_flows[from_code != to_code, value]) - sum(dexp$d_exports)
if (abs(lost) > 1)
  message(">>> 08_04: ", round(lost), " t of origin exports could not be attributed to an ",
          "item (origin absent from the CBS) — cbs and btd will differ by that much.")

cbs_sua_adjusted[dexp, on = .(year, area_code, item_code),
                 exports := fcoalesce(exports, 0) + i.d_exports]

# --- mass conservation across the whole block --------------------------------
# What the ladder pulled out of the consumers' other uses must equal what the diagonal
# gained plus what the origins now ship, less what nothing could cover.
# --- mass conservation across the whole block --------------------------------
# What the ladder pulled out of the consumers' other uses must equal what the diagonal
# gained plus what the origins now ship, less what nothing could cover.

moved   <- sum(self_add$value) + sum(topup_flows$value)
claimed <- sum(cbs_sua_adjusted$self_topup,   na.rm = TRUE) +
  sum(cbs_sua_adjusted$import_topup, na.rm = TRUE) -
  sum(resid$unallocated)

gap_abs <- abs(moved - claimed)
gap_rel <- gap_abs / pmax(claimed, 1)

message(sprintf(">>> 08_04: mass check — asked %.0f t, routed %.0f t (gap %.0f t, %.2e rel)",
                claimed, moved, gap_abs, gap_rel))

fabio_assert(gap_rel < 1e-4 || gap_abs < 100,
             "08_04: routed %.0f t but the ladder asked for %.0f t (%.3g%% of the total) — the top-up is not mass-conserving. A gap this large is a lost commodity or country, not rounding.",
             moved, claimed, 100 * gap_rel)


###########################################################
#1c. INVARIANTS
###########################################################

# (i) THE BUG 1 TRIPWIRE. Production must not have moved, anywhere, for any reason.
minted <- cbs_sua_adjusted[na_sum(production, -production_pre) > 1,
                           .(area_code, item_code, year, production_pre, production,
                             input_use, self_topup, import_topup)]
fabio_assert(nrow(minted) == 0,
             "08_04: %d cells had production MINTED by the ladder — step 8 is topping up production again.",
             nrow(minted), data = minted[order(-production)])

# (ii) a country cannot export more than it has. FAO's own data violates this (03_1b warns
#      and carries it through), so we stop only on violations WE created.
viol <- cbs_sua_adjusted[na_sum(exports, -production, -imports, -stock_withdrawal) > 1,
                         .(area_code, item_code, year, production, imports,
                           stock_withdrawal, exports)]
viol_new <- viol[!viol_pre, on = .(area_code, item_code, year)]
fabio_assert(nrow(viol_new) == 0,
             "08_04: %d NEW (area,item,year) export more than production + imports + stock_withdrawal — the headroom cap in 1b is too loose.",
             nrow(viol_new), data = viol_new)


###########################################################
#2. Recalculate supply & use ######
###########################################################

cbs_sua_adjusted[, `:=`(domestic_supply = production)]
cbs_sua_adjusted[, `:=`(supply = na_sum(domestic_supply, imports))]
cbs_sua_adjusted[, `:=`(domestic_use = na_sum(food, feed, other, tourist, seed, losses,
                                              processing, stock_addition, -stock_withdrawal))]
cbs_sua_adjusted[, `:=`(use = na_sum(domestic_use, exports))]

# drop the scratch columns so they never reach cbs_sua_bal
cbs_sua_adjusted[, c("self_topup", "import_topup", "production_pre") := NULL]

cbs_sua_full <- cbs_sua_full %>% mutate(input_use = 0)
cbs_sua_bal <- rows_update(cbs_sua_full, cbs_sua_adjusted,
                           by = c("item_code", "year", "area_code")) %>%
  relocate(input_use, .after = balancing)


###########################################################
########### SAVING TABLES #########
###########################################################

setwd(fabio_root)

saveRDS(cbs_sua_bal, tag("data/cbs_sua_bal.rds"))
saveRDS(btd_bal,     tag("data/btd_final_bal.rds"))   # NEW: btd + the 08_04 origin routing
# btd_final_resc.rds / btd_final.rds are NOT mutated -> the step stays idempotent on re-run.


###########################################################
########### CANONICAL-CASE CHECK #########
###########################################################

chk <- as.data.table(cbs_sua_bal)
print(chk[area == "Finland" & item == "Palm Oil", .(year, production, imports, input_use)])
print(chk[area == "Netherlands" & item == "Palm Oil" & year == 2019,
          .(production, imports, input_use)])

fabio_assert(chk[area %in% c("Finland", "Netherlands") & item == "Palm Oil",
                 sum(production, na.rm = TRUE)] < 1,
             "08_04: FIN/NLD still 'produce' palm oil — Bug 1 is not fixed.")

rm(list = ls())