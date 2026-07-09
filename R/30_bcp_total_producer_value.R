# =============================================================================
# 30_bcp_total_producer_value.R
# Producer total value = total product output x unit price, at (area x commodity
# x year) grain, for two disjoint commodity sets:
#   * the bio-based commodities (comm_group in Biofuels / Building blocks /
#     Biopolymers), priced from the bilateral-flow producer prices; and
#   * the plain-tonne commodities c141-c145 (Triticale, Molasses, Castor oil
#     seeds, Oil of castor beans, chemically modified fats/oils), priced in
#     USD/tonne from the FABIO v2 price outputs on a FAO-then-trade ladder (see
#     the dedicated block near the output section).
#
# Inputs :  <output_dir_mode>/X.rds            total product output (rows = iso3c_commcode)
#           <output_dir_mode>/io_labels.csv    row-aligned labels for X
#           intermediate_data/prices_bcp.rds   bilateral-flow producer prices
#           inst/regions_full.csv              exporter_iso3 -> area_code
#           inst/items_full_bcp.csv            in-scope commodity master
#           intermediate_data/tcf_table_clean.rds  biofuel volume<->mass factors
#           intermediate_data/biogasoline_blend_shares.rds  per-country ETBE/eth shares
#           <FABIO v2>/data/total_value/Prices_E_All_Data_with_USD.csv  FAO producer prices (USD/tonne)
#           <FABIO v2>/data/total_value/bilateral_trade_prices.rds      trade prices (USD/unit)
#           inst/value_added/concordance_areas_fao_producer_prices_fabio.csv  FAO->FABIO area map
# Outputs:  intermediate_data/bcp_producer_total_values.rds / .csv
#           intermediate_data/value_added_diagnostics/bcp_producer_total_values_diagnostics.csv
#             Summary table: input-filtering funnel, winsor-cap counts,
#             unmatched products / exporters, priceless commodities, and the
#             biogasoline blend coverage (one row per finding).
#           intermediate_data/value_added_diagnostics/bcp_price_winsor_entries.csv
#             Per-entry MAD-cap audit: one row per priced (exporter x product x
#             year) the cap was evaluated against (NOT just clipped ones), with
#             price_pre / price_post, the cap band [lo, hi], per-entry mad_z and
#             a winsorized flag. Sorted by |mad_z| desc.
#
# Data-cleaning diagnostics (console + CSV) mirror the 13-series price scripts:
# a cat() funnel of row drops at each stage, per-product winsor-cap counts with
# a per-entry CSV, a fallback-ladder fill-mix table, and a valuation-coverage
# table by commodity group.
#
# Biogasoline (c146) note: the physical model (07_06_balancing_bf.R) builds
# Biogasoline by summing Bioethanol + ETBE volumes per country-year. Its producer
# price is therefore a blend of the Bioethanol and ETBE prices (both already in
# USD/tonne in prices_bcp), weighted by the REAL per-country production shares
# that 07_06 exports to biogasoline_blend_shares.rds -- not the bare Bioethanol
# price and not a global trade-weighted proxy. The two components are priced on
# independent fallback ladders and blended per (iso3c, year); weights are
# renormalised onto whichever component carries a price, and country-years absent
# from the share table fall back to pure Bioethanol. Reported in diagnostics.
# =============================================================================

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

library(data.table)

source("R/00_system_variables.R")
source("R/00_run_config.R")
output_dir_mode <- mode_dir(output_dir_bcp)

IN_SCOPE_GROUPS <- c("Biofuels", "Building blocks", "Biopolymers")


# --- local helpers -----------------------------------------------------------

wq_mean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

wq_median <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  x <- x[ok]; cw <- cumsum(w[ok][order(x)]) / sum(w[ok]); x <- sort(x)
  x[which(cw >= 0.5)[1L]]
}

# Per-product winsor band in log space (prices are positive, heavy-tailed).
# Returns one row per group: log-space center/scale, the back-transformed price
# cap band [lo, hi], and n (positive obs). Products with < min_obs positive
# prices, or a degenerate MAD, carry an NA band and pass through UNCAPPED. This
# is the band-returning form of the old scalar mad_cap(): the applied cap is
# identical, but the band is now auditable per entry (mirrors 13_2's
# compute_winsor_stats() + build_winsor_diagnostic()).
mad_cap_stats <- function(x, k = 3, min_obs = 8L) {
  v <- x[is.finite(x) & x > 0]
  if (length(v) < min_obs)
    return(list(n = length(v), center = NA_real_, scale = NA_real_,
                lo = NA_real_, hi = NA_real_))
  lx <- log(v); m <- median(lx); s <- mad(lx, constant = 1.4826)
  if (!is.finite(s) || s <= 0)
    return(list(n = length(v), center = m, scale = NA_real_,
                lo = NA_real_, hi = NA_real_))
  list(n = length(v), center = m, scale = s,
       lo = exp(m - k * s), hi = exp(m + k * s))
}


# --- product -> comm_code map (exact string) ---------------------------------
# Biogasoline (c146) is modelled as Bioethanol + ETBE (see 07_06_balancing_bf.R).
# The two are priced SEPARATELY (own fallback ladders, keyed by `pcode`) and then
# blended per country-year with the REAL production shares from
# intermediate_data/biogasoline_blend_shares.rds. `pcode` == comm_code for every
# other commodity (single component). Cellulose acetate has no in-scope commodity
# and is dropped/reported.
product_map <- data.table(
  product   = c("Bioethanol", "ETBE", "Biodiesel", "Glycerol, crude", "Lactic acid",
                "Sebacic acid", "Succinic acid", "1,4-butanediol",
                "Epichlorohydrin", "Polylactic acid"),
  comm_code = c("c146", "c146", "c147", "c148", "c152",
                "c153", "c154", "c159",
                "c162", "c163"),
  pcode     = c("c146_eth", "c146_etbe", "c147", "c148", "c152",
                "c153", "c154", "c159",
                "c162", "c163"))

# Biofuel comm_code -> tcf fuel name (bridge; tcf_table_clean has no Biogasoline row).
biofuel_bridge <- data.table(comm_code = c("c146", "c147"),
                             fuel      = c("Bioethanol", "Biodiesel"))


# --- total output X (long) ---------------------------------------------------
X <- readRDS(file.path(output_dir_mode, "X.rds"))
io_labels <- fread(file.path(output_dir_mode, "io_labels.csv"))
io_labels[, row_id := paste(iso3c, comm_code, sep = "_")]

X_long <- melt(as.data.table(X, keep.rownames = "row_id"),
               id.vars       = "row_id",
               variable.name  = "year",
               value.name     = "total_product_output",
               variable.factor = FALSE)

X_long <- merge(
  X_long,
  io_labels[, .(row_id, iso3c, area_code, comm_code, item, unit, comm_group)],
  by = "row_id", all.x = TRUE)

X_long <- X_long[comm_group %in% IN_SCOPE_GROUPS & year %in% as.character(years)]


# --- masters -----------------------------------------------------------------
items          <- fread("inst/items_full_bcp.csv")
in_scope_items <- unique(items[comm_group %in% IN_SCOPE_GROUPS, .(comm_code, item)])
regions        <- fread("inst/regions_full.csv")[current == TRUE, .(iso3c, area_code = code)]


# --- prices ------------------------------------------------------------------
prices <- as.data.table(readRDS("intermediate_data/prices_bcp.rds"))
prices[, year := as.character(year)]
n_raw <- nrow(prices)

# Stage the input filter so each drop reason is countable (funnel).
prices  <- prices[year %in% as.character(years)]
n_year  <- nrow(prices)
n_bad_qty   <- prices[!(is.finite(qty) & qty > 0), .N]
n_bad_price <- prices[  is.finite(qty) & qty > 0 &
                          !(is.finite(unit_price) & unit_price > 0), .N]
prices  <- prices[is.finite(qty) & qty > 0 & is.finite(unit_price) & unit_price > 0]
n_valid <- nrow(prices)

# product -> comm_code
unmatched_products <- setdiff(unique(prices$product), product_map$product)
n_drop_product <- prices[!product %in% product_map$product, .N]
prices <- merge(prices, product_map, by = "product", all.x = TRUE)
prices <- prices[!is.na(comm_code)]

# exporter -> area_code
prices[regions, area_code := i.area_code, on = c(exporter_iso3 = "iso3c")]
unmatched_iso3 <- sort(unique(prices[is.na(area_code), exporter_iso3]))
n_drop_region  <- prices[is.na(area_code), .N]
prices <- prices[!is.na(area_code)]
n_priced_rows <- nrow(prices)

cat("\nInput price-table filtering (prices_bcp)\n")
cat("  Raw rows:                              ", n_raw,          "\n", sep = "")
cat("  Dropped: outside model years:          ", n_raw - n_year, "\n", sep = "")
cat("  Dropped: non-positive / NA quantity:   ", n_bad_qty,      "\n", sep = "")
cat("  Dropped: non-positive / NA unit_price: ", n_bad_price,    "\n", sep = "")
cat("  Dropped: product not in scope map:     ", n_drop_product, "\n", sep = "")
cat("  Dropped: exporter not in regions:      ", n_drop_region,  "\n", sep = "")
cat("  Rows surviving to pricing:             ", n_priced_rows,  "\n", sep = "")
if (length(unmatched_products))
  cat("  Unmatched products:                    ",
      paste(unmatched_products, collapse = ", "), "\n", sep = "")
if (length(unmatched_iso3))
  cat("  Unmatched exporter iso3 (-> dropped):  ",
      paste(unmatched_iso3, collapse = ", "), "\n", sep = "")


# --- MAD winsorization (cap) with per-entry diagnostics ----------------------
# Same k = 3 / min_obs = 8 log-space cap as before, but driven off a per-product
# stats table so the applied band is auditable. Snapshot the pre-cap price and
# the log-space robust z BEFORE clipping, so sign / magnitude are directly
# comparable to the implied |z| = 3 boundary. Entries in short or degenerate
# products carry mad_z = NA and cannot be winsorized (NA band => cap fails).
wins_stats <- prices[, mad_cap_stats(unit_price), by = product]
prices[wins_stats, `:=`(wins_n = i.n, wins_center = i.center, wins_scale = i.scale,
                        wins_lo = i.lo, wins_hi = i.hi), on = "product"]

prices[, unit_price_pre := unit_price]
prices[, mad_z := fifelse(!is.finite(wins_scale) | wins_scale == 0, NA_real_,
                          (log(unit_price) - wins_center) / wins_scale)]

prices[, unit_price_cap := unit_price]
prices[!is.na(wins_lo) & unit_price_cap < wins_lo, unit_price_cap := wins_lo]
prices[!is.na(wins_hi) & unit_price_cap > wins_hi, unit_price_cap := wins_hi]
prices[, winsorized := is.finite(unit_price_pre) & is.finite(unit_price_cap) &
         unit_price_pre != unit_price_cap]

n_prod_capped   <- wins_stats[!is.na(scale), .N]
n_prod_untouch  <- wins_stats[ is.na(scale), .N]
n_wins_obs      <- prices[winsorized == TRUE, .N]

cat("\nMAD winsorization diagnostics (k = 3, min_obs = 8, log space)\n")
cat("  Products with a cap band applied:      ", n_prod_capped,  "\n", sep = "")
cat("  Products left untouched (<8 obs / MAD0):", n_prod_untouch, "\n", sep = "")
cat("  Observations capped:                   ", n_wins_obs,     "\n", sep = "")
if (n_wins_obs > 0) {
  top_wins <- prices[winsorized == TRUE, .N, by = .(comm_code, product)][order(-N)]
  cat("  Products by observations capped:\n")
  print(head(top_wins, 10L))
}

# Per-entry winsor diagnostic: one row per priced entry the cap was evaluated
# against (NOT just clipped ones), sorted by |mad_z| desc so the tail floats to
# the top. Mirrors output/diagnostics/trade_prices_winsorized_entries.csv.
winsor_entries <- prices[, .(
  exporter_iso3, area_code, product, comm_code, year,
  price_pre = unit_price_pre, price_post = unit_price_cap,
  lo = wins_lo, hi = wins_hi, n_obs = wins_n, mad_z, winsorized)]
winsor_entries <- winsor_entries[order(-abs(mad_z))]

prices[, c("wins_n", "wins_center", "wins_scale", "wins_lo", "wins_hi",
           "unit_price_pre", "mad_z", "winsorized") := NULL]


# --- price fallback ladder, at the price-component (`pcode`) grain -------------
# Same 4-tier ladder as before, but keyed by pcode so the two biogasoline
# components (c146_eth, c146_etbe) each get an independent price to blend later.
price_direct  <- prices[, .(price = wq_mean(unit_price_cap, qty)),   by = .(area_code, pcode, year)]
price_country <- prices[, .(price = wq_mean(unit_price_cap, qty)),   by = .(area_code, pcode)]
price_py      <- prices[, .(price = wq_median(unit_price_cap, qty)), by = .(pcode, year)]
price_p       <- prices[, .(price = median(unit_price_cap)),         by = .(pcode)]

attach_ladder <- function(dt) {
  dt[, `:=`(price = NA_real_, price_source = NA_character_)]
  dt[price_direct, `:=`(price = i.price, price_source = "trade_weighted"),
     on = .(area_code, pcode, year)]
  dt[price_country, `:=`(
    price        = fifelse(is.na(price), i.price, price),
    price_source = fifelse(is.na(price), "trade_country", price_source)),
    on = .(area_code, pcode)]
  dt[price_py, `:=`(
    price        = fifelse(is.na(price), i.price, price),
    price_source = fifelse(is.na(price), "trade_product_year_median", price_source)),
    on = .(pcode, year)]
  dt[price_p, `:=`(
    price        = fifelse(is.na(price), i.price, price),
    price_source = fifelse(is.na(price), "trade_product_median", price_source)),
    on = .(pcode)]
  dt[]
}

# --- non-biogasoline commodities: pcode == comm_code, one price per row --------
X_main <- X_long[comm_code != "c146"]
X_main[, pcode := comm_code]
attach_ladder(X_main)

# --- biogasoline (c146): blend Bioethanol & ETBE prices by REAL country shares -
X_bio <- X_long[comm_code == "c146"]

price_eth  <- attach_ladder(copy(X_bio)[, pcode := "c146_eth"])[
  , .(row_id, year, price_eth = price, src_eth = price_source)]
price_etbe <- attach_ladder(copy(X_bio)[, pcode := "c146_etbe"])[
  , .(row_id, year, price_etbe = price, src_etbe = price_source)]
X_bio <- merge(X_bio, price_eth,  by = c("row_id", "year"), all.x = TRUE)
X_bio <- merge(X_bio, price_etbe, by = c("row_id", "year"), all.x = TRUE)

# Real, production-based per-country blend shares from 07_06_balancing_bf.R.
# Graceful fallback to pure Bioethanol (etbe_share = 0) when the table or a
# particular country-year key is unavailable.
shares_path <- "intermediate_data/biogasoline_blend_shares.rds"
if (file.exists(shares_path)) {
  shares <- as.data.table(readRDS(shares_path))
  shares[, year := as.character(year)]
  X_bio[shares, `:=`(etbe_share = i.etbe_share, eth_share = i.eth_share),
        on = .(iso3c, year)]
} else {
  warning("biogasoline_blend_shares.rds not found; c146 priced as pure Bioethanol")
}
if (!"etbe_share" %in% names(X_bio)) X_bio[, `:=`(etbe_share = NA_real_, eth_share = NA_real_)]
X_bio[is.na(etbe_share) | is.na(eth_share), `:=`(etbe_share = 0, eth_share = 1)]

# Blend: renormalise the shares over the components that actually carry a price,
# so a country wanting ETBE but lacking any ETBE price falls back onto ethanol.
X_bio[, `:=`(w_eth  = fifelse(is.finite(price_eth),  eth_share,  0),
             w_etbe = fifelse(is.finite(price_etbe), etbe_share, 0))]
X_bio[, w_sum := w_eth + w_etbe]
X_bio[, price := fifelse(
  w_sum > 0,
  (w_eth * fcoalesce(price_eth, 0) + w_etbe * fcoalesce(price_etbe, 0)) / w_sum,
  NA_real_)]
X_bio[, price_source := fifelse(
  is.na(price), NA_character_,
  sprintf("biogasoline_blend(eth=%.2f,etbe=%.2f)", w_eth / w_sum, w_etbe / w_sum))]

# --- recombine (keep a copy of the blend detail for diagnostics) --------------
X_bio_keep <- copy(X_bio)
blend_cols <- c("price_eth", "src_eth", "price_etbe", "src_etbe",
                "etbe_share", "eth_share", "w_eth", "w_etbe", "w_sum")
X_bio[, (blend_cols) := NULL]
X_main[, pcode := NULL]
X_long <- rbind(X_main, X_bio, use.names = TRUE)


# --- universal assumed price for the HVO co-products (c149/c150/c151) ---------
# These carry no trade-based producer price, so the fallback ladder above leaves
# them unpriced. Assign ONE flat USD/t price each (from 00_system_variables.R),
# constant across countries and years, wherever the ladder produced no price.
# Applied after the ladder so the assumed constant is never itself capped or
# trade-weighted; tagged as its own source tier for the fill-mix diagnostic.
X_long[universal_bcp_prices, `:=`(
  price        = fifelse(is.na(price), i.price_usd_t, price),
  price_source = fifelse(is.na(price), "assumption_universal", price_source)),
  on = "comm_code"]


# --- price fallback-ladder fill-mix ------------------------------------------
# How each priced (area x commodity x year) cell got its price. Biogasoline
# cells carry a per-country blend tag; bucket those together so the mix stays
# readable. Mirrors 13_2's "Fill-mix (price_source counts)" table.
X_long[, price_tier := fcase(
  is.na(price_source),                          "unpriced",
  grepl("^biogasoline_blend", price_source),    "biogasoline_blend",
  default = price_source)]
fill_mix <- X_long[, .(n_cells = .N), by = price_tier][order(-n_cells)]
cat("\nPrice fallback-ladder fill-mix (cells by source tier)\n")
print(fill_mix)
X_long[, price_tier := NULL]


# --- unit reconciliation and value -------------------------------------------
tcf    <- as.data.table(readRDS("intermediate_data/tcf_table_clean.rds"))
tcf_kl <- tcf[Type == "conversion_rate" & input == output & output_unit == "kl",
              .(fuel = output, output_qty)]
tcf_kl <- tcf_kl[is.finite(output_qty) & output_qty > 0,
                 .(kl_per_t = output_qty[1L]), by = fuel]

X_long[biofuel_bridge, fuel := i.fuel, on = "comm_code"]
X_long[tcf_kl, kl_per_t := i.kl_per_t, on = "fuel"]

# HVO co-products (c149/c150/c151): "1000 liters" output with no tcf fuel bridge
# -> attach the density directly by comm_code (kl per t = 1 / t_per_1000L), so the
# qty_t reconciliation below converts their output to tonnes.
X_long[universal_bcp_prices, kl_per_t := 1 / i.t_per_1000L, on = "comm_code"]

# tonnes: direct where unit == "tonnes"; X / (kl per tonne) where unit == "1000 liters".
X_long[, qty_t := fcase(
  unit == "tonnes",                                    total_product_output,
  unit == "1000 liters" & is.finite(kl_per_t),         total_product_output / kl_per_t,
  default = NA_real_)]

X_long[, total_value := fcase(
  is.na(price) | is.na(qty_t),   NA_real_,
  qty_t == 0 | price == 0,       0,
  default = qty_t * price)]
X_long[, total_value_source := fifelse(is.finite(total_value), "output_x_price", NA_character_)]


# --- valuation coverage by group ---------------------------------------------
# Final funnel: of the in-scope cells per group, how many carry a price and how
# many resolve to a value (a priced cell can still be value-less if unit
# reconciliation failed -- e.g. a "1000 liters" biofuel with no density factor).
cov <- X_long[, .(cells  = .N,
                  priced = sum(is.finite(price)),
                  valued = sum(is.finite(total_value))), by = comm_group]
setorder(cov, comm_group)
cat("\nValuation coverage by commodity group\n")
print(cov)


# --- diagnostics -------------------------------------------------------------
priced_codes  <- unique(X_long[is.finite(price), comm_code])
priceless     <- in_scope_items[!comm_code %in% priced_codes]
biofuel_no_fac <- unique(X_long[is.finite(price) & unit == "1000 liters" &
                                  !is.finite(kl_per_t), .(comm_code, item)])

# c146 blend diagnostics: realized ethanol/ETBE weights from the real-share blend.
bio_diag    <- X_bio_keep[is.finite(price)]
n_bio       <- nrow(bio_diag)
n_bio_etbe  <- bio_diag[etbe_share > 0, .N]
mean_etbe_w <- if (n_bio) bio_diag[, mean(w_etbe / w_sum)] else NA_real_
n_renorm    <- bio_diag[etbe_share > 0 & !is.finite(price_etbe), .N]

diagnostics <- rbindlist(list(
  data.table(category = "input_filtering",
             comm_code = NA_character_, item = NA_character_,
             detail = sprintf(paste0("raw=%d kept=%d | dropped: years=%d qty=%d ",
                                     "price=%d unmapped_product=%d unmapped_region=%d"),
                              n_raw, n_priced_rows, n_raw - n_year, n_bad_qty,
                              n_bad_price, n_drop_product, n_drop_region)),
  data.table(category = "winsor_cap",
             comm_code = NA_character_, item = NA_character_,
             detail = sprintf(paste0("k=3 min_obs=8 log-space | products capped=%d ",
                                     "untouched=%d | observations capped=%d"),
                              n_prod_capped, n_prod_untouch, n_wins_obs)),
  if (length(unmatched_iso3))
    data.table(category = "unmatched_exporter_iso3",
               comm_code = NA_character_, item = unmatched_iso3,
               detail = "exporter has no regions_full area_code; price rows dropped"),
  if (length(unmatched_products))
    data.table(category = "unmatched_product",
               comm_code = NA_character_, item = unmatched_products,
               detail = "in prices_bcp, no in-scope comm_code"),
  if (nrow(priceless))
    data.table(category = "priceless_in_scope_commodity",
               comm_code = priceless$comm_code, item = priceless$item,
               detail = "no producer price available"),
  if (nrow(biofuel_no_fac))
    data.table(category = "biofuel_no_density_factor",
               comm_code = biofuel_no_fac$comm_code, item = biofuel_no_fac$item,
               detail = "priced but no conversion_rate row; total_value NA"),
  data.table(category = "modelling_choice",
             comm_code = "c146", item = "Biogasoline",
             detail = paste0("Per-country production-share blend of Bioethanol & ETBE ",
                             "prices onto c146 (shares: 07_06 biogasoline_blend_shares)")),
  data.table(category = "blend_coverage",
             comm_code = "c146", item = "Biogasoline",
             detail = sprintf("%d priced c146 country-years; %d with ETBE>0; mean ETBE weight %.1f%%",
                              n_bio, n_bio_etbe, 100 * mean_etbe_w)),
  if (n_renorm > 0)
    data.table(category = "blend_renormalised",
               comm_code = "c146", item = "Biogasoline",
               detail = sprintf(paste0("%d country-years had ETBE share>0 but no ETBE ",
                                       "price; renormalised to Bioethanol"), n_renorm))
), use.names = TRUE, fill = TRUE)


# --- output ------------------------------------------------------------------
out <- X_long[, .(iso3c, area_code, comm_code, item, year,
                  total_product_output, unit, price, price_source,
                  total_value, total_value_source)]


# =============================================================================
# Plain-tonne commodities c141-c145 (Triticale, Molasses, Castor oil seeds,
# Oil of castor beans, chemically modified fats/oils). These are priced in
# USD/tonne straight from the FABIO v2 price outputs and carry no biofuel
# volume/blend machinery, so they are valued in a self-contained block scoped
# to their five comm_codes and appended to `out` with the same schema.
#
# Price per (area_code, comm_code, year) is filled on a three-rung ladder,
# FAO producer price always preferred over trade price:
#   1. fao_producer                -- FAO producer price for the own bcp area.
#   2. fao_producer_global_median  -- FAO producer global-median row (area 5000)
#                                     for that item/year, when the country has
#                                     no own FAO price.
#   3. trade_<src>                 -- bilateral trade price (area x item x year);
#                                     the catch-all, and the only source for the
#                                     Molasses / castor-oil / modified-fats items
#                                     (item_codes 165 / 266 / 1274), which FAOSTAT
#                                     never prices. The trade file's own
#                                     price_source is carried through, prefixed
#                                     "trade_".
# total_value = total_product_output * price (tonnes x USD/tonne).
# =============================================================================
AG_COMM_CODES          <- c("c141", "c142", "c143", "c144", "c145", "c171")
FAO_GLOBAL_MEDIAN_AREA  <- 5000L   # FAO producer-price global-median area row

# comm_code <-> item_code from the bcp item master already read above.
ag_map <- unique(items[comm_code %in% AG_COMM_CODES, .(comm_code, item_code = as.integer(item_code))])

# FABIO v2 price outputs. Paths follow the v2 repo's 00_value_added_config.R
# resolution: FABIO_ROOT (default ~/fabio, with FABIO_DATA_ROOT honoured as a
# fallback) and the price handoffs under data/total_value; VA_PRICE_OUTPUT_DIR
# overrides that directory. Empty-string env values are treated as unset.
va_env <- function(var, default) {
  v <- Sys.getenv(var, unset = "")
  if (!nzchar(v)) v <- default
  path.expand(v)
}
fabio_v2_root     <- va_env("FABIO_ROOT", Sys.getenv("FABIO_DATA_ROOT", unset = "~/fabio"))
va_price_dir      <- va_env("VA_PRICE_OUTPUT_DIR", file.path(fabio_v2_root, "data", "total_value"))
fao_prices_path   <- file.path(va_price_dir, "Prices_E_All_Data_with_USD.csv")
trade_prices_path <- file.path(va_price_dir, "bilateral_trade_prices.rds")

# Total product output for the five commodities, pulled straight from X (bypasses
# the IN_SCOPE_GROUPS-filtered, biofuel-mutated X_long entirely).
X_ag <- melt(as.data.table(X, keep.rownames = "row_id"),
             id.vars        = "row_id",
             variable.name   = "year",
             value.name      = "total_product_output",
             variable.factor = FALSE)
X_ag <- merge(X_ag, io_labels[, .(row_id, iso3c, area_code, comm_code, item, unit)],
              by = "row_id", all.x = TRUE)
X_ag <- X_ag[comm_code %in% AG_COMM_CODES & year %in% as.character(years)]
X_ag[ag_map, item_code := i.item_code, on = "comm_code"]
X_ag[, area_code := as.integer(area_code)]

# --- rung 1 & 2: FAO producer prices (mirror 13_3's load recipe) --------------
fao_raw <- fread(fao_prices_path)
setnames(fao_raw, old = c("Area Code", "Item Code"), new = c("area_code", "item_code"))
fao_yr_cols <- grep("^[A-Z][0-9]{4}$", names(fao_raw), value = TRUE)   # "Y2010"
setnames(fao_raw, old = fao_yr_cols, new = sub("^[A-Z]", "", fao_yr_cols))
fao_price_yr <- grep("^[0-9]{4}$", names(fao_raw), value = TRUE)
fao_long <- melt(fao_raw[Unit == "USD"],
                 id.vars        = setdiff(names(fao_raw), fao_price_yr),
                 measure.vars   = fao_price_yr,
                 variable.name   = "year",
                 value.name      = "price",
                 variable.factor = FALSE)[, .(area_code = as.integer(area_code),
                                              item_code = as.integer(item_code),
                                              year, price)]
fao_long <- fao_long[!is.na(price) & item_code %in% ag_map$item_code]

# Only item_codes 97 (Triticale) and 265 (Castor oil seeds) are ever priced by
# FAOSTAT; 165 / 266 / 1274 never appear and fall through to trade.
fao_gm <- fao_long[area_code == FAO_GLOBAL_MEDIAN_AREA,
                   .(price = median(price)), by = .(item_code, year)]

# FAO area_code -> FABIO (== bcp) area_code via the producer-price concordance.
conc_areas <- fread("inst/value_added/concordance_areas_fao_producer_prices_fabio.csv",
                    encoding = "UTF-8")
if ("comments; second area" %in% names(conc_areas)) conc_areas[, `comments; second area` := NULL]
conc_areas[, `:=`(FAO_area_code = as.integer(FAO_area_code),
                  FABIO_area_code = as.integer(FABIO_area_code))]
fao_own <- merge(fao_long[area_code != FAO_GLOBAL_MEDIAN_AREA], conc_areas,
                 by.x = "area_code", by.y = "FAO_area_code")
fao_own <- fao_own[, .(price = median(price)),
                   by = .(area_code = FABIO_area_code, item_code, year)]

# --- rung 3: bilateral trade prices (catch-all) ------------------------------
trade <- as.data.table(readRDS(trade_prices_path))
trade <- trade[, .(area_code = as.integer(area_code), item_code = as.integer(item_code),
                   year = as.character(year), price, price_source)]

# --- fallback ladder (FAO own -> FAO global median -> trade) ------------------
X_ag[, `:=`(price = NA_real_, price_source = NA_character_)]
X_ag[fao_own, `:=`(price = i.price, price_source = "fao_producer"),
     on = .(area_code, item_code, year)]
X_ag[fao_gm, `:=`(
  price        = fifelse(is.na(price), i.price, price),
  price_source = fifelse(is.na(price), "fao_producer_global_median", price_source)),
  on = .(item_code, year)]
X_ag[trade, `:=`(
  price        = fifelse(is.na(price) & is.finite(i.price), i.price, price),
  price_source = fifelse(is.na(price) & is.finite(i.price),
                         paste0("trade_", i.price_source), price_source)),
  on = .(area_code, item_code, year)]

X_ag[, total_value := fifelse(is.finite(price) & is.finite(total_product_output),
                              total_product_output * price, NA_real_)]
X_ag[, total_value_source := fifelse(is.finite(total_value), "output_x_price", NA_character_)]

# --- coverage diagnostic (ladder rung mix + any commodity left unpriced) ------
cat("\nProducer value for plain-tonne commodities c141-c145 (USD/tonne)\n")
ag_cov <- X_ag[, .(cells  = .N,
                   priced = sum(is.finite(price)),
                   valued = sum(is.finite(total_value))), by = .(comm_code, item)]
setorder(ag_cov, comm_code)
print(ag_cov)
ag_rung <- X_ag[is.finite(price), .(cells = .N), by = .(rung = fcase(
  price_source == "fao_producer",               "fao_producer",
  price_source == "fao_producer_global_median", "fao_producer_global_median",
  default                                       = "trade"))][order(-cells)]
cat("  Cells priced by ladder rung:\n")
print(ag_rung)
ag_unpriced <- X_ag[, .(unpriced = sum(!is.finite(price))), by = comm_code][unpriced > 0]
if (nrow(ag_unpriced))
  cat("  Commodities with unpriced cells: ",
      paste(sprintf("%s(%d)", ag_unpriced$comm_code, ag_unpriced$unpriced), collapse = ", "),
      "\n", sep = "")

out_ag <- X_ag[, .(iso3c, area_code, comm_code, item, year,
                   total_product_output, unit, price, price_source,
                   total_value, total_value_source)]
out <- rbind(out, out_ag, use.names = TRUE)

setorder(out, iso3c, comm_code, year)

dir.create("intermediate_data", showWarnings = FALSE)
diag_dir <- "intermediate_data/value_added_diagnostics"
dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
rds_path  <- tag("intermediate_data/bcp_producer_total_values.rds")
csv_path  <- sub("\\.csv$", paste0(mode_tag, ".csv"), "intermediate_data/bcp_producer_total_values.csv")
diag_path <- file.path(diag_dir, paste0("bcp_producer_total_values_diagnostics", mode_tag, ".csv"))
wins_path <- file.path(diag_dir, paste0("bcp_price_winsor_entries", mode_tag, ".csv"))

saveRDS(out, rds_path)
fwrite(out, csv_path)
fwrite(diagnostics, diag_path)
fwrite(winsor_entries, wins_path)

message(sprintf("30_bcp_total_producer_value: %d rows, %d priced, %d valued -> %s",
                nrow(out), out[is.finite(price), .N], out[is.finite(total_value), .N],
                rds_path))