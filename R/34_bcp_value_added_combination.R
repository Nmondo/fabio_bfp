# =============================================================================
# 34_bcp_value_added_combination.R
# Bio-based-commodity value added -- COMBINED (one synthesised estimate per
# base), overlaying the direct / measured value-added sources on top of the
# MRIO-intensity base.  Analogous to 14_4_value_added_FABIO_v2_synthesis.R in
# the FABIOv2 value-added repository: a base is kept, and higher-authority
# sources overwrite the (iso3c, comm_code, year) cells they cover.
#
# SOURCES combined here (BioSAMs -- script 35 -- are NOT combined: they overlap
# the model window in a single useful year and are reserved for validation):
#
#   BASE     31_bcp_value_added_MRIOTs.R   MRIO chemical-sector VA intensity x
#            (bcp_value_added_MRIOTs.rds)   producer value.  Two independent base
#                                           variants -- GLORIA and EXIOBASE --
#                                           carried side by side in that one file
#                                           as `value_added_gloria [USD]` and
#                                           `value_added_exiobase [USD]`.  The
#                                           synthesis therefore runs ONCE PER
#                                           BASE (see BASES below), producing a
#                                           parallel COMBINED file per base, just
#                                           as 14_4 does for its GLORIA/EXIOBASE
#                                           bases.
#
#   OVERWRITE overlays (a direct national/company measurement beats the MRIO
#   proxy for the cells it covers):
#     32_bcp_value_added_brazil_sut.R  Brazil IBGE SUT, direct VA for BRA c146 /
#            (bcp_value_added_brazil_sut.rds)  c147 (`value_added_sut [USD]`).
#     US RFA/ABF reports (hard-coded below)  Direct value added (the "Direct"
#            GDP-contribution line of the annual Renewable Fuels Association /
#            ABF Economics (Urbanchuk) US ethanol economic-contribution reports)
#            for USA Biogasoline (c146), 2014-2022, in nominal USD millions.
#            One report per year; see US_REPORTS_VA below for the per-year source
#            file.  These are national totals already, so no FX / no scaling
#            beyond millions -> USD.  NOTE: from the 2014 report these figures
#            EMBED the co-product (DDGS + corn distillers oil) value added and
#            cannot be reported net of it -- see the US_REPORTS_VA block below.
#
#   FILL overlay (the base has NO row for these commodities -- they carry no
#   producer price in script 30, so script 31 never values them -- so this is
#   the sole source and is UNIONED in, not overwritten):
#     33_bcp_value_added_neste.R       Neste segment GVA intensity x physical
#            (bcp_value_added_neste.rds)   output for the renewable-diesel bundle
#                                          c149 / c150 / c151 (`value_added_usd`).
#
# PRIORITY (per iso3c x comm_code x year):
#     direct national/company measurement  >  Neste fill  >  MRIO base
#   The two overwrite overlays never collide (Brazil = BRA c146/c147, US = USA
#   c146) and the Neste fill only touches commodities absent from the base, so
#   the ladder is unambiguous; it is still applied explicitly so any future
#   overlap resolves deterministically and is surfaced in the diagnostics.
#
# Currency: every source is already USD, so `value_added_usd` is USD throughout
# with no conversion here.
#
# Outputs (per base; <base> = "gloria" / "exiobase", <tag> = run-mode suffix):
#   intermediate_data/bcp_value_added_combined_<base><tag>.rds / .csv
#     keys iso3c, area_code, comm_code, item, year; the chosen `value_added_usd`
#     and its three components `value_added_{wages,capital,tls}_usd`; the
#     `va_source`; the base MRIO value (`base_value_added_usd`, pre-overwrite) and
#     each overlay's raw contribution (total + three components), so the row is
#     self-documenting and the later validation can compare.  Components sum to
#     the total in every valued cell; a US overlay supplying only wages has its
#     capital/tls apportioned from the residual by the base's capital:tls ratio.
#   intermediate_data/value_added_diagnostics/
#     bcp_value_added_combined_<base>_source_mix<tag>.csv   rows & VA sum by source
#     bcp_value_added_combined_<base>_overwrite_audit<tag>.csv  base-vs-overlay,
#       one row per cell an overwrite/fill was evaluated against, with the base
#       value, the overlay value, the applied source, and the relative gap.
# =============================================================================

## --- portable repo root: FABIO_BFP_ROOT override, else walk up to the marker ---
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

source("R/00_system_variables.R")   # years
source("R/00_run_config.R")          # tag() / mode_tag

keep_years <- as.integer(years)

# VA components carried through the ladder, in a fixed order.
VA_COMPONENTS <- c("wages", "capital", "tls")


# --- configuration -----------------------------------------------------------

# Source files (tagged so a bypass run reads its own bypass inputs).
MRIOT_RDS  <- tag("intermediate_data/bcp_value_added_MRIOTs.rds")
BRAZIL_RDS <- tag("intermediate_data/bcp_value_added_brazil_sut.rds")
NESTE_RDS  <- tag("intermediate_data/bcp_value_added_neste.rds")

# The two MRIO base variants carried in the single script-31 file.  One full
# synthesis pass per entry -> one COMBINED file per base (mirrors 14_4's BASES).
#   base_col   backtick-quoted VA column in bcp_value_added_MRIOTs.rds
#   label      value written to va_source on kept (un-overwritten) base rows
#   file_key   lower-case token baked into the output file names
BASES <- list(
  gloria   = list(base_col = "value_added_gloria [USD]",
                  label = "MRIO: GLORIA",   file_key = "gloria"),
  exiobase = list(base_col = "value_added_exiobase [USD]",
                  label = "MRIO: EXIOBASE", file_key = "exiobase")
)

# va_source labels for the overlays.
SRC_BRAZIL <- "National SUT: Brazil (IBGE)"
SRC_US     <- "National reports: US (RFA/ABF)"
SRC_NESTE  <- "Company GVA: Neste (renewable-diesel bundle)"

# US Biogasoline (c146) direct value added, nominal USD millions, one figure per
# year from the annual RFA / ABF Economics (Urbanchuk) US ethanol economic-
# contribution reports.  The figure taken is the "Ethanol Production -> Direct"
# GDP cell of each report's Table 2 (the direct value added of the ethanol-
# production segment itself: NOT the agriculture / construction / R&D / exports
# segments, and NOT the indirect or induced multiplier rounds).
#
# 2012 and 2013 are DELIBERATELY OMITTED (removed outright, not toggled off).
# Those two reports book the "Ethanol Production -> Direct" GDP cell on a far
# narrower basis than 2014+ (2012 = 783, essentially direct labour income only;
# 2013 = 2,185), because the ethanol operating margin and the co-product value
# had NOT yet been re-booked into the Ethanol Production segment.  Splicing them
# onto the 2014+ series produced an artificial ~7x discontinuity at 2013/2014
# (Ethanol Production -> Direct 2,185 -> 15,357 with direct jobs flat ~13k ->
# ~10k, and Agriculture -> Direct collapsing 1,445 -> 21), so they are NOT
# comparable and are not carried here.  For 2012-2013 the USA c146 cell simply
# falls through to the MRIO base (script 31), like any other uncovered cell.
#
# ---------------------------------------------------------------------------
# CO-PRODUCT (BYPRODUCT) VALUE ADDED IS INCLUDED IN THESE FIGURES -- IMPORTANT.
# ---------------------------------------------------------------------------
# From the 2014 report onward ABF injects the FULL market value of the ethanol
# co-products -- distillers dried grains (DDGS) and (industrial) corn distillers
# oil (DCO/CDO), plus corn gluten meal & feed from the 2022 report -- straight
# into the Ethanol Production value-added column, "treated as income and value
# added only" (their backward linkages are already captured in the ethanol
# production expenditures).  The "Ethanol Production -> Direct" GDP cell used
# below is therefore a SINGLE NET number equal to
#     (thin / volatile ethanol conversion margin) + (co-product value added).
#
# The reports disclose only the co-product GROSS market value (see
# `coproduct_gross_musd` below), NOT a separable co-product value-added line,
# and in every year except 2014 that gross value EXCEEDS the entire Direct cell
# (e.g. 2022: 12,600 gross co-product vs 2,383 total Direct).  The co-product
# contribution therefore CANNOT be netted out arithmetically from these reports
# -- subtracting the gross value yields negative "ethanol-only" VA in 2015-2022.
#
# CONSEQUENCE for this model, which carries the co-products as their OWN
# commodities (DDGS -> oil cakes / fodder crops; corn oil -> the oils commodity,
# each valued elsewhere in the pipeline): this US overlay DOUBLE-COUNTS those
# co-products against USA c146 (Biogasoline) across all of 2014-2022, and the
# double-count dominates the figure in lean-margin years.  It is retained here
# as the only direct national measurement available for the US, but if
# ethanol-CONVERSION-ONLY value added is required, prefer the MRIO base
# (script 31), which values the ethanol commodity on its own output without the
# co-product bundle.
US_REPORTS_VA <- data.table(
  year = 2014:2022,
  value_added_musd = c(15357, 7382, 6432, 4325, 5021, 3709, 3055, 5023, 2383),
  # Labor income (the ethanol-production "Direct" row's INCOME cell of each
  # report's Table 2), nominal USD millions, aligned to value_added_musd (Direct
  # GDP) above.  Used as the wages component; the residual (Direct GDP − wages)
  # is apportioned into capital/tls by the base's capital:tls ratio (R/31).
  # IMPLAN "labor income" includes proprietor income, so this is a
  # wages+mixed-income proxy.
  wages_income_musd = c(4489, 2536, 2386, 1842, 2199,
                        1862, 1321, 2357, 1017),
  source_report = c(
    "Contribution of Ethanol Industry to Economy of US in 2014 (Urbanchuk, 2015)",
    "Ethanol Economic Impact for 2015 (RFA/ABF)",
    "Ethanol Economic Impact for 2016 (RFA/ABF)",
    "RFA 2017 Ethanol Economic Impact",
    "RFA 2018 Ethanol Economic Impact",
    "2019 Ethanol Economic Contribution Report (RFA/ABF)",
    "Ethanol Industry Contribution to the US Economy in 2020",
    "RFA 2021 Economic Impact Report",
    "Ethanol Industry Contribution to the US Economy in 2022"),
  # Co-product GROSS market value ($m), booked by ABF as direct value added and
  # thus embedded in `value_added_musd` above.  DOCUMENTATION ONLY -- this is a
  # gross-output figure, not a deductible VA line, and exceeds value_added_musd
  # in every year but 2014 (see the note above).  DDGS + DCO, plus corn gluten
  # meal & feed from 2022.
  coproduct_gross_musd = c(8800, 8000, 7200, 6000, 8300, 7500, 6100, 9400, 12600))

US_ISO3C <- "USA"
US_COMM  <- "c146"
US_ITEM  <- "Biogasoline"

# Keys the overlays are matched on.  area_code is a function of iso3c (one FABIO
# area per country) and item a function of comm_code, so (iso3c, comm_code,
# year) is the natural unique key in every table; area_code / item are carried
# from the base (or from Neste, for the fill rows) rather than matched on.
JOIN_KEYS <- c("iso3c", "comm_code", "year")

DIAG_ZERO_TOL <- 1  # USD; below this a base value is treated as ~0 for rel-diff


# --- load & normalise the sources -------------------------------------------

need_file <- function(path, producer) {
  if (!file.exists(path))
    stop("Missing input '", path, "'.  Run ", producer, " first.")
  as.data.table(readRDS(path))
}

mriot  <- need_file(MRIOT_RDS,  "31_bcp_value_added_MRIOTs.R")
brazil <- need_file(BRAZIL_RDS, "32_bcp_value_added_brazil_sut.R")
neste  <- need_file(NESTE_RDS,  "33_bcp_value_added_neste.R")

for (b in BASES) {
  need_cols <- c(b$base_col,
                 sprintf("value_added_%s_%s [USD]", VA_COMPONENTS, b$file_key),
                 sprintf("va_intensity_%s_%s",      VA_COMPONENTS, b$file_key))
  miss <- setdiff(need_cols, names(mriot))
  if (length(miss))
    stop("bcp_value_added_MRIOTs.rds is missing column(s): ",
         paste(miss, collapse = ", "), " -- re-run 31_bcp_value_added_MRIOTs.R.")
}

mriot[, year := as.integer(year)]

# -- Brazil overlay -> common overlay schema (total + the three components).
if (!"value_added_sut [USD]" %in% names(brazil))
  stop("bcp_value_added_brazil_sut.rds has no `value_added_sut [USD]` column.")
for (cc in sprintf("value_added_sut_%s [USD]", VA_COMPONENTS))
  if (!cc %in% names(brazil))
    stop("bcp_value_added_brazil_sut.rds has no `", cc, "` column -- re-run 32.")
brazil_ov <- brazil[, .(iso3c, comm_code, item, year = as.integer(year),
                        overlay_value   = `value_added_sut [USD]`,
                        overlay_wages   = `value_added_sut_wages [USD]`,
                        overlay_capital = `value_added_sut_capital [USD]`,
                        overlay_tls     = `value_added_sut_tls [USD]`,
                        overlay_source = SRC_BRAZIL, priority = 1L)]
brazil_ov <- brazil_ov[is.finite(overlay_value)]

# -- US overlay: millions -> USD, tag as USA c146.  Wages (labor income) is
#    known; capital/tls are left NA and apportioned per base from the residual.
us_ov <- US_REPORTS_VA[year %in% keep_years,
                       .(iso3c = US_ISO3C, comm_code = US_COMM, item = US_ITEM,
                         year = as.integer(year),
                         overlay_value   = value_added_musd * 1e6,
                         overlay_wages   = wages_income_musd * 1e6,
                         overlay_capital = NA_real_,
                         overlay_tls     = NA_real_,
                         overlay_source = SRC_US, priority = 1L)]

# -- The two OVERWRITE overlays, stacked and de-duplicated by an explicit
#    priority (lower wins).  Both are direct national measurements (priority 1)
#    and their key sets are disjoint (Brazil = BRA c146/c147, US = USA c146), so
#    nothing is actually dropped; the ladder is applied explicitly so any future
#    overlap resolves deterministically rather than by sort order, and is
#    reported below.
overwrite_ov <- rbindlist(list(us_ov, brazil_ov), use.names = TRUE)
setorderv(overwrite_ov, c(JOIN_KEYS, "priority"))
n_ow_raw <- nrow(overwrite_ov)
overwrite_ov <- unique(overwrite_ov, by = JOIN_KEYS)  # keeps lowest priority per key
overwrite_ov[, priority := NULL]
if (nrow(overwrite_ov) != n_ow_raw)
  message(sprintf("  note: %d overlapping direct-overlay cell(s) resolved by priority.",
                  n_ow_raw - nrow(overwrite_ov)))

# -- Neste FILL rows (renewable-diesel bundle; absent from the MRIO base).  R/33
#    already carries all three components (tls a genuine 0), so no fallback.
if (!"value_added_usd" %in% names(neste))
  stop("bcp_value_added_neste.rds has no `value_added_usd` column.")
for (cc in sprintf("value_added_%s_usd", VA_COMPONENTS))
  if (!cc %in% names(neste))
    stop("bcp_value_added_neste.rds has no `", cc, "` column -- re-run 33.")
neste_fill <- neste[is.finite(value_added_usd) &
                      year %in% keep_years,
                    .(iso3c, area_code, comm_code, item, year = as.integer(year),
                      value_added_usd,
                      value_added_wages_usd, value_added_capital_usd, value_added_tls_usd,
                      va_source = SRC_NESTE,
                      total_product_output = output, unit)]


# --- combine, once per base --------------------------------------------------

dir.create("intermediate_data", showWarnings = FALSE, recursive = TRUE)
DIAG_DIR <- "intermediate_data/value_added_diagnostics"
dir.create(DIAG_DIR, showWarnings = FALSE, recursive = TRUE)

# Component value + intensity column names, per fixed VA_COMPONENTS order.
COMP_VAL_COLS <- sprintf("value_added_%s_usd", VA_COMPONENTS)
COMP_INT_COLS <- sprintf("base_int_%s",        VA_COMPONENTS)

# APPORTIONMENT FALLBACK: for any row with a finite total but only a SUBSET of
# components known, distribute residual = total − sum(known) across the MISSING
# components by that base's per-component intensity ratio (same cell), so the
# three always sum to the total.  When the base ratio is unavailable (missing or
# ~0 denominator), the residual is placed on the first missing component.
apportion_missing <- function(dt) {
  tot  <- dt$value_added_usd
  vals <- as.matrix(dt[, ..COMP_VAL_COLS])
  ints <- as.matrix(dt[, ..COMP_INT_COLS])
  need <- which(is.finite(tot) & rowSums(is.na(vals)) > 0L)
  for (i in need) {
    miss  <- which(is.na(vals[i, ]))
    known <- which(!is.na(vals[i, ]))
    residual <- tot[i] - (if (length(known)) sum(vals[i, known]) else 0)
    w     <- ints[i, miss]
    denom <- sum(w)
    if (is.finite(denom) && abs(denom) > .Machine$double.eps) {
      vals[i, miss] <- residual * w / denom
    } else {
      vals[i, miss]     <- 0
      vals[i, miss[1L]] <- residual
    }
  }
  for (k in seq_along(COMP_VAL_COLS)) set(dt, j = COMP_VAL_COLS[k], value = vals[, k])
  dt
}

combine_one_base <- function(base) {
  
  cat(sprintf("\n=== combining value added on the %s base ===\n", base$label))
  
  # 1. base table -> the common `value_added_usd` name; keep context, an untouched
  #    copy of the base total + components for the audit, and this base's per-
  #    component intensities for the apportionment fallback.
  base_val_cols <- sprintf("value_added_%s_%s [USD]", VA_COMPONENTS, base$file_key)
  base_int_cols <- sprintf("va_intensity_%s_%s",      VA_COMPONENTS, base$file_key)
  base_dt <- mriot[, .(iso3c, area_code, comm_code, item, year,
                       total_product_output, unit, total_value,
                       value_added_usd         = get(base$base_col),
                       value_added_wages_usd   = get(base_val_cols[1]),
                       value_added_capital_usd = get(base_val_cols[2]),
                       value_added_tls_usd     = get(base_val_cols[3]),
                       base_value_added_usd    = get(base$base_col),
                       va_mriot_wages_usd      = get(base_val_cols[1]),
                       va_mriot_capital_usd    = get(base_val_cols[2]),
                       va_mriot_tls_usd        = get(base_val_cols[3]),
                       base_int_wages          = get(base_int_cols[1]),
                       base_int_capital        = get(base_int_cols[2]),
                       base_int_tls            = get(base_int_cols[3]),
                       va_source               = base$label)]
  n_base <- nrow(base_dt)
  
  # 2. OVERWRITE overlays: update total + components + source in place on the
  #    matched cells (US leaves capital/tls NA for the apportionment step),
  #    leaving area_code / item / context (from the base) untouched.
  base_dt[overwrite_ov,
          `:=`(value_added_usd         = i.overlay_value,
               value_added_wages_usd   = i.overlay_wages,
               value_added_capital_usd = i.overlay_capital,
               value_added_tls_usd     = i.overlay_tls,
               va_source               = i.overlay_source),
          on = JOIN_KEYS]
  n_overwritten <- base_dt[va_source %in% c(SRC_BRAZIL, SRC_US), .N]
  
  # Direct overlays that matched no base cell (would silently vanish) -> warn.
  matched <- unique(base_dt[va_source %in% c(SRC_BRAZIL, SRC_US), ..JOIN_KEYS])
  unmatched_ov <- overwrite_ov[!matched, on = JOIN_KEYS]
  if (nrow(unmatched_ov))
    message(sprintf("  WARNING: %d direct-overlay cell(s) matched no %s base row (dropped): %s",
                    nrow(unmatched_ov), base$label,
                    paste(unmatched_ov[, paste0(iso3c, "/", comm_code, "/", year)],
                          collapse = ", ")))
  
  # 3. Neste FILL: the renewable-diesel bundle (c149/c150/c151) carries no
  #    producer price in script 30, so script 31 never *values* it -- but script
  #    30 still emits an in-scope placeholder ROW per bundle key (it keeps every
  #    in-group row, priced or not: the bundle is comm_group "Biofuels"), and
  #    that row flows through the base with NA total_value / NA VA.  Neste is the
  #    sole VA source for these keys, so the un-valued placeholder base rows are
  #    dropped and the Neste rows unioned in their place.  Give the Neste rows the
  #    same columns as the base; total_value has no meaning here (no price).
  neste_rows <- copy(neste_fill)
  neste_rows[, `:=`(total_value = NA_real_, base_value_added_usd = NA_real_)]
  neste_keys <- unique(neste_rows[, ..JOIN_KEYS])
  
  # Overlap between the base and the Neste keys.  An un-valued placeholder is
  # expected and simply superseded; a *valued* base row on a Neste key is a
  # genuine conflict (the bundle somehow acquired a producer price upstream) and
  # is surfaced rather than silently discarded.
  base_on_neste <- base_dt[neste_keys, on = JOIN_KEYS, nomatch = 0L]
  valued_clash  <- base_on_neste[is.finite(value_added_usd)]
  if (nrow(valued_clash))
    stop("Neste fill keys carry a *valued* row in the ", base$label,
         " base -- the RD bundle was expected to be un-valued (no producer price). ",
         "Resolve the overlap explicitly before unioning.  Keys: ",
         paste(valued_clash[, paste0(iso3c, "/", comm_code, "/", year)], collapse = ", "))
  
  n_placeholder <- nrow(base_on_neste)
  if (n_placeholder) {
    base_dt <- base_dt[!neste_keys, on = JOIN_KEYS]   # drop un-valued placeholders
    message(sprintf(paste0("  note: superseded %d un-valued RD-bundle placeholder ",
                           "row(s) in the %s base with the Neste fill."),
                    n_placeholder, base$label))
  }
  
  combined <- rbindlist(list(base_dt, neste_rows), use.names = TRUE, fill = TRUE)
  
  # 3b. APPORTIONMENT FALLBACK: fill any component left NA under a finite total
  #     (the US overlay's capital/tls) from the residual, by this base's ratio.
  combined <- apportion_missing(combined)
  
  # Invariant: the three components sum to the total in every valued cell.
  inv <- combined[is.finite(value_added_usd)]
  if (nrow(inv) && inv[, max(abs(value_added_wages_usd + value_added_capital_usd +
                                 value_added_tls_usd - value_added_usd) /
                             pmax(1, abs(value_added_usd)))] > 1e-6)
    stop("34 [", base$label, "]: components do not sum to the total after apportionment.")
  
  # 4. Per-source raw columns (total + the three components), so the combined row
  #    is self-documenting.  va_mriot_* is always the base; the others carry the
  #    chosen value only on the rows that source supplied.
  combined[, `:=`(
    va_mriot_usd      = base_value_added_usd,
    va_brazil_sut_usd = NA_real_, va_us_reports_usd = NA_real_, va_neste_usd = NA_real_,
    va_brazil_sut_wages_usd = NA_real_, va_brazil_sut_capital_usd = NA_real_, va_brazil_sut_tls_usd = NA_real_,
    va_us_reports_wages_usd = NA_real_, va_us_reports_capital_usd = NA_real_, va_us_reports_tls_usd = NA_real_,
    va_neste_wages_usd = NA_real_, va_neste_capital_usd = NA_real_, va_neste_tls_usd = NA_real_)]
  combined[va_source == SRC_BRAZIL, `:=`(
    va_brazil_sut_usd = value_added_usd, va_brazil_sut_wages_usd = value_added_wages_usd,
    va_brazil_sut_capital_usd = value_added_capital_usd, va_brazil_sut_tls_usd = value_added_tls_usd)]
  combined[va_source == SRC_US, `:=`(
    va_us_reports_usd = value_added_usd, va_us_reports_wages_usd = value_added_wages_usd,
    va_us_reports_capital_usd = value_added_capital_usd, va_us_reports_tls_usd = value_added_tls_usd)]
  combined[va_source == SRC_NESTE, `:=`(
    va_neste_usd = value_added_usd, va_neste_wages_usd = value_added_wages_usd,
    va_neste_capital_usd = value_added_capital_usd, va_neste_tls_usd = value_added_tls_usd)]
  
  combined[, c("base_int_wages", "base_int_capital", "base_int_tls") := NULL]
  
  setcolorder(combined, c("iso3c", "area_code", "comm_code", "item", "year",
                          "value_added_usd",
                          "value_added_wages_usd", "value_added_capital_usd", "value_added_tls_usd",
                          "va_source", "base_value_added_usd",
                          "va_mriot_usd", "va_mriot_wages_usd", "va_mriot_capital_usd", "va_mriot_tls_usd",
                          "va_brazil_sut_usd", "va_brazil_sut_wages_usd", "va_brazil_sut_capital_usd", "va_brazil_sut_tls_usd",
                          "va_us_reports_usd", "va_us_reports_wages_usd", "va_us_reports_capital_usd", "va_us_reports_tls_usd",
                          "va_neste_usd", "va_neste_wages_usd", "va_neste_capital_usd", "va_neste_tls_usd",
                          "total_product_output", "unit", "total_value"))
  setorder(combined, iso3c, comm_code, year)
  
  # 5. write ------------------------------------------------------------------
  rds_path <- tag(sprintf("intermediate_data/bcp_value_added_combined_%s.rds", base$file_key))
  csv_path <- sub("\\.csv$", paste0(mode_tag, ".csv"),
                  sprintf("intermediate_data/bcp_value_added_combined_%s.csv", base$file_key))
  saveRDS(combined, rds_path)
  fwrite(combined, csv_path)
  
  # 6. diagnostics ------------------------------------------------------------
  # (a) source mix: rows and VA sum by provenance.
  mix <- combined[, .(rows = .N,
                      value_added_usd = sum(value_added_usd, na.rm = TRUE)),
                  by = va_source][order(-value_added_usd)]
  mix <- rbind(mix, data.table(va_source = "TOTAL", rows = combined[, .N],
                               value_added_usd = combined[, sum(value_added_usd, na.rm = TRUE)]))
  cat("  source mix (rows | VA USD):\n"); print(mix)
  fwrite(mix, file.path(DIAG_DIR,
                        sprintf("bcp_value_added_combined_%s_source_mix%s.csv", base$file_key, mode_tag)))
  
  # (b) overwrite audit: for every cell a direct overlay was evaluated against,
  #     the base value, the overlay value, which won, and the relative gap.
  ov_named <- overwrite_ov[, .(iso3c, comm_code, year, overlay_value, overlay_source)]
  base_kept <- mriot[, .(iso3c, comm_code, year,
                         base_value_added_usd = get(base$base_col))]
  audit <- merge(ov_named, base_kept, by = JOIN_KEYS, all.x = TRUE)
  audit[, applied_source := overlay_source]
  audit[, rel_diff_vs_base := fifelse(
    is.finite(base_value_added_usd) & abs(base_value_added_usd) > DIAG_ZERO_TOL,
    (overlay_value - base_value_added_usd) / base_value_added_usd, NA_real_)]
  setnames(audit, "overlay_value", "overlay_value_usd")
  setorder(audit, iso3c, comm_code, year)
  fwrite(audit, file.path(DIAG_DIR,
                          sprintf("bcp_value_added_combined_%s_overwrite_audit%s.csv", base$file_key, mode_tag)))
  if (nrow(audit)) {
    cat("  overwrite audit (overlay vs MRIO base):\n")
    print(audit[, .(iso3c, comm_code, year, applied_source,
                    overlay_value_usd = round(overlay_value_usd),
                    base_value_added_usd = round(base_value_added_usd),
                    rel_diff_vs_base = round(rel_diff_vs_base, 3))])
  }
  
  message(sprintf("34_bcp_value_added_combination [%s]: %d rows (%d overwritten, %d Neste fill) -> %s",
                  base$label, nrow(combined), n_overwritten, nrow(neste_rows), rds_path))
  invisible(combined)
}

for (b in BASES) combine_one_base(b)