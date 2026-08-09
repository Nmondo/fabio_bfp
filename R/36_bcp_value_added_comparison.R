# =============================================================================
# 36_bcp_value_added_comparison.R
# -----------------------------------------------------------------------------
# Validate the MRIO-intensity value added (VA) of the bio-based commodities
# against every independent reference we have.
#
#   1. BioSAM extraction   Read the JRC BioSAM Social Accounting Matrices and
#                          derive direct biofuel gross value added (GVA at basic
#                          prices) for the EU Member States, split into the same
#                          three primary-factor accounts the MRIO uses.
#   2. Canada report GVA   Scale the CFA/GlobalData per-tonne intensity by the
#                          model's own Canadian output to get an independent
#                          CAN x fuel x year GVA in USD.
#   3. Cells and metrics   Match every independent reference (BioSAM, Brazil SUT,
#                          US reports, Canada) against each MRIO base over a
#                          common grid of cells, at L3 (country x commodity x
#                          year, components summed) and L4 (x component), and
#                          score the agreement in dex.
#   4. Symlog scatters     One figure per resolution: the reference on x, the
#                          MRIO base on y, both symlog so the subsidy-driven
#                          negative taxes-less-subsidies stay on the plot.
#
# Value-added definition (shared by every section).  In a SAM an activity's VA is
# what its column pays the primary-factor rows; full GVA at basic prices =
# LABOUR + CAPITAL + TLS-A (taxes less subsidies on production).  TLS-C is a tax
# on products (never in an activity column) and is excluded.  Biofuel subsidies
# routinely make TLS-A -- and occasionally CAPITAL -- negative; those signs are
# genuine and are preserved throughout, which is why the subcomponent scatter in
# section 4 uses a signed (pseudo-log) scale rather than log-log.
#
# Fuel groups, per the modelling scope:
#   Biogasoline = Biogasoline (A_BIOG) + 2nd-gen biochemical pathway (A_ETH)
#   Biodiesel   = Biodiesel   (A_BIOD)
# The 2nd-gen thermal pathway (A_FTFUEL) is out of scope and ignored.
#
# Inputs :  input/value_added/BioSAMs/*BioSAMs*<year>.csv           (section 1)
#             JRC BioSAM, long format: Receiving Agent (row/income) gets Value
#             from Spending Agent (column/expenditure), in million EUR.
#           intermediate_data/bcp_value_added_MRIOTs.rds            (section 2)
#           intermediate_data/bcp_value_added_combined_{gloria,exiobase}.csv
#                                                                   (sections 3-4)
# Outputs:  intermediate_data/bcp_value_added_biosam.rds / .csv
#             one row per (country x year x fuel): the three GVA components and
#             the total in EUR mn, plus fx and value_added_usd.
#           intermediate_data/bcp_value_added_canada.rds / .csv
#           intermediate_data/value_added_diagnostics/
#             bcp_value_added_biosam_diagnostics.csv     (funnel, coverage, ...)
#             bcp_value_added_comparison_metrics.csv     tidy metrics, one row per
#               (level x measure x reference x MRIO base)
#             bcp_value_added_comparison_table.csv       the same numbers in the
#               write-up's table shape
#             bcp_value_added_comparison_cells.csv       the matched cells behind
#               both, so any dot in either figure is traceable
#           output/plot/bcp_value_added_comparison_L3.svg            (section 4)
#           output/plot/bcp_value_added_comparison_L4.svg            (section 4)
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

# All libraries up front so the script reads as one program, not three stitched
# together: data.table for the extraction, matching and metrics, ggplot2 for the
# two comparison figures in section 4.
library(data.table)
library(ggplot2)

source("R/00_run_config.R")   # tag() / mode_tag for output naming


# =============================================================================
# SECTION 1 -- BioSAM extraction: direct biofuel GVA for the EU Member States
# =============================================================================

# --- configuration -----------------------------------------------------------

# Where the BioSAM CSVs live and how the year is read off the filename.
BIOSAM_DIR      <- "input/value_added/BioSAMs"
BIOSAM_PATTERN  <- "BioSAMs.*([0-9]{4})\\.csv$"

# Primary-factor rows whose signed sum is gross value added at basic prices.
VA_FACTORS <- data.table(
  code      = c("LABOUR", "CAPITAL",  "TLS-A"),
  component = c("labour", "capital",  "tls_a"))

# Fuel groups -> the BioSAM activity columns they aggregate.
FUEL_MAP <- data.table(
  activity_code = c("A_BIOG",      "A_ETH",       "A_BIOD"),
  fuel          = c("Biogasoline", "Biogasoline", "Biodiesel"))

# ISO2 (BioSAM) -> ISO3. EU27-2020 is an aggregate and stays iso3c = NA.
ISO2_TO_ISO3 <- c(
  AT="AUT", BE="BEL", BG="BGR", CY="CYP", CZ="CZE", DE="DEU", DK="DNK",
  EE="EST", ES="ESP", FI="FIN", FR="FRA", GB="GBR", GR="GRC", HR="HRV",
  HU="HUN", IE="IRL", IT="ITA", LT="LTU", LU="LUX", LV="LVA", MT="MLT",
  NL="NLD", PL="POL", PT="PRT", RO="ROU", SE="SWE", SI="SVN", SK="SVK")
AGGREGATE_ISO2 <- "EU27-2020"

# ECB euro reference rate, annual period average, USD per EUR. Extend as needed;
# a year without a rate keeps EUR value added but leaves value_added_usd = NA.
FX_USD_PER_EUR <- c("2010" = 1.3257, "2015" = 1.1095)


# --- read the BioSAM files ---------------------------------------------------

files <- list.files(BIOSAM_DIR, pattern = BIOSAM_PATTERN, full.names = TRUE)
if (!length(files))
  stop("No BioSAM CSVs matching '", BIOSAM_PATTERN, "' in '", BIOSAM_DIR, "'.")

read_biosam <- function(path) {
  yr <- as.integer(sub(".*?([0-9]{4})\\.csv$", "\\1", basename(path)))
  d  <- fread(path, showProgress = FALSE)
  setnames(d,
           c("Country (ISO2)", "Country", "Receiving Agent (Code)",
             "Spending Agent (Code)", "Value (MILLION EUROS)"),
           c("iso2", "country", "row_code", "col_code", "value_eur_mn"))
  # Trust the filename year, but flag any disagreement with the Year column.
  if ("Year" %in% names(d) && any(d$Year != yr))
    warning(basename(path), ": filename year ", yr,
            " disagrees with the Year column.")
  d[, .(iso2, country, row_code, col_code, value_eur_mn, year = yr)]
}

sam <- rbindlist(lapply(files, read_biosam), use.names = TRUE)
n_raw <- nrow(sam)


# --- value added = factor rows x fuel-activity columns -----------------------

va <- sam[row_code %in% VA_FACTORS$code & col_code %in% FUEL_MAP$activity_code]
n_scope <- nrow(va)

va[FUEL_MAP,      fuel      := i.fuel,      on = c(col_code = "activity_code")]
va[VA_FACTORS,    component := i.component, on = c(row_code  = "code")]

# Signed sum of the three components -> long, one value per component present.
comp <- va[, .(value_eur_mn = sum(value_eur_mn)),
           by = .(iso2, country, year, fuel, component)]

# Wide: a column per GVA component, plus the signed total.
out <- dcast(comp, iso2 + country + year + fuel ~ component,
             value.var = "value_eur_mn", fill = 0)
for (cc in VA_FACTORS$component)                 # guarantee all three exist
  if (!cc %in% names(out)) out[, (cc) := 0]
out[, value_added_eur_mn := labour + capital + tls_a]


# --- geography, FX, and USD conversion ---------------------------------------

out[, is_aggregate := iso2 == AGGREGATE_ISO2]
out[, iso3c        := ISO2_TO_ISO3[iso2]]
unmapped_iso2 <- sort(unique(out[is.na(iso3c) & !is_aggregate, iso2]))

out[, fx_usd_per_eur := FX_USD_PER_EUR[as.character(year)]]
out[, value_added_usd := value_added_eur_mn * 1e6 * fx_usd_per_eur]
missing_fx_years <- sort(unique(out[is.na(fx_usd_per_eur), year]))

setcolorder(out, c("iso2", "iso3c", "country", "is_aggregate", "year", "fuel",
                   "labour", "capital", "tls_a", "value_added_eur_mn",
                   "fx_usd_per_eur", "value_added_usd"))
setnames(out, c("labour", "capital", "tls_a"),
         c("labour_eur_mn", "capital_eur_mn", "tls_a_eur_mn"))
setorder(out, year, fuel, iso2)


# --- diagnostics -------------------------------------------------------------

cat("\nBioSAM value-added extraction\n")
cat("  Files read:                    ", length(files),
    " (", paste(sort(unique(out$year)), collapse = ", "), ")\n", sep = "")
cat("  Raw SAM rows:                  ", n_raw,   "\n", sep = "")
cat("  Rows in scope (VA x fuel):     ", n_scope, "\n", sep = "")
cat("  Output rows (country x yr x fuel):", nrow(out), "\n", sep = "")
if (length(unmapped_iso2))
  cat("  Unmapped ISO2 (iso3c = NA):    ", paste(unmapped_iso2, collapse = ", "), "\n", sep = "")
if (length(missing_fx_years))
  cat("  Years without FX (USD = NA):   ", paste(missing_fx_years, collapse = ", "), "\n", sep = "")

cov <- out[is_aggregate == FALSE,
           .(countries = uniqueN(iso2)), by = .(year, fuel)]
setorder(cov, year, fuel)
cat("\nCountry coverage (excl. aggregate)\n"); print(cov)

neg <- out[value_added_eur_mn < 0]
cat("\nNegative GVA country-years (net subsidy): ", nrow(neg), "\n", sep = "")
if (nrow(neg)) print(neg[order(value_added_eur_mn),
                         .(iso2, year, fuel, value_added_eur_mn)])

diagnostics <- rbindlist(list(
  data.table(category = "input_funnel", iso2 = NA_character_, year = NA_integer_,
             fuel = NA_character_,
             detail = sprintf("files=%d raw_rows=%d in_scope=%d out_rows=%d",
                              length(files), n_raw, n_scope, nrow(out))),
  data.table(category = "value_added_definition", iso2 = NA_character_,
             year = NA_integer_, fuel = NA_character_,
             detail = "GVA = LABOUR + CAPITAL + TLS-A (basic prices); TLS-C excluded"),
  data.table(category = "scope", iso2 = NA_character_, year = NA_integer_,
             fuel = "Biogasoline", detail = "A_BIOG + A_ETH; A_FTFUEL excluded"),
  cov[, .(category = "country_coverage", iso2 = NA_character_, year, fuel,
          detail = sprintf("%d countries", countries))],
  if (length(unmapped_iso2))
    data.table(category = "unmapped_iso2", iso2 = unmapped_iso2,
               year = NA_integer_, fuel = NA_character_,
               detail = "no ISO2->ISO3 mapping; iso3c = NA"),
  if (length(missing_fx_years))
    data.table(category = "missing_fx", iso2 = NA_character_, year = missing_fx_years,
               fuel = NA_character_, detail = "no FX rate; value_added_usd = NA"),
  if (nrow(neg))
    neg[, .(category = "negative_gva", iso2, year, fuel,
            detail = sprintf("net subsidy, GVA = %.3f EUR mn", value_added_eur_mn))]
), use.names = TRUE, fill = TRUE)


# --- output ------------------------------------------------------------------

dir.create("intermediate_data", showWarnings = FALSE, recursive = TRUE)
diag_dir <- "intermediate_data/value_added_diagnostics"
dir.create(diag_dir, showWarnings = FALSE, recursive = TRUE)

rds_path  <- tag("intermediate_data/bcp_value_added_biosam.rds")
csv_path  <- sub("\\.csv$", paste0(mode_tag, ".csv"),
                 "intermediate_data/bcp_value_added_biosam.csv")
diag_path <- file.path(diag_dir, paste0("bcp_value_added_biosam_diagnostics", mode_tag, ".csv"))

saveRDS(out, rds_path)
fwrite(out, csv_path)
fwrite(diagnostics, diag_path)

message(sprintf("36 section 1 (BioSAM GVA): %d rows across %d year(s), %d fuel group(s) -> %s",
                nrow(out), uniqueN(out$year), uniqueN(out$fuel), rds_path))


# =============================================================================
# SECTION 2 -- Canada report GVA (an independent reference, keyed to CAN cells)
# -----------------------------------------------------------------------------
# Section 1 leaves the BioSAM table `out` in scope.  Here we build the second
# independent reference -- direct biofuel-processing GVA from the CFA/GlobalData
# Canada report -- so that both are ready when the comparison figures in
# sections 3-4 line every independent source up against the MRIO base.
# =============================================================================

# --- Canada report: direct biofuel-processing GVA ----------------------------
# CFA / GlobalData, "The Economic Impact of Biofuels on the Canadian Economy"
# (Apr 2025), Table 3, direct economic impact, 2021-2023 average.  A per-tonne
# intensity from the report is scaled by the model's own CAN output, so each row
# is one CAN x fuel x year GVA total in USD.  ml_per_kt are the model's volume
# <-> mass factors (07_01 / 07_02): ethanol 1.267, FAME 1.136.

CA_REPORT <- data.table(
  comm_code = c("c146",        "c147"),
  item      = c("Biogasoline", "Biodiesel"),
  va_cad_mn = c(419.6,          185.4),
  output_kt = c(1295,           405),
  ml_per_kt = c(1.267,          1.136))
CA_REPORT[, intensity_cad_t := va_cad_mn * 1e6 / (output_kt * 1e3)]

FX_USD_PER_CAD <- c("2021" = 0.7980, "2022" = 0.7686)   # annual avg, Bank of Canada
CA_YEARS       <- c(2021L, 2022L)

mriot  <- readRDS(tag("intermediate_data/bcp_value_added_MRIOTs.rds"))
canada <- mriot[iso3c == "CAN" & comm_code %in% CA_REPORT$comm_code & year %in% CA_YEARS,
                .(iso3c, area_code, comm_code, item, year = as.integer(year),
                  total_product_output, unit)]

canada[CA_REPORT, `:=`(intensity_cad_t = i.intensity_cad_t, ml_per_kt = i.ml_per_kt),
       on = "comm_code"]
canada[, output_t        := total_product_output / ml_per_kt]      # 1000 L -> tonnes
canada[, intensity_usd_t := intensity_cad_t * FX_USD_PER_CAD[as.character(year)]]
canada[, value_added_usd := intensity_usd_t * output_t]
canada[, source          := "Canada report (CFA)"]

saveRDS(canada, tag("intermediate_data/bcp_value_added_canada.rds"))
fwrite(canada, sub("\\.csv$", paste0(mode_tag, ".csv"),
                   "intermediate_data/bcp_value_added_canada.csv"))


# =============================================================================
# SECTION 3 -- Cells, matching and agreement metrics
# -----------------------------------------------------------------------------
# Every independent reference is scored against the MRIO base over a common grid
# of cells, a cell being the intersection of the classification dimensions the
# two tables share.  Two resolutions are used, following the FABIO-v2 value-added
# validation:
#     L3  country x commodity x year                  VA components summed
#     L4  country x commodity x year x component
# so the total answers "does the base reproduce the independent LEVEL?" and the
# components the sharper "does it reproduce the wages / capital / taxes-less-
# subsidies SPLIT?".  L1 / L2 (which sum the commodities away) are not scored:
# with two commodities in scope they would carry one cell per country-year and
# no sample size worth a metric.
#
# The reference is always the independent figure and the source always the MRIO
# base, so a log ratio above zero means the base OVER-states the reference.
#
# Matching is over the UNION of the two sides within the (country, commodity,
# year) footprint the reference populates, an absent row carried as a structural
# zero.  A component the base leaves empty where the reference books a value
# therefore registers as missing coverage rather than vanishing from the
# comparison; the reference is never asked about a country, a year or a
# commodity it does not cover -- every reference here is about the two biofuel
# commodities, not about the rest of the BCP set the combined file carries.
#
# The comparison is driven off three small declarations -- the base files, the
# references carried as columns of those files, and the references that live in
# their own tables -- so adding a source is a one-line edit, not a new branch.

# (1) MRIO base files, one facet each.  Read the mode-tagged CSV so a bypass run
#     compares its own combined outputs rather than the rescaled ones.
COMPARISON_BASES <- c(
  GLORIA   = "intermediate_data/bcp_value_added_combined_gloria.csv",
  EXIOBASE = "intermediate_data/bcp_value_added_combined_exiobase.csv")

FUEL2COMM <- c(Biogasoline = "c146", Biodiesel = "c147")
COMM_ITEM <- setNames(names(FUEL2COMM), FUEL2COMM)

# The scored measures: the three components and their sum.  Correspondence of
# the accounts across sources (identical economics, different vocabularies):
#     wages   <- MRIO "wages"    <- Brazil "Remuneracoes"    <- BioSAM LABOUR
#     capital <- MRIO "capital"  <- Brazil operating surplus <- BioSAM CAPITAL
#     tls     <- MRIO "tls"      <- Brazil taxes - subsidies <- BioSAM TLS-A
# tls is the production-side measure on every side, and is genuinely NEGATIVE
# for biofuels wherever subsidies exceed taxes.
VA_COMPONENTS <- c("wages", "capital", "tls")
MEASURES      <- c("total", VA_COMPONENTS)
VA_MEASURE    <- c(total = "value added", wages = "wages",
                   capital = "capital", tls = "taxes less subsidies")

# (2) Independent references carried as COLUMNS inside each combined file.
#     `components` marks those that resolve the same three accounts and are
#     therefore scored at L4 as well.  The US reports' component split is the
#     base's own ratio (34's apportionment fallback), so it is a total-only
#     reference; Neste fills the renewable-diesel bundle, has no base cell to
#     sit against, and is absent by construction.
INLINE_REFERENCES <- data.table(
  label      = c("Brazil SUT (IBGE)", "US reports (RFA/ABF)"),
  prefix     = c("va_brazil_sut",     "va_us_reports"),
  components = c(TRUE,                FALSE))

# (3) Independent references that live in their OWN tables, long over
#     (iso3c, comm_code, year, component, ref) and already in USD.  BioSAM
#     resolves the three accounts; the Canada report gives one number per fuel.
biosam_ref <- out[is_aggregate == FALSE & is.finite(fx_usd_per_eur),
                  .(iso3c, comm_code = FUEL2COMM[fuel], year,
                    total   = value_added_eur_mn * 1e6 * fx_usd_per_eur,
                    wages   = labour_eur_mn      * 1e6 * fx_usd_per_eur,
                    capital = capital_eur_mn     * 1e6 * fx_usd_per_eur,
                    tls     = tls_a_eur_mn       * 1e6 * fx_usd_per_eur)]
biosam_ref <- melt(biosam_ref, id.vars = c("iso3c", "comm_code", "year"),
                   variable.name = "component", value.name = "ref",
                   variable.factor = FALSE)

canada_ref <- canada[, .(iso3c, comm_code, year = as.integer(year),
                         component = "total", ref = value_added_usd)]

EXTERNAL_REFERENCES <- list("BioSAM (JRC) 2015"   = biosam_ref,
                            "Canada report (CFA)" = canada_ref)

# Fixed reference order + colourblind-safe palette (Dark2), stable across facets
# and independent of which cells happen to be present in a given run.
REFERENCE_LEVELS <- c(INLINE_REFERENCES$label, names(EXTERNAL_REFERENCES))
REFERENCE_COLORS <- setNames(
  c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")[seq_along(REFERENCE_LEVELS)],
  REFERENCE_LEVELS)

# Component colours, shared with the FABIO-v2 validation figures.
VA_COMPONENT_COLOURS <- c(wages = "#1f77b4", capital = "#d62728",
                          tls   = "#2ca02c", total   = "#4d4d4d")


# --- cells -------------------------------------------------------------------

#' The `<prefix>_usd` / `<prefix>_<component>_usd` columns of a combined file,
#' long over measure.  Non-finite cells are dropped here and come back as
#' structural zeros in the match, so an unpopulated cell reads as missing
#' coverage rather than as agreement.
va_long <- function(comb, prefix, components = TRUE) {
  cols <- c(total = paste0(prefix, "_usd"))
  if (components)
    cols <- c(cols, setNames(sprintf("%s_%s_usd", prefix, VA_COMPONENTS),
                             VA_COMPONENTS))
  miss <- setdiff(cols, names(comb))
  if (length(miss))
    stop("combined file is missing column(s): ", paste(miss, collapse = ", "),
         " -- re-run 34_bcp_value_added_combination.R.")
  d <- melt(comb[, c("iso3c", "comm_code", "year", unname(cols)), with = FALSE],
            id.vars = c("iso3c", "comm_code", "year"), variable.name = "column",
            value.name = "value", variable.factor = FALSE)
  d[, component := names(cols)[match(column, cols)]]
  d[is.finite(value), .(iso3c, comm_code, year, component, value)]
}

#' Pair one reference against the base cells.  The universe is the union of the
#' two sides over the components the reference resolves and within the
#' (country, commodity, year) footprint it populates; an absent row on either
#' side is a structural zero.
va_match <- function(src_cells, ref_cells) {
  keys  <- c("iso3c", "comm_code", "year", "component")
  scope <- unique(ref_cells[, .(iso3c, comm_code, year)])
  s     <- src_cells[component %in% unique(ref_cells$component)][
    scope, on = .(iso3c, comm_code, year), nomatch = 0L]
  univ  <- unique(rbindlist(list(ref_cells[, ..keys], s[, ..keys])))
  m     <- merge(merge(univ, ref_cells, by = keys, all.x = TRUE),
                 s, by = keys, all.x = TRUE)
  m[is.na(ref), ref := 0]
  m[is.na(src), src := 0]
  m[]
}

#' One tidy (base, reference, cell, ref, src) table per MRIO base.
comparison_cells <- function(base_name, csv_path) {
  # integer64 = "double": some value columns (e.g. va_us_reports_usd) are whole
  # numbers > 2^31, which fread would otherwise read as bit64::integer64. When
  # melt() later stacks such a column with a plain double column, the integer64
  # values are copied by their raw 64-bit pattern instead of value-converted,
  # turning ~1.5e10 into a denormalised ~1e-313. Forcing double on read avoids it.
  comb <- fread(sub("\\.csv$", paste0(mode_tag, ".csv"), csv_path),
                integer64 = "double")
  comb[, year := as.integer(year)]
  
  src  <- setnames(va_long(comb, "va_mriot"), "value", "src")
  refs <- c(
    setNames(lapply(seq_len(nrow(INLINE_REFERENCES)), function(i)
      setnames(va_long(comb, INLINE_REFERENCES$prefix[i],
                       INLINE_REFERENCES$components[i]), "value", "ref")),
      INLINE_REFERENCES$label),
    lapply(EXTERNAL_REFERENCES, function(d) d[is.finite(ref)]))
  
  # The reference is cut to the model window: a BioSAM year the pipeline does
  # not model is out of scope, not a coverage failure.
  yrs <- unique(src$year)
  rbindlist(Map(function(label, tbl) {
    tbl <- tbl[year %in% yrs]
    if (!nrow(tbl)) return(NULL)
    va_match(src, tbl)[, reference := label]
  }, names(refs), refs), use.names = TRUE)[, base := base_name][]
}

cells <- rbindlist(Map(comparison_cells, names(COMPARISON_BASES), COMPARISON_BASES))
cells[, `:=`(item      = COMM_ITEM[comm_code],
             level     = ifelse(component == "total", "L3", "L4"),
             base      = factor(base,      levels = names(COMPARISON_BASES)),
             reference = factor(reference, levels = REFERENCE_LEVELS),
             component = factor(component, levels = MEASURES))]
setcolorder(cells, c("base", "reference", "level", "iso3c", "comm_code", "item",
                     "year", "component", "ref", "src"))
setorder(cells, base, reference, component, iso3c, comm_code, year)


# --- agreement metrics -------------------------------------------------------
#
# All dispersion is in log10 units ("dex").  On the cells that are non-zero on
# both sides and of the same sign, with l = log10(|src| / |ref|):
#
#   med_ratio  = 10^median(l)                typical multiplicative departure
#   mad_fold   = 10^median(|l - median(l)|)  dispersion about that centre
#   rmsle_dex  = sqrt(mean(l^2))             uncentred, about zero -- the
#                                            identity, not the fitted centre
#
# reported alongside:
#
#   n          cells in the group -- the designed grid
#   n_pop      cells at least one side populates
#   n_used     cells surviving the non-zero + same-sign filter
#   coverage   share of POPULATED cells non-zero on both sides
#   sign_agree share of same-sign cells, among cells non-zero on both sides
#
# A cell zero on both sides is agreement on an empty cell, not a miss, so it is
# counted by n but by neither coverage nor sign_agree.  sign_agree conditions on
# the non-zero cells so that a structural zero reads as missing coverage rather
# than as a sign flip.

VA_MIN_USED <- 10L

va_metrics <- function(ref, src) {
  pop <- ref != 0 | src != 0
  nz  <- ref != 0 & src != 0
  use <- nz & sign(ref) == sign(src)
  l   <- log10(abs(src[use]) / abs(ref[use]))
  ok  <- length(l) >= VA_MIN_USED
  data.table(
    n          = length(ref),
    n_pop      = sum(pop),
    n_used     = length(l),
    coverage   = if (any(pop)) sum(nz) / sum(pop) else NA_real_,
    sign_agree = if (any(nz))  sum(use) / sum(nz) else NA_real_,
    med_ratio  = if (ok) 10^median(l) else NA_real_,
    mad_fold   = if (ok) 10^median(abs(l - median(l))) else NA_real_,
    rmsle_dex  = if (ok) sqrt(mean(l^2)) else NA_real_)
}

metrics <- cells[, va_metrics(ref, src), by = .(level, component, reference, base)]
setorder(metrics, component, reference, base)

fwrite(metrics, file.path(diag_dir,
                          paste0("bcp_value_added_comparison_metrics", mode_tag, ".csv")),
       na = "NA")

# The same numbers in the write-up's table shape: one row per (measure, metric),
# one column per (reference x MRIO base).
VA_METRIC_LABELS <- c(coverage   = "coverage",     sign_agree = "sign agreement",
                      med_ratio  = "median ratio", mad_fold   = "MAD fold",
                      rmsle_dex  = "RMSLE")

va_fmt <- function(x)
  ifelse(!is.finite(x), "\u2013",
         ifelse(abs(x) < 0.01, formatC(x, format = "g", digits = 2),
                formatC(x, format = "f", digits = 2)))

tbl <- melt(metrics, id.vars = c("level", "component", "reference", "base"),
            measure.vars = names(VA_METRIC_LABELS), variable.name = "metric",
            value.name = "value", variable.factor = FALSE)
row_levels <- sprintf("%s (%s)", VA_MEASURE[MEASURES],
                      ifelse(MEASURES == "total", "L3", "L4"))
tbl[, `:=`(measure = factor(sprintf("%s (%s)", VA_MEASURE[as.character(component)], level),
                            levels = row_levels),
           metric  = factor(VA_METRIC_LABELS[metric], levels = VA_METRIC_LABELS),
           column  = paste(reference, base, sep = " | "),
           value   = va_fmt(value))]
metrics_table <- dcast(tbl, measure + metric ~ column, value.var = "value",
                       fill = "\u2013")
setcolorder(metrics_table, c("measure", "metric",
                             intersect(as.vector(t(outer(REFERENCE_LEVELS,
                                                         names(COMPARISON_BASES),
                                                         paste, sep = " | "))),
                                       names(metrics_table))))
setorder(metrics_table, measure, metric)
fwrite(metrics_table, file.path(diag_dir,
                                paste0("bcp_value_added_comparison_table", mode_tag, ".csv")))

# The point-level frame behind both, so any dot in either figure can be traced
# back to its cell.
fwrite(cells, file.path(diag_dir,
                        paste0("bcp_value_added_comparison_cells", mode_tag, ".csv")),
       na = "NA")

cat("\nAgreement of the MRIO bases with the independent references\n")
print(metrics_table)


# =============================================================================
# SECTION 4 -- Symlog scatter panels
# -----------------------------------------------------------------------------
# One point per cell: the independent reference on x, the MRIO base on y, both
# on a symlog scale that is linear within a dollar of zero and logarithmic
# beyond, so the subsidy-driven negative taxes-less-subsidies stay on the plot
# and the linear core takes the same width as every decade outside it.  Axes are
# shared across the panels, so the dashed identity reads as a true 45 deg and the
# panels remain directly comparable.
#
# Nothing is encoded twice: at L3 the component is summed away and the reference
# takes the colour with the base as the panel; at L4 the component takes the
# colour, so the reference becomes the second panel dimension.
# =============================================================================

#' Where the linear core ends: below a dollar a cell is rounding, so nothing
#' under it needs resolving and the log region can start at 10^0.
VA_SYMLOG_LIN <- 1

#' Linear inside the core and logarithmic beyond, joined so that the core and
#' every decade outside it are one unit wide.
va_symlog <- function(x) {
  a <- abs(x) / VA_SYMLOG_LIN
  sign(x) * ifelse(a <= 1, a, 1 + log10(a))
}

#' Mantissa and exponent of `x` in scientific notation.  Rounding can push a
#' mantissa to 10 (9.999 -> 10.0), which is carried into the exponent.
va_sci_parts <- function(x, digits = 3) {
  ok <- is.finite(x) & x != 0
  e  <- ifelse(ok, floor(log10(abs(x))), 0)
  m  <- ifelse(ok, signif(x / 10^e, digits), x)
  up <- is.finite(m) & abs(m) >= 10
  e[up] <- e[up] + 1L
  m[up] <- m[up] / 10
  list(mantissa = m, exponent = as.integer(e))
}

#' Axis labels as plotmath, so the exponent sets as a true superscript: 0,
#' 10^9, 1.5 x 10^9.  A mantissa of 1 is left implicit, and NA breaks (ggplot
#' passes them for censored values) label as blank.
va_sci_expr <- function(x, digits = 3) {
  p   <- va_sci_parts(x, digits)
  txt <- ifelse(
    !is.finite(x), "''",
    ifelse(x == 0, "0",
           ifelse(abs(p$mantissa) == 1,
                  sprintf("%s10^%d", ifelse(p$mantissa < 0, "-", ""), p$exponent),
                  sprintf("%s %%*%% 10^%d", sprintf("%g", p$mantissa), p$exponent))))
  parse(text = txt)
}

#' Decade ticks either side of zero, from the linear core out to the largest
#' decade the data actually reach.  Every decade takes a gridline; on a crowded
#' axis the plotmath would collide, so only every second one is labelled.
va_symlog_axis <- function(v) {
  hi   <- max(abs(v[is.finite(v)]), VA_SYMLOG_LIN)
  lo   <- round(log10(VA_SYMLOG_LIN))
  dec  <- 10^(lo:max(lo, floor(log10(hi))))
  at   <- sort(unique(c(-rev(dec), 0, dec)))
  step <- abs(seq_along(at) - (length(at) + 1L) / 2L)
  lab  <- at
  if (max(step) > 8L) lab[step > 0L & step %% 2L == 0L] <- NA
  list(breaks = va_symlog(at), labels = va_sci_expr(lab))
}

SCALE_NOTE <- sprintf(
  paste0("Both axes symlog: linear within \u00b1US$%g and logarithmic beyond, so ",
         "the linear core and every decade outside it are equally wide and the ",
         "panels share a scale. Dashed line = identity; grey lines mark zero, so ",
         "sign disagreement reads off the off-diagonal quadrants. Taxes less ",
         "subsidies is the production-side measure on both sides."),
  VA_SYMLOG_LIN)

#' One symlog scatter.  `colour` names the aesthetic column and `facets` the
#' panel spec.
va_symlog_plot <- function(d, colour, colours, colour_name, facets,
                           title, subtitle, colour_labels = waiver()) {
  # Cells empty on both sides sit exactly on the origin and carry no
  # disagreement to read; they are not plotted.
  d <- copy(d[ref != 0 | src != 0])
  d[, `:=`(xt = va_symlog(ref), yt = va_symlog(src))]
  ax <- va_symlog_axis(c(d$ref, d$src))
  
  ggplot(d, aes(x = xt, y = yt, colour = .data[[colour]])) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey75") +
    geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey75") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                linewidth = 0.4, colour = "black") +
    geom_point(shape = 16, size = 2, alpha = 0.8) +
    facets +
    scale_x_continuous(breaks = ax$breaks, labels = ax$labels) +
    scale_y_continuous(breaks = ax$breaks, labels = ax$labels) +
    scale_colour_manual(values = colours, labels = colour_labels,
                        drop = FALSE, name = colour_name) +
    labs(title = title, subtitle = subtitle,
         x = "Independent reference, value added (current US$, symlog scale)",
         y = "FABIO-BCP value-added extension (current US$, symlog scale)") +
    theme_minimal(base_size = 10) +
    theme(
      aspect.ratio        = 1,
      panel.grid.minor    = element_blank(),
      panel.grid.major    = element_line(colour = "grey90", linewidth = 0.25),
      panel.spacing       = unit(1.2, "lines"),
      strip.text          = element_text(face = "bold", size = 9.5),
      strip.placement     = "outside",
      legend.position     = "bottom",
      legend.box          = "vertical",
      # Title + subtitle form a left-aligned top band, set off by whitespace so
      # they can be cropped off cleanly when placing the figure in publication.
      plot.title.position = "plot",
      plot.title          = element_text(face = "bold", size = 12,
                                         margin = margin(b = 4)),
      plot.subtitle       = element_text(size = 8.5, lineheight = 1.2,
                                         margin = margin(b = 16))) +
    guides(colour = guide_legend(override.aes = list(shape = 16, size = 2.8)))
}

out_plot_dir <- file.path("output", "plot")
dir.create(out_plot_dir, showWarnings = FALSE, recursive = TRUE)

p_l3 <- va_symlog_plot(
  cells[component == "total"],
  colour = "reference", colours = REFERENCE_COLORS, colour_name = "Independent reference",
  facets = facet_wrap(~ base, nrow = 1),
  title    = paste0("FABIO-BCP value added against the independent references ",
                    "at country x commodity x year resolution (L3)"),
  subtitle = paste0("One point per country x commodity x year cell, value-added ",
                    "components summed; panels are the two MRIO bases. ", SCALE_NOTE))

out_svg_l3 <- file.path(out_plot_dir,
                        paste0("bcp_value_added_comparison_L3", mode_tag, ".svg"))
ggsave(out_svg_l3, p_l3, width = 10, height = 6.5, device = "svg")
message(sprintf("36 section 4 (L3 scatter): %d cells, %d base(s) x %d reference(s) -> %s",
                cells[component == "total", .N], uniqueN(cells$base),
                cells[component == "total", uniqueN(reference)], out_svg_l3))

sub_cells <- cells[component != "total"][
  , component := factor(as.character(component), levels = VA_COMPONENTS)]
if (!nrow(sub_cells)) {
  message("36 section 4 (L4 scatter): no component cells - skipping the L4 plot.")
} else {
  p_l4 <- va_symlog_plot(
    sub_cells,
    colour = "component", colours = VA_COMPONENT_COLOURS,
    colour_name = "Value-added component", colour_labels = VA_MEASURE[VA_COMPONENTS],
    # switch = "both": each strip sits beside the axis it qualifies -- the MRIO
    # base down the left, the reference along the foot.
    facets = facet_grid(base ~ reference, switch = "both"),
    title    = paste0("FABIO-BCP value added against the independent references at ",
                      "country x commodity x year x component resolution (L4)"),
    subtitle = paste0("One point per country x commodity x year x value-added ",
                      "component cell; panel rows are the two MRIO bases, columns ",
                      "the references that resolve the same three accounts. ", SCALE_NOTE))
  
  out_svg_l4 <- file.path(out_plot_dir,
                          paste0("bcp_value_added_comparison_L4", mode_tag, ".svg"))
  # Square panels, so each MRIO base row adds its own height.
  ggsave(out_svg_l4, p_l4, width = 8.5,
         height = 2.7 + 3.5 * uniqueN(sub_cells$base), device = "svg", limitsize = FALSE)
  message(sprintf("36 section 4 (L4 scatter): %d cells, %d base(s) x %d reference(s) -> %s",
                  nrow(sub_cells), uniqueN(sub_cells$base),
                  sub_cells[, uniqueN(as.character(reference))], out_svg_l4))
}