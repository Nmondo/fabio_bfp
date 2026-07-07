# =============================================================================
# 36_bcp_value_added_comparison.R
# Direct biofuel value added for the EU Member States, read from the JRC BioSAM
# Social Accounting Matrices. In a SAM an activity's value added is what its
# column pays to the primary-factor rows; here full gross value added (GVA at
# basic prices) = LABOUR + CAPITAL + TLS-A (taxes less subsidies on production).
# TLS-C is a tax on products (never in an activity column) and is excluded.
# Biofuel subsidies routinely make TLS-A -- and occasionally CAPITAL -- negative;
# those signs are genuine and are preserved.
#
# Two fuel groups, per the modelling scope:
#   Biogasoline = Biogasoline (A_BIOG) + 2nd-gen biochemical pathway (A_ETH)
#   Biodiesel   = Biodiesel   (A_BIOD)
# The 2nd-gen thermal pathway (A_FTFUEL) is out of scope and ignored.
#
# GVA is summed over each group's activities per (country, year), reported in
# native million EUR and converted to USD with a hard-coded annual FX rate
# (ECB euro reference rate, period average, USD per EUR). Individual countries
# and the EU27-2020 aggregate are both kept; the aggregate carries is_aggregate
# = TRUE so it can be dropped in one filter.
#
# Inputs :  input/value_added/BioSAMs/*BioSAMs*<year>.csv
#             JRC BioSAM, long format: Receiving Agent (row/income) gets Value
#             from Spending Agent (column/expenditure), in million EUR.
# Outputs:  intermediate_data/bcp_value_added_biosam.rds / .csv
#             one row per (country x year x fuel): the three GVA components and
#             the total in EUR mn, plus fx and value_added_usd.
#           intermediate_data/value_added_diagnostics/bcp_value_added_biosam_diagnostics.csv
#             input funnel, per-year country coverage, unmapped ISO codes,
#             missing-FX years, and negative-GVA (net-subsidy) country-years.
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

source("R/00_run_config.R")   # tag() / mode_tag for output naming


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

message(sprintf("33_bcp_value_added_biosam: %d rows across %d year(s), %d fuel group(s) -> %s",
                nrow(out), uniqueN(out$year), uniqueN(out$fuel), rds_path))


# =============================================================================
# APPEND to 36_bcp_value_added_comparison.R (after the BioSAM extraction, which
# leaves `out` in scope).  Runs after the BioSAM block: first derive the
# Canada-report GVA, then draw the comparison scatter that pits the MRIO base
# against every independent estimate.
# =============================================================================

library(ggplot2)
library(scales)


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


# --- comparison scatter: MRIO base VA vs each independent estimate -----------
# One dot = one (country x commodity x year) cell.  x = the MRIO-intensity base
# value added (script 31); y = an INDEPENDENT estimate for the SAME cell.  A dot
# on the dashed identity line (y = x) means the MRIO base already reproduces the
# independent estimate; distance from the line is the disagreement.  Log-log,
# with the two MRIO bases (GLORIA, EXIOBASE) as the two facets; every source
# keeps one fixed colour across both so "same source, same colour" reads at a
# glance.  Neste is a *fill* for the renewable-diesel bundle (no MRIO base to
# sit against), so it has no x and is absent by construction.
#
# The plot is driven off three small declarations — the base files, the
# estimates carried as columns of those files, and the estimates that live in
# their own tables — so adding a source is a one-line edit, not a new branch.

# (1) MRIO base files, one facet each.  Read the mode-tagged CSV so a bypass run
#     compares its own combined outputs rather than the rescaled ones.
COMPARISON_BASES <- c(
  GLORIA   = "intermediate_data/bcp_value_added_combined_gloria.csv",
  EXIOBASE = "intermediate_data/bcp_value_added_combined_exiobase.csv")

# (2) Independent estimates carried as COLUMNS inside each combined file, mapped
#     raw-column -> display label.
INLINE_SOURCES <- c(
  va_brazil_sut_usd = "Brazil SUT (IBGE)",
  va_us_reports_usd = "US reports (RFA/ABF)")

# (3) Independent estimates that live in their OWN tables, keyed to a base cell
#     by (iso3c, comm_code, year).  Each is already USD and exposes
#     value_added_usd; BioSAM overlaps the model window in one useful year only.
FUEL2COMM        <- c(Biogasoline = "c146", Biodiesel = "c147")
EXTERNAL_SOURCES <- list(
  "BioSAM (JRC) 2015" = out[year == 2015L & is_aggregate == FALSE,
                            .(iso3c, comm_code = FUEL2COMM[fuel], year = 2015L,
                              value_added_usd)],
  "Canada report (CFA)" = canada[, .(iso3c, comm_code, year = as.integer(year),
                                     value_added_usd)])

# Fixed source order + colourblind-safe palette (Dark2), stable across facets
# and independent of which cells happen to be present in a given run.
SOURCE_LEVELS <- c(unname(INLINE_SOURCES), names(EXTERNAL_SOURCES))
SOURCE_COLORS <- setNames(
  c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")[seq_along(SOURCE_LEVELS)],
  SOURCE_LEVELS)

# One tidy (base, source, x = base VA, y = independent VA) table per base.
comparison_points <- function(base_name, csv_path) {
  # integer64 = "double": some value columns (e.g. va_us_reports_usd) are whole
  # numbers > 2^31, which fread would otherwise read as bit64::integer64. When
  # melt() later stacks such a column with a plain double column, the integer64
  # values are copied by their raw 64-bit pattern instead of value-converted,
  # turning ~1.5e10 into a denormalised ~1e-313. Forcing double on read avoids it.
  comb <- fread(sub("\\.csv$", paste0(mode_tag, ".csv"), csv_path),
                integer64 = "double")
  comb[, year := as.integer(year)]
  
  inline <- melt(
    comb[, c("iso3c", "comm_code", "year", "va_mriot_usd", names(INLINE_SOURCES)),
         with = FALSE],
    id.vars       = c("iso3c", "comm_code", "year", "va_mriot_usd"),
    variable.name = "source", value.name = "y", variable.factor = FALSE)
  inline[, source := INLINE_SOURCES[source]]
  
  base_cell <- comb[, .(iso3c, comm_code, year, va_mriot_usd)]
  external  <- rbindlist(Map(function(label, dt)
    merge(dt[, .(iso3c, comm_code, year, y = value_added_usd, source = label)],
          base_cell, by = c("iso3c", "comm_code", "year")),
    names(EXTERNAL_SOURCES), EXTERNAL_SOURCES), use.names = TRUE)
  
  rbind(inline, external, use.names = TRUE)[
    is.finite(va_mriot_usd) & is.finite(y) & va_mriot_usd > 0 & y > 0][
      , base := base_name][]
}

plot_dt <- rbindlist(Map(comparison_points, names(COMPARISON_BASES), COMPARISON_BASES))
plot_dt[, `:=`(base   = factor(base,   levels = names(COMPARISON_BASES)),
               source = factor(source, levels = SOURCE_LEVELS))]

# Fit metrics per base, in log10 space (a USD RMSE would be size-dominated):
#   RMSLE        = sqrt(mean((log10 y - log10 x)^2))      spread about identity
#   median ratio = 10^median(log10 y - log10 x)           signed typical bias
#   median fold  = 10^median|log10 y - log10 x|           typical |disagreement|
metrics <- plot_dt[, {
  lr <- log10(y) - log10(va_mriot_usd)
  .(n = .N, rmsle = sqrt(mean(lr^2)),
    median_ratio = 10^median(lr), median_fold = 10^median(abs(lr)))
}, by = base]
metrics[, label := sprintf("n = %d\nRMSLE = %.2f\nmedian ratio = %.2f\u00d7\nmedian fold = %.2f\u00d7",
                           n, rmsle, median_ratio, median_fold)]

comparison_diag_dir <- "intermediate_data/value_added_diagnostics"
dir.create(comparison_diag_dir, showWarnings = FALSE, recursive = TRUE)
fwrite(metrics[, .(base, n, rmsle, median_ratio, median_fold)],
       file.path(comparison_diag_dir,
                 paste0("bcp_value_added_comparison_metrics", mode_tag, ".csv")))

# Shared, square, equal-decade window across BOTH facets, so the dashed identity
# reads as a true 45 deg and the two bases are directly comparable.  aspect.ratio
# (not coord_equal) keeps the panel square without collapsing the log axes.
lim     <- 10^(range(log10(c(plot_dt$va_mriot_usd, plot_dt$y))) + c(-0.3, 0.3))
log_lab <- trans_format("log10", math_format(10^.x))
metrics[, `:=`(x = lim[1], y = lim[2])]   # top-left metric anchor, per facet

p <- ggplot(plot_dt, aes(va_mriot_usd, y, colour = source)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              linewidth = 0.5, colour = "grey35") +
  geom_point(size = 1.9, alpha = 0.85, stroke = 0) +
  geom_text(data = metrics, aes(x = x, y = y, label = label), inherit.aes = FALSE,
            hjust = 0, vjust = 1, size = 3, colour = "grey25", lineheight = 0.95) +
  facet_wrap(~ base, nrow = 1) +
  scale_x_log10(limits = lim, labels = log_lab) +
  scale_y_log10(limits = lim, labels = log_lab) +
  scale_colour_manual(values = SOURCE_COLORS, drop = FALSE, name = NULL) +
  labs(
    title    = "Bio-based-commodity value added: MRIO base vs independent estimates",
    subtitle = paste0("One point per country x commodity x year.  Dashed line = identity (y = x).\n",
                      "Points above the line mean the independent estimate exceeds the MRIO base."),
    x = "MRIO-intensity base value added (current US$, log scale)",
    y = "Independent estimate of value added (current US$, log scale)") +
  theme_minimal(base_size = 11) +
  theme(
    aspect.ratio        = 1,
    panel.grid.minor    = element_line(colour = "grey93", linewidth = 0.2),
    panel.grid.major    = element_line(colour = "grey87", linewidth = 0.3),
    panel.spacing       = unit(1.4, "lines"),
    strip.text          = element_text(face = "bold"),
    legend.position     = "bottom",
    plot.title.position = "plot",
    plot.title          = element_text(face = "bold", margin = margin(b = 3)),
    plot.subtitle       = element_text(size = 9, colour = "grey30", lineheight = 1.1,
                                       margin = margin(b = 10))) +
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1)))

# Label the strongest identity-line outliers (|log residual| past the 1.5*IQR
# fence) per facet, if ggrepel is available — a light touch, not every point.
outliers <- plot_dt[, {
  lr <- log10(y) - log10(va_mriot_usd)
  qr <- quantile(lr, c(0.25, 0.75), names = FALSE); f <- 1.5 * (qr[2] - qr[1])
  .SD[lr < qr[1] - f | lr > qr[2] + f]
}, by = base]
if (nrow(outliers) && requireNamespace("ggrepel", quietly = TRUE)) {
  p <- p + ggrepel::geom_text_repel(
    data = outliers, aes(label = iso3c), inherit.aes = TRUE,
    size = 2.5, colour = "grey30", max.overlaps = Inf,
    min.segment.length = 0, segment.size = 0.2, show.legend = FALSE)
}

out_plot_dir <- file.path("output", "plot")
dir.create(out_plot_dir, showWarnings = FALSE, recursive = TRUE)
out_svg <- file.path(out_plot_dir, paste0("bcp_value_added_comparison", mode_tag, ".svg"))
ggsave(out_svg, p, width = 11, height = 6.6)

message(sprintf("35_bcp_value_added_comparison: %d points across %d base(s) -> %s",
                nrow(plot_dt), uniqueN(plot_dt$base), out_svg))