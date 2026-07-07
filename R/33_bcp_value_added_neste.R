# =============================================================================
# 33_bcp_value_added_neste.R
# Direct value added for the renewable-diesel bundle (Renewable diesel c149 and
# its HVO co-products Biopropane c150 / Bionaphtha c151), which carry no producer
# price in script 30 and so receive no value added from script 31.
#
# Method: a Gross Value Added intensity (USD per tonne) is built from Neste's
# Renewable Products segment and multiplied by each country's physical output.
#
#   GVA (income approach) = comparable EBITDA + compensation of employees.
#   (Net other taxes on production less subsidies are omitted — small, and not
#   separable from Neste's segment accounts.)
#   Employee cost is reported only for the whole group, so the segment's share is
#   allocated by its share of group average headcount. Comparable EBITDA strips
#   inventory-holding gains, matching the national-accounts GVA concept.
#
#   intensity[y] = (comparable_EBITDA[y] + allocated_personnel[y]) [MEUR]
#                  ------------------------------------------------
#                            RP sales volume[y]                    [Mt]
#                = EUR per tonne  ->  x ECB FX  ->  USD per tonne.
#
# Each model year uses its own Neste year; the all-years median is a fallback for
# any year with no Neste figure.
#
# Co-products: the segment's GVA is jointly produced across renewable diesel,
# naphtha and propane (+SAF) and the sales-volume denominator spans them, so the
# same per-tonne intensity is applied to c149 + c150 + c151.
#
# Every Neste input figure is hard-coded with its Annual Report page below.
#
# Inputs :  <output_dir_mode>/X.rds          total product output (rows iso3c_commcode)
#           <output_dir_mode>/io_labels.csv  row-aligned labels for X
# Outputs:  intermediate_data/bcp_value_added_neste.rds / .csv
#           intermediate_data/value_added_diagnostics/
#             bcp_value_added_neste_intensity.csv   per-year GVA build + citations
#             bcp_value_added_neste_coverage.csv    valued-row funnel by commodity
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

source("R/00_system_variables.R")   # years (2012:2022), output_dir_bcp
source("R/00_run_config.R")          # tag() / mode_tag / mode_dir()
output_dir_mode <- mode_dir(output_dir_bcp)


# --- configuration -----------------------------------------------------------

# Personnel allocation key. Only headcount inputs are provided below.
ALLOC_KEY <- "headcount"

# Renewable-diesel bundle commodities and their density (tonnes per 1000 L),
# matching the model's own conversions in 07_06 / 07_10_compile_bcp.R.
BUNDLE <- data.table(
  comm_code   = c("c149",              "c150",       "c151"),
  item        = c("Renewable diesel",  "Biopropane", "Bionaphtha"),
  t_per_1000L = c(1 / 1.282,           0.51,         0.71))     # HVO 0.780 / propane / naphtha


# --- Neste source figures (Annual Reports 2012-2025) -------------------------
# MEUR unless noted. `src_*` gives the report and page each figure is read from.
# comp_ebitda : Renewable Products/Fuels segment, Comparable EBITDA. Reported
#               directly except 2018/2019, derived as comparable operating profit
#               + (EBITDA - operating profit); see src_ebitda for the per-year page.
# emp_group   : Group employee benefit costs, total (income statement).
# hc_rp/hc_tot: Renewable Products/Fuels & group average headcount. Segment
#               definitions change at the 2019/2020 break; harmless, as each
#               year's share is applied within its own year.
# sales_mt    : Renewable Products/Fuels sales volume, million tonnes (renewable-
#               diesel sales through 2019, when it was essentially the segment).
neste <- data.table(
  year        = 2012:2025,
  comp_ebitda = c(  43,  371,  335,  497,  578,  671, 1110, 1765,
                    1518, 1460, 1762, 1906,  514,  764),
  emp_group   = c( 342,  353,  339,  351,  349,  372,  400,  395,
                   431,  431,  545,  642,  582,  584),
  hc_rp       = c( 260,  261,  259,  263,  267,  301,  372,  539,
                   1014, 1245, 1528, 1983, 2046, 1894),
  hc_tot      = c(5031, 5097, 4989, 4906, 5013, 5297, 5468, 5474,
                  4833, 4872, 5244, 6018, 5796, 5214),
  sales_mt    = c(1.700, 1.900, 2.104, 2.267, 2.222, 2.567, 2.261, 2.846,
                  2.97,  3.021, 3.03,  3.3,   3.7,   4.1),
  # ECB euro reference rate, annual average, USD per EUR (2025 provisional).
  fx_usd_eur  = c(1.2848, 1.3281, 1.3285, 1.1095, 1.1069, 1.1297, 1.1810, 1.1195,
                  1.1422, 1.1827, 1.0530, 1.0813, 1.0824, 1.0800))

# Page citations, carried through to the diagnostics file for auditability.
neste_src <- data.table(
  year        = 2012:2025,
  src_ebitda  = c("AR2012 p.204", "AR2014 p.167", "AR2014 p.167", "AR2015 p.95",
                  "AR2016 p.103", "AR2017 p.102", "AR2018 p.108 (derived 983+127)",
                  "AR2019 p.108 (derived 1599+166)",
                  "AR2021 p.146 (2020 col)", "AR2021 p.146", "AR2022 p.150",
                  "AR2023 p.148", "AR2024 p.93", "AR2025 p.81"),
  src_emp_hc  = c("AR2012 p.218/252", "AR2013 p.210/248", "AR2014 p.178/208",
                  "AR2016 p.117 (2015 col)", "AR2016 p.117/145", "AR2017 p.140",
                  "AR2018 p.151", "AR2019 p.153",
                  "AR2020 p.158", "AR2021 p.191", "AR2022 p.201",
                  "AR2023 p.201", "AR2024 p.179", "AR2025 p.175"),
  src_sales   = c("AR2012 p.34", "AR2013 p.31", "AR2015 p.95 (2014 col)", "AR2015 p.95",
                  "AR2017 p.103 (2016 col)", "AR2017 p.103", "AR2018 p.109", "AR2019 p.109",
                  "AR2020 p.115", "AR2021 p.146", "AR2022 p.150",
                  "AR2023 p.147", "AR2024 p.91", "AR2025 p.79"))


# --- intensity (USD per tonne) -----------------------------------------------
neste[, hc_share    := hc_rp / hc_tot]
neste[, personnel   := emp_group * hc_share]           # group cost -> RP by headcount
neste[, gva_meur    := comp_ebitda + personnel]        # income-approach GVA, MEUR
neste[, intensity_eur_t := gva_meur / sales_mt]        # MEUR / Mt == EUR / t
neste[, intensity_usd_t := intensity_eur_t * fx_usd_eur]

median_usd_t <- median(neste$intensity_usd_t)

# Each model year uses its own Neste intensity; the all-years median is a fallback
# for any year with no Neste figure.
intensity_by_year <- data.table(year = years)
intensity_by_year[neste, own := i.intensity_usd_t, on = "year"]
intensity_by_year[, `:=`(
  intensity_usd_t = fifelse(is.na(own), median_usd_t, own),
  intensity_basis = fifelse(is.na(own), "median_all_neste_years", "neste_own_year"))]
intensity_by_year[, own := NULL]


# --- model physical output for the bundle ------------------------------------
X         <- readRDS(file.path(output_dir_mode, "X.rds"))
io_labels <- fread(file.path(output_dir_mode, "io_labels.csv"))
io_labels[, row_id := paste(iso3c, comm_code, sep = "_")]

X_long <- melt(as.data.table(X, keep.rownames = "row_id"),
               id.vars = "row_id", variable.name = "year",
               value.name = "output", variable.factor = FALSE)
X_long <- merge(
  X_long, io_labels[, .(row_id, iso3c, area_code, comm_code, unit)],
  by = "row_id", all.x = TRUE)

X_long <- X_long[comm_code %in% BUNDLE$comm_code & year %in% as.character(years)]
X_long[, year := as.integer(year)]
X_long <- merge(X_long, BUNDLE, by = "comm_code")


# --- output -> tonnes -> value added -----------------------------------------
# Bundle output is in "1000 liters"; match the exact unit string so any unexpected
# unit falls to NA and surfaces in the coverage diagnostic rather than being mis-scaled.
X_long[, output_t := fcase(
  unit == "1000 liters", output * t_per_1000L,
  unit == "tonnes",      output,
  default = NA_real_)]

X_long[intensity_by_year, `:=`(intensity_usd_t = i.intensity_usd_t,
                               intensity_basis  = i.intensity_basis), on = "year"]

X_long[, value_added_usd := fcase(
  is.na(output_t) | is.na(intensity_usd_t), NA_real_,
  output_t == 0,                            0,
  default = output_t * intensity_usd_t)]


# --- assemble ----------------------------------------------------------------
out <- X_long[, .(iso3c, area_code, comm_code, item, year,
                  output, unit, output_t,
                  intensity_usd_t, intensity_basis, value_added_usd)]
setorder(out, iso3c, comm_code, year)


# --- diagnostics -------------------------------------------------------------
intensity_diag <- merge(
  neste[, .(year, comp_ebitda, emp_group, hc_rp, hc_tot,
            hc_share = round(hc_share, 4), personnel = round(personnel, 1),
            gva_meur = round(gva_meur, 1), sales_mt,
            intensity_eur_t = round(intensity_eur_t), fx_usd_eur,
            intensity_usd_t = round(intensity_usd_t))],
  neste_src, by = "year")
intensity_diag[, alloc_key := ALLOC_KEY]

coverage <- X_long[, .(cells = .N,
                       valued  = sum(is.finite(value_added_usd)),
                       output_t = round(sum(output_t, na.rm = TRUE)),
                       va_usd_mn = round(sum(value_added_usd, na.rm = TRUE) / 1e6, 1)),
                   by = .(comm_code, item)]
setorder(coverage, comm_code)


# --- write -------------------------------------------------------------------
diag_dir <- "intermediate_data/value_added_diagnostics"
dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)

rds_path  <- tag("intermediate_data/bcp_value_added_neste.rds")
csv_path  <- sub("\\.csv$", paste0(mode_tag, ".csv"), "intermediate_data/bcp_value_added_neste.csv")

saveRDS(out, rds_path)
fwrite(out, csv_path)
fwrite(intensity_diag, file.path(diag_dir, paste0("bcp_value_added_neste_intensity", mode_tag, ".csv")))
fwrite(coverage,       file.path(diag_dir, paste0("bcp_value_added_neste_coverage",  mode_tag, ".csv")))

cat("\nNeste GVA intensity (USD/t), by Neste year\n")
print(neste[, .(year, gva_meur = round(gva_meur), sales_mt,
                intensity_usd_t = round(intensity_usd_t))])
cat(sprintf("\nMedian intensity across Neste years (fallback only): %.0f USD/t\n", median_usd_t))
message(sprintf("34_bcp_value_added_neste: %d rows, %d valued -> %s",
                nrow(out), out[is.finite(value_added_usd), .N], rds_path))