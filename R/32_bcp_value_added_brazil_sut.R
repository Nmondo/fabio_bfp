# =============================================================================
# 32_bcp_value_added_brazil_sut.R
# Direct Brazil biofuel value added, measured from the national IBGE Supply &
# Use Tables. The IBGE product 19921 ("Etanol e outros biocombustiveis") pools
# ethanol and biodiesel, so its measured value added is split across Biogasoline
# (c146) and Biodiesel (c147) in proportion to their Brazil producer values
# (script 30). The result is a per-year, per-commodity table meant to later
# OVERWRITE and VALIDATE the Brazil c146 / c147 rows of 31_bcp_value_added.R --
# this script only produces that table; it does not touch script 31's output.
#
# Method: product 19921 is co-produced by several activities (notably sugar
# refining and the dedicated biofuels industry), and its main activity also
# makes non-fuel products, so its VA cannot be read off a single industry. We
# use the industry-technology assumption (make-share allocation): each activity's
# total value added is split across the products it makes in proportion to
# gross-output share, and we keep product 19921's slice:
#     VA_pool[y] = sum_i  VA_activity_i[y] * make[19921, i, y] / output_i[y]
# VA is the whole block (labour + capital + taxes - subsidies), taken directly
# from the workbook's "Valor adicionado bruto" row, in 10^6 BRL, then converted
# to USD with a hard-coded annual exchange rate. That pooled VA is then divided
# between c146 and c147 by their Brazil producer-value shares for the year.
#
# Inputs :  inst/value_added/brazil_sut/68_tab1_<year>.xls   Recursos (Supply / Make)
#           inst/value_added/brazil_sut/68_tab2_<year>.xls   Usos     (Use / VA)
#           intermediate_data/bcp_producer_total_values.rds  producer values (script 30)
# Outputs:  intermediate_data/bcp_value_added_brazil_sut.rds / .csv
#             two rows per available year (c146, c147) carrying the pooled 19921
#             VA, the producer-value split share, and the split value_added_sut
#             [USD], keyed (iso3c, comm_code, item, year) so it can be joined
#             onto script 31's Brazil rows.
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
if (!requireNamespace("readxl", quietly = TRUE))
  stop("This script needs the 'readxl' package to read the .xls workbooks.")

source("R/00_system_variables.R")   # provides `years`
source("R/00_run_config.R")         # provides tag() / mode_tag for the split key
keep_years <- as.integer(years)


# --- configuration -----------------------------------------------------------

# Where the IBGE workbook pairs live, and their filename templates.
SUT_DIR         <- "input/value_added/Brazil_SUTs"
SUPPLY_FILE_FMT <- "68_tab1_%d.xls"   # Recursos (Supply / Make matrix)
USE_FILE_FMT    <- "68_tab2_%d.xls"   # Usos     (Use / Value added)

SUPPLY_SHEET <- "producao"            # make matrix (product x activity)
USE_SHEET    <- "VA"                  # value added by activity

# The pooled biofuel product line in the IBGE product classification.
SUT_PRODUCT_CODE <- "19921"
SUT_PRODUCT_NAME <- "Etanol e outros biocombustiveis"

# Row label (column A of the VA sheet) carrying gross value added per activity.
VA_ROW_PREFIX <- "valor adicionado bruto"

# FABIO commodities the pooled VA is split across, and the producer-value key.
TARGET_ISO3C      <- "BRA"
SPLIT_COMMODITIES <- data.table(comm_code = c("c146",        "c147"),
                                item      = c("Biogasoline", "Biodiesel"))
PRODUCER_VALUE_PATH <- tag("intermediate_data/bcp_producer_total_values.rds")

# Annual BRL per USD, period-average official rate (hard-coded on request).
# Extend this table when adding more workbook years.
FX_BRL_PER_USD <- c(
  "2012" = 1.9546, "2013" = 2.1561, "2014" = 2.3533, "2015" = 3.3387,
  "2016" = 3.4901, "2017" = 3.1922, "2018" = 3.6540, "2019" = 3.9445,
  "2020" = 5.1558, "2021" = 5.3944)


# --- workbook parsing helpers ------------------------------------------------

# Leading code token of a cell: text before the first line break, trimmed.
lead_token <- function(x) trimws(sub("[\r\n].*$", "", as.character(x)))

# Normalise a label: collapse internal whitespace, trim, lower-case.
norm_label <- function(x) tolower(trimws(gsub("[[:space:]]+", " ", as.character(x))))

# Case-insensitive sheet resolver (guards against capitalisation drift).
resolve_sheet <- function(path, want) {
  sh  <- readxl::excel_sheets(path)
  hit <- sh[tolower(trimws(sh)) == tolower(trimws(want))]
  if (!length(hit))
    stop("Workbook ", basename(path), " has no sheet '", want,
         "'. Sheets present: ", paste(sh, collapse = ", "))
  hit[1L]
}

# Read a whole sheet as a character matrix (no header interpretation).
read_sheet_matrix <- function(path, sheet) {
  df <- suppressMessages(readxl::read_excel(
    path, sheet = sheet, col_names = FALSE,
    col_types = "text", .name_repair = "minimal"))
  as.matrix(df)
}

# Locate the activity header row: the row holding the most 4-digit codes.
find_activity_header <- function(mat) {
  counts <- apply(mat, 1L, function(r) sum(grepl("^[0-9]{4}$", lead_token(r))))
  hrow   <- which.max(counts)
  if (!length(hrow) || counts[hrow] < 5L)
    stop("Could not locate the activity header row (expected many 4-digit codes in one row).")
  codes <- lead_token(mat[hrow, ])
  cols  <- which(grepl("^[0-9]{4}$", codes))
  list(cols = cols, codes = codes[cols])
}

# Locate the product-code column: the column holding the most 5-digit codes.
find_code_col <- function(mat) {
  counts <- apply(mat, 2L, function(cc) sum(grepl("^[0-9]{5}$", lead_token(cc))))
  cc     <- which.max(counts)
  if (!length(cc) || counts[cc] < 5L)
    stop("Could not locate the product-code column (expected many 5-digit codes in one column).")
  as.integer(cc)
}

num <- function(v) suppressWarnings(as.numeric(v))


# --- per-year pooled value added ---------------------------------------------

# From the Supply workbook: per-activity gross output (make column sums) and the
# pooled product's make value per activity.
read_supply <- function(supply_path, year) {
  mat  <- read_sheet_matrix(supply_path, resolve_sheet(supply_path, SUPPLY_SHEET))
  hdr  <- find_activity_header(mat)
  pcol <- find_code_col(mat)
  
  code <- lead_token(mat[, pcol])
  code <- ifelse(grepl("^[0-9]{4}$", code), paste0("0", code), code)  # repair lost leading zero
  prows <- which(grepl("^[0-9]{5}$", code))
  
  vals <- matrix(num(mat[prows, hdr$cols]), nrow = length(prows),
                 dimnames = list(code[prows], hdr$codes))
  vals[!is.finite(vals)] <- 0
  
  output_i <- colSums(vals)                    # per-activity total gross output
  prow     <- which(rownames(vals) == SUT_PRODUCT_CODE)
  if (!length(prow))
    stop("Product ", SUT_PRODUCT_CODE, " not found in ", basename(supply_path), ".")
  make_pool <- colSums(vals[prow, , drop = FALSE])   # pooled make per activity
  
  data.table(activity_code = hdr$codes, year = as.integer(year),
             make_pool = as.numeric(make_pool), output_i = as.numeric(output_i))
}

# From the Use workbook: total value added per activity (the "Valor adicionado
# bruto" row = labour + capital + taxes - subsidies).
read_va <- function(use_path, year) {
  mat <- read_sheet_matrix(use_path, resolve_sheet(use_path, USE_SHEET))
  hdr <- find_activity_header(mat)
  lab <- norm_label(mat[, 1L])
  vrow <- which(startsWith(lab, VA_ROW_PREFIX))
  if (!length(vrow))
    stop("Value-added row ('", VA_ROW_PREFIX, "...') not found in ", basename(use_path), ".")
  data.table(activity_code = hdr$codes, year = as.integer(year),
             va_activity = num(mat[vrow[1L], hdr$cols]))
}

# Make-share allocation: pooled VA = sum_i VA_i * make[19921,i] / output_i.
pooled_va_year <- function(year) {
  supply <- file.path(SUT_DIR, sprintf(SUPPLY_FILE_FMT, year))
  use    <- file.path(SUT_DIR, sprintf(USE_FILE_FMT,    year))
  if (!file.exists(supply) || !file.exists(use)) return(NULL)
  
  d <- merge(read_supply(supply, year), read_va(use, year),
             by = c("activity_code", "year"))
  d[, make_share := fifelse(output_i > 0, make_pool / output_i, 0)]
  d[, va_alloc   := va_activity * make_share]
  data.table(year = as.integer(year),
             pooled_output_brl_mn = d[, sum(make_pool, na.rm = TRUE)],
             pooled_va_brl_mn     = d[, sum(va_alloc,  na.rm = TRUE)])
}


# --- pooled VA per year ------------------------------------------------------

res <- rbindlist(lapply(keep_years, pooled_va_year))
if (is.null(res) || !nrow(res))
  stop("No Brazil SUT workbook pair found in '", SUT_DIR, "' for years ",
       paste(range(keep_years), collapse = "-"), ".")

missing_years <- setdiff(keep_years, res$year)
if (length(missing_years))
  message(sprintf("  No workbook pair for %d year(s) (skipped): %s",
                  length(missing_years), paste(missing_years, collapse = ", ")))


# --- producer-value split c146 / c147 ----------------------------------------

if (!file.exists(PRODUCER_VALUE_PATH))
  stop("Producer values not found (", PRODUCER_VALUE_PATH, ") - run script 30 first.")
pv  <- as.data.table(readRDS(PRODUCER_VALUE_PATH))
key <- pv[iso3c == TARGET_ISO3C & comm_code %in% SPLIT_COMMODITIES$comm_code,
          .(comm_code, year = as.integer(year), producer_value_usd = total_value)]
key[, producer_value_usd := fifelse(is.finite(producer_value_usd) & producer_value_usd > 0,
                                    producer_value_usd, 0)]

grid <- key[CJ(comm_code = SPLIT_COMMODITIES$comm_code, year = res$year),
            on = .(comm_code, year)]
grid[is.na(producer_value_usd), producer_value_usd := 0]
grid[, denom := sum(producer_value_usd), by = year]
grid[, key_missing := denom <= 0]
# fallback when a year has no Brazil producer value at all: all VA to c146.
grid[, share := fifelse(denom > 0, producer_value_usd / denom,
                        as.numeric(comm_code == "c146"))]

fallback_years <- sort(unique(grid[key_missing == TRUE, year]))
if (length(fallback_years))
  message(sprintf("  No producer-value split for %d year(s); pooled VA assigned to c146: %s",
                  length(fallback_years), paste(fallback_years, collapse = ", ")))


# --- build the table ---------------------------------------------------------

d <- merge(grid, res,               by = "year")
d <- merge(d,    SPLIT_COMMODITIES, by = "comm_code")
d[, value_added_brl_mn := pooled_va_brl_mn * share]

# Attach the hard-coded FX and convert 10^6 BRL -> USD.
d[, fx_brl_per_usd := FX_BRL_PER_USD[as.character(year)]]
no_fx <- unique(d[!is.finite(fx_brl_per_usd), year])
if (length(no_fx)) {
  warning("No hard-coded FX rate for year(s): ", paste(no_fx, collapse = ", "),
          " - add them to FX_BRL_PER_USD. Those rows are dropped.")
  d <- d[is.finite(fx_brl_per_usd)]
}
d[, `value_added_sut [USD]` := value_added_brl_mn * 1e6 / fx_brl_per_usd]

out <- d[share > 0, .(iso3c         = TARGET_ISO3C,
                      comm_code,
                      item,
                      year,
                      sut_item_code = SUT_PRODUCT_CODE,
                      sut_item      = SUT_PRODUCT_NAME,
                      pooled_output_brl_mn,
                      pooled_va_brl_mn,
                      producer_value_usd,
                      split_share   = share,
                      value_added_brl_mn,
                      fx_brl_per_usd,
                      `value_added_sut [USD]`)]
setorder(out, year, comm_code)


# --- output ------------------------------------------------------------------
dir.create("intermediate_data", showWarnings = FALSE, recursive = TRUE)
rds_path <- tag("intermediate_data/bcp_value_added_brazil_sut.rds")
csv_path <- sub("\\.csv$", paste0(mode_tag, ".csv"),
                "intermediate_data/bcp_value_added_brazil_sut.csv")
saveRDS(out, rds_path)
fwrite(out, csv_path)

print(out)
message(sprintf("32_bcp_value_added_brazil_sut: %d row(s) across %d year(s) -> %s.",
                nrow(out), uniqueN(out$year), rds_path))