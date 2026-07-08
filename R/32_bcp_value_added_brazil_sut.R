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
# value added is split across the products it makes in proportion to gross-output
# share, and we keep product 19921's slice:
#     VA_pool[y] = sum_i  VA_activity_i[y] * make[19921, i, y] / output_i[y]
# VA is read as three accounts — wages ("Remunerações"), capital ("Excedente
# operacional bruto e rendimento misto bruto") and tls ("Outros impostos sobre a
# produção" + "Outros subsídios à produção", subsidies stored NEGATIVE so both
# are added as-is) — each allocated separately, in 10^6 BRL, then converted to
# USD with a hard-coded annual exchange rate. Each account's pooled VA is then
# divided between c146 and c147 by their Brazil producer-value shares for the
# year; the total is the signed sum of the three.
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

# VA accounts, and the column-A labels of the "VA" sheet that feed each (PARENT
# rows only).  IBGE stores subsidies NEGATIVE, so both TLS rows are added as-is.
VA_ACCOUNTS       <- c("wages", "capital", "tls")
BRA_LABOUR_LABELS  <- c("Remunera\u00e7\u00f5es")                                  # Remunerações
BRA_CAPITAL_LABELS <- c("Excedente operacional bruto e rendimento misto bruto")
BRA_TLS_POS_LABELS <- c("Outros impostos sobre a produ\u00e7\u00e3o")              # Outros impostos sobre a produção
BRA_TLS_NEG_LABELS <- c("Outros subs\u00eddios \u00e0 produ\u00e7\u00e3o")         # Outros subsídios à produção
BRA_TLS_SUBSIDY_SIGN <- 1
# Gross-output row label (make-share denominator cross-check).
BRA_OUTPUT_ROW_LABEL <- "Valor da produ\u00e7\u00e3o"                              # Valor da produção

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

# From the Supply workbook: the full make matrix in long form (product x
# activity) plus per-activity gross output (make column sums).
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
  if (!SUT_PRODUCT_CODE %in% rownames(vals))
    stop("Product ", SUT_PRODUCT_CODE, " not found in ", basename(supply_path), ".")
  
  make_long <- as.data.table(as.data.frame(as.table(vals)))
  setnames(make_long, c("sut_item_code", "activity_code", "sup_val"))
  make_long[, `:=`(sut_item_code = as.character(sut_item_code),
                   activity_code = as.character(activity_code),
                   sup_val       = as.numeric(sup_val),
                   year          = as.integer(year))]
  output_i <- data.table(activity_code = hdr$codes, year = as.integer(year),
                         output_i = as.numeric(colSums(vals)))
  list(make_long = make_long[is.finite(sup_val)], output_i = output_i)
}

# From the Use workbook: VA per activity for the three accounts, long
# (activity_code, va_account, year, va_value).  Subsidies (tls) are added with
# BRA_TLS_SUBSIDY_SIGN (+1: IBGE stores them negative).
read_va_by_activity <- function(use_path, year) {
  mat <- read_sheet_matrix(use_path, resolve_sheet(use_path, USE_SHEET))
  hdr <- find_activity_header(mat)
  colA <- norm_label(mat[, 1L])
  
  acct_spec <- list(
    list(acct = "wages",   labels = BRA_LABOUR_LABELS,  sign = 1),
    list(acct = "capital", labels = BRA_CAPITAL_LABELS, sign = 1),
    list(acct = "tls",     labels = BRA_TLS_POS_LABELS, sign = 1),
    list(acct = "tls",     labels = BRA_TLS_NEG_LABELS, sign = BRA_TLS_SUBSIDY_SIGN))
  
  found <- character(0); pieces <- list()
  for (sp in acct_spec) {
    targets <- norm_label(sp$labels)
    rws <- which(colA %in% targets)
    if (!length(rws)) next
    found  <- c(found, targets[targets %in% colA[rws]])
    vals   <- matrix(num(mat[rws, hdr$cols]), nrow = length(rws))
    peract <- colSums(vals, na.rm = TRUE) * sp$sign
    pieces[[length(pieces) + 1L]] <- data.table(
      activity_code = hdr$codes, va_account = sp$acct,
      year = as.integer(year), va_value = peract)
  }
  
  expect      <- norm_label(c(BRA_LABOUR_LABELS, BRA_CAPITAL_LABELS,
                              BRA_TLS_POS_LABELS, BRA_TLS_NEG_LABELS))
  missing_lab <- setdiff(expect, unique(found))
  if (length(missing_lab))
    warning(sprintf("[%d] VA account label(s) NOT found in the 'VA' sheet: %s",
                    year, paste(missing_lab, collapse = " | ")))
  
  rbindlist(pieces, use.names = TRUE)[
    , .(va_value = sum(va_value, na.rm = TRUE)),
    by = .(activity_code, va_account, year)]
}

# Make-share allocation per account: VA_product = sum_i VA_i * make[p,i] /
# output_i, keeping product 19921's slice.  Also runs the full-identity sanity
# check Sum_p VA_p = Sum_i VA_i per account (make-shares sum to 1 over all
# products for each activity with output_i > 0).
pooled_va_year <- function(year) {
  supply <- file.path(SUT_DIR, sprintf(SUPPLY_FILE_FMT, year))
  use    <- file.path(SUT_DIR, sprintf(USE_FILE_FMT,    year))
  if (!file.exists(supply) || !file.exists(use)) return(NULL)
  
  sup     <- read_supply(supply, year)
  va_ind  <- read_va_by_activity(use, year)
  
  shares <- merge(sup$make_long, sup$output_i, by = c("activity_code", "year"))
  shares[, make_share := fifelse(output_i > 0, sup_val / output_i, 0)]
  alloc  <- merge(va_ind, shares, by = c("activity_code", "year"),
                  allow.cartesian = TRUE)
  alloc[, va_share := va_value * make_share]
  va_prod <- alloc[, .(va_value = sum(va_share, na.rm = TRUE)),
                   by = .(sut_item_code, va_account, year)]
  
  # Full-identity sanity check per account.
  ind_tot  <- va_ind[activity_code %in% sup$output_i[output_i > 0, activity_code],
                     .(ind = sum(va_value, na.rm = TRUE)), by = va_account]
  prod_tot <- va_prod[, .(prod = sum(va_value, na.rm = TRUE)), by = va_account]
  chk      <- merge(ind_tot, prod_tot, by = "va_account", all = TRUE)
  chk[is.na(ind), ind := 0][is.na(prod), prod := 0]
  bad <- chk[abs(prod - ind) > 1e-6 * pmax(1, abs(ind))]
  if (nrow(bad))
    warning(sprintf("[%d] make-share identity gap (Sum_p != Sum_i): %s", year,
                    paste(sprintf("%s %+.3g", bad$va_account, bad$prod - bad$ind),
                          collapse = "; ")))
  
  pool <- merge(data.table(va_account = VA_ACCOUNTS),
                va_prod[sut_item_code == SUT_PRODUCT_CODE, .(va_account, va_value)],
                by = "va_account", all.x = TRUE)
  pool[is.na(va_value), va_value := 0]
  
  pooled_output <- sup$make_long[sut_item_code == SUT_PRODUCT_CODE,
                                 sum(sup_val, na.rm = TRUE)]
  res_row <- as.data.table(as.list(setNames(pool$va_value, pool$va_account)))
  res_row[, `:=`(year = as.integer(year), pooled_output_brl_mn = pooled_output)]
  res_row[]
}


# --- pooled VA per year ------------------------------------------------------

res <- rbindlist(lapply(keep_years, pooled_va_year), use.names = TRUE, fill = TRUE)
if (is.null(res) || !nrow(res))
  stop("No Brazil SUT workbook pair found in '", SUT_DIR, "' for years ",
       paste(range(keep_years), collapse = "-"), ".")

for (acct in VA_ACCOUNTS) if (!acct %in% names(res)) res[, (acct) := 0]
res[, pooled_va_brl_mn := wages + capital + tls]
setnames(res, VA_ACCOUNTS, sprintf("pooled_va_%s_brl_mn", VA_ACCOUNTS))

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

# Split each account's pooled VA by the producer-value share; total is the sum.
for (acct in VA_ACCOUNTS)
  d[, (sprintf("value_added_%s_brl_mn", acct)) :=
      get(sprintf("pooled_va_%s_brl_mn", acct)) * share]
d[, value_added_brl_mn := value_added_wages_brl_mn + value_added_capital_brl_mn +
    value_added_tls_brl_mn]

# Attach the hard-coded FX and convert 10^6 BRL -> USD, per account and total.
d[, fx_brl_per_usd := FX_BRL_PER_USD[as.character(year)]]
no_fx <- unique(d[!is.finite(fx_brl_per_usd), year])
if (length(no_fx)) {
  warning("No hard-coded FX rate for year(s): ", paste(no_fx, collapse = ", "),
          " - add them to FX_BRL_PER_USD. Those rows are dropped.")
  d <- d[is.finite(fx_brl_per_usd)]
}
for (acct in VA_ACCOUNTS)
  d[, (sprintf("value_added_sut_%s [USD]", acct)) :=
      get(sprintf("value_added_%s_brl_mn", acct)) * 1e6 / fx_brl_per_usd]
d[, `value_added_sut [USD]` := value_added_brl_mn * 1e6 / fx_brl_per_usd]

# Invariant: the three components sum to the total in every kept cell.
d[, .comp_sum := `value_added_sut_wages [USD]` + `value_added_sut_capital [USD]` +
    `value_added_sut_tls [USD]`]
if (d[share > 0 & is.finite(`value_added_sut [USD]`) &
      abs(.comp_sum - `value_added_sut [USD]`) > 1e-6 * pmax(1, abs(`value_added_sut [USD]`)), .N])
  stop("32: Brazil SUT components do not sum to the total.")
d[, .comp_sum := NULL]

out <- d[share > 0, .(iso3c         = TARGET_ISO3C,
                      comm_code,
                      item,
                      year,
                      sut_item_code = SUT_PRODUCT_CODE,
                      sut_item      = SUT_PRODUCT_NAME,
                      pooled_output_brl_mn,
                      pooled_va_wages_brl_mn,
                      pooled_va_capital_brl_mn,
                      pooled_va_tls_brl_mn,
                      pooled_va_brl_mn,
                      producer_value_usd,
                      split_share   = share,
                      value_added_wages_brl_mn,
                      value_added_capital_brl_mn,
                      value_added_tls_brl_mn,
                      value_added_brl_mn,
                      fx_brl_per_usd,
                      `value_added_sut_wages [USD]`,
                      `value_added_sut_capital [USD]`,
                      `value_added_sut_tls [USD]`,
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