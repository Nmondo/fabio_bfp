# =============================================================================
# 31_bcp_value_added_MRIOTs.R
# Value added for the bio-based commodities, estimated by multiplying each
# commodity's total producer value by a chemical-sector value-added intensity
# (VA / producer-price output) borrowed from two upstream MRIO databases:
#   GLORIA   — sector "Basic organic chemicals"
#   EXIOBASE — sector "Chemicals nec"
# VA is split into three components — wages (compensation of employees),
# capital (net operating surplus + net mixed income + consumption of fixed
# capital) and tls (taxes on production − subsidies, signed) — so one intensity
# per component is built per base; the two aggregate totals are the row-wise
# sums of their three components.
#
# For each database the intensity is built per (region, year), optionally
# cleaned (Hampel spike filter + MAD winsor, toggled by APPLY_INTENSITY_FILTER),
# then mapped to FABIO areas — output-weighted where several upstream regions
# share one area. Each producer-value row is matched to its own country's
# intensity for that year, giving two independent value-added estimates.
#
# Inputs :  intermediate_data/bcp_producer_total_values.rds   producer values (script 30)
#           GLORIA V / X matrices + ReadMe labels              raw upstream base
#           EXIOBASE x / factor-input F matrices + labels      raw upstream base
#           inst/value_added/concordance_areas_gloria_fabio.csv    region -> FABIO area
#           inst/value_added/concordance_areas_exiobase_fabio.csv  region -> FABIO area
# Outputs:  intermediate_data/bcp_value_added_MRIOTs.rds / .csv
#             producer-value context, per-component VA intensities and VA
#             (value_added_{wages,capital,tls}_{gloria,exiobase} [USD]), and the
#             aggregate totals value_added_gloria [USD] / value_added_exiobase
#             [USD] (row-wise sums of their three components).
#           intermediate_data/value_added_diagnostics/
#             bcp_value_added_intensity_{gloria,exiobase}.csv  region x year x
#               component VA,
#               output, and the raw -> Hampel -> winsor intensity with the spike
#               audit (window_median, series_mad, hampel_z, is_spike) and the cap
#               audit (cap_lower, cap_upper, mad_z, winsorized).
#             bcp_value_added_coverage.csv        per-base valued-row funnel.
#             bcp_value_added_unmatched_areas.csv producer-value areas that
#               matched no intensity, per base.
#
# Currency: the intensity is dimensionless and total_value is already USD, so
# value added is USD with no currency conversion.
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

source("R/00_system_variables.R")
source("R/00_run_config.R")

keep_years <- as.integer(years)


# --- configuration -----------------------------------------------------------

# Master switch for the intensity cleaning (Hampel spike filter + MAD winsor).
# FALSE uses the raw VA / X intensity as-is.
APPLY_INTENSITY_FILTER <- TRUE

# Cleaning parameters.
HAMPEL_HALF_WINDOW <- 3L    # rolling-median half-window (full window = 2*hw + 1)
HAMPEL_THRESHOLD   <- 3     # robust-z spike cutoff
WINSOR_MAD_K       <- 2.5   # robust-z cap for the pooled MAD winsor
WINSOR_MIN_OBS     <- 8L    # minimum pooled obs before a winsor band is built

# VA components, in a fixed order used for every per-component emission.
VA_COMPONENTS <- c("wages", "capital", "tls")

# Target sectors (exact label match in each base).
GLORIA_SECTOR   <- "Basic organic chemicals"
EXIOBASE_SECTOR <- "Chemicals nec"

# Raw upstream database trees (environment-overridable).
FINEPRINT_ROOT <- Sys.getenv("FINEPRINT_ROOT", unset = "/mnt/nfs_fineprint/tmp")
GLORIA_DIR     <- file.path(FINEPRINT_ROOT, "gloria", "v060-compiled")
EXIOBASE_DIR   <- file.path(FINEPRINT_ROOT, "exiobase", "v3.10")

GLORIA_README <- file.path(GLORIA_DIR, "labels", "GLORIA_ReadMe_060.xlsx")
GLORIA_V_DIR  <- file.path(GLORIA_DIR, "IOTs_producer_prices", "V")
GLORIA_X_DIR  <- file.path(GLORIA_DIR, "IOTs_producer_prices", "X")

# GLORIA value-added-and-final-demand row label -> VA component (the rest of that
# sheet is final demand and is excluded).  "Subsidies on production D.39" is
# signed in V, so summing it with taxes inside `tls` gives taxes − subsidies.
GLORIA_VA_LABEL_TO_COMPONENT <- c(
  "Compensation of employees D.1"    = "wages",
  "Net operating surplus B.2n"       = "capital",
  "Net mixed income B.3n"            = "capital",
  "Consumption of fixed capital K.1" = "capital",
  "Taxes on production D.29"         = "tls",
  "Subsidies on production D.39"     = "tls")

# EXIOBASE factor-input rows: row 1 lifts basic prices to producer prices (the
# intensity denominator, not VA); rows 2:9 partition into components.
EXIOBASE_LIFT_ROW    <- 1L
EXIOBASE_ROWS_BY_COMPONENT <- list(wages = 3:5, capital = 6:9, tls = 2L)

# Area concordances (region code -> FABIO area code).
GLORIA_AREA_CONC    <- "inst/value_added/concordance_areas_gloria_fabio.csv"
GLORIA_REGION_COL   <- "GLORIA_region_code"
EXIOBASE_AREA_CONC  <- "inst/value_added/concordance_areas_exiobase_fabio.csv"
EXIOBASE_REGION_COL <- "EXIOBASE_region"


# --- robust-statistics helpers ----------------------------------------------

scaled_mad <- function(x) stats::mad(x, constant = 1.4826, na.rm = TRUE)

skewness <- function(v) {
  n <- length(v); m <- mean(v); s <- stats::sd(v)
  if (!is.finite(s) || s < .Machine$double.eps * max(1, abs(m))) return(NA_real_)
  (n / ((n - 1) * (n - 2))) * sum(((v - m) / s)^3)
}

ex_kurtosis <- function(v) {
  n <- length(v); m <- mean(v); s <- stats::sd(v)
  if (!is.finite(s) || s < .Machine$double.eps * max(1, abs(m))) return(NA_real_)
  (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3)) * sum(((v - m) / s)^4) -
    3 * (n - 1)^2 / ((n - 2) * (n - 3))
}

# Per-series Hampel spike filter over year-ordered values: replace a point with
# the local rolling median when it exceeds threshold * series-MAD. Returns the
# filtered values plus the spike audit (window median, series MAD, robust z,
# spike flag). Short or degenerate series pass through unchanged.
hampel_series <- function(x) {
  n   <- length(x)
  fin <- is.finite(x)
  s   <- if (sum(fin) >= 2L * HAMPEL_HALF_WINDOW + 1L) scaled_mad(x[fin]) else NA_real_
  med <- vapply(seq_len(n), function(i) {
    w <- x[max(1L, i - HAMPEL_HALF_WINDOW):min(n, i + HAMPEL_HALF_WINDOW)]
    w <- w[is.finite(w)]
    if (length(w)) median(w) else NA_real_
  }, numeric(1))
  if (!is.finite(s) || s <= 0)
    return(list(values = x, window_median = med, series_mad = rep(s, n),
                hampel_z = rep(NA_real_, n), is_spike = rep(FALSE, n)))
  z     <- (x - med) / s
  spike <- is.finite(z) & abs(z) > HAMPEL_THRESHOLD
  list(values = fifelse(spike, med, x), window_median = med,
       series_mad = rep(s, n), hampel_z = z, is_spike = spike)
}

# Scale that makes asinh(x * theta) most Gaussian (minimises |skew| + |kurtosis|)
# over a fixed grid; NA signals a degenerate fit -> a raw-space cap is used.
ihs_theta <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < WINSOR_MIN_OBS) return(NA_real_)
  grid   <- 10^seq(-4, 12, by = 0.5)
  scores <- vapply(grid, function(th) {
    z <- asinh(x * th); abs(skewness(z)) + abs(ex_kurtosis(z))
  }, numeric(1))
  if (!any(is.finite(scores))) return(NA_real_)
  best <- which.min(scores); th <- grid[best]; obj <- scores[best]
  if (!is.finite(obj) || obj > 10 || th == min(grid) || th == max(grid)) NA_real_ else th
}

# Pooled MAD winsor at median +/- k * MAD, in IHS space when a theta is found,
# else in raw space. Returns the capped values plus the cap band and robust z.
mad_winsor <- function(x, theta, k = WINSOR_MAD_K) {
  use <- is.finite(theta)
  w   <- if (use) asinh(x * theta) else x
  med <- median(w, na.rm = TRUE)
  sc  <- scaled_mad(w)
  if (!is.finite(sc) || sc <= 0)
    return(list(values = x, cap_lower = NA_real_, cap_upper = NA_real_,
                mad_z = rep(NA_real_, length(x))))
  lo_w <- med - k * sc; hi_w <- med + k * sc
  lo   <- if (use) sinh(lo_w) / theta else lo_w
  hi   <- if (use) sinh(hi_w) / theta else hi_w
  list(values = pmin(pmax(x, lo), hi), cap_lower = lo, cap_upper = hi,
       mad_z = (w - med) / sc)
}

# region x year x component intensity cleaning: per-(region, component) Hampel
# over years, then one pooled winsor per component across all region-years. Both
# stages are kept as columns (va_intensity -> va_intensity_hampel ->
# va_intensity_winsor) with their audit fields, so the diagnostic can show each
# stage explicitly.
clean_intensity <- function(d) {
  setorder(d, va_component, region_code, year)
  if (!APPLY_INTENSITY_FILTER) {
    d[, `:=`(va_intensity_hampel = va_intensity,
             window_median = NA_real_, series_mad = NA_real_,
             hampel_z = NA_real_, is_spike = FALSE,
             va_intensity_winsor = va_intensity,
             cap_lower = NA_real_, cap_upper = NA_real_,
             mad_z = NA_real_, winsorized = FALSE)]
    return(d[])
  }
  d[, c("va_intensity_hampel", "window_median", "series_mad", "hampel_z", "is_spike") :=
      hampel_series(va_intensity), by = .(va_component, region_code)]
  d[, `:=`(va_intensity_winsor = NA_real_, cap_lower = NA_real_,
           cap_upper = NA_real_, mad_z = NA_real_)]
  for (comp in VA_COMPONENTS) {
    idx <- which(d$va_component == comp)
    if (!length(idx)) next
    hh <- d$va_intensity_hampel[idx]
    w  <- mad_winsor(hh, ihs_theta(hh))
    d[idx, `:=`(va_intensity_winsor = w$values, cap_lower = w$cap_lower,
                cap_upper = w$cap_upper, mad_z = w$mad_z)]
  }
  d[, winsorized := is.finite(va_intensity_hampel) & is.finite(va_intensity_winsor) &
      va_intensity_hampel != va_intensity_winsor]
  d[]
}


# --- intensity -> FABIO area (output-weighted) -------------------------------
# Aggregate the cleaned per-component intensity to FABIO areas by output-
# weighting, so several upstream regions mapping to one area combine as
# sum(VA) / sum(X) within each component. region_col is the concordance column
# holding the upstream region id.
intensity_to_area <- function(d, area_conc_path, region_col) {
  conc <- fread(area_conc_path, encoding = "UTF-8")
  conc <- unique(conc[, .(region_code     = as.character(get(region_col)),
                          fabio_area_code = as.integer(FABIO_area_code))])
  conc <- conc[!is.na(region_code) & region_code != "" & !is.na(fabio_area_code)]
  
  d <- merge(d, conc, by = "region_code", allow.cartesian = TRUE)
  area <- d[is.finite(va_intensity_winsor) & is.finite(x) & x > 0,
            .(va_num = sum(va_intensity_winsor * x), x_den = sum(x)),
            by = .(fabio_area_code, va_component, year)]
  area[, va_intensity := fifelse(x_den > 0, va_num / x_den, NA_real_)]
  area[, .(area_code = fabio_area_code, va_component, year, va_intensity)]
}


# --- GLORIA intensity: total VA / X for one sector, per region x year --------
gloria_intensity <- function(load_years) {
  for (pkg in c("readxl", "qs2"))
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("The GLORIA base needs the '", pkg, "' package.")
  
  regions <- as.data.table(readxl::read_excel(GLORIA_README, sheet = "Regions"))
  sectors <- as.data.table(readxl::read_excel(GLORIA_README, sheet = "Sectors"))
  setorder(sectors, Lfd_Nr)
  va_fd   <- as.data.table(readxl::read_excel(GLORIA_README,
                                              sheet = "Value added and final demand"))
  
  n_regions <- nrow(regions)
  n_sectors <- nrow(sectors)
  n_va      <- nrow(va_fd)
  
  sec_pos <- which(trimws(as.character(sectors$Sector_names)) == GLORIA_SECTOR)
  if (length(sec_pos) != 1L)
    stop("GLORIA sector '", GLORIA_SECTOR, "' not found (or not unique) in the ReadMe Sectors sheet.")
  
  # Identify the value-added rows: the ReadMe column carrying the most known VA
  # labels is the label column; assign each VA row to its component.
  known   <- names(GLORIA_VA_LABEL_TO_COMPONENT)
  hits    <- vapply(va_fd, function(col) sum(trimws(as.character(col)) %in% known), integer(1))
  labels  <- trimws(as.character(va_fd[[names(va_fd)[which.max(hits)]]]))
  row_comp <- unname(GLORIA_VA_LABEL_TO_COMPONENT[labels])   # NA = final demand
  rows_by_component <- lapply(VA_COMPONENTS, function(comp) which(row_comp == comp))
  names(rows_by_component) <- VA_COMPONENTS
  for (comp in VA_COMPONENTS)
    if (!length(rows_by_component[[comp]]))
      stop("GLORIA component '", comp, "' matched no VA-row label in the ReadMe sheet.")
  
  target_cols <- (seq_len(n_regions) - 1L) * n_sectors + sec_pos
  
  process_year <- function(yr) {
    v_path <- file.path(GLORIA_V_DIR, sprintf("V_%d.qs2", yr))
    x_path <- file.path(GLORIA_X_DIR, sprintf("X_%d.qs2", yr))
    if (!file.exists(v_path) || !file.exists(x_path)) return(NULL)
    V_mat <- as.matrix(qs2::qs_read(v_path))
    X_vec <- as.numeric(qs2::qs_read(x_path))
    stopifnot(nrow(V_mat) >= n_regions * n_va,
              ncol(V_mat) == n_regions * n_sectors,
              length(X_vec) == n_regions * n_sectors)
    X_vec[X_vec < 0] <- 0
    x  <- X_vec[target_cols]
    rbindlist(lapply(VA_COMPONENTS, function(comp) {
      sel <- rows_by_component[[comp]]
      va  <- vapply(seq_len(n_regions), function(r)
        sum(V_mat[(r - 1L) * n_va + sel, target_cols[r]]), numeric(1))
      data.table(region_code = as.character(regions$Region_acronyms),
                 va_component = comp, year = yr, va = va, x = x)
    }))
  }
  
  disk_years <- sort(as.integer(gsub("V_|\\.qs2", "",
                                     list.files(GLORIA_V_DIR, pattern = "^V_\\d+\\.qs2$"))))
  yrs <- intersect(disk_years, load_years)
  if (length(yrs) == 0L) stop("No GLORIA V matrices on disk for the requested years.")
  
  d <- rbindlist(lapply(yrs, process_year))
  d[, va_intensity := fifelse(x > 0, va / x, NA_real_)]
  message(sprintf("  GLORIA: %d region-year-components, %d with finite intensity.",
                  nrow(d), d[is.finite(va_intensity), .N]))
  d[]
}


# --- EXIOBASE intensity: total VA / producer-price X for one sector ----------
exiobase_intensity <- function(load_years) {
  common <- as.data.table(readRDS(file.path(EXIOBASE_DIR, "IOT_2022_ixi", "unit.rds")))
  region_col <- names(common)[grep("^reg",        tolower(names(common)))[1]]
  sector_col <- names(common)[grep("^(sec|prod)", tolower(names(common)))[1]]
  if (is.na(region_col) || is.na(sector_col))
    stop("Could not locate region / sector columns in the EXIOBASE common unit labels.")
  
  labels <- data.table(col         = seq_len(nrow(common)),
                       region_code = as.character(common[[region_col]]),
                       sector_name = as.character(common[[sector_col]]))
  target <- labels[sector_name == EXIOBASE_SECTOR]
  if (nrow(target) == 0L)
    stop("EXIOBASE sector '", EXIOBASE_SECTOR, "' not found in the common unit labels.")
  
  max_va_row <- max(unlist(EXIOBASE_ROWS_BY_COMPONENT, use.names = FALSE))
  
  process_year <- function(yr) {
    x_path <- file.path(EXIOBASE_DIR, sprintf("IOT_%d_ixi", yr), "x.rds")
    f_path <- file.path(EXIOBASE_DIR, sprintf("IOT_%d_ixi", yr), "factor_inputs", "F.rds")
    if (!file.exists(x_path) || !file.exists(f_path)) return(NULL)
    
    X_raw <- readRDS(x_path)
    if (is.data.frame(X_raw)) {
      vcol  <- grep("^(indout|value|x)$", tolower(names(X_raw)))
      if (!length(vcol)) vcol <- which(vapply(X_raw, is.numeric, logical(1)))
      X_vec <- as.numeric(X_raw[[vcol[length(vcol)]]])
    } else X_vec <- as.numeric(X_raw)
    F_mat <- as.matrix(readRDS(f_path))
    if (!is.numeric(F_mat)) storage.mode(F_mat) <- "double"
    if (nrow(F_mat) < max_va_row)
      stop("EXIOBASE F for year ", yr, " has fewer than ", max_va_row, " factor rows.")
    
    cols <- target$col
    x_pp <- X_vec[cols] + F_mat[EXIOBASE_LIFT_ROW, cols]
    x_pp[x_pp < 0] <- 0
    rbindlist(lapply(VA_COMPONENTS, function(comp) {
      va <- colSums(F_mat[EXIOBASE_ROWS_BY_COMPONENT[[comp]], cols, drop = FALSE])
      data.table(region_code = target$region_code,
                 va_component = comp, year = yr, va = va, x = x_pp)
    }))
  }
  
  year_dirs  <- list.dirs(EXIOBASE_DIR, recursive = FALSE)
  disk_years <- sort(as.integer(sub(".*IOT_(\\d{4})_ixi.*", "\\1",
                                    grep("IOT_\\d{4}_ixi", year_dirs, value = TRUE))))
  yrs <- intersect(disk_years, load_years)
  if (length(yrs) == 0L) stop("No EXIOBASE year folders on disk for the requested years.")
  
  d <- rbindlist(lapply(yrs, process_year))
  d[, va_intensity := fifelse(x > 0, va / x, NA_real_)]
  message(sprintf("  EXIOBASE: %d region-year-components, %d with finite intensity.",
                  nrow(d), d[is.finite(va_intensity), .N]))
  d[]
}


# --- build both area-level intensities ---------------------------------------
# When cleaning is on, load a Hampel buffer of years for edge context, then keep
# only the model years after filtering.
buffer_years <- c(seq(min(keep_years) - HAMPEL_HALF_WINDOW, min(keep_years) - 1L),
                  seq(max(keep_years) + 1L, max(keep_years) + HAMPEL_HALF_WINDOW))
load_years   <- if (APPLY_INTENSITY_FILTER) sort(union(keep_years, buffer_years)) else keep_years

message("Building GLORIA intensity ('", GLORIA_SECTOR, "') ...")
gloria_reg  <- clean_intensity(gloria_intensity(load_years))[year %in% keep_years]
gloria_area <- intensity_to_area(gloria_reg, GLORIA_AREA_CONC, GLORIA_REGION_COL)

message("Building EXIOBASE intensity ('", EXIOBASE_SECTOR, "') ...")
exiobase_reg  <- clean_intensity(exiobase_intensity(load_years))[year %in% keep_years]
exiobase_area <- intensity_to_area(exiobase_reg, EXIOBASE_AREA_CONC, EXIOBASE_REGION_COL)


# --- producer values -> value added ------------------------------------------
pv <- as.data.table(readRDS(tag("intermediate_data/bcp_producer_total_values.rds")))
pv[, year := as.integer(year)]

bases <- c("gloria", "exiobase")

# Attach the per-component area intensities for one base as
# va_intensity_{wages,capital,tls}_<base>.
attach_intensities <- function(pv, area_long, base) {
  aw <- dcast(area_long, area_code + year ~ va_component, value.var = "va_intensity")
  for (comp in VA_COMPONENTS) if (!comp %in% names(aw)) aw[, (comp) := NA_real_]
  setnames(aw, VA_COMPONENTS, sprintf("va_intensity_%s_%s", VA_COMPONENTS, base))
  merge(pv, aw, by = c("area_code", "year"), all.x = TRUE, sort = FALSE)
}
pv <- attach_intensities(pv, gloria_area,   "gloria")
pv <- attach_intensities(pv, exiobase_area, "exiobase")

# Per-component VA = intensity x producer value; aggregate total + aggregate
# intensity are the row-wise sums of the three components (NA if any missing).
for (base in bases) {
  for (comp in VA_COMPONENTS) {
    icol <- sprintf("va_intensity_%s_%s", comp, base)
    vcol <- sprintf("value_added_%s_%s [USD]", comp, base)
    pv[, (vcol) := fifelse(is.finite(get(icol)) & is.finite(total_value),
                           get(icol) * total_value, NA_real_)]
  }
  vcols <- sprintf("value_added_%s_%s [USD]", VA_COMPONENTS, base)
  icols <- sprintf("va_intensity_%s_%s",       VA_COMPONENTS, base)
  pv[, (sprintf("value_added_%s [USD]", base)) :=
       get(vcols[1]) + get(vcols[2]) + get(vcols[3])]
  pv[, (sprintf("va_intensity_%s", base)) :=
       get(icols[1]) + get(icols[2]) + get(icols[3])]
}

base_block <- function(b) c(
  sprintf("va_intensity_%s", b),
  sprintf("va_intensity_%s_%s", VA_COMPONENTS, b),
  sprintf("value_added_%s_%s [USD]", VA_COMPONENTS, b),
  sprintf("value_added_%s [USD]", b))
keep_cols <- c("iso3c", "area_code", "comm_code", "item", "year",
               "total_product_output", "unit", "total_value",
               base_block("gloria"), base_block("exiobase"))
out <- pv[, ..keep_cols]
setorder(out, iso3c, comm_code, year)

# Invariant: the three components sum to the aggregate total in every cell.
for (base in bases) {
  vcols <- sprintf("value_added_%s_%s [USD]", VA_COMPONENTS, base)
  tcol  <- sprintf("value_added_%s [USD]", base)
  s <- out[[vcols[1]]] + out[[vcols[2]]] + out[[vcols[3]]]
  t <- out[[tcol]]
  ok <- !is.finite(t) | (is.finite(s) & abs(s - t) <= 1e-6 * pmax(1, abs(t)))
  if (!all(ok))
    stop(sprintf("31: %s components do not sum to the total in %d valued cell(s).",
                 base, sum(!ok)))
}


# --- diagnostics -------------------------------------------------------------
# Per-base intensity audit (region x year, raw vs cleaned + winsor flag), the
# coverage funnel, and the producer-value areas that found no intensity.
DIAG_DIR <- "intermediate_data/value_added_diagnostics"
dir.create(DIAG_DIR, recursive = TRUE, showWarnings = FALSE)
diag_path <- function(name) file.path(DIAG_DIR, paste0(name, mode_tag, ".csv"))

write_intensity_diag <- function(reg, base) {
  dg <- reg[, .(region_code, va_component, year, va, x,
                va_intensity, va_intensity_hampel,
                window_median, series_mad, hampel_z, is_spike,
                va_intensity_winsor, cap_lower, cap_upper, mad_z, winsorized)]
  setorder(dg, va_component, region_code, year)
  fwrite(dg, diag_path(sprintf("bcp_value_added_intensity_%s", base)))
}
write_intensity_diag(gloria_reg,   "gloria")
write_intensity_diag(exiobase_reg, "exiobase")

coverage <- data.table(
  base            = bases,
  area_years      = c(nrow(gloria_area[is.finite(va_intensity)]),
                      nrow(exiobase_area[is.finite(va_intensity)])),
  rows_valued     = c(out[is.finite(`value_added_gloria [USD]`),   .N],
                      out[is.finite(`value_added_exiobase [USD]`), .N]),
  rows_total      = nrow(out),
  value_added_sum = c(out[, sum(`value_added_gloria [USD]`,   na.rm = TRUE)],
                      out[, sum(`value_added_exiobase [USD]`, na.rm = TRUE)]))
print(coverage)
fwrite(coverage, diag_path("bcp_value_added_coverage"))

# Per-component VA sums, so the wages/capital/tls split is auditable per base.
component_sums <- rbindlist(lapply(bases, function(b)
  data.table(base = b, va_component = VA_COMPONENTS,
             value_added_sum = vapply(VA_COMPONENTS, function(comp)
               out[, sum(get(sprintf("value_added_%s_%s [USD]", comp, b)), na.rm = TRUE)],
               numeric(1)))))
print(component_sums)
fwrite(component_sums, diag_path("bcp_value_added_component_sums"))

pv_areas  <- unique(pv[, .(iso3c, area_code)])
unmatched <- rbindlist(list(
  data.table(base = "gloria",   pv_areas[!area_code %in% gloria_area$area_code]),
  data.table(base = "exiobase", pv_areas[!area_code %in% exiobase_area$area_code])))
setorder(unmatched, base, iso3c)
fwrite(unmatched, diag_path("bcp_value_added_unmatched_areas"))


# --- output ------------------------------------------------------------------
rds_path <- tag("intermediate_data/bcp_value_added_MRIOTs.rds")
csv_path <- sub("\\.csv$", paste0(mode_tag, ".csv"), "intermediate_data/bcp_value_added_MRIOTs.csv")

saveRDS(out, rds_path)
fwrite(out, csv_path)

message(sprintf("31_bcp_value_added_MRIOTs: %d rows -> %s (gloria valued %d, exiobase valued %d).",
                nrow(out), rds_path,
                out[is.finite(`value_added_gloria [USD]`),   .N],
                out[is.finite(`value_added_exiobase [USD]`), .N]))