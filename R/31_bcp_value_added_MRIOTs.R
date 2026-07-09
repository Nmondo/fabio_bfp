# =============================================================================
# 31_bcp_value_added_MRIOTs.R
# Value added for the bcp commodities, estimated by multiplying each commodity's
# total producer value by a value-added intensity (VA / producer-price output)
# borrowed from two upstream MRIO databases (GLORIA and EXIOBASE). Sectors are
# selected by their canonical ordinal code, never by display name: GLORIA by
# Lfd_Nr, EXIOBASE by ixi industry position. Each commodity draws from its own
# sector — c141-c145 from their agricultural/processing sectors (BCP_AG_SECTORS),
# every other commodity (c146+) from the default chemical sector (GLORIA "Basic
# organic chemicals" / EXIOBASE "Chemicals nec").
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

# Default target sector, selected by the database's canonical sector ordinal
# (never by display name): GLORIA = Lfd_Nr (1..120 in v060); EXIOBASE = the ixi
# industry position (1..163). This default is used for every commodity not named
# in BCP_AG_SECTORS below (i.e. the bio-based commodities c146+).
GLORIA_SECTOR_CODE   <- 68L   # Basic organic chemicals  (default, c146+)
EXIOBASE_SECTOR_CODE <- 63L   # Chemicals nec            (default, c146+)

# Per-commodity sector override, by code. Commodities c141-c145 draw their VA
# intensity from their own agricultural/processing sector rather than the
# chemical default. Any comm_code absent here falls back to the default codes.
BCP_AG_SECTORS <- data.table(
  comm_code     = c("c141", "c142", "c143", "c144", "c145", "c171"),
  gloria_code   = c(3L, 51L, 4L, 53L, 53L, 50L),   # cereals nec; sugar refining; leguminous/oilseeds; veg oils&fats; veg oils&fats; food products & feeds n.e.c.
  exiobase_code = c(3L, 42L, 5L, 39L, 39L, 43L))   # cereal grains nec; sugar refining; oil seeds; proc. veg oils&fats; proc. veg oils&fats; processing of food products nec

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


# --- GLORIA intensity: total VA / X per sector, per region x year ------------
# Builds the raw VA/X intensity for each requested GLORIA sector code (Lfd_Nr),
# tagging every region x year x component row with its `sector_code`.
gloria_intensity <- function(load_years, sector_codes) {
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
  
  # Locate each requested sector by its canonical ordinal (Lfd_Nr), never by
  # display name. Fail loud (naming the code) if a code is not a unique Lfd_Nr,
  # so no commodity is silently dropped.
  sector_codes <- sort(unique(as.integer(sector_codes)))
  sec_pos_by_code <- vapply(sector_codes, function(code) {
    pos <- which(sectors$Lfd_Nr == code)
    if (length(pos) != 1L)
      stop("GLORIA sector code ", code, " is not a unique Lfd_Nr in the ReadMe Sectors sheet.")
    pos
  }, integer(1))
  names(sec_pos_by_code) <- as.character(sector_codes)
  
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
  
  # One region-indexed column vector per requested sector.
  target_cols_by_code <- lapply(sec_pos_by_code,
                                function(sp) (seq_len(n_regions) - 1L) * n_sectors + sp)
  
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
    rbindlist(lapply(seq_along(sector_codes), function(k) {
      code        <- sector_codes[k]
      target_cols <- target_cols_by_code[[k]]
      x <- X_vec[target_cols]
      rbindlist(lapply(VA_COMPONENTS, function(comp) {
        sel <- rows_by_component[[comp]]
        va  <- vapply(seq_len(n_regions), function(r)
          sum(V_mat[(r - 1L) * n_va + sel, target_cols[r]]), numeric(1))
        data.table(region_code = as.character(regions$Region_acronyms),
                   sector_code = code, va_component = comp, year = yr, va = va, x = x)
      }))
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


# --- EXIOBASE intensity: total VA / producer-price X per sector --------------
# Builds the raw VA/X intensity for each requested EXIOBASE ixi sector code,
# tagging every region x year x component row with its `sector_code`.
exiobase_intensity <- function(load_years, sector_codes) {
  common <- as.data.table(readRDS(file.path(EXIOBASE_DIR, "IOT_2022_ixi", "unit.rds")))
  region_col <- names(common)[grep("^reg", tolower(names(common)))[1]]
  if (is.na(region_col))
    stop("Could not locate the region column in the EXIOBASE common unit labels.")
  
  # The label table is in canonical (region-major, ixi-sector) order, so a
  # sector's ixi code is its position within each region block. Select sectors
  # by that position (rowid within region), never by display name.
  labels <- data.table(col         = seq_len(nrow(common)),
                       region_code = as.character(common[[region_col]]))
  labels[, sec_idx := rowid(region_code)]
  n_reg              <- uniqueN(labels$region_code)
  sectors_per_region <- max(labels$sec_idx)
  
  # Fail loud (naming the code) if a code is outside 1..sectors_per_region or
  # does not select exactly one industry per region, so no commodity is dropped.
  sector_codes <- sort(unique(as.integer(sector_codes)))
  bad <- sector_codes[sector_codes < 1L | sector_codes > sectors_per_region]
  if (length(bad))
    stop("EXIOBASE sector code(s) ", paste(bad, collapse = ", "),
         " outside 1..", sectors_per_region, " (sectors per region).")
  targets_by_code <- lapply(sector_codes, function(code) {
    tgt <- labels[sec_idx == code]
    if (nrow(tgt) != n_reg)
      stop("EXIOBASE sector code ", code, " does not select exactly one industry per region.")
    tgt
  })
  names(targets_by_code) <- as.character(sector_codes)
  
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
    
    rbindlist(lapply(seq_along(sector_codes), function(k) {
      code <- sector_codes[k]
      cols <- targets_by_code[[k]]$col
      x_pp <- X_vec[cols] + F_mat[EXIOBASE_LIFT_ROW, cols]
      x_pp[x_pp < 0] <- 0
      rbindlist(lapply(VA_COMPONENTS, function(comp) {
        va <- colSums(F_mat[EXIOBASE_ROWS_BY_COMPONENT[[comp]], cols, drop = FALSE])
        data.table(region_code = targets_by_code[[k]]$region_code,
                   sector_code = code, va_component = comp, year = yr, va = va, x = x_pp)
      }))
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

# The distinct set of sector codes actually needed per base: the chemical
# default plus every code named in BCP_AG_SECTORS.
gloria_codes   <- sort(unique(c(GLORIA_SECTOR_CODE,   BCP_AG_SECTORS$gloria_code)))
exiobase_codes <- sort(unique(c(EXIOBASE_SECTOR_CODE, BCP_AG_SECTORS$exiobase_code)))

# Clean (Hampel + winsor) and map-to-area SEPARATELY per sector code -- so the
# per-series cleaning never pools different sectors -- then stack the results,
# each region/area row tagged with its sector_code.
build_base_intensity <- function(raw, area_conc, region_col, codes) {
  regs  <- vector("list", length(codes))
  areas <- vector("list", length(codes))
  for (i in seq_along(codes)) {
    code <- codes[i]
    reg  <- clean_intensity(raw[sector_code == code])[year %in% keep_years]
    reg[, sector_code := code]
    area <- intensity_to_area(reg, area_conc, region_col)
    area[, sector_code := code]
    regs[[i]]  <- reg
    areas[[i]] <- area
  }
  list(reg = rbindlist(regs), area = rbindlist(areas))
}

message("Building GLORIA intensities for sector codes: ",
        paste(gloria_codes, collapse = ", "), " ...")
gloria_built <- build_base_intensity(gloria_intensity(load_years, gloria_codes),
                                     GLORIA_AREA_CONC, GLORIA_REGION_COL, gloria_codes)
gloria_reg  <- gloria_built$reg
gloria_area <- gloria_built$area

message("Building EXIOBASE intensities for sector codes: ",
        paste(exiobase_codes, collapse = ", "), " ...")
exiobase_built <- build_base_intensity(exiobase_intensity(load_years, exiobase_codes),
                                       EXIOBASE_AREA_CONC, EXIOBASE_REGION_COL, exiobase_codes)
exiobase_reg  <- exiobase_built$reg
exiobase_area <- exiobase_built$area


# --- producer values -> value added ------------------------------------------
pv <- as.data.table(readRDS(tag("intermediate_data/bcp_producer_total_values.rds")))
pv[, year := as.integer(year)]

bases <- c("gloria", "exiobase")

# Per-row sector code for each base: the BCP_AG_SECTORS override where the
# commodity is listed (c141-c145), the chemical default otherwise (c146+).
pv[BCP_AG_SECTORS, `:=`(gloria_code = i.gloria_code, exiobase_code = i.exiobase_code),
   on = "comm_code"]
pv[is.na(gloria_code),   gloria_code   := GLORIA_SECTOR_CODE]
pv[is.na(exiobase_code), exiobase_code := EXIOBASE_SECTOR_CODE]

# Attach the per-component area intensities for one base as
# va_intensity_{wages,capital,tls}_<base>, matching each producer-value row to
# its own area, year AND the sector code its comm_code resolves to.
attach_intensities <- function(pv, area_long, base, code_col) {
  aw <- dcast(area_long, area_code + year + sector_code ~ va_component, value.var = "va_intensity")
  for (comp in VA_COMPONENTS) if (!comp %in% names(aw)) aw[, (comp) := NA_real_]
  setnames(aw, VA_COMPONENTS, sprintf("va_intensity_%s_%s", VA_COMPONENTS, base))
  merge(pv, aw,
        by.x = c("area_code", "year", code_col),
        by.y = c("area_code", "year", "sector_code"),
        all.x = TRUE, sort = FALSE)
}
pv <- attach_intensities(pv, gloria_area,   "gloria",   "gloria_code")
pv <- attach_intensities(pv, exiobase_area, "exiobase", "exiobase_code")

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
  dg <- reg[, .(sector_code, region_code, va_component, year, va, x,
                va_intensity, va_intensity_hampel,
                window_median, series_mad, hampel_z, is_spike,
                va_intensity_winsor, cap_lower, cap_upper, mad_z, winsorized)]
  setorder(dg, sector_code, va_component, region_code, year)
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