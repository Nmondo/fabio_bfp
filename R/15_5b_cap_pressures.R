# 15_5b - Outlier capping of environmental PRESSURES (before characterization)
# ==============================================================================
# Runs AFTER 15_1..15_5 (raw pressures written to data/extensions/{sua,cbs}/) and
# BEFORE 15_6..15_9 (characterized impacts = pressures x CFs). It winsorizes
# implausibly high pressure INTENSITIES so that the characterized impacts and the
# compiled E inherit capped pressures automatically.
#
# METHOD (see 15_5b_pressure_capping_method.md for the full justification):
#   * Detect on log-intensity  g = log(e) - log(x)  = log(pressure/output),
#     i.e. the exact quantity f_i = e_i / x_i that enters the footprint model.
#   * Reference distribution: pooled over ALL countries and ALL years, per
#     (comm_code, indicator); production-weighted by output x; small-x floor so
#     negligible producers neither define nor absorb the correction.
#   * Robust threshold (Hampel / Leys et al. 2013): weighted median + K*weighted-MAD
#     on the log scale (default), or a production-weighted upper quantile.
#   * Winsorize (never delete): e_capped = exp(hi) * x, keeping x. Upper tail only
#     (pressures are non-negative). Correction is multiplicative: shrink = e_cap/e.
#
# INPUTS  (drawn exactly like temp_quantiles.R / 18_01b):
#   X <- losses/X.rds          (output / production vector, io-row x year)
#   E <- E.rds                 (compiled extensions; contains BOTH pressures AND
#                               the 15_6..15_9 characterized impacts -> we FILTER
#                               to pressure rows by name)
#
# OUTPUTS:
#   * output/FABIO_pressure_capping_audit_<alloc>_<yrs>.csv   (per-cell: orig vs capped)
#   * output/FABIO_pressure_capping_summary_<alloc>_<yrs>.csv (per comm x indicator)
#   * output/FABIO_pressure_capping_shrink_<alloc>_<yrs>.rds  (full shrink table)
#   * capped pressure files rewritten in data/extensions/{sua,cbs}/  (originals
#     backed up once to data/extensions/{sua,cbs}_uncapped_backup/)
#
# AFTER THIS SCRIPT: re-run 15_6..15_9 then 16 so impacts + E reflect the caps.
#
# NOTE on years: the cap is estimated only where output exists, i.e. on the
#   intersection of the E and X year sets (2012-2022 with the current MRIO). The
#   extension files keep their full native span (2011-2023); cells in years with
#   no output (2011, 2023) are left untouched -- see the .md, section 3/6.
# ==============================================================================

library(data.table)
library(Matrix)
library(tidyverse)

## --- portable repo root (same pattern as 16 / 18_01b) -------------------------
fabio_root <- Sys.getenv("FABIO_BFP_ROOT", unset = "")
if (!nzchar(fabio_root)) {
  fabio_root <- getwd()
  while (!file.exists(file.path(fabio_root, "R", "00_system_variables.R")) &&
         dirname(fabio_root) != fabio_root) fabio_root <- dirname(fabio_root)
  if (!file.exists(file.path(fabio_root, "R", "00_system_variables.R")))
    stop("Repo root not found above ", getwd(), " - set FABIO_BFP_ROOT.")
}
setwd(fabio_root)

source("R/00_system_variables.R")   # NB: defines years <- 2012:2022 (we DON'T rely on it)
source("R/01_tidy_functions.R")     # unformat_extension(), format_extension()
source("R/00_prep_functions.R")

# ---- MODEL VERSION (mirror 18_01b) -------------------------------------------
model_version <- Sys.getenv("FABIO_RUN_MODE", unset = "rescaled")
model_version <- if (tolower(trimws(model_version)) == "bypass") "bypass" else "rescaled"

base_path <- "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/"
MRIO_PATH <- if (model_version == "bypass")
  paste0(sub("/+$", "", base_path), "/bypass/") else base_path
OUT_DIR   <- if (model_version == "bypass") "output/bypass" else "output"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
message(sprintf(">>> [15_5b capping] model_version = '%s'  (MRIO: %s | out: %s)",
                model_version, MRIO_PATH, OUT_DIR))

# ==============================================================================
# 0. CONFIG
# ==============================================================================
ALLOC       <- "value"

METHOD      <- "mad"     # "mad" (Hampel, robust; default) or "quantile"
K           <- 5         # MAD multiplier on the log scale (generous; see .md)
P_HI        <- 0.995     # upper quantile if METHOD == "quantile"
X_FLOOR_Q   <- 0.02      # per-group output floor: rows below this output quantile
# do NOT set the reference distribution (still get capped)
MIN_N       <- 30        # min non-zero obs in a (comm_code,indicator) group to cap
WEIGHTED    <- TRUE      # production-weight the reference distribution by output x

WRITE_FILES <- TRUE      # FALSE = audit-only dry run (nothing on disk is changed)
EXT_DIRS    <- c("sua", "cbs")   # final-demand (fd_*) extensions are NOT capped here

# Rows in E that are CHARACTERIZED IMPACTS, not pressures -> excluded by prefix.
IMPACT_PREFIXES <- c("ES_", "FD_", "LCIM_", "ibif_")
# 'biomass' is the production/output measure itself (intensity ~ 1 by construction).
NON_PRESSURE_EXACT <- c("biomass")

# ==============================================================================
# 1. LOAD LABELS + DATA  (output drawn like temp_quantiles.R)
# ==============================================================================
regions    <- fread("inst/regions_full.csv")[current == TRUE]
io         <- fread(paste0(MRIO_PATH, "io_labels.csv"))
items_sua  <- fread("inst/sua/items_sua.csv")
items_cbs  <- fread("inst/items_full_123.csv")

X <- readRDS(paste0(MRIO_PATH, "losses/X.rds"))   # io-row x year
E <- readRDS(paste0(base_path,  "E.rds"))          # stressors x (iso3c_comm) per year

YEARS <- sort(as.integer(intersect(names(E), colnames(X))))
message("Years with both E and X (used to estimate caps): ",
        paste(range(YEARS), collapse = "-"))

# ---- resolve the pressure set from the mixed rownames -------------------------
all_rows      <- rownames(E[[as.character(YEARS[1])]])
is_impact     <- Reduce(`|`, lapply(IMPACT_PREFIXES, function(p) startsWith(all_rows, p)))
PRESSURE_NMS  <- setdiff(all_rows[!is_impact], NON_PRESSURE_EXACT)

message("Resolved ", length(PRESSURE_NMS), " pressure rows (of ",
        length(all_rows), " total). Excluded ", sum(is_impact),
        " impact rows + ", length(NON_PRESSURE_EXACT), " non-pressure.")
message("Pressure set: ", paste(PRESSURE_NMS, collapse = ", "))

# ==============================================================================
# 2. HELPERS
# ==============================================================================
# Weighted quantile (Hazen), base R, no extra dependencies.
w_quantile <- function(x, w, probs) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (!length(x)) return(rep(NA_real_, length(probs)))
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w) - 0.5 * w
  cw <- cw / sum(w)
  stats::approx(cw, x, xout = probs, rule = 2, ties = "ordered")$y
}

# Upper threshold on the log-intensity g for one (comm_code, indicator) group.
group_cap <- function(g, x) {
  n <- length(g)
  if (n < MIN_N) return(Inf)                      # too few obs to judge -> no cap
  xf   <- stats::quantile(x, X_FLOOR_Q, na.rm = TRUE, names = FALSE)
  keep <- x >= xf & is.finite(g)
  if (sum(keep) < MIN_N) keep <- is.finite(g)     # fallback if floor too aggressive
  gg <- g[keep]
  ww <- if (WEIGHTED) x[keep] else rep(1, sum(keep))
  if (METHOD == "quantile") {
    return(w_quantile(gg, ww, P_HI))
  }
  med  <- w_quantile(gg, ww, 0.5)
  madv <- 1.4826 * w_quantile(abs(gg - med), ww, 0.5)
  med + K * madv
}

# ==============================================================================
# 3. BUILD LONG (pressure, output) TABLE  (compute_intensity_table, restricted)
# ==============================================================================
# X[i, yr] and E[[yr]][ext, ] are both positionally aligned to io rows.
intensity_dt <- rbindlist(lapply(YEARS, function(yr) {
  Xi <- as.vector(X[, as.character(yr)])
  Ey <- E[[as.character(yr)]]
  rbindlist(lapply(PRESSURE_NMS, function(ext) {
    if (!ext %in% rownames(Ey)) return(NULL)
    e_i  <- as.numeric(Ey[ext, ])
    keep <- e_i != 0                              # only non-zero pressure cells
    if (!any(keep)) return(NULL)
    data.table(
      iso3c     = io$iso3c[keep],
      comm_code = io$comm_code[keep],
      item      = io$item[keep],
      year      = yr,
      indicator = ext,
      output    = Xi[keep],
      stressor  = e_i[keep]
    )
  }), use.names = TRUE)
}), use.names = TRUE)

# Intensity is only defined (and only matters to the model) where output > 0:
# 18_01b sets f_i = e_i/x_i to 0 whenever x_i == 0, so zero-output pressure cells
# contribute nothing to footprints and need no cap. Drop them.
intensity_dt <- intensity_dt[output > 0 & stressor > 0]
intensity_dt[, g := log(stressor) - log(output)]           # log-intensity
intensity_dt[, allocation := ALLOC]

# ==============================================================================
# 4. GROUP CAPS + WINSORIZE
# ==============================================================================
caps <- intensity_dt[, .(hi = group_cap(g, output), n = .N),
                     by = .(comm_code, indicator)]

intensity_dt <- merge(intensity_dt, caps, by = c("comm_code", "indicator"))
intensity_dt[, capped := is.finite(hi) & g > hi]
intensity_dt[, stressor_capped := fifelse(capped, exp(hi) * output, stressor)]
intensity_dt[, shrink := fifelse(capped, stressor_capped / stressor, 1)]

n_cap <- intensity_dt[capped == TRUE, .N]
message(sprintf("Flagged %d / %d non-zero pressure cells for capping (%.3f%%).",
                n_cap, nrow(intensity_dt), 100 * n_cap / max(nrow(intensity_dt), 1)))

# ==============================================================================
# 5. AUDIT + SUMMARY EXPORTS
# ==============================================================================
tag <- paste0(ALLOC, "_", min(YEARS), "-", max(YEARS))

audit <- intensity_dt[capped == TRUE, .(
  year, iso3c, comm_code, item, indicator, allocation,
  output,
  stressor_orig       = stressor,
  stressor_capped,
  intensity_orig      = stressor / output,
  intensity_capped    = stressor_capped / output,      # == exp(hi)
  threshold_log       = hi,
  threshold_intensity = exp(hi),
  shrink,
  delta_abs           = stressor_capped - stressor,    # negative
  delta_rel           = shrink - 1,
  group_n             = n
)][order(indicator, comm_code, -abs(delta_abs))]

fwrite(audit, file.path(OUT_DIR, paste0("FABIO_pressure_capping_audit_", tag, ".csv")))
message("Wrote audit (", nrow(audit), " capped cells).")

summary_dt <- intensity_dt[, .(
  n              = .N,
  n_capped       = sum(capped),
  share_capped   = mean(capped),
  threshold_int  = exp(hi[1]),
  max_int_before = max(stressor / output),
  max_int_after  = max(stressor_capped / output),
  med_int        = median(stressor / output)
), by = .(comm_code, indicator)][order(-share_capped)]
fwrite(summary_dt, file.path(OUT_DIR, paste0("FABIO_pressure_capping_summary_", tag, ".csv")))

shrink_tab <- intensity_dt[capped == TRUE, .(indicator, iso3c, comm_code,
                                             year = as.integer(year), shrink)]
saveRDS(shrink_tab, file.path(OUT_DIR, paste0("FABIO_pressure_capping_shrink_", tag, ".rds")))

# ---- diagnostic: does capping touch known "consistent extremes"? -------------
# (report only -- NOT a hardcoded exception). Inspect before trusting the caps.
defo <- audit[grepl("deforest|luc_forest", indicator) & iso3c == "IDN"]
message("\n--- Diagnostic: Indonesia deforestation/LUC cells that got capped ---")
if (nrow(defo)) print(defo[, .(year, comm_code, item, indicator,
                               intensity_orig, threshold_intensity, shrink)]) else
                                 message("  (none capped -- consistent extreme preserved)")
message("If a genuine extreme is capped, raise K / P_HI globally (do not hardcode).\n")

# ==============================================================================
# 6. PROPAGATE CAPS TO THE ON-DISK PRESSURE FILES
# ==============================================================================
# Multiplicative shrink applied to each pressure file, matched on
# (iso3c, comm_code, year). Non-matching keys keep shrink = 1 (untouched); the
# per-file match report surfaces any comm_code-convention mismatch between the
# compiled E (BCP) and the sua/cbs files. Round-trip: unformat_extension(long) ->
# scale value -> format_extension (which needs only area_code, comm_code, year,
# value). yrs is taken from the file itself so its native span (2011-2023) is kept.
if (WRITE_FILES && nrow(shrink_tab) > 0) {
  
  itms_for <- list(sua = items_sua, cbs = items_cbs)
  
  for (dir_nm in EXT_DIRS) {
    ext_path    <- file.path("data/extensions", dir_nm)
    backup_path <- file.path("data/extensions", paste0(dir_nm, "_uncapped_backup"))
    dir.create(backup_path, showWarnings = FALSE, recursive = TRUE)
    itms_dir <- itms_for[[dir_nm]]
    
    # only touch files for indicators that actually have >=1 capped cell
    inds_here <- intersect(unique(shrink_tab$indicator),
                           gsub("\\.rds$", "", list.files(ext_path, pattern = "\\.rds$")))
    
    for (ind in inds_here) {
      fpath <- file.path(ext_path, paste0(ind, ".rds"))
      orig  <- readRDS(fpath)
      
      lng <- unformat_extension(setNames(list(orig), ind), ind, long = TRUE)
      setDT(lng); lng[, year := as.integer(year)]
      
      sh  <- shrink_tab[indicator == ind, .(iso3c, comm_code, year, shrink)]
      lng <- merge(lng, sh, by = c("iso3c", "comm_code", "year"), all.x = TRUE)
      matched  <- sum(!is.na(lng$shrink)); expected <- nrow(sh)
      lng[is.na(shrink), shrink := 1]
      lng[, value := value * shrink]
      lng[, area_code := regions$code[match(iso3c, regions$iso3c)]]
      
      file_years <- sort(unique(lng$year))
      tab <- lng[, .(year, area_code, comm_code, value)]
      formatted <- format_extension(tab, yrs = file_years, reg = regions, itms = itms_dir)
      
      # back up original once, then overwrite
      bpath <- file.path(backup_path, paste0(ind, ".rds"))
      if (!file.exists(bpath)) file.copy(fpath, bpath)
      saveRDS(formatted, fpath)
      
      message(sprintf("  [%s] %-28s %d/%d expected caps matched; years %d-%d.",
                      dir_nm, ind, matched, expected, min(file_years), max(file_years)))
    }
  }
  message("\nDone. Now RE-RUN 15_6..15_9 then 16 to rebuild impacts + E on capped pressures.")
} else if (!WRITE_FILES) {
  message("\nDRY RUN (WRITE_FILES = FALSE): audit written, no extension files changed.")
} else {
  message("\nNothing to cap under the current thresholds; no files changed.")
}