# =============================================================================
# 41_responsibility_hdi.R
# Environmentally-just (HDI-weighted) responsibility sharing between producers
# and consumers (Sun et al. 2022, Ecol. Econ. 194:107339, operationalising
# Oliveira 2019) for the bio-based transport fuels, using the IBIF biodiversity
# impact as the environmental extension.
#
# WHAT THIS COMPUTES ----------------------------------------------------------
# The consumer footprint of each biofuel chain, resolved bilaterally into
# D[p, c] = impact occurring in producing country p, driven by final demand in
# consuming country c (same object as 18_01's fp_trade_breakdown_feedstock(
# bilateral = TRUE), aggregated over feedstock and commodity):
#     D = T' diag(f) B Y_g ,   f = e / x ,  B = (I - A)^-1 = L_<allocation>
# Every bilateral flow is then SPLIT between its two agents by their relative
# HDI (Sun eqs. 1-3, with C = P = HDI):
#     producer p keeps   HDI_p / (HDI_p + HDI_c)      (beta)
#     consumer c keeps   HDI_c / (HDI_p + HDI_c)      (alpha)
# The two shares sum to one, so domestic flows (p == c) return in full and the
# global footprint is conserved: production-, consumption- and justice-based
# accounts are three re-attributions of one and the same number. A country's
# justice-based footprint therefore decomposes into
#     justice = domestic + export share (as producer) + import share (as consumer)
# and imbalance = justice - mean(production, consumption) is Sun's Fig. 2 / 4:
# positive where a high-HDI country shoulders more than the naive 50/50 split.
#
# BIOFUEL SCOPE (comm_codes, per 07_03a/07_03b/33 and 18_01's bf_set) ---------
#     biogasoline       c146
#     biodiesel         c147
#     renewable_diesel  c149
#
# INPUTS ----------------------------------------------------------------------
#   <MRIO>/losses/X.rds                  total output, columns = years
#   <MRIO>/losses/Y.rds                  year-keyed final-demand matrices
#   <MRIO>/losses/<yr>_L_<allocation>.rds  year-keyed Leontief inverse (14)
#   <MRIO>/losses/fd_labels.csv          consuming region per Y column (13)
#   <base>/E.rds                         stressor x sector extensions (16)
#   <base>/io_labels.csv                 row/col labels of the grid (12_b)
#   input/value_added/HDR25_Composite_indices_complete_time_series.csv
#                                        UNDP HDR composite indices, wide
#                                        (hdi_1990..hdi_2023); downloaded from
#                                        HDI_URL if absent
#   inst/regions_full.csv                continent, for the HDI fallback
#
# HDI COVERAGE -- the one judgement call --------------------------------------
# The weighting grid is (every FABIO country) x (every model year), i.e. the
# MRIO's `years`, NOT the HDR's 1990..2023: an HDI outside the model years has
# no D[p, c] to weight. The HDR's other years are still read, because they are
# the donor pool the `nearest_year` rung draws on.
#
# The HDR does not resolve every FABIO region, so HDI is filled by an explicit
# ladder whose choice is recorded per row in `hdi_source`:
#   undp             year-matched HDR value                       -- the ~95% case
#   nearest_year     nearest year of the SAME country (gaps, edge years); still a
#                    real UNDP number, only off by a year or two
#   row_residual     ROW: the median HDI of every HDR country the model does NOT
#                    resolve separately -- i.e. exactly the countries ROW lumps
#                    together (unweighted; the HDR file carries no population)
#   continent_median other uncovered iso3c (small territories and the handful of
#                    economies the HDR omits outright), by continent
# The console reports the year-matched share first and the fallbacks as its
# complement, broken down per source and per country-year -- a bare fallback
# count reads like a hit rate, and a country list hides that one country with a
# short HDR history (SOM: no entry before 2022) can dominate a whole rung.
#
# `imputed_hdi_exposure` in the coverage file is the share of each chain's
# footprint whose bilateral split leans on any hdi_source != "undp". It is
# deliberately conservative: an off-by-one-year UNDP value counts against it
# exactly like a continent median. Read it before trusting a country's
# imbalance, and if it is ever large enough to report, split it into
# off-year vs synthetic rather than calling all of it "imputed".
#
# OUTPUTS ---------------------------------------------------------------------
# Named like 40's, so no two settings of the run switches ever overwrite:
#   <metric> = STAG from STRESSOR      "ibif_total" -> "ibif",
#                                      "LCIM_EQ_terrestrial" -> "lcim_eq_terrestrial"
#   <alloc>  = ATAG from `allocation`  "mass" | "value" (the co-product rule of B)
#
#   <OUT_DIR>/FABIO_bcp_<metric>_hdi_responsibility_<alloc>.csv
#       long: year, biofuel_group, iso3c, continent, hdi, hdi_source,
#             production_based, consumption_based, avg_prod_cons, justice_based,
#             justice_domestic, justice_export, justice_import, imbalance
#   <OUT_DIR>/FABIO_bcp_<metric>_hdi_coverage_<alloc>.csv
#       per (year, biofuel_group): footprint, justice_total,
#       conservation_gap_pct, imputed_hdi_exposure
#
# RUN -------------------------------------------------------------------------
#   Rscript R/41_responsibility_hdi.R
#   FABIO_RUN_MODE=bypass Rscript R/41_responsibility_hdi.R   # counterfactual
#   (must run AFTER 14 and 16)
# =============================================================================

# --- portable repo root: FABIO_BFP_ROOT override, else walk up to the marker -
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
library(Matrix)

source("R/00_system_variables.R")   # years, output_dir_bcp
source("R/00_run_config.R")         # RUN_MODE / mode_dir()

# --- run config --------------------------------------------------------------
model_version <- if (tolower(trimws(Sys.getenv("FABIO_RUN_MODE", "rescaled"))) == "bypass")
  "bypass" else "rescaled"

base_path <- sub("/+$", "", output_dir_bcp)                     # version-invariant (E, io_labels)
MRIO_PATH <- if (model_version == "bypass") file.path(base_path, "bypass") else base_path
OUT_DIR   <- if (model_version == "bypass") "output/bypass" else "output"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# --- run switches ------------------------------------------------------------
tag <- function(x, fallback) {
  s <- gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", tolower(x)))
  if (nzchar(s)) s else fallback
}

allocation <- "value"                # co-product rule of the Leontief inverse: "mass" | "value"
STRESSOR   <- "LCIM_EQ_terrestrial"  # "ibif_total" | "LCIM_EQ_terrestrial"
resp_years <- as.character(years)    # 2012:2022 from 00_system_variables

HDI_FILE  <- "input/value_added/HDR25_Composite_indices_complete_time_series.csv"
HDI_URL   <- "https://hdr.undp.org/sites/default/files/2025_HDR/HDR25_Composite_indices_complete_time_series.csv"
ROW_CODES <- "ROW"                   # FABIO regions that lump countries the model does not resolve

ATAG <- tag(allocation, "alloc")                     # "mass" / "value"
STAG <- tag(sub("_total$", "", STRESSOR), "metric")  # "ibif" / "lcim_eq_terrestrial"

# every CSV this script writes goes through here: one naming convention, one place
out_csv <- function(what)
  file.path(OUT_DIR, sprintf("FABIO_bcp_%s_%s_%s.csv", STAG, what, ATAG))

biofuel_groups <- list(
  biogasoline      = "c146",
  biodiesel        = "c147",
  renewable_diesel = "c149"                       # c149 only; HVO co-products
  # biopropane/bio-LPG (c150) and
  # bionaphtha (c151) are excluded
)

message(sprintf(">>> [41] model_version='%s' | allocation='%s' | stressor='%s'",
                model_version, allocation, STRESSOR))

# --- inputs ------------------------------------------------------------------
need <- function(path, who) {
  if (!file.exists(path)) stop("Missing input: ", path, "  (produced by ", who, ")")
  path
}
X  <- readRDS(need(file.path(MRIO_PATH, "losses", "X.rds"), "14_leontief_inverse.R / 13_mrio.R"))
Y  <- readRDS(need(file.path(MRIO_PATH, "losses", "Y.rds"), "13_mrio.R"))
E  <- readRDS(need(file.path(base_path, "E.rds"),           "16_extensions_main.R"))
io <- fread(   need(file.path(base_path, "io_labels.csv"),  "12_b_update_labels.R"))
fd <- fread(   need(file.path(MRIO_PATH, "losses", "fd_labels.csv"), "13_mrio.R"))
regions <- fread("inst/regions_full.csv")[current == TRUE]

stopifnot(all(c("iso3c", "comm_code", "item") %in% names(io)), "iso3c" %in% names(fd))
N <- nrow(io)

# --- alignment guard: E columns and X rows MUST match the io grid ------------
# The math is positional (f = e/X, then B, T and Y are indexed by the same grid),
# so a re-ordered artefact would silently attribute impacts to the wrong item.
io_key <- paste0(io$iso3c, "_", io$comm_code)
assert_grid <- function(obj, axis, who) {
  nm <- if (axis == "row") rownames(obj) else colnames(obj)
  if (is.null(nm)) {
    warning(sprintf("[41] %s has no %snames -- cannot verify item alignment; trusting position.",
                    who, axis)); return(invisible())
  }
  if (!identical(nm, io_key)) {
    i <- which(nm != io_key)[1]
    stop(sprintf(paste0("[41] %s %s order != io_labels grid -- the extension would be ",
                        "misattributed to the wrong items.\n",
                        "     first mismatch at index %d: %s = '%s' vs io = '%s'.\n",
                        "     Re-run 16 (E) against THIS io_labels.csv."),
                 who, axis, i, who, nm[i], io_key[i]))
  }
  invisible()
}
assert_grid(X, "row", "X.rds")
for (yr in intersect(as.character(years), names(E))) assert_grid(E[[yr]], "col", sprintf("E[[%s]]", yr))
message(">>> [41] alignment guard passed: X rows and E columns match the io_labels grid.")

# --- country grid ------------------------------------------------------------
# One country universe for both axes of D: producers (io rows) and consumers
# (Y columns). T_origin rolls the io grid up to producing countries, S_fd rolls
# the final-demand columns up to consuming countries.
countries <- sort(unique(c(io$iso3c, fd$iso3c)))
R <- length(countries)
T_origin <- sparseMatrix(i = seq_len(N),        j = match(io$iso3c, countries), x = 1,
                         dims = c(N, R))
S_fd     <- sparseMatrix(i = seq_along(fd$iso3c), j = match(fd$iso3c, countries), x = 1,
                         dims = c(length(fd$iso3c), R))

# --- HDI weights -------------------------------------------------------------
read_hdr <- function(file, url) {
  if (!file.exists(file)) {
    dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
    message(">>> [41] HDI file not found, downloading: ", url)
    ok <- tryCatch(download.file(url, file, method = "auto", quiet = TRUE) == 0,
                   error = function(e) FALSE, warning = function(w) FALSE)
    if (!ok) stop("HDI download failed -- put the HDR composite-indices CSV at ", file)
  }
  fread(file, encoding = "Latin-1")
}

# (iso3c, year) -> hdi, with the provenance of every value in `hdi_source`:
# undp -> nearest_year -> row_residual (ROW) -> continent_median.
hdi_weights <- function(iso, yrs, regions) {
  raw  <- read_hdr(HDI_FILE, HDI_URL)
  cols <- grep("^hdi_[0-9]{4}$", names(raw), value = TRUE)
  if (!length(cols)) stop("No hdi_<year> columns in ", HDI_FILE)
  
  long <- melt(raw[, c("iso3", cols), with = FALSE], id.vars = "iso3",
               variable.name = "year", value.name = "hdi")
  long[, year := as.integer(sub("^hdi_", "", as.character(year)))]
  long[, hdi  := suppressWarnings(as.numeric(hdi))]
  long <- long[nchar(iso3) == 3 & is.finite(hdi) & hdi > 0]   # drop the HDR aggregate rows
  
  # ROW lumps together every country the model does not resolve separately: its
  # HDI is the median over exactly those HDR countries (unweighted -- the HDR
  # file carries no population), computed per year.
  residual <- long[!iso3 %in% iso, .(hdi_row = median(hdi)), by = year]
  message(sprintf(">>> [41] ROW (%s) = median HDI of %d HDR countries not resolved by FABIO",
                  paste(ROW_CODES, collapse = ", "), uniqueN(long[!iso3 %in% iso, iso3])))
  
  near <- function(dt, y) if (!nrow(dt)) NA_real_ else dt$hdi[which.min(abs(dt$year - y))]
  
  w <- CJ(iso3c = iso, year = as.integer(yrs))
  w[long, on = .(iso3c = iso3, year), hdi := i.hdi]
  w[, hdi_source := fifelse(is.finite(hdi), "undp", NA_character_)]
  
  i <- which(is.na(w$hdi_source))                  # country-year gaps / edge years
  if (length(i)) {
    v <- vapply(i, function(k) near(long[iso3 == w$iso3c[k]], w$year[k]), numeric(1))
    w[i, hdi_source := fifelse(is.finite(v), "nearest_year", NA_character_)]
    w[i, hdi := v]
  }
  
  w[residual, on = "year", hdi_row := i.hdi_row]
  i <- which(!is.finite(w$hdi) & w$iso3c %in% ROW_CODES & is.finite(w$hdi_row))
  if (length(i)) w[i, `:=`(hdi = hdi_row, hdi_source = "row_residual")]
  
  w[, continent := regions$continent[match(iso3c, regions$iso3c)]]
  cm <- w[is.finite(hdi) & hdi_source != "row_residual",
          .(hdi_cont = median(hdi)), by = .(continent, year)]
  w[cm, on = .(continent, year), hdi_cont := i.hdi_cont]
  i <- which(!is.finite(w$hdi) & is.finite(w$hdi_cont))   # small territories outside the HDR
  if (length(i)) w[i, `:=`(hdi = hdi_cont, hdi_source = "continent_median")]
  
  i <- which(!is.finite(w$hdi))                    # anything still uncovered: the residual again
  if (length(i)) w[i, `:=`(hdi = hdi_row, hdi_source = "row_residual")]
  w[, c("hdi_row", "hdi_cont") := NULL]
  
  if (!all(is.finite(w$hdi) & w$hdi > 0))
    stop("No HDI and no fallback for: ",
         paste(unique(w[!is.finite(hdi), iso3c]), collapse = ", "))
  # Report the MATCH RATE first and the fallbacks as its complement: printing
  # only the fallback count invites reading it as the hit rate. The breakdown is
  # per source AND per country-year, because the three rungs are not equally
  # weak -- `nearest_year` is a real UNDP value for the right country, off by a
  # year or two; only `continent_median` / `row_residual` are genuinely
  # synthetic. Country counts alone would hide that, e.g. SOM (no HDR entry
  # before 2022) carries nearly the whole `nearest_year` bill on its own.
  n_ok <- sum(w$hdi_source == "undp")
  message(sprintf(">>> [41] HDI: %d/%d country-years year-matched to UNDP (%.1f%%)",
                  n_ok, nrow(w), 100 * n_ok / nrow(w)))
  imp <- w[hdi_source != "undp"]
  if (nrow(imp)) {
    brk <- imp[, .N, by = .(hdi_source, iso3c)][order(hdi_source, -N)][
      , .(n = sum(N), who = paste(sprintf("%s(%d)", iso3c, N), collapse = "/")),
      by = hdi_source][order(-n)]
    message(sprintf(">>> [41]      %d filled by fallback:", nrow(imp)))
    for (k in seq_len(nrow(brk)))
      message(sprintf(">>> [41]        %-16s %3d  %s", brk$hdi_source[k], brk$n[k], brk$who[k]))
  }
  w[]
}

hdi <- hdi_weights(countries, resp_years, regions)

# --- per-year worker ---------------------------------------------------------
# Returns list(resp = long dt by country, cover = 1 row per biofuel group).
compute_year <- function(yr) {
  if (!yr %in% colnames(X)) { warning("year ", yr, " absent from X"); return(NULL) }
  if (is.null(Y[[yr]]) || is.null(E[[yr]])) { warning("year ", yr, " absent from Y/E"); return(NULL) }
  
  L_path <- file.path(MRIO_PATH, "losses", paste0(yr, "_L_", allocation, ".rds"))
  if (!file.exists(L_path)) { warning("no Leontief for ", yr, ": ", L_path); return(NULL) }
  B <- readRDS(L_path)                                   # N x N Leontief inverse
  
  Xi <- as.vector(X[, yr]);  stopifnot(length(Xi) == N)
  Ei <- E[[yr]];             stopifnot(ncol(Ei) == N)
  Yi <- Y[[yr]];             stopifnot(nrow(Yi) == N, ncol(Yi) == length(fd$iso3c))
  if (!STRESSOR %in% rownames(Ei)) stop("Stressor '", STRESSOR, "' not in E[[", yr, "]]")
  
  f <- as.numeric(Ei[STRESSOR, ]) / Xi; f[!is.finite(f)] <- 0   # intensity, as in 18_01
  Yc <- Yi %*% S_fd                                             # N x R: final demand per consumer
  
  # HDI weights of THIS year, on the country grid. Wp[p, c] is the producer's
  # share of a flow from p to c (Sun's beta); the consumer keeps 1 - Wp.
  h  <- hdi[year == as.integer(yr)]
  hv <- h$hdi[match(countries, h$iso3c)]
  Wp <- outer(hv, hv, function(p, c) p / (p + c))
  imputed <- h$hdi_source[match(countries, h$iso3c)] != "undp"
  pair_imputed <- outer(imputed, imputed, `|`)
  
  comm <- io$comm_code
  resp_rows <- list(); cover_rows <- list()
  
  for (g in names(biofuel_groups)) {
    sel <- as.numeric(comm %in% biofuel_groups[[g]])   # biofuel-group final demand only
    Yg  <- Diagonal(x = sel) %*% Yc
    if (sum(Yg) == 0) next
    
    # D[p, c]: impact in producing country p driven by final demand in consumer c
    D  <- as.matrix(crossprod(T_origin, Diagonal(x = f) %*% (B %*% Yg)))
    fp <- sum(D)
    if (fp == 0) next
    
    prod_v <- rowSums(D)                              # production-based
    cons_v <- colSums(D)                              # consumption-based
    dom_v  <- diag(D)                                 # domestic (p == c), split 50/50 -> returns whole
    exp_v  <- rowSums(D * Wp)       - dom_v * diag(Wp)        # producer share of exports
    imp_v  <- colSums(D * (1 - Wp)) - dom_v * (1 - diag(Wp))  # consumer share of imports
    just_v <- dom_v + exp_v + imp_v                   # F^just (Sun eq. 1)
    
    keep <- which(prod_v != 0 | cons_v != 0)
    if (length(keep)) resp_rows[[g]] <- data.table(
      year              = as.integer(yr),
      biofuel_group     = g,
      iso3c             = countries[keep],
      hdi               = hv[keep],
      hdi_source        = h$hdi_source[match(countries[keep], h$iso3c)],
      production_based  = prod_v[keep],
      consumption_based = cons_v[keep],
      avg_prod_cons     = 0.5 * (prod_v[keep] + cons_v[keep]),
      justice_based     = just_v[keep],
      justice_domestic  = dom_v[keep],
      justice_export    = exp_v[keep],
      justice_import    = imp_v[keep]
    )
    
    cover_rows[[g]] <- data.table(
      year                 = as.integer(yr),
      biofuel_group        = g,
      footprint            = fp,                      # the chain's consumer footprint
      justice_total        = sum(just_v),             # must equal it: the split conserves
      conservation_gap_pct = 100 * (sum(just_v) - fp) / fp,
      imputed_hdi_exposure = sum(D * pair_imputed) / fp   # share of flows split on a non-year-matched HDI
    )
  }
  
  list(resp  = rbindlist(resp_rows,  use.names = TRUE, fill = TRUE),
       cover = rbindlist(cover_rows, use.names = TRUE, fill = TRUE))
}

# --- run all years -----------------------------------------------------------
res   <- lapply(resp_years, function(yr) { message("  year ", yr); compute_year(yr) })
res   <- Filter(Negate(is.null), res)
resp  <- rbindlist(lapply(res, `[[`, "resp"),  use.names = TRUE, fill = TRUE)
cover <- rbindlist(lapply(res, `[[`, "cover"), use.names = TRUE, fill = TRUE)

resp[, imbalance := justice_based - avg_prod_cons]   # Sun Fig. 2 / Fig. 4
resp[, continent := regions$continent[match(iso3c, regions$iso3c)]]
setcolorder(resp, c("year", "biofuel_group", "iso3c", "continent", "hdi", "hdi_source",
                    "production_based", "consumption_based", "avg_prod_cons",
                    "justice_based", "justice_domestic", "justice_export",
                    "justice_import", "imbalance"))

# --- validation / console summary -------------------------------------------
cat("\n================  HDI-weighted (environmentally just) responsibility  ================\n")
cat(sprintf("stressor: %-24s allocation: %-5s  years: %s-%s\n",
            STRESSOR, allocation, min(resp$year), max(resp$year)))
if (nrow(cover)) {
  print(cover[, .(footprint            = sum(footprint),
                  justice_total        = sum(justice_total),
                  conservation_gap_pct = 100 * (sum(justice_total) - sum(footprint)) / sum(footprint),
                  imputed_hdi_exposure = weighted.mean(imputed_hdi_exposure, footprint)),
              by = biofuel_group])
  cat("\n - conservation_gap_pct must be ~0: the HDI split re-attributes, never creates.\n")
  cat(" - imputed_hdi_exposure is the share of the footprint split on a non-year-matched\n")
  cat("   HDI (nearest_year, continent_median or row_residual) -- see hdi_source.\n")
}
cat("\n-- largest shifts vs the 50/50 production/consumption average (all chains) --\n")
shift <- resp[, .(hdi           = mean(hdi),
                  avg_prod_cons = sum(avg_prod_cons),
                  justice_based = sum(justice_based),
                  imbalance     = sum(imbalance)), by = .(iso3c, continent)]
print(head(shift[order(-abs(imbalance))], 15))

# --- save --------------------------------------------------------------------
r_path <- out_csv("hdi_responsibility")
c_path <- out_csv("hdi_coverage")
fwrite(resp[order(year, biofuel_group, -justice_based)], r_path)
fwrite(cover[order(year, biofuel_group)], c_path)
message(sprintf("Wrote %s (%d rows)\nWrote %s (%d rows)",
                r_path, nrow(resp), c_path, nrow(cover)))