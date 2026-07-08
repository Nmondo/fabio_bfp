# =============================================================================
# 40_responsibility_value_added.R
# Value-added-based responsibility allocation (Piñero et al. 2019, Econ. Syst.
# Res. 31:2) for the bio-based transport fuels, using the IBIF biodiversity
# impact as the environmental extension.
#
# WHAT THIS COMPUTES ----------------------------------------------------------
# For each biofuel "chain" (final product) we take the supply-chain-wide IBIF
# pressure embodied in that biofuel's final demand -- the standard FABIO
# consumer/upstream footprint, identical in construction to 18_01's
# `ext = E/X ; MP = ext * L` -- and RE-ALLOCATE it to every value-generating
# sector along the chain in proportion to the value added it captures. This is
# the value-added-based metric h = v_hat B t of Piñero et al., where
#     f  = e / x                     IBIF intensity per unit output   (from E, X)
#     B  = (I - A)^-1                Leontief inverse                 (L_value)
#     t_j = y_j (f' B)_j             consumer footprint of final product j
#     h_i = v_i sum_j B_ij t_j       value-added responsibility of sector i
#     v  = p / x                     value-added share per unit output (from V)
#
# BIOFUEL SCOPE (comm_codes, per 07_03a/07_03b/33 and 18_01's bf_set) ---------
#     biogasoline       c146
#     biodiesel         c147
#     renewable_diesel  c149, c150, c151   (Neste renewable-diesel bundle)
#
# INPUTS  (same artefacts the footprint pipeline uses) ------------------------
#   <MRIO>/losses/X.rds                total output, columns = years
#   <MRIO>/losses/Y.rds                year-keyed final-demand matrices
#   <MRIO>/losses/<yr>_L_value.rds     year-keyed Leontief inverse (14)
#   <base>/E.rds                       stressor x sector extensions (16)
#   <base>/V.rds                       VA (USD) x sector, 2 rows gloria/exiobase (35)
#   <base>/io_labels.csv               row/col labels for the grid (iso3c, comm_code, item)
#
# OUTPUTS ---------------------------------------------------------------------
#   <OUT_DIR>/FABIO_bcp_ibif_value_added_responsibility_<base>.csv
#       long: year, biofuel_group, va_iso3c, va_comm_code, va_item, ibif_va_resp
#             (the normalised value-added responsibility -- the deliverable)
#   <OUT_DIR>/FABIO_bcp_ibif_value_added_coverage_<base>.csv
#       per (year, biofuel_group): ibif_consumer_footprint, ibif_allocated_norm,
#       conservation_gap_pct, implied_unit_value_usd_per_t, throughput_coverage.
#       throughput_coverage is the HONESTY CHECK -- read the CAVEAT block.
#
# CAVEAT -- FABIO is a PHYSICAL table; V is the only monetary object -----------
# Z, X and Y are in MASS (13_mrio builds Z_value = mr_use[mass] %*% value-based
# co-product shares -- "value" is the allocation rule, not the unit). So B is a
# physical Leontief inverse (tonnes of i per tonne of final j), and the two
# intensities have DIFFERENT dimensions:
#     f = e/X  -> impact per tonne          (ordinary physical footprint)
#     v = p/X  -> USD per tonne             (a value/PRICE intensity, NOT a share)
# There is no monetary output vector in the model, so v does not form a
# dimensionless value-added share and sum_i v_i B_ij is NOT 1. Instead
#     s_j = (v' B)_j = USD value added embodied per tonne of final product j
#         = the implied UNIT VALUE (price) of j.
# The value added captured by sector i in chain j is v_i B_ij y_j (USD), so the
# value-added SHARE of i is v_i B_ij / s_j (dimensionless). The physically
# correct value-added responsibility is therefore the NORMALISED allocation
#     h_i = v_i * sum_j B_ij (t_j / s_j),
# which distributes each biofuel's physical IBIF footprint by value-added share
# and conserves it: sum_i h_i = sum_{j: s_j>0} t_j. This is the default and the
# only allocation shipped in the responsibility CSV. (The un-normalised product
# v_i * sum_j B_ij t_j mixes units -- impact x price -- and is NOT a
# responsibility; NORMALISE=FALSE returns it for debugging only.)
#
# COVERAGE. Because v_i = 0 wherever V is zero-filled (35), those sectors get
# zero value-added share and the normalisation redistributes each chain's
# footprint among only the *valued* sectors. The true value-added denominator of
# the uncovered part is unknown, so we report an honest PHYSICAL proxy instead:
#     throughput_coverage = share of the chain's upstream tonnage (B y_g) that
#                           passes through sectors with V > 0.
# Low throughput_coverage => the split leans on few valued sectors; treat those
# biofuel-years with caution. implied_unit_value_usd_per_t (= sum_j s_j y_j /
# sum_j y_j) is a sanity check: does the biofuel's implied USD/tonne look real?
#
# RUN -------------------------------------------------------------------------
#   Rscript R/40_responsibility_value_added.R
#   FABIO_RUN_MODE=bypass Rscript R/40_responsibility_value_added.R   # counterfactual
#   (must run AFTER 14 and 35; VA_BASE below selects gloria/exiobase)
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

base_path <- sub("/+$", "", output_dir_bcp)                      # version-invariant (E, V, io_labels)
MRIO_PATH <- if (model_version == "bypass") file.path(base_path, "bypass") else base_path
OUT_DIR   <- if (model_version == "bypass") "output/bypass" else "output"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

allocation <- "value"                # income/value work is on the monetary table
STRESSOR   <- "ibif_total"           # IBIF aggregate biodiversity impact (15_6)
VA_BASE    <- "exiobase"             # "gloria" | "exiobase"
NORMALISE  <- TRUE                   # per-chain renormalisation (see CAVEAT)
resp_years <- as.character(years)    # 2012:2022 from 00_system_variables

biofuel_groups <- list(
  biogasoline      = "c146",
  biodiesel        = "c147",
  renewable_diesel = c("c149", "c150", "c151")
)

message(sprintf(">>> [40] model_version='%s' | VA_BASE='%s' | stressor='%s' | normalise=%s",
                model_version, VA_BASE, STRESSOR, NORMALISE))

# --- static inputs (year-independent) ----------------------------------------
need <- function(path, who) {
  if (!file.exists(path)) stop("Missing input: ", path, "  (produced by ", who, ")")
  path
}
X  <- readRDS(need(file.path(MRIO_PATH, "losses", "X.rds"),   "14_leontief_inverse.R / 13_mrio.R"))
Y  <- readRDS(need(file.path(MRIO_PATH, "losses", "Y.rds"),   "13_mrio.R"))
E  <- readRDS(need(file.path(base_path, "E.rds"),             "16_extensions_main.R"))
V  <- readRDS(need(file.path(base_path, "V.rds"),             "35_bcp_value_added_extension.R"))
io <- fread(   need(file.path(base_path, "io_labels.csv"),    "12_b_update_labels.R"))

stopifnot(all(c("iso3c", "comm_code", "item") %in% names(io)))
N <- nrow(io)

# Pick the VA row from V (rows are value_added_gloria / value_added_exiobase)
va_row_for <- function(Vi) {
  pick <- switch(VA_BASE,
                 gloria   = "value_added_gloria",
                 exiobase = "value_added_exiobase",
                 stop("VA_BASE must be gloria/exiobase"))
  if (!pick %in% rownames(Vi)) stop("V.rds has no row '", pick, "'")
  as.numeric(Vi[pick, ])
}

# --- per-year worker ---------------------------------------------------------
# Returns list(alloc = long dt of h by value generator, cover = 1-row diagnostics)
compute_year <- function(yr) {
  if (!yr %in% colnames(X))         { warning("year ", yr, " absent from X");  return(NULL) }
  if (is.null(Y[[yr]]))             { warning("year ", yr, " absent from Y");  return(NULL) }
  if (is.null(E[[yr]]) || is.null(V[[yr]])) { warning("year ", yr, " absent from E/V"); return(NULL) }
  
  L_path <- file.path(MRIO_PATH, "losses", paste0(yr, "_L_", allocation, ".rds"))
  if (!file.exists(L_path))         { warning("no Leontief for ", yr, ": ", L_path); return(NULL) }
  B <- readRDS(L_path)                                   # N x N Leontief inverse
  
  Xi <- as.vector(X[, yr]);           stopifnot(length(Xi) == N)
  Ei <- E[[yr]];                      stopifnot(ncol(Ei) == N)
  Vi <- V[[yr]];                      stopifnot(ncol(Vi) == N)
  Yi <- Y[[yr]];                      stopifnot(nrow(Yi) == N)
  if (!STRESSOR %in% rownames(Ei)) stop("Stressor '", STRESSOR, "' not in E[[", yr, "]]")
  
  # intensity f = e / x  (matches 18_01)             ; VA share v = p / x
  f <- as.numeric(Ei[STRESSOR, ]) / Xi; f[!is.finite(f)] <- 0
  p <- va_row_for(Vi)
  v <- p / Xi;                          v[!is.finite(v)] <- 0
  
  # year-level row vectors over final products j
  fB <- as.vector(crossprod(f, B))      # (f' B)_j : IBIF impact embodied per tonne of final demand j
  s  <- as.vector(crossprod(v, B))      # (v' B)_j : USD value added per tonne of j = implied UNIT VALUE
  #            (NOT a share; physical table -- see header CAVEAT)
  
  # world final demand by product node (sum over all consuming fd columns)
  y_all <- as.vector(rowSums(as.matrix(Yi)))
  comm  <- io$comm_code
  
  alloc_rows <- list(); cover_rows <- list()
  
  for (g in names(biofuel_groups)) {
    cc_set <- biofuel_groups[[g]]
    in_g   <- comm %in% cc_set
    
    y_g <- numeric(N); y_g[in_g] <- y_all[in_g]         # biofuel-group final demand only (tonnes)
    if (sum(y_g) == 0) next
    
    t_j         <- y_g * fB                             # IBIF footprint by final product j
    fp_consumer <- sum(t_j)                             # = f' B y_g  (upstream/consumer footprint)
    if (fp_consumer == 0) next
    
    # value-added responsibility = physical footprint distributed by VA share.
    # share of i in chain j = v_i B_ij / s_j  ->  h_i = v_i sum_j B_ij (t_j / s_j).
    # (NORMALISE=FALSE returns the unit-mixed v_i sum_j B_ij t_j for debugging only.)
    t_use <- if (NORMALISE) ifelse(s > 0, t_j / s, 0) else t_j
    h     <- v * as.vector(B %*% t_use)
    allocated_norm <- sum(h)                            # ~ fp_consumer for chains with s_j > 0
    
    # --- honest diagnostics for a physical table ------------------------------
    # implied unit value of the biofuel group (USD/tonne): sanity check on V.
    implied_unit_value <- sum(s * y_g) / sum(y_g)
    # throughput coverage: share of upstream tonnage from sectors we can value.
    BY <- as.vector(B %*% y_g)                          # upstream throughput (tonnes)
    throughput_coverage <- if (sum(BY) > 0) sum(BY[v > 0]) / sum(BY) else NA_real_
    
    keep <- which(h != 0)
    if (length(keep)) alloc_rows[[g]] <- data.table(
      year          = as.integer(yr),
      biofuel_group = g,
      va_iso3c      = io$iso3c[keep],
      va_comm_code  = io$comm_code[keep],
      va_item       = io$item[keep],
      ibif_va_resp  = h[keep]                           # normalised VA responsibility (the deliverable)
    )
    
    cover_rows[[g]] <- data.table(
      year                     = as.integer(yr),
      biofuel_group            = g,
      ibif_consumer_footprint  = fp_consumer,           # what the biofuel supply chain causes (upstream)
      ibif_allocated_norm      = allocated_norm,        # normalised total (should match consumer fp)
      conservation_gap_pct     = 100 * (allocated_norm - fp_consumer) / fp_consumer,
      implied_unit_value_usd_per_t = implied_unit_value,# sanity check on V (USD/tonne)
      throughput_coverage      = throughput_coverage    # in [0,1] -- honesty check (see CAVEAT)
    )
  }
  
  list(alloc = rbindlist(alloc_rows, use.names = TRUE, fill = TRUE),
       cover = rbindlist(cover_rows, use.names = TRUE, fill = TRUE))
}

# --- run all years -----------------------------------------------------------
res   <- lapply(resp_years, function(yr) { message("  year ", yr); compute_year(yr) })
res   <- Filter(Negate(is.null), res)
alloc <- rbindlist(lapply(res, `[[`, "alloc"), use.names = TRUE, fill = TRUE)
cover <- rbindlist(lapply(res, `[[`, "cover"), use.names = TRUE, fill = TRUE)

# annotate value-generator continent (nice-to-have, mirrors 18_01)
regions <- tryCatch(fread("inst/regions_full.csv"), error = function(e) NULL)
if (!is.null(regions) && "continent" %in% names(regions))
  alloc[, va_continent := regions$continent[match(va_iso3c, regions$iso3c)]]

# --- validation / console summary -------------------------------------------
cat("\n================  IBIF value-added responsibility  ================\n")
cat(sprintf("VA base: %-8s   normalise: %s\n", VA_BASE, NORMALISE))
if (nrow(cover)) {
  chk <- cover[, .(consumer   = sum(ibif_consumer_footprint),
                   norm       = sum(ibif_allocated_norm),
                   unit_value = weighted.mean(implied_unit_value_usd_per_t, ibif_consumer_footprint),
                   throughput_cov = weighted.mean(throughput_coverage, ibif_consumer_footprint)),
               by = biofuel_group]
  chk[, conservation_gap_pct := 100 * (norm - consumer) / consumer]
  print(chk)
  cat("\nInterpretation (physical table -- see CAVEAT):\n")
  cat(" - `norm` should match `consumer` (per-chain conservation of the footprint);\n")
  cat("   a gap means some chains have s_j = 0, i.e. NO valued sector to carry them.\n")
  cat(" - `unit_value` is the implied USD/tonne of the biofuel -- a sanity check on V.\n")
  cat(" - `throughput_cov` in [0,1] is the share of upstream tonnage from valued\n")
  cat("   sectors; low values mean the split leans on few imputed sectors.\n")
}

# --- save --------------------------------------------------------------------
alloc_path <- file.path(OUT_DIR, sprintf("FABIO_bcp_ibif_value_added_responsibility_%s.csv", VA_BASE))
cover_path <- file.path(OUT_DIR, sprintf("FABIO_bcp_ibif_value_added_coverage_%s.csv",       VA_BASE))
fwrite(alloc, alloc_path)
fwrite(cover, cover_path)
message(sprintf("\nWrote %s (%d rows)\nWrote %s (%d rows)",
                alloc_path, nrow(alloc), cover_path, nrow(cover)))