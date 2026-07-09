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
#     renewable_diesel  c149
#
# INPUTS  (same artefacts the footprint pipeline uses) ------------------------
#   <MRIO>/losses/X.rds                total output, columns = years
#   <MRIO>/losses/Y.rds                year-keyed final-demand matrices
#   <MRIO>/losses/<yr>_L_value.rds     year-keyed Leontief inverse (14)
#   <base>/E.rds                       stressor x sector extensions (16)
#   <base>/V.rds                       VA (USD) x sector, 2 rows gloria/exiobase (35)
#   <base>/io_labels.csv               row/col labels for the grid (iso3c, comm_code, item)
#
# OUTPUTS  (one pair of files per VA variant; 'full' keeps the base name, the
#   wages+capital variant gets an '_ex_tls' suffix) ---------------------------
#   <OUT_DIR>/FABIO_bcp_ibif_value_added_responsibility_<base>[_ex_tls].csv
#       long: year, va_variant, biofuel_group, va_iso3c, va_comm_code, va_item,
#             ibif_va_resp  (the normalised value-added responsibility)
#   <OUT_DIR>/FABIO_bcp_ibif_value_added_coverage_<base>[_ex_tls].csv
#       per (year, va_variant, biofuel_group): ibif_consumer_footprint,
#       ibif_allocated_norm, conservation_gap_pct, implied_unit_value_usd_per_t,
#       throughput_coverage (the HONESTY CHECK -- read the CAVEAT block).
#   va_variant: 'full' uses total value added; 'ex_tls' excludes taxes-less-
#   subsidies (value added = wages + capital), so a sector that is net-subsidised
#   does not carry lower responsibility on that account.
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
# ECOSYSTEM-SERVICE EXEMPTION. Some upstream nodes are ecosystem services rather
# than market commodities (e.g. Grazing, c062): they have no monetary output and
# so v = 0 by construction, NOT because we failed to value a real transaction.
# Counting their tonnage as "missed" would wrongly depress throughput_coverage.
# The items in ECOSYSTEM_SERVICE_ITEMS are therefore removed from BOTH the
# numerator and denominator of throughput_coverage (their tonnage is reported
# separately as ecosystem_service_throughput). The allocation h is untouched --
# a v = 0 node never carries responsibility regardless.
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

# Ecosystem-service items EXEMPT from the missed-tonnage (throughput_coverage)
# diagnostic. These nodes carry no monetary value added (v = p/X = 0) by
# construction, so their upstream tonnage would otherwise be booked as "missed"
# and unfairly depress the coverage honesty check. They are removed from BOTH
# the numerator and the denominator of throughput_coverage only; the
# responsibility allocation h is untouched (a v = 0 node carries no h anyway).
# Match is by item label so it is robust to comm_code renumbering.
ECOSYSTEM_SERVICE_ITEMS <- c("Grazing")   # c062 in inst/items_full_bcp.csv

biofuel_groups <- list(
  biogasoline      = "c146",
  biodiesel        = "c147",
  renewable_diesel = "c149"                       # c149 only; HVO co-products
  # biopropane/bio-LPG (c150) and
  # bionaphtha (c151) are excluded
)

va_variants <- c(full = FALSE, ex_tls = TRUE)   # ex_tls: value added net of taxes-less-subsidies

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

# static mask of ecosystem-service nodes excluded from the coverage diagnostic
exempt_node <- io$item %in% ECOSYSTEM_SERVICE_ITEMS
if (sum(exempt_node))
  message(sprintf(">>> [40] exempting %d ecosystem-service node(s) from coverage: %s",
                  sum(exempt_node), paste(unique(io$item[exempt_node]), collapse = ", ")))

# Pick the VA vector from V for VA_BASE. exclude_tls drops the taxes-less-
# subsidies component (value added = wages + capital = total - tls).
va_row_for <- function(Vi, exclude_tls = FALSE) {
  base  <- switch(VA_BASE, gloria = "gloria", exiobase = "exiobase",
                  stop("VA_BASE must be gloria/exiobase"))
  total <- paste0("value_added_", base)
  if (!total %in% rownames(Vi)) stop("V.rds has no row '", total, "'")
  p <- as.numeric(Vi[total, ])
  if (exclude_tls) {
    tls <- paste0("value_added_tls_", base)
    if (!tls %in% rownames(Vi)) stop("V.rds has no row '", tls, "'")
    p <- p - as.numeric(Vi[tls, ])
  }
  p
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
  
  # intensity f = e / x  (matches 18_01) -- variant-independent
  f <- as.numeric(Ei[STRESSOR, ]) / Xi; f[!is.finite(f)] <- 0
  fB    <- as.vector(crossprod(f, B))   # (f' B)_j : IBIF impact embodied per tonne of final demand j
  y_all <- as.vector(rowSums(as.matrix(Yi)))   # world final demand by product node
  comm  <- io$comm_code
  
  alloc_rows <- list(); cover_rows <- list()
  
  for (variant in names(va_variants)) {
    # VA intensity v = p / x  (p drops tls for the ex_tls variant)
    p <- va_row_for(Vi, exclude_tls = va_variants[[variant]])
    v <- p / Xi; v[!is.finite(v)] <- 0
    s <- as.vector(crossprod(v, B))     # (v' B)_j : USD value added per tonne of j = implied UNIT VALUE
    #            (NOT a share; physical table -- see header CAVEAT)
    
    for (g in names(biofuel_groups)) {
      cc_set <- biofuel_groups[[g]]
      in_g   <- comm %in% cc_set
      
      y_g <- numeric(N); y_g[in_g] <- y_all[in_g]       # biofuel-group final demand only (tonnes)
      if (sum(y_g) == 0) next
      
      t_j         <- y_g * fB                           # IBIF footprint by final product j
      fp_consumer <- sum(t_j)                           # = f' B y_g  (upstream/consumer footprint)
      if (fp_consumer == 0) next
      
      # value-added responsibility = physical footprint distributed by VA share
      # v_i B_ij / s_j  ->  h_i = v_i sum_j B_ij (t_j / s_j).
      t_use <- if (NORMALISE) ifelse(s > 0, t_j / s, 0) else t_j
      h     <- v * as.vector(B %*% t_use)
      allocated_norm <- sum(h)                          # ~ fp_consumer for chains with s_j > 0
      
      implied_unit_value <- sum(s * y_g) / sum(y_g)     # implied USD/tonne -- sanity check on V
      BY <- as.vector(B %*% y_g)                        # upstream throughput (tonnes)
      BYd <- BY; BYd[exempt_node] <- 0                  # drop ecosystem services (e.g. Grazing) from the check
      exempt_throughput   <- sum(BY[exempt_node])       # tonnage set aside as ecosystem services (transparency)
      throughput_coverage <- if (sum(BYd) > 0) sum(BYd[v > 0]) / sum(BYd) else NA_real_
      
      key  <- paste(variant, g, sep = ".")
      keep <- which(h != 0)
      if (length(keep)) alloc_rows[[key]] <- data.table(
        year          = as.integer(yr),
        va_variant    = variant,
        biofuel_group = g,
        va_iso3c      = io$iso3c[keep],
        va_comm_code  = io$comm_code[keep],
        va_item       = io$item[keep],
        ibif_va_resp  = h[keep]                         # normalised VA responsibility (the deliverable)
      )
      
      cover_rows[[key]] <- data.table(
        year                     = as.integer(yr),
        va_variant               = variant,
        biofuel_group            = g,
        ibif_consumer_footprint  = fp_consumer,         # what the biofuel supply chain causes (upstream)
        ibif_allocated_norm      = allocated_norm,      # normalised total (should match consumer fp)
        conservation_gap_pct     = 100 * (allocated_norm - fp_consumer) / fp_consumer,
        implied_unit_value_usd_per_t = implied_unit_value,
        ecosystem_service_throughput = exempt_throughput,  # upstream tonnes excluded from the check
        throughput_coverage      = throughput_coverage  # in [0,1], EXCL. ecosystem services -- honesty check
      )
    }
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
               by = .(va_variant, biofuel_group)]
  chk[, conservation_gap_pct := 100 * (norm - consumer) / consumer]
  print(chk)
  cat("\nInterpretation (physical table -- see CAVEAT):\n")
  cat(" - `norm` should match `consumer` (per-chain conservation of the footprint);\n")
  cat("   a gap means some chains have s_j = 0, i.e. NO valued sector to carry them.\n")
  cat(" - `unit_value` is the implied USD/tonne of the biofuel -- a sanity check on V.\n")
  cat(" - `throughput_cov` in [0,1] is the share of upstream tonnage from valued\n")
  cat("   sectors; low values mean the split leans on few imputed sectors.\n")
  if (exists("ECOSYSTEM_SERVICE_ITEMS") && length(ECOSYSTEM_SERVICE_ITEMS))
    cat(sprintf("   (ecosystem services exempt from the check: %s)\n",
                paste(ECOSYSTEM_SERVICE_ITEMS, collapse = ", ")))
}

# --- save (one pair of files per VA variant) ---------------------------------
for (variant in names(va_variants)) {
  tagv   <- if (variant == "full") "" else paste0("_", variant)   # 'full' keeps the base filename
  a_path <- file.path(OUT_DIR, sprintf("FABIO_bcp_ibif_value_added_responsibility_%s%s.csv", VA_BASE, tagv))
  c_path <- file.path(OUT_DIR, sprintf("FABIO_bcp_ibif_value_added_coverage_%s%s.csv",       VA_BASE, tagv))
  a <- alloc[va_variant == variant]; c <- cover[va_variant == variant]
  fwrite(a, a_path)
  fwrite(c, c_path)
  message(sprintf("Wrote %s (%d rows)\nWrote %s (%d rows)", a_path, nrow(a), c_path, nrow(c)))
}






# =============================================================================
# 40b_diagnose_coverage.R
# WHERE did throughput_coverage leak?  Companion diagnostic to
# 40_responsibility_value_added.R.
#
# throughput_coverage (in FABIO_bcp_ibif_value_added_coverage_*.csv) is
#     sum(BY[v > 0]) / sum(BY),   BY = B %*% y_g,   v = p / X   (VA intensity).
# The "uncovered" complement is the upstream tonnage BY that flows through
# sectors with v = 0.  This script reconstructs BY with the SAME inputs and math
# as code 40, splits it into covered (v>0) vs uncovered (v==0), and ranks the
# uncovered nodes so you can see exactly which (iso3c, comm_code, item) the
# coverage leaked into -- plus WHY each is zero (no VA cell vs no output).
#
# Outputs (OUT_DIR/coverage_diagnostics/):
#   coverage_check_<base>.csv        per (year,group): recomputed coverage vs CSV
#   uncovered_nodes_<base>.csv       every v=0 upstream node, ranked by BY, with
#                                    BY, share-of-uncovered, cumulative, reason
#   uncovered_by_commodity_<base>.csv rollup of uncovered BY by comm_code (feedstock)
#   uncovered_by_country_<base>.csv   rollup of uncovered BY by iso3c
#   uncovered_growth_<base>.csv       first-year vs last-year uncovered BY by comm_code
#
# RUN:  Rscript R/40b_diagnose_coverage.R
#       FABIO_RUN_MODE=bypass Rscript R/40b_diagnose_coverage.R
# =============================================================================

# --- portable repo root (same as 40) -----------------------------------------
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

source("R/00_system_variables.R")
source("R/00_run_config.R")

# --- config (mirror 40) ------------------------------------------------------
model_version <- if (tolower(trimws(Sys.getenv("FABIO_RUN_MODE", "rescaled"))) == "bypass")
  "bypass" else "rescaled"
base_path <- sub("/+$", "", output_dir_bcp)
MRIO_PATH <- if (model_version == "bypass") file.path(base_path, "bypass") else base_path
OUT_DIR   <- if (model_version == "bypass") "output/bypass" else "output"
DIAG_DIR  <- file.path(OUT_DIR, "coverage_diagnostics")
dir.create(DIAG_DIR, showWarnings = FALSE, recursive = TRUE)

allocation <- "value"
STRESSOR   <- "ibif_total"
VA_BASE    <- "exiobase"                 # match the CSV you're diagnosing
resp_years <- as.character(years)

# Ecosystem-service items exempt from the missed-tonnage diagnostic (mirror 40).
# Removed from the coverage numerator/denominator and from the ranked leak list.
ECOSYSTEM_SERVICE_ITEMS <- c("Grazing")  # c062; keep in sync with code 40

# The group to dissect. Default reproduces the UPLOADED csv (3-code bundle) so the
# self-check matches; switch to renewable_diesel = "c149" to match the edited 40.
biofuel_groups <- list(
  biogasoline      = "c146",
  biodiesel        = "c147",
  renewable_diesel = c("c149", "c150", "c151")
)
FOCUS <- names(biofuel_groups)           # or e.g. c("renewable_diesel")

message(sprintf(">>> [40b] model_version='%s' | VA_BASE='%s' | groups: %s",
                model_version, VA_BASE, paste(FOCUS, collapse = ", ")))

# --- inputs (same artefacts as 40) -------------------------------------------
need <- function(path) { if (!file.exists(path)) stop("Missing input: ", path); path }
X  <- readRDS(need(file.path(MRIO_PATH, "losses", "X.rds")))
Y  <- readRDS(need(file.path(MRIO_PATH, "losses", "Y.rds")))
V  <- readRDS(need(file.path(base_path, "V.rds")))
io <- fread(   need(file.path(base_path, "io_labels.csv")))
stopifnot(all(c("iso3c", "comm_code", "item") %in% names(io)))
N  <- nrow(io)

# ecosystem-service mask, exempt from the missed-tonnage diagnostic (see code 40)
exempt_node <- io$item %in% ECOSYSTEM_SERVICE_ITEMS

# optional ISIC enrichment (explains WHY v = 0 for source (A))
isic_map <- tryCatch({
  it <- fread("inst/items_full_bcp.csv")
  col <- intersect(c("ISIC", "isic"), names(it))
  if (length(col) && "comm_code" %in% names(it))
    setNames(as.character(it[[col[1]]]), it$comm_code) else NULL
}, error = function(e) NULL)

va_row <- paste0("value_added_", VA_BASE)

# --- worker: one (year, group) -----------------------------------------------
diagnose <- function(yr, g) {
  if (!yr %in% colnames(X) || is.null(Y[[yr]]) || is.null(V[[yr]])) return(NULL)
  L_path <- file.path(MRIO_PATH, "losses", paste0(yr, "_L_", allocation, ".rds"))
  if (!file.exists(L_path)) return(NULL)
  B  <- readRDS(L_path)
  Xi <- as.vector(X[, yr]);  Vi <- V[[yr]];  Yi <- Y[[yr]]
  if (!va_row %in% rownames(Vi)) stop("V.rds has no row '", va_row, "'")
  
  p <- as.numeric(Vi[va_row, ])          # VA (full variant = total)
  v <- p / Xi; v[!is.finite(v)] <- 0     # VA intensity  (identical to code 40)
  
  cc  <- biofuel_groups[[g]]
  in_g <- io$comm_code %in% cc
  y_all <- as.vector(rowSums(as.matrix(Yi)))
  y_g <- numeric(N); y_g[in_g] <- y_all[in_g]
  if (sum(y_g) == 0) return(NULL)
  
  BY  <- as.vector(B %*% y_g)            # upstream throughput (tonnes)
  tot <- sum(BY)                         # full upstream tonnage (incl. ecosystem services)
  tot_diag <- sum(BY[!exempt_node])      # honesty-check denominator (ecosystem services removed)
  covered <- v > 0                       # exactly code 40's test
  # coverage EXCLUDES ecosystem-service nodes from both numerator and denominator
  coverage <- if (tot_diag > 0) sum(BY[covered & !exempt_node]) / tot_diag else NA_real_
  
  # WHY is a node uncovered? decompose the v==0 reason (exempt overrides).
  reason <- rep("covered", N)
  reason[!covered & Xi == 0]           <- "no_output_X0"
  reason[!covered & Xi != 0 & p == 0]  <- "no_VA_cell"        # V zero-filled here
  reason[!covered & Xi != 0 & p < 0]   <- "negative_VA"
  reason[exempt_node]                  <- "exempt_ecosystem_service"
  
  unc <- BY > 0 & !covered & !exempt_node   # ranked leaks exclude ecosystem services
  nodes <- data.table(
    year        = as.integer(yr),
    biofuel_group = g,
    iso3c       = io$iso3c,
    comm_code   = io$comm_code,
    item        = io$item,
    isic        = if (!is.null(isic_map)) isic_map[io$comm_code] else NA_character_,
    throughput  = BY,
    v           = v,
    reason      = reason
  )[unc][order(-throughput)]
  if (nrow(nodes)) {
    U <- sum(nodes$throughput)
    nodes[, `:=`(share_of_uncovered = throughput / U,
                 cum_share          = cumsum(throughput) / U)]
  }
  
  chk <- data.table(year = as.integer(yr), biofuel_group = g,
                    upstream_throughput  = tot,
                    exempt_throughput    = sum(BY[exempt_node]),             # ecosystem services set aside
                    covered_throughput   = sum(BY[covered & !exempt_node]),
                    uncovered_throughput = sum(BY[!covered & !exempt_node]), # missed, excl. ecosystem svcs
                    throughput_coverage  = coverage)
  list(nodes = nodes, chk = chk)
}

# --- run ---------------------------------------------------------------------
res <- list()
for (yr in resp_years) for (g in FOCUS) res[[paste(yr, g)]] <- diagnose(yr, g)
res <- Filter(Negate(is.null), res)

chk   <- rbindlist(lapply(res, `[[`, "chk"),   use.names = TRUE, fill = TRUE)
nodes <- rbindlist(lapply(res, `[[`, "nodes"), use.names = TRUE, fill = TRUE)

# --- self-check against the shipped coverage CSV (if present) ----------------
csv_path <- file.path(OUT_DIR, sprintf("FABIO_bcp_ibif_value_added_coverage_%s.csv", VA_BASE))
if (file.exists(csv_path)) {
  ship <- fread(csv_path)[va_variant == "full", .(year, biofuel_group, throughput_coverage)]
  cmp  <- merge(chk[, .(year, biofuel_group, recomputed = throughput_coverage)],
                ship, by = c("year", "biofuel_group"), all.x = TRUE)
  cmp[, abs_diff := abs(recomputed - throughput_coverage)]
  cat("\n-- self-check: recomputed vs shipped throughput_coverage (max abs diff) --\n")
  print(cmp[, .(max_abs_diff = max(abs_diff, na.rm = TRUE)), by = biofuel_group])
}

# --- rollups: WHERE the uncovered tonnage sits -------------------------------
by_comm <- nodes[, .(uncovered_throughput = sum(throughput),
                     reason = reason[1], isic = isic[1]),
                 by = .(year, biofuel_group, comm_code, item)][order(-uncovered_throughput)]
by_ctry <- nodes[, .(uncovered_throughput = sum(throughput)),
                 by = .(year, biofuel_group, iso3c)][order(-uncovered_throughput)]

# --- growth: what drove the decline (first vs last year, per commodity) ------
yr_lo <- min(nodes$year); yr_hi <- max(nodes$year)
growth <- dcast(
  by_comm[year %in% c(yr_lo, yr_hi)],
  biofuel_group + comm_code + item ~ year,
  value.var = "uncovered_throughput", fill = 0)
setnames(growth, as.character(c(yr_lo, yr_hi)), c("uncovered_lo", "uncovered_hi"))
growth[, delta := uncovered_hi - uncovered_lo]
setorder(growth, biofuel_group, -delta)

# --- console summary ---------------------------------------------------------
cat("\n================  coverage decomposition  ================\n")
print(chk[order(biofuel_group, year)])
for (g in FOCUS) {
  cat(sprintf("\n---- %s: top uncovered feedstocks in %d (by upstream tonnage) ----\n", g, yr_hi))
  top <- by_comm[biofuel_group == g & year == yr_hi][1:min(10, .N)]
  if (nrow(top)) print(top[, .(comm_code, item, isic, reason, uncovered_throughput)])
  cat(sprintf("---- %s: biggest RISE in uncovered tonnage %d -> %d ----\n", g, yr_lo, yr_hi))
  gr <- growth[biofuel_group == g][1:min(10, .N)]
  if (nrow(gr)) print(gr[, .(comm_code, item, uncovered_lo, uncovered_hi, delta)])
}

# --- save --------------------------------------------------------------------
fwrite(chk,     file.path(DIAG_DIR, sprintf("coverage_check_%s.csv",         VA_BASE)))
fwrite(nodes,   file.path(DIAG_DIR, sprintf("uncovered_nodes_%s.csv",        VA_BASE)))
fwrite(by_comm, file.path(DIAG_DIR, sprintf("uncovered_by_commodity_%s.csv", VA_BASE)))
fwrite(by_ctry, file.path(DIAG_DIR, sprintf("uncovered_by_country_%s.csv",   VA_BASE)))
fwrite(growth,  file.path(DIAG_DIR, sprintf("uncovered_growth_%s.csv",       VA_BASE)))
message("Wrote diagnostics to ", DIAG_DIR)