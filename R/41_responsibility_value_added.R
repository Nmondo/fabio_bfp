# =============================================================================
# 41_responsibility_value_added.R
# Value-added-based responsibility allocation (Pinero et al. 2019, Econ. Syst.
# Res. 31:2) for the bio-based transport fuels, using the IBIF biodiversity
# impact as the environmental extension.
#
# WHAT THIS COMPUTES ----------------------------------------------------------
# For each biofuel "chain" (final product) we take the supply-chain-wide IBIF
# pressure embodied in that biofuel's final demand -- the standard FABIO
# consumer/upstream footprint, identical in construction to 18_01's
# `ext = E/X ; MP = ext * L` -- and RE-ALLOCATE it to every value-generating
# sector along the chain in proportion to the value added it captures:
#     f   = e / x                      impact intensity per unit output  (E, X)
#     v   = p / x                      value-added intensity per unit output (V, X)
#     B   = (I - A)^-1                 Leontief inverse          (L_<allocation>)
#     t_j = y_j (f' B)_j               consumer footprint of final product j
#     s_j = (v' B)_j                   value added embodied per tonne of j
#     h_i = v_i sum_j B_ij (t_j / s_j) value-added responsibility of sector i
#
# WHY THE s_j NORMALISATION ---------------------------------------------------
# Z, X and Y are in MASS under EITHER co-product rule (13_mrio builds both
# Z_mass and Z_value from mr_use[mass]; "mass"/"value" name the allocation rule,
# not the unit). B is therefore a physical Leontief inverse and v is a PRICE
# intensity (USD per tonne), not a dimensionless share: sum_i v_i B_ij is not 1
# but s_j, the implied UNIT VALUE of j. The value-added share of sector i in
# chain j is v_i B_ij / s_j, so dividing each chain by its own s_j is what makes
# h a re-allocation that conserves the footprint:
#     sum_i h_i = sum_{j: s_j > 0} t_j
# Chains with s_j <= 0 have no valued sector to carry them and drop out; the
# shortfall is reported as conservation_gap_pct rather than normalised away.
# (NORMALISE = FALSE returns the un-normalised v_i sum_j B_ij t_j, which mixes
# impact with price and is NOT a responsibility -- debugging only.)
#
# VA VARIANTS -----------------------------------------------------------------
#   full     wages + capital + taxes-less-subsidies
#   ex_tls   wages + capital, so a net-subsidised sector does not carry lower
#            responsibility on that account
# One pair of files per variant.
#
# BIOFUEL SCOPE (comm_codes, per 07_03a/07_03b/33 and 18_01's bf_set) ---------
#     biogasoline       c146
#     biodiesel         c147
#     renewable_diesel  c149
#
# INPUTS  (same artefacts the footprint pipeline uses) ------------------------
#   <MRIO>/losses/X.rds                total output, columns = years
#   <MRIO>/losses/Y.rds                year-keyed final-demand matrices
#   <MRIO>/losses/<yr>_L_<allocation>.rds  year-keyed Leontief inverse (14);
#                                      co-product rule set by `allocation` below
#   <base>/E.rds                       stressor x sector extensions (16)
#   intermediate_data/V.rds            VA (USD) x sector, 2 rows gloria/exiobase (35)
#   <base>/io_labels.csv               row/col labels for the grid (iso3c, comm_code, item)
#
# OUTPUTS ---------------------------------------------------------------------
# Every file is named   FABIO_bcp_<metric>_<what>_<base>_<alloc>[_<variant>].csv
# so that no two settings of the run switches ever overwrite each other:
#   <metric>  = STAG, from STRESSOR    "ibif_total" -> "ibif",
#                                      "LCIM_EQ_terrestrial" -> "lcim_eq_terrestrial"
#   <base>    = VA_BASE                "gloria" | "exiobase"
#   <alloc>   = ATAG, from `allocation` "mass" | "value" (the co-product rule of B)
#   <variant> = va_variant             omitted for 'full', '_ex_tls' otherwise
#
#   <OUT_DIR>/FABIO_bcp_<metric>_value_added_responsibility_<base>_<alloc>[_ex_tls].csv
#       long: year, va_variant, biofuel_group, va_iso3c, va_comm_code, va_item,
#             va_resp  (the normalised value-added responsibility, in <metric> units)
#   <OUT_DIR>/FABIO_bcp_<metric>_value_added_coverage_<base>_<alloc>[_ex_tls].csv
#       per (year, va_variant, biofuel_group): consumer_footprint,
#       allocated_norm, conservation_gap_pct, implied_unit_value_usd_per_t,
#       throughput_coverage
#   <OUT_DIR>/FABIO_bcp_<metric>_value_added_gaps_<base>_<alloc>.csv
#       per (year, iso3c, comm_code, item) with output but no value added that
#       feeds the studied chains: throughput, value_added, reason
#
# COVERAGE AND GAPS -----------------------------------------------------------
# v = 0 wherever V is zero-filled (35), so the normalisation splits each chain
# among the valued sectors only. The true VA denominator of the uncovered part
# is unknown, so the honest proxy reported instead is
#     throughput_coverage = share of the chain's upstream tonnage (B y_g) that
#                           passes through sectors with V > 0
# Low values mean the split leans on few sectors; treat those biofuel-years with
# caution. implied_unit_value_usd_per_t is a sanity check on V: does the
# biofuel's implied USD/tonne look real?
#
# Most zero-valued sectors are harmless -- off-chain or genuinely empty (X = 0).
# What matters is a cell with real output (X > 0) but no value added (VA <= 0)
# sitting upstream of the studied biofuels: its footprint is real but has no
# valued sector to carry it. Those cells are listed in the gaps CSV, ranked by
# the tonnage they feed into the chains. Three classes are held out of both the
# gap list and the coverage check, because they carry v = 0 by construction
# rather than through a missed transaction: ecosystem services
# (ECOSYSTEM_SERVICE_ITEMS, e.g. Grazing -- no monetary output), residual
# catch-all nodes (RESIDUAL_ITEMS -- no producing sector to value) and countries
# absent from the VA source (ABSENT_VA_COUNTRIES, handled in fabio_bcp). Their
# tonnage is reported separately. The allocation h is untouched either way -- a
# v = 0 node never carries responsibility.
#
# RUN -------------------------------------------------------------------------
#   Rscript R/41_responsibility_value_added.R
#   FABIO_RUN_MODE=bypass Rscript R/41_responsibility_value_added.R   # counterfactual
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

# --- run switches ------------------------------------------------------------
# tag(): filename-safe slug of a switch, so the switches and the file names can
# never drift apart (see the OUTPUTS block in the header).
tag <- function(x, fallback) {
  s <- gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", tolower(x)))
  if (nzchar(s)) s else fallback
}

allocation <- "value"                 # co-product rule of the Leontief inverse: "mass" | "value"
STRESSOR   <- "ibif_total"  # "ibif_total" | "LCIM_EQ_terrestrial"
VA_BASE    <- "exiobase"             # "gloria" | "exiobase"
NORMALISE  <- TRUE                   # per-chain renormalisation by the implied unit value
resp_years <- as.character(years)    # 2012:2022 from 00_system_variables

ATAG <- tag(allocation, "alloc")                     # "mass" / "value"
STAG <- tag(sub("_total$", "", STRESSOR), "metric")  # "ibif" / "lcim_eq_terrestrial"

# every CSV this script writes goes through here: one naming convention, one place
out_csv <- function(what, variant_tag = "")
  file.path(OUT_DIR, sprintf("FABIO_bcp_%s_%s_%s_%s%s.csv",
                             STAG, what, VA_BASE, ATAG, variant_tag))

# Ecosystem-service items EXEMPT from the missed-tonnage (throughput_coverage)
# diagnostic. These nodes carry no monetary value added (v = p/X = 0) by
# construction, so their upstream tonnage would otherwise be booked as "missed"
# and unfairly depress the coverage honesty check. They are removed from BOTH
# the numerator and the denominator of throughput_coverage only; the
# responsibility allocation h is untouched (a v = 0 node carries no h anyway).
# Match is by item label so it is robust to comm_code renumbering.
ECOSYSTEM_SERVICE_ITEMS <- c("Grazing")   # c062 in inst/items_full_bcp.csv

# Countries entirely absent from the upstream VA source (35): they carry v = 0
# across every commodity and every year, so they never receive responsibility.
# They are dropped in the fabio_bcp build, so the structural-gap diagnostic below
# excludes them to keep the focus on genuine, fixable (country, commodity) cells.
ABSENT_VA_COUNTRIES <- c("ANT", "BRN", "DMA", "ERI", "PRI", "SSD")

# Residual catch-all nodes with no producing sector by construction (FABIO
# aggregation buckets). Like ecosystem services they carry v = 0 structurally, not
# because a real transaction went unvalued, so counting them as "fixable" gaps
# would swamp the list with non-actionable tonnage. They are set aside from the
# structural-gap diagnostic and their upstream tonnage reported separately.
# Match is by item label (robust to comm_code renumbering).
RESIDUAL_ITEMS <- c("Other, Waste", "Other, Unknown")   # c901, c999

biofuel_groups <- list(
  biogasoline      = "c146",
  biodiesel        = "c147",
  renewable_diesel = "c149"                       # c149 only; HVO co-products
  # biopropane/bio-LPG (c150) and
  # bionaphtha (c151) are excluded
)

va_variants <- c(full = FALSE, ex_tls = TRUE)   # ex_tls: value added net of taxes-less-subsidies

message(sprintf(">>> [41] model_version='%s' | VA_BASE='%s' | allocation='%s' | stressor='%s' | normalise=%s",
                model_version, VA_BASE, allocation, STRESSOR, NORMALISE))

# --- static inputs (year-independent) ----------------------------------------
need <- function(path, who) {
  if (!file.exists(path)) stop("Missing input: ", path, "  (produced by ", who, ")")
  path
}
X  <- readRDS(need(file.path(MRIO_PATH, "losses", "X.rds"),   "14_leontief_inverse.R / 13_mrio.R"))
Y  <- readRDS(need(file.path(MRIO_PATH, "losses", "Y.rds"),   "13_mrio.R"))
E  <- readRDS(need(file.path(base_path, "E.rds"),             "16_extensions_main.R"))
V  <- readRDS(need(file.path("intermediate_data", "V.rds"),  "35_bcp_value_added_extension.R"))
io <- fread(   need(file.path(base_path, "io_labels.csv"),    "12_b_update_labels.R"))

stopifnot(all(c("iso3c", "comm_code", "item") %in% names(io)))
N <- nrow(io)

# --- ALIGNMENT GUARD: E / V columns and X rows MUST match the io grid --------
# The footprint / responsibility math is purely POSITIONAL. Both here (f = e/X,
# v = p/X) and in 18_01b (ext = E[stressor,]/X) the vectors are coerced with
# as.vector()/as.numeric(), which STRIPS names -- so R lines up E, V, X, B and
# `io` by INDEX, not by label. That is correct only because 13_mrio names X rows
# "<iso3c>_<comm_code>" and 16/35 reorder E/V to the identical `target_order`.
# If any artefact is ever rebuilt against a differently-ordered io_labels.csv,
# the extension for one item gets divided by another item's output and the
# impact is silently attributed to the WRONG item (e.g. cattle -> rapeseed oil)
# with no error. Verify it once, loudly, before any footprint is computed.
io_key <- paste0(io$iso3c, "_", io$comm_code)
assert_grid <- function(obj, axis, who) {
  nm <- if (axis == "row") rownames(obj) else colnames(obj)
  if (is.null(nm)) {
    warning(sprintf("[41] %s has no %snames -- cannot verify item alignment; trusting position.",
                    who, axis)); return(invisible())
  }
  if (!identical(nm, io_key)) {
    i <- which(nm != io_key)[1]
    stop(sprintf(paste0("[41] %s %s order != io_labels grid -- environmental/VA ",
                        "extensions would be misattributed to the wrong items.\n",
                        "     first mismatch at index %d: %s = '%s' vs io = '%s'.\n",
                        "     Re-run 16 (E) and 35 (V) against THIS io_labels.csv."),
                 who, axis, i, who, nm[i], io_key[i]))
  }
  invisible()
}
assert_grid(X, "row", "X.rds")
for (yr in intersect(as.character(years), names(E))) assert_grid(E[[yr]], "col", sprintf("E[[%s]]", yr))
for (yr in intersect(as.character(years), names(V))) assert_grid(V[[yr]], "col", sprintf("V[[%s]]", yr))
message(">>> [41] alignment guard passed: X rows and E/V columns match the io_labels grid.")

# static mask of ecosystem-service nodes excluded from the coverage diagnostic
exempt_node <- io$item %in% ECOSYSTEM_SERVICE_ITEMS
if (sum(exempt_node))
  message(sprintf(">>> [41] exempting %d ecosystem-service node(s) from coverage: %s",
                  sum(exempt_node), paste(unique(io$item[exempt_node]), collapse = ", ")))

# static mask of nodes in countries absent from the VA source (excluded from the
# structural-gap diagnostic; handled in fabio_bcp)
absent_va_node <- io$iso3c %in% ABSENT_VA_COUNTRIES

# static mask of residual catch-all nodes (set aside from the structural-gap list)
residual_node <- io$item %in% RESIDUAL_ITEMS

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
  
  # accumulators for the structural-gap diagnostic (full-variant VA; throughput
  # summed over the studied chains)
  BY_studied <- numeric(N); p_full <- rep(NA_real_, N)
  
  for (variant in names(va_variants)) {
    # VA intensity v = p / x  (p drops tls for the ex_tls variant)
    p <- va_row_for(Vi, exclude_tls = va_variants[[variant]])
    if (variant == "full") p_full <- p       # total VA, for the structural-gap check
    v <- p / Xi; v[!is.finite(v)] <- 0
    s <- as.vector(crossprod(v, B))     # (v' B)_j : USD value added per tonne of j = implied UNIT VALUE
    #            (a price intensity, NOT a share -- see the header)
    
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
      if (variant == "full") BY_studied <- BY_studied + BY   # tonnage feeding the studied chains
      
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
        va_resp       = h[keep]                         # normalised VA responsibility (the deliverable)
      )
      
      cover_rows[[key]] <- data.table(
        year                     = as.integer(yr),
        va_variant               = variant,
        biofuel_group            = g,
        consumer_footprint       = fp_consumer,         # what the biofuel supply chain causes (upstream)
        allocated_norm           = allocated_norm,      # normalised total (should match consumer fp)
        conservation_gap_pct     = 100 * (allocated_norm - fp_consumer) / fp_consumer,
        implied_unit_value_usd_per_t = implied_unit_value,
        ecosystem_service_throughput = exempt_throughput,  # upstream tonnes excluded from the check
        throughput_coverage      = throughput_coverage  # in [0,1], EXCL. ecosystem services -- honesty check
      )
    }
  }
  
  # structural VA gaps that actually feed the studied chains: real output but no
  # value added, upstream of a biofuel. Ecosystem services, absent-VA countries
  # and residual catch-all nodes are set aside (see header); ranked by tonnage.
  no_va      <- BY_studied > 0 & Xi > 0 & p_full <= 0 & !exempt_node & !absent_va_node
  gap_mask   <- no_va & !residual_node
  residual_t <- sum(BY_studied[no_va & residual_node])   # set-aside tonnage (transparency)
  gk <- which(gap_mask)
  gap_dt <- if (length(gk)) data.table(
    year        = as.integer(yr),
    iso3c       = io$iso3c[gk],
    comm_code   = io$comm_code[gk],
    item        = io$item[gk],
    throughput  = BY_studied[gk],                    # upstream tonnes feeding the biofuels
    value_added = p_full[gk],
    reason      = ifelse(p_full[gk] < 0, "negative_VA", "no_VA_cell")
  )[order(-throughput)] else NULL
  
  list(alloc = rbindlist(alloc_rows, use.names = TRUE, fill = TRUE),
       cover = rbindlist(cover_rows, use.names = TRUE, fill = TRUE),
       residual = residual_t,
       gap   = gap_dt)
}

# --- run all years -----------------------------------------------------------
res   <- lapply(resp_years, function(yr) { message("  year ", yr); compute_year(yr) })
res   <- Filter(Negate(is.null), res)
alloc <- rbindlist(lapply(res, `[[`, "alloc"), use.names = TRUE, fill = TRUE)
cover <- rbindlist(lapply(res, `[[`, "cover"), use.names = TRUE, fill = TRUE)
gaps  <- rbindlist(lapply(res, `[[`, "gap"),   use.names = TRUE, fill = TRUE)
residual_set_aside <- sum(unlist(lapply(res, `[[`, "residual")))   # tonnes in catch-all nodes

# annotate value-generator continent (nice-to-have, mirrors 18_01)
regions <- tryCatch(fread("inst/regions_full.csv"), error = function(e) NULL)
if (!is.null(regions) && "continent" %in% names(regions))
  alloc[, va_continent := regions$continent[match(va_iso3c, regions$iso3c)]]

# --- validation / console summary -------------------------------------------
cat("\n================  value-added responsibility  ================\n")
cat(sprintf("VA base: %-8s   allocation: %-5s   normalise: %s\n", VA_BASE, allocation, NORMALISE))
if (nrow(cover)) {
  chk <- cover[, .(consumer   = sum(consumer_footprint),
                   norm       = sum(allocated_norm),
                   unit_value = weighted.mean(implied_unit_value_usd_per_t, consumer_footprint),
                   throughput_cov = weighted.mean(throughput_coverage, consumer_footprint)),
               by = .(va_variant, biofuel_group)]
  chk[, conservation_gap_pct := 100 * (norm - consumer) / consumer]
  print(chk)
  cat("\nInterpretation (physical table -- see the header):\n")
  cat(" - `norm` should match `consumer` (per-chain conservation of the footprint);\n")
  cat("   a gap means some chains have s_j = 0, i.e. NO valued sector to carry them.\n")
  cat(" - `unit_value` is the implied USD/tonne of the biofuel -- a sanity check on V.\n")
  cat(" - `throughput_cov` in [0,1] is the share of upstream tonnage from valued\n")
  cat("   sectors; low values mean the split leans on few imputed sectors.\n")
  if (exists("ECOSYSTEM_SERVICE_ITEMS") && length(ECOSYSTEM_SERVICE_ITEMS))
    cat(sprintf("   (ecosystem services exempt from the check: %s)\n",
                paste(ECOSYSTEM_SERVICE_ITEMS, collapse = ", ")))
}

# --- structural value-added gaps feeding the studied chains ------------------
# (year, iso3c, comm_code, item) cells with real output but no value added that
# sit upstream of the biofuels -- the genuine, fixable gaps. Residual catch-all
# nodes and absent-VA countries are set aside (see header); ranked by contributed
# tonnage and rolled up by commodity for a one-glance read.
cat("\n================  structural VA gaps upstream of the biofuels  ================\n")
cat(sprintf("Set aside (no producing sector to value): %.4g t via residual catch-all\n",
            residual_set_aside))
cat(sprintf("Countries excluded (handled in fabio_bcp): %s\n",
            paste(ABSENT_VA_COUNTRIES, collapse = ", ")))
if (!nrow(gaps)) {
  cat("No fixable gaps: every on-chain node with output also carries value added.\n")
} else {
  by_item <- gaps[, .(throughput   = sum(throughput),
                      n_countries  = uniqueN(iso3c),
                      years        = paste0(min(year), "-", max(year)),
                      reason       = paste(sort(unique(reason)), collapse = "+")),
                  by = .(comm_code, item)][order(-throughput)]
  cat(sprintf("\n%d fixable cells across %d commodities and %d countries, %.4g t total.\n",
              nrow(gaps), uniqueN(gaps$comm_code), uniqueN(gaps$iso3c), sum(gaps$throughput)))
  cat("-- top real feedstocks by tonnage fed into the biofuel chains --\n")
  print(head(by_item, 15))
  g_path <- out_csv("value_added_gaps")
  fwrite(gaps[order(year, -throughput)], g_path)
  message(sprintf("Wrote %s (%d rows)", g_path, nrow(gaps)))
}

# --- save (one pair of files per VA variant) ---------------------------------
for (variant in names(va_variants)) {
  tagv   <- if (variant == "full") "" else paste0("_", variant)   # 'full' keeps the base filename
  a_path <- out_csv("value_added_responsibility", tagv)
  c_path <- out_csv("value_added_coverage",       tagv)
  a <- alloc[va_variant == variant]; c <- cover[va_variant == variant]
  fwrite(a, a_path)
  fwrite(c, c_path)
  message(sprintf("Wrote %s (%d rows)\nWrote %s (%d rows)", a_path, nrow(a), c_path, nrow(c)))
}