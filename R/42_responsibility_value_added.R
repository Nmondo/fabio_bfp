# =============================================================================
# 42_responsibility_value_added.R
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
#   <OUT_DIR>/FABIO_bcp_<metric>_value_added_trade_split_<base>_<alloc>[_ex_tls].csv
#       Pinero eq. 8, H* = v_hat B T' with T = f_hat B y_hat, rolled up to
#       countries on BOTH axes: rows = value generator, columns = extraction
#       origin. Written as margins, one row per (year, va_variant,
#       biofuel_group, iso3c):
#             va_based      row margin   = the VA account (= va_resp by country)
#             va_domestic   diagonal     = extracted here, charged here
#             va_import     row off-diag = extracted abroad, charged to this VA
#             va_export     col off-diag = extracted here, charged to FOREIGN VA
#             va_production column margin = extraction here (cross-check vs 41)
#       This is the dimension va_resp alone cannot give: h sums over the final
#       products j and so forgets where the pressure was extracted. 45 needs it
#       to split the VA bar the way it splits the PBA/CBA/HDI bars.
#   <OUT_DIR>/FABIO_bcp_<metric>_value_added_continent_flows_<base>_<alloc>[_ex_tls].csv
#       the SAME H*, folded onto continents on both axes and written WHOLE:
#       year, va_variant, biofuel_group, va_continent (value generator),
#       origin_continent (extraction), impact.   (zero cells dropped)
#       48 needs the fold rather than the margins above, for the reason 41's
#       header spells out: summing va_domestic over the countries of a region
#       keeps the COUNTRY meaning of "charged where it was extracted", so value
#       captured in NLD on pressure extracted in DEU would read as a foreign
#       flow inside an EU panel. The ROW margin is invariant under the fold, so
#       the VA account totals are unchanged -- only the diagonal moves.
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
#   Rscript R/42_responsibility_value_added.R
#   FABIO_RUN_MODE=bypass Rscript R/42_responsibility_value_added.R   # counterfactual
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

source("R/00_run_config.R")               # RUN_MODE / mode_dir()
source("R/40_responsibility_shared.R")    # the run, its file names, biofuel_groups
SCRIPT <- "42"

# --- run switches ------------------------------------------------------------
# The indicator, allocation, VA base and every derived file name come from 40;
# only what this script alone decides is set here.
NORMALISE <- TRUE                    # per-chain renormalisation by the implied unit value

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

va_variants <- c(full = FALSE, ex_tls = TRUE)   # ex_tls: value added net of taxes-less-subsidies

run_banner()
message(sprintf(">>> [42] normalise=%s", NORMALISE))

# --- static inputs (year-independent) ----------------------------------------
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
    warning(sprintf("[42] %s has no %snames -- cannot verify item alignment; trusting position.",
                    who, axis)); return(invisible())
  }
  if (!identical(nm, io_key)) {
    i <- which(nm != io_key)[1]
    stop(sprintf(paste0("[42] %s %s order != io_labels grid -- environmental/VA ",
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
message(">>> [42] alignment guard passed: X rows and E/V columns match the io_labels grid.")

# --- country universe for the eq. 8 origin split ------------------------------
# BOTH axes of H* are io nodes (rows = value generators, columns = extraction
# origin), so one universe serves both. It is the io grid only -- unlike 41's
# country_grid(), which also has to cover Y's final-demand columns. `ctry` is
# sorted so the axis order matches 41's `countries` for every iso3c they share,
# and 45 joins on the LABEL anyway.
ctry <- sort(unique(io$iso3c))
R_c  <- length(ctry)
cidx <- match(io$iso3c, ctry)
# plain 0/1 roll-up, for aggregating node vectors (h, extraction) to countries
S1   <- Matrix::sparseMatrix(i = seq_len(N), j = cidx, x = 1, dims = c(N, R_c))

# --- continent grid: the second axis, for 48 ---------------------------------
# 48 needs H* folded onto regions on BOTH axes, for the same reason 41 folds D:
# summing va_domestic over the countries of a region keeps the COUNTRY meaning
# of "extracted here, charged here", so value captured in NLD on pressure
# extracted in DEU would still read as a foreign flow inside an EU panel. The
# ROW margin is invariant under the fold, so the VA account totals are exactly
# what 43 plots; only the diagonal moves. Same continent_of() as 41, so the two
# matrices are folded onto the SAME partition -- an axis built independently
# here is how the two files would come to disagree about where a country sits.
S_cont_va <- continent_aggregator(ctry)
message(sprintf(">>> [42] continent grid (%d regions): %s",
                ncol(S_cont_va), paste(colnames(S_cont_va), collapse = " ")))

# static mask of ecosystem-service nodes excluded from the coverage diagnostic
exempt_node <- io$item %in% ECOSYSTEM_SERVICE_ITEMS
if (sum(exempt_node))
  message(sprintf(">>> [42] exempting %d ecosystem-service node(s) from coverage: %s",
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
  # Matrix::rowSums, NOT rowSums(as.matrix(Yi)): Y is sparse and N x n_fd, so the
  # densification cost ~150 MB per year to produce an N-vector. (41 already does
  # this the sparse way.)
  y_all <- as.vector(Matrix::rowSums(Yi))      # world final demand by product node
  comm  <- io$comm_code
  
  # f-weighted roll-up: crossprod(S_f, B)[p, j] = extraction in country p per unit
  # of final demand for j. Built from the sparse aggregator rather than
  # Diagonal(f) %*% B, which would copy the whole N x N inverse.
  S_f <- Matrix::sparseMatrix(i = seq_len(N), j = cidx, x = f, dims = c(N, R_c))
  
  alloc_rows <- list(); cover_rows <- list(); split_rows <- list(); cont_rows <- list()
  
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
    # v-weighted roll-up: crossprod(S_v, B)[r, j] = value added captured in country
    # r per tonne of final demand for j. Its column sums are s_j by construction.
    S_v <- Matrix::sparseMatrix(i = seq_len(N), j = cidx, x = v, dims = c(N, R_c))
    
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
      
      # --- Pinero et al. (2019) eq. 8: the ORIGIN of the re-allocated pressure ---
      # h above answers "who captured the value", but sums over j and so forgets
      # WHERE the pressure was extracted. Eq. 8 keeps both:
      #     T  = f_hat B y_hat        (extraction node k x final product j)
      #     H* = v_hat B T'           (value generator i x extraction node k)
      # rolled up to countries. Row margin = h by country (the VA account), column
      # margin = extraction by country (= 41's production_based for this chain),
      # diagonal = domestic, off-diagonals = foreign. Aggregating INSIDE the
      # product keeps this at R x R and never materialises an N x N T'.
      #     H[r, p] = sum_j (S_v' B)[r, j] * w_j * (S_f' B)[p, j]
      # with w_j = y_j / s_j -- the SAME per-chain normalisation as t_use, which is
      # what makes both margins land where they should (see the checks below).
      w_all <- if (NORMALISE) ifelse(s > 0, y_g / s, 0) else y_g
      jj    <- which(w_all != 0)
      if (length(jj)) {
        Bg <- B[, jj, drop = FALSE]                             # N x n_g
        Mv <- as.matrix(Matrix::crossprod(S_v, Bg))             # R x n_g
        Mf <- as.matrix(Matrix::crossprod(S_f, Bg))             # R x n_g
        H  <- Mv %*% (w_all[jj] * t(Mf))                        # R x R
      } else {
        H <- matrix(0, R_c, R_c)
      }
      dimnames(H) <- list(ctry, ctry)
      
      va_tot <- rowSums(H)          # the VA account, by country  (= h rolled up)
      va_dom <- diag(H)             # extracted here, charged here
      va_imp <- va_tot - va_dom     # extracted ABROAD, charged to this country's VA
      va_prd <- colSums(H)          # extracted here, charged to anyone (= production)
      va_exp <- va_prd - va_dom     # extracted here, charged to FOREIGN value added
      
      # The two margins are the whole claim of eq. 8, so verify them rather than
      # assert them. Row: the split must reproduce 42's own h. Column: it must
      # reproduce the physical extraction by origin, which is what makes the VA
      # bar commensurable with 41's PBA/CBA bars in 45.
      h_ctry  <- as.vector(Matrix::crossprod(S1, h))
      e_ctry  <- as.vector(Matrix::crossprod(S1, f * BY))
      scale_h <- max(sum(abs(h_ctry)), .Machine$double.eps)
      scale_e <- max(sum(abs(e_ctry)), .Machine$double.eps)
      dev_row <- max(abs(va_tot - h_ctry)) / scale_h
      # NOTE the column margin is only the FULL extraction for chains with s_j > 0;
      # where s_j <= 0 the chain drops out of h as well, and the shortfall is the
      # conservation_gap already reported. Compare against the same restriction.
      # Only the NORMALISED product has a physical column margin: unnormalised,
      # w_j = y_j leaves colSums(H) = sum_j y_j s_j (S_f' B)[p, j], an s-weighted
      # quantity in no unit at all. NORMALISE = FALSE is debugging-only anyway.
      e_kept  <- as.vector(Matrix::crossprod(S1, f * as.vector(B %*% ifelse(w_all != 0, y_g, 0))))
      dev_col <- if (NORMALISE) max(abs(va_prd - e_kept)) / scale_e else NA_real_
      if (dev_row > 1e-9 || isTRUE(dev_col > 1e-9))
        warning(sprintf(paste0("[42] eq. 8 margins off for %s/%s/%s: row dev %.3g, col dev %.3g. ",
                               "H* should reproduce h by country and extraction by origin."),
                        yr, variant, g, dev_row, dev_col))
      
      # --- the same matrix, folded onto continents (for 48) -------------------
      # H*[r, p] rolled up on both axes: rows stay value generators, columns stay
      # extraction origins, so the diagonal is "extracted in this region, charged
      # to value added in this region" -- which is NOT the sum of the country
      # diagonals, and is exactly the block 48 needs. Written whole rather than
      # as margins: the off-diagonal is a regional value-capture map in its own
      # right, and deriving margins downstream costs nothing.
      Hc  <- fold_continents(H, S_cont_va)
      flc <- melt_continent_matrix(Hc, "impact")[impact != 0]
      if (nrow(flc)) {
        setnames(flc, c("from", "to"), c("va_continent", "origin_continent"))
        cont_rows[[paste(variant, g, sep = ".")]] <-
          data.table(year = as.integer(yr), va_variant = variant,
                     biofuel_group = g, flc)
      }
      
      keep_s <- which(va_tot != 0 | va_prd != 0)
      if (length(keep_s)) split_rows[[paste(variant, g, sep = ".")]] <- data.table(
        year          = as.integer(yr),
        va_variant    = variant,
        biofuel_group = g,
        iso3c         = ctry[keep_s],
        va_based      = va_tot[keep_s],      # the account = domestic + import
        va_domestic   = va_dom[keep_s],
        va_import     = va_imp[keep_s],
        va_export     = va_exp[keep_s],      # booked to foreign VA, NOT charged here
        va_production = va_prd[keep_s]       # column margin; cross-check against 41
      )
      
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
        throughput_coverage      = throughput_coverage, # in [0,1], EXCL. ecosystem services -- honesty check
        eq8_row_margin_dev       = dev_row,             # H* row margin vs h (should be ~0)
        eq8_col_margin_dev       = dev_col              # H* col margin vs extraction by origin
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
       split = rbindlist(split_rows, use.names = TRUE, fill = TRUE),
       cont  = rbindlist(cont_rows,  use.names = TRUE, fill = TRUE),
       residual = residual_t,
       gap   = gap_dt)
}

# --- run all years -----------------------------------------------------------
res   <- lapply(resp_years, function(yr) { message("  year ", yr); compute_year(yr) })
res   <- Filter(Negate(is.null), res)
alloc <- rbindlist(lapply(res, `[[`, "alloc"), use.names = TRUE, fill = TRUE)
cover <- rbindlist(lapply(res, `[[`, "cover"), use.names = TRUE, fill = TRUE)
split <- rbindlist(lapply(res, `[[`, "split"), use.names = TRUE, fill = TRUE)
cflow <- rbindlist(lapply(res, `[[`, "cont"),  use.names = TRUE, fill = TRUE)
gaps  <- rbindlist(lapply(res, `[[`, "gap"),   use.names = TRUE, fill = TRUE)

# --- the fold must not move the VA account ----------------------------------
# Row margin of S'H*S = row margin of H* summed by region = the VA account by
# region. Checked against this run's own country-level split file, at source:
# see 41 for why a fold bug must not be allowed to reach 48 as a plotting
# mystery. (The COLUMN margin is extraction by origin and is checked per year
# inside the loop, against e_kept.)
if (nrow(cflow) && nrow(split)) {
  ref <- split[, .(va = sum(va_based)),
               by = .(year, va_variant, biofuel_group, continent = continent_of(iso3c))]
  got <- cflow[, .(va = sum(impact)),
               by = .(year, va_variant, biofuel_group, continent = va_continent)]
  cmp <- merge(ref, got, by = c("year", "va_variant", "biofuel_group", "continent"),
               all = TRUE, suffixes = c("", "_fold"))
  for (j in c("va", "va_fold")) set(cmp, which(is.na(cmp[[j]])), j, 0)
  dev <- max(abs(cmp$va - cmp$va_fold)) / max(abs(cmp$va), 1e-12)
  if (dev > 1e-9)
    warning(sprintf(paste0("[42] the continent fold does not reproduce the VA account ",
                           "(max rel. %.3g) -- rowSums(S'H*S) should equal va_based summed by ",
                           "region. Do NOT plot 48 until this is 0."), dev))
  else
    message(sprintf(">>> [42] continent fold verified against the VA account (max rel. dev %.1e)", dev))
  # How much of the value-capture story the fold absorbs: pressure extracted in
  # one country and charged to value added in another, but WITHIN one region.
  vt_ctry <- split[va_variant == VA_VARIANT, sum(va_import)]
  vt_cont <- cflow[va_variant == VA_VARIANT & va_continent != origin_continent, sum(impact)]
  message(sprintf(">>> [42] of the VA account resting on foreign extraction, %.1f%% stays inside one region.",
                  100 * (1 - vt_cont / max(vt_ctry, .Machine$double.eps))))
}
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
  cat(sprintf(" - eq. 8 margins: worst row dev %.2e, worst col dev %.2e (both should be ~1e-15;\n",
              max(cover$eq8_row_margin_dev, na.rm = TRUE),
              suppressWarnings(max(cover$eq8_col_margin_dev, na.rm = TRUE))))
  cat("   row = H* reproduces h by country, col = H* reproduces extraction by origin).\n")
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
  g_path <- va_csv("value_added_gaps")
  fwrite(gaps[order(year, -throughput)], g_path)
  message(sprintf("Wrote %s (%d rows)", g_path, nrow(gaps)))
}

# --- save (one pair of files per VA variant) ---------------------------------
for (variant in names(va_variants)) {
  tagv   <- if (variant == "full") "" else paste0("_", variant)   # 'full' keeps the base filename
  a_path <- va_csv("value_added_responsibility", tagv)
  c_path <- va_csv("value_added_coverage",       tagv)
  s_path <- va_csv("value_added_trade_split",    tagv)   # eq. 8 margins, for 45
  f_path <- va_csv("value_added_continent_flows", tagv)  # eq. 8 folded to regions, for 48
  a_dt <- alloc[va_variant == variant]; c_dt <- cover[va_variant == variant]
  s_dt <- split[va_variant == variant]
  f_dt <- if (nrow(cflow)) cflow[va_variant == variant] else cflow
  fwrite(a_dt, a_path)
  fwrite(c_dt, c_path)
  fwrite(s_dt[order(year, biofuel_group, -va_based)], s_path)
  fwrite(if (nrow(f_dt)) f_dt[order(year, biofuel_group, va_continent, origin_continent)] else f_dt,
         f_path)
  message(sprintf("Wrote %s (%d rows)\nWrote %s (%d rows)\nWrote %s (%d rows)\nWrote %s (%d rows)",
                  a_path, nrow(a_dt), c_path, nrow(c_dt), s_path, nrow(s_dt),
                  f_path, nrow(f_dt)))
}