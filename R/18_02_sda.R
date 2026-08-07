# 18_02 - Structural decomposition analysis of footprints
#
# Six-factor, two-polar (Dietzenbacher-Los 1998) decomposition of the
# consumption-based footprint of biofuel final demand:
#
#     F_k = e' L Phi y_k ,     y_k = s_k * sum_c sigma_{k,c} m_{k,c}
#
# Factors, in decomposition order: e (intensity), L (upstream sourcing structure),
# Phi (tier-1 biofuel recipe), s (scale), sigma (composition), m (origin).
#
# SEVEN REPORTED TERMS, SIX FACTORS: Delta_Phi is split exactly into a conversion
# term and a share-constrained mix term, which sum to it. The mix term is reported
# in a centered gauge so that a feedstock whose SHARE RISES and whose intensity is
# BELOW the recipe average receives a negative contribution - i.e. the arriving
# feedstock is credited, not only the departing one. Totals are gauge-invariant.
#
# This supersedes the earlier tau = T_agg %*% L / mu formulation. tau was a TOTAL
# requirement: its column sums added every stage of the cascade plus the Leontief
# identity term, so recipe shares built on it were meaningless and "feedstock_mix"
# silently absorbed upstream technology change.
#
# Diagnostics and acceptance tests: 18_05_sda_checks.R

# Setup ------------------------------------------------------------------------
library(data.table)
library(Matrix)
library(tidyverse)
source("R/00_system_variables.R")

## --- portable repo root: FABIO_BFP_ROOT override, else walk up to the repo marker ---
fabio_root <- Sys.getenv("FABIO_BFP_ROOT", unset = "")
if (!nzchar(fabio_root)) {
  fabio_root <- getwd()
  while (!file.exists(file.path(fabio_root, "R", "00_system_variables.R")) &&
         dirname(fabio_root) != fabio_root) fabio_root <- dirname(fabio_root)
  if (!file.exists(file.path(fabio_root, "R", "00_system_variables.R")))
    stop("Repo root not found above ", getwd(), " - set FABIO_BFP_ROOT or run from inside the repo.")
}
setwd(fabio_root)

# Read labels ------------------------------------------------------------------
input_path <- "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/"

# ---- CAPPING VARIANT SWITCH --------------------------------------------------
# FABIO_VARIANT = "" (baseline, E.rds -> output/) or "capped" (E_capped.rds ->
# output_capped/). OUT_DIR is the default output dir for the SDA functions below.
VARIANT <- tolower(trimws(Sys.getenv("FABIO_VARIANT", unset = "capped")))
vsuf    <- if (VARIANT == "capped") "_capped" else ""
E_file  <- paste0("E", vsuf, ".rds")
OUT_DIR <- paste0("output", vsuf)
message(sprintf(">>> [18_02 SDA] variant = '%s'  (E: %s | out: %s)",
                if (nzchar(VARIANT)) VARIANT else "baseline", E_file, OUT_DIR))

regions <- fread(file = "inst/regions_full.csv") %>% filter(current == TRUE)
io      <- fread(paste0(input_path, "io_labels.csv"))
items   <- fread(file = "inst/items_full_bcp.csv") %>% filter(comm_code %in% unique(io$comm_code))
fd      <- fread(file = paste0(input_path, "losses/fd_labels.csv"))
ex      <- fread(file = paste0(input_path, "ex_labels.csv"))

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Load static data once (all years, value allocation) ---------------------------
# X, Y, Z and E are year-independent containers and only need to be read once.
# L is year-specific and is loaded inside each function call.
allocation <- "value"
X <- readRDS(paste0(input_path, "losses/X.rds"))
Y <- readRDS(paste0(input_path, "losses/Y.rds"))
Z <- readRDS(paste0(input_path, "losses/Z_", allocation, ".rds"))
E <- readRDS(paste0(input_path, E_file))

# Make E_bar, 3-year centred average of the environmental extensions.
yrs_E_bar <- as.character(2012:2022)
E_bar <- vector("list", length(yrs_E_bar))
names(E_bar) <- yrs_E_bar
for (yr in yrs_E_bar) {
  y <- as.integer(yr)
  E_bar[[yr]] <- (E[[as.character(y - 1)]] + E[[as.character(y)]] + E[[as.character(y + 1)]]) / 3
}


######################################################################################################################
############################## FILENAME HELPERS #######################
######################################################################################################################

commodity_slug <- function(commodity) {
  if (is.null(commodity)) return("all")
  bf_set <- c("c146", "c147", "c149", "c150", "c151")
  bp_set <- paste0("c", 152:170)
  in_bf <- commodity %in% bf_set
  in_bp <- commodity %in% bp_set
  if (all(in_bf | in_bp)) {
    has_bf <- any(in_bf); has_bp <- any(in_bp)
    if (has_bf && has_bp) return("BF_BP")
    if (has_bf)           return("BF")
    if (has_bp)           return("BP")
  }
  if (length(commodity) <= 4) return(paste(commodity, collapse = "-"))
  paste0(length(commodity), "comms")
}

fmt_part <- function(name, val) {
  if (is.null(val)) return(NULL)
  if (length(val) == 1 && is.na(val)) return(NULL)
  if (is.logical(val)) return(if (isTRUE(val)) name else NULL)
  if (length(val) > 1) val <- commodity_slug(val)
  as.character(val)
}

build_filename <- function(prefix, ext = "csv", ...) {
  parts <- list(...)
  segs  <- mapply(fmt_part, names(parts), parts, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  segs  <- Filter(Negate(is.null), segs)
  paste0(prefix, "_", paste(segs, collapse = "_"), ".", ext)
}


###########################################################
########### SDA OF THE CONSUMPTION-BASED FOOTPRINT
########### Two-polar (Dietzenbacher-Los 1998), six factors
###########################################################

fp_sda <- function(country         = NULL,
                   year_base,
                   year_current,
                   extension       = NULL,
                   commodity,
                   allocation      = "value",
                   input_path      = "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/",
                   losses          = TRUE,
                   breakdown       = FALSE,
                   mix_gauge       = c("centered", "raw"),
                   col_detail      = c("commodity", "cell"),
                   pairwise        = FALSE,
                   save            = FALSE,
                   output_dir      = OUT_DIR,
                   regions, io, fd, ex,
                   X = NULL, Y = NULL, Z = NULL, E = NULL,
                   L_base = NULL, L_curr = NULL) {
  
  mix_gauge  <- match.arg(mix_gauge)
  col_detail <- match.arg(col_detail)
  sub <- if (losses) "losses/" else ""
  
  # --- Load ------------------------------------------------------------------
  if (is.null(X)) X <- readRDS(paste0(input_path, sub, "X.rds"))
  if (is.null(Y)) Y <- readRDS(paste0(input_path, sub, "Y.rds"))
  if (is.null(Z)) Z <- readRDS(paste0(input_path, sub, "Z_", allocation, ".rds"))
  if (!is.null(extension) && is.null(E)) E <- readRDS(paste0(input_path, E_file))
  if (is.null(L_base))
    L_base <- readRDS(paste0(input_path, sub, year_base,    "_L_", allocation, ".rds"))
  if (is.null(L_curr))
    L_curr <- readRDS(paste0(input_path, sub, year_current, "_L_", allocation, ".rds"))
  
  RN      <- nrow(io)
  C_cells <- which(io$comm_code %in% commodity)     # (country x biofuel) columns of Phi
  nC      <- length(C_cells)
  if (nC == 0L) stop("No io rows match `commodity`.")
  
  # --- Per-year factors ------------------------------------------------------
  # e   : direct intensity per unit output, length RN
  # Phi : RN x nC tier-1 recipe = Z[, C] / X[C]
  # g   : e' L         (length RN)  full upstream intensity of each cell
  # q   : g Phi        (length nC)  footprint per unit of each biofuel cell
  build_factors <- function(yr, L_yr) {
    Xi <- X[, as.character(yr)]
    
    if (!is.null(extension)) {
      Ei <- E[[as.character(yr)]]
      if (is.null(Ei) || length(Ei) == 0L)
        stop("No extension matrix for year ", yr,
             " - check E_bar window (needs yr-1 and yr+1 in E).")
      if (is.null(rownames(Ei)))
        stop("E for year ", yr, " has no rownames - cannot resolve stressors by name.")
      row_sel <- which(rownames(Ei) == extension)
      if (length(row_sel) != 1L)
        stop("Extension '", extension, "' matched ", length(row_sel),
             " of ", nrow(Ei), " stressor rows; expected exactly 1.")
      e_vec <- as.numeric(Ei[row_sel, ])
      if (length(e_vec) != RN)
        stop("Extension row has length ", length(e_vec), " but io has ", RN,
             " rows - E may be transposed.")
      e <- e_vec / as.vector(Xi)
      # Cells with X == 0 and E > 0 have an undefined intensity and are set to zero;
      # their stressor is excluded from all footprints. A cell whose output appears
      # or disappears between adjacent years therefore produces a discontinuous jump
      # in e, which the decomposition books as an `intensity` effect even though
      # nothing technological changed. Check 5 of 18_05 quantifies this component;
      # freeze_test() there re-runs with the extension suppressed on those cells.
      e[!is.finite(e)] <- 0
    } else {
      e <- rep(1, length(Xi))
    }
    
    Zi  <- if (is.list(Z)) Z[[as.character(yr)]] else Z
    inv <- 1 / as.vector(Xi[C_cells]); inv[!is.finite(inv)] <- 0
    Phi <- Zi[, C_cells, drop = FALSE] %*% Diagonal(x = inv)
    
    g <- as.numeric(crossprod(e, L_yr))
    q <- as.numeric(g %*% Phi)
    
    list(e = e, L = L_yr, Phi = Phi, g = g, q = q, Y = Y[[as.character(yr)]])
  }
  
  fac0 <- build_factors(year_base,    L_base)
  fac1 <- build_factors(year_current, L_curr)
  
  # The Phi rewriting is exact only where the biofuel commodities carry no direct
  # extension. Fail loudly rather than silently mis-decompose.
  if (max(abs(fac0$e[C_cells])) > 0 || max(abs(fac1$e[C_cells])) > 0)
    stop("Non-zero direct extension on biofuel cells: F = e'L Phi y fails. See 18_05 check 1.")
  
  # --- Pattern-level Phi quantities: computed ONCE per year-pair -------------
  # Only the kernel K depends on the consumer country; everything here is a
  # property of the year-pair and must not sit inside the country loop.
  phi_pat <- local({
    P0 <- as(fac0$Phi, "TsparseMatrix"); P1 <- as(fac1$Phi, "TsparseMatrix")
    key  <- function(M) M@i * nC + M@j
    keys <- sort(unique(c(key(P0), key(P1))))
    if (length(keys) == 0L) return(NULL)
    
    ii <- as.integer(keys %/% nC) + 1L
    cc <- as.integer(keys %%  nC) + 1L
    
    getx <- function(M) { o <- numeric(length(keys)); o[match(key(M), keys)] <- M@x; o }
    p0 <- getx(P0); p1 <- getx(P1)
    
    grp  <- factor(cc, levels = seq_len(nC))
    csum <- function(v) { s <- as.numeric(tapply(v, grp, sum)); s[is.na(s)] <- 0; s }
    E0 <- csum(p0); E1 <- csum(p1)
    
    f0 <- ifelse(E0[cc] > 0, p0 / E0[cc], NA_real_)
    f1 <- ifelse(E1[cc] > 0, p1 / E1[cc], NA_real_)
    # A column with no production in one period has an undefined recipe there; the
    # mix is copied from the other period, booking the whole change as conversion.
    # This fires at COLUMN level only. An entry going 0 -> positive inside a live
    # column is exactly defined and needs no convention - that is the
    # new-feedstock-entry case.
    f0[is.na(f0)] <- f1[is.na(f0)]; f1[is.na(f1)] <- f0[is.na(f1)]
    f0[is.na(f0)] <- 0; f1[is.na(f1)] <- 0
    
    list(ii = ii, cc = cc,
         dE = E1 - E0, Ebar = (E0 + E1) / 2,
         fbar = (f0 + f1) / 2, dphi = f1 - f0,
         g1 = fac1$g[ii], g0 = fac0$g[ii],
         ug = as.integer(sort(unique(cc))))
  })
  
  split_phi <- function(y0_C, y1_C) {
    if (is.null(phi_pat)) return(list(conv = data.table(), mix = data.table()))
    P <- phi_pat
    K <- (P$g1 * y0_C[P$cc] + P$g0 * y1_C[P$cc]) / 2      # two-polar kernel
    conv_x <- P$dE[P$cc] * P$fbar * K
    
    if (mix_gauge == "centered") {
      num <- as.numeric(rowsum(P$fbar * K, P$cc, reorder = TRUE))
      den <- as.numeric(rowsum(P$fbar,     P$cc, reorder = TRUE)); den[den == 0] <- 1
      Kbar <- numeric(nC); Kbar[P$ug] <- num / den
      mix_x <- P$Ebar[P$cc] * P$dphi * (K - Kbar[P$cc])
    } else {
      mix_x <- P$Ebar[P$cc] * P$dphi * K
    }
    
    list(conv = data.table(row = P$ii, col = P$cc, value = conv_x),
         mix  = data.table(row = P$ii, col = P$cc, value = mix_x,
                           dphi = P$dphi, Ebar = P$Ebar[P$cc], K = K))
  }
  
  # --- Pairwise substitution attribution (optional reporting layer) ----------
  # Proportional (max-entropy) matching of share losses to share gains within a
  # column; sums exactly to the raw-gauge mix effect. THE MATCHING IS AN
  # ASSUMPTION - net share changes do not identify who replaced whom.
  pair_attrib <- function(mixdt) {
    if (nrow(mixdt) == 0L) return(data.table())
    mixdt[, {
      gp <- which(dphi > 0); gn <- which(dphi < 0)
      if (!length(gp) || !length(gn)) {
        data.table(from_row = integer(0), to_row = integer(0), value = numeric(0))
      } else {
        M  <- sum(dphi[gp])
        gr <- expand.grid(l = gn, g = gp)
        fl <- (-dphi[gr$l]) * dphi[gr$g] / M
        data.table(from_row = row[gr$l], to_row = row[gr$g],
                   value = Ebar[1] * fl * (K[gr$g] - K[gr$l]))
      }
    }, by = col]
  }
  
  # --- Country list ----------------------------------------------------------
  if (is.null(country)) country <- unique(fd$iso3c)
  unknown <- setdiff(country, fd$iso3c)
  if (length(unknown) > 0) {
    warning("Unknown countries dropped: ", paste(unknown, collapse = ", "))
    country <- setdiff(country, unknown)
  }
  
  # --- Demand decomposition (restricted to the C cells) ----------------------
  decompose_demand <- function(Yi, ctry) {
    y_C    <- as.vector(rowSums(as.matrix(Yi[, fd$iso3c == ctry, drop = FALSE])))[C_cells]
    totals <- numeric(length(commodity))
    origin <- vector("list", length(commodity)); names(origin) <- commodity
    for (i in seq_along(commodity)) {
      cc <- commodity[i]
      v  <- y_C * (io$comm_code[C_cells] == cc)
      totals[i]    <- sum(v)
      origin[[cc]] <- if (totals[i] > 0) v / totals[i] else v
    }
    s     <- sum(totals)
    sigma <- if (s > 0) totals / s else rep(0, length(commodity))
    names(sigma) <- commodity
    list(s = s, sigma = sigma, m = origin)
  }
  build_y <- function(s, sigma, m) {
    y <- numeric(nC)
    for (i in seq_along(commodity)) y <- y + s * sigma[i] * m[[commodity[i]]]
    y
  }
  
  effect_names <- c("intensity", "sourcing", "feedstock_conv", "feedstock_mix",
                    "scale", "composition", "origin")
  comm_of_col <- io$comm_code[C_cells]
  iso_of_col  <- io$iso3c[C_cells]
  
  # --- Per-country decomposition --------------------------------------------
  sda_country <- function(ctry) {
    dem0 <- decompose_demand(fac0$Y, ctry)
    dem1 <- decompose_demand(fac1$Y, ctry)
    for (cc in commodity) {
      if (sum(dem0$m[[cc]]) == 0 && sum(dem1$m[[cc]]) > 0) dem0$m[[cc]] <- dem1$m[[cc]]
      if (sum(dem1$m[[cc]]) == 0 && sum(dem0$m[[cc]]) > 0) dem1$m[[cc]] <- dem0$m[[cc]]
    }
    if (dem0$s == 0 && dem1$s > 0) dem0$sigma <- dem1$sigma
    if (dem1$s == 0 && dem0$s > 0) dem1$sigma <- dem0$sigma
    
    y0_C <- build_y(dem0$s, dem0$sigma, dem0$m)
    y1_C <- build_y(dem1$s, dem1$sigma, dem1$m)
    
    v0 <- as.numeric(fac0$Phi %*% y0_C)
    v1 <- as.numeric(fac1$Phi %*% y1_C)
    a00 <- as.numeric(fac0$L %*% v0); a10 <- as.numeric(fac1$L %*% v0)
    a01 <- as.numeric(fac0$L %*% v1); a11 <- as.numeric(fac1$L %*% v1)
    
    F0 <- sum(fac0$e * a00)
    F1 <- sum(fac1$e * a11)
    
    delta_e_cell <- (fac1$e - fac0$e) * (a00 + a11) / 2
    delta_L_cell <- (fac1$e * (a10 - a00) + fac0$e * (a11 - a01)) / 2
    
    sp <- split_phi(y0_C, y1_C)
    
    eLm_A <- vapply(commodity, function(cc) sum(fac1$q * dem0$m[[cc]]), numeric(1))
    eLm_B <- vapply(commodity, function(cc) sum(fac0$q * dem1$m[[cc]]), numeric(1))
    
    delta_s_c     <- (dem1$s - dem0$s) *
      (dem0$sigma * eLm_A + dem1$sigma * eLm_B) / 2
    delta_sigma_c <- (dem1$sigma - dem0$sigma) *
      (dem1$s * eLm_A + dem0$s * eLm_B) / 2
    
    delta_m_cj <- vector("list", length(commodity)); names(delta_m_cj) <- commodity
    for (cc in commodity) {
      kern <- (dem1$s * dem1$sigma[cc] * fac1$q +
                 dem0$s * dem0$sigma[cc] * fac0$q) / 2
      delta_m_cj[[cc]] <- (dem1$m[[cc]] - dem0$m[[cc]]) * kern
    }
    
    deltas <- c(
      intensity      = sum(delta_e_cell),
      sourcing       = sum(delta_L_cell),
      feedstock_conv = if (nrow(sp$conv)) sum(sp$conv$value) else 0,
      feedstock_mix  = if (nrow(sp$mix))  sum(sp$mix$value)  else 0,
      scale          = sum(delta_s_c),
      composition    = sum(delta_sigma_c),
      origin         = sum(unlist(delta_m_cj))
    )
    
    scalar_dt <- data.table(
      country_consumer = ctry,
      effect = c(effect_names, "total_t0", "total_t1", "delta"),
      value  = c(deltas, F0, F1, F1 - F0))
    
    if (!breakdown || (F0 == 0 && F1 == 0))
      return(list(scalars = scalar_dt, breakdown = data.table(), pairs = data.table()))
    
    # Raw driver granularity is (RN origin cell x nC biofuel cell); columns are
    # aggregated to commodity code unless col_detail = "cell".
    phi_tab <- function(dt, eff) {
      if (!nrow(dt)) return(NULL)
      # In the centered gauge a zero-intensity feedstock with a moving share has a
      # NON-zero contribution, so dropping zeros is safe. In the raw gauge it does
      # not, so rows with a share change are retained explicitly.
      keep <- if (mix_gauge == "raw" && eff == "feedstock_mix")
        (dt$value != 0 | dt$dphi != 0) else dt$value != 0
      dt <- dt[keep]
      if (!nrow(dt)) return(NULL)
      data.table(country_consumer = ctry, effect = eff,
                 country_origin = io$iso3c[dt$row],
                 item_origin    = io$item[dt$row],
                 commodity      = if (col_detail == "commodity") comm_of_col[dt$col]
                 else paste0(comm_of_col[dt$col], "@", iso_of_col[dt$col]),
                 value = dt$value
      )[, .(value = sum(value)),
        by = .(country_consumer, effect, country_origin, item_origin, commodity)]
    }
    
    bd <- rbindlist(list(
      data.table(country_consumer = ctry, effect = "intensity",
                 country_origin = io$iso3c, item_origin = io$item,
                 commodity = NA_character_, value = delta_e_cell)[value != 0],
      data.table(country_consumer = ctry, effect = "sourcing",
                 country_origin = io$iso3c, item_origin = io$item,
                 commodity = NA_character_, value = delta_L_cell)[value != 0],
      phi_tab(sp$conv, "feedstock_conv"),
      phi_tab(sp$mix,  "feedstock_mix"),
      data.table(country_consumer = ctry, effect = "composition",
                 country_origin = NA_character_, item_origin = NA_character_,
                 commodity = commodity, value = delta_sigma_c)[value != 0],
      data.table(country_consumer = ctry, effect = "scale",
                 country_origin = NA_character_, item_origin = NA_character_,
                 commodity = commodity, value = delta_s_c)[value != 0],
      rbindlist(lapply(commodity, function(cc)
        data.table(country_consumer = ctry, effect = "origin",
                   country_origin = io$iso3c[C_cells], item_origin = io$item[C_cells],
                   commodity = cc, value = delta_m_cj[[cc]])))[value != 0]
    ), use.names = TRUE, fill = TRUE)
    
    pr <- data.table()
    if (pairwise && nrow(sp$mix)) {
      pr <- pair_attrib(sp$mix)
      if (nrow(pr)) {
        pr <- data.table(country_consumer = ctry,
                         commodity = comm_of_col[pr$col],
                         from_item = io$item[pr$from_row],
                         to_item   = io$item[pr$to_row],
                         value     = pr$value)[value != 0]
        # A pair with from_item == to_item is not a substitution: it is a shift
        # between origins of the SAME feedstock inside one recipe column. Tag it so
        # the substitution table can be filtered without discarding the mass.
        pr[, kind := fifelse(from_item == to_item, "origin_shift", "substitution")]
        pr <- pr[, .(value = sum(value)),
                 by = .(country_consumer, commodity, from_item, to_item, kind)]
      }
    }
    
    list(scalars = scalar_dt, breakdown = bd, pairs = pr)
  }
  
  res_list       <- lapply(country, sda_country)
  scalar_results <- rbindlist(lapply(res_list, `[[`, "scalars"))
  bd_results     <- rbindlist(lapply(res_list, `[[`, "breakdown"), use.names = TRUE, fill = TRUE)
  pair_results   <- rbindlist(lapply(res_list, `[[`, "pairs"),     use.names = TRUE, fill = TRUE)
  
  indicator_label <- if (!is.null(extension)) extension else "material"
  meta <- function(dt) { if (nrow(dt))
    dt[, `:=`(year_base = year_base, year_current = year_current,
              indicator = indicator_label, allocation = allocation,
              mix_gauge = mix_gauge)]; dt }
  scalar_results <- meta(scalar_results)
  scalar_results[, commodities := paste(commodity, collapse = "-")]
  setcolorder(scalar_results, c("country_consumer", "year_base", "year_current",
                                "indicator", "allocation", "commodities",
                                "effect", "value"))
  bd_results   <- meta(bd_results)
  pair_results <- meta(pair_results)
  if (nrow(bd_results))
    setcolorder(bd_results, c("country_consumer", "year_base", "year_current",
                              "indicator", "allocation", "effect",
                              "country_origin", "item_origin", "commodity", "value"))
  
  if (save) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    fname <- build_filename("FABIO_SDA",
                            country = if (length(country) > 4) NULL else country,
                            yearBase = year_base, yearCurr = year_current,
                            indicator = indicator_label, alloc = allocation,
                            comm = commodity)
    fwrite(scalar_results, file.path(output_dir, fname))
    message("Wrote ", file.path(output_dir, fname))
    if (breakdown) {
      fbd <- sub("\\.csv$", "_drivers.csv", fname)
      fwrite(bd_results, file.path(output_dir, fbd)); message("Wrote ", file.path(output_dir, fbd))
    }
    if (nrow(pair_results)) {
      fpr <- sub("\\.csv$", "_pairs.csv", fname)
      fwrite(pair_results, file.path(output_dir, fpr)); message("Wrote ", file.path(output_dir, fpr))
    }
  }
  
  if (breakdown) list(scalars = scalar_results, drivers = bd_results, pairs = pair_results)
  else scalar_results
}


###########################################################
########### CHAINED SDA (year-on-year)
###########################################################

fp_sda_chained <- function(years      = 2012:2022,
                           country    = NULL,
                           extension  = NULL,
                           commodity,
                           allocation = "value",
                           input_path = "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/",
                           losses     = TRUE,
                           breakdown  = FALSE,
                           mix_gauge  = "centered",
                           col_detail = "commodity",
                           pairwise   = FALSE,
                           save       = FALSE,
                           output_dir = OUT_DIR,
                           regions, io, fd, ex,
                           X = NULL, Y = NULL, Z = NULL, E = NULL) {
  
  stopifnot(length(years) >= 2)
  years <- sort(unique(years))
  sub   <- if (losses) "losses/" else ""
  
  load_L <- function(yr) {
    message("Loading L for ", yr, " ...")
    readRDS(paste0(input_path, sub, yr, "_L_", allocation, ".rds"))
  }
  
  L_curr <- load_L(years[1])
  steps  <- vector("list", length(years) - 1)
  
  for (i in seq_len(length(years) - 1)) {
    t0 <- years[i]; t1 <- years[i + 1]
    L_base <- L_curr
    L_curr <- load_L(t1)
    message("SDA: ", t0, " -> ", t1)
    steps[[i]] <- fp_sda(
      country = country, year_base = t0, year_current = t1,
      extension = extension, commodity = commodity, allocation = allocation,
      input_path = input_path, losses = losses, breakdown = breakdown,
      mix_gauge = mix_gauge, col_detail = col_detail, pairwise = pairwise,
      save = FALSE, regions = regions, io = io, fd = fd, ex = ex,
      X = X, Y = Y, Z = Z, E = E, L_base = L_base, L_curr = L_curr)
  }
  
  out <- if (breakdown) list(
    scalars = rbindlist(lapply(steps, `[[`, "scalars"), use.names = TRUE, fill = TRUE),
    drivers = rbindlist(lapply(steps, `[[`, "drivers"), use.names = TRUE, fill = TRUE),
    pairs   = rbindlist(lapply(steps, `[[`, "pairs"),   use.names = TRUE, fill = TRUE)
  ) else rbindlist(steps, use.names = TRUE, fill = TRUE)
  
  if (save) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    lbl   <- if (!is.null(extension)) extension else "material"
    fname <- build_filename("FABIO_SDA_chained",
                            yearBase = min(years), yearCurr = max(years),
                            indicator = lbl, alloc = allocation, comm = commodity)
    if (breakdown) {
      fwrite(out$scalars, file.path(output_dir, fname))
      fwrite(out$drivers, file.path(output_dir, sub("\\.csv$", "_drivers.csv", fname)))
      if (nrow(out$pairs))
        fwrite(out$pairs, file.path(output_dir, sub("\\.csv$", "_pairs.csv", fname)))
    } else fwrite(out, file.path(output_dir, fname))
    message("Wrote ", file.path(output_dir, fname))
  }
  out
}


###########################################################
########### SMOOTHED SDA (multi-year endpoint averaging)
###########################################################

fp_sda_smoothed <- function(years_base    = 2012:2014,
                            years_current = 2020:2022,
                            country       = NULL,
                            extension     = NULL,
                            commodity,
                            allocation    = "value",
                            input_path    = "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/",
                            losses        = TRUE,
                            breakdown     = FALSE,
                            mix_gauge     = "centered",
                            col_detail    = "commodity",
                            pairwise      = FALSE,
                            save          = FALSE,
                            output_dir    = OUT_DIR,
                            regions, io, fd, ex,
                            X = NULL, Y = NULL, Z = NULL, E = NULL,
                            L_method      = c("average", "midyear")) {
  
  L_method <- match.arg(L_method)
  sub      <- if (losses) "losses/" else ""
  
  if (is.null(X)) X <- readRDS(paste0(input_path, sub, "X.rds"))
  if (is.null(Y)) Y <- readRDS(paste0(input_path, sub, "Y.rds"))
  if (is.null(Z)) Z <- readRDS(paste0(input_path, sub, "Z_", allocation, ".rds"))
  if (!is.null(extension) && is.null(E)) E <- readRDS(paste0(input_path, E_file))
  
  # X, Y, Z and E are all averaged within the window. Z must be averaged alongside
  # X, since Phi = Z[, C] / X[C] and mixing a window mean with a single year would
  # give a recipe that balances against neither endpoint.
  avg_window <- function(years) {
    yrs <- as.character(years)
    list(X = rowMeans(X[, yrs, drop = FALSE]),
         Y = Reduce("+", Y[yrs]) / length(yrs),
         Z = Reduce("+", if (is.list(Z)) Z[yrs] else list(Z)) / length(yrs),
         E = if (!is.null(extension)) Reduce("+", E[yrs]) / length(yrs) else NULL)
  }
  
  build_L_window <- function(years) {
    yrs <- as.character(years)
    if (L_method == "average") {
      Reduce("+", lapply(yrs, function(yr) {
        message("  loading L for ", yr)
        readRDS(paste0(input_path, sub, yr, "_L_", allocation, ".rds"))
      })) / length(yrs)
    } else {
      midyr <- years[ceiling(length(years) / 2)]
      message("  loading L for midyear ", midyr)
      readRDS(paste0(input_path, sub, midyr, "_L_", allocation, ".rds"))
    }
  }
  
  message("Building averaged endpoint t0 from: ", paste(years_base, collapse = ", "))
  ep0    <- avg_window(years_base);    L_base <- build_L_window(years_base)
  message("Building averaged endpoint t1 from: ", paste(years_current, collapse = ", "))
  ep1    <- avg_window(years_current); L_curr <- build_L_window(years_current)
  
  lab0 <- paste0(min(years_base),    "-", max(years_base))
  lab1 <- paste0(min(years_current), "-", max(years_current))
  
  X_s <- cbind(ep0$X, ep1$X); colnames(X_s) <- c(lab0, lab1)
  Y_s <- setNames(list(ep0$Y, ep1$Y), c(lab0, lab1))
  Z_s <- setNames(list(ep0$Z, ep1$Z), c(lab0, lab1))
  E_s <- if (!is.null(extension)) setNames(list(ep0$E, ep1$E), c(lab0, lab1)) else NULL
  
  results <- fp_sda(
    country = country, year_base = lab0, year_current = lab1,
    extension = extension, commodity = commodity, allocation = allocation,
    input_path = input_path, losses = losses, breakdown = breakdown,
    mix_gauge = mix_gauge, col_detail = col_detail, pairwise = pairwise,
    save = FALSE, regions = regions, io = io, fd = fd, ex = ex,
    X = X_s, Y = Y_s, Z = Z_s, E = E_s, L_base = L_base, L_curr = L_curr)
  
  if (save) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    lbl   <- if (!is.null(extension)) extension else "material"
    fname <- paste0("FABIO_SDA_smoothed_", lab0, "_vs_", lab1, "_", lbl, "_",
                    allocation, "_", paste(commodity, collapse = "-"), ".csv")
    if (breakdown) {
      fwrite(results$scalars, file.path(output_dir, fname))
      fwrite(results$drivers, file.path(output_dir, sub("\\.csv$", "_drivers.csv", fname)))
      if (nrow(results$pairs))
        fwrite(results$pairs, file.path(output_dir, sub("\\.csv$", "_pairs.csv", fname)))
    } else fwrite(results, file.path(output_dir, fname))
    message("Wrote ", file.path(output_dir, fname))
  }
  results
}


###########################################################
########### GUARD: confirm the seven-term version is in scope
###########################################################
# fp_sda must report seven terms over six factors. If a stale definition is ever
# re-introduced or sourced from elsewhere, fail loudly instead of silently writing
# columns that 19_02 cannot plot.
stopifnot(
  identical(
    eval(body(fp_sda)[[which(vapply(as.list(body(fp_sda)),
                                    function(x) any(grepl("effect_names <- ", deparse(x))),
                                    logical(1)))]][[3]]),
    c("intensity", "sourcing", "feedstock_conv", "feedstock_mix",
      "scale", "composition", "origin")
  )
)


###########################################################
########### RUN
###########################################################

SDA_chain <- fp_sda_chained(
  years      = 2012:2022,
  extension  = "ibif_total",
  commodity  = c("c146", "c147", "c149"),
  breakdown  = TRUE,
  mix_gauge  = "centered",
  pairwise   = TRUE,
  save       = TRUE,
  regions = regions, io = io, fd = fd, ex = ex,
  X = X, Y = Y, Z = Z, E = E_bar
)

# SDA_smoothed <- fp_sda_smoothed(
#   years_base = 2012:2014, years_current = 2020:2022,
#   extension = "ibif_total", commodity = c("c146", "c147", "c149"),
#   breakdown = TRUE, save = TRUE,
#   regions = regions, io = io, fd = fd, ex = ex,
#   X = X, Y = Y, Z = Z, E = E_bar)


###########################################################
########### SANITY CHECKS
###########################################################

six <- c("intensity", "sourcing", "feedstock_conv", "feedstock_mix",
         "scale", "composition", "origin")

# 0) The seven terms are present
stopifnot(setequal(
  setdiff(unique(SDA_chain$scalars$effect), c("total_t0", "total_t1", "delta")), six))

# 1) Per-country, per year-pair: effects sum to delta
SDA_chain$scalars[, .(check = sum(value[effect %in% six]) - value[effect == "delta"]),
                  by = .(country_consumer, year_current)][, max(abs(check))]   # ~1e-11

# 2) Per-element drivers sum to the scalar effects
merge(SDA_chain$drivers[, .(bd = sum(value)), by = .(country_consumer, year_current, effect)],
      SDA_chain$scalars[effect %in% six,
                        .(country_consumer, year_current, effect, sc = value)],
      by = c("country_consumer", "year_current", "effect"))[, max(abs(bd - sc))]  # ~1e-11

drv <- SDA_chain$drivers
eu  <- regions[continent == "EU", iso3c]

# Which feedstocks gained or lost share in the recipe (EU consumers)
drv[effect == "feedstock_mix" & country_consumer %in% eu,
    .(value = sum(value)), by = item_origin][order(value)][1:20]

# Conversion efficiency, by biofuel
drv[effect == "feedstock_conv" & country_consumer %in% eu,
    .(value = sum(value)), by = .(commodity, year_current)][order(commodity, year_current)]

# Substitution table: who displaced whom (origin shifts excluded)
SDA_chain$pairs[country_consumer %in% eu & kind == "substitution",
                .(value = sum(value)), by = .(from_item, to_item)][order(value)][1:20]