# 18_02 - Structural decomposition analysis of footprints

# Setup ------------------------------------------------------------------------
library(data.table)
library(Matrix)
library(tidyverse)
source("R/00_system_variables.R")

setwd("/home/mmondolfo/fabio_bfp/")

# Read labels ------------------------------------------------------------------
input_path <- "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/"
regions <- fread(file="inst/regions_full.csv") %>% filter(current==TRUE)
io <- fread(paste0(input_path,"io_labels.csv"))
items <- fread(file="inst/items_full_bcp.csv") %>% filter(comm_code %in% unique(io$comm_code))
fd <- fread(file=paste0(input_path,"losses/fd_labels.csv"))
ex <- fread(file=paste0(input_path,"ex_labels.csv"))

# Create output directory ----------------------------------------
dir.create("output", showWarnings = FALSE, recursive = TRUE)

# Load static data once (all years, value allocation) -------------------------
# X, Y, Z, E are year-independent containers and only need to be read once.
# L is year-specific and is loaded inside each function call.
allocation <- "value"

X <- readRDS(paste0(input_path, "losses/X.rds"))
Y <- readRDS(paste0(input_path, "losses/Y.rds"))
Z <- readRDS(paste0(input_path, "losses/Z_", allocation, ".rds"))
E <- readRDS(paste0(input_path, "E.rds"))

# Make E_bar, 3-years average of the environmental extensions.
yrs_E_bar <- as.character(2012:2022)
E_bar <- vector("list", length(yrs_E_bar))
names(E_bar) <- yrs_E_bar

for (yr in yrs_E_bar) {
  y <- as.integer(yr)
  E_bar[[yr]] <- (E[[as.character(y - 1)]] + E[[as.character(y)]] + E[[as.character(y + 1)]]) / 3
} 


######################################################################################################################
############################## FUNCTIONS TO SAVE RESULTS (FILENAMES) #######################
######################################################################################################################
# --- Filename helpers ----------------------------------------------------
commodity_slug <- function(commodity) {
  if (is.null(commodity)) return("all")
  
  bf_set <- c("c146", "c147", "c149", "c150", "c151")
  bp_set <- paste0("c", 152:170)
  
  in_bf <- commodity %in% bf_set
  in_bp <- commodity %in% bp_set
  
  # Apply BF / BP grouping only when every commodity belongs to BF ∪ BP
  if (all(in_bf | in_bp)) {
    has_bf <- any(in_bf)
    has_bp <- any(in_bp)
    if (has_bf && has_bp) return("BF_BP")
    if (has_bf)           return("BF")
    if (has_bp)           return("BP")
  }
  
  # Fallback: keep existing behaviour
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
  segs  <- mapply(fmt_part, names(parts), parts,
                  SIMPLIFY = FALSE, USE.NAMES = FALSE)
  segs  <- Filter(Negate(is.null), segs)
  paste0(prefix, "_", paste(segs, collapse = "_"), ".", ext)
}





###########################################################
########### STRUCTURAL DECOMPOSITION ANALYSIS
########### OF CONSUMPTION-BASED FOOTPRINT
########### Effects: intensity, technique, scale, composition, origin
###########################################################

fp_sda <- function(country         = NULL,
                   year_base,
                   year_current,
                   extension       = NULL,
                   commodity,
                   allocation      = "value",
                   input_path      = "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/",
                   losses          = TRUE,
                   breakdown       = FALSE,                # NEW
                   save            = FALSE,
                   output_dir      = "output",
                   regions, io, fd, ex,
                   X = NULL, Y = NULL, E = NULL,
                   L_base = NULL, L_curr = NULL) {
  
  sub <- if (losses) "losses/" else ""
  
  # --- Load static data ----------------------------------------------------
  if (is.null(X)) X <- readRDS(paste0(input_path, sub, "X.rds"))
  if (is.null(Y)) Y <- readRDS(paste0(input_path, sub, "Y.rds"))
  if (!is.null(extension) && is.null(E))
    E <- readRDS(paste0(input_path, "E.rds"))
  if (is.null(L_base))
    L_base <- readRDS(paste0(input_path, sub, year_base,    "_L_", allocation, ".rds"))
  if (is.null(L_curr))
    L_curr <- readRDS(paste0(input_path, sub, year_current, "_L_", allocation, ".rds"))
  
  # --- Per-year factors ----------------------------------------------------
  build_factors <- function(yr, L_yr) {
    Xi <- X[, as.character(yr)]
    Yi <- Y[[as.character(yr)]]
    if (!is.null(extension)) {
      Ei <- E[[as.character(yr)]]
      e <- as.numeric(Ei[ex$Stressor == extension, ]) / as.vector(Xi)
      e[!is.finite(e)] <- 0
    } else {
      e <- rep(1, length(Xi))
    }
    list(e = e, L = L_yr, X = Xi, Y = Yi)
  }
  fac0 <- build_factors(year_base,    L_base)
  fac1 <- build_factors(year_current, L_curr)
  
  if (is.null(country)) country <- unique(fd$iso3c)
  unknown <- setdiff(country, fd$iso3c)
  if (length(unknown) > 0) {
    warning("Unknown countries dropped: ", paste(unknown, collapse = ", "))
    country <- setdiff(country, unknown)
  }
  
  # --- Demand decomposition (s, sigma, m) ---------------------------------
  decompose_demand <- function(Yi, ctry) {
    Yc <- Yi[, fd$iso3c == ctry, drop = FALSE]
    y_full <- as.vector(rowSums(as.matrix(Yc)))
    totals <- numeric(length(commodity))
    origin <- vector("list", length(commodity)); names(origin) <- commodity
    for (i in seq_along(commodity)) {
      cc <- commodity[i]
      idx <- io$comm_code == cc
      y_cc <- y_full * idx
      totals[i] <- sum(y_cc)
      origin[[cc]] <- if (totals[i] > 0) y_cc / totals[i] else y_cc
    }
    s     <- sum(totals)
    sigma <- if (s > 0) totals / s else rep(0, length(commodity))
    names(sigma) <- commodity
    list(s = s, sigma = sigma, m = origin)
  }
  
  build_y <- function(s, sigma, m) {
    y <- numeric(nrow(io))
    for (i in seq_along(commodity)) y <- y + s * sigma[i] * m[[commodity[i]]]
    y
  }
  
  effect_names  <- c("intensity", "technique", "scale", "composition", "origin")
  
  # 4-factor "other" configs (shared across effects) and Shapley weights
  other_configs <- as.matrix(expand.grid(replicate(4, 0:1, simplify = FALSE)))
  card <- rowSums(other_configs)
  sw   <- factorial(card) * factorial(4 - card) / factorial(5)   # sums to 1
  
  pick <- function(v0, v1, v) if (v == 1) v1 else v0
  
  sda_country <- function(ctry) {
    dem0 <- decompose_demand(fac0$Y, ctry)
    dem1 <- decompose_demand(fac1$Y, ctry)
    
    for (cc in commodity) {
      if (sum(dem0$m[[cc]]) == 0 && sum(dem1$m[[cc]]) > 0) dem0$m[[cc]] <- dem1$m[[cc]]
      if (sum(dem1$m[[cc]]) == 0 && sum(dem0$m[[cc]]) > 0) dem1$m[[cc]] <- dem0$m[[cc]]
    }
    if (dem0$s == 0 && dem1$s > 0) dem0$sigma <- dem1$sigma
    if (dem1$s == 0 && dem0$s > 0) dem1$sigma <- dem0$sigma
    
    nC <- length(commodity); RN <- length(fac0$e)
    
    # ---- Caches over factor specs (0 = base, 1 = current) ----
    y_cache  <- array(list(), c(2, 2, 2))                # [s, sig, m]
    for (sv in 0:1) for (gv in 0:1) for (mv in 0:1)
      y_cache[[sv+1, gv+1, mv+1]] <- build_y(
        pick(dem0$s,     dem1$s,     sv),
        pick(dem0$sigma, dem1$sigma, gv),
        pick(dem0$m,     dem1$m,     mv))
    
    Ly_cache <- array(list(), c(2, 2, 2, 2))             # [L, s, sig, m]
    for (lv in 0:1) for (sv in 0:1) for (gv in 0:1) for (mv in 0:1)
      Ly_cache[[lv+1, sv+1, gv+1, mv+1]] <- as.numeric(
        pick(fac0$L, fac1$L, lv) %*% y_cache[[sv+1, gv+1, mv+1]])
    
    eL_cache <- array(list(), c(2, 2))                   # [e, L]
    for (ev in 0:1) for (lv in 0:1)
      eL_cache[[ev+1, lv+1]] <- as.numeric(crossprod(
        pick(fac0$e, fac1$e, ev), pick(fac0$L, fac1$L, lv)))
    
    eLm_cache <- array(0, c(2, 2, 2, nC))                # [e, L, m, c]
    for (ev in 0:1) for (lv in 0:1) for (mv in 0:1) for (ci in seq_len(nC)) {
      cc <- commodity[ci]
      m_v <- pick(dem0$m, dem1$m, mv)[[cc]]
      eLm_cache[ev+1, lv+1, mv+1, ci] <- sum(eL_cache[[ev+1, lv+1]] * m_v)
    }
    
    F_cache <- array(0, c(2, 2, 2, 2, 2))                # [e, L, s, sig, m]
    for (ev in 0:1) for (lv in 0:1) for (sv in 0:1) for (gv in 0:1) for (mv in 0:1) {
      s_v   <- pick(dem0$s,     dem1$s,     sv)
      sig_v <- pick(dem0$sigma, dem1$sigma, gv)
      F_cache[ev+1, lv+1, sv+1, gv+1, mv+1] <-
        s_v * sum(sig_v * eLm_cache[ev+1, lv+1, mv+1, ])
    }
    
    F0 <- F_cache[1,1,1,1,1]; F1 <- F_cache[2,2,2,2,2]
    
    # ---- Scalar Shapley effects (unchanged math, cache-driven) ----
    deltas <- numeric(5); names(deltas) <- effect_names
    for (k in 1:5) {
      others <- setdiff(1:5, k); total <- 0
      for (i in seq_len(16)) {
        cfg <- other_configs[i, ]
        s1 <- s0 <- numeric(5); s1[others] <- cfg; s0[others] <- cfg
        s1[k] <- 1; s0[k] <- 0
        total <- total + sw[i] * (
          F_cache[s1[1]+1, s1[2]+1, s1[3]+1, s1[4]+1, s1[5]+1] -
            F_cache[s0[1]+1, s0[2]+1, s0[3]+1, s0[4]+1, s0[5]+1])
      }
      deltas[k] <- total
    }
    
    scalar_dt <- data.table(
      country_consumer = ctry,
      effect           = c(effect_names, "total_t0", "total_t1", "delta"),
      value            = c(deltas, F0, F1, F1 - F0))
    
    if (F0 == 0 && F1 == 0) {
      if (breakdown) return(list(scalars = scalar_dt, breakdown = data.table()))
      return(scalar_dt)
    }
    if (!breakdown) return(scalar_dt)
    
    # ---- Per-element decomposition ----
    
    # Δ_e per RN cell: others = (L, s, sig, m)
    A_e <- numeric(RN)
    for (i in seq_len(16)) {
      cfg <- other_configs[i, ]
      A_e <- A_e + sw[i] * Ly_cache[[cfg[1]+1, cfg[2]+1, cfg[3]+1, cfg[4]+1]]
    }
    delta_e_cell <- (fac1$e - fac0$e) * A_e
    
    # Δ_L per RN cell: others = (e, s, sig, m)
    delta_L_cell <- numeric(RN)
    for (i in seq_len(16)) {
      cfg <- other_configs[i, ]
      e_v <- pick(fac0$e, fac1$e, cfg[1])
      dLy <- Ly_cache[[2, cfg[2]+1, cfg[3]+1, cfg[4]+1]] -
        Ly_cache[[1, cfg[2]+1, cfg[3]+1, cfg[4]+1]]
      delta_L_cell <- delta_L_cell + sw[i] * e_v * dLy
    }
    
    # Δ_σ per commodity: others = (e, L, s, m)
    A_sigma <- numeric(nC)
    for (i in seq_len(16)) {
      cfg <- other_configs[i, ]
      s_v <- pick(dem0$s, dem1$s, cfg[3])
      A_sigma <- A_sigma + sw[i] * s_v * eLm_cache[cfg[1]+1, cfg[2]+1, cfg[4]+1, ]
    }
    delta_sigma_c <- (dem1$sigma - dem0$sigma) * A_sigma
    
    # Δ_s per commodity: others = (e, L, sig, m)
    A_s <- numeric(nC)
    for (i in seq_len(16)) {
      cfg <- other_configs[i, ]
      sig_v <- pick(dem0$sigma, dem1$sigma, cfg[3])
      A_s <- A_s + sw[i] * sig_v * eLm_cache[cfg[1]+1, cfg[2]+1, cfg[4]+1, ]
    }
    delta_s_c <- (dem1$s - dem0$s) * A_s
    
    # Δ_m per (commodity, RN cell): others = (e, L, s, sig)
    delta_m_cj <- vector("list", nC); names(delta_m_cj) <- commodity
    for (ci in seq_len(nC)) {
      cc <- commodity[ci]
      A_m_c <- numeric(RN)
      for (i in seq_len(16)) {
        cfg <- other_configs[i, ]
        s_v   <- pick(dem0$s,     dem1$s,     cfg[3])
        sig_v <- pick(dem0$sigma, dem1$sigma, cfg[4])[cc]
        A_m_c <- A_m_c + sw[i] * s_v * sig_v * eL_cache[[cfg[1]+1, cfg[2]+1]]
      }
      delta_m_cj[[cc]] <- A_m_c * (dem1$m[[cc]] - dem0$m[[cc]])
    }
    
    # ---- Assemble breakdown table ----
    bd <- rbindlist(list(
      data.table(country_consumer = ctry, effect = "intensity",
                 country_origin = io$iso3c, item_origin = io$item,
                 commodity = NA_character_, value = delta_e_cell),
      data.table(country_consumer = ctry, effect = "technique",
                 country_origin = io$iso3c, item_origin = io$item,
                 commodity = NA_character_, value = delta_L_cell),
      data.table(country_consumer = ctry, effect = "composition",
                 country_origin = NA_character_, item_origin = NA_character_,
                 commodity = commodity, value = delta_sigma_c),
      data.table(country_consumer = ctry, effect = "scale",
                 country_origin = NA_character_, item_origin = NA_character_,
                 commodity = commodity, value = delta_s_c),
      rbindlist(lapply(commodity, function(cc) {
        data.table(country_consumer = ctry, effect = "origin",
                   country_origin = io$iso3c, item_origin = io$item,
                   commodity = cc, value = delta_m_cj[[cc]])
      }))
    ), use.names = TRUE, fill = TRUE)
    
    bd <- bd[value != 0]
    list(scalars = scalar_dt, breakdown = bd)
  }
  
  # ---- Loop over consumer countries ----
  if (breakdown) {
    res_list       <- lapply(country, sda_country)
    scalar_results <- rbindlist(lapply(res_list, `[[`, "scalars"))
    bd_results     <- rbindlist(lapply(res_list, `[[`, "breakdown"),
                                use.names = TRUE, fill = TRUE)
  } else {
    scalar_results <- rbindlist(lapply(country, sda_country))
  }
  
  indicator_label <- if (!is.null(extension)) extension else "material"
  scalar_results[, `:=`(year_base = year_base, year_current = year_current,
                        indicator = indicator_label, allocation = allocation,
                        commodities = paste(commodity, collapse = "-"))]
  setcolorder(scalar_results, c("country_consumer", "year_base", "year_current",
                                "indicator", "allocation", "commodities",
                                "effect", "value"))
  
  if (breakdown && nrow(bd_results) > 0) {
    bd_results[, `:=`(year_base = year_base, year_current = year_current,
                      indicator = indicator_label, allocation = allocation)]
    setcolorder(bd_results, c("country_consumer", "year_base", "year_current",
                              "indicator", "allocation", "effect",
                              "country_origin", "item_origin", "commodity", "value"))
  }
  
  if (save) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    fname <- build_filename("FABIO_SDA",
                            country   = if (length(country) > 4) NULL else country,
                            yearBase  = year_base, yearCurr = year_current,
                            indicator = indicator_label, alloc = allocation,
                            comm      = commodity)
    fwrite(scalar_results, file.path(output_dir, fname))
    message("Wrote ", file.path(output_dir, fname))
    if (breakdown) {
      fname_bd <- sub("\\.csv$", "_drivers.csv", fname)
      fwrite(bd_results, file.path(output_dir, fname_bd))
      message("Wrote ", file.path(output_dir, fname_bd))
    }
  }
  
  if (breakdown) list(scalars = scalar_results, drivers = bd_results) else scalar_results
}


# sda <- fp_sda(
#   year_base    = 2013,
#   year_current = 2022,
#   extension    = "land_harv",
#   commodity    = c("c146", "c147", "c149"),
#   save         = TRUE,
#   regions = regions, io = io, fd = fd, ex = ex,
#   X = X, Y = Y, E = E_bar
# )
# 



###########################################################
########### STRUCTURAL DECOMPOSITION ANALYSIS
########### OF CONSUMPTION-BASED FOOTPRINT
########### Chained: year-on-year. 
###########################################################

fp_sda_chained <- function(years           = 2012:2022,
                           country         = NULL,
                           extension       = NULL,
                           commodity,
                           allocation      = "value",
                           input_path      = "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/",
                           losses          = TRUE,
                           breakdown       = FALSE,           # NEW
                           save            = FALSE,
                           output_dir      = "output",
                           regions, io, fd, ex,
                           X = NULL, Y = NULL, E = NULL) {
  
  stopifnot(length(years) >= 2)
  years <- sort(unique(years))
  sub <- if (losses) "losses/" else ""
  
  if (is.null(X)) X <- readRDS(paste0(input_path, sub, "X.rds"))
  if (is.null(Y)) Y <- readRDS(paste0(input_path, sub, "Y.rds"))
  if (!is.null(extension) && is.null(E))
    E <- readRDS(paste0(input_path, "E.rds"))
  
  load_L <- function(yr) {
    message("Loading L for ", yr, " ...")
    readRDS(paste0(input_path, sub, yr, "_L_", allocation, ".rds"))
  }
  
  L_curr <- load_L(years[1])
  step_results <- vector("list", length(years) - 1)
  
  for (i in seq_len(length(years) - 1)) {
    t0 <- years[i]; t1 <- years[i + 1]
    L_base <- L_curr
    L_curr <- load_L(t1)
    
    message("SDA: ", t0, " -> ", t1)
    step_results[[i]] <- fp_sda(
      country      = country,
      year_base    = t0,
      year_current = t1,
      extension    = extension,
      commodity    = commodity,
      allocation   = allocation,
      input_path   = input_path,
      losses       = losses,
      breakdown    = breakdown,
      save         = FALSE,
      regions      = regions, io = io, fd = fd, ex = ex,
      X = X, Y = Y, E = E,
      L_base = L_base, L_curr = L_curr
    )
  }
  
  if (breakdown) {
    scalar_results <- rbindlist(lapply(step_results, `[[`, "scalars"))
    bd_results     <- rbindlist(lapply(step_results, `[[`, "drivers"),
                                use.names = TRUE, fill = TRUE)
  } else {
    scalar_results <- rbindlist(step_results)
  }
  
  if (save) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    indicator_label <- if (!is.null(extension)) extension else "material"
    fname <- build_filename(
      "FABIO_SDA_chained",
      yearBase  = min(years), yearCurr = max(years),
      indicator = indicator_label, alloc = allocation,
      comm = commodity)
    fwrite(scalar_results, file.path(output_dir, fname))
    message("Wrote ", file.path(output_dir, fname))
    if (breakdown) {
      fname_bd <- sub("\\.csv$", "_drivers.csv", fname)
      fwrite(bd_results, file.path(output_dir, fname_bd))
      message("Wrote ", file.path(output_dir, fname_bd))
    }
  }
  
  if (breakdown) list(scalars = scalar_results, drivers = bd_results) else scalar_results
}




###########################################################
########### STRUCTURAL DECOMPOSITION ANALYSIS
########### Two-polar (Dietzenbacher–Los 1998) decomposition.
########### Effects: intensity, feedstock_mix, sourcing, scale, composition, origin
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
                   save            = FALSE,
                   output_dir      = "output",
                   regions, io, fd, ex,
                   X = NULL, Y = NULL, E = NULL,
                   L_base = NULL, L_curr = NULL) {
  
  sub <- if (losses) "losses/" else ""
  
  # --- Load static data ----------------------------------------------------
  if (is.null(X)) X <- readRDS(paste0(input_path, sub, "X.rds"))
  if (is.null(Y)) Y <- readRDS(paste0(input_path, sub, "Y.rds"))
  if (!is.null(extension) && is.null(E))
    E <- readRDS(paste0(input_path, "E.rds"))
  if (is.null(L_base))
    L_base <- readRDS(paste0(input_path, sub, year_base,    "_L_", allocation, ".rds"))
  if (is.null(L_curr))
    L_curr <- readRDS(paste0(input_path, sub, year_current, "_L_", allocation, ".rds"))
  
  # --- Item aggregator T_agg : N_items × RN -------------------------------
  items     <- unique(io$comm_code)
  item_idx  <- match(io$comm_code, items)
  N_items   <- length(items)
  RN        <- nrow(io)
  T_agg     <- sparseMatrix(i = item_idx, j = seq_len(RN), x = 1,
                            dims = c(N_items, RN))
  
  item_lab_dt <- unique(as.data.table(io)[, .(comm_code, item)])
  item_label_lookup <- setNames(item_lab_dt$item, item_lab_dt$comm_code)
  
  # --- Per-year factors: e, τ (item-level L), μ (within-item shares) ------
  build_factors <- function(yr, L_yr) {
    Xi <- X[, as.character(yr)]
    Yi <- Y[[as.character(yr)]]
    if (!is.null(extension)) {
      Ei <- E[[as.character(yr)]]
      e <- as.numeric(Ei[ex$Stressor == extension, ]) / as.vector(Xi)
      e[!is.finite(e)] <- 0
    } else {
      e <- rep(1, length(Xi))
    }
    tau <- T_agg %*% L_yr
    L_T <- as(L_yr, "TsparseMatrix")
    tau_dense <- as.matrix(tau)
    rows_items <- item_idx[L_T@i + 1L]
    targets    <- L_T@j + 1L
    tau_at_trip <- tau_dense[cbind(rows_items, targets)]
    mu_x <- L_T@x / tau_at_trip
    mu_x[!is.finite(mu_x)] <- 0
    mu <- sparseMatrix(i = L_T@i + 1L, j = L_T@j + 1L, x = mu_x,
                       dims = dim(L_yr))
    list(e = e, L = L_yr, tau = tau, mu = mu, X = Xi, Y = Yi)
  }
  fac0 <- build_factors(year_base,    L_base)
  fac1 <- build_factors(year_current, L_curr)
  
  # --- Patch μ at τ-zero-flips --------------------------------------------
  patch_mu <- function(mu_target, mu_source, tau_target, tau_source) {
    tt <- as(tau_target, "TsparseMatrix")
    ts <- as(tau_source, "TsparseMatrix")
    s_keys <- ts@i * ncol(tau_target) + ts@j
    t_keys <- tt@i * ncol(tau_target) + tt@j
    pmask <- !(s_keys %in% t_keys)
    if (!any(pmask)) return(mu_target)
    p_items   <- ts@i[pmask]
    p_targets <- ts@j[pmask]
    p_keys    <- p_items * ncol(tau_target) + p_targets
    
    ms <- as(mu_source, "TsparseMatrix")
    ms_items   <- item_idx[ms@i + 1L] - 1L
    ms_targets <- ms@j
    ms_keys    <- ms_items * ncol(mu_source) + ms_targets
    
    in_patch <- ms_keys %in% p_keys
    if (!any(in_patch)) return(mu_target)
    
    patch_mat <- sparseMatrix(i = ms@i[in_patch] + 1L,
                              j = ms@j[in_patch] + 1L,
                              x = ms@x[in_patch],
                              dims = dim(mu_target))
    mu_target + patch_mat
  }
  fac0$mu <- patch_mu(fac0$mu, fac1$mu, fac0$tau, fac1$tau)
  fac1$mu <- patch_mu(fac1$mu, fac0$mu, fac1$tau, fac0$tau)
  
  # --- Country list --------------------------------------------------------
  if (is.null(country)) country <- unique(fd$iso3c)
  unknown <- setdiff(country, fd$iso3c)
  if (length(unknown) > 0) {
    warning("Unknown countries dropped: ", paste(unknown, collapse = ", "))
    country <- setdiff(country, unknown)
  }
  
  # --- Demand decomposition ----------------------------------------------
  decompose_demand <- function(Yi, ctry) {
    Yc <- Yi[, fd$iso3c == ctry, drop = FALSE]
    y_full <- as.vector(rowSums(as.matrix(Yc)))
    totals <- numeric(length(commodity))
    origin <- vector("list", length(commodity)); names(origin) <- commodity
    for (i in seq_along(commodity)) {
      cc <- commodity[i]
      idx <- io$comm_code == cc
      y_cc <- y_full * idx
      totals[i] <- sum(y_cc)
      origin[[cc]] <- if (totals[i] > 0) y_cc / totals[i] else y_cc
    }
    s     <- sum(totals)
    sigma <- if (s > 0) totals / s else rep(0, length(commodity))
    names(sigma) <- commodity
    list(s = s, sigma = sigma, m = origin)
  }
  
  build_y <- function(s, sigma, m) {
    y <- numeric(nrow(io))
    for (i in seq_along(commodity)) y <- y + s * sigma[i] * m[[commodity[i]]]
    y
  }
  
  # --- Global precomputations (once per year-pair, shared across countries)
  
  tau_0_dense <- as.matrix(fac0$tau)
  tau_1_dense <- as.matrix(fac1$tau)
  dtau_dense  <- tau_1_dense - tau_0_dense
  
  # h(μ_v, e_v)[i, j] = Σ_r e_v[(r,i)] · μ_v[(r,i), j]
  # Polar A (later factors at base) uses h(μ_0, e_1).
  # Polar B (later factors at current) uses h(μ_1, e_0).
  h_mu0_e1 <- as.matrix(T_agg %*% (Diagonal(x = fac1$e) %*% fac0$mu))
  h_mu1_e0 <- as.matrix(T_agg %*% (Diagonal(x = fac0$e) %*% fac1$mu))
  
  # Pre-multiplied dense matrices for Δ_τ aggregation
  dtau_h_A <- dtau_dense * h_mu0_e1
  dtau_h_B <- dtau_dense * h_mu1_e0
  
  # eL vectors (length RN)
  eL_00 <- as.numeric(crossprod(fac0$e, fac0$L))
  eL_11 <- as.numeric(crossprod(fac1$e, fac1$L))
  
  # Δμ triplet and country-independent kernel parts for Δ_μ
  dmu     <- fac1$mu - fac0$mu
  dmu_T   <- as(dmu, "TsparseMatrix")
  has_dmu <- length(dmu_T@x) > 0
  if (has_dmu) {
    mu_rows  <- dmu_T@i + 1L
    mu_cols  <- dmu_T@j + 1L
    mu_items <- item_idx[mu_rows]
    pre_eTau_A <- fac1$e[mu_rows] * tau_1_dense[cbind(mu_items, mu_cols)]
    pre_eTau_B <- fac0$e[mu_rows] * tau_0_dense[cbind(mu_items, mu_cols)]
  }
  
  effect_names <- c("intensity", "feedstock_mix", "sourcing",
                    "scale", "composition", "origin")
  
  sda_country <- function(ctry) {
    dem0 <- decompose_demand(fac0$Y, ctry)
    dem1 <- decompose_demand(fac1$Y, ctry)
    
    for (cc in commodity) {
      if (sum(dem0$m[[cc]]) == 0 && sum(dem1$m[[cc]]) > 0) dem0$m[[cc]] <- dem1$m[[cc]]
      if (sum(dem1$m[[cc]]) == 0 && sum(dem0$m[[cc]]) > 0) dem1$m[[cc]] <- dem0$m[[cc]]
    }
    if (dem0$s == 0 && dem1$s > 0) dem0$sigma <- dem1$sigma
    if (dem1$s == 0 && dem0$s > 0) dem1$sigma <- dem0$sigma
    
    nC <- length(commodity)
    
    y_0 <- build_y(dem0$s, dem0$sigma, dem0$m)
    y_1 <- build_y(dem1$s, dem1$sigma, dem1$m)
    
    # Endpoints
    Ly_00 <- as.numeric(fac0$L %*% y_0)
    Ly_11 <- as.numeric(fac1$L %*% y_1)
    F0 <- sum(fac0$e * Ly_00)
    F1 <- sum(fac1$e * Ly_11)
    
    # Δ_e per cell
    delta_e_cell <- (fac1$e - fac0$e) * (Ly_00 + Ly_11) / 2
    
    # Δ_τ per item (aggregated over j)
    delta_tau_item <- as.numeric((dtau_h_A %*% y_0 + dtau_h_B %*% y_1) / 2)
    
    # Δ_μ per cell, then aggregate to (origin, item)
    if (has_dmu) {
      kA <- pre_eTau_A * y_0[mu_cols]
      kB <- pre_eTau_B * y_1[mu_cols]
      delta_mu_x <- dmu_T@x * (kA + kB) / 2
      delta_mu_dt <- data.table(
        country_origin = io$iso3c[mu_rows],
        item_origin    = io$item[mu_rows],
        value          = delta_mu_x
      )[, .(value = sum(value)), by = .(country_origin, item_origin)]
    } else {
      delta_mu_x  <- numeric(0)
      delta_mu_dt <- data.table(country_origin = character(0),
                                item_origin = character(0),
                                value = numeric(0))
    }
    
    # eLm scalars: A = eL_11 · m_0_c (polar A), B = eL_00 · m_1_c (polar B)
    eLm_A <- vapply(commodity, function(cc) sum(eL_11 * dem0$m[[cc]]), numeric(1))
    eLm_B <- vapply(commodity, function(cc) sum(eL_00 * dem1$m[[cc]]), numeric(1))
    
    # Δ_s per commodity
    delta_s_c     <- (dem1$s - dem0$s) *
      (dem0$sigma * eLm_A + dem1$sigma * eLm_B) / 2
    
    # Δ_σ per commodity
    delta_sigma_c <- (dem1$sigma - dem0$sigma) *
      (dem1$s * eLm_A + dem0$s * eLm_B) / 2
    
    # Δ_m per (commodity, RN cell)
    delta_m_cj <- vector("list", nC); names(delta_m_cj) <- commodity
    for (ci in seq_len(nC)) {
      cc <- commodity[ci]
      kernel <- (dem1$s * dem1$sigma[cc] * eL_11 +
                   dem0$s * dem0$sigma[cc] * eL_00) / 2
      delta_m_cj[[cc]] <- (dem1$m[[cc]] - dem0$m[[cc]]) * kernel
    }
    
    # ---- Aggregate scalar effects ----
    deltas <- numeric(6); names(deltas) <- effect_names
    deltas["intensity"]     <- sum(delta_e_cell)
    deltas["feedstock_mix"] <- sum(delta_tau_item)
    deltas["sourcing"]      <- if (has_dmu) sum(delta_mu_x) else 0
    deltas["scale"]         <- sum(delta_s_c)
    deltas["composition"]   <- sum(delta_sigma_c)
    deltas["origin"]        <- sum(unlist(delta_m_cj))
    
    scalar_dt <- data.table(
      country_consumer = ctry,
      effect = c(effect_names, "total_t0", "total_t1", "delta"),
      value  = c(deltas, F0, F1, F1 - F0))
    
    if (F0 == 0 && F1 == 0) {
      if (breakdown) return(list(scalars = scalar_dt, breakdown = data.table()))
      return(scalar_dt)
    }
    if (!breakdown) return(scalar_dt)
    
    # ---- Assemble breakdown table ----
    bd <- rbindlist(list(
      data.table(country_consumer = ctry, effect = "intensity",
                 country_origin = io$iso3c, item_origin = io$item,
                 commodity = NA_character_, value = delta_e_cell),
      data.table(country_consumer = ctry, effect = "feedstock_mix",
                 country_origin = NA_character_,
                 item_origin = item_label_lookup[items],
                 commodity = NA_character_, value = delta_tau_item),
      data.table(country_consumer = ctry, effect = "sourcing",
                 country_origin = delta_mu_dt$country_origin,
                 item_origin    = delta_mu_dt$item_origin,
                 commodity = NA_character_, value = delta_mu_dt$value),
      data.table(country_consumer = ctry, effect = "composition",
                 country_origin = NA_character_, item_origin = NA_character_,
                 commodity = commodity, value = delta_sigma_c),
      data.table(country_consumer = ctry, effect = "scale",
                 country_origin = NA_character_, item_origin = NA_character_,
                 commodity = commodity, value = delta_s_c),
      rbindlist(lapply(commodity, function(cc) {
        data.table(country_consumer = ctry, effect = "origin",
                   country_origin = io$iso3c, item_origin = io$item,
                   commodity = cc, value = delta_m_cj[[cc]])
      }))
    ), use.names = TRUE, fill = TRUE)
    
    bd <- bd[value != 0]
    list(scalars = scalar_dt, breakdown = bd)
  }
  
  # ---- Loop over consumer countries ----
  if (breakdown) {
    res_list       <- lapply(country, sda_country)
    scalar_results <- rbindlist(lapply(res_list, `[[`, "scalars"))
    bd_results     <- rbindlist(lapply(res_list, `[[`, "breakdown"),
                                use.names = TRUE, fill = TRUE)
  } else {
    scalar_results <- rbindlist(lapply(country, sda_country))
  }
  
  indicator_label <- if (!is.null(extension)) extension else "material"
  scalar_results[, `:=`(year_base = year_base, year_current = year_current,
                        indicator = indicator_label, allocation = allocation,
                        commodities = paste(commodity, collapse = "-"))]
  setcolorder(scalar_results, c("country_consumer", "year_base", "year_current",
                                "indicator", "allocation", "commodities",
                                "effect", "value"))
  
  if (breakdown && nrow(bd_results) > 0) {
    bd_results[, `:=`(year_base = year_base, year_current = year_current,
                      indicator = indicator_label, allocation = allocation)]
    setcolorder(bd_results, c("country_consumer", "year_base", "year_current",
                              "indicator", "allocation", "effect",
                              "country_origin", "item_origin", "commodity", "value"))
  }
  
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
      fname_bd <- sub("\\.csv$", "_drivers.csv", fname)
      fwrite(bd_results, file.path(output_dir, fname_bd))
      message("Wrote ", file.path(output_dir, fname_bd))
    }
  }
  
  if (breakdown) list(scalars = scalar_results, drivers = bd_results) else scalar_results
}



###########################################################
########### CHAINED SDA (year-on-year)
###########################################################

fp_sda_chained <- function(years           = 2012:2022,
                           country         = NULL,
                           extension       = NULL,
                           commodity,
                           allocation      = "value",
                           input_path      = "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/",
                           losses          = TRUE,
                           breakdown       = FALSE,
                           save            = FALSE,
                           output_dir      = "output",
                           regions, io, fd, ex,
                           X = NULL, Y = NULL, E = NULL) {
  
  stopifnot(length(years) >= 2)
  years <- sort(unique(years))
  sub <- if (losses) "losses/" else ""
  
  if (is.null(X)) X <- readRDS(paste0(input_path, sub, "X.rds"))
  if (is.null(Y)) Y <- readRDS(paste0(input_path, sub, "Y.rds"))
  if (!is.null(extension) && is.null(E))
    E <- readRDS(paste0(input_path, "E.rds"))
  
  load_L <- function(yr) {
    message("Loading L for ", yr, " ...")
    readRDS(paste0(input_path, sub, yr, "_L_", allocation, ".rds"))
  }
  
  L_curr <- load_L(years[1])
  step_results <- vector("list", length(years) - 1)
  
  for (i in seq_len(length(years) - 1)) {
    t0 <- years[i]; t1 <- years[i + 1]
    L_base <- L_curr
    L_curr <- load_L(t1)
    
    message("SDA: ", t0, " -> ", t1)
    step_results[[i]] <- fp_sda(
      country      = country,
      year_base    = t0,
      year_current = t1,
      extension    = extension,
      commodity    = commodity,
      allocation   = allocation,
      input_path   = input_path,
      losses       = losses,
      breakdown    = breakdown,
      save         = FALSE,
      regions      = regions, io = io, fd = fd, ex = ex,
      X = X, Y = Y, E = E,
      L_base = L_base, L_curr = L_curr
    )
  }
  
  if (breakdown) {
    scalar_results <- rbindlist(lapply(step_results, `[[`, "scalars"))
    bd_results     <- rbindlist(lapply(step_results, `[[`, "drivers"),
                                use.names = TRUE, fill = TRUE)
  } else {
    scalar_results <- rbindlist(step_results)
  }
  
  if (save) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    indicator_label <- if (!is.null(extension)) extension else "material"
    fname <- build_filename(
      "FABIO_SDA_chained",
      yearBase = min(years), yearCurr = max(years),
      indicator = indicator_label, alloc = allocation,
      comm = commodity)
    fwrite(scalar_results, file.path(output_dir, fname))
    message("Wrote ", file.path(output_dir, fname))
    if (breakdown) {
      fname_bd <- sub("\\.csv$", "_drivers.csv", fname)
      fwrite(bd_results, file.path(output_dir, fname_bd))
      message("Wrote ", file.path(output_dir, fname_bd))
    }
  }
  
  if (breakdown) list(scalars = scalar_results, drivers = bd_results) else scalar_results
}




###########################################################
########### SMOOTHED SDA (multi-year endpoint averaging)
###########################################################

fp_sda_smoothed <- function(years_base      = 2012:2014,
                            years_current   = 2020:2022,
                            country         = NULL,
                            extension       = NULL,
                            commodity,
                            allocation      = "value",
                            input_path      = "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/",
                            losses          = TRUE,
                            breakdown       = FALSE,
                            save            = FALSE,
                            output_dir      = "output",
                            regions, io, fd, ex,
                            X = NULL, Y = NULL, E = NULL,
                            L_method        = c("average", "midyear")) {
  
  L_method <- match.arg(L_method)
  sub      <- if (losses) "losses/" else ""
  
  if (is.null(X)) X <- readRDS(paste0(input_path, sub, "X.rds"))
  if (is.null(Y)) Y <- readRDS(paste0(input_path, sub, "Y.rds"))
  if (!is.null(extension) && is.null(E))
    E <- readRDS(paste0(input_path, "E.rds"))
  
  avg_window <- function(years) {
    yrs <- as.character(years)
    list(
      X = rowMeans(X[, yrs, drop = FALSE]),
      Y = Reduce("+", Y[yrs]) / length(yrs),
      E = if (!is.null(extension)) Reduce("+", E[yrs]) / length(yrs) else NULL
    )
  }
  
  build_L_window <- function(years) {
    yrs <- as.character(years)
    if (L_method == "average") {
      L_list <- lapply(yrs, function(yr) {
        message("  loading L for ", yr)
        readRDS(paste0(input_path, sub, yr, "_L_", allocation, ".rds"))
      })
      Reduce("+", L_list) / length(L_list)
    } else if (L_method == "midyear") {
      midyr <- years[ceiling(length(years) / 2)]
      message("  loading L for midyear ", midyr)
      readRDS(paste0(input_path, sub, midyr, "_L_", allocation, ".rds"))
    }
  }
  
  message("Building averaged endpoint t0 from: ", paste(years_base, collapse = ", "))
  ep0    <- avg_window(years_base)
  L_base <- build_L_window(years_base)
  
  message("Building averaged endpoint t1 from: ", paste(years_current, collapse = ", "))
  ep1    <- avg_window(years_current)
  L_curr <- build_L_window(years_current)
  
  label_base <- paste0(min(years_base),    "-", max(years_base))
  label_curr <- paste0(min(years_current), "-", max(years_current))
  
  X_synth <- cbind(ep0$X, ep1$X)
  colnames(X_synth) <- c(label_base, label_curr)
  Y_synth <- setNames(list(ep0$Y, ep1$Y), c(label_base, label_curr))
  E_synth <- if (!is.null(extension))
    setNames(list(ep0$E, ep1$E), c(label_base, label_curr)) else NULL
  
  results <- fp_sda(
    country      = country,
    year_base    = label_base,
    year_current = label_curr,
    extension    = extension,
    commodity    = commodity,
    allocation   = allocation,
    input_path   = input_path,
    losses       = losses,
    breakdown    = breakdown,
    save         = FALSE,
    regions      = regions, io = io, fd = fd, ex = ex,
    X = X_synth, Y = Y_synth, E = E_synth,
    L_base = L_base, L_curr = L_curr
  )
  
  if (save) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    indicator_label <- if (!is.null(extension)) extension else "material"
    fname <- paste0(
      "FABIO_SDA_smoothed_",
      label_base, "_vs_", label_curr, "_",
      indicator_label, "_", allocation, "_",
      paste(commodity, collapse = "-"), ".csv"
    )
    if (breakdown) {
      fwrite(results$scalars, file.path(output_dir, fname))
      message("Wrote ", file.path(output_dir, fname))
      fname_bd <- sub("\\.csv$", "_drivers.csv", fname)
      fwrite(results$drivers, file.path(output_dir, fname_bd))
      message("Wrote ", file.path(output_dir, fname_bd))
    } else {
      fwrite(results, file.path(output_dir, fname))
      message("Wrote ", file.path(output_dir, fname))
    }
  }
  
  results
}



# sda <- fp_sda(
#   year_base    = 2013,
#   year_current = 2022,
#   extension    = "land_harv",
#   commodity    = c("c146", "c147", "c149"),
#   breakdown    = TRUE,
#   save         = TRUE,
#   regions = regions, io = io, fd = fd, ex = ex,
#   X = X, Y = Y, E = E_bar
# )

SDA_chain <- fp_sda_chained(
  years        = 2012:2022,
  extension    = "LCIM_EQ_terrestrial",
  commodity    = c("c146", "c147", "c149"),
  breakdown    = TRUE,
  save         = TRUE,
  regions = regions, io = io, fd = fd, ex = ex,
  X = X, Y = Y, E = E_bar
)

# SDA_chain <- fp_sda_chained(
#   years        = 2012:2022,
#   extension    = "ibif_total",
#   commodity    = c("c146", "c147", "c149"),
#   breakdown    = TRUE,
#   save         = TRUE,
#   regions = regions, io = io, fd = fd, ex = ex,
#   X = X, Y = Y, E = E_bar
# )

# SDA_smoothed <- fp_sda_smoothed(
#   years_base    = 2012:2014,
#   years_current = 2020:2022,
#   extension     = "land_harv",
#   commodity     = c("c146", "c147", "c149"),
#   breakdown     = TRUE,
#   save          = TRUE,
#   regions = regions, io = io, fd = fd, ex = ex,
#   X = X, Y = Y, E = E_bar
# )
# 
# 


###########################################################
########### SANITY CHECKS
###########################################################

# 1) Per-country, per year-pair: scalar effects sum to delta
SDA_chain$scalars[, .(check = sum(value[effect %in% c("intensity","feedstock_mix",
                                                      "sourcing","scale","composition","origin")]) -
                        value[effect == "delta"]), by = .(country_consumer, year_current)
][, max(abs(check))]   # ~1e-9

# 2) Per-element drivers sum to scalar effects per (country, year, effect)
chk <- merge(
  SDA_chain$drivers[, .(sum_bd = sum(value)),
                    by = .(country_consumer, year_current, effect)],
  SDA_chain$scalars[effect %in% c("intensity","feedstock_mix","sourcing",
                                  "scale","composition","origin"),
                    .(country_consumer, year_current, effect, scalar = value)],
  by = c("country_consumer", "year_current", "effect"))
chk[, max(abs(sum_bd - scalar))]   # ~1e-9
drv <- SDA_chain$drivers

# Which feedstocks gained/lost role in the L recipe (globally)
drv[effect == "feedstock_mix",
    .(value = sum(value)), by = .(item_origin)
][order(-abs(value))][1:20]

# For soybeans, which origins gained or lost share within sourcing
drv[effect == "sourcing" & item_origin == "Soyabeans",
    .(value = sum(value)), by = .(country_origin)
][order(-value)]

# Trajectory: feedstock_mix vs sourcing for FRA over time
drv[effect %in% c("feedstock_mix","sourcing") & country_consumer == "FRA",
    .(value = sum(value)), by = .(year_current, effect)]


