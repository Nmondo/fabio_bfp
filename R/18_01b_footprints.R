# 18b - Extension (pressure/impact) footprints from FABIO-BCP MRIO  (VALUE allocation)
# ------------------------------------------------------------------------------
# Consumption-based environmental footprints: every table here is driven by a
# stressor (ibif*, LCIM_*, land_harv), so it uses the VALUE-allocated tables
# (Z_value) and <year>_L_value.
#
# Companion script: 18a_material_flows.R  (MASS allocation, physical flows).
# NOTE: fp_trade_breakdown_feedstock() and the filename helpers are duplicated in
#       both scripts. If you edit that function, edit it in both (or factor the
#       shared core into an 18_00_footprint_core.R that both source()).
# ------------------------------------------------------------------------------

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

# ---- MODEL VERSION -----------------------------------------------------------
# "rescaled" (default) -> RED-rescaled tables at v2_bcp/, CSVs to output/.
# "bypass"             -> non-rescaled counterfactual under v2_bcp/bypass/,
#                         CSVs to output/bypass/. Flip via FABIO_RUN_MODE=bypass.
model_version <- Sys.getenv("FABIO_RUN_MODE", unset = "rescaled")
model_version <- if (tolower(trimws(model_version)) == "bypass") "bypass" else "rescaled"

base_path <- "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/"          # version-invariant root (E, ex)
MRIO_PATH <- if (model_version == "bypass")
  paste0(sub("/+$", "", base_path), "/bypass/") else base_path   # version-specific MRIO
OUT_DIR   <- if (model_version == "bypass") "output/bypass" else "output"
message(sprintf(">>> [18b extensions] model_version = '%s'  (MRIO: %s | out: %s)",
                model_version, MRIO_PATH, OUT_DIR))

# Read labels ------------------------------------------------------------------
input_path <- MRIO_PATH
regions <- fread(file="inst/regions_full.csv") %>% filter(current==TRUE)
io <- fread(paste0(input_path,"io_labels.csv"))
fd <- fread(file=paste0(input_path,"losses/fd_labels.csv"))
ex <- fread(file=paste0(base_path,"ex_labels.csv"))         # shared across versions; needed for stressor selection
items_full_bcp <- as.data.table(fread("inst/items_full_bcp.csv"))  # comm_code -> comm_group (end-product grouping)

# Create output directory ------------------------------------------------------
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Load static data once (VALUE allocation) -------------------------------------
# X, Y, Z, E are read once. L is year-specific and loaded inside each call
# (as <year>_L_value, driven by `allocation`).
allocation <- "value"

X <- readRDS(paste0(input_path, "losses/X.rds"))
Y <- readRDS(paste0(input_path, "losses/Y.rds"))
Z <- readRDS(paste0(input_path, "losses/Z_", allocation, ".rds"))   # Z_value
E <- readRDS(paste0(base_path, "E.rds"))                    # extensions: shared across versions

# Make E_bar, 3-year average of the environmental extensions.
yrs_E_bar <- as.character(2012:2022)
E_bar <- vector("list", length(yrs_E_bar))
names(E_bar) <- yrs_E_bar

for (yr in yrs_E_bar) {
  y <- as.integer(yr)
  E_bar[[yr]] <- (E[[as.character(y - 1)]] + E[[as.character(y)]] + E[[as.character(y + 1)]]) / 3
}

#### Making general vectors
bf_set <- c("c146", "c147", "c149", "c150", "c151")
bp_set <- paste0("c", 152:170)

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


######################################################################################################################
############################## EXTENSION FOOTPRINT FUNCTIONS #######################
######################################################################################################################

###########################################################
########### CONSUMPTION-BASED IMPACT
########### OF SELECTED COUNTRIES (default: all)
########### OF SELECTED COMMODITIES
########### BY CONTINENT WHERE IMPACTS OCCUR
########### BY FEEDSTOCK (DIRECT REQUIREMENT) RESPONSIBLE FOR THE IMPACT
###########################################################

fp_feedstock <- function(country       = NULL,
                         year,
                         extension     = NULL,
                         commodity,
                         allocation    = "value",
                         input_path    = MRIO_PATH,
                         losses        = TRUE,
                         by_commodity  = TRUE,
                         by_country    = TRUE,
                         top_n         = NULL,        # keep top-N feedstocks per (country, commodity); NULL = all
                         drop_zero     = TRUE,        # drop zero-value entries early
                         save          = FALSE,
                         output_dir    = OUT_DIR,
                         regions, io, fd, ex,
                         X = NULL, Y = NULL, Z = NULL, E = NULL, L = NULL,
                         return_full   = FALSE) {
  
  sub <- if (losses) "losses/" else ""
  
  # --- Load only what wasn't passed in ---------------------------------------
  if (is.null(X)) X <- readRDS(paste0(input_path, sub, "X.rds"))
  if (is.null(Y)) Y <- readRDS(paste0(input_path, sub, "Y.rds"))
  if (is.null(Z)) Z <- readRDS(paste0(input_path, sub, "Z_", allocation, ".rds"))
  if (!is.null(extension) && is.null(E))
    E <- readRDS(paste0(base_path, "E.rds"))
  if (is.null(L)) L <- readRDS(paste0(input_path, sub, year, "_L_", allocation, ".rds"))
  
  Xi <- X[, as.character(year)]
  Yi <- Y[[as.character(year)]]
  Zi <- Z[[as.character(year)]]
  Ei <- if (!is.null(extension)) E[[as.character(year)]] else NULL
  
  # --- Extension intensity ---------------------------------------------------
  if (!is.null(extension)) {
    if (!extension %in% rownames(Ei))
      stop("Stressor '", extension, "' not found in rownames(E[[", year, "]]).")
    ext <- as.numeric(Ei[extension, ]) / as.vector(Xi)
    ext[!is.finite(ext)] <- 0
  } else {
    ext <- rep(1, length(Xi))
  }
  MP <- ext * L
  
  # --- Resolve country list -------------------------------------------------
  if (is.null(country)) country <- unique(fd$iso3c)
  unknown <- setdiff(country, fd$iso3c)
  if (length(unknown) > 0) {
    warning("Unknown countries dropped: ", paste(unknown, collapse = ", "))
    country <- setdiff(country, unknown)
  }
  if (length(country) == 0) stop("No valid countries supplied.")
  
  # --- Validate commodity input ---------------------------------------------
  unknown_c <- setdiff(commodity, io$comm_code)
  if (length(unknown_c) > 0) {
    warning("Unknown comm_codes dropped: ", paste(unknown_c, collapse = ", "))
    commodity <- intersect(commodity, io$comm_code)
  }
  if (length(commodity) == 0) stop("No valid comm_codes supplied.")
  
  io_label <- paste0(io$iso3c, "_", io$item)
  
  # --- Helper: per-country, per-commodity footprint -------------------------
  compute_for <- function(ctry, cc) {
    # Country final demand
    Y_country <- Yi[, fd$iso3c == ctry, drop = FALSE]
    y_full <- as.vector(rowSums(as.matrix(Y_country)))
    
    comm_idx <- io$comm_code == cc
    y_comm <- y_full
    y_comm[!comm_idx] <- 0
    
    if (sum(y_comm) == 0) return(NULL)              # early skip: no demand
    
    x_triggered <- as.vector(L %*% y_comm)
    x_comm <- x_triggered
    x_comm[!comm_idx] <- 0
    
    denom <- Xi[comm_idx]
    denom[denom == 0] <- 1
    f <- as.vector(Zi[, comm_idx] %*% (x_comm[comm_idx] / denom))
    
    if (sum(f) == 0) return(NULL)
    
    # --- top-N filter on f BEFORE expanding to FP ---------------------------
    if (!is.null(top_n) && top_n < length(unique(io$item))) {
      f_by_item <- tapply(f, io$item, sum)
      keep_items <- names(sort(f_by_item, decreasing = TRUE))[seq_len(top_n)]
      f[!(io$item %in% keep_items)] <- 0
      if (sum(f) == 0) return(NULL)
    }
    
    # --- footprint matrix, sparse from the start --------------------------
    nz_target <- which(f != 0)
    if (length(nz_target) == 0) return(NULL)
    
    MP_sub <- MP[, nz_target, drop = FALSE]
    FP_sub <- t(t(MP_sub) * f[nz_target])           # only nonzero target columns
    FP_sub <- as(FP_sub, "TsparseMatrix")
    
    if (FP_sub@x |> length() == 0) return(NULL)
    
    target_lbl <- io_label[nz_target]
    dt <- data.table(
      country_consumer = ctry,
      commodity        = cc,
      origin           = io_label[FP_sub@i + 1],
      target           = target_lbl[FP_sub@j + 1],
      value            = FP_sub@x
    )
    
    if (drop_zero) dt <- dt[value != 0]
    dt
  }
  
  # --- Loop over (country, commodity) ---------------------------------------
  grid <- CJ(ctry = country, cc = commodity, sorted = FALSE)
  per_cell <- rbindlist(
    Map(compute_for, grid$ctry, grid$cc),
    use.names = TRUE, fill = TRUE
  )
  
  if (nrow(per_cell) == 0) {
    warning("Empty footprint for year ", year, " / countries: ",
            paste(country, collapse = ","), " / commodities: ",
            paste(commodity, collapse = ","))
    return(per_cell)
  }
  
  # --- Annotate --------------------------------------------------------------
  indicator_label <- if (!is.null(extension)) extension else "material"
  
  per_cell[, `:=`(
    year             = year,
    indicator        = indicator_label,
    allocation       = allocation,
    country_origin   = substr(origin, 1, 3),
    item_origin      = substr(origin, 5, 100),
    country_target   = substr(target, 1, 3),
    item_target      = substr(target, 5, 100)
  )]
  per_cell[, continent_origin :=
             regions$continent[match(country_origin, regions$iso3c)]]
  
  # --- Aggregate -------------------------------------------------------------
  group_cols <- c("year", "indicator", "allocation",
                  "feedstock", "continent_origin")
  if (by_country)   group_cols <- c("country_consumer", group_cols)
  if (by_commodity) group_cols <- c(group_cols, "commodity")
  
  agg_results <- per_cell[, .(value = sum(value)),
                          by = c(setdiff(group_cols, "feedstock"), "item_target")]
  setnames(agg_results, "item_target", "feedstock")
  setcolorder(agg_results, group_cols)
  
  if (drop_zero) agg_results <- agg_results[value != 0]
  
  if (save) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    fname <- build_filename("FABIO_feedstock",
                            country   = if (length(country) > 4) NULL else country,
                            year      = year,
                            indicator = indicator_label,
                            alloc     = allocation,
                            comm      = commodity,
                            topN      = top_n,
                            byCountry = by_country,
                            byComm    = by_commodity)
    fwrite(agg_results, file.path(output_dir, fname))
    message("Wrote ", file.path(output_dir, fname))
  }
  
  if (return_full) list(results = per_cell, agg = agg_results) else agg_results
}


###########################################################
########### CONSUMPTION-BASED IMPACT
########### OF SELECTED COMMODITIES
########### TRADE BREAKDOWN OR BILATERAL FLOWS
########### BY 1ST-DEGREE FEEDSTOCK
########### FOR ALL REPORTING COUNTRIES
###########################################################

fp_trade_breakdown_feedstock <- function(year,
                                         extension    = NULL,     # NULL = material flows
                                         commodity    = NULL,     # NULL = all commodities
                                         allocation   = "value",
                                         input_path   = MRIO_PATH,
                                         losses       = TRUE,
                                         by_commodity = FALSE,
                                         by_feedstock = TRUE,
                                         indirect     = TRUE,
                                         bilateral    = FALSE,
                                         drop_zero    = TRUE,
                                         save         = FALSE,
                                         output_dir   = OUT_DIR,
                                         regions, io, fd, ex,
                                         X = NULL, Y = NULL, Z = NULL,
                                         E = NULL, L = NULL,
                                         return_full  = FALSE) {
  
  sub <- if (losses) "losses/" else ""
  
  # --- Load only what wasn't passed in ---------------------------------------
  if (is.null(X)) X <- readRDS(paste0(input_path, sub, "X.rds"))
  if (is.null(Y)) Y <- readRDS(paste0(input_path, sub, "Y.rds"))
  if (is.null(Z)) Z <- readRDS(paste0(input_path, sub, "Z_", allocation, ".rds"))
  if (!is.null(extension) && is.null(E))
    E <- readRDS(paste0(base_path, "E.rds"))
  if (is.null(L)) L <- readRDS(paste0(input_path, sub, year, "_L_", allocation, ".rds"))
  
  Xi <- X[, as.character(year)]
  Yi <- Y[[as.character(year)]]
  Zi <- Z[[as.character(year)]]
  Ei <- if (!is.null(extension)) E[[as.character(year)]] else NULL
  
  # --- Extension intensity ---------------------------------------------------
  if (!is.null(extension)) {
    if (!extension %in% rownames(Ei)) {
      stop("Stressor '", extension, "' not found in rownames(E[[", year, "]]).")
    }
    ext <- as.numeric(Ei[extension, ]) / as.vector(Xi)
    ext[!is.finite(ext)] <- 0
  } else {
    ext <- rep(1, length(Xi))
  }
  
  # --- Final demand collapsed to one column per consumer country ------------
  colnames(Yi) <- fd$iso3c
  Y_country <- agg(Yi)                    # RN × R
  countries <- colnames(Y_country)
  R <- length(countries)
  
  # --- Validate commodity input ---------------------------------------------
  if (is.null(commodity)) commodity <- unique(io$comm_code)
  unknown <- setdiff(commodity, io$comm_code)
  if (length(unknown) > 0) {
    warning("Unknown comm_codes dropped: ", paste(unknown, collapse = ", "))
    commodity <- intersect(commodity, io$comm_code)
  }
  if (length(commodity) == 0) stop("No valid comm_codes supplied.")
  
  # --- Origin aggregator (RN -> R) ------------------------------------------
  T_origin <- sparseMatrix(
    i    = seq_len(nrow(io)),
    j    = match(io$iso3c, countries),
    x    = 1,
    dims = c(nrow(io), R)
  )
  
  # --- Pre-aggregate origin: G[r, j] = sum_{i in country r} ext[i] * M[i, j]
  #   indirect = TRUE  -> M = L : feedstock + its full upstream chain
  #   indirect = FALSE -> M = I : stop at Z, direct feedstock only
  if (indirect) {
    MP <- if (!is.null(extension)) Diagonal(x = ext) %*% L else L
    G  <- crossprod(T_origin, MP)          # R × RN
  } else {
    G  <- if (!is.null(extension)) crossprod(T_origin, Diagonal(x = ext)) else t(T_origin)
  }          # R × RN
  
  # Map comm_code -> item label (for feedstock naming)
  feedstock_map <- unique(as.data.table(io)[, .(comm_code, item)])
  
  # --- Per-commodity computation --------------------------------------------
  results_list <- lapply(commodity, function(cc) {
    
    cc_idx <- which(io$comm_code == cc)
    
    # Final demand restricted to cc rows (across all FD components, all consumers)
    Y_masked <- Y_country
    Y_masked[-cc_idx, ] <- 0
    
    # cc output triggered everywhere by each consumer's cc consumption
    X_triggered <- L %*% Y_masked
    X_comm_cc   <- as.matrix(X_triggered[cc_idx, , drop = FALSE])    # R × R
    
    # Tier-1 input vector (per consumer)
    denom <- Xi[cc_idx]
    denom[denom == 0] <- 1
    X_comm_norm <- sweep(X_comm_cc, 1, denom, "/")
    F_cc <- Zi[, cc_idx, drop = FALSE] %*% X_comm_norm               # RN × R
    
    if (by_feedstock) {
      active <- which(Matrix::rowSums(F_cc) != 0)
      if (length(active) == 0) return(NULL)
      active_comms <- unique(io$comm_code[active])
      
      per_m <- rbindlist(lapply(active_comms, function(m) {
        rows_m   <- which(io$comm_code == m)
        G_m      <- as.matrix(G[, rows_m, drop = FALSE])             # R × R
        F_m      <- as.matrix(F_cc[rows_m, , drop = FALSE])          # R × R
        impact_m <- G_m %*% F_m                                      # R × R
        
        dt <- data.table(
          country_origin   = rep(countries, times = R),
          country_consumer = rep(countries, each  = R),
          value            = as.vector(impact_m),
          feedstock_code   = m
        )
        if (drop_zero) dt <- dt[value != 0]
        dt
      }), use.names = TRUE)
      
      per_m[, commodity := cc]
      return(per_m)
      
    } else {
      impact <- as.matrix(G %*% F_cc)                                # R × R
      dt <- data.table(
        country_origin   = rep(countries, times = R),
        country_consumer = rep(countries, each  = R),
        value            = as.vector(impact),
        commodity        = cc
      )
      if (drop_zero) dt <- dt[value != 0]
      return(dt)
    }
  })
  
  long <- rbindlist(results_list, use.names = TRUE, fill = TRUE)
  
  if (nrow(long) == 0) {
    warning("Empty footprint for ", year, " / ",
            paste(commodity, collapse = ","))
    return(long)
  }
  
  # --- Map feedstock codes -> item names ------------------------------------
  if (by_feedstock) {
    long[, feedstock := feedstock_map$item[match(feedstock_code,
                                                 feedstock_map$comm_code)]]
    long[, feedstock_code := NULL]
  }
  
  # --- Collapse over commodity if requested ---------------------------------
  if (!by_commodity) {
    grp <- setdiff(names(long), c("value", "commodity"))
    long <- long[, .(value = sum(value)), by = grp]
  }
  
  indicator_label <- if (!is.null(extension)) extension else "material"
  long[, `:=`(year = year, indicator = indicator_label, allocation = allocation)]
  
  # Group keys = everything except origin/consumer/value
  group_keys <- c("year", "indicator", "allocation")
  if (by_feedstock) group_keys <- c(group_keys, "feedstock")
  if (by_commodity) group_keys <- c(group_keys, "commodity")
  
  # --- Bilateral mode: prepare long with flow_type --------------------------
  if (bilateral) {
    long[, flow_type := fifelse(country_origin == country_consumer,
                                "self", "trade")]
    setcolorder(long, c("country_origin", "country_consumer", "flow_type",
                        group_keys, "value"))
    out_to_save <- long
    
  } else {
    # --- Aggregate mode: self / exports / imports per country --------------
    per_origin <- long[, .(row_total = sum(value)),
                       by = c("country_origin", group_keys)]
    setnames(per_origin, "country_origin", "country")
    
    per_consumer <- long[, .(col_total = sum(value)),
                         by = c("country_consumer", group_keys)]
    setnames(per_consumer, "country_consumer", "country")
    
    self_dt <- long[country_origin == country_consumer,
                    .(self_consumption = sum(value)),
                    by = c("country_origin", group_keys)]
    setnames(self_dt, "country_origin", "country")
    
    agg_dt <- merge(per_origin,  per_consumer, by = c("country", group_keys), all = TRUE)
    agg_dt <- merge(agg_dt,      self_dt,      by = c("country", group_keys), all = TRUE)
    
    for (col in c("row_total", "col_total", "self_consumption")) {
      set(agg_dt, which(is.na(agg_dt[[col]])), col, 0)
    }
    agg_dt[, exports := row_total - self_consumption]
    agg_dt[, imports := col_total - self_consumption]
    agg_dt[, c("row_total", "col_total") := NULL]
    
    setcolorder(agg_dt, c("country", group_keys,
                          "self_consumption", "exports", "imports"))
    
    long_out <- melt(agg_dt,
                     id.vars       = c("country", group_keys),
                     measure.vars  = c("self_consumption", "exports", "imports"),
                     variable.name = "category",
                     value.name    = "value")
    long_out[, category := as.character(category)]
    setcolorder(long_out, c("country", group_keys, "category", "value"))
    
    out_to_save <- long_out
  }
  
  # --- Optionally save -----------------------------------------------------
  if (save) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    fname <- build_filename("FABIO_tradeFeed",
                            year      = year,
                            indicator = indicator_label,
                            alloc     = allocation,
                            comm      = commodity,
                            byComm    = by_commodity,
                            byFeed    = by_feedstock,
                            direct    = !indirect,
                            bilat     = bilateral)
    fwrite(out_to_save, file.path(output_dir, fname))
    message("Wrote ", file.path(output_dir, fname))
  }
  
  # --- Return --------------------------------------------------------------
  if (return_full) {
    if (bilateral) return(list(long = long))
    return(list(wide = agg_dt, long = long_out))
  }
  out_to_save
}


###########################################################
########### PRODUCER-LOCATED, END-USE-DECOMPOSED IMPACT
########### (symmetric Ly footprint, kept at the producer)
###########################################################
# For a chosen stressor, split each PRODUCING country's territorial agricultural
# impact by the END-PRODUCT whose (global) final demand ultimately drives it:
#
#     E_{c,k} = sum_{i in c, crop} e_i * x^(k)_i / x_i ,   x^(k) = L y^(k)
#
# where y^(k) is total final demand (summed over ALL consumers) for end-product
# group k = comm_group of the finally consumed product. Because sum_k x^(k) = x,
# sum_k E_{c,k} = sum_{i in c} e_i = the country's territorial total. So every
# end-product (biofuels, vegetable oils, food, feed, ...) is treated the SAME way
# (a symmetric Ly footprint) and the pieces sum to the producer's own total.
#
# Crucially the impact stays at the PRODUCING country i (where it physically
# occurs); the end-product is only a *label*, never a geographic re-aggregation.
# Impacts from different producers are never summed together - each country keeps
# its own row - which is what makes this appropriate for non-spatially-additive
# indicators like MSA/ibif. It uses the final-demand footprint L y^(k) (not a
# gross-output form), so all end-products are treated symmetrically.
#
# Additivity to the territorial total holds to the extent x = L*rowSums(Y) in the
# tables (true for a balanced IO system).

fp_enduse_origin <- function(year,
                             extension,
                             ag_comm    = sprintf("c%03d", 1:145),   # origin sectors (where impact occurs)
                             allocation = "value",
                             input_path = MRIO_PATH,
                             losses     = TRUE,
                             drop_zero  = TRUE,
                             save       = FALSE,
                             output_dir = OUT_DIR,
                             regions, io, items,
                             X = NULL, Y = NULL, E = NULL, L = NULL) {
  
  sub <- if (losses) "losses/" else ""
  if (is.null(X)) X <- readRDS(paste0(input_path, sub, "X.rds"))
  if (is.null(Y)) Y <- readRDS(paste0(input_path, sub, "Y.rds"))
  if (is.null(E)) E <- readRDS(paste0(base_path, "E.rds"))
  if (is.null(L)) L <- readRDS(paste0(input_path, sub, year, "_L_", allocation, ".rds"))
  
  Xi <- X[, as.character(year)]
  Yi <- Y[[as.character(year)]]
  Ei <- E[[as.character(year)]]
  
  if (!extension %in% rownames(Ei))
    stop("Stressor '", extension, "' not found in rownames(E[[", year, "]]).")
  
  # --- Direct intensity f_i = e_i / x_i (production-based; e_i on crop sectors)
  e_i   <- as.numeric(Ei[extension, ])
  inv_x <- 1 / as.vector(Xi); inv_x[!is.finite(inv_x)] <- 0
  f_i   <- e_i * inv_x
  
  # --- Global final demand per product row (sum over ALL consumers) ----------
  y_tot <- as.numeric(Matrix::rowSums(Yi))
  
  # --- End-product group of each product = comm_group of the consumed product -
  grp <- items$comm_group[match(io$comm_code, items$comm_code)]
  grp[is.na(grp)] <- "Unknown"
  groups <- sort(unique(grp))
  G <- length(groups)
  
  # Y_grp[, k] = y_tot masked to products in group k   (RN x G, sparse)
  Ygrp <- sparseMatrix(i = seq_len(nrow(io)),
                       j = match(grp, groups),
                       x = y_tot,
                       dims = c(nrow(io), G))
  
  # Output at each origin driven by group-k final demand: x^(k) = L y^(k)
  Xgrp <- as.matrix(L %*% Ygrp)                        # RN x G
  
  # Impact at origin i attributed to end-product k: e_i * x^(k)_i / x_i --------
  contrib <- Xgrp * f_i                                # row-scale by f_i
  
  # --- Keep agricultural origin sectors, aggregate to producing country x group
  ag_present <- intersect(ag_comm, io$comm_code)
  if (length(ag_present) == 0) stop("No valid agricultural comm_codes supplied.")
  ag_idx <- which(io$comm_code %in% ag_present)
  
  M <- rowsum(contrib[ag_idx, , drop = FALSE], group = io$iso3c[ag_idx])  # n_country x G
  out <- data.table(country   = rep(rownames(M), times = G),
                    end_group = rep(groups,      each  = nrow(M)),
                    enduse_impact = as.vector(M))
  
  # --- Totals + share --------------------------------------------------------
  tot <- out[, .(total_ag_impact = sum(enduse_impact)), by = country]
  out <- merge(out, tot, by = "country", all.x = TRUE)
  out[, share := fifelse(total_ag_impact != 0, enduse_impact / total_ag_impact, NA_real_)]
  out[, `:=`(year = year, indicator = extension, allocation = allocation)]
  out[, continent := regions$continent[match(country, regions$iso3c)]]
  
  if (drop_zero) out <- out[enduse_impact != 0]
  
  setcolorder(out, c("country", "continent", "year", "indicator", "allocation",
                     "end_group", "enduse_impact", "total_ag_impact", "share"))
  setorder(out, -total_ag_impact, country, -enduse_impact)
  
  if (save) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    fname <- build_filename("FABIO_endUseOrigin",
                            year      = year,
                            indicator = extension,
                            alloc     = allocation)
    fwrite(out, file.path(output_dir, fname))
    message("Wrote ", file.path(output_dir, fname))
  }
  
  out
}


######################################################################################################################
############################## RUN: EXTENSION FOOTPRINTS (value allocation) ###########################################
######################################################################################################################

## Bilateral trade footprints, feedstock breakdown, per stressor -------------
ibif_stressors <- unique(ex[Stressor %like% "ibif", Stressor])
extensions_choice <- as.list(c(ibif_stressors,
                               "LCIM_EQ_terrestrial_climate","LCIM_EQ_terrestrial_acidification",
                               "land_harv"))

for (yr in 2012:2022) {
  for (ext in extensions_choice) {
    ext_val <- ext
    message("--- ", yr, " | ext: ", ext, " ---")
    fp_trade_breakdown_feedstock(
      year         = yr,
      extension    = ext_val,
      commodity    = c("c146", "c147", "c149"),
      by_commodity = TRUE,
      by_feedstock = TRUE,
      bilateral    = TRUE,
      save         = TRUE,
      regions = regions, io = io, fd = fd, ex = ex,
      X = X, Y = Y, Z = Z, E = E_bar
    )
  }
}

## Feedstock-attributed footprints (fp_feedstock), per stressor --------------
extensions_choice <- as.list(c("ibif_total", "LCIM_EQ_terrestrial_acidification",
                               "LCIM_EQ_terrestrial_climate", "land_harv"))

for (yr in 2012:2022) {
  for (ext in extensions_choice) {
    ext_val <- ext
    message("--- ", yr, " | ext: ", ext, " ---")
    fp_feedstock(
      country      = NULL, # all countries
      year = yr,
      extension    = ext_val,
      commodity    = c("c146", "c147", "c149"),
      by_commodity = TRUE,
      save = TRUE,
      regions = regions, io = io, fd = fd, ex = ex,
      X = X, Y = Y, Z = Z, E = E_bar
    )
  }
}

extensions_choice <- as.list(ibif_stressors)

for (yr in 2012:2022) {
  for (ext in extensions_choice) {
    ext_val <- ext
    message("--- ", yr, " | ext: ", ext, " ---")
  fp_trade_breakdown_feedstock(
    year         = yr,
    extension    = ext_val,
    commodity    = bp_set,
    by_commodity = TRUE,
    by_feedstock = TRUE,
    bilateral    = TRUE,
    save         = TRUE,
    regions = regions, io = io, fd = fd, ex = ex,
    X = X, Y = Y, Z = Z, E = E_bar
  )
}
}


######################################################################################################################
############################## RUN: PRODUCER-LOCATED END-USE DECOMPOSITION (value allocation) ########################
######################################################################################################################
# Per producing country: territorial agricultural impact split by the end-product
# (global final demand) that drives it. Symmetric across end-products, sums to the
# producer's own total. One combined (all-years) CSV per indicator, consumed by 19b.

years_to_run      <- 2012:2022
enduse_ext_choice <- c("ibif_total", "LCIM_EQ_terrestrial_land_use", "LCIM_EQ_terrestrial_acidification", "LCIM_EQ_terrestrial_climate")

for (ext in enduse_ext_choice) {
  message("=== end-use origin decomposition | ext: ", ext, " ===")
  enduse_bcp <- rbindlist(
    lapply(years_to_run, function(yr) {
      fp_enduse_origin(
        year       = yr,
        extension  = ext,
        allocation = allocation,
        regions    = regions, io = io, items = items_full_bcp,
        X = X, Y = Y, E = E_bar,     # L loaded per-year inside the function
        save       = FALSE
      )
    }),
    use.names = TRUE, fill = TRUE
  )
  
  fname <- build_filename("FABIO_endUseOrigin",
                          years     = paste0(min(years_to_run), "-", max(years_to_run)),
                          indicator = ext,
                          alloc     = allocation)
  fwrite(enduse_bcp, file.path(OUT_DIR, fname))
  message("Wrote ", file.path(OUT_DIR, fname))
}

######################################################################################################################
################# FUNCTIONS NOT CURRENTLY USED (kept in case we want to re-use them) ##################################
######################################################################################################################


# ###########################################################
# ########### ENVIRONMENTAL INTENSITY (per unit of output)
# ########### OF SELECTED COMMODITIES PRODUCED IN COUNTRIES
# ########### BY FEEDSTOCK (DIRECT REQUIREMENT)
# ########### BY CONTINENT WHERE IMPACTS OCCUR
# ###########################################################
#
# intensity_feedstock <- function(country         = NULL,
#                                 year,
#                                 extension,
#                                 commodity,
#                                 allocation      = "value",
#                                 input_path      = MRIO_PATH,
#                                 losses          = TRUE,
#                                 by_origin       = TRUE,
#                                 drop_zero       = TRUE,
#                                 save            = FALSE,
#                                 output_dir      = OUT_DIR,
#                                 regions, io, ex,
#                                 X = NULL, Z = NULL, E = NULL, L = NULL,
#                                 return_full     = FALSE) {
#
#   sub <- if (losses) "losses/" else ""
#
#   # --- Load only what wasn't passed in ---------------------------------------
#   if (is.null(X)) X <- readRDS(paste0(input_path, sub, "X.rds"))
#   if (is.null(Z)) Z <- readRDS(paste0(input_path, sub, "Z_", allocation, ".rds"))
#   if (!is.null(extension) && is.null(E))
#     E <- readRDS(paste0(base_path, "E.rds"))
#   if (is.null(L)) L <- readRDS(paste0(input_path, sub, year, "_L_", allocation, ".rds"))
#
#   Xi <- X[, as.character(year)]
#   Zi <- Z[[as.character(year)]]
#   Ei <- if (!is.null(extension)) E[[as.character(year)]] else NULL
#
#   # --- Direct extension intensity (per unit output) -------------------------
# if (!is.null(extension)) {
#   if (!extension %in% rownames(Ei))
#     stop("Stressor '", extension, "' not found in rownames(E[[", year, "]]).")
#   ext <- as.numeric(Ei[extension, ]) / as.vector(Xi)
#   ext[!is.finite(ext)] <- 0
# } else {
#   ext <- rep(1, length(Xi))
# }
#   MP <- ext * L                        # MP[origin, sector] = total ext per unit FD of sector
#
#   # --- Validate commodity input ---------------------------------------------
#   unknown_c <- setdiff(commodity, io$comm_code)
#   if (length(unknown_c) > 0) {
#     warning("Unknown comm_codes dropped: ", paste(unknown_c, collapse = ", "))
#     commodity <- intersect(commodity, io$comm_code)
#   }
#   if (length(commodity) == 0) stop("No valid comm_codes supplied.")
#
#   # --- Resolve producer country list ----------------------------------------
#   if (is.null(country)) country <- unique(io$iso3c)
#   unknown <- setdiff(country, io$iso3c)
#   if (length(unknown) > 0) {
#     warning("Unknown countries dropped: ", paste(unknown, collapse = ", "))
#     country <- setdiff(country, unknown)
#   }
#   if (length(country) == 0) stop("No valid countries supplied.")
#
#   io_label <- paste0(io$iso3c, "_", io$item)
#
#   # --- Helper: per-producer, per-commodity intensity ------------------------
#   compute_for <- function(ctry, cc) {
#     target_idx <- which(io$iso3c == ctry & io$comm_code == cc)
#     if (length(target_idx) == 0) return(NULL)
#
#     x_target <- Xi[target_idx]
#     if (x_target == 0) return(NULL)               # no output -> no intensity defined
#
#     # Technical coefficient column: a[, target] = Z[, target] / X[target]
#     a_vec   <- as.vector(Zi[, target_idx]) / x_target
#     nz_feed <- which(a_vec != 0)
#     if (length(nz_feed) == 0) return(NULL)
#
#     # Intensity[origin, feedstock] = ext[origin] * L[origin, feedstock] * a[feedstock, target]
#     MP_sub  <- MP[, nz_feed, drop = FALSE]
#     Int_sub <- t(t(MP_sub) * a_vec[nz_feed])
#     Int_sub <- as(Int_sub, "TsparseMatrix")
#
#     if (length(Int_sub@x) == 0) return(NULL)
#
#     feed_lbl <- io_label[nz_feed]
#     dt <- data.table(
#       country_producer = ctry,
#       commodity        = cc,
#       origin           = io_label[Int_sub@i + 1],
#       feedstock_full   = feed_lbl[Int_sub@j + 1],
#       value            = Int_sub@x
#     )
#
#     if (drop_zero) dt <- dt[value != 0]
#     dt
#   }
#
#   # --- Loop over (country_producer, commodity) ------------------------------
#   grid <- CJ(ctry = country, cc = commodity, sorted = FALSE)
#   per_cell <- rbindlist(
#     Map(compute_for, grid$ctry, grid$cc),
#     use.names = TRUE, fill = TRUE
#   )
#
#   if (nrow(per_cell) == 0) {
#     warning("Empty intensity for year ", year, " / countries: ",
#             paste(country, collapse = ","), " / commodities: ",
#             paste(commodity, collapse = ","))
#     return(per_cell)
#   }
#
#   # --- Annotate --------------------------------------------------------------
#   indicator_label <- if (!is.null(extension)) extension else "material"
#
#   per_cell[, `:=`(
#     year              = year,
#     indicator         = indicator_label,
#     allocation        = allocation,
#     country_origin    = substr(origin, 1, 3),
#     item_origin       = substr(origin, 5, 100),
#     country_feedstock = substr(feedstock_full, 1, 3),
#     feedstock         = substr(feedstock_full, 5, 100)
#   )]
#   per_cell[, continent_origin :=
#              regions$continent[match(country_origin, regions$iso3c)]]
#
#   # --- Aggregate to country_producer x year x commodity x feedstock --------
#   group_cols <- c("country_producer", "year", "indicator", "allocation",
#                   "commodity", "feedstock")
#   if (by_origin) group_cols <- c(group_cols, "continent_origin")
#
#   agg_results <- per_cell[, .(value = sum(value)), by = group_cols]
#   setcolorder(agg_results, group_cols)
#
#   if (drop_zero) agg_results <- agg_results[value != 0]
#
#   if (save) {
#     dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
#     fname <- build_filename("FABIO_intensity",
#                             country   = if (length(country) > 4) NULL else country,
#                             year      = year,
#                             indicator = indicator_label,
#                             alloc     = allocation,
#                             comm      = commodity,
#                             byOrigin  = by_origin)
#     fwrite(agg_results, file.path(output_dir, fname))
#     message("Wrote ", file.path(output_dir, fname))
#   }
#
#   if (return_full) list(results = per_cell, agg = agg_results) else agg_results
# }
#
# # --- Run intensities for biogasoline / biodiesel / renewable diesel ---------
# years_to_run <- 2012:2022
#
# intensity_land_bcp <- rbindlist(
#   lapply(years_to_run, function(yr) {
#     intensity_feedstock(
#       year       = yr,
#       extension  = "land_harv",
#       commodity  = c("c146", "c147", "c149"),
#       allocation = allocation,
#       input_path = input_path,
#       regions    = regions, io = io, ex = ex,
#       X = X, Z = Z, E = E_bar,
#       save       = FALSE
#     )
#   }),
#   use.names = TRUE, fill = TRUE
# )
#
# fname <- build_filename(
#   "FABIO_intensity",
#   years     = paste0(min(years_to_run), "-", max(years_to_run)),
#   indicator = "land_harv",
#   alloc     = allocation,
#   comm      = c("c146", "c147", "c149"),
#   byOrigin  = TRUE
# )
# fwrite(intensity_land_bcp, file.path(OUT_DIR, fname))
# message("Wrote ", file.path(OUT_DIR, fname))