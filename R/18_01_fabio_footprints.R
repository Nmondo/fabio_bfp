# 18 - Calculate footprints from FABIO MRIO

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
setwd(fabio_root)

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
############################## MAKING FUNCTIONS TO EXTRACT RESULTS #######################
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
                         input_path    = "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/",
                         losses        = TRUE,
                         by_commodity  = TRUE,
                         by_country    = TRUE,
                         top_n         = NULL,        # NEW: keep top-N feedstocks per (country, commodity); NULL = all
                         drop_zero     = TRUE,        # NEW: drop zero-value entries early
                         save          = FALSE,
                         output_dir    = "output",
                         regions, io, fd, ex,
                         X = NULL, Y = NULL, Z = NULL, E = NULL, L = NULL,
                         return_full   = FALSE) {
  
  sub <- if (losses) "losses/" else ""
  
  # --- Load only what wasn't passed in ---------------------------------------
  if (is.null(X)) X <- readRDS(paste0(input_path, sub, "X.rds"))
  if (is.null(Y)) Y <- readRDS(paste0(input_path, sub, "Y.rds"))
  if (is.null(Z)) Z <- readRDS(paste0(input_path, sub, "Z_", allocation, ".rds"))
  if (!is.null(extension) && is.null(E))
    E <- readRDS(paste0(input_path, "E.rds"))
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
    # f is the per-target feedstock loading; aggregating by feedstock item
    # gives one number per feedstock, which is what top_n should rank on.
    if (!is.null(top_n) && top_n < length(unique(io$item))) {
      f_by_item <- tapply(f, io$item, sum)
      keep_items <- names(sort(f_by_item, decreasing = TRUE))[seq_len(top_n)]
      f[!(io$item %in% keep_items)] <- 0
      if (sum(f) == 0) return(NULL)
    }
    
    # --- footprint matrix, sparse from the start --------------------------
    # build directly as a TsparseMatrix to avoid the dense intermediate
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
########### OF SELECTED COUNTRIES (default: all)
########### OF SELECTED COMMODITIES
########### BY CONTINENT WHERE IMPACTS OCCUR
########### FULL UPSTREAM, INCL. INDIRECT REQUIREMENTS
###########################################################
# Counterpart to fp_feedstock(): instead of decomposing the footprint via
# the first-degree feedstock column A[,target] and then propagating through
# L, this function uses the full Leontief inverse directly:

fp_indirect <- function(country       = NULL,
                        year,
                        extension     = NULL,
                        commodity,
                        allocation    = "value",
                        input_path    = "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/",
                        losses        = TRUE,
                        by_commodity  = TRUE,
                        by_country    = TRUE,
                        top_n         = NULL,        # keep top-N origin sectors per (country, commodity)
                        drop_zero     = TRUE,
                        save          = FALSE,
                        output_dir    = "output",
                        regions, io, fd, ex,
                        X = NULL, Y = NULL, E = NULL, L = NULL,
                        return_full   = FALSE) {
  
  sub <- if (losses) "losses/" else ""
  
  # --- Load only what wasn't passed in ---------------------------------------
  # NOTE: Z is intentionally NOT loaded here. The whole point of the "all-the-way"
  # version is to skip the explicit A[,target] step and let L handle every stage
  # of the upstream chain.
  if (is.null(X)) X <- readRDS(paste0(input_path, sub, "X.rds"))
  if (is.null(Y)) Y <- readRDS(paste0(input_path, sub, "Y.rds"))
  if (!is.null(extension) && is.null(E))
    E <- readRDS(paste0(input_path, "E.rds"))
  if (is.null(L)) L <- readRDS(paste0(input_path, sub, year, "_L_", allocation, ".rds"))
  
  Xi <- X[, as.character(year)]
  Yi <- Y[[as.character(year)]]
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
    Y_country <- Yi[, fd$iso3c == ctry, drop = FALSE]
    y_full <- as.vector(rowSums(as.matrix(Y_country)))
    
    comm_idx <- io$comm_code == cc
    y_comm <- y_full
    y_comm[!comm_idx] <- 0
    
    if (sum(y_comm) == 0) return(NULL)
    
    # --- footprint matrix: full Leontief, no Z step -----------------------
    # Columns of FP_sub correspond to non-zero entries of y_comm (producer
    # countries of the consumed commodity). Rows correspond to all sectors;
    # the actual non-zero rows after sparsification are the impact origins.
    nz_target <- which(y_comm != 0)
    if (length(nz_target) == 0) return(NULL)
    
    MP_sub <- MP[, nz_target, drop = FALSE]
    FP_sub <- t(t(MP_sub) * y_comm[nz_target])
    FP_sub <- as(FP_sub, "TsparseMatrix")
    
    if (length(FP_sub@x) == 0) return(NULL)
    
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
  # Attribution dimension: item_origin (where the impact physically occurs
  # after the full upstream chain), replacing the "feedstock" (= 1st-degree
  # input item) used in fp_feedstock().
  group_cols <- c("year", "indicator", "allocation",
                  "item_origin", "continent_origin")
  if (by_country)   group_cols <- c("country_consumer", group_cols)
  if (by_commodity) group_cols <- c(group_cols, "commodity")
  
  agg_results <- per_cell[, .(value = sum(value)), by = group_cols]
  setcolorder(agg_results, group_cols)
  
  if (drop_zero) agg_results <- agg_results[value != 0]
  
  # --- top-N filter on aggregated origin sectors ----------------------------
  # Done post-aggregation: with full L there is no compact pre-FP vector to
  # rank on (unlike `f` in fp_feedstock). Ranking is per (consumer, commodity)
  # if those dimensions are kept; otherwise global.
  if (!is.null(top_n)) {
    keep_grp <- c(if (by_country)   "country_consumer",
                  if (by_commodity) "commodity")
    if (length(keep_grp) > 0) {
      agg_results[, .rk := frank(-value, ties.method = "first"), by = keep_grp]
    } else {
      agg_results[, .rk := frank(-value, ties.method = "first")]
    }
    agg_results <- agg_results[.rk <= top_n][, .rk := NULL]
  }
  
  if (save) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    fname <- build_filename("FABIO_totalreq",
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
                                         input_path   = "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/",
                                         losses       = TRUE,
                                         by_commodity = FALSE,
                                         by_feedstock = TRUE,
                                         indirect     = TRUE,
                                         bilateral    = FALSE,
                                         drop_zero    = TRUE,
                                         save         = FALSE,
                                         output_dir   = "output",
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
    E <- readRDS(paste0(input_path, "E.rds"))
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
  #   indirect = TRUE  -> M = L : feedstock + its full upstream chain   [original]
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




######################################################################################################################
############################## HOW TO USE THE FUNCTIONS: EXAMPLE #######################
######################################################################################################################



## 2012-2022, saving impacts embodied in each year's bilateral trade, with feedstock breakdown
ibif_stressors <- unique(ex[Stressor %like% "ibif", Stressor])
# FD_EQ_terrestrial_stressors <- unique(ex[grepl("^FD_EQ.*terrestrial", Stressor), Stressor])
extensions_choice <- as.list(c(ibif_stressors,
                               "LCIM_EQ_terrestrial_climate","LCIM_EQ_terrestrial_acidification",
                               "land_harv")
)




######################################################################################################################
############################## LOOPS TO EXTRACT THE RESULTS #######################
######################################################################################################################

# Silenced loops are those which have already run.

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

extensions_choice <- as.list(c("ibif_total", "LCIM_EQ_terrestrial", "land_harv"))

for (yr in 2012:2022) {
  for (ext in extensions_choice) {
    ext_val <- ext
    message("--- ", yr, " | ext: ", ext, " ---")
    fp_feedstock(
      country      = NULL, #all countries
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



for (yr in 2012:2022) {
  message("--- ", yr, " ---")
  fp_indirect(
    year         = yr,
    extension    = NULL,
    commodity    = c("c146", "c147", "c149"),
    by_commodity = TRUE,
    by_country   = TRUE,
    drop_zero    = TRUE,
    save         = TRUE,
    output_dir  = "output",
    regions = regions, io = io, fd = fd, ex = ex,
    X = X, Y = Y, E = E_bar
  )
}



for (yr in 2012:2022) {  
  fp_trade_breakdown_feedstock(
    year         = yr,
    extension    = NULL,           # material mode -> feedstock tonnes (E ignored here)
    commodity    =  c("c146", "c147", "c149"),
    allocation   = "value",
    by_commodity = TRUE,
    by_feedstock = TRUE,
    indirect     = FALSE,        
    bilateral    = TRUE,
    drop_zero    = TRUE,
    save         = TRUE,
    output_dir   = "output",
    regions = regions, io = io, fd = fd, ex = ex,
    X = X, Y = Y, Z = Z,
    E = E_bar,                    
    L = NULL                       
  )
}






########## Biopolymers ##########

for (yr in 2012:2022) {
  message("--- ", yr, " ---")
  fp_trade_breakdown_feedstock(
    year         = yr,
    extension    = "ibif_total",
    commodity    = bp_set,
    by_commodity = TRUE,
    by_feedstock = TRUE,
    bilateral    = TRUE,
    save         = TRUE,
    regions = regions, io = io, fd = fd, ex = ex,
    X = X, Y = Y, Z = Z, E = E_bar
  )
}







######################################################################################################################
############################## SAVE DESCRIPTIVE STATISTICS (BIOFUEL CONSUMPTION, FEEDSTOCK COMPOSITION) #######################
######################################################################################################################

extract_Y_long <- function(Y, commodities = c("c146", "c147", "c149"),
                           uses = c("fuel", "other_industrial")) {
  
  # build patterns once
  row_pattern <- paste0("_(", paste(commodities, collapse = "|"), ")$")
  col_pattern <- paste0("_(", paste(uses, collapse = "|"), ")$")
  
  out <- rbindlist(lapply(names(Y), function(yr) {
    M <- Y[[yr]]
    
    # subset rows (commodities) and cols (uses)
    row_idx <- grep(row_pattern, rownames(M))
    col_idx <- grep(col_pattern, colnames(M))
    
    if (length(row_idx) == 0 || length(col_idx) == 0) return(NULL)
    
    M_sub <- M[row_idx, col_idx, drop = FALSE]
    
    # aggregate uses (fuel + other_industrial) per target_country
    target_country <- substr(colnames(M_sub), 1, 3)
    M_agg <- t(rowsum(t(as.matrix(M_sub)), group = target_country))
    
    # melt to long
    dt <- as.data.table(as.table(M_agg))
    setnames(dt, c("row_id", "target_country", "value"))
    
    # parse row_id -> origin_country + comm_code
    dt[, `:=`(
      origin_country = substr(row_id, 1, 3),
      comm_code      = sub(".*_", "", row_id),
      year           = as.integer(yr)
    )]
    
    dt[value > 0, .(year, origin_country, target_country, comm_code, value)]
  }))
  
  out
}

# usage
dt_Y_long <- extract_Y_long(Y)
dt_Y_BP <- extract_Y_long(Y, commodities = c("c060", "c146", "c148", c(paste0("c",150:170))),
                          uses = c("other_industrial", "unknown"))

fwrite(dt_Y_long, file.path("output", "Y_summary_c146_c147_c149.csv"))
fwrite(dt_Y_BP, file.path("output", "Y_summary_BP.csv"))



extract_Z_long <- function(Z, commodities = c("c146", "c147", "c149")) {
  
  col_pattern <- paste0("_(", paste(commodities, collapse = "|"), ")$")
  
  out <- rbindlist(lapply(names(Z), function(yr) {
    M <- Z[[yr]]
    
    # subset cols (target = selected commodities)
    col_idx <- grep(col_pattern, colnames(M))
    if (length(col_idx) == 0) return(NULL)
    
    M_sub <- M[, col_idx, drop = FALSE]
    
    # keep only non-zero rows
    row_keep <- which(rowSums(abs(M_sub)) > 0)
    if (length(row_keep) == 0) return(NULL)
    M_sub <- M_sub[row_keep, , drop = FALSE]
    
    # extract non-zero elements directly from sparse triplet form
    trip <- as(M_sub, "TsparseMatrix")
    
    dt <- data.table(
      origin_id = rownames(M_sub)[trip@i + 1L],
      target_id = colnames(M_sub)[trip@j + 1L],
      value     = trip@x
    )
    
    # parse ids
    dt[, `:=`(
      origin_country = substr(origin_id, 1, 3),
      origin_comm    = sub(".*_", "", origin_id),
      target_country = substr(target_id, 1, 3),
      target_comm    = sub(".*_", "", target_id),
      year           = as.integer(yr)
    )]
    
    dt[, .(year, origin_country, origin_comm, target_country, target_comm, value)]
  }))
  
  # join commodity names for origin_comm
  out <- merge(
    out,
    items[, .(origin_comm = comm_code, origin_comm_name = item)],
    by = "origin_comm",
    all.x = TRUE,
    sort = FALSE
  )
  
  setcolorder(out, c("year", "origin_country", "origin_comm", "origin_comm_name",
                     "target_country", "target_comm", "value"))
  out[]
}

# usage
dt_Z_long <- extract_Z_long(Z)

fwrite(dt_Z_long, file.path("output", "Z_summary_c146_c147_c149.csv"))







######################################################################################################################
############################## DIRECT BIOFUEL SUPPLY-CHAIN FLOWS (3-TIER, CONTINENT) ##################################
######################################################################################################################
# Builds the two DIRECT linkages of the biofuel supply chain while keeping the
# biofuel PRODUCER as an explicit middle node (this is what fp_trade_breakdown_feedstock
# cannot give you: it multiplies feedstock by X_comm_norm and integrates the producer out,
# so feedstock lands on the *final consumer*). Here the producer is preserved:
#
#   Stage 1  feedstock_to_producer : 1st-degree feedstock m, origin i  -> producer p
#            direct tier-1 input, straight from Z[ m-rows , cc-cols ].  Unit: feedstock tonnes.
#   Stage 2  producer_to_consumer  : biofuel cc,             producer p -> consumer k
#            "final_demand" -> Y_country[cc,] (same basis as y_sourcing_shares, fully direct)
#            "consumption"  -> (L %*% Y_masked)[cc,]  (L-allocated, captures biofuel used indirectly)
#            Unit: biofuel output.
#
# The producer is the SHARED node, so the two legs connect into one chain.
#
# Domestic vs intra-regional is resolved at COUNTRY level BEFORE continent aggregation,
# so a within-continent flow keeps its split:
#   domestic       : same iso3c
#   intra_regional : same continent, different iso3c
#   inter_regional : different continent
#
# Output is long & flow-chart ready (source_node / target_node carry the tier so the three
# tiers stay distinct in a left->right Sankey; producer-tier nodes are shared across stages).
#
# NB on units: stage-1 (feedstock tonnes) and stage-2 (biofuel output) do NOT balance at the
# producer node. Keep them as two stacked sub-flows, or normalise per stage, when you draw it.

bf_supply_chain_flows <- function(years,
                                  biofuel,                         # cc codes, e.g. c("c146","c147","c149")
                                  feedstock_codes = NULL,          # restrict stage-1 inputs; NULL = all tier-1 inputs
                                  stage2_basis    = "final_demand",# "final_demand" (Y, direct) | "consumption" (L-allocated)
                                  level           = "continent",   # "continent" | "country" (no aggregation)
                                  clamp_neg       = FALSE,         # set negative flows (e.g. stock changes in Y) to 0
                                  drop_zero       = TRUE,
                                  input_path      = "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/",
                                  losses          = TRUE,
                                  allocation      = "value",
                                  save            = FALSE,
                                  output_dir      = "output",
                                  regions, io, fd,
                                  X = NULL, Y = NULL, Z = NULL, L = NULL) {
  
  sub <- if (losses) "losses/" else ""
  if (is.null(X)) X <- readRDS(paste0(input_path, sub, "X.rds"))
  if (is.null(Y)) Y <- readRDS(paste0(input_path, sub, "Y.rds"))
  if (is.null(Z)) Z <- readRDS(paste0(input_path, sub, "Z_", allocation, ".rds"))
  
  biofuel <- intersect(biofuel, unique(io$comm_code))
  if (length(biofuel) == 0) stop("No valid biofuel comm_codes supplied.")
  
  # iso3c -> continent (works whether `regions` is data.table or data.frame)
  cont_vec <- setNames(as.character(regions$continent), regions$iso3c)
  feedstock_map <- unique(as.data.table(io)[, .(comm_code, item)])
  
  per_year <- lapply(years, function(yr) {
    yc <- as.character(yr)
    Yi <- Y[[yc]]; colnames(Yi) <- fd$iso3c
    Y_country <- agg(Yi)                              # RN x R, FD collapsed within consumer
    countries <- colnames(Y_country)
    Zi <- Z[[yc]]
    
    Li <- NULL
    if (stage2_basis == "consumption") {
      Li <- if (is.null(L)) readRDS(paste0(input_path, sub, yr, "_L_", allocation, ".rds")) else L[[yc]]
    }
    
    rbindlist(lapply(biofuel, function(cc) {
      
      cc_idx    <- which(io$comm_code == cc)          # one row/col per producer country
      producers <- io$iso3c[cc_idx]
      
      ## ---- STAGE 1 : feedstock origin -> producer (direct, from Z) ----------------------
      Zcc <- Zi[, cc_idx, drop = FALSE]               # RN x P
      active   <- which(Matrix::rowSums(Zcc) != 0)
      active_m <- unique(io$comm_code[active])
      if (!is.null(feedstock_codes)) active_m <- intersect(active_m, feedstock_codes)
      
      s1 <- if (length(active_m)) rbindlist(lapply(active_m, function(m) {
        rows_m <- which(io$comm_code == m)
        M      <- as.matrix(Zcc[rows_m, , drop = FALSE])          # origin_i x producer_p
        data.table(
          origin_iso = rep(io$iso3c[rows_m], times = ncol(M)),    # varies fastest (as.vector col-major)
          dest_iso   = rep(producers,        each  = nrow(M)),
          value      = as.vector(M),
          feedstock  = feedstock_map$item[match(m, feedstock_map$comm_code)]
        )
      }), use.names = TRUE) else NULL
      
      if (!is.null(s1)) s1[, `:=`(stage = "feedstock_to_producer",
                                  tier_from = "origin", tier_to = "producer",
                                  unit = "feedstock_tonnes")]
      
      ## ---- STAGE 2 : producer -> consumer (biofuel) -------------------------------------
      if (stage2_basis == "final_demand") {
        B <- as.matrix(Y_country[cc_idx, , drop = FALSE])         # producer_p x consumer_k
      } else {                                                    # "consumption" (L-allocated)
        Y_masked <- Y_country; Y_masked[-cc_idx, ] <- 0
        B <- as.matrix((Li %*% Y_masked)[cc_idx, , drop = FALSE]) # producer_p x consumer_k
      }
      s2 <- data.table(
        origin_iso = rep(producers, times = ncol(B)),
        dest_iso   = rep(countries, each  = nrow(B)),
        value      = as.vector(B),
        feedstock  = NA_character_,
        stage      = "producer_to_consumer",
        tier_from  = "producer", tier_to = "consumer",
        unit       = "biofuel_output"
      )
      
      out <- rbindlist(list(s1, s2), use.names = TRUE, fill = TRUE)
      out[, `:=`(biofuel = cc, year = yr)]
      out
    }), use.names = TRUE, fill = TRUE)
  })
  
  dt <- rbindlist(per_year, use.names = TRUE, fill = TRUE)
  if (clamp_neg) dt[value < 0, value := 0]
  if (drop_zero) dt <- dt[value != 0]
  if (nrow(dt) == 0) { warning("No flows produced."); return(dt[]) }
  
  # ---- classify at COUNTRY level (before any continent aggregation) ----------------------
  dt[, `:=`(source_continent = cont_vec[origin_iso],
            target_continent = cont_vec[dest_iso])]
  if (anyNA(dt$source_continent) || anyNA(dt$target_continent))
    warning("Some iso3c had no continent in `regions`; check NA source/target_continent.")
  
  dt[, flow_class := fcase(
    origin_iso == dest_iso,                "domestic",
    source_continent == target_continent,  "intra_regional",
    default =                              "inter_regional")]
  
  if (level == "continent") {
    keys <- c("stage", "tier_from", "tier_to", "year", "biofuel", "feedstock", "unit",
              "source_continent", "target_continent", "flow_class")
    dt <- dt[, .(value = sum(value)), by = keys]
    # tier-tagged node ids so the 3 tiers stay distinct in a left->right flow chart;
    # producer-tier nodes are identical across the two stages -> they connect.
    dt[, `:=`(source_node = paste(source_continent, tier_from, sep = " | "),
              target_node = paste(target_continent, tier_to,   sep = " | "))]
    setcolorder(dt, c("stage", "year", "biofuel", "feedstock",
                      "source_continent", "target_continent",
                      "source_node", "target_node", "flow_class", "unit", "value"))
    setorderv(dt, c("year", "biofuel", "stage", "source_continent", "target_continent", "flow_class"))
  } else {
    setnames(dt, c("origin_iso", "dest_iso"), c("source_iso", "target_iso"))
    setcolorder(dt, c("stage", "year", "biofuel", "feedstock",
                      "source_iso", "target_iso",
                      "source_continent", "target_continent", "flow_class", "unit", "value"))
    setorderv(dt, c("year", "biofuel", "stage", "source_iso", "target_iso"))
  }
  
  if (save) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    yr_slug <- if (length(years) == 1) as.character(years) else paste0(min(years), "-", max(years))
    fname <- build_filename("FABIO_bfChain",
                            year  = yr_slug,
                            comm  = biofuel,
                            basis = stage2_basis,
                            level = level)
    fwrite(dt, file.path(output_dir, fname))
    message("Wrote ", file.path(output_dir, fname))
  }
  dt[]
}


# --- Call -------------------------------------------------------------------------
bf_supply_chain_flows(
  years        = 2012:2022,
  biofuel      = c("c146", "c147", "c149"),
  stage2_basis = "final_demand",
  level        = "continent",
  regions = regions, io = io, fd = fd,
  X = X, Y = Y, Z = Z,          
  save = TRUE
)




######################################################################################################################
############################## FINAL-DEMAND SOURCING SHARES (DIRECT FROM Y) ###########################################
######################################################################################################################
# For each year & commodity: rows of Y filtered to `commodity` (origin = iso3 prefix),
# columns summed within consumer iso3 (across all FD components). Share = each origin's
# supply / consumer's total final consumption of that commodity. Global, country detail.

y_sourcing_shares <- function(years,
                              commodity,
                              by_commodity = TRUE,
                              include_self = TRUE,
                              consumer     = NULL,        # NULL = all consumers
                              wide         = FALSE,       # one row per (consumer, year, commodity)
                              clamp_neg    = FALSE,       # set negative FD entries (e.g. stock changes) to 0
                              save         = FALSE,
                              output_dir   = "output",
                              input_path   = "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/",
                              losses       = TRUE,
                              io, fd,
                              Y = NULL) {
  
  sub <- if (losses) "losses/" else ""
  if (is.null(Y)) Y <- readRDS(paste0(input_path, sub, "Y.rds"))
  
  commodity <- intersect(commodity, unique(io$comm_code))
  if (length(commodity) == 0) stop("No valid comm_codes supplied.")
  
  per_year <- lapply(years, function(yr) {
    Yi <- Y[[as.character(yr)]]
    colnames(Yi) <- fd$iso3c
    Y_country <- agg(Yi)                 # RN × R: FD columns summed within consumer
    countries <- colnames(Y_country)
    
    rbindlist(lapply(commodity, function(cc) {
      rows_cc <- which(io$comm_code == cc)              # one row per origin country
      M_cc    <- as.matrix(Y_country[rows_cc, , drop = FALSE])   # origin × consumer
      if (clamp_neg) M_cc[M_cc < 0] <- 0
      origin  <- io$iso3c[rows_cc]
      
      data.table(
        country_origin   = rep(origin,    times = ncol(M_cc)),
        country_consumer = rep(countries, each  = nrow(M_cc)),
        value            = as.vector(M_cc),
        commodity        = cc,
        year             = yr
      )
    }), use.names = TRUE)
  })
  
  dt <- rbindlist(per_year, use.names = TRUE)
  if (!is.null(consumer)) dt <- dt[country_consumer %in% consumer]
  
  if (!by_commodity) {
    dt <- dt[, .(value = sum(value)),
             by = .(country_origin, country_consumer, year)]
  }
  
  key_cols <- c("country_consumer", "year")
  if (by_commodity) key_cols <- c(key_cols, "commodity")
  
  dt[, .tot := sum(value), by = key_cols]      # consumer's total final consumption of cc
  dt <- dt[.tot > 0]                            # undefined/degenerate shares dropped
  dt[, share := value / .tot][, .tot := NULL]
  
  if (!include_self) dt <- dt[country_origin != country_consumer]
  
  dt[, flow_type := fifelse(country_origin == country_consumer, "self", "trade")]
  setcolorder(dt, c(key_cols, "country_origin", "flow_type", "value", "share"))
  setorderv(dt, c(key_cols, "share"), order = c(rep(1L, length(key_cols)), -1L))
  
  if (wide) {
    dt <- dcast(dt,
                as.formula(paste(paste(key_cols, collapse = " + "), "~ country_origin")),
                value.var = "share", fill = 0)
  }
  
  # --- Optionally save -----------------------------------------------------
  if (save) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    yr_slug <- if (length(years) == 1) as.character(years)
    else paste0(min(years), "-", max(years))
    fname <- build_filename("FABIO_Ysourcing",
                            year     = yr_slug,
                            comm     = commodity,
                            byComm   = by_commodity,
                            inclSelf = include_self,
                            wide     = wide)
    fwrite(dt, file.path(output_dir, fname))
    message("Wrote ", file.path(output_dir, fname))
  }
  
  dt[]
}


y_sourcing_shares(years = 2012:2022, 
                  commodity = bf_set,
                  io = io, 
                  fd = fd, 
                  Y = Y, 
                  save = TRUE)



















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
#                                 input_path      = "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/",
#                                 losses          = TRUE,
#                                 by_origin       = TRUE,
#                                 drop_zero       = TRUE,
#                                 save            = FALSE,
#                                 output_dir      = "output",
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
#     E <- readRDS(paste0(input_path, "E.rds"))
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
# fwrite(intensity_land_bcp, file.path("output", fname))
# message("Wrote ", file.path("output", fname))