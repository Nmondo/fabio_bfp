# 18 - Calculate footprints from FABIO MRIO

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
    ext <- as.numeric(Ei[ex$Stressor == extension, ]) / as.vector(Xi)
    ext[!is.finite(ext)] <- 0
  } else {
    ext <- rep(1, length(Xi))
  }
  MP <- ext * L
  
  # --- Resolve country list -------------------------------------------------
  if (is.null(country)) country <- c(unique(fd$iso3c), "EU27")
  unknown <- setdiff(setdiff(country, "EU27"), fd$iso3c)
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
    if (ctry == "EU27") {
      Y_country <- Yi[, fd$continent == "EU", drop = FALSE]
    } else {
      Y_country <- Yi[, fd$iso3c == ctry, drop = FALSE]
    }
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
########### OF SELECTED COMMODITIES
########### TRADE BREAKDOWN OR BILATERAL FLOWS
########### FOR ALL REPORTING COUNTRIES
###########################################################

fp_trade_breakdown <- function(year,
                               extension    = NULL,        # NULL = material flows
                               commodity    = NULL,        # NULL = all commodities
                               allocation   = "value",
                               input_path   = "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/",
                               losses       = TRUE,
                               by_commodity = FALSE,
                               bilateral    = FALSE,
                               drop_zero    = TRUE,
                               save         = FALSE,
                               output_dir   = "output",
                               regions, io, fd, ex,
                               X = NULL, Y = NULL, E = NULL, L = NULL,
                               return_full  = FALSE) {
  
  sub <- if (losses) "losses/" else ""
  
  # --- Load only what wasn't passed in ---------------------------------------
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
    ext <- as.numeric(Ei[ex$Stressor == extension, ]) / as.vector(Xi)
    ext[!is.finite(ext)] <- 0
  } else {
    ext <- NULL                           # signals material mode to compute_oc
  }
  
  # --- Final demand collapsed to one column per consumer country ------------
  colnames(Yi) <- fd$iso3c
  Y_country    <- agg(Yi)
  countries    <- colnames(Y_country)
  
  # --- Validate commodity input ---------------------------------------------
  if (is.null(commodity)) commodity <- unique(io$comm_code)
  unknown <- setdiff(commodity, io$comm_code)
  if (length(unknown) > 0) {
    warning("Unknown comm_codes dropped: ", paste(unknown, collapse = ", "))
    commodity <- intersect(commodity, io$comm_code)
  }
  if (length(commodity) == 0) stop("No valid comm_codes supplied.")
  
  # --- Sparse row aggregator: (R·N) → R, by country of origin ---------------
  row_agg <- sparseMatrix(
    i    = seq_len(nrow(io)),
    j    = match(io$iso3c, countries),
    x    = 1,
    dims = c(nrow(io), length(countries))
  )
  
  # --- Origin × consumer impact matrix for a commodity set ------------------
  compute_oc <- function(cc_set) {
    mask <- io$comm_code %in% cc_set
    Y_masked <- Y_country
    Y_masked[!mask, ] <- 0
    
    x_triggered <- L %*% Y_masked
    # skip identity multiply in material mode
    impact <- if (!is.null(ext)) {
      Diagonal(x = ext) %*% x_triggered
    } else {
      x_triggered
    }
    
    impact_oc <- as.matrix(t(row_agg) %*% impact)
    dimnames(impact_oc) <- list(countries, countries)
    impact_oc
  }
  
  # --- self / exports / imports breakdown -----------------------------------
  build_breakdown <- function(impact_oc, label_commodity = NA_character_) {
    d  <- diag(impact_oc)
    rs <- rowSums(impact_oc)
    cs <- colSums(impact_oc)
    
    data.table(
      country          = rownames(impact_oc),
      self_consumption = d,
      exports          = rs - d,
      imports          = cs - d,
      commodity        = label_commodity
    )
  }
  
  # --- bilateral long-format from R x R matrix -----------------------------
  build_bilateral <- function(impact_oc, label_commodity = NA_character_) {
    n <- nrow(impact_oc)
    out <- data.table(
      country_origin   = rep(rownames(impact_oc), times = n),
      country_consumer = rep(colnames(impact_oc), each  = n),
      value            = as.vector(impact_oc),
      commodity        = label_commodity
    )
    if (drop_zero) out <- out[value != 0]
    out[, flow_type := fifelse(country_origin == country_consumer, "self", "trade")]
    out
  }
  
  # --- Compute and assemble -------------------------------------------------
  indicator_label <- if (!is.null(extension)) extension else "material"
  
  if (bilateral) {
    matrices <- if (by_commodity) {
      setNames(lapply(commodity, compute_oc), commodity)
    } else {
      list(compute_oc(commodity))
    }
    
    long <- if (by_commodity) {
      rbindlist(lapply(commodity, function(cc) {
        build_bilateral(matrices[[cc]], label_commodity = cc)
      }), use.names = TRUE)
    } else {
      tmp <- build_bilateral(matrices[[1]])
      tmp[, commodity := NULL][]
    }
    
    long[, `:=`(year = year, indicator = indicator_label, allocation = allocation)]
    id_vars <- c("country_origin", "country_consumer", "flow_type",
                 "year", "indicator", "allocation")
    if (by_commodity) id_vars <- c(id_vars, "commodity")
    setcolorder(long, c(id_vars, "value"))
    
    out_to_save <- long
    
  } else {
    # --- Non-bilateral: trade breakdown ------------------------------------
    if (by_commodity) {
      wide <- rbindlist(lapply(commodity, function(cc) {
        build_breakdown(compute_oc(cc), label_commodity = cc)
      }), use.names = TRUE)
    } else {
      wide <- build_breakdown(compute_oc(commodity))
      wide[, commodity := NULL]
    }
    
    wide[, `:=`(year = year, indicator = indicator_label, allocation = allocation)]
    
    id_vars <- c("country", "year", "indicator", "allocation")
    if (by_commodity) id_vars <- c(id_vars, "commodity")
    
    long <- melt(wide,
                 id.vars       = id_vars,
                 measure.vars  = c("self_consumption", "exports", "imports"),
                 variable.name = "category",
                 value.name    = "value")
    long[, category := as.character(category)]
    
    setcolorder(wide, c(id_vars, "self_consumption", "exports", "imports"))
    setcolorder(long, c(id_vars, "category", "value"))
    
    out_to_save <- long
  }
  
  if (save) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    fname <- build_filename("FABIO_trade",
                            year      = year,
                            indicator = indicator_label,
                            alloc     = allocation,
                            comm      = commodity,
                            byComm    = by_commodity,
                            bilat     = bilateral)
    fwrite(out_to_save, file.path(output_dir, fname))
    message("Wrote ", file.path(output_dir, fname))
  }
  
  if (return_full) {
    if (bilateral) return(list(matrices = matrices, long = long))
    return(list(wide = wide, long = long))
  }
  out_to_save
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
    ext <- as.numeric(Ei[ex$Stressor == extension, ]) / as.vector(Xi)
    ext[!is.finite(ext)] <- 0
  } else {
    ext <- rep(1, length(Xi))   # material flow: no environmental weighting
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
  
  # --- Pre-aggregate origin: G[r, j] = sum_{i in country r} ext[i] * L[i, j]
  # skip identity multiply in material mode
  MP <- if (!is.null(extension)) Diagonal(x = ext) %*% L else L
  G  <- crossprod(T_origin, MP)            # R × RN
  
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
# X, Y, Z, E are pre-loaded above; we pass them in to avoid re-reading the
# .rds files every call. Only L (year-specific) is read inside the function.

# # --- Environmental footprint (land) ---
# out_feedstock <- fp_feedstock(
#   country      = "CHN", 
#   year = 2022,
#   extension    = "land_harv",
#   commodity    = c("c146", "c147", "c149"),
#   by_commodity = TRUE,
#   regions = regions, io = io, fd = fd, ex = ex,
#   X = X, Y = Y, Z = Z, E = E_bar
# )



# 2012-2022, saving impacts embodied in each year's bilateral trade, with feedstock breakdown
ibif_stressors <- unique(ex[Stressor %like% "ibif", Stressor])
extensions_choice <- c(list("land_harv"), as.list(ibif_stressors))


######################################################################################################################
############################## LOOPS TO EXTRACT THE RESULTS #######################
######################################################################################################################

## Silenced loops are those which have already been run. 

# for (yr in 2012:2022) {
#   for (ext in extensions_choice) {
#     ext_val <- ext
#   message("--- ", yr, " | ext: ", ext, " ---")
#   fp_trade_breakdown_feedstock(
#     year         = yr,
#     extension    = ext_val,
#     commodity    = c("c146", "c147", "c149"),
#     by_commodity = TRUE,
#     by_feedstock = TRUE,
#     bilateral    = TRUE,
#     save         = TRUE,
#     regions = regions, io = io, fd = fd, ex = ex,
#     X = X, Y = Y, Z = Z, E = E_bar
#   )
#   }
# }

extensions_choice <- c(list("land_harv"))

# for (yr in 2012:2022) {
#   for (ext in extensions_choice) {
#     ext_val <- ext
#     message("--- ", yr, " | ext: ", ext, " ---")
#     fp_feedstock(
#       country      = NULL, #all countries 
#       year = yr,
#       extension    = ext_val,
#       commodity    = c("c146", "c147", "c149"),
#       by_commodity = TRUE,
#       save = TRUE,
#       regions = regions, io = io, fd = fd, ex = ex,
#       X = X, Y = Y, Z = Z, E = E_bar
# )
#   }
# }
# 
# 




###########################################################
########### STRUCTURAL DECOMPOSITION ANALYSIS
########### OF CONSUMPTION-BASED FOOTPRINT
########### Effects: intensity, technique, scale, composition, origin
###########################################################

fp_sda <- function(country         = NULL,
                   year_base,                              # t0
                   year_current,                           # t1
                   extension       = NULL,
                   commodity,                              # vector, e.g. c("c146","c147","c149")
                   allocation      = "value",
                   input_path      = "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/",
                   losses          = TRUE,
                   save            = FALSE,
                   output_dir      = "output",
                   regions, io, fd, ex,
                   X = NULL, Y = NULL, E = NULL,
                   L_base = NULL, L_curr = NULL,
                   return_full     = FALSE) {
  
  sub <- if (losses) "losses/" else ""
  
  # --- Load static data ----------------------------------------------------
  if (is.null(X)) X <- readRDS(paste0(input_path, sub, "X.rds"))
  if (is.null(Y)) Y <- readRDS(paste0(input_path, sub, "Y.rds"))
  if (!is.null(extension) && is.null(E))
    E <- readRDS(paste0(input_path, "E.rds"))
  
  # --- Load year-specific Leontief inverses --------------------------------
  if (is.null(L_base))
    L_base <- readRDS(paste0(input_path, sub, year_base,    "_L_", allocation, ".rds"))
  if (is.null(L_curr))
    L_curr <- readRDS(paste0(input_path, sub, year_current, "_L_", allocation, ".rds"))
  
  # --- Build the 5 factors per year ---------------------------------------
  build_factors <- function(yr, L_yr) {
    Xi <- X[, as.character(yr)]
    Yi <- Y[[as.character(yr)]]
    
    # Intensity vector e
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
  
  # --- Resolve country list ------------------------------------------------
  if (is.null(country)) country <- unique(fd$iso3c)
  unknown <- setdiff(country, fd$iso3c)
  if (length(unknown) > 0) {
    warning("Unknown countries dropped: ", paste(unknown, collapse = ", "))
    country <- setdiff(country, unknown)
  }
  
  # --- Helper: extract (s, sigma, m) from Yi for one country --------------
  # s     : scalar — total final demand for commodity bundle
  # sigma : length |C| — commodity shares within the bundle
  # m     : list of length |C|, each a length-RN normalised origin pattern
  decompose_demand <- function(Yi, ctry) {
    Yc <- Yi[, fd$iso3c == ctry, drop = FALSE]
    y_full <- as.vector(rowSums(as.matrix(Yc)))   # length RN
    
    # Per-commodity totals and origin patterns
    totals <- numeric(length(commodity))
    origin <- vector("list", length(commodity))
    names(origin) <- commodity
    
    for (i in seq_along(commodity)) {
      cc <- commodity[i]
      idx <- io$comm_code == cc
      y_cc <- y_full * idx
      tot  <- sum(y_cc)
      totals[i] <- tot
      # Origin pattern: normalised (or zero-vector if no demand)
      origin[[cc]] <- if (tot > 0) y_cc / tot else y_cc
    }
    
    s     <- sum(totals)
    sigma <- if (s > 0) totals / s else rep(0, length(commodity))
    names(sigma) <- commodity
    
    list(s = s, sigma = sigma, m = origin)
  }
  
  # --- Helper: assemble y from factors ------------------------------------
  build_y <- function(s, sigma, m) {
    y <- numeric(nrow(io))
    for (i in seq_along(commodity)) {
      cc <- commodity[i]
      y <- y + s * sigma[i] * m[[cc]]
    }
    y
  }
  
  # --- Dietzenbacher-Los average decomposition ----------------------------
  factor_names <- c("e", "L", "s", "sigma", "m")
  effect_names <- c("intensity", "technique", "scale", "composition", "origin")
  
  sda_country <- function(ctry) {
    dem0 <- decompose_demand(fac0$Y, ctry)
    dem1 <- decompose_demand(fac1$Y, ctry)
    
    for (cc in commodity) {
      zero0 <- sum(dem0$m[[cc]]) == 0
      zero1 <- sum(dem1$m[[cc]]) == 0
      if (zero0 && !zero1) dem0$m[[cc]] <- dem1$m[[cc]]
      if (zero1 && !zero0) dem1$m[[cc]] <- dem0$m[[cc]]
    }
    
    if (dem0$s == 0 && dem1$s > 0) dem0$sigma <- dem1$sigma
    if (dem1$s == 0 && dem0$s > 0) dem1$sigma <- dem0$sigma
    
    fp_spec <- function(spec) {
      e     <- if (spec[1] == 1) fac1$e     else fac0$e
      L     <- if (spec[2] == 1) fac1$L     else fac0$L
      s     <- if (spec[3] == 1) dem1$s     else dem0$s
      sigma <- if (spec[4] == 1) dem1$sigma else dem0$sigma
      m     <- if (spec[5] == 1) dem1$m     else dem0$m
      y <- build_y(s, sigma, m)
      as.numeric(crossprod(e, L %*% y))
    }
    
    n_fac <- 5
    
    # Precompute F at all 2^n_fac = 32 specs once per country
    all_specs <- as.matrix(expand.grid(replicate(n_fac, c(0, 1), simplify = FALSE)))
    F_cache   <- setNames(
      apply(all_specs, 1, fp_spec),
      apply(all_specs, 1, paste, collapse = "")
    )
    get_F <- function(spec) F_cache[[paste(spec, collapse = "")]]
    
    F0 <- get_F(rep(0, n_fac))
    F1 <- get_F(rep(1, n_fac))
    
    if (F0 == 0 && F1 == 0) {
      return(data.table(country_consumer = ctry,
                        effect = c(effect_names, "total_t0", "total_t1", "delta"),
                        value  = c(rep(0, 5), 0, 0, 0)))
    }
    
    # Configurations of the other n_fac - 1 factors
    other_configs <- as.matrix(expand.grid(replicate(n_fac - 1, c(0, 1), simplify = FALSE)))
    
    # Shapley weights: depend only on |S| = number of other factors at t1
    s_card    <- rowSums(other_configs)
    shapley_w <- factorial(s_card) *
      factorial(n_fac - 1 - s_card) /
      factorial(n_fac)
    
    deltas <- numeric(n_fac)
    names(deltas) <- effect_names
    
    for (k in 1:n_fac) {
      others <- setdiff(1:n_fac, k)
      total  <- 0
      for (i in seq_len(nrow(other_configs))) {
        cfg   <- other_configs[i, ]
        spec1 <- spec0 <- numeric(n_fac)
        spec1[others] <- cfg; spec1[k] <- 1
        spec0[others] <- cfg; spec0[k] <- 0
        total <- total + shapley_w[i] * (get_F(spec1) - get_F(spec0))
      }
      deltas[k] <- total
    }
    
    data.table(
      country_consumer = ctry,
      effect           = c(effect_names, "total_t0", "total_t1", "delta"),
      value            = c(deltas, F0, F1, F1 - F0)
    )
  }
    
  # --- Loop over countries -------------------------------------------------
  results <- rbindlist(lapply(country, sda_country))
  
  # --- Annotate ------------------------------------------------------------
  indicator_label <- if (!is.null(extension)) extension else "material"
  results[, `:=`(
    year_base    = year_base,
    year_current = year_current,
    indicator    = indicator_label,
    allocation   = allocation,
    commodities  = paste(commodity, collapse = "-")
  )]
  setcolorder(results, c("country_consumer", "year_base", "year_current",
                         "indicator", "allocation", "commodities",
                         "effect", "value"))
  
  if (save) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    fname <- build_filename("FABIO_SDA",
                            country   = if (length(country) > 4) NULL else country,
                            yearBase  = year_base,
                            yearCurr  = year_current,
                            indicator = indicator_label,
                            alloc     = allocation,
                            comm      = commodity)
    fwrite(results, file.path(output_dir, fname))
    message("Wrote ", file.path(output_dir, fname))
  }
  
  results
}


sda <- fp_sda(
  year_base    = 2013,
  year_current = 2022,
  extension    = "land_harv",
  commodity    = c("c146", "c147", "c149"),
  save         = TRUE,
  regions = regions, io = io, fd = fd, ex = ex,
  X = X, Y = Y, E = E_bar
)




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
                           save            = FALSE,
                           output_dir      = "output",
                           regions, io, fd, ex,
                           X = NULL, Y = NULL, E = NULL) {
  
  stopifnot(length(years) >= 2)
  years <- sort(unique(years))
  sub <- if (losses) "losses/" else ""
  
  # --- Static data loaded once and reused ---------------------------------
  if (is.null(X)) X <- readRDS(paste0(input_path, sub, "X.rds"))
  if (is.null(Y)) Y <- readRDS(paste0(input_path, sub, "Y.rds"))
  if (!is.null(extension) && is.null(E))
    E <- readRDS(paste0(input_path, "E.rds"))
  
  load_L <- function(yr) {
    message("Loading L for ", yr, " ...")
    readRDS(paste0(input_path, sub, yr, "_L_", allocation, ".rds"))
  }
  
  # --- Rolling-L loop over consecutive year pairs -------------------------
  L_curr <- load_L(years[1])
  step_results <- vector("list", length(years) - 1)
  
  for (i in seq_len(length(years) - 1)) {
    t0 <- years[i]
    t1 <- years[i + 1]
    L_base <- L_curr           # previous t1 becomes new t0
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
      save         = FALSE,
      regions      = regions, io = io, fd = fd, ex = ex,
      X = X, Y = Y, E = E_bar,
      L_base = L_base, L_curr = L_curr
    )
  }
  
  results <- rbindlist(step_results)
  
  # --- Optional save of the combined chained table ------------------------
  if (save) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    indicator_label <- if (!is.null(extension)) extension else "material"
    fname <- build_filename(
      "FABIO_SDA_chained",
      yearBase  = min(years),
      yearCurr  = max(years),
      indicator = indicator_label,
      alloc     = allocation,
      comm      = commodity
    )
    fwrite(results, file.path(output_dir, fname))
    message("Wrote ", file.path(output_dir, fname))
  }
  
  results
}

SDA_chain <- fp_sda_chained(
  years        = 2012:2022,
  extension    = "land_harv",
  commodity    = c("c146", "c147", "c149"),
  save         = TRUE,
  regions = regions, io = io, fd = fd, ex = ex,
  X = X, Y = Y, E = E_bar
)




###########################################################
########### STRUCTURAL DECOMPOSITION ANALYSIS
########### OF CONSUMPTION-BASED FOOTPRINT
########### Smoothed over 3-yr averages.
###########################################################

fp_sda_smoothed <- function(years_base      = 2012:2014,
                            years_current   = 2020:2022,
                            country         = NULL,
                            extension       = NULL,
                            commodity,
                            allocation      = "value",
                            input_path      = "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/",
                            losses          = TRUE,
                            save            = FALSE,
                            output_dir      = "output",
                            regions, io, fd, ex,
                            X = NULL, Y = NULL, E = NULL,
                            L_method        = c("average", "midyear")) {
  
  L_method <- match.arg(L_method)
  sub      <- if (losses) "losses/" else ""
  
  # --- Static data loaded once --------------------------------------------
  if (is.null(X)) X <- readRDS(paste0(input_path, sub, "X.rds"))
  if (is.null(Y)) Y <- readRDS(paste0(input_path, sub, "Y.rds"))
  if (!is.null(extension) && is.null(E))
    E <- readRDS(paste0(input_path, "E.rds"))
  
  # --- Average X, Y, E within a window ------------------------------------
  avg_window <- function(years) {
    yrs <- as.character(years)
    list(
      X = rowMeans(X[, yrs, drop = FALSE]),
      Y = Reduce("+", Y[yrs]) / length(yrs),
      E = if (!is.null(extension)) Reduce("+", E[yrs]) / length(yrs) else NULL
    )
  }
  
  # --- Build the window's L (average yearly L's, or pick midyear) ---------
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
  
  message("Building averaged endpoint t0 from: ",
          paste(years_base, collapse = ", "))
  ep0    <- avg_window(years_base)
  L_base <- build_L_window(years_base)
  
  message("Building averaged endpoint t1 from: ",
          paste(years_current, collapse = ", "))
  ep1    <- avg_window(years_current)
  L_curr <- build_L_window(years_current)
  
  # --- Wrap averaged data in the structure fp_sda expects -----------------
  label_base <- paste0(min(years_base),    "-", max(years_base))
  label_curr <- paste0(min(years_current), "-", max(years_current))
  
  X_synth <- cbind(ep0$X, ep1$X)
  colnames(X_synth) <- c(label_base, label_curr)
  
  Y_synth <- setNames(list(ep0$Y, ep1$Y), c(label_base, label_curr))
  
  E_synth <- if (!is.null(extension))
    setNames(list(ep0$E, ep1$E), c(label_base, label_curr))
  else NULL
  
  # --- Call the existing engine -------------------------------------------
  results <- fp_sda(
    country      = country,
    year_base    = label_base,
    year_current = label_curr,
    extension    = extension,
    commodity    = commodity,
    allocation   = allocation,
    input_path   = input_path,
    losses       = losses,
    save         = FALSE,
    regions      = regions, io = io, fd = fd, ex = ex,
    X = X_synth, Y = Y_synth, E = E_synth,
    L_base = L_base, L_curr = L_curr
  )
  
  # --- Save -----------------------------------------------------------------
  if (save) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    indicator_label <- if (!is.null(extension)) extension else "material"
    fname <- paste0(
      "FABIO_SDA_smoothed_",
      label_base, "_vs_", label_curr, "_",
      indicator_label, "_", allocation, "_",
      paste(commodity, collapse = "-"), ".csv"
    )
    fwrite(results, file.path(output_dir, fname))
    message("Wrote ", file.path(output_dir, fname))
  }
  
  results
}

SDA_smoothed <- fp_sda_smoothed(
  years_base    = 2012:2014,
  years_current = 2020:2022,
  extension     = "land_harv",
  commodity     = c("c146", "c147", "c149"),
  save          = TRUE,
  regions = regions, io = io, fd = fd, ex = ex,
  X = X, Y = Y, E = E_bar
)

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

fwrite(dt_Y_long, file.path("output", "Y_summary_c146_c147_c149.csv"))


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




###########################################################
########### ENVIRONMENTAL INTENSITY (per unit of output)
########### OF SELECTED COMMODITIES PRODUCED IN COUNTRIES
########### BY FEEDSTOCK (DIRECT REQUIREMENT)
########### BY CONTINENT WHERE IMPACTS OCCUR
###########################################################

intensity_feedstock <- function(country         = NULL,
                                year,
                                extension,
                                commodity,
                                allocation      = "value",
                                input_path      = "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/",
                                losses          = TRUE,
                                by_origin       = TRUE,
                                drop_zero       = TRUE,
                                save            = FALSE,
                                output_dir      = "output",
                                regions, io, ex,
                                X = NULL, Z = NULL, E = NULL, L = NULL,
                                return_full     = FALSE) {
  
  sub <- if (losses) "losses/" else ""
  
  # --- Load only what wasn't passed in ---------------------------------------
  if (is.null(X)) X <- readRDS(paste0(input_path, sub, "X.rds"))
  if (is.null(Z)) Z <- readRDS(paste0(input_path, sub, "Z_", allocation, ".rds"))
  if (!is.null(extension) && is.null(E))
    E <- readRDS(paste0(input_path, "E.rds"))
  if (is.null(L)) L <- readRDS(paste0(input_path, sub, year, "_L_", allocation, ".rds"))
  
  Xi <- X[, as.character(year)]
  Zi <- Z[[as.character(year)]]
  Ei <- if (!is.null(extension)) E[[as.character(year)]] else NULL
  
  # --- Direct extension intensity (per unit output) -------------------------
  if (!is.null(extension)) {
    ext <- as.numeric(Ei[ex$Stressor == extension, ]) / as.vector(Xi)
    ext[!is.finite(ext)] <- 0
  } else {
    ext <- rep(1, length(Xi))
  }
  MP <- ext * L                        # MP[origin, sector] = total ext per unit FD of sector
  
  # --- Validate commodity input ---------------------------------------------
  unknown_c <- setdiff(commodity, io$comm_code)
  if (length(unknown_c) > 0) {
    warning("Unknown comm_codes dropped: ", paste(unknown_c, collapse = ", "))
    commodity <- intersect(commodity, io$comm_code)
  }
  if (length(commodity) == 0) stop("No valid comm_codes supplied.")
  
  # --- Resolve producer country list ----------------------------------------
  if (is.null(country)) country <- unique(io$iso3c)
  unknown <- setdiff(country, io$iso3c)
  if (length(unknown) > 0) {
    warning("Unknown countries dropped: ", paste(unknown, collapse = ", "))
    country <- setdiff(country, unknown)
  }
  if (length(country) == 0) stop("No valid countries supplied.")
  
  io_label <- paste0(io$iso3c, "_", io$item)
  
  # --- Helper: per-producer, per-commodity intensity ------------------------
  compute_for <- function(ctry, cc) {
    target_idx <- which(io$iso3c == ctry & io$comm_code == cc)
    if (length(target_idx) == 0) return(NULL)
    
    x_target <- Xi[target_idx]
    if (x_target == 0) return(NULL)               # no output -> no intensity defined
    
    # Technical coefficient column: a[, target] = Z[, target] / X[target]
    a_vec   <- as.vector(Zi[, target_idx]) / x_target
    nz_feed <- which(a_vec != 0)
    if (length(nz_feed) == 0) return(NULL)
    
    # Intensity[origin, feedstock] = ext[origin] * L[origin, feedstock] * a[feedstock, target]
    MP_sub  <- MP[, nz_feed, drop = FALSE]
    Int_sub <- t(t(MP_sub) * a_vec[nz_feed])
    Int_sub <- as(Int_sub, "TsparseMatrix")
    
    if (length(Int_sub@x) == 0) return(NULL)
    
    feed_lbl <- io_label[nz_feed]
    dt <- data.table(
      country_producer = ctry,
      commodity        = cc,
      origin           = io_label[Int_sub@i + 1],
      feedstock_full   = feed_lbl[Int_sub@j + 1],
      value            = Int_sub@x
    )
    
    if (drop_zero) dt <- dt[value != 0]
    dt
  }
  
  # --- Loop over (country_producer, commodity) ------------------------------
  grid <- CJ(ctry = country, cc = commodity, sorted = FALSE)
  per_cell <- rbindlist(
    Map(compute_for, grid$ctry, grid$cc),
    use.names = TRUE, fill = TRUE
  )
  
  if (nrow(per_cell) == 0) {
    warning("Empty intensity for year ", year, " / countries: ",
            paste(country, collapse = ","), " / commodities: ",
            paste(commodity, collapse = ","))
    return(per_cell)
  }
  
  # --- Annotate --------------------------------------------------------------
  indicator_label <- if (!is.null(extension)) extension else "material"
  
  per_cell[, `:=`(
    year              = year,
    indicator         = indicator_label,
    allocation        = allocation,
    country_origin    = substr(origin, 1, 3),
    item_origin       = substr(origin, 5, 100),
    country_feedstock = substr(feedstock_full, 1, 3),
    feedstock         = substr(feedstock_full, 5, 100)
  )]
  per_cell[, continent_origin :=
             regions$continent[match(country_origin, regions$iso3c)]]
  
  # --- Aggregate to country_producer x year x commodity x feedstock --------
  group_cols <- c("country_producer", "year", "indicator", "allocation",
                  "commodity", "feedstock")
  if (by_origin) group_cols <- c(group_cols, "continent_origin")
  
  agg_results <- per_cell[, .(value = sum(value)), by = group_cols]
  setcolorder(agg_results, group_cols)
  
  if (drop_zero) agg_results <- agg_results[value != 0]
  
  if (save) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    fname <- build_filename("FABIO_intensity",
                            country   = if (length(country) > 4) NULL else country,
                            year      = year,
                            indicator = indicator_label,
                            alloc     = allocation,
                            comm      = commodity,
                            byOrigin  = by_origin)
    fwrite(agg_results, file.path(output_dir, fname))
    message("Wrote ", file.path(output_dir, fname))
  }
  
  if (return_full) list(results = per_cell, agg = agg_results) else agg_results
}

# --- Run intensities for biogasoline / biodiesel / renewable diesel ---------
years_to_run <- 2012:2022

intensity_land_bcp <- rbindlist(
  lapply(years_to_run, function(yr) {
    intensity_feedstock(
      year       = yr,
      extension  = "land_harv",
      commodity  = c("c146", "c147", "c149"),
      allocation = allocation,
      input_path = input_path,
      regions    = regions, io = io, ex = ex,
      X = X, Z = Z, E = E_bar,
      save       = FALSE
    )
  }),
  use.names = TRUE, fill = TRUE
)

fname <- build_filename(
  "FABIO_intensity",
  years     = paste0(min(years_to_run), "-", max(years_to_run)),
  indicator = "land_harv",
  alloc     = allocation,
  comm      = c("c146", "c147", "c149"),
  byOrigin  = TRUE
)
fwrite(intensity_land_bcp, file.path("output", fname))
message("Wrote ", file.path("output", fname))