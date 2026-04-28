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
########### OF SELECTED COUNTRY
########### OF SELECTED COMMODITIES
########### BY CONTINENT WHERE IMPACTS OCCUR
########### BY FEEDSTOCK (DIRECT REQUIREMENT) RESPONSIBLE FOR THE IMPACT
###########################################################

fp_feedstock <- function(country,                   # 
                         year,                      # single year
                         extension,                 # extension item
                         commodity,                 # scalar or vector of comm_codes
                         allocation    = "value",
                         input_path    = "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/",
                         losses        = TRUE,
                         by_commodity  = TRUE,      # TRUE: keep per-commodity rows ; FALSE: collapse
                         save = FALSE,
                         output_dir = "output",
                         regions, io, fd, ex,
                         X = NULL, Y = NULL, Z = NULL, E = NULL, L = NULL,
                         return_full   = FALSE) {
  
  sub <- if (losses) "losses/" else ""
  
  # --- Load only what wasn't passed in ---------------------------------------
  if (is.null(X)) X <- readRDS(paste0(input_path, sub, "X.rds"))
  if (is.null(Y)) Y <- readRDS(paste0(input_path, sub, "Y.rds"))
  if (is.null(Z)) Z <- readRDS(paste0(input_path, sub, "Z_", allocation, ".rds"))
  if (is.null(E)) E <- readRDS(paste0(input_path,      "E.rds"))
  if (is.null(L)) L <- readRDS(paste0(input_path, sub, year, "_L_", allocation, ".rds"))
  
  Xi <- X[, as.character(year)]
  Yi <- Y[[as.character(year)]]
  Zi <- Z[[as.character(year)]]
  Ei <- E[[as.character(year)]]
  
  # --- Extension intensity ---------------------------------------------------
  ext <- as.numeric(Ei[ex$Stressor == extension, ]) / as.vector(Xi)
  ext[!is.finite(ext)] <- 0
  MP <- ext * L
  
  # --- Country final demand (summed across all FD components) ---------------
  if (country == "EU27") {
    Y_country <- Yi[, fd$continent == "EU"]
  } else {
    Y_country <- Yi[, fd$iso3c == country]
  }
  y_full <- as.vector(rowSums(as.matrix(Y_country)))
  
  # --- Validate commodity input ---------------------------------------------
  unknown <- setdiff(commodity, io$comm_code)
  if (length(unknown) > 0) {
    warning("Unknown comm_codes dropped: ", paste(unknown, collapse = ", "))
    commodity <- intersect(commodity, io$comm_code)
  }
  if (length(commodity) == 0) stop("No valid comm_codes supplied.")
  
  io_label <- paste0(io$iso3c, "_", io$item)
  
  # --- Per-commodity computation --------------------------------------------
  per_comm <- rbindlist(lapply(commodity, function(cc) {
    
    comm_idx <- io$comm_code == cc
    
    # demand vector restricted to this commodity (across all FD components)
    y_comm <- y_full
    y_comm[!comm_idx] <- 0
    
    # output of `cc` triggered by C's total consumption
    x_triggered <- as.vector(L %*% y_comm)
    x_comm <- x_triggered
    x_comm[!comm_idx] <- 0
    
    # first-degree feedstock vector
    denom <- Xi[comm_idx]
    denom[denom == 0] <- 1
    f <- as.vector(Zi[, comm_idx] %*% (x_comm[comm_idx] / denom))
    
    if (sum(f) == 0) return(NULL)
    
    # footprint matrix for this commodity
    FP <- t(t(MP) * f)
    colnames(FP) <- rownames(FP) <- io_label
    FP <- as(FP, "TsparseMatrix")
    
    data.table(
      commodity = cc,
      origin    = rownames(FP)[FP@i + 1],
      target    = colnames(FP)[FP@j + 1],
      value     = FP@x
    )
  }), use.names = TRUE, fill = TRUE)
  
  if (nrow(per_comm) == 0) {
    warning("Empty footprint for ", country, " / ", year, " / ",
            paste(commodity, collapse = ","))
    return(per_comm)
  }
  
  # --- Annotate --------------------------------------------------------------
  per_comm[, `:=`(
    country_consumer = country,
    year             = year,
    indicator        = extension,
    allocation       = allocation,
    country_origin   = substr(origin, 1, 3),
    item_origin      = substr(origin, 5, 100),
    country_target   = substr(target, 1, 3),
    item_target      = substr(target, 5, 100)
  )]
  per_comm[, continent_origin :=
             regions$continent[match(country_origin, regions$iso3c)]]
  
  # --- Aggregate -------------------------------------------------------------
  group_cols <- c("country_consumer", "year", "indicator", "allocation",
                  "feedstock", "continent_origin")
  if (by_commodity) group_cols <- c(group_cols, "commodity")
  
  agg_results <- per_comm[, .(value = sum(value)),
                          by = c(setdiff(group_cols, "feedstock"),
                                 "item_target")]
  setnames(agg_results, "item_target", "feedstock")
  setcolorder(agg_results, group_cols)
  
  if (save) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    fname <- build_filename("FABIO_feedstock",
                            country = country,
                            year    = year,
                            indicator = extension,
                            alloc   = allocation,
                            comm    = commodity,
                            byComm  = by_commodity)
    fwrite(agg_results, file.path(output_dir, fname))
    message("Wrote ", file.path(output_dir, fname))
  }
  
  if (return_full) list(results = per_comm, agg = agg_results) else agg_results
  
}



###########################################################
########### CONSUMPTION-BASED IMPACT
########### OF SELECTED COMMODITIES
########### TRADE BREAKDOWN OR BILATERAL FLOWS
########### FOR ALL REPORTING COUNTRIES
###########################################################

fp_trade_breakdown <- function(year,
                               extension,
                               commodity    = NULL,        # NULL = all commodities
                               allocation   = "value",
                               input_path   = "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/",
                               losses       = TRUE,
                               by_commodity = FALSE,
                               bilateral    = FALSE,       # full origin×consumer detail
                               drop_zero    = TRUE,        # used in bilateral mode
                               save = FALSE,
                               output_dir = "output",
                               regions, io, fd, ex,
                               X = NULL, Y = NULL, E = NULL, L = NULL,
                               return_full  = FALSE) {
  
  sub <- if (losses) "losses/" else ""
  
  # --- Load only what wasn't passed in ---------------------------------------
  if (is.null(X)) X <- readRDS(paste0(input_path, sub, "X.rds"))
  if (is.null(Y)) Y <- readRDS(paste0(input_path, sub, "Y.rds"))
  if (is.null(E)) E <- readRDS(paste0(input_path,      "E.rds"))
  if (is.null(L)) L <- readRDS(paste0(input_path, sub, year, "_L_", allocation, ".rds"))
  
  Xi <- X[, as.character(year)]
  Yi <- Y[[as.character(year)]]
  Ei <- E[[as.character(year)]]
  
  # --- Extension intensity ---------------------------------------------------
  ext <- as.numeric(Ei[ex$Stressor == extension, ]) / as.vector(Xi)
  ext[!is.finite(ext)] <- 0
  
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
    impact      <- Diagonal(x = ext) %*% x_triggered
    
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
    out[, flow_type := fifelse(country_origin == country_consumer,
                               "self", "trade")]
    out
  }
  
  # --- Compute and assemble -------------------------------------------------
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
    
    long[, `:=`(year = year, indicator = extension, allocation = allocation)]
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
    
    wide[, `:=`(year = year, indicator = extension, allocation = allocation)]
    
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
                            indicator = extension,
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
                                         extension,
                                         commodity     = NULL,    # NULL = all commodities
                                         allocation    = "value",
                                         input_path    = "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/",
                                         losses        = TRUE,
                                         by_commodity  = FALSE,
                                         by_feedstock  = TRUE,
                                         bilateral     = FALSE,
                                         drop_zero     = TRUE,
                                         save = FALSE,
                                         output_dir = "output",
                                         regions, io, fd, ex,
                                         X = NULL, Y = NULL, Z = NULL,
                                         E = NULL, L = NULL,
                                         return_full   = FALSE) {
  
  sub <- if (losses) "losses/" else ""
  
  # --- Load only what wasn't passed in ---------------------------------------
  if (is.null(X)) X <- readRDS(paste0(input_path, sub, "X.rds"))
  if (is.null(Y)) Y <- readRDS(paste0(input_path, sub, "Y.rds"))
  if (is.null(Z)) Z <- readRDS(paste0(input_path, sub, "Z_", allocation, ".rds"))
  if (is.null(E)) E <- readRDS(paste0(input_path,      "E.rds"))
  if (is.null(L)) L <- readRDS(paste0(input_path, sub, year, "_L_", allocation, ".rds"))
  
  Xi <- X[, as.character(year)]
  Yi <- Y[[as.character(year)]]
  Zi <- Z[[as.character(year)]]
  Ei <- E[[as.character(year)]]
  
  # --- Extension intensity ---------------------------------------------------
  ext <- as.numeric(Ei[ex$Stressor == extension, ]) / as.vector(Xi)
  ext[!is.finite(ext)] <- 0
  
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
  
  # --- Pre-aggregate origin: G[r, j] = sum_{i in country r} ext[i] * L[i, j] -
  MP <- Diagonal(x = ext) %*% L            # RN × RN
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
    
    # Tier-1 input vector (per consumer): F_cc[j, k] = direct input j into cc
    # industries to produce the cc that consumer k consumes
    denom <- Xi[cc_idx]
    denom[denom == 0] <- 1
    X_comm_norm <- sweep(X_comm_cc, 1, denom, "/")
    F_cc <- Zi[, cc_idx, drop = FALSE] %*% X_comm_norm                # RN × R
    
    if (by_feedstock) {
      active <- which(Matrix::rowSums(F_cc) != 0)
      if (length(active) == 0) return(NULL)
      active_comms <- unique(io$comm_code[active])
      
      per_m <- rbindlist(lapply(active_comms, function(m) {
        rows_m   <- which(io$comm_code == m)
        G_m      <- as.matrix(G[, rows_m, drop = FALSE])               # R × R
        F_m      <- as.matrix(F_cc[rows_m, , drop = FALSE])            # R × R
        impact_m <- G_m %*% F_m                                        # R × R
        
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
      impact <- as.matrix(G %*% F_cc)                                  # R × R
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
  
  long[, `:=`(year = year, indicator = extension, allocation = allocation)]
  
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
    
    agg_dt <- merge(per_origin,   per_consumer, by = c("country", group_keys), all = TRUE)
    agg_dt <- merge(agg_dt,       self_dt,      by = c("country", group_keys), all = TRUE)
    
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
                            indicator = extension,
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

# # Extract CHN consumption's impact, by commodity, by continent of origin
# out_feedstock <- fp_feedstock(
#   country      = "CHN", year = 2022,
#   extension    = "land_harv",
#   commodity    = c("c146", "c147", "c149"),
#   by_commodity = TRUE,
#   return_full  = FALSE,
#   regions = regions, io = io, fd = fd, ex = ex,
#   X = X, Y = Y, Z = Z, E = E
# ) 
# 
# 
# # Extract all countries' exported, imported, and self-consumed impacts by commodity. Here, detailed bilateral impact flows 
# out_trade <- fp_trade_breakdown(
#   year         = 2022,
#   extension    = "land_harv",
#   commodity    = c("c146", "c147", "c149"),
#   bilateral    = TRUE,
#   by_commodity = TRUE,
#   regions = regions, io = io, fd = fd, ex = ex,
#   X = X, Y = Y, E = E
# )
# 
# 
# # Extract all countries' exported, imported, and self-consumed impacts by commodity AND by feedstock of origin. Here, detailed bilateral impact flows 
# out_trade_feedstock <- fp_trade_breakdown_feedstock(
#   year         = 2022, 
#   extension    = "land_harv",
#   commodity    = c("c146", "c147", "c149"),
#   by_commodity = TRUE,
#   by_feedstock = TRUE,
#   bilateral    = TRUE,
#   save         = TRUE, # argument to save the dataset created
#   regions = regions, io = io, fd = fd, ex = ex,
#   X = X, Y = Y, Z = Z, E = E
# )


# Loop over years 2012-2022, saving each year's bilateral trade-feedstock breakdown
for (yr in 2012:2022) {
  message("--- ", yr, " ---")
  fp_trade_breakdown_feedstock(
    year         = yr, 
    extension    = "land_harv",
    commodity    = c("c146", "c147", "c149"),
    by_commodity = TRUE,
    by_feedstock = TRUE,
    bilateral    = TRUE,
    save         = TRUE,
    regions = regions, io = io, fd = fd, ex = ex,
    X = X, Y = Y, Z = Z, E = E
  )
}