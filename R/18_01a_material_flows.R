# 18a - Material-flow tables from FABIO-BCP MRIO  (MASS allocation)
# ------------------------------------------------------------------------------
# Physical product flows only: feedstock tonnes and biofuel output.
# NO environmental extensions here (extension = NULL everywhere), so this script
# uses the MASS-allocated inter-industry flows (Z_mass) and, wherever a Leontief
# inverse is needed, <year>_L_mass.
#
# E / E_bar are deliberately NOT loaded: material-flow tables carry no stressor.
#
# Companion script: 18b_extension_footprints.R  (VALUE allocation, stressors).
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
message(sprintf(">>> [18a material] model_version = '%s'  (MRIO: %s | out: %s)",
                model_version, MRIO_PATH, OUT_DIR))

# Read labels ------------------------------------------------------------------
input_path <- MRIO_PATH
regions <- fread(file="inst/regions_full.csv") %>% filter(current==TRUE)
io <- fread(paste0(input_path,"io_labels.csv"))
items <- fread(file="inst/items_full_bcp.csv") %>% filter(comm_code %in% unique(io$comm_code))
fd <- fread(file=paste0(input_path,"losses/fd_labels.csv"))
ex <- fread(file=paste0(base_path,"ex_labels.csv"))         # shared across versions (only for the unused `ex` arg)

# Technical conversion factors (feedstock use -> biofuel supply) --------------
# Used by bf_supply_chain_flows() to convert stage-1 rows from feedstock tonnes
# to biofuel-output-equivalent BEFORE saving, so the producer node balances.
tcf <- readRDS("intermediate_data/tcf_table_final.rds")
setDT(tcf)
tcf[, item := trimws(item)]        # strip stray whitespace that breaks the join to io$item
tcf[, biofuel_code := fcase(
  grepl("Biogasoline",      proc), "c146",
  grepl("Biodiesel",        proc), "c147",
  grepl("Renewable diesel", proc), "c149",
  default = NA_character_)]
tcf <- tcf[!item %in% c("Oilcrops Oil, Other", "Total")]

# Create output directory ------------------------------------------------------
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Load static data once (MASS allocation) --------------------------------------
# X, Y, Z are year-independent containers, read once. L is year-specific and is
# loaded inside each function call (as <year>_L_mass, driven by `allocation`).
allocation <- "mass"

X <- readRDS(paste0(input_path, "losses/X.rds"))
Y <- readRDS(paste0(input_path, "losses/Y.rds"))
Z <- readRDS(paste0(input_path, "losses/Z_", allocation, ".rds"))   # Z_mass

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
############################## MATERIAL-FLOW FUNCTIONS #######################
######################################################################################################################

###########################################################
########### CONSUMPTION-BASED FLOW
########### OF SELECTED COUNTRIES (default: all)
########### OF SELECTED COMMODITIES
########### BY CONTINENT WHERE FLOWS OCCUR
########### FULL UPSTREAM, INCL. INDIRECT REQUIREMENTS
###########################################################

fp_indirect <- function(country       = NULL,
                        year,
                        extension     = NULL,
                        commodity,
                        allocation    = "value",
                        input_path    = MRIO_PATH,
                        losses        = TRUE,
                        by_commodity  = TRUE,
                        by_country    = TRUE,
                        top_n         = NULL,
                        drop_zero     = TRUE,
                        save          = FALSE,
                        output_dir    = OUT_DIR,
                        regions, io, fd, ex,
                        X = NULL, Y = NULL, E = NULL, L = NULL,
                        return_full   = FALSE) {
  
  sub <- if (losses) "losses/" else ""
  
  # --- Load only what wasn't passed in ---------------------------------------
  if (is.null(X)) X <- readRDS(paste0(input_path, sub, "X.rds"))
  if (is.null(Y)) Y <- readRDS(paste0(input_path, sub, "Y.rds"))
  if (!is.null(extension) && is.null(E))
    E <- readRDS(paste0(base_path, "E.rds"))
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
  group_cols <- c("year", "indicator", "allocation",
                  "item_origin", "continent_origin")
  if (by_country)   group_cols <- c("country_consumer", group_cols)
  if (by_commodity) group_cols <- c(group_cols, "commodity")
  
  agg_results <- per_cell[, .(value = sum(value)), by = group_cols]
  setcolorder(agg_results, group_cols)
  
  if (drop_zero) agg_results <- agg_results[value != 0]
  
  # --- top-N filter on aggregated origin sectors ----------------------------
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
########### CONSUMPTION-BASED FLOW
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


######################################################################################################################
############################## DIRECT BIOFUEL SUPPLY-CHAIN FLOWS (3-TIER, CONTINENT) ##################################
######################################################################################################################
# Builds the two DIRECT linkages of the biofuel supply chain while keeping the
# biofuel PRODUCER as an explicit middle node.
#   Stage 1  feedstock_to_producer : feedstock m, origin i -> producer p  (Z, feedstock tonnes)
#   Stage 2  producer_to_consumer  : biofuel cc, producer p -> consumer k (Y or L%*%Y, biofuel output)
# Pass `tcf` to convert stage-1 to biofuel-output-equivalent so the producer node balances.

bf_supply_chain_flows <- function(years,
                                  biofuel,                         # cc codes, e.g. c("c146","c147","c149")
                                  feedstock_codes = NULL,          # restrict stage-1 inputs; NULL = all tier-1 inputs
                                  stage2_basis    = "final_demand",# "final_demand" (Y, direct) | "consumption" (L-allocated)
                                  level           = "continent",   # "continent" | "country" (no aggregation)
                                  clamp_neg       = FALSE,         # set negative flows (e.g. stock changes in Y) to 0
                                  drop_zero       = TRUE,
                                  input_path      = MRIO_PATH,
                                  losses          = TRUE,
                                  allocation      = "value",
                                  save            = FALSE,
                                  output_dir      = OUT_DIR,
                                  regions, io, fd,
                                  X = NULL, Y = NULL, Z = NULL, L = NULL,
                                  # --- TCF conversion of stage-1 (feedstock use -> biofuel supply) ---------------
                                  tcf            = NULL,                    # data.table of conversion factors
                                  tcf_stage      = "feedstock_to_producer", # which `stage` carries feedstock-use rows
                                  tcf_feed_col   = "item",                  # column in `tcf` holding the feedstock id
                                  tcf_bf_col     = "biofuel_code",          # column in `tcf` holding the biofuel id
                                  tcf_val_col    = "output_qty",            # column in `tcf` holding the conversion factor
                                  feed_join      = "feedstock",             # column in the flows matched to tcf_feed_col
                                  bf_join        = "biofuel",               # column in the flows matched to tcf_bf_col
                                  tcf_invert     = FALSE,                   # TRUE if tcf = feedstock-per-biofuel (input coeff)
                                  tcf_on_missing = "warn") {                # "warn" | "drop" | "error"
  
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
  
  # ---- TCF: feedstock USE -> biofuel SUPPLY on stage-1 rows ------------------------------
  if (!is.null(tcf)) {
    tcf <- as.data.table(tcf)
    need <- c(tcf_feed_col, tcf_bf_col, tcf_val_col)
    if (!all(need %in% names(tcf)))
      stop("`tcf` is missing column(s): ", paste(setdiff(need, names(tcf)), collapse = ", "),
           ". Set tcf_feed_col / tcf_bf_col / tcf_val_col to match names(tcf).")
    if (!all(c(feed_join, bf_join) %in% names(dt)))
      stop("flows have no `", feed_join, "` / `", bf_join,
           "` column \u2014 stage-1 rows must carry feedstock & biofuel ids to attach a tcf.")
    
    tcf_lu <- unique(tcf[, .(feed = trimws(as.character(get(tcf_feed_col))),
                             bf   = as.character(get(tcf_bf_col)),
                             cf   = as.numeric(get(tcf_val_col)))])
    tcf_lu <- tcf_lu[!is.na(cf) & !is.na(feed) & !is.na(bf)]
    dup <- tcf_lu[, .N, by = .(feed, bf)][N > 1L]
    if (nrow(dup))
      stop(nrow(dup), " (feedstock, biofuel) pair(s) map to more than one tcf value \u2014 ",
           "collapse `tcf` to one factor per pair before passing it in.")
    if (isTRUE(tcf_invert)) tcf_lu[, cf := 1 / cf]
    
    s1_idx <- which(dt$stage == tcf_stage)
    if (length(s1_idx)) {
      key    <- data.table(feed = trimws(as.character(dt[[feed_join]][s1_idx])),
                           bf   = as.character(dt[[bf_join]][s1_idx]))
      cf_vec <- tcf_lu[key, on = c("feed", "bf"), cf]   # one cf per stage-1 row, key order; NA = no match
      
      na_i <- is.na(cf_vec)
      if (any(na_i)) {
        bad <- unique(key[na_i])
        msg <- sprintf("TCF: %d stage-1 row(s) across %d feedstock\u00d7biofuel pair(s) have no match",
                       sum(na_i), nrow(bad))
        if (tcf_on_missing == "error") stop(msg, ".")
        if (tcf_on_missing == "drop")  warning(msg, " \u2014 dropped from the flows.")
        if (tcf_on_missing == "warn")  warning(msg,
                                               " \u2014 left in feedstock units; the producer node will not balance for these.")
      }
      
      keep <- !na_i
      if (any(keep)) {
        dt[s1_idx[keep], value := value * cf_vec[keep]]
        if ("unit" %in% names(dt)) dt[s1_idx[keep], unit := "biofuel output (via tcf)"]
      }
      if (any(na_i) && tcf_on_missing == "drop") dt <- dt[-s1_idx[na_i]]
    }
  }
  
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


######################################################################################################################
############################## FINAL-DEMAND SOURCING SHARES (DIRECT FROM Y) ###########################################
######################################################################################################################
# Y-based, allocation-invariant (no Z / L), but a physical descriptive statistic
# so it lives with the material tables.

y_sourcing_shares <- function(years,
                              commodity,
                              by_commodity = TRUE,
                              include_self = TRUE,
                              consumer     = NULL,        # NULL = all consumers
                              wide         = FALSE,       # one row per (consumer, year, commodity)
                              clamp_neg    = FALSE,       # set negative FD entries (e.g. stock changes) to 0
                              save         = FALSE,
                              output_dir   = OUT_DIR,
                              input_path   = MRIO_PATH,
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


######################################################################################################################
############################## DESCRIPTIVE STATISTICS (Y / Z DUMPS) ###################################################
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

extract_Z_long <- function(Z, commodities = c("c146", "c147", "c149"), io) {
  
  col_pattern <- paste0("_(", paste(commodities, collapse = "|"), ")$")
  
  # canonical labels in MRIO row/col order: <iso3c>_<comm_code> (e.g. "AUT_c147")
  io_lab <- paste0(io$iso3c, "_", io$comm_code)
  
  yrs <- names(Z)
  if (is.null(yrs)) stop("Z has no names(); expected a per-year named list ",
                         "(set names(Z) <- as.character(2012:2022)).")
  
  out <- rbindlist(lapply(yrs, function(yr) {
    M <- Z[[yr]]
    
    # mass Z may be saved without dimnames -> reattach from io (order-aligned)
    if (is.null(rownames(M)) || is.null(colnames(M))) {
      if (nrow(M) != length(io_lab) || ncol(M) != length(io_lab))
        stop("Z[['", yr, "']] is ", nrow(M), "x", ncol(M),
             " but io has ", length(io_lab), " rows - can't align labels.")
      dimnames(M) <- list(io_lab, io_lab)
    }
    
    col_idx <- grep(col_pattern, colnames(M))
    if (length(col_idx) == 0) return(NULL)
    
    M_sub <- M[, col_idx, drop = FALSE]
    row_keep <- which(rowSums(abs(M_sub)) > 0)
    if (length(row_keep) == 0) return(NULL)
    M_sub <- M_sub[row_keep, , drop = FALSE]
    
    trip <- as(M_sub, "TsparseMatrix")
    dt <- data.table(
      origin_id = rownames(M_sub)[trip@i + 1L],
      target_id = colnames(M_sub)[trip@j + 1L],
      value     = trip@x
    )
    dt[, `:=`(
      origin_country = substr(origin_id, 1, 3),
      origin_comm    = sub(".*_", "", origin_id),
      target_country = substr(target_id, 1, 3),
      target_comm    = sub(".*_", "", target_id),
      year           = as.integer(yr)
    )]
    dt[, .(year, origin_country, origin_comm, target_country, target_comm, value)]
  }))
  
  if (nrow(out) == 0) {
    warning("extract_Z_long: no columns matched ", paste(commodities, collapse = ","),
            " in any year - returning empty.")
    return(out)
  }
  
  out <- merge(
    out,
    items[, .(origin_comm = comm_code, origin_comm_name = item)],
    by = "origin_comm", all.x = TRUE, sort = FALSE
  )
  setcolorder(out, c("year", "origin_country", "origin_comm", "origin_comm_name",
                     "target_country", "target_comm", "value"))
  out[]
}

######################################################################################################################
############################## RUN: MATERIAL FLOWS (mass allocation) ##################################################
######################################################################################################################

## Full upstream requirements (total-requirement material flow) --------------
for (yr in 2012:2022) {
  message("--- ", yr, " ---")
  fp_indirect(
    year         = yr,
    extension    = NULL,           # material flows
    allocation   = "mass",         # -> loads <year>_L_mass
    commodity    = c("c146", "c147", "c149"),
    by_commodity = TRUE,
    by_country   = TRUE,
    drop_zero    = TRUE,
    save         = TRUE,
    output_dir   = OUT_DIR,
    regions = regions, io = io, fd = fd, ex = ex,
    X = X, Y = Y                    # E not needed: no extension
  )
}

## Direct (tier-1) feedstock trade breakdown, bilateral ----------------------
for (yr in 2012:2022) {
  fp_trade_breakdown_feedstock(
    year         = yr,
    extension    = NULL,           # material mode -> feedstock tonnes
    commodity    = c("c146", "c147", "c149"),
    allocation   = "mass",
    by_commodity = TRUE,
    by_feedstock = TRUE,
    indirect     = FALSE,
    bilateral    = TRUE,
    drop_zero    = TRUE,
    save         = TRUE,
    output_dir   = OUT_DIR,
    regions = regions, io = io, fd = fd, ex = ex,
    X = X, Y = Y, Z = Z,           # Z_mass
    L = NULL
  )
}

## Direct 3-tier biofuel supply-chain flows (feedstock -> producer -> consumer)
bf_supply_chain_flows(
  years        = 2012:2022,
  biofuel      = c("c146", "c147", "c149"),
  stage2_basis = "final_demand",
  level        = "continent",
  allocation   = "mass",
  regions = regions, io = io, fd = fd,
  X = X, Y = Y, Z = Z,             # Z_mass
  tcf          = tcf,              # convert stage-1 feedstock use -> biofuel supply before saving
  tcf_val_col  = "output_qty",
  save = TRUE
)

## Final-demand sourcing shares (Y-based, allocation-invariant) --------------
y_sourcing_shares(
  years     = 2012:2022,
  commodity = bf_set,
  io = io, fd = fd, Y = Y,
  save = TRUE
)

## Descriptive Y / Z dumps ---------------------------------------------------
dt_Y_long <- extract_Y_long(Y)
dt_Y_BP   <- extract_Y_long(Y, commodities = c("c060", "c146", "c148", c(paste0("c",150:170))),
                            uses = c("other_industrial", "unknown"))
fwrite(dt_Y_long, file.path(OUT_DIR, "Y_summary_c146_c147_c149.csv"))
fwrite(dt_Y_BP,   file.path(OUT_DIR, "Y_summary_BP.csv"))

dt_Z_long <- extract_Z_long(Z, io = io)     # Z_mass
fwrite(dt_Z_long, file.path(OUT_DIR, "Z_summary_c146_c147_c149.csv"))