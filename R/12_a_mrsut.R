###########################################################
########### LOADING PACKAGES #########
###########################################################

library("data.table")
library("Matrix")
library("parallel")
library("future.apply")


###########################################################
########### LOADING DATA #########
###########################################################

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

source("R/01_tidy_functions.R")
source("R/00_system_variables.R")
source("R/00_run_config.R")                 # RUN_MODE / tag() / mode_dir()
output_dir_mode <- mode_dir(output_dir_bcp) # rescaled -> base dir; bypass -> base/bypass/

regions <- fread("inst/regions_full.csv")[current==TRUE]
items <- fread("inst/items_full_bcp.csv")

btd <- readRDS(tag("data/btd_final_merged.rds"))
sup <- readRDS(tag("data/sup_final_merged.rds"))
use <- readRDS(tag("data/use_final_merged.rds"))
use_fd <- readRDS(tag("data/use_fd_final_merged.rds"))

areas <- regions$code
commodities <- sort(unique(c(use$comm_code, sup$comm_code,
                             btd$comm_code, use_fd$comm_code)))

processes <- sort(unique(c(use$proc_code, sup$proc_code)))
fd_vars <- sort(c("food", "losses", "other", "stock_addition", "tourist",
                  "fuel", "other_industrial", "unknown_use"))

saveRDS(list(areas = areas, processes = processes, commodities = commodities, fd_vars = fd_vars),
        file.path(output_dir_mode, "mrsut_dims_bcp.rds"))




###########################################################
########### SUPPLY #######################
###########################################################

###########################################################
#1. With mass allocation
###########################################################

# Template to always get full tables
template <- data.table(expand.grid(
  proc_code = processes, comm_code = commodities, stringsAsFactors = FALSE))
setkey(template, proc_code, comm_code)

# Ensure sup is a data.table before the (mass and value) mclapply blocks below,
# so data.table subsetting works inside the forked workers.
setDT(sup)

# List with block-diagonal supply matrices, per year
mr_sup_mass <- mclapply(years, function(x) {
  
  matrices <- lapply(areas, function(y, sup_y) {
    # Get supply for area y and merge with the template
    sup_x <- sup_y[area_code == y, .(proc_code, comm_code, supply)]
    out <- if(nrow(sup_x) == 0) {
      template[, .(proc_code, comm_code, supply = 0)]
    } else {merge(template, sup_x, all.x = TRUE)}
    
    # Cast the datatable to convert into a matrix
    out <- tryCatch(data.table::dcast(out, proc_code ~ comm_code,
                                      value.var = "supply", fun.aggregate = sum, na.rm = TRUE, fill = 0),
                    error = function(e) {stop("Issue at ", x, "_", y, ": ", e)})
    
    # Return a (sparse) matrix of supply for region y and year x
    return(Matrix(data.matrix(out[, c(-1)]), sparse = TRUE,
                  dimnames = list(out$proc_code, colnames(out)[-1])))
    
  }, sup_y = sup[year == x, .(area_code, proc_code, comm_code, supply)])
  
  # Return a block-diagonal matrix with all countries for year x
  return(bdiag(matrices))
}, mc.cores = detectCores() - 2)

names(mr_sup_mass) <- years

###########################################################
#2. With value allocation
###########################################################

# Convert to monetary values
sup[!is.na(price) & is.finite(price), value := supply * price]
# If no price available, keep physical quantities
sup[is.na(price) | !is.finite(price), value := supply]

# List with block-diagonal supply matrices in value, per year
mr_sup_value <- mclapply(years, function(x) {
  
  matrices <- lapply(areas, function(y, sup_y) {
    # Get supply for area y and merge with the template
    sup_x <- sup_y[area_code == y, .(proc_code, comm_code, value)]
    out <- if(nrow(sup_x) == 0) {
      template[, .(proc_code, comm_code, value = 0)]
    } else {merge(template, sup_x, all.x = TRUE)}
    
    # Cast the datatable to convert into a matrix
    out <- tryCatch(data.table::dcast(out, proc_code ~ comm_code,
                                      value.var = "value", fun.aggregate = sum, na.rm = TRUE, fill = 0),
                    error = function(e) {stop("Issue at ", x, "_", y, ": ", e)})
    
    # Return a (sparse) matrix of supply for region y and year x
    return(Matrix(data.matrix(out[, c(-1)]), sparse = TRUE,
                  dimnames = list(out$proc_code, colnames(out)[-1])))
    
  }, sup_y = sup[year == x, .(area_code, proc_code, comm_code, value)])
  
  # Return a block-diagonal matrix with all countries for year x
  return(bdiag(matrices))
}, mc.cores = detectCores() - 2)

names(mr_sup_value) <- years

saveRDS(mr_sup_mass, file.path(output_dir_mode,"mr_sup_mass.rds"))
saveRDS(mr_sup_value, file.path(output_dir_mode,"mr_sup_value.rds"))


# Bilateral supply shares ---

# Add grazing
btd <- merge(btd, sup[item=="Grazing", .(from_code = area_code, to_code = area_code,
                                         grazing = supply, year, item_code, comm_code)],
             by = c("from_code", "to_code", "year", "item_code", "comm_code"), all.x = TRUE)
btd[!is.na(grazing), value := grazing]
btd[, grazing := NULL]

# Template to always get full tables
template <- data.table(expand.grid(
  from_code = areas, to_code = areas,
  comm_code = commodities, stringsAsFactors = FALSE))
setkey(template, from_code, comm_code, to_code)

# Yearly list of BTD in matrix format
# Note that btd_final includes not only re-export adjusted bilateral trade flows,
# but also domestic production for domestic use, i.e. it gives the sources
# (domestic and imported) of each country's domestic use of any item.
btd_cast <- mclapply(years, function(x, btd_x) {
  # Cast to convert to matrix
  out <- data.table::dcast(merge(template,
                                 btd_x[year == x, .(from_code, to_code, comm_code, value)],
                                 by = c("from_code", "to_code", "comm_code"), all.x = TRUE),
                           to_code + comm_code ~ from_code,
                           value.var = "value", fun.aggregate = sum, na.rm = TRUE, fill = 0)
  
  return(Matrix(data.matrix(out[, c(-1, -2)]), sparse = TRUE,
                dimnames = list(paste0(out$to_code, "_", out$comm_code),
                                colnames(out)[c(-1, -2)])))
  
}, btd_x = btd[, .(year, from_code, to_code, comm_code, value)], mc.cores = detectCores() - 2)

names(btd_cast) <- years

# # Get commodities and their positions from total supply for domestic use
# comms <- gsub("(^[0-9]+)_(c[0-9]+)", "\\2", rownames(btd_cast[[1]]))
# is <- as.numeric(vapply(unique(comms), function(x) {which(comms == x)},
#   numeric(length(unique(areas)))))
# js <- rep(seq(unique(comms)), each = length(unique(areas)))
# # Matrix used to aggregate over commodities
# agg <- Matrix::sparseMatrix(i = is, j = js)
# 
# # Build supply shares, per year
# supply_shares <- mclapply(btd_cast, function(x, agg, js) {
#   # x_agg <- colSums(crossprod(x, agg)) # Aggregate total supply (all countries)
#   x_agg <- crossprod(x, agg) # Aggregate total supply (per country)
#   denom <- data.table(as.matrix(t(x_agg)))
#   # Calculate shares (per country)
#   out <- as.matrix(x / as.matrix(denom[rep(seq(length(commodities)), length(areas)), ]))
#   out[!is.finite(out)] <- 0 # See Issue #75
# 
#   # # source is domestic, where no sources given in btd_final
#   # # this isn't needed anymore as domestic grazing supply is now included in btd
#   # for(i in 1:nrow(regions)){
#   #   out[nrow(items)*(i-1)+62, i] <- 1
#   # }
# 
#   return(as(out, "Matrix"))
# }, agg = agg, js = js, mc.cores = detectCores() - 2)


# supply_shares <- readRDS("data/sup_shares_list.rds")

###########################################################
########### USE #######################
###########################################################

# Build use shares, per year
use_shares <- mclapply(btd_cast, function(x) {
  rs <- rowSums(x)
  rs[rs == 0] <- 1  
  # normalize each row
  shares <- x / rs
  
  
  # reshape shares from target-country * product (Ct–P) × source-country (Cs) to Cs-P × Ct
  n_ctry <- length(areas)
  n_prod <- length(commodities)
  
  mat <- matrix(0, nrow = n_ctry * n_prod, ncol = n_ctry)
  
  for (co in seq_len(n_ctry)) {
    v <- as.numeric(shares[, co])
    M <- matrix(v, nrow = n_ctry, ncol = n_prod, byrow = TRUE) # R × P
    block <- t(M)  # P × R
    rows <- ((co - 1) * n_prod + 1):(co * n_prod)
    mat[rows, ] <- block
  }
  
  return(as(mat, "Matrix"))
}, mc.cores = detectCores() - 2)



# Use ---

# Template to always get full tables
template <- data.table(expand.grid(
  area_code = areas, proc_code = processes, comm_code = commodities,
  stringsAsFactors = FALSE))
setkey(template, area_code, proc_code, comm_code)

# List with use matrices, per year
use_cast <- mclapply(years, function(x, use_x) {
  # Cast use to convert to a matrix
  out <- data.table::dcast(merge(template[, .(area_code, proc_code, comm_code)],
                                 use_x[year == x, .(area_code, proc_code, comm_code, use)],
                                 by = c("area_code", "proc_code", "comm_code"), all.x = TRUE),
                           comm_code ~ area_code + proc_code,
                           value.var = "use", fun.aggregate = sum, na.rm = TRUE, fill = 0)
  
  return(Matrix(data.matrix(out[, c(-1)]), sparse = TRUE,
                dimnames = list(out$comm_code, colnames(out)[-1])))
  
}, use_x = use[, .(year, area_code, proc_code, comm_code, use)], mc.cores = detectCores() - 2)


# # Apply supply shares to the use matrix
# mr_use <- mcmapply(function(x, y) {
#   # Repeat use values, then adapted according to shares
#   mr_x <- x[rep(seq_along(commodities), length(areas)), ]
#   n_proc <- length(processes)
# 
#   for(j in seq_along(areas)) { # Per country j
#     mr_x[, seq(1 + (j - 1) * n_proc, j * n_proc)] <-
#       mr_x[, seq(1 + (j - 1) * n_proc, j * n_proc)] * y[, j]
#   }
# 
#   return(mr_x)
# }, use_cast, supply_shares, mc.cores = detectCores() - 2)



# Apply supply shares to the use matrix
mr_use <- mcmapply(function(x, y) {
  # dimensions
  C <- nrow(x)      # number of commodities
  RP <- ncol(x)     # regions * processes
  RC <- nrow(y)     # regions * commodities
  R  <- ncol(y)     # number of regions
  P  <- RP / R      # processes
  
  # Expand x and y to dimension (R * C) × (R * P) = 23001 × 22253
  
  # Expand x: replicate each commodity row for each region
  # dim(x) = C × (R * P) = 123 × 22253
  X_expanded <- kronecker(Matrix::Matrix(1, R, 1), x)
  
  # Expand y: replicate supply shares for each process
  # dim(y) = (R * C) × R = 23001 × 187
  Y_expanded <- kronecker(y, Matrix::Matrix(1, 1, P))
  Y_expanded <- Y_expanded[, order(rep(1:ncol(y), each = P))]
  
  # Multiply elementwise
  result <- Y_expanded * X_expanded
  
  return(result)
}, use_cast, use_shares, mc.cores = detectCores() - 2)




# # Apply supply shares to the use matrix
# # This code does the same. It offers more robustness and clarity, but takes 4 times as long to run.
# future::plan(multisession, workers = 10)
# mr_use <- future_lapply(seq_along(btd_cast), function(t) {
#   B <- btd_cast[[t]]  # (R·C) × R
#   U <- use_cast[[t]]  # C × (R·P)
#   
#   rc_ids <- rownames(B)
#   rp_ids <- colnames(U)
#   c_ids  <- rownames(U)
#   
#   rc_split <- do.call(rbind, strsplit(rc_ids, "_", fixed = TRUE))
#   region_B <- rc_split[, 1]
#   commodity_B <- rc_split[, 2]
#   
#   rp_split <- do.call(rbind, strsplit(rp_ids, "_", fixed = TRUE))
#   region_U <- rp_split[, 1]
#   process_U <- rp_split[, 2]
#   
#   col_B_map <- match(region_U, colnames(B))
#   
#   A <- Matrix(0, nrow = nrow(B), ncol = ncol(U), sparse = TRUE,
#               dimnames = list(rc_ids, rp_ids))
#   
#   for (j in seq_along(rp_ids)) {
#     r2_col <- col_B_map[j]
#     if (is.na(r2_col)) next
#     
#     c_demand <- U[, j]
#     nz <- which(c_demand != 0)
#     
#     for (i in nz) {
#       c <- c_ids[i]
#       demand <- c_demand[i]
#       
#       rows_c <- which(commodity_B == c)
#       if (length(rows_c) == 0) next
#       
#       supply <- B[rows_c, r2_col]
#       s_total <- sum(supply)
#       if (s_total == 0) next
#       
#       A[rows_c, j] <- A[rows_c, j] + (supply / s_total) * demand
#     }
#   }
#   
#   return(A)
# })

names(mr_use) <- years
saveRDS(mr_use, file.path(output_dir_mode,"mr_use.rds"))


# Final Demand ---

# Template to always get full tables
template <- data.table(expand.grid(
  area_code = areas, comm_code = commodities,
  variable = fd_vars,
  stringsAsFactors = FALSE))
setkey(template, area_code, comm_code, variable)

use_fd <- melt(use_fd[, c("year", "area_code", "comm_code", fd_vars), with = FALSE],
               id.vars = c("year", "area_code", "comm_code"))

# List with final use matrices, per year
use_fd_cast <- mclapply(years, function(x, use_fd_x) {
  # Cast final use to convert to a matrix
  out <- data.table::dcast(merge(template[, .(area_code, comm_code, variable)],
                                 use_fd_x[year == x, .(area_code, comm_code, variable, value)],
                                 by = c("area_code", "comm_code", "variable"), all.x = TRUE),
                           comm_code ~ area_code + variable,
                           value.var = "value", fun.aggregate = sum, na.rm = TRUE, fill = 0)
  
  Matrix(data.matrix(out[, -1]), sparse = TRUE,
         dimnames = list(out$comm_code, colnames(out)[-1]))
}, use_fd[, .(year, area_code, comm_code, variable, value)], mc.cores = 6)

# Apply use shares to the use_fd matrix
mr_use_fd <- mcmapply(function(x, y) {
  # dimensions
  C  <- nrow(x)     # number of commodities
  RD <- ncol(x)     # regions * final demand categories
  RC <- nrow(y)     # regions * commodities
  R  <- ncol(y)     # number of regions
  D  <- RD / R      # final demand categories
  
  # Expand x and y to dimension (R * C) × (R * FD) = 23001 × 1122
  
  # Expand x: replicate each commodity row for each region
  # dim(x) = C × (R * D) = 123 × 1122
  X_expanded <- kronecker(Matrix::Matrix(1, R, 1), x)
  
  # Expand y: replicate supply shares for each process
  # dim(y) = (R * C) × R = 23001 × 187
  Y_expanded <- kronecker(y, Matrix::Matrix(1, 1, D))
  Y_expanded <- Y_expanded[, order(rep(1:ncol(y), each = D))]
  
  # Multiply elementwise
  result <- Y_expanded * X_expanded
  
  colnames(result) <- colnames(x)
  
  return(result)
}, use_fd_cast, use_shares, mc.cores = detectCores() - 2)



mr_use_fd <- lapply(mr_use_fd, round)
names(mr_use_fd) <- years
saveRDS(mr_use_fd, file.path(output_dir_mode,"mr_use_fd.rds"))