###########################################################
########### LOADING PACKAGES #########
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

library(data.table)
library(Matrix)
source("R/00_system_variables.R")
source("R/00_run_config.R")                 # RUN_MODE / mode_dir()
output_dir_mode <- mode_dir(output_dir_bcp) # rescaled -> base dir; bypass -> base/bypass/
# Leontief inverse ---
prep_solve <- function(year, Z, X,
                       adj_X = FALSE, adj_A = TRUE, adj_diag = FALSE) {
  if(adj_X) {X <- X + 1e-10}
  A <- Matrix(0, nrow(Z), ncol(Z))
  idx <- X != 0
  A[, idx] <- t(t(Z[, idx]) / X[idx])
  #A <- Z
  #A@x <- A@x / rep.int(X, diff(A@p))
  if(adj_A) {A@x <- pmax(A@x, 0)}
  if(adj_diag) {diag(A)[diag(A) == 1] <- 1 - 1e-10}
  L <- .sparseDiagonal(nrow(A)) - A
  lu(L) # Computes LU decomposition and stores it in L
  #tryCatch({
  L_inv <- solve(L, tol = .Machine[["double.eps"]], sparse = TRUE)
  #}, error=function(e){cat("ERROR in ", year, "\n")})
  #L_inv[L_inv<0] <- 0
  return(L_inv)
}
years_singular <- 0 #c(2011,2013)
years_singular_losses <- 2010 #c(2011,2013,2021,2022)
Z_m <- readRDS(file.path(output_dir_mode,"Z_mass.rds"))
Z_v <- readRDS(file.path(output_dir_mode,"Z_value.rds"))
Y <- readRDS(file.path(output_dir_mode,"Y.rds"))
X <- readRDS(file.path(output_dir_mode,"X.rds"))
#year <- 2013
for(year in years){
  print(year)
  adjust <- ifelse(year %in% years_singular, TRUE, FALSE)
  L <- prep_solve(year = year, Z = Z_m[[as.character(year)]],
                  X = X[, as.character(year)], adj_diag = adjust)
  L[L<0] <- 0
  saveRDS(L, paste0(output_dir_mode,"/", year, "_L_mass.rds"))
  L <- prep_solve(year = year, Z = Z_v[[as.character(year)]],
                  X = X[, as.character(year)], adj_diag = adjust)
  L[L<0] <- 0
  saveRDS(L, paste0(output_dir_mode,"/", year, "_L_value.rds"))
}
# L inverse for losses version of fabio ---
X <- readRDS(file.path(output_dir_mode,"losses/X.rds"))
Y <- readRDS(file.path(output_dir_mode,"losses/Y.rds"))
Z_m <- readRDS(file.path(output_dir_mode,"losses/Z_mass.rds"))
Z_v <- readRDS(file.path(output_dir_mode,"losses/Z_value.rds"))
#year <- 2022
for(year in years){
  print(year)
  adjust_losses <- ifelse(year %in% years_singular_losses, TRUE, FALSE)
  L <- prep_solve(year = year, Z = Z_m[[as.character(year)]],
                  X = X[, as.character(year)], adj_diag = adjust_losses)
  L[L<0] <- 0
  saveRDS(L, paste0(output_dir_mode,"/losses/", year, "_L_mass.rds"))
  L <- prep_solve(year = year, Z = Z_v[[as.character(year)]],
                  X = X[, as.character(year)], adj_diag = adjust_losses)
  L[L<0] <- 0
  saveRDS(L, paste0(output_dir_mode,"/losses/", year, "_L_value.rds"))
}
## Checking some rows to check consistency
# 
# L         <- readRDS(file.path(output_dir_mode, "2019_L_value.rds"))
# io_labels <- fread(file.path(output_dir_mode, "io_labels.csv"))
# items     <- fread("inst/items_full_bcp.csv")
# 
# rownames(L) <- colnames(L) <- paste0(io_labels$iso3c, "_", io_labels$comm_code)
# 
# target <- paste0("CHN_c", 145)
# 
# col_long <- as.data.table(as.data.frame(as.matrix(L[, target])),
#                           keep.rownames = "src") |>
#   melt(id.vars = "src", variable.name = "target", value.name = "coef") |>
#   _[coef > 1e-10]
# 
# col_long[, `:=`(src_region = sub("_.*$",    "", src),
#                 src_comm   = sub("^[^_]+_", "", src))]
# 
# col_long <- merge(col_long, unique(items[, .(comm_code, item)]),
#                   by.x = "src_comm", by.y = "comm_code",
#                   all.x = TRUE, sort = FALSE)
# col_long[src_comm == "c145", item := "Used cooking oil"]
# 
# View(col_long)