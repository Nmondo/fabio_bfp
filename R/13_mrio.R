###########################################################
########### LOADING PACKAGES #########
###########################################################

setwd("/home/mmondolfo/fabio_bfp/")

library(Matrix)
library(parallel)
library(data.table)
library(readr)

source("R/00_system_variables.R")
agg <- function(x) { as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x)) }




###########################################################
########### MRIO TABLE #########
###########################################################


# Load multi-regional supply and use tables ---
# mr_sup_m <- readRDS(file.path(output_dir_bcp,"mr_sup_mass.rds"))  # list of industry × product in mass units
mr_sup_v <- readRDS(file.path(output_dir_bcp,"mr_sup_value.rds")) # list of industry × product in monetary units
mr_use <- readRDS(file.path(output_dir_bcp,"mr_use.rds"))         # list of product × industry in mass units




###########################################################
#1. With mass allocation
###########################################################

# # --- Step 1: Normalize supply (industry shares of products) ---
# trans_m <- mclapply(mr_sup_m, function(x) {
#   rs <- rowSums(x)                # row sums (industry outputs)
#   rs[rs == 0] <- 1                # avoid division by zero
#   out <- x
#   out@x <- out@x / rs[out@i + 1]
#   out
# }, mc.cores = detectCores() - 2)
# 
# # --- Step 2: Compute symmetric product × product tables ---
# Z_m <- mcmapply(function(use, trans) {
#   out <- use %*% trans
#   out[!is.finite(out)] <- 0  # just in case
#   out
# }, mr_use, trans_m, SIMPLIFY = FALSE, mc.cores = detectCores() - 2)
# 
# Z_m <- lapply(Z_m, round)




###########################################################
#2. With value allocation
###########################################################

# --- Step 1: Normalize supply (industry shares of products) ---
trans_v <- mclapply(mr_sup_v, function(x) {
  rs <- rowSums(x)                # row sums (industry outputs)
  rs[rs == 0] <- 1                # avoid division by zero
  out <- x
  out@x <- out@x / rs[out@i + 1]
  out
}, mc.cores = detectCores() - 2)

# --- Step 2: Compute symmetric product × product tables ---
Z_v <- mcmapply(function(use, trans) {
  out <- use %*% trans
  out[!is.finite(out)] <- 0  # just in case
  out
}, mr_use, trans_v, SIMPLIFY = FALSE, mc.cores = detectCores() - 2)


Z_v <- lapply(Z_v, round)



# Rebalance row sums in Z and Y -----------------------------------------

regions <- fread("inst/regions_full.csv")[current==TRUE]
items <- fread("inst/items_full_bcp.csv")
nrcom <- nrow(items)
Y <- readRDS(file.path(output_dir_bcp,"mr_use_fd.rds"))




# Derive total output X ---------------------------------------------

X <- mapply(function(x, y) {
  rowSums(x) + rowSums(y)
}, x = Z_v, y = Y)




X <- mcmapply(function(z, y) {
  # Initial row sums
  x <- rowSums(z) + rowSums(y)
  
  # Find negative entries
  neg_idx <- which(x < 0)
  
  if (length(neg_idx) > 0) {
    for (i in neg_idx) {
      diff <- -x[i]  # amount needed to fix
      
      # Find a negative in y to adjust
      y_row <- y[i, ]
      y_neg <- which(y_row < 0)
      
      if (length(y_neg) > 0) {
        j <- y_neg[1] # pick first negative (could also choose biggest abs value etc.)
        y[i, j] <- y[i, j] + diff
      }
    }
  }
  
  # Return corrected row sums
  rowSums(z) + rowSums(y)
}, z = Z_v, y = Y, SIMPLIFY = FALSE, mc.cores = detectCores() - 2)

# Convert list of vectors to a matrix
X <- do.call(cbind, X)




# PROBLEM: There are some products with only zeros in the rows, except of the main diagonal
# i.e. the value on the main diagonal equals total output
# this is mainly due to reporting issues in FAOSTAT, where some countries report seed = production
# SOLUTION: We move 80% of the value to final demand, equally spreading over all fd-categories

fd_labels <- fread(file.path(output_dir_bcp,"fd_labels.csv"))
io_labels <- read_csv(file.path(output_dir_bcp,"io_labels.csv"))

# year <- 2020
for(year in years){
  
  print(year)
  
  #  Zmi <- Z_m[[as.character(year)]]
  Zvi <- Z_v[[as.character(year)]]
  Yi <- Y[[as.character(year)]]
  Xi <- X[,as.character(year)]
  
  # Assign column and row names
  colnames(Yi) <- fd_labels$fd
  rownames(Yi) <- io_labels$comm_code
  
  # Precompute global totals
  Y_global <- t(agg(t(agg(Yi))))
  
  # Pre-identify relevant indices where update is needed
  diag_Zvi <- Matrix::diag(Zvi)
  valid <- (Xi != 0) & (diag_Zvi >= Xi)
  # print(table(valid))
  
  # Get area match matrix (cache once)
  area_match <- fd_labels$area_code
  
  for (i in which(valid)) {
    
    area_col <- which(area_match == io_labels$area_code[i])
    temp <- Yi[i, area_col]
    
    if (sum(temp) == 0) {
      temp <- Y_global[rownames(Y_global) == io_labels$comm_code[i], ]
    }
    
    if (sum(temp) > 0) {
      # Compute new Yi row for area
      bal <- Zvi[i,i] * 0.8
      share <- temp / sum(temp)
      Yi[i, area_col] <- temp + bal * share
      
      # Update Z matrices
      Zvi[i, i] <- Zvi[i,i] * 0.2
      
      # Update X
      Xi[i] <- sum(Zvi[i, ]) + sum(Yi[i, ])
    }
  }
  
  # Save back results
  #  Z_m[[as.character(year)]] <- Zmi
  Z_v[[as.character(year)]] <- Zvi
  Y[[as.character(year)]] <- Yi
  X[,as.character(year)] <- Xi
}


# set row and column names in X, Z and Y
io_names <- paste0(io_labels$iso3c, "_", io_labels$comm_code)
fd_names <- paste0(fd_labels$iso3c, "_", fd_labels$fd)

colnames(X) <- years
rownames(X) <- io_names

for(year in years){
  
  print(year)
  
  #  Zmi <- Z_m[[as.character(year)]]
  Zvi <- Z_v[[as.character(year)]]
  Yi <- Y[[as.character(year)]]
  
  # Assign row and column names
  rownames(Yi) <- rownames(Zvi) <- colnames(Zvi) <- io_names
  colnames(Yi) <- fd_names
  
  
  # Save back results
  #  Z_m[[as.character(year)]] <- Zmi
  Z_v[[as.character(year)]] <- Zvi
  Y[[as.character(year)]] <- Yi
}


# Store X, Y, Z variables
# saveRDS(Z_m, file.path(output_dir_bcp,"Z_mass.rds"))
saveRDS(Z_v, file.path(output_dir_bcp,"Z_value.rds"))
saveRDS(Y, file.path(output_dir_bcp,"Y.rds"))
saveRDS(X, file.path(output_dir_bcp,"X.rds"))




# create version of fabio with losses endogenized (on the main diagonal of Z) ---
# i.e. a version where losses are considered an own use of each sector instead of being a final demand category

for(year in years){
  
  print(year)
  
  # remove losses from Y
  Yi <- Y[[as.character(year)]]
  losses <- as.matrix(Yi[, grepl("losses", colnames(Yi))])
  Yi <- Yi[, !grepl("losses", colnames(Yi))]
  
  Y[[as.character(year)]] <- Yi
  
  # reshape losses + balancing for adding them later to the main diagonals of each submatrix of Z
  ## Get the number of rows and columns in the data matrix
  num_rows <- nrow(losses)
  num_cols <- nrow(losses) / ncol(losses)
  
  ## Define a function for reshaping
  reshape_column <- function(v) {
    m <- matrix(0, ncol = num_cols, nrow = num_rows)
    indices <- ((seq_len(length(v)) - 1) %% num_cols) + 1
    m[cbind(seq_len(length(v)), indices)] <- v
    return(m)
  }
  
  ## Apply the reshape_column function to each column using lapply
  matrix_list <- lapply(1:ncol(losses), function(i) {
    v <- losses[, i]
    reshape_column(v)
  })
  
  ## Combine the matrices in the list using cbind()
  combined_matrix <- do.call(cbind, matrix_list)
  combined_matrix <- as(combined_matrix, "dgCMatrix")
  
  # # add losses to the main diagonals of each submatrix of Z_m
  # Zi <- Z_m[[as.character(year)]]
  # Zi <- Zi + combined_matrix
  # Z_m[[as.character(year)]] <- Zi
  
  # add losses to the main diagonals of each submatrix of Z_v
  Zi <- Z_v[[as.character(year)]]
  Zi <- Zi + combined_matrix
  Z_v[[as.character(year)]] <- Zi
  
}


# PROBLEM: There are some products with only zeros in the rows, except of the main diagonal
# i.e. the value on the main diagonal equals total output
# this is mainly due to reporting issues in FAOSTAT, where some countries report seed = production
# SOLUTION: We move 80% of the value to final demand, equally spreading over all fd-categories

fd_labels <- fread(file.path(output_dir_bcp,"losses/fd_labels.csv"))

# year <- 2019
for(year in years){
  
  print(year)
  
  # Zmi <- Z_m[[as.character(year)]]
  Zvi <- Z_v[[as.character(year)]]
  Yi <- Y[[as.character(year)]]
  Xi <- X[,as.character(year)]
  
  # Precompute global totals
  Y_global <- copy(Yi)
  colnames(Y_global) <- fd_labels$fd
  rownames(Y_global) <- io_labels$comm_code
  Y_global <- t(agg(t(agg(Y_global))))
  
  # Pre-identify relevant indices where update is needed
  diag_Zvi <- Matrix::diag(Zvi)
  valid <- (Xi != 0) & (diag_Zvi >= Xi)
  # print(table(valid))
  
  # Get area match matrix (cache once)
  area_match <- fd_labels$area_code
  
  for (i in which(valid)) {
    
    area_col <- which(area_match == io_labels$area_code[i])
    temp <- Yi[i, area_col]
    
    if (sum(temp) == 0) {
      temp <- Y_global[rownames(Y_global) == io_labels$comm_code[i], ]
    }
    
    if (sum(temp) > 0) {
      # Compute new Yi row for area
      bal <- Zvi[i,i] * 0.8
      share <- temp / sum(temp)
      Yi[i, area_col] <- temp + bal * share
      
      # Update Z matrices
      Zvi[i, i] <- Zvi[i,i] * 0.2
      
      # Update X
      Xi[i] <- sum(Zvi[i, ]) + sum(Yi[i, ])
    }
  }
  
  # Save back results
  # Z_m[[as.character(year)]] <- Zmi
  Z_v[[as.character(year)]] <- Zvi
  Y[[as.character(year)]] <- Yi
  X[,as.character(year)] <- Xi
  
}


saveRDS(X, file.path(output_dir_bcp,"losses/X.rds"))
saveRDS(Y, file.path(output_dir_bcp,"losses/Y.rds"))
# saveRDS(Z_m, file.path(output_dir_bcp,"losses/Z_mass.rds"))
saveRDS(Z_v, file.path(output_dir_bcp,"losses/Z_value.rds"))






# Correction of Other vs. Food use of Chinese veg. oils --------------------------------------------------------------
# FAO overstates other and understates food use. We use official Chinese data and USDA data to correct this.
io <- fread(file.path(output_dir_bcp,"io_labels.csv"))
su <- fread(file.path(output_dir_bcp,"su_labels.csv"))
fd <- fread(file.path(output_dir_bcp,"fd_labels.csv"))
Y <- readRDS(file.path(output_dir_bcp,"Y.rds"))
fd_l <- fread(file.path(output_dir_bcp,"losses/fd_labels.csv"))
Y_l <- readRDS(file.path(output_dir_bcp,"losses/Y.rds"))

# Chinese edible oil statistics
# Sources: 
# - China National Grain & Oils Information Center, Comprehensive balance analysis of China's edible oil market, http://www.grainoil.com.cn/, accessed on 01/03/2023
# - USDA GAIN, Oilseeds and Products Annual, https://gain.fas.usda.gov/#/search
oil <- fread("input/oils_china.csv")

Y_new <- Y
Y_l_new <- Y_l

# correct food and other use of veg. oils for China
i = 1
for(i in seq_along(Y)){
  print(years[i])
  data <- merge(io, oil[year==oil$year[which.min(abs(oil$year - years[i]))],.(comm_code, food_share)], 
                by = "comm_code", all.x = TRUE, sort = FALSE)
  
  data <- cbind(data, as.matrix(Y[[i]][,fd$area=="China, mainland"]))
  # data[, food_share_fao := `41_food` / (`41_food` + `41_other`)]
  data[, `:=`(food = `CHN_food`, other = `CHN_other`)]
  data[!is.na(food_share), `:=`(food = round((food + other) * food_share),
                                other = round((food + other) * (1-food_share)))]
  Y_new[[i]][, fd$area_code==41 & fd$fd=="food"] <- data$food
  Y_new[[i]][, fd$area_code==41 & fd$fd=="other"] <- data$other
  
  data_l <- cbind(data, as.matrix(Y_l[[i]][,fd_l$area=="China, mainland"]))
  data_l[!is.na(food_share), `:=`(food = round((food + other) * food_share),
                                  other = round((food + other) * (1-food_share)))]
  Y_l_new[[i]][, fd_l$area_code==41 & fd_l$fd=="food"] <- data_l$food
  Y_l_new[[i]][, fd_l$area_code==41 & fd_l$fd=="other"] <- data_l$other
}

# compare old and new values
for(i in seq_along(Y)){
  food <- sum(Y[[i]][io$comm_code %in% oil$comm_code, fd$area_code==41 & fd$fd=="food"])
  other <- sum(Y[[i]][io$comm_code %in% oil$comm_code, fd$area_code==41 & fd$fd=="other"])
  share <- food / (food + other)
  food_new <- sum(Y_new[[i]][io$comm_code %in% oil$comm_code, fd$area_code==41 & fd$fd=="food"])
  other_new <- sum(Y_new[[i]][io$comm_code %in% oil$comm_code, fd$area_code==41 & fd$fd=="other"])
  share_new <- food_new / (food_new + other_new)
  print(paste0(years[i], ": ", round(food/1000000), "/", round(other/1000000), " Mt, ", round(share*100), "% // ",
               round(food_new/1000000), "/", round(other_new/1000000), " Mt, ", round(share_new*100), "%"))
}

saveRDS(Y_new, file.path(output_dir_bcp,"Y.rds"))
saveRDS(Y_l_new, file.path(output_dir_bcp,"losses/Y.rds"))



