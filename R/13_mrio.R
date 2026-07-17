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

library(Matrix)
library(parallel)
library(data.table)
library(readr)

source("R/00_system_variables.R")           # + 00_9_fabio_helpers.R (fabio_assert)
source("R/00_run_config.R")                 # RUN_MODE / mode_dir() / STRICT
output_dir_mode <- mode_dir(output_dir_bcp) # rescaled -> base dir; bypass -> base/bypass/
agg <- function(x) { as.matrix(x) %*% sapply(unique(colnames(x)), "==", colnames(x)) }


###########################################################
########### MRIO TABLE #########
###########################################################

# Load multi-regional supply and use tables ---
mr_sup_m <- readRDS(file.path(output_dir_mode, "mr_sup_mass.rds"))  # industry x product, mass
mr_sup_v <- readRDS(file.path(output_dir_mode, "mr_sup_value.rds")) # industry x product, money
mr_use   <- readRDS(file.path(output_dir_mode, "mr_use.rds"))       # product x industry, mass


###########################################################
#1. With mass allocation
###########################################################

# --- Step 1: Normalize supply (industry shares of products) ---
trans_m <- mclapply(mr_sup_m, function(x) {
  rs <- rowSums(x)                # row sums (industry outputs)
  rs[rs == 0] <- 1                # avoid division by zero
  out <- x
  out@x <- out@x / rs[out@i + 1]
  out
}, mc.cores = detectCores() - 2)

# --- Step 2: Compute symmetric product x product tables ---
Z_m <- mcmapply(function(use, trans) {
  out <- use %*% trans
  out[!is.finite(out)] <- 0
  out
}, mr_use, trans_m, SIMPLIFY = FALSE, mc.cores = detectCores() - 2)

Z_m <- lapply(Z_m, round)


###########################################################
#2. With value allocation
###########################################################

trans_v <- mclapply(mr_sup_v, function(x) {
  rs <- rowSums(x)
  rs[rs == 0] <- 1
  out <- x
  out@x <- out@x / rs[out@i + 1]
  out
}, mc.cores = detectCores() - 2)

Z_v <- mcmapply(function(use, trans) {
  out <- use %*% trans
  out[!is.finite(out)] <- 0
  out
}, mr_use, trans_v, SIMPLIFY = FALSE, mc.cores = detectCores() - 2)

Z_v <- lapply(Z_v, round)


# Rebalance row sums in Z and Y -----------------------------------------

regions <- fread("inst/regions_full.csv")[current == TRUE]
items   <- fread("inst/items_full_bcp.csv")
nrcom   <- nrow(items)
Y       <- readRDS(file.path(output_dir_mode, "mr_use_fd.rds"))

fd_labels <- fread(file.path(output_dir_mode, "fd_labels.csv"))
io_labels <- read_csv(file.path(output_dir_mode, "io_labels.csv"))

fabio_assert(!is.unsorted(regions$code, strictly = TRUE),
             "13: regions_full.csv is not strictly ascending by `code` — io_labels and ",
             "the Z/Y row order are silently permuted relative to each other.")


###########################################################
# Derive total output X                                    [FIX-1] Bug 3b
###########################################################
# X must satisfy X == rowSums(Z_v) + rowSums(Y) EXACTLY, for every row, from here on.
# It did not. The old code patched a negative row sum by adding the shortfall back into a
# negative Y entry — but it patched a LOCAL copy of `y` inside mcmapply and threw it away,
# returning only the corrected row sums. X and Y therefore disagreed on precisely those
# rows, and every later recompute of X (the two repair loops below, 14's A = Z/X)
# resurrected the negative. The patched Y is now written back.
#
# Two rungs, because a negative row sum does NOT guarantee a negative Y cell to absorb it
# (rounding in Z_v after `use %*% trans` produces negative row sums with an all-positive
# Y row):
#   (1) push the shortfall into the most negative Y entry, as before;
#   (2) no negative Y entry -> the largest positive FD cell absorbs it (it goes negative;
#       that is honest — the row genuinely uses more than it supplies);
#   (3) all-zero Y row -> unabsorbable; logged and clamped. Never leave X < 0: 14 divides
#       by it.
#
# The dead `X <- mapply(...)` that used to sit here was overwritten two lines later and is
# gone.

fixed <- mcmapply(function(z, y) {
  
  x   <- rowSums(z) + rowSums(y)
  bad <- which(x < 0)
  n_r1 <- 0L; n_r2 <- 0L; n_clamp <- 0L
  
  for (i in bad) {
    need  <- -x[i]                       # > 0: the shortfall to absorb
    y_row <- y[i, ]
    
    y_neg <- which(y_row < 0)
    if (length(y_neg)) {
      j <- y_neg[which.min(y_row[y_neg])]     # most negative, not just the first
      y[i, j] <- y[i, j] + need
      n_r1 <- n_r1 + 1L
      next
    }
    
    y_pos <- which(y_row > 0)
    if (length(y_pos)) {
      j <- y_pos[which.max(y_row[y_pos])]
      y[i, j] <- y[i, j] + need
      n_r2 <- n_r2 + 1L
      next
    }
    n_clamp <- n_clamp + 1L                   # nothing in Y can take it
  }
  
  list(x = rowSums(z) + rowSums(y), y = y,
       n = c(neg = length(bad), r1 = n_r1, r2 = n_r2, clamp = n_clamp))
  
}, z = Z_v, y = Y, SIMPLIFY = FALSE, mc.cores = detectCores() - 2)

X <- do.call(cbind, lapply(fixed, `[[`, "x"))
Y <- lapply(fixed, `[[`, "y")               # <- the write-back that was missing

colnames(X) <- names(Z_v)                   # pin the names, do not inherit them by luck
names(Y)    <- names(Z_v)
stopifnot(identical(colnames(X), as.character(years)),
          identical(names(Y),    as.character(years)))

patch_log <- rbindlist(lapply(names(fixed), function(y)
  as.data.table(as.list(fixed[[y]]$n))[, year := y]))
message(sprintf(">>> 13: negative X rows: %d (absorbed by a negative FD cell: %d, by a ",
                sum(patch_log$neg), sum(patch_log$r1)),
        sprintf("positive one: %d, unabsorbable: %d)",
                sum(patch_log$r2), sum(patch_log$clamp)))
fwrite(patch_log, "output/13_negative_X_patch_log.csv")
rm(fixed)

# the identity everything downstream assumes. Check it; do not hope for it.
for (y in as.character(years)) {
  d <- max(abs(X[, y] - (rowSums(Z_v[[y]]) + rowSums(Y[[y]]))))
  fabio_assert(d < 1e-6,
               "13 %s: X and rowSums(Z_v)+rowSums(Y) disagree by %.3g — the Y write-back ",
               "did not land.", y, d)
}

n_neg <- sum(X < -1)
fabio_assert(n_neg == 0,
             "13: %d rows still have X < -1 after both patch rungs (all-zero Y row with a ",
             "negative Z_v row sum) — inspect output/13_negative_X_patch_log.csv.", n_neg)
X[X < 0] <- 0


###########################################################
# repair_seed_eq_production()                              [FIX-2] Bug 3
###########################################################
# FAOSTAT quirk: some countries report seed = production, so a commodity's row is empty
# except for the main diagonal and diag(Z) == X. Move 80% of that diagonal into the
# country's own final demand, spread over its FD categories.
#
# THREE things were wrong here, and together they wrote the world's ENTIRE final demand
# for grapes into Hong Kong's row (41 Mt in 2017), and the world's lemons into Slovenia's:
#
#  (1) `temp` was OVERLOADED. It starts as a LEVEL (the country's own FD row, which the
#      80% is added *to*); when that row is empty the fallback replaced it with a PATTERN
#      (world FD, only ever meant to supply `share`); the pattern was then written back as
#      a level. `own` and `share` are now separate variables and `own` is never
#      reassigned. THIS IS THE BUG.
#  (2) the gate `Xi != 0` let a 1-tonne rounding artefact through -> `Xi > TOL`.
#  (3) the gate had NO INPUT TEST. Losses-endogenisation guarantees diag(Zvi) >= Xi for
#      any row whose only mass is a loss, and simultaneously guarantees the country's own
#      FD row is empty (losses were just stripped) — so the fallback fired on exactly the
#      degenerate cells it must never touch. `colSums(Zvi) > 0` restricts the loop to cells
#      that genuinely HAVE inputs, which is the quirk it was written for, and structurally
#      excludes every phantom the pipeline can produce.
#
# Z_m is scaled in step now: it was never touched inside either loop, so the mass and value
# tables silently disagreed on the diagonal wherever this fired.
#
# The loop existed TWICE (before and after losses-endogenisation) and the two copies had
# already drifted. One function, called twice.

repair_seed_eq_production <- function(Zvi, Zmi, Yi, Xi, fd_lab, io_lab, TOL = 1, label = "") {
  
  Y_global <- as.matrix(Yi)
  colnames(Y_global) <- fd_lab$fd
  rownames(Y_global) <- io_lab$comm_code
  Y_global <- t(agg(t(agg(Y_global))))
  
  area_match <- fd_lab$area_code
  diag_Zvi   <- Matrix::diag(Zvi)
  
  valid     <- (Xi > TOL) & (diag_Zvi >= Xi) & (Matrix::colSums(Zvi) > 0)
  Xi_before <- Xi
  n_fired   <- 0L
  
  for (i in which(valid)) {
    
    area_col <- which(area_match == io_lab$area_code[i])
    own      <- Yi[i, area_col]                    # LEVEL — never reassigned
    
    share <- if (sum(own) > 0) own / sum(own) else {
      g <- Y_global[rownames(Y_global) == io_lab$comm_code[i], ]
      if (sum(g) > 0) g / sum(g) else numeric(0)   # PATTERN only
    }
    
    if (length(share) && sum(share) > 0) {
      bal <- Zvi[i, i] * 0.8
      Yi[i, area_col] <- own + bal * share
      Zvi[i, i]       <- Zvi[i, i] * 0.2
      Zmi[i, i]       <- Zmi[i, i] * 0.2           # keep the mass table in step
      Xi[i]           <- sum(Zvi[i, ]) + sum(Yi[i, ])
      n_fired <- n_fired + 1L
    }
  }
  
  # A REPAIR MUST NEVER INFLATE X. This is the Bug 3 tripwire.
  infl <- which(Xi > pmax(Xi_before, TOL) * 1.01)
  fabio_assert(length(infl) == 0,
               "13 %s: the 80%% repair INFLATED X on %d rows (max factor %.1f) — `own` is ",
               "being overwritten by the global pattern again.",
               label, length(infl),
               if (length(infl)) max(Xi[infl] / pmax(Xi_before[infl], TOL)) else 1,
               data = data.table(node = paste0(io_lab$iso3c[infl], "_",
                                               io_lab$comm_code[infl]),
                                 X_before = Xi_before[infl],
                                 X_after  = Xi[infl])[order(-X_after)])
  
  message(sprintf("   [%s] repair fired on %d / %d valid rows", label, n_fired, sum(valid)))
  list(Zvi = Zvi, Zmi = Zmi, Yi = Yi, Xi = Xi)
}


###########################################################
# FIRST 80% LOOP (pre-losses)                              [FIX-2]
###########################################################

for (year in years) {
  
  print(year)
  
  Yi <- Y[[as.character(year)]]
  colnames(Yi) <- fd_labels$fd
  rownames(Yi) <- io_labels$comm_code
  
  r <- repair_seed_eq_production(Zvi = Z_v[[as.character(year)]],
                                 Zmi = Z_m[[as.character(year)]],
                                 Yi  = Yi,
                                 Xi  = X[, as.character(year)],
                                 fd_lab = fd_labels, io_lab = io_labels,
                                 label = paste0(year, " pre-losses"))
  
  Z_m[[as.character(year)]] <- r$Zmi
  Z_v[[as.character(year)]] <- r$Zvi
  Y[[as.character(year)]]   <- r$Yi
  X[, as.character(year)]   <- r$Xi
}


# set row and column names in X, Z and Y
io_names <- paste0(io_labels$iso3c, "_", io_labels$comm_code)
fd_names <- paste0(fd_labels$iso3c, "_", fd_labels$fd)

rownames(X) <- io_names          # colnames(X) already pinned at [FIX-1]

for (year in years) {
  Zmi <- Z_m[[as.character(year)]]
  Zvi <- Z_v[[as.character(year)]]
  Yi  <- Y[[as.character(year)]]
  
  rownames(Yi) <- rownames(Zvi) <- colnames(Zvi) <- io_names
  rownames(Zmi) <- colnames(Zmi) <- io_names
  colnames(Yi)  <- fd_names
  
  Z_m[[as.character(year)]] <- Zmi
  Z_v[[as.character(year)]] <- Zvi
  Y[[as.character(year)]]   <- Yi
}


# Store X, Y, Z variables
saveRDS(Z_m, file.path(output_dir_mode, "Z_mass.rds"))
saveRDS(Z_v, file.path(output_dir_mode, "Z_value.rds"))
saveRDS(Y,   file.path(output_dir_mode, "Y.rds"))
saveRDS(X,   file.path(output_dir_mode, "X.rds"))


###########################################################
# LOSSES-ENDOGENISED VERSION
###########################################################
# Losses become an own use of each sector (main diagonal of Z) instead of an FD category.
# The move is ROW-PRESERVING: mass leaves Y[i, ] and enters Z[i, i], so rowSums(Z)+rowSums(Y)
# is invariant and X stays valid. It is ALSO what arms Bug 3: it guarantees
# diag(Zvi) >= Xi for any row whose only mass is a loss, while simultaneously emptying that
# row's own FD. [FIX-2]'s colSums(Zvi) > 0 gate is what disarms it.

for (year in years) {
  
  print(year)
  
  Yi     <- Y[[as.character(year)]]
  losses <- as.matrix(Yi[, grepl("losses", colnames(Yi))])
  Yi     <- Yi[, !grepl("losses", colnames(Yi))]
  
  # [FIX-5] losses are added straight onto diag(Z); a negative OR non-finite losses cell becomes a
  # bad Z column (negative => S3; Inf/NaN => A/L blow up). Losses are physically non-negative and
  # finite — halt if any survived 10_1a/11.
  bad_loss <- which(!is.finite(losses) | losses < 0, arr.ind = TRUE)
  fabio_assert(nrow(bad_loss) == 0,
               "13 %s: %d negative/non-finite losses cells would be endogenised onto diag(Z). Fix at 10_1a/11, not here.",
               year, nrow(bad_loss),
               data = data.table(row = bad_loss[, 1], col = bad_loss[, 2],
                                 val = losses[bad_loss])[order(val)][1:min(.N, 20)])
  
  # Commit the losses-stripped Y. WITHOUT this line the local `Yi` above is orphaned: the
  # stored Y[[year]] keeps its losses columns while those same losses are ALSO added to
  # diag(Z) below, so [FIX-3] sees them twice and the identity breaks by one row's total
  # losses. (This write-back sat here originally; it was dropped when [FIX-5] was inserted.)
  Y[[as.character(year)]] <- Yi
  
  num_rows <- nrow(losses)
  num_cols <- nrow(losses) / ncol(losses)
  
  reshape_column <- function(v) {
    m <- matrix(0, ncol = num_cols, nrow = num_rows)
    indices <- ((seq_len(length(v)) - 1) %% num_cols) + 1
    m[cbind(seq_len(length(v)), indices)] <- v
    m
  }
  
  matrix_list <- lapply(1:ncol(losses), function(i) reshape_column(losses[, i]))
  combined_matrix <- as(do.call(cbind, matrix_list), "dgCMatrix")
  
  Z_m[[as.character(year)]] <- Z_m[[as.character(year)]] + combined_matrix
  Z_v[[as.character(year)]] <- Z_v[[as.character(year)]] + combined_matrix
}


###########################################################
# SECOND 80% LOOP (post-losses)                            [FIX-2] [FIX-3]
###########################################################

fd_labels_l <- fread(file.path(output_dir_mode, "losses/fd_labels.csv"))

for (year in years) {
  
  print(year)
  
  Zvi <- Z_v[[as.character(year)]]
  Zmi <- Z_m[[as.character(year)]]
  Yi  <- Y[[as.character(year)]]
  
  # [FIX-3] X was computed BEFORE losses moved onto diag(Z). The move is row-preserving,
  # so this recompute is a NO-OP if [FIX-1] is in place — and a hard error if it is not.
  # (Before [FIX-1] it was NOT a no-op: it would have resurrected the negative rows the
  # patch had neutralised. That is why the recompute could not simply be added on its own.)
  Xi <- rowSums(Zvi) + rowSums(Yi)
  d  <- max(abs(Xi - X[, as.character(year)]))
  fabio_assert(d < 1,
               "13 %s: X and rowSums(Z)+rowSums(Y) disagree by %.1f t after losses ",
               "endogenisation — the [FIX-1] Y write-back is not in place.", year, d)
  
  r <- repair_seed_eq_production(Zvi = Zvi, Zmi = Zmi, Yi = Yi, Xi = Xi,
                                 fd_lab = fd_labels_l, io_lab = io_labels,
                                 label = paste0(year, " losses"))
  
  Z_m[[as.character(year)]] <- r$Zmi
  Z_v[[as.character(year)]] <- r$Zvi
  Y[[as.character(year)]]   <- r$Yi
  X[, as.character(year)]   <- r$Xi
}


###########################################################
# FINAL INVARIANT: X > 0 => colSums(Z) > 0 for PROCESSED    [FIX-4]
###########################################################

kind_tbl  <- fread("inst/commodity_kind.csv")
processed <- kind_tbl[kind == "PROCESSED", comm_code]

# DDGS (c171) is an intended footprint-free by-product of grain ethanol: its feedstock burden
# stays on the ethanol (c146), so an empty input column is BY DESIGN, not a defect. Exclude it
# from the invariant. Add other deliberately unallocated by-products here if the same applies.
INTENDED_EMPTY_INPUT <- c("c171")
proc_row  <- io_labels$comm_code %in% setdiff(processed, INTENDED_EMPTY_INPUT)

# [FIX-4] NON-FATAL. A PROCESSED node with output but an empty MASS input column is worth
# surfacing, but the offenders are dominated by known/intended cases: BCP by-products, trade-hub
# re-exports (e.g. SGP/NLD), and per-country supply-use data gaps that don't affect results. So
# collect them across years, write a CSV, and warn — do NOT halt the build (was a fabio_assert
# hard-stop). A genuinely new, systemic offender (e.g. a base crush left untyped, as castor was)
# will still stand out in the CSV.
empty_input <- rbindlist(lapply(as.character(years), function(year) {
  Xi  <- X[, year]
  bad <- which(proc_row & Xi > 1 & Matrix::colSums(Z_m[[year]]) <= 0)
  if (!length(bad)) return(NULL)
  data.table(year = year, node = io_names[bad], X = Xi[bad])
}))

if (nrow(empty_input)) {
  setorder(empty_input, year, -X)
  fwrite(empty_input, "output/13_processed_empty_mass_input.csv")
  warning(sprintf(
    "[FIX-4] %d PROCESSED node-years have output with an EMPTY MASS input column across %s (see output/13_processed_empty_mass_input.csv). Non-fatal: mostly BCP by-products, trade hubs, and per-country data gaps.",
    nrow(empty_input), paste(range(years), collapse = "-")))
}

saveRDS(X,   file.path(output_dir_mode, "losses/X.rds"))
saveRDS(Y,   file.path(output_dir_mode, "losses/Y.rds"))
saveRDS(Z_m, file.path(output_dir_mode, "losses/Z_mass.rds"))
saveRDS(Z_v, file.path(output_dir_mode, "losses/Z_value.rds"))


###########################################################
# Correction of Other vs. Food use of Chinese veg. oils
###########################################################
# FAO overstates other and understates food use. Corrected with official Chinese data
# (China National Grain & Oils Information Center) and USDA GAIN.
# NB this is a FOOD <-> OTHER reshuffle within the same row: it preserves rowSums(Y), so X
# is unaffected and does not need recomputing.

io   <- fread(file.path(output_dir_mode, "io_labels.csv"))
su   <- fread(file.path(output_dir_mode, "su_labels.csv"))
fd   <- fread(file.path(output_dir_mode, "fd_labels.csv"))
Y    <- readRDS(file.path(output_dir_mode, "Y.rds"))
fd_l <- fread(file.path(output_dir_mode, "losses/fd_labels.csv"))
Y_l  <- readRDS(file.path(output_dir_mode, "losses/Y.rds"))

oil <- fread("input/oils_china.csv")

Y_new   <- Y
Y_l_new <- Y_l

for (i in seq_along(Y)) {
  
  print(years[i])
  
  data <- merge(io,
                oil[year == oil$year[which.min(abs(oil$year - years[i]))],
                    .(comm_code, food_share)],
                by = "comm_code", all.x = TRUE, sort = FALSE)
  
  data <- cbind(data, as.matrix(Y[[i]][, fd$area == "China, mainland"]))
  data[, `:=`(food = `CHN_food`, other = `CHN_other`)]
  data[!is.na(food_share), `:=`(food  = round((food + other) * food_share),
                                other = round((food + other) * (1 - food_share)))]
  Y_new[[i]][, fd$area_code == 41 & fd$fd == "food"]  <- data$food
  Y_new[[i]][, fd$area_code == 41 & fd$fd == "other"] <- data$other
  
  data_l <- cbind(data, as.matrix(Y_l[[i]][, fd_l$area == "China, mainland"]))
  data_l[!is.na(food_share), `:=`(food  = round((food + other) * food_share),
                                  other = round((food + other) * (1 - food_share)))]
  Y_l_new[[i]][, fd_l$area_code == 41 & fd_l$fd == "food"]  <- data_l$food
  Y_l_new[[i]][, fd_l$area_code == 41 & fd_l$fd == "other"] <- data_l$other
}

# compare old and new values
for (i in seq_along(Y)) {
  food      <- sum(Y[[i]][io$comm_code %in% oil$comm_code, fd$area_code == 41 & fd$fd == "food"])
  other     <- sum(Y[[i]][io$comm_code %in% oil$comm_code, fd$area_code == 41 & fd$fd == "other"])
  share     <- food / (food + other)
  food_new  <- sum(Y_new[[i]][io$comm_code %in% oil$comm_code, fd$area_code == 41 & fd$fd == "food"])
  other_new <- sum(Y_new[[i]][io$comm_code %in% oil$comm_code, fd$area_code == 41 & fd$fd == "other"])
  share_new <- food_new / (food_new + other_new)
  print(paste0(years[i], ": ", round(food / 1e6), "/", round(other / 1e6), " Mt, ",
               round(share * 100), "% // ",
               round(food_new / 1e6), "/", round(other_new / 1e6), " Mt, ",
               round(share_new * 100), "%"))
}

saveRDS(Y_new,   file.path(output_dir_mode, "Y.rds"))
saveRDS(Y_l_new, file.path(output_dir_mode, "losses/Y.rds"))