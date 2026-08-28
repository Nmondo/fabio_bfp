##############################################################################################
######################################  LOADING PACKAGES ######################################
##############################################################################################

library(data.table)
library(tidyverse)

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

source("R/00_system_variables.R")
source("R/01_tidy_functions.R")
source("R/00_prep_functions.R")

# ---- CAPPING VARIANT SWITCH --------------------------------------------------
# FABIO_VARIANT = "" (baseline) or "capped": compile the parallel extension tree
# data/extensions_capped/ into E_capped.rds, leaving E.rds untouched. ex_labels
# is identical across runs (capping preserves the stressor row set), so it is not
# suffixed.
VARIANT  <- tolower(trimws(Sys.getenv("FABIO_VARIANT", unset = "")))
vsuf     <- if (nzchar(VARIANT)) paste0("_", VARIANT) else ""
EXT_ROOT <- paste0("data/extensions", vsuf)
E_file   <- paste0("E", vsuf, ".rds")
message(sprintf(">>> [16] variant = '%s'  (ext tree: %s | E file: %s)",
                if (nzchar(VARIANT)) VARIANT else "baseline", EXT_ROOT, E_file))

years <- 2011:2023


##############################################################################################
######################################  LOADING DATA ######################################
##############################################################################################

items_v525 <- fread(file.path(output_dir_v525, "items.csv"))
items_v123 <- read_csv("inst/items_full_123.csv")
items_bcp <- read_csv("inst/items_full_bcp.csv")
io_labels_bcp <- fread(file.path(output_dir_bcp, "io_labels.csv"))
areas_bcp <- unique(io_labels_bcp$iso3c)

# E_label checks -----------
E_labels <- fread("inst/E_labels_initial.csv")
E_fd_labels <- fread("inst/E_fd_labels_initial.csv")

# Prepping E_labels --------------
# Check that the same files are present in sua and cbs folders

nms_cbs    <- gsub(".rds", "", list.files(file.path(EXT_ROOT, "cbs"),    pattern = "\\.rds$"))
nms_sua    <- gsub(".rds", "", list.files(file.path(EXT_ROOT, "sua"),    pattern = "\\.rds$"))
nms_fd_cbs <- gsub(".rds", "", list.files(file.path(EXT_ROOT, "fd_cbs"), pattern = "\\.rds$"))
nms_fd_sua <- gsub(".rds", "", list.files(file.path(EXT_ROOT, "fd_sua"), pattern = "\\.rds$"))

pairs <- list(
  list(a = nms_cbs,    b = nms_sua,    name_a = "cbs",    name_b = "sua"),
  list(a = nms_fd_cbs, b = nms_fd_sua, name_a = "fd_cbs", name_b = "fd_sua")
)

msg <- ""
for (p in pairs) {
  if (!identical(p$a, p$b)) {
    only_in_a <- setdiff(p$a, p$b)
    only_in_b <- setdiff(p$b, p$a)
    msg <- paste0(msg, "File lists are NOT identical between '", p$name_a, "' and '", p$name_b, "':\n")
    if (length(only_in_a) > 0) msg <- paste0(msg,"  Only in ", p$name_a, ": ", paste(only_in_a, collapse = ", "), "\n")
    if (length(only_in_b) > 0) msg <- paste0(msg,"  Only in ", p$name_b, ": ", paste(only_in_b, collapse = ", "), "\n")
  }
}
if (nchar(msg) > 0) stop(msg) else message("All file lists are identical within pairs.")

nms <- copy(nms_sua)
nms_fd <- copy(nms_fd_sua)
rm(nms_cbs, nms_sua, nms_fd_sua, nms_fd_cbs, p, pairs, msg)

# check if E_labels is fully updated
# Compare against the file lists BEFORE any filtering: compile_extension() below
# takes its rownames straight from list.files(), so an unlabelled extension file
# would otherwise add a row to E without adding one to ex_labels.csv, silently
# offsetting the two.
missing_labels    <- setdiff(nms,    E_labels$Stressor)
missing_labels_fd <- setdiff(nms_fd, E_fd_labels$Stressor)

if (length(missing_labels) > 0)    stop("E_labels needs to be updated -> ", paste(missing_labels,    collapse = ", "), " is present in extension folder but not in E_labels")
if (length(missing_labels_fd) > 0) stop("E_fd_labels needs to be updated -> ", paste(missing_labels_fd, collapse = ", "), " is present in fd extension folder but not in E_fd_labels")

# exclude possible extra lines in E_labels
E_labels <- E_labels[Stressor %in% nms]
E_fd_labels <- E_fd_labels[Stressor %in% nms_fd]

# sort labels in the same order as the files
E_labels <- E_labels[order(match(E_labels$Stressor, nms)), ]
E_fd_labels <- E_fd_labels[order(match(E_fd_labels$Stressor, nms_fd)), ]

# check that the ordering worked
if (!all(E_labels$Stressor == nms)) stop("Re-do column ordering for E_labels to match files")
if (!all(E_fd_labels$Stressor == nms_fd)) stop("Re-do column ordering for E_fd_labels to match files")

rm(missing_labels, missing_labels_fd)

# Compile all extensions -----------------
files_cbs <- list.files(file.path(EXT_ROOT, "cbs"), pattern = "\\.rds$", full.names = TRUE)
data_cbs <- lapply(files_cbs, readRDS)

files_fd_cbs <- list.files(file.path(EXT_ROOT, "fd_cbs"), pattern = "\\.rds$", full.names = TRUE)
data_fd_cbs <- lapply(files_fd_cbs, readRDS)

files_sua <- list.files(file.path(EXT_ROOT, "sua"), pattern = "\\.rds$", full.names = TRUE)
data_sua <- lapply(files_sua, readRDS)

files_fd_sua <- list.files(file.path(EXT_ROOT, "fd_sua"), pattern = "\\.rds$", full.names = TRUE)
data_fd_sua <- lapply(files_fd_sua, readRDS)

# Combine
E_cbs <- compile_extension(data_cbs, files_cbs)  
E_fd_cbs <- compile_extension(data_fd_cbs, files_fd_cbs)  

E_sua <- compile_extension(data_sua, files_sua)  
E_fd_sua <- compile_extension(data_fd_sua, files_fd_sua)  

# Row order guard: compiled row order must equal the label order, since ex_labels.csv
# is written separately from E and nothing downstream re-checks the pairing.
if (!all(rownames(E_cbs[[as.character(years[1])]]) == E_labels$Stressor))
  stop("E_cbs row order does not match E_labels.")
if (!all(rownames(E_sua[[as.character(years[1])]]) == E_labels$Stressor))
  stop("E_sua row order does not match E_labels.")




##############################################################################################
######################################  EXTENSIONS FOR FABIO V2_BCP ##############################
##############################################################################################


##############################################################################################
#1. Column-suffix remapping (comm_code to v_bcp)
##############################################################################################

# Build item_code -> comm_code lookup tables for each source's commodity convention,
# then build a per-source map from "old comm_code" -> "BCP comm_code"
build_remap <- function(items_src, items_bcp) {
  # one item_code -> one comm_code per source (collapse if duplicates exist)
  src <- unique(as.data.table(items_src)[, .(item_code, comm_code_src = comm_code)])
  bcp <- unique(as.data.table(items_bcp)[, .(item_code, comm_code_bcp = comm_code)])
  merge(src, bcp, by = "item_code", all = FALSE)   # inner join: keep only items present in BCP
}

remap_v123 <- build_remap(items_v123, items_bcp)   # for E_cbs / E_fd_cbs
remap_v525 <- build_remap(items_v525, items_bcp)   # for E_sua / E_fd_sua

# Helper: rewrite "<area>_<old_comm>" -> "<area>_<new_comm>"; drop columns whose
# comm_code has no BCP counterpart.
remap_columns <- function(yr_mat, remap_dt) {
  cn <- colnames(yr_mat)
  if (is.null(cn)) return(yr_mat)
  area_part <- sub("_[^_]+$", "", cn)
  comm_part <- sub("^[^_]+_", "", cn)
  new_comm  <- remap_dt$comm_code_bcp[match(comm_part, remap_dt$comm_code_src)]
  keep <- !is.na(new_comm)
  out  <- yr_mat[, keep, drop = FALSE]
  colnames(out) <- paste0(area_part[keep], "_", new_comm[keep])
  out
}

remap_extension <- function(ext_list, remap_dt) {
  lapply(ext_list, remap_columns, remap_dt = remap_dt) |> setNames(names(ext_list))
}

# Apply remap so all four extension lists share the BCP comm_code suffix convention
E_cbs    <- remap_extension(E_cbs,    remap_v123)
E_fd_cbs <- remap_extension(E_fd_cbs, remap_v123)
E_sua    <- remap_extension(E_sua,    remap_v525)
E_fd_sua <- remap_extension(E_fd_sua, remap_v525)

##############################################################################################
#2. Bind CBS and SUA, filtering
##############################################################################################

bind_extensions <- function(ext_a, ext_b, comms_keep) {
  lapply(names(ext_a), function(yr) {
    a <- ext_a[[yr]]; b <- ext_b[[yr]]
    a <- a[, !colnames(a) %in% colnames(b), drop = FALSE]   # SUA wins on overlap
    bound <- cbind(a, b)
    cn <- colnames(bound); if (is.null(cn)) return(bound)
    comm_part <- sub("^[^_]+_", "", cn)
    bound[, comm_part %in% comms_keep, drop = FALSE]
  }) |> setNames(names(ext_a))
}

comms_bcp <- unique(io_labels_bcp$comm_code)
E_bcp     <- bind_extensions(E_cbs,    E_sua,    comms_bcp)
# E_fd_bcp  <- bind_extensions(E_fd_cbs, E_fd_sua, comms_bcp)

##############################################################################################
#3. Sanity checks
##############################################################################################

# Stressors must align for cbind
stopifnot(identical(rownames(E_cbs[["2012"]]), rownames(E_sua[["2012"]])))

# What BCP commodities still have no extension data after remap+bind?
covered <- unique(sub("^[^_]+_", "", colnames(E_bcp[["2012"]])))
cat("BCP comm_codes with no extension coverage:\n")
print(setdiff(comms_bcp, covered))




##############################################################################################
######################################  ZERO-FILL MISSING BCP COMMODITIES ###################
##############################################################################################

zero_fill_missing <- function(ext_list, comms_keep, areas_keep) {
  lapply(ext_list, function(yr_mat) {
    cn         <- colnames(yr_mat)
    covered    <- unique(sub("^[^_]+_", "", cn))
    missing    <- setdiff(comms_keep, covered)
    if (length(missing) == 0) return(yr_mat)
    
    # Build all (area, missing_comm) combinations -> column names
    new_cols <- as.vector(outer(areas_keep, missing, paste, sep = "_"))
    pad      <- matrix(0, nrow = nrow(yr_mat), ncol = length(new_cols),
                       dimnames = list(rownames(yr_mat), new_cols))
    cbind(yr_mat, pad)
  }) |> setNames(names(ext_list))
}

target_order <- paste0(io_labels_bcp$iso3c, "_", io_labels_bcp$comm_code)

reorder_cols <- function(ext_list, order_vec) {
  lapply(ext_list, function(yr_mat) {
    yr_mat[, order_vec, drop = FALSE]
  }) |> setNames(names(ext_list))
}


E_bcp    <- zero_fill_missing(E_bcp,    comms_bcp, areas_bcp)
E_bcp    <- reorder_cols(E_bcp,    target_order)



##############################################################################################
######################################  SAVE ################################################
##############################################################################################

saveRDS(E_bcp,    paste0(output_dir_bcp, E_file))
# saveRDS(E_fd_bcp, paste0(output_dir_bcp, "E_fd.rds"))
fwrite(E_labels,    paste0(output_dir_bcp, "ex_labels.csv"))   # identical across variants
# fwrite(E_fd_labels, paste0(output_dir_bcp, "ex_fd_labels.csv"))

# 
# # save
# saveRDS(E_cbs, paste0(output_dir, "E.rds"))
# saveRDS(E_fd_cbs, paste0(output_dir, "E_fd.rds"))
# 
# saveRDS(E_sua, paste0(output_dir_v525, "E.rds"))
# saveRDS(E_fd_sua, paste0(output_dir_v525, "E_fd.rds"))
# 
# fwrite(E_labels, paste0(output_dir, "ex_labels.csv"))
# fwrite(E_fd_labels, paste0(output_dir, "ex_fd_labels.csv"))
# 
# fwrite(E_labels, paste0(output_dir_v525, "ex_labels.csv"))
# fwrite(E_fd_labels, paste0(output_dir_v525, "ex_fd_labels.csv"))