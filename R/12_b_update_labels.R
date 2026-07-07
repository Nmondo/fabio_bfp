# libraries first, so this runs as a standalone `Rscript` (read_csv used below is
# from readr/tidyverse and would otherwise not be loaded yet).
suppressPackageStartupMessages({
  library(data.table)
  library(tidyverse)
})

###########################################################
########### LOADING INITIAL LABELS - TO COPY THOSE UNCHANGED #######################
###########################################################

setwd("/mnt/nfs_fineprint/tmp/fabio/v2/")
ex_fd_labels <- read_csv("ex_fd_labels.csv")
ex_labels    <- read_csv("ex_labels.csv")
fd_labels    <- read_csv("fd_labels.csv")
io_labels    <- read_csv("io_labels.csv")
items        <- read_csv("items.csv")
regions      <- read_csv("regions.csv")
su_labels    <- read_csv("su_labels.csv")

# setwd("/mnt/nfs_fineprint/tmp/fabio/v2_bcp/")




###########################################################
########### MAKING UPDATED LABELS #######################
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
setwd(fabio_root)

library(data.table)
library(tidyverse)
source("R/00_system_variables.R")
source("R/01_tidy_functions.R")
source("R/00_run_config.R")                 # RUN_MODE / tag() / mode_dir()
output_dir_mode <- mode_dir(output_dir_bcp) # rescaled -> base dir; bypass -> base/bypass/

# Read dims emitted by 12_mrsut.R -------------------------------------------
dims <- readRDS(file.path(output_dir_mode, "mrsut_dims_bcp.rds"))
areas_vec       <- dims$areas
processes_vec   <- dims$processes
commodities_vec <- dims$commodities
fd              <- dims$fd_vars

# Regions --------------------------------------------------------------------
regions_full <- fread("inst/regions_full.csv")
regions <- regions_full[code %in% areas_vec,
                        .(iso3c, area_code = code, area = name,
                          continent, region, EU27)]
# keep the same order mrsut used
regions <- regions[match(areas_vec, area_code)]
nrreg <- nrow(regions)

# Items ----------------------------------------------------------------------
items_ref <- fread("inst/items_full_bcp.csv")
items <- items_ref[comm_code %in% commodities_vec]
items <- items[match(commodities_vec, comm_code)]   # enforce mrsut order
stopifnot(!anyNA(items$comm_code))                  # catches missing metadata
nrcom <- nrow(items)

# Processes ------------------------------------------------------------------
use <- readRDS(tag("data/use_final_merged.rds"))
sup <- readRDS(tag("data/sup_final_merged.rds"))
sup <- setDT(sup)
proc_ref <- unique(rbindlist(list(
  use[, .(proc_code, proc)],
  sup[, .(proc_code, proc)]
)))
processes <- proc_ref[match(processes_vec, proc_code)]
stopifnot(!anyNA(processes$proc))
nrproc <- nrow(processes)

# FD -------------------------------------------------------------------------
nrfd <- length(fd)

# Build labels ---------------------------------------------------------------
io_labels <- data.table(
  iso3c      = rep(regions$iso3c,     each = nrcom),
  area_code  = rep(regions$area_code, each = nrcom),
  area       = rep(regions$area,      each = nrcom),
  continent  = rep(regions$continent, each = nrcom),
  comm_code  = rep(items$comm_code,  nrreg),
  item_code  = rep(items$item_code,  nrreg),
  item       = rep(items$item,       nrreg),
  unit       = rep(items$unit,       nrreg),
  comm_group = rep(items$comm_group, nrreg),
  group      = rep(items$group,      nrreg))

su_labels <- data.table(
  iso3c     = rep(regions$iso3c,     each = nrproc),
  area_code = rep(regions$area_code, each = nrproc),
  area      = rep(regions$area,      each = nrproc),
  continent = rep(regions$continent, each = nrproc),
  proc_code = rep(processes$proc_code, nrreg),
  proc      = rep(processes$proc,      nrreg))

fd_labels <- data.table(
  iso3c     = rep(regions$iso3c,     each = nrfd),
  area_code = rep(regions$area_code, each = nrfd),
  area      = rep(regions$area,      each = nrfd),
  continent = rep(regions$continent, each = nrfd),
  fd        = rep(fd, nrreg))

# Sanity-check dims against the actual mrsut outputs ------------------------
mr_use    <- readRDS(file.path(output_dir_mode, "mr_use.rds"))
mr_use_fd <- readRDS(file.path(output_dir_mode, "mr_use_fd.rds"))
stopifnot(nrow(io_labels) == nrow(mr_use[[1]]))
stopifnot(nrow(su_labels) == ncol(mr_use[[1]]))
stopifnot(nrow(fd_labels) == ncol(mr_use_fd[[1]]))




###########################################################
########### WRITING UPDATED LABELS #######################
###########################################################

dir.create(file.path(output_dir_mode, "losses"),
           recursive = TRUE, showWarnings = FALSE)

fwrite(io_labels, file = file.path(output_dir_mode, "io_labels.csv"))
fwrite(su_labels, file = file.path(output_dir_mode, "su_labels.csv"))
fwrite(fd_labels, file = file.path(output_dir_mode, "fd_labels.csv"))
fwrite(fd_labels[!fd %in% "losses"],
       file = file.path(output_dir_mode, "losses/fd_labels.csv"))
fwrite(items[, .(comm_code, item_code, item, unit, group, comm_group)],
       file = file.path(output_dir_mode, "items.csv"))
fwrite(regions, file = file.path(output_dir_mode, "regions.csv"))

# Copying the unchanged ones 
file.copy(
  from      = file.path("/mnt/nfs_fineprint/tmp/fabio/v2/",
                        c("ex_fd_labels.csv", "ex_labels.csv")),
  to        = output_dir_mode,
  overwrite = TRUE
)

# Setting the WD back to own Git repository
setwd(fabio_root)