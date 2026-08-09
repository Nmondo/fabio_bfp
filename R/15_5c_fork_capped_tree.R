# 15_5c - Fork a parallel CAPPED extension tree for side-by-side comparison
# ==============================================================================
# Run ONCE, after 15_5b (which capped the pressure files in place under
# data/extensions/{sua,cbs}/ and backed the originals up to
# data/extensions/{sua,cbs}_uncapped_backup/).
#
# It (1) copies the current extension folders -- which now hold the CAPPED
# pressures -- into a parallel tree data/extensions_capped/, and (2) restores the
# baseline data/extensions/{sua,cbs}/ pressures from backup so the uncapped
# baseline is coherent again. The impact files copied into the new tree are stale
# at this point; they are overwritten when you re-run 15_6/15_8/15_9 with
# FABIO_VARIANT=capped.
#
# CAPPED RUN (prefix every call with the env var):
#   FABIO_VARIANT=capped Rscript R/15_6_biodiversity_ibif.R
#   FABIO_VARIANT=capped Rscript R/15_8_biodiversity_lc_fd.R
#   FABIO_VARIANT=capped Rscript R/15_9_ecosystem_services.R
#   FABIO_VARIANT=capped Rscript R/16_extensions_main.R      # -> v2_bcp/E_capped.rds
#   FABIO_VARIANT=capped Rscript R/18_01b_footprints.R       # -> output_capped/
#   FABIO_VARIANT=capped Rscript R/18_02_sda.R               # -> output_capped/
# 15_7 is NOT re-run: its tidy CF outputs are identical across variants and are
# always read from the baseline data/extensions/tidy/.
# ==============================================================================

## --- portable repo root -------------------------------------------------------
fabio_root <- Sys.getenv("FABIO_BFP_ROOT", unset = "")
if (!nzchar(fabio_root)) {
  fabio_root <- getwd()
  while (!file.exists(file.path(fabio_root, "R", "00_system_variables.R")) &&
         dirname(fabio_root) != fabio_root) fabio_root <- dirname(fabio_root)
  if (!file.exists(file.path(fabio_root, "R", "00_system_variables.R")))
    stop("Repo root not found above ", getwd(), " - set FABIO_BFP_ROOT.")
}
setwd(fabio_root)

# ---- config ------------------------------------------------------------------
RESTORE_BASELINE <- TRUE    # restore data/extensions/{sua,cbs} pressures from backup
FORCE            <- FALSE   # TRUE = overwrite an existing data/extensions_capped/

SRC  <- "data/extensions"
DST  <- "data/extensions_capped"
SUBS <- c("sua", "cbs", "fd_sua", "fd_cbs")   # tidy/ deliberately excluded (shared CFs)

if (dir.exists(DST) && !FORCE)
  stop(DST, " already exists. Set FORCE <- TRUE to overwrite, or remove it first.")

# 1. Fork the capped tree (current folders hold the capped pressures) ----------
dir.create(DST, showWarnings = FALSE, recursive = TRUE)
for (sub in SUBS) {
  from <- file.path(SRC, sub)
  to   <- file.path(DST, sub)
  if (!dir.exists(from)) { warning("Missing source dir: ", from, " -- skipped."); next }
  if (dir.exists(to) && FORCE) unlink(to, recursive = TRUE)
  dir.create(to, showWarnings = FALSE, recursive = TRUE)
  fs <- list.files(from, pattern = "\\.rds$", full.names = TRUE)
  ok <- file.copy(fs, to, overwrite = TRUE)
  message(sprintf("  copied %3d/%3d .rds  %s -> %s", sum(ok), length(fs), from, to))
}

# 2. Restore the uncapped baseline pressures from 15_5b's backup ---------------
# (impact files were not touched by 15_5b, so they are already the correct
#  uncapped-derived impacts and need no restore.)
if (RESTORE_BASELINE) {
  for (sub in c("sua", "cbs")) {
    bak <- file.path(SRC, paste0(sub, "_uncapped_backup"))
    if (!dir.exists(bak)) {
      warning("No backup dir ", bak, " -- baseline pressures NOT restored for ", sub); next
    }
    fs <- list.files(bak, pattern = "\\.rds$", full.names = TRUE)
    ok <- file.copy(fs, file.path(SRC, sub), overwrite = TRUE)
    message(sprintf("  restored %3d/%3d baseline pressures -> %s",
                    sum(ok), length(fs), file.path(SRC, sub)))
  }
}

message("\nFork complete.")
message("  baseline tree: ", SRC,  "         -> E.rds        -> output/")
message("  capped   tree: ", DST, "  -> E_capped.rds -> output_capped/")
message("Next: run 15_6, 15_8, 15_9, 16, 18_01b, 18_02 with FABIO_VARIANT=capped.")
