# =============================================================================
# 00_run_config.R  -  Rescaled-vs-bypass run mode for the FABIO-BCP pipeline
# -----------------------------------------------------------------------------
# ONE switch controls whether 08_04 -> 14 consume the 08_03 RED-rescaled inputs
# ("rescaled") or the pre-rescale 08_02 inputs ("bypass"). The two runs write to
# parallel files / directories so they never clobber each other and can be opened
# side by side in 20_01.
#
# The mode is carried in an ENVIRONMENT VARIABLE so it survives the `rm(list=ls())`
# that several pipeline scripts run at their top. Each pipeline script sources THIS
# file *after* its rm()/setwd(), which re-derives the helpers in the fresh session.
#
# HOW TO SET THE MODE
#   - one interactive R session:   Sys.setenv(FABIO_RUN_MODE = "bypass")  # then source 08_04..14
#   - per Rscript process:         FABIO_RUN_MODE=bypass Rscript R/08_04_....R
#   - unset / "rescaled"  => default behaviour, identical filenames to before.
#
# WHAT GETS PARALLELISED
#   - data/*.rds intermediate + merged tables  -> suffixed via tag()      ("_noresc")
#   - the v2_bcp MRIO output directory (12-14)  -> sub-dir via mode_dir()  ("bypass/")
# =============================================================================

# --- the switch --------------------------------------------------------------
if (!exists("RUN_MODE") || !nzchar(RUN_MODE))
  RUN_MODE <- Sys.getenv("FABIO_RUN_MODE", unset = "rescaled")
RUN_MODE <- tolower(trimws(RUN_MODE))
if (!RUN_MODE %in% c("rescaled", "bypass"))
  stop("FABIO_RUN_MODE must be 'rescaled' or 'bypass', got: ", RUN_MODE)

BYPASS_RESCALE <- identical(RUN_MODE, "bypass")

# Modelling choice for the bypass counterfactual (see 11_merge_all.R):
#   TRUE  => keep the RED-driven c901 waste_flows from 08_03 even in bypass
#   FALSE => true "no-rescale" baseline: drop them, c901 self-sourced from the
#            pre-rescale use table only. (default)
if (!exists("BYPASS_KEEP_WASTE")) BYPASS_KEEP_WASTE <- FALSE

# --- filename helper for data/ tables ---------------------------------------
# inserts the mode suffix before the .rds extension:
#   tag("data/sup.rds")  ->  "data/sup.rds"          (rescaled)
#                            "data/sup_noresc.rds"   (bypass)
mode_tag <- if (BYPASS_RESCALE) "_noresc" else ""
tag <- function(path) sub("\\.rds$", paste0(mode_tag, ".rds"), path, ignore.case = TRUE)

# --- output-directory helper for the 12-14 MRIO artefacts -------------------
# rescaled -> the base dir unchanged; bypass -> a `bypass/` sub-dir of it.
# Creates the dir (and its losses/ child) so the first writer never fails.
mode_dir <- function(base) {
  d <- if (BYPASS_RESCALE) file.path(sub("/+$", "", base), "bypass") else base
  dir.create(file.path(d, "losses"), recursive = TRUE, showWarnings = FALSE)
  d
}

message(sprintf(">>> [run_config] RUN_MODE = '%s'  (data tag = '%s'%s)",
                RUN_MODE, mode_tag,
                if (BYPASS_RESCALE) sprintf(", BYPASS_KEEP_WASTE = %s", BYPASS_KEEP_WASTE) else ""))