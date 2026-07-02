#!/usr/bin/env bash
# =============================================================================
# run_compare.sh  -  run the FABIO-BCP tail (08_04 -> 14) in BOTH run modes.
#                    20_01 (the rescaled-vs-bypass compare) is left for you to run
#                    separately once both modes are on disk.
# -----------------------------------------------------------------------------
# Each step is a fresh `Rscript` process on purpose: several of these scripts
# call rm(list=ls()) at the top, so a single long-lived R session would lose the
# run-mode object. The mode travels in the FABIO_RUN_MODE env var, which every
# script re-reads via R/00_run_config.R after its own rm()/setwd(). A fresh
# process per step is also the most faithful reproduction of the manual workflow.
#
# Usage:
#   ./run_compare.sh                 # runs both modes (rescaled then bypass), then 20_01
#   ./run_compare.sh bypass          # runs ONLY the bypass mode (e.g. rescaled already exists)
#   ./run_compare.sh rescaled bypass # explicit
#
# Prerequisites: the pre-rescale (08_02) and rescale (08_03) outputs must already
# exist on disk, plus the static *_bcp tables. This script only runs 08_04+.
# =============================================================================
set -euo pipefail

REPO="/home/mmondolfo/fabio_bfp"
cd "$REPO"

# the mode-dependent chain that produces the final MRIO tables, in order.
# (09_2 / 10_2 are the SUA branch: mode-independent, not consumed by 11 -> not here.)
CHAIN=(
  R/08_04_bcp_use_cbs_final_balancing.R
  R/09_1_supply_cbs.R
  R/10_1a_use_cbs.R
  R/11_merge_all.R
  R/12_a_mrsut.R
  R/12_b_update_labels.R
  R/13_mrio.R
  R/14_leontief_inverse.R
)

MODES=("${@:-rescaled bypass}")
# normalise: if no args, the line above gives a single "rescaled bypass" token; split it.
if [[ $# -eq 0 ]]; then MODES=(rescaled bypass); fi

for MODE in "${MODES[@]}"; do
  echo "============================================================"
  echo ">>> RUN MODE = ${MODE}"
  echo "============================================================"
  for SCRIPT in "${CHAIN[@]}"; do
    echo "--- [${MODE}] ${SCRIPT}"
    FABIO_RUN_MODE="${MODE}" Rscript "${SCRIPT}"
  done
done

echo
echo ">>> pipeline done for: ${MODES[*]}"
