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
# 0_prep ------------------------------------------------------------------

# Will download ZIP files from FAOSTAT
source("R/00_1_prep_fao.R")
source("R/00_2_prep_fao_reshape.R")

# Requires downloading BACI ZIP files available from:
# https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37
# source("R/00_3_prep_trade.R")

# Requires EIA and IEA CSV files available from:
# https://www.eia.gov/opendata/qb.php?category=2135203 (in 1000 bbl/d)
# http://dx.doi.org/10.1787/data-00550-en
# source("R/00_4_prep_eth.R")

# Will download ZIP files from FAOSTAT
# source("R/00_5_prep_fish.R")
source("R/00_6_labels.R")
rm(list = ls()); gc()

# Requires downloading some data (see script)
# source("R/00_7_prep_spatial_NPK.R")
rm(list = ls()); gc()


# 1_tidy ------------------------------------------------------------------
source("R/01_1_tidy_fao.R")
source("R/01_2_tidy_trade.R")
# source("R/01_3_tidy_eth.R")
# source("R/01_4_tidy_fish.R")
rm(list = ls()); gc()


# Build full BTD ---------------------------------------------------------
source("R/02_build_btd.R"); rm(list = ls()); gc()

# Build full CBS ---------------------------------------------------------
source("R/03_1a_build_cbs.R"); rm(list = ls()); gc()
source("R/03_1b_balance_cbs.R"); rm(list = ls()); gc()

# Build full SUA ---------------------------------------------------------
source("R/03_2a_build_tcf_sua.R"); rm(list = ls()); gc()
source("R/03_2b_build_sua.R"); rm(list = ls()); gc()

# Estimate BTD from totals -----------------------------------------------
source("R/04_estimate_btd.R"); rm(list = ls()); gc()

# Balance trade using RAS ------------------------------------------------
source("R/05_balance_btd.R"); rm(list = ls()); gc()

# Allocate re-exports ----------------------------------------------------
source("R/06_re-exports.R"); rm(list = ls()); gc()

# Create biofuels and biopolymers supply and use tables ----------------------------------------------------
source("R/07_01_sup_y_cleaning.R"); rm(list = ls()); gc()
source("R/07_02_use_cleaning.R"); rm(list = ls()); gc()
source("R/07_03a_sup_use_biogasoline.R"); rm(list = ls()); gc()
source("R/07_03b_sup_use_biodiesel.R"); rm(list = ls()); gc()
source("R/07_04_btd_cleaning.R"); rm(list = ls()); gc()
source("R/07_05_y_cleaning.R"); rm(list = ls()); gc()
source("R/07_06_balancing_bf.R"); rm(list = ls()); gc()
source("R/07_07_use_final.R"); rm(list = ls()); gc()
source("R/07_08_sup_use_bp.R"); rm(list = ls()); gc()
source("R/07_09_use_bf_coproducts.R"); rm(list = ls()); gc()
source("R/07_10_compile_bcp.R"); rm(list = ls()); gc()

# Adapting CBS to match biofuels and biopolymers requirements ----------------------------------------------------
source("R/08_01_cbs_missing_countries.R"); rm(list = ls()); gc()
source("R/08_02_bcp_use_first_rebalancing.R"); rm(list = ls()); gc()
source("R/08_03_rescale_bcp_use_empirical.R"); rm(list = ls()); gc()
source("R/08_04_bcp_use_cbs_final_balancing.R"); rm(list = ls()); gc()


# Creating supply table for CBS ----------------------------------------------------
source("R/09_1_supply_cbs.R"); rm(list = ls()); gc()

# Creating use table for CBS ----------------------------------------------------
source("R/10_1a_use_cbs.R"); rm(list = ls()); gc()

# Merging CBS and BPC tables ----------------------------------------------------
source("R/11_merge_all.R"); rm(list = ls()); gc()

# Making SUTs (and updating labels accordingly) ----------------------------------------------------
source("R/12_a_mrsut.R"); 
source("R/12_b_update_labels.R"); rm(list = ls()); gc()

# Making IOTs ----------------------------------------------------
source("R/13_mrio.R"); rm(list = ls()); gc()

# Computing Leontief inverse ----------------------------------------------------
source("R/14_leontief_inverse.R"); rm(list = ls()); gc()








