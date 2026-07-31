# 20_02 - Make SDA plots (executes the functions defined in 19_02_plot_function_sda.R)
# ------------------------------------------------------------------------------
# Run 18_02_sda.R first to produce the input CSVs this script's sourced file
# reads. This script is self-contained from there: it sources the SDA
# plot-function definitions itself, so it can be run on its own (no need to
# have already run 19_02 in this session).
# NOTE: 19_02_plot_function_sda.R does not source 19_plot_definitions.R (it
# defines its own fuel/feedstock colour vocabulary), so it is not sourced here
# either - unlike the 01a/01b make-plot scripts.
# ------------------------------------------------------------------------------

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

source("R/19_02_plot_function_sda.R")

######################################################################################################################
#2 / 2b usage - SDA time series for top countries / continents
######################################################################################################################

########### Plotting for various extensions

plot_SDA_chain(2012, 2022, "ibif_total")
# plot_SDA_chain(2012, 2022, "LCIM_EQ_terrestrial")

plot_SDA_chain_continent(2012, 2022, "ibif_total",
                         continents = c("EU", "ASI", "LAM", "NAM"))

# plot_SDA_chain_continent(2012, 2022, "ibif_total",
#                          continents     = character(0),
#                          include_global = TRUE)

######################################################################################################################
#3 usage - SDA chained driver decomposition by consumer continent
######################################################################################################################

# --- Usage -----------------------------------------------------------------
plot_SDA_chained_drivers("ibif_total")

######################################################################################################################
#3b usage - SDA chained driver decomposition summed across one consumer continent
######################################################################################################################

# --- Usage -----------------------------------------------------------------
plot_SDA_chained_drivers_continent("ibif_total", consumer_continent = "EU")