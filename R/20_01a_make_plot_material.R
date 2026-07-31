# 20a - Make MATERIAL-FLOW plots (executes the functions defined in 19_01a_plot_function_material.R)
# ------------------------------------------------------------------------------
# Run 18_01a_material_flows.R first to produce the input CSVs/RDS this script's
# sourced files read. This script is self-contained from there: it sources the
# shared plot definitions and the material plot-function definitions itself, so
# it can be run on its own (no need to have already run 19_01a in this session).
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

source("R/19_plot_definitions.R")
source("R/19_01a_plot_function_material.R")

######################################################################################################################
############################## PLOT RESULTS #########################################################################
######################################################################################################################

######################################################################################################################
#0. Plot descriptive statistics
######################################################################################################################

Y_global_plot <- plot_consumption(Y_summary, items_full_bcp, fuel_colors, regions = regions,
                                  y_lab        = "Biofuel consumption (M liters)",
                                  fill_lab     = "Fuel type",
                                  title        = "Global biofuel consumption by fuel type")
Y_global_BP <- plot_consumption(Y_summary_BP %>% filter(comm_code != "c146"), items_full_bcp, regions = regions,
                                y_lab          = "Consumption (kilotonnes)",
                                fill_lab       = "Commodity",
                                title          = "Global biopolymer and building block consumption by product")

## Saving
ggsave(filename = file.path("output", "plot", "Y_global_2012_2022_BF.svg"),
       plot = Y_global_plot,
       width = 10, height = 6, dpi = 300)

ggsave(filename = file.path("output", "plot", "Y_global_2012_2022_BP.svg"),
       plot = Y_global_BP,
       width = 10, height = 6, dpi = 300)


# Supply by region and feedstocks.
p <- plot_feedstock_desc(Z_per_year,
                         c("c146","c147","c149"),
                         c("EU","ASI","NAM","LAM"),
                         scale        = 1e3,
                         accuracy     = 1,
                         y_lab        = "Biofuel production (B liters)")  # default tcf_on_missing = "drop"

ggsave(
  filename = file.path("output", "plot", "desc_embodied_feedstock_BF_ASI-EU-NAM.svg"),
  plot = p,
  width = 10, height = 8, dpi = 300)

# Biofuel consumption, disaggregated
p_country <- plot_country_consumption(
  Y_summary, regions,
  comm_in      = c("c146", "c147", "c149"),
  continent_in = c("EU", "ASI", "NAM", "LAM"),
  n_top        = 10,
  share_thresh = 0.10,
  scale        = 1e6,
  accuracy     = 1,
  y_lab        = "Biofuel consumption (B liters)")

ggsave(file.path("output", "plot", "consumption_by_country_BF_EU-ASI-NAM-LAM.svg"),
       p_country, width = 12, height = 8, dpi = 300)


######################################################################################################################
#1. Plot sourcing decomposition
######################################################################################################################

# --- (1) feedstock embodied: production vs consumption sourcing, two periods ---
p_feed_early <- plot_sourcing_feedstock(
  dt_feedsrc, regions,
  commodities = c("c146", "c147", "c149"),
  continents  = c("EU", "ASI", "NAM", "LAM"),
  period      = 2012:2014) + ggtitle("2012-2014")

p_feed_late <- plot_sourcing_feedstock(
  dt_feedsrc, regions,
  commodities = c("c146", "c147", "c149"),
  continents  = c("EU", "ASI", "NAM", "LAM"),
  period      = 2020:2022) + ggtitle("2020-2022")

p_feed_src <- (p_feed_early / p_feed_late) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Sourcing of feedstock embodied in biofuel production vs consumption",
    theme = theme(plot.title = element_text(face = "bold"))) &
  theme(legend.position = "bottom")

ggsave(file.path("output", "plot", "sourcing_feedstock_BF.svg"),
       p_feed_src, width = 11, height = 11, dpi = 300, device = svg)


######################################################################################################################
#2. Flow chart
######################################################################################################################

codes <- commodity_meta$comm_code   # c146, c147, c149

trace_flows <- fread(file.path(IN_DIR, "FABIO_bfTrace_2012-2022_BF_continent.csv"))

# Helper: wrap a rotated title as a patchwork-compatible grob panel
make_title_strip <- function(label, font_size = 10) {
  wrap_elements(
    grid::textGrob(
      label,
      rot    = 90,
      gp     = grid::gpar(fontface = "bold", fontsize = font_size),
      hjust  = 0.5,
      vjust  = 0.5
    )
  ) &
    theme(plot.margin = margin(0, 0, 0, 0))
}

# Build the two trace Sankeys (origin -> producer -> consumer, mass-conserving -> raw)
p1 <- bf_sankey_gg(
  trace_flows,
  year_sel    = 2020:2022,
  biofuel_sel = "c146",
  cont_order  = c("LAM", "NAM", "ASI", "EU"),
  merge_regions = c("EUR", "OCE", "ROW", "AFR"),
  merge_label   = "Other",
  normalize   = "raw"
)

p2 <- bf_sankey_gg(
  trace_flows,
  year_sel    = 2020:2022,
  biofuel_sel = c("c147", "c149"),
  cont_order  = c("LAM", "NAM", "ASI", "EU"),
  merge_regions = c("EUR", "OCE", "ROW", "AFR"),
  merge_label   = "Other",
  normalize   = "raw"
)

# Build vertical title strips
t1 <- make_title_strip(commodity_meta[comm_code == "c146", item])
t2 <- make_title_strip(
  paste(commodity_meta[comm_code %in% c("c147", "c149"), item], collapse = " + ")
)

# Compose: [title strip | sankey] stacked twice
row1 <- t1 + p1 + plot_layout(widths = c(0.04, 1))   # strip ~4% of width
row2 <- t2 + p2 + plot_layout(widths = c(0.04, 1))

grid <- (row1 / row2) +
  plot_layout(
    guides = "collect",
    heights = c(1, 1)
  ) &
  theme(
    legend.position = "bottom",
    plot.margin     = margin(2, 2, 2, 2)
  )

svglite::svglite("output/plot/BF_Trace_grid_c146_c147+c149_2020-2022.svg",
                 width = 6, height = 10); print(grid); dev.off()
