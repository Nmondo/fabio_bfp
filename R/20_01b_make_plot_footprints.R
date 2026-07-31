# 20b - Make FOOTPRINT plots (executes the functions defined in 19_01b_plot_function_footprints.R)
# ------------------------------------------------------------------------------
# Run 18_01b_footprints.R first to produce the input CSVs/RDS this script's
# sourced files read. This script is self-contained from there: it sources the
# shared plot definitions and the footprint plot-function definitions itself, so
# it can be run on its own (no need to have already run 19_01b in this session).
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
source("R/19_01b_plot_function_footprints.R")

######################################################################################################################
############################## PLOT RESULTS #########################################################################
######################################################################################################################

######################################################################################################################
#1. Plot countries' impact balance (exported, self, imported)
######################################################################################################################

plot_netLU_2012_2022 <- plot_balance(dt_tradeFeed, Y_summary)

plot_ibif_2012_2022 <- plot_balance(dt_tradeFeed, Y_summary, indicator = "ibif_total")

plot_lcim_terrestrial_2012_2022 <- plot_balance(dt_tradeFeed, Y_summary, indicator = "LCIM_EQ_terrestrial")


######################################################################################################################
#2. Plot impacts by commodity, by feedstock over time.
######################################################################################################################

# Grid labelling (mirrors 19_01a's facet_grid layout): columns = indicator,
# rows = commodity. Per-panel titles are off; override the strip text with
# `indicator_labels` / `commodity_labels` (named vectors) when the metadata
# short_label is too long for a strip.
IND_STRIP <- c(ibif_total          = "IBIF",
               LCIM_EQ_terrestrial = "LC-Impact",
               land_harv           = "Land use")

p_ibif <- plot_commodity_feedstock_grid(
  dt_tradeFeed,
  commodities = c("c146", "c147", "c149"),
  indicators  = "ibif_total",
  Y_summary = Y_summary,
  top_n_feedstock = 7,
  indicator_labels = IND_STRIP
)

p_lcim <- plot_commodity_feedstock_grid(
  dt_tradeFeed,
  commodities = c("c146", "c147", "c149"),
  indicators  = "LCIM_EQ_terrestrial",
  Y_summary = Y_summary,
  top_n_feedstock = 7,
  indicator_labels = IND_STRIP
)

p_ibif_lcim <- plot_commodity_feedstock_grid(
  dt_tradeFeed,
  commodities = c("c146", "c147", "c149"),
  indicators  = c("ibif_total","LCIM_EQ_terrestrial"),
  Y_summary = Y_summary,
  top_n_feedstock = 7,
  indicator_labels = IND_STRIP
)

p_grid <- plot_commodity_feedstock_grid(
  dt_tradeFeed,
  commodities = c("c146", "c147", "c149"),
  indicators  = c("ibif_total", "land_harv"),
  Y_summary = Y_summary,
  top_n_feedstock = 7,
  indicator_labels = IND_STRIP
)

## For full comparison with LCIM terrestrial
# p_grid_full <- plot_commodity_feedstock_grid(
#   dt_tradeFeed,
#   commodities = c("c146", "c147", "c149"),
#   indicators  = c("land_harv", "ibif_total", "LCIM_EQ_terrestrial")
# )

p_grid_BP <- plot_commodity_grid(
  dt_tradeFeed_BP,
  commodities = bp_set,
  indicators  = "ibif_total",
  indicator_labels = IND_STRIP
)


p_lh    <- plot_continent_heatmap(dt_feedstock, 2012:2014, 2020:2022,
                                  "land_harv", save = FALSE)
ref_max <- max(p_lh$data$value)
ord     <- attr(p_lh, "cont_order")

plot_continent_heatmap(dt_feedstock, 2012:2014, 2020:2022, "ibif_total",
                       ref_max = ref_max, gamma = 1.5, region_order = ord,
                       breaks = c(0, 10, 100, round(ref_max)))
plot_continent_heatmap(dt_feedstock, 2012:2014, 2020:2022, "LCIM_EQ_terrestrial",
                       ref_max = ref_max, gamma = 1.5, region_order = ord,
                       breaks = c(0, 20, 100, round(ref_max)))
plot_continent_heatmap(dt_feedstock, 2012:2014, 2020:2022, "land_harv",
                       ref_max = ref_max, gamma = 1.5, region_order = ord,
                       breaks = c(0, 1000, 10000, round(ref_max)))



## --- Two-panel figure: (a) IBIF above (b) LC-Impact terrestrial ---------------
## Each panel keeps its own colourbar (the two indicators have different units).
## Comment this block out to skip it.
p_hm_a <- plot_continent_heatmap(dt_feedstock, 2012:2014, 2020:2022, "ibif_total",
                                 ref_max = ref_max, gamma = 1.5, region_order = ord,
                                 breaks = c(0, 10, 100, round(ref_max)), save = FALSE)
p_hm_b <- plot_continent_heatmap(dt_feedstock, 2012:2014, 2020:2022, "LCIM_EQ_terrestrial",
                                 ref_max = ref_max, gamma = 1.5, region_order = ord,
                                 breaks = c(0, 20, 100, round(ref_max)), save = FALSE)

p_hm_ab <- (p_hm_a / p_hm_b) +
  patchwork::plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag          = element_text(size = 14, face = "bold"),
        plot.tag.position = "top",
        plot.margin       = margin(t = 12, r = 5, b = 5, l = 5))

ggsave(file.path("output", "plot",
                 "2012-2014_vs_2020-2022_IBIF_LCIM_flows_continent_ab.svg"),
       p_hm_ab, width = 10, height = 12.5, dpi = 300)
p_hm_ab


# Producer-located, end-product-decomposed (symmetric Ly footprint), RELATIVE %.
# Ranked by absolute biofuel-driven impact ...
p_enduse_ibif <- plot_enduse_origin(dt_endUseOrigin,
                                    indicator_select = "ibif_total",
                                    years    = c(2012, 2022),
                                    top_n    = 8,
                                    rank_by  = "biofuel_total",
                                    n_groups = 5,
                                    save = TRUE)

p_enduse_lcim <- plot_enduse_origin(dt_endUseOrigin,
                                    indicator_select = "LCIM_EQ_terrestrial",
                                    years    = c(2012, 2022),
                                    top_n    = 8,
                                    rank_by  = "biofuel_total",
                                    n_groups = 5,
                                    save = TRUE)

p_enduse_lcim_lu <- plot_enduse_origin(dt_endUseOrigin,
                                       indicator_select = "LCIM_EQ_terrestrial_land_use",
                                       years    = c(2012, 2022),
                                       top_n    = 8,
                                       rank_by  = "biofuel_total",
                                       n_groups = 5,
                                       save = TRUE)

# time series across all years present
rbindlist(lapply(sort(unique(dt_endUseOrigin$year)), function(yr)
  compute_global_biofuel_share(dt_endUseOrigin, "ibif_total", yr)))

rbindlist(lapply(sort(unique(dt_endUseOrigin$year)), function(yr)
  compute_global_biofuel_share(dt_endUseOrigin, "LCIM_EQ_terrestrial", yr)))

rbindlist(lapply(sort(unique(dt_endUseOrigin$year)), function(yr)
  compute_global_biofuel_share(dt_endUseOrigin, "LCIM_EQ_terrestrial_land_use", yr)))


######################################################################################################################
############################## SAVE PLOTS ###########################################################################
######################################################################################################################

dir.create(file.path("output", "plot"), recursive = TRUE, showWarnings = FALSE)

ggsave(filename = file.path("output", "plot", "balance_LU_2012_2022.svg"),
       plot = plot_netLU_2012_2022,
       width = 10, height = 10, dpi = 300)

ggsave(filename = file.path("output", "plot", "balance_ibif_2012_2022.svg"),
       plot = plot_ibif_2012_2022,
       width = 10, height = 10, dpi = 300)

ggsave(filename = file.path("output", "plot", "balance_lcim_terrestrial_2012_2022.svg"),
       plot = plot_lcim_terrestrial_2012_2022,
       width = 10, height = 10, dpi = 300)

# ggsave(filename = file.path("output", "plot", "feedstock_impact_by_indicator_grid.svg"),
#        plot = p_grid_full,
#        device = svg,
#        width = 19, height = 12 , dpi = 300)

ggsave(filename = file.path("output", "plot", "feedstock_impact_ibif_lu.svg"),
       plot = p_grid,
       device = svg,
       width = 13, height = 12 , dpi = 300)

ggsave(filename = file.path("output", "plot", "feedstock_impact_ibif_lcim.svg"),
       plot = p_ibif_lcim,
       device = svg,
       width = 13, height = 12 , dpi = 300)

ggsave(filename = file.path("output", "plot", "feedstock_impact_ibif.svg"),
       plot = p_ibif,
       device = svg,
       width = 7, height = 12 , dpi = 300)

ggsave(filename = file.path("output", "plot", "feedstock_impact_lcim.svg"),
       plot = p_lcim,
       device = svg,
       width = 7, height = 12 , dpi = 300)




######################################################################################################################
############################## FOCAL-SOURCE SHARE PLOTS (IDN palm / BRA cane / USA maize) ############################
######################################################################################################################

sub_map <- data.table(
  indicator = c(
    "LCIM_EQ_terrestrial_land_use", "LCIM_EQ_terrestrial_acidification", "LCIM_EQ_terrestrial_climate",
    "ibif_co2_eq", "ibif_land_crop", "ibif_land_grass", "ibif_nh3",
    "FD_EQ_ric_terrestrial_climate"
  ),
  parent = c(
    rep("LCIM_EQ_terrestrial", 3),
    rep("ibif_total", 4),
    "FD_EQ_ric_terrestrial"
  )
)


# ── helpers ────────────────────────────────────────────────────────────────────

compute_global_share <- function(co, fs, label) {
  total <- dt_tradeFeed[, .(total = sum(value, na.rm = TRUE)), by = .(indicator, year)]
  focal <- dt_tradeFeed[country_origin == co & feedstock == fs,
                        .(focal = sum(value, na.rm = TRUE)), by = .(indicator, year)]
  total[focal, on = .(indicator, year)][, `:=`(share = focal / total, source = label)]
}

compute_sub_share <- function(co, fs, label) {
  focal <- dt_tradeFeed[country_origin == co & feedstock == fs,
                        .(focal = sum(value, na.rm = TRUE)), by = .(indicator, year)]
  focal_sub <- focal[sub_map, on = "indicator", nomatch = 0]
  parent_totals <- focal[indicator %in% unique(sub_map$parent)
  ][, .(parent = indicator, year, parent_total = focal)]
  focal_sub[parent_totals, on = c(parent = "parent", "year")
  ][, `:=`(share = focal / parent_total, source = label)]
}

# ── compute ────────────────────────────────────────────────────────────────────

share_idn_palm       <- compute_global_share("IDN", "Palm Oil",        "IDN Palm Oil")
share_bra_sugarcane  <- compute_global_share("BRA", "Sugar cane",      "BRA Sugar Cane")
share_usa_maize      <- compute_global_share("USA", "Maize and products", "USA Maize")
share_global         <- rbind(share_idn_palm, share_bra_sugarcane, share_usa_maize)

share_sub_combined   <- rbind(
  compute_sub_share("IDN", "Palm Oil",           "IDN Palm Oil"),
  compute_sub_share("BRA", "Sugar cane",         "BRA Sugar Cane"),
  compute_sub_share("USA", "Maize and products", "USA Maize")
)

# ── plot 1 ─────────────────────────────────────────────────────────────────────

indicators_sel <- c("ibif_total", "LCIM_EQ_terrestrial", "FD_EQ_ric_terrestrial")

ggplot(share_global[indicator %in% indicators_sel],
       aes(x = year, y = share,
           color = source,
           shape = indicator)) +
  geom_line() +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = scales::breaks_width(1),
                     labels = scales::label_number(accuracy = 1, big.mark = "")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_color_manual(values = c("IDN Palm Oil"   = "#E76F51",
                                "BRA Sugar Cane" = "#2A9D8F",
                                "USA Maize"      = "#E9C46A")) +
  scale_shape_manual(values = c("ibif_total"           = 16,
                                "LCIM_EQ_terrestrial"  = 17,
                                "FD_EQ_ric_terrestrial" = 15)) +
  labs(
    title = "Share of IDN Palm Oil, BRA Sugar Cane & USA Maize in global footprint",
    y     = "Share of global total", x = "Year",
    color = "Source", shape = "Indicator"
  ) +
  theme_minimal()

# ── plot 2 ─────────────────────────────────────────────────────────────────────

line_data_combined <- share_global[indicator %in% indicators_sel][, parent := indicator]
scale_factor <- 1 / max(line_data_combined$share, na.rm = TRUE)

p_indicator_BRA_IDN_USA <-
  ggplot() +
  geom_area(
    data = share_sub_combined,
    aes(x = year, y = share, fill = indicator),
    alpha = 0.8, position = "stack"
  ) +
  geom_line(
    data = line_data_combined,
    aes(x = year, y = share * scale_factor),
    color = "black", linewidth = 0.9, linetype = "dashed"
  ) +
  geom_point(
    data = line_data_combined,
    aes(x = year, y = share * scale_factor),
    color = "black", size = 1.5
  ) +
  scale_x_continuous(breaks = scales::breaks_width(1),
                     labels = scales::label_number(accuracy = 1, big.mark = "")) +
  scale_y_continuous(
    labels   = scales::percent_format(accuracy = 1),
    sec.axis = sec_axis(~ . / scale_factor,
                        labels = scales::percent_format(accuracy = 0.1),
                        name   = "Share of global total (dashed)")
  ) +
  facet_grid(parent ~ source, scales = "free_y") +
  labs(
    title = "IDN Palm Oil, BRA Sugar Cane & USA Maize: subcomponent composition & global share",
    y     = "Share of parent indicator", x = "Year", fill = "Subcomponent"
  ) +
  theme_minimal() +
  theme(
    strip.text         = element_text(size = 8, face = "bold"),
    legend.text        = element_text(size = 8),
    axis.title.y.right = element_text(size = 8, color = "grey40"),
    axis.text.x        = element_text(angle = 45, hjust = 1)
  )

ggsave(plot = p_indicator_BRA_IDN_USA,
       filename = file.path(fabio_root, "output/plot/p_indicator_BRA_IDN_USA.pdf"),
       width = 12,
       height = 9,
       dpi = 300)



######################################################################################################################
############################## EU FOOTPRINT TABLE: DOMESTIC vs IMPORTS BY ORIGIN CONTINENT ###########################
######################################################################################################################
# Consumer continent == "EU". Rows split into:
#   - "Domestic (EU)" : origin continent == consumer continent (self-flows + intra-EU trade)
#   - one row per other origin continent (imports)
# BF  (dt_tradeFeed)    : by (commodity, year)
# BP  (dt_tradeFeed_BP) : by year only, summed across commodities
# Indicators kept: the four ibif components + ibif_total.
# Output: output/table/EU_footprint_by_origin_continent.xlsx
# ------------------------------------------------------------------------------

CONSUMER_CONT <- "EU"
IND_KEEP <- c("ibif_co2_eq", "ibif_land_crop", "ibif_land_grass",
              "ibif_nh3", "ibif_total")          # ibif_total = sum of the four

TAB_DIR  <- file.path("output", "table")
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)
TAB_FILE <- file.path(TAB_DIR, "EU_footprint_by_origin_continent.xlsx")

reg_cont <- unique(as.data.table(regions)[, .(iso3c, continent)])

# ── core: attach continents, filter to EU consumers, aggregate ─────────────────
#   by_commodity = TRUE  -> keep `commodity` as a dimension (BF)
#   by_commodity = FALSE -> sum across all commodities (BP)
eu_origin_table <- function(dt, by_commodity = TRUE) {
  
  d <- as.data.table(dt)[indicator %in% IND_KEEP]
  
  ## origin continent
  d <- merge(d, reg_cont, by.x = "country_origin", by.y = "iso3c",
             all.x = TRUE, sort = FALSE)
  setnames(d, "continent", "continent_origin")
  
  ## consumer continent
  d <- merge(d, reg_cont, by.x = "country_consumer", by.y = "iso3c",
             all.x = TRUE, sort = FALSE)
  setnames(d, "continent", "continent_consumer")
  
  ## unmatched origins -> explicit bucket instead of silent NA
  if (d[is.na(continent_origin), .N] > 0L) {
    warning(sprintf("origin country/-ies without continent in `regions`: %s",
                    paste(sort(unique(d[is.na(continent_origin), country_origin])),
                          collapse = ", ")))
    d[is.na(continent_origin), continent_origin := "Unmatched"]
  }
  
  d <- d[continent_consumer == CONSUMER_CONT]
  if (nrow(d) == 0L)
    stop("No rows with consumer continent == ", CONSUMER_CONT)
  
  dom <- paste0("Domestic (", CONSUMER_CONT, ")")
  d[, origin_group := fifelse(continent_origin == CONSUMER_CONT, dom, continent_origin)]
  
  ## keep allocation as a dimension only if the input mixes mass/value
  alloc_col <- if (uniqueN(d$allocation) > 1L) "allocation" else NULL
  
  by_cols <- c(alloc_col, "year",
               if (by_commodity) "commodity",
               "origin_group", "indicator")
  
  agg <- d[, .(value = sum(value, na.rm = TRUE)), by = by_cols]
  
  ## indicators to columns, in the requested order
  id_cols <- setdiff(by_cols, "indicator")
  wide <- dcast(agg,
                as.formula(paste(paste(id_cols, collapse = " + "), "~ indicator")),
                value.var = "value", fill = 0)
  for (ind in IND_KEEP) if (!ind %in% names(wide)) wide[, (ind) := 0]
  setcolorder(wide, c(id_cols, IND_KEEP))
  
  ## commodity labels, if available
  if (by_commodity && exists("items_full_bcp")) {
    lbl <- unique(as.data.table(items_full_bcp)[, .(comm_code, item)])
    wide <- merge(wide, lbl, by.x = "commodity", by.y = "comm_code",
                  all.x = TRUE, sort = FALSE)
    setnames(wide, "item", "commodity_name")
    setcolorder(wide, c(setdiff(id_cols, "commodity"), "commodity", "commodity_name"))
    id_cols <- c(id_cols, "commodity_name")
  }
  
  ## domestic row first, then origin continents alphabetically
  wide[, .ord := fifelse(origin_group == dom, 0L, 1L)]
  setorderv(wide, c(setdiff(id_cols, c("origin_group", "commodity_name")),
                    ".ord", "origin_group"))
  wide[, .ord := NULL]
  wide[]
}

# ── same table as % of the EU total (within year x commodity) ──────────────────
share_table <- function(wide) {
  s   <- copy(wide)
  key <- setdiff(names(s), c("origin_group", IND_KEEP))
  s[, (IND_KEEP) := lapply(.SD, function(x) 100 * x / sum(x)),
    by = key, .SDcols = IND_KEEP]
  s[]
}

# ── build ─────────────────────────────────────────────────────────────────────
tab_EU_BF <- eu_origin_table(dt_tradeFeed,    by_commodity = TRUE)
tab_EU_BP <- eu_origin_table(dt_tradeFeed_BP, by_commodity = FALSE)

## BP is aggregated over all commodities -> enters the table as a single
## pseudo-commodity so that BF and BP can share one sheet
tab_EU_BP[, `:=`(commodity = "Bio-based chemicals", commodity_name = "Bio-based chemicals")]

tab_EU <- rbindlist(list(tab_EU_BF, tab_EU_BP), use.names = TRUE, fill = TRUE)
setcolorder(tab_EU, c(intersect(c("allocation", "year", "commodity", "commodity_name",
                                  "origin_group"), names(tab_EU)), IND_KEEP))

## order: year, commodity (BF codes first, bio-based chemicals last),
## domestic row before imports
dom_lbl <- paste0("Domestic (", CONSUMER_CONT, ")")
tab_EU[, `:=`(.comm_ord = fifelse(commodity == "Bio-based chemicals", 1L, 0L),
              .ord      = fifelse(origin_group == dom_lbl, 0L, 1L))]
setorderv(tab_EU, c("year", ".comm_ord", "commodity", ".ord", "origin_group"))
tab_EU[, c(".comm_ord", ".ord") := NULL]

tab_EU_sh <- share_table(tab_EU)

## sanity check: stored ibif_total vs sum of the four components
message("Max |ibif_total - sum(components)|: ",
        signif(tab_EU[, max(abs(ibif_total - (ibif_co2_eq + ibif_land_crop +
                                                ibif_land_grass + ibif_nh3)))], 4))

# ── write Excel ───────────────────────────────────────────────────────────────
wb_EU <- createWorkbook()
hdr_style <- createStyle(textDecoration = "bold", halign = "center",
                         fgFill = "#EFEFEF", border = "bottom")

add_eu_sheet <- function(name, dt, num_fmt = "0.000E+00") {
  addWorksheet(wb_EU, name)
  writeData(wb_EU, name, dt, headerStyle = hdr_style)
  addStyle(wb_EU, name, createStyle(numFmt = num_fmt),
           rows = 2:(nrow(dt) + 1),
           cols = which(names(dt) %in% IND_KEEP), gridExpand = TRUE)
  freezePane(wb_EU, name, firstRow = TRUE)
  setColWidths(wb_EU, name, cols = seq_along(dt), widths = "auto")
}

add_eu_sheet("EU_footprint", tab_EU)
add_eu_sheet("shares",       tab_EU_sh, num_fmt = "0.00")

saveWorkbook(wb_EU, TAB_FILE, overwrite = TRUE)


######################################################################################################################
############################## BIO-BASED PRODUCTS (bp_set): ibif_total BY COMMODITY ##################################
######################################################################################################################
# Two deliverables, both driven by dt_tradeFeed_BP (VALUE-allocated, from 18b):
#   (1) TABLE  output/table/BP_footprint_ibif_by_commodity.xlsx
#          consumption-based ibif_total per comm_code x year (global and EU-consumer)
#   (2) PLOT   plot_continent_heatmap, summed over ALL bp_set commodities, 2012 vs 2022
#
# NOTE ON AGGREGATION. dt_tradeFeed_BP is a bilateral (country_origin x
# country_consumer) table split by flow_type into "self" and "trade". The
# consumption-based footprint of a consumer is therefore the sum over BOTH
# flow types for that country_consumer -- exactly the `self + imports` quantity
# built in `.balance_data()`. No de-duplication is needed; summing is correct.
# ------------------------------------------------------------------------------

BP_IND  <- "ibif_total"
TAB_DIR <- file.path("output", "table")
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)

# reg_cont is defined in the EU-table section above; redefine defensively so this
# block also works when run on its own.
if (!exists("reg_cont")) reg_cont <- unique(as.data.table(regions)[, .(iso3c, continent)])

## ---- 0. Base BP slice, with origin continent attached -----------------------
dt_bp <- as.data.table(dt_tradeFeed_BP)[commodity %in% bp_set]

missing_bp <- setdiff(bp_set, unique(dt_bp$commodity))
if (length(missing_bp) > 0L)
  message("bp_set codes with no rows in dt_tradeFeed_BP: ",
          paste(missing_bp, collapse = ", "))

if (!BP_IND %in% unique(dt_bp$indicator))
  stop("Indicator '", BP_IND, "' not present in dt_tradeFeed_BP.")

# self-flows must carry their own country as consumer for the consumption-based
# aggregation below; patch if 18b left them blank
if (dt_bp[flow_type == "self" & (is.na(country_consumer) | country_consumer == ""), .N] > 0L) {
  warning("self-flow rows with empty country_consumer found; filling from country_origin.")
  dt_bp[flow_type == "self" & (is.na(country_consumer) | country_consumer == ""),
        country_consumer := country_origin]
}

dt_bp <- merge(dt_bp, reg_cont, by.x = "country_origin", by.y = "iso3c",
               all.x = TRUE, sort = FALSE)
setnames(dt_bp, "continent", "continent_origin")
dt_bp[country_origin == "EU27", continent_origin := "EU"]

if (dt_bp[is.na(continent_origin), .N] > 0L) {
  warning(sprintf("BP origin country/-ies without continent in `regions`: %s",
                  paste(sort(unique(dt_bp[is.na(continent_origin), country_origin])),
                        collapse = ", ")))
  dt_bp[is.na(continent_origin), continent_origin := "Unmatched"]
}

# consumer continent, for the EU-restricted sheet
dt_bp <- merge(dt_bp, reg_cont, by.x = "country_consumer", by.y = "iso3c",
               all.x = TRUE, sort = FALSE)
setnames(dt_bp, "continent", "continent_consumer")
dt_bp[country_consumer == "EU27", continent_consumer := "EU"]


######################################################################################################################
# (1) TABLE: ibif_total by comm_code
######################################################################################################################

TAB_FILE_BP <- file.path(TAB_DIR, "BP_footprint_ibif_by_commodity.xlsx")

comm_lbl <- unique(as.data.table(items_full_bcp)[, .(comm_code, item)])

# ---- long form: one row per commodity x year (optionally restricted to a
#      consumer continent) --------------------------------------------------
bp_comm_long <- function(dt, consumer_cont = NULL) {
  d <- dt[indicator == BP_IND]
  if (!is.null(consumer_cont)) d <- d[continent_consumer == consumer_cont]
  if (nrow(d) == 0L)
    stop("No BP rows for consumer continent = ",
         if (is.null(consumer_cont)) "<all>" else consumer_cont)
  
  agg <- d[, .(ibif_total = sum(value, na.rm = TRUE)), by = .(commodity, year)]
  agg <- merge(agg, comm_lbl, by.x = "commodity", by.y = "comm_code",
               all.x = TRUE, sort = FALSE)
  setnames(agg, c("commodity", "item"), c("comm_code", "commodity_name"))
  agg[is.na(commodity_name), commodity_name := comm_code]
  setcolorder(agg, c("comm_code", "commodity_name", "year", "ibif_total"))
  setorderv(agg, c("year", "comm_code"))
  agg[]
}

# ---- wide form: rows = commodity, columns = years, + Total row -------------
bp_comm_wide <- function(long) {
  w <- dcast(long, comm_code + commodity_name ~ year,
             value.var = "ibif_total", fill = 0)
  yr_cols <- setdiff(names(w), c("comm_code", "commodity_name"))
  
  # order commodities by their footprint in the most recent year, descending
  setorderv(w, yr_cols[length(yr_cols)], order = -1L)
  
  tot <- w[, lapply(.SD, sum, na.rm = TRUE), .SDcols = yr_cols]
  tot[, `:=`(comm_code = "TOTAL", commodity_name = "All bio-based products")]
  setcolorder(tot, names(w))
  
  rbind(w, tot)[]
}

# ---- same, as % of the all-BP total within each year ----------------------
bp_comm_share <- function(wide) {
  s       <- copy(wide)[comm_code != "TOTAL"]
  yr_cols <- setdiff(names(s), c("comm_code", "commodity_name"))
  s[, (yr_cols) := lapply(.SD, function(x) 100 * x / sum(x)), .SDcols = yr_cols]
  s[]
}

bp_long_glob <- bp_comm_long(dt_bp)
bp_wide_glob <- bp_comm_wide(bp_long_glob)
bp_shr_glob  <- bp_comm_share(bp_wide_glob)

bp_long_EU   <- bp_comm_long(dt_bp, consumer_cont = "EU")
bp_wide_EU   <- bp_comm_wide(bp_long_EU)
bp_shr_EU    <- bp_comm_share(bp_wide_EU)

# ---- write ---------------------------------------------------------------
wb_BP <- createWorkbook()
hdr_style_bp <- createStyle(textDecoration = "bold", halign = "center",
                            fgFill = "#EFEFEF", border = "bottom")

add_bp_sheet <- function(name, dt, num_cols, num_fmt = "0.000E+00") {
  addWorksheet(wb_BP, name)
  writeData(wb_BP, name, dt, headerStyle = hdr_style_bp)
  addStyle(wb_BP, name, createStyle(numFmt = num_fmt),
           rows = 2:(nrow(dt) + 1),
           cols = which(names(dt) %in% num_cols), gridExpand = TRUE)
  freezePane(wb_BP, name, firstRow = TRUE, firstCol = TRUE)
  setColWidths(wb_BP, name, cols = seq_along(dt), widths = "auto")
}

yr_cols_glob <- setdiff(names(bp_wide_glob), c("comm_code", "commodity_name"))
yr_cols_EU   <- setdiff(names(bp_wide_EU),   c("comm_code", "commodity_name"))

add_bp_sheet("ibif_by_commodity",      bp_wide_glob, yr_cols_glob)
add_bp_sheet("shares_pct",             bp_shr_glob,  yr_cols_glob, num_fmt = "0.00")
add_bp_sheet("ibif_by_commodity_EU",   bp_wide_EU,   yr_cols_EU)
add_bp_sheet("shares_pct_EU",          bp_shr_EU,    yr_cols_EU,   num_fmt = "0.00")
add_bp_sheet("long",                   bp_long_glob, "ibif_total")

saveWorkbook(wb_BP, TAB_FILE_BP, overwrite = TRUE)
message("Wrote ", TAB_FILE_BP,
        "  (units: MSA*km2*yr; ", length(yr_cols_glob), " years, ",
        nrow(bp_wide_glob) - 1L, " commodities)")


######################################################################################################################
# (2) PLOT: continent heatmap, all bp_set commodities summed, 2012 vs 2022
######################################################################################################################

# Reshape into the layout plot_continent_heatmap() expects:
#   country_consumer | continent_origin | commodity | year | feedstock | indicator | value
dt_feedstock_BP <- dt_bp[, .(value = sum(value, na.rm = TRUE)),
                         by = .(country_consumer, continent_origin,
                                commodity, year, feedstock, indicator)]

# --- probe run to size the colour breaks to the BP order of magnitude --------
p_bp_probe <- plot_continent_heatmap(
  dt_feedstock_BP, 2012, 2022, "ibif_total",
  commodities = bp_set,
  save        = FALSE
)
bp_max    <- max(p_bp_probe$data$value, na.rm = TRUE)
bp_breaks <- unique(c(0, signif(bp_max * c(0.01, 0.1, 1), 2)))

p_bp_heatmap <- plot_continent_heatmap(
  dt_feedstock_BP, 2012, 2022, "ibif_total",
  commodities  = bp_set,
  gamma        = 1.5,
  breaks       = bp_breaks,
  title        = "Bio-based product consumer region",
  legend_title = "Mean species abundance loss (1000 MSA\u00b7km\u00b2\u00b7yr)",
  file_tag     = "BP",
  save         = TRUE
)
p_bp_heatmap
