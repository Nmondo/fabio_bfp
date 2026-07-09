# 19a - Plot MATERIAL-FLOW / descriptive results  (reads MASS-allocated files)
# ------------------------------------------------------------------------------
# Descriptive, physical-flow plots only:
#   - biofuel & biopolymer consumption (Y)                    plot_consumption
#   - feedstock mass embodied in consumption (Z)              plot_feedstock_desc
#   - geographic sourcing of feedstock / final product        plot_sourcing_*
#   - 3-tier biofuel supply-chain Sankey                      bf_sankey_gg
#
# All footprint tables read here are the MASS-allocated ones written by
# 18a_material_flows.R (feedstock tonnes / biofuel output). The tradeFeed and
# totalreq CSVs are selected by the "_mass_" tag in their filename; Y_/Z_summary,
# Ysourcing and bfChain are Y/Z-based (no allocation tag) and come from 18a too.
#
# Companion: 19b_plot_environmental.R (reads the "_value_" stressor files).
# NOTE: the setup block, `fabio_files()`, `items_full_bcp`, `regions` and `tcf`
#       are duplicated across 19a/19b. Edit both, or factor into a shared file.
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

# Setup ------------------------------------------------------------------------
library(data.table)
library(Matrix)
library(tidyverse)
library(ggplot2)
library(scales)
library(paletteer)
library(RColorBrewer)
library(gridExtra)
library(patchwork)
library(svglite)

items_full_bcp <- read_csv("inst/items_full_bcp.csv")
items_full_bcp <- as.data.table(items_full_bcp)
regions <- setDT(read_csv("inst/regions.csv"))[current==TRUE]
tcf <- readRDS("intermediate_data/tcf_table_final.rds")

# ---- MODEL VERSION -----------------------------------------------------------
#   "rescaled" (default) -> results in output/         (RED-rescaled run)
#   "bypass"             -> results in output/bypass/  (non-rescaled counterfactual)
# Keep in sync with 18a.
model_version <- Sys.getenv("FABIO_RUN_MODE", unset = "rescaled")
model_version <- if (tolower(trimws(model_version)) == "bypass") "bypass" else "rescaled"
IN_DIR <- if (model_version == "bypass") "output/bypass" else "output"
message(sprintf(">>> [19a material] model_version = '%s'  (reading footprints from: %s)",
                model_version, IN_DIR))

source("R/19_plot_definitions.R")

dir.create(file.path("output", "plot"), recursive = TRUE, showWarnings = FALSE)

## Clean TCF -------------------------------------------------------------------
setDT(tcf)
tcf[, biofuel_code := fcase(
  grepl("Biogasoline",      proc), "c146",
  grepl("Biodiesel",        proc), "c147",
  grepl("Renewable diesel", proc), "c149",
  default = NA_character_)]
tcf <- tcf[!item %in% c("Oilcrops Oil, Other", "Total")]

######################################################################################################################
############################## HELPER FUNCTION TO EXTRACT RESULTS FILES FROM SPECIFIC PATTERN ########################
######################################################################################################################
# `alloc` filters on the allocation tag in the filename ("mass" | "value" | NULL = any).
# `must_match` is an optional vector of extra regexes that ALL must match (e.g. "_material_").

fabio_files <- function(prefix, group = c("BF", "BP", "BF_BP"),
                        alloc      = NULL,
                        must_match = NULL,
                        dir = IN_DIR) {
  group <- match.arg(group)
  yr    <- "(201[2-9]|202[0-2])"
  all   <- list.files(dir,
                      pattern    = sprintf("^%s_%s_.*\\.csv$", prefix, yr),
                      full.names = TRUE)
  all   <- all[!file.info(all)$isdir]          # drop any stray directories
  all   <- switch(group,
                  BF    = all[grepl("_BF_", all) & !grepl("_BF_BP_", all)],
                  BP    = all[grepl("_BP_", all) & !grepl("_BF_BP_", all)],
                  BF_BP = all[grepl("_BF_BP_", all)]
  )
  if (!is.null(alloc))      all <- all[grepl(paste0("_", alloc, "_"), all)]
  if (!is.null(must_match)) for (rx in must_match) all <- all[grepl(rx, all)]
  all
}

######################################################################################################################
############################## READ RESULTS DATA (MASS) ##############################################################
######################################################################################################################

## Descriptive stats (Y / Z based, allocation-invariant filenames) ------------
Y_summary    <- fread(file.path(IN_DIR, "Y_summary_c146_c147_c149.csv"))
Y_summary_BP <- fread(file.path(IN_DIR, "Y_summary_BP.csv"))
Z_summary    <- fread(file.path(IN_DIR, "Z_summary_c146_c147_c149.csv"))
dt_ysrc      <- fread(file.path(IN_DIR, "FABIO_Ysourcing_2012-2022_BF_byComm_inclSelf.csv"))
sankey_flows <- fread(file.path(IN_DIR, "FABIO_bfChain_2012-2022_BF_final_demand_continent.csv"))

## Material feedstock trade breakdown -----------------------------------------
# Now the MASS-allocated, direct (tier-1) bilateral files written by 18a, i.e.
#   FABIO_tradeFeed_<year>_material_mass_BF_byComm_byFeed_direct_bilat.csv
# (was "_material_value_..." before the mass/value split).
files_tradeFeed_material <- fabio_files("FABIO_tradeFeed", "BF",
                                        alloc = "mass", must_match = "_material_")
dt_material <- rbindlist(lapply(files_tradeFeed_material, fread))

## Total-requirement (full upstream) material flows ---------------------------
# MASS-allocated FABIO_totalreq (fp_indirect, extension = NULL). Currently read
# for reference only; no active plot below consumes it.
files_totalreq_BF <- fabio_files("FABIO_totalreq", "BF", alloc = "mass")
dt_totalreq       <- rbindlist(lapply(files_totalreq_BF, fread))

######################################################################################################################
############################## DESCRIPTIVE STATS ####################################################################
######################################################################################################################

######################################################################################################################
#1. Descriptive statistics for consumption / fuel types.
######################################################################################################################

plot_consumption <- function(df,
                             items,
                             fill_colors  = NULL,
                             regions      = NULL,        # if given, adds origin/target continent
                             group_var    = NULL,        # e.g. "target_continent" -> facet
                             scale        = 1000,
                             accuracy     = 1,
                             y_lab        = "Biofuel consumption (M liters)",
                             fill_lab     = "Fuel type",
                             title        = "Global biofuel consumption by fuel type",
                             seg_labels   = TRUE,
                             total_labels = TRUE,
                             return_data  = FALSE) {
  
  # optional continent enrichment (origin + target)
  if (!is.null(regions)) {
    df <- df %>%
      left_join(regions %>% select(iso3c, continent),
                by = c("origin_country" = "iso3c")) %>%
      rename(origin_continent = continent) %>%
      left_join(regions %>% select(iso3c, continent),
                by = c("target_country" = "iso3c")) %>%
      rename(target_continent = continent)
  }
  
  grp <- c("year", "comm_code", group_var)
  agg <- df %>%
    group_by(across(all_of(grp))) %>%
    summarize(value = sum(value) / scale, .groups = "drop") %>%
    left_join(items %>% select(comm_code, item), by = "comm_code")
  
  # order items by earliest-year value (largest at bottom of stack)
  item_order <- agg %>%
    filter(year == min(year)) %>%
    group_by(item) %>%
    summarize(value = sum(value), .groups = "drop") %>%
    arrange(desc(value)) %>%
    pull(item)
  agg$item <- factor(agg$item, levels = rev(item_order))
  
  # per-year (x group) totals
  totals <- agg %>%
    group_by(across(all_of(c("year", group_var)))) %>%
    summarize(total = sum(value), .groups = "drop")
  
  num <- scales::label_number(accuracy = accuracy)
  
  p <- ggplot(agg, aes(x = year, y = value, fill = item)) +
    geom_col() +
    scale_x_continuous(breaks = unique(agg$year)) +
    scale_y_continuous(labels = scales::label_number(),
                       expand = expansion(mult = c(0, 0.08))) +
    labs(x = "Year", y = y_lab, fill = fill_lab, title = title) +
    theme_minimal()
  
  if (!is.null(fill_colors))
    p <- p + scale_fill_manual(values = fill_colors)
  
  if (seg_labels)
    p <- p + geom_text(aes(label = num(value)),
                       position = position_stack(vjust = 0.5),
                       size = 2.8, colour = "white")
  
  if (total_labels)
    p <- p + geom_text(data = totals,
                       aes(x = year, y = total, label = num(total)),
                       inherit.aes = FALSE,
                       vjust = -0.5, size = 3, fontface = "bold")
  
  if (!is.null(group_var))
    p <- p + facet_wrap(vars(.data[[group_var]]), scales = "free_y")
  
  if (return_data) list(plot = p, data = agg, totals = totals) else p
}


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

######################################################################################################################
#2. Descriptive statistics for production technologies
######################################################################################################################

Z_summary <- Z_summary %>%
  left_join(regions %>% select(iso3c, continent), by = c("origin_country" = "iso3c")) %>%
  rename(origin_continent = continent) %>%
  left_join(regions %>% select(iso3c, continent), by = c("target_country" = "iso3c")) %>%
  rename(target_continent = continent)

Z_per_year <- Z_summary %>%
  group_by(origin_comm_name, year, target_comm, target_continent) %>%
  summarize(value = sum(value)/1000,
            .groups = "drop") %>%
  filter(origin_comm_name != "Other, Unknown") %>%
  mutate(origin_comm_name = ifelse(grepl("^Animal or vegetable fats and oils", origin_comm_name), "Used Cooking Oil", origin_comm_name))


######################################################################################################################
############################## FUNCTIONS TO PLOT RESULTS #############################################################
######################################################################################################################

######################################################################################################################
#0. Plot countries' feedstock use
######################################################################################################################

plot_feedstock_desc <- function(data,
                                target_comm_in      = NULL,   # scalar, vector, or NULL = all
                                target_continent_in = NULL,   # scalar, vector, or NULL = all
                                n_top               = 12,
                                include_other       = TRUE,
                                top_scope           = c("per_comm", "global"),
                                facet_scales        = "free_y",
                                ncol                = NULL,
                                y_label             = "Feedstock mass embodied in biofuel consumption (ktonnes)") {
  
  top_scope <- match.arg(top_scope)
  
  comm_lookup  <- setNames(as.character(commodity_meta$item),
                           as.character(commodity_meta$comm_code))
  relabel_comm <- function(x) ifelse(x %in% names(comm_lookup), comm_lookup[x], x)
  
  # feedstock → category lookup (Oil / Sugar / Other), with safe fallback
  feed_lookup <- setNames(as.character(feedstock_meta$category),
                          as.character(feedstock_meta$feedstock))
  feed_cat    <- function(x) ifelse(x %in% names(feed_lookup),
                                    feed_lookup[x], "Other")
  
  # default to "all" when nothing is supplied
  if (is.null(target_comm_in))      target_comm_in      <- unique(data$target_comm)
  if (is.null(target_continent_in)) target_continent_in <- unique(data$target_continent)
  
  d <- data %>%
    filter(target_comm      %in% target_comm_in,
           target_continent %in% target_continent_in)
  
  if (nrow(d) == 0) stop("No rows for the requested target_comm / target_continent.")
  
  # helper: top-n feedstocks in a data frame, ranked by total value
  rank_top <- function(df, n) {
    df %>%
      group_by(origin_comm_name) %>%
      summarize(total = sum(value), .groups = "drop") %>%
      slice_max(total, n = n) %>%
      arrange(desc(total)) %>%
      pull(origin_comm_name)
  }
  
  # build the candidate set of "top" feedstocks
  if (top_scope == "global") {
    top_candidates <- rank_top(d, n_top)
  } else {                                      # per_comm: union of within-commodity top-N
    top_candidates <- d %>%
      group_by(target_comm) %>%
      group_modify(~ tibble(origin_comm_name = rank_top(.x, n_top))) %>%
      ungroup() %>%
      pull(origin_comm_name) %>%
      unique()
  }
  
  # order: largest category first, then by total value within each category
  top_feedstocks <- d %>%
    filter(origin_comm_name %in% top_candidates) %>%
    group_by(origin_comm_name) %>%
    summarize(total = sum(value), .groups = "drop") %>%
    mutate(category = feed_cat(origin_comm_name)) %>%
    group_by(category) %>%
    mutate(cat_total = sum(total)) %>%
    ungroup() %>%
    arrange(desc(cat_total), category, desc(total)) %>%
    pull(origin_comm_name)
  
  # bucket non-top feedstocks into "Other"; aggregate
  d_plot <- d %>%
    mutate(feedstock = ifelse(origin_comm_name %in% top_feedstocks,
                              origin_comm_name, "Other"))
  
  if (!include_other) d_plot <- d_plot %>% filter(feedstock != "Other")
  
  d_plot <- d_plot %>%
    mutate(feedstock = factor(feedstock,
                              levels = c(top_feedstocks,
                                         if (include_other) "Other"))) %>%
    group_by(year, target_comm, target_continent, feedstock) %>%
    summarize(value = sum(value), .groups = "drop")
  
  # fixed, one-feedstock = one-colour palette (see R/feedstock_palette.R).
  cats <- feed_cat(top_feedstocks)
  pal  <- get_feedstock_palette(top_feedstocks, feedstock_meta)
  
  # ---- legend breaks/labels ---------------------------------------------------
  cat_display <- c("Starchy / Sugar crops" = "Sugar and starch",
                   "Oilcrops"              = "Oils and fats")
  cat_order   <- names(cat_display)                    # sugar/starch first
  
  present_levels  <- levels(droplevels(d_plot$feedstock))
  neutral_present <- setdiff(present_levels[feed_cat(present_levels) == "Other"],
                             "Other")
  neutral_cols    <- setNames(unname(feedstock_color_map[neutral_present]),
                              neutral_present)
  
  legend_breaks <- character()
  legend_labels <- character()
  header_pal    <- character()
  
  for (k in cat_order) {
    fs <- top_feedstocks[cats == k]
    if (length(fs) == 0 && length(neutral_present) == 0) next
    hdr <- paste0("__hdr_", k)
    legend_breaks <- c(legend_breaks, hdr, fs)
    legend_labels <- c(legend_labels, paste0("<b>", cat_display[[k]], "</b>"), fs)
    header_pal[hdr] <- "transparent"
    
    for (nm in neutral_present) {                      # neutral once per header
      key <- paste0("__ow_", k, "_", nm)
      legend_breaks   <- c(legend_breaks, key)
      legend_labels   <- c(legend_labels, nm)
      header_pal[key] <- neutral_cols[[nm]]
    }
  }
  
  # Safety net: top feedstocks that are neither sugar/oil nor a neutral.
  placed   <- unlist(lapply(cat_order, function(k) top_feedstocks[cats == k]))
  leftover <- setdiff(top_feedstocks, c(placed, neutral_present))
  if (length(leftover)) {
    legend_breaks <- c(legend_breaks, leftover)
    legend_labels <- c(legend_labels, leftover)
  }
  
  if (include_other) {
    legend_breaks <- c(legend_breaks, "Other")
    legend_labels <- c(legend_labels, "Other")
  }
  
  pal_full <- c(header_pal, pal, neutral_cols)
  pal_full <- pal_full[!duplicated(names(pal_full))]     # neutral may already be in pal
  if (include_other) pal_full <- c(pal_full, Other = "grey70")
  
  # Register phantom keys (__hdr_*, __ow_*) as empty factor levels so
  # scale_fill_manual(drop = FALSE) keeps them in the limits and draws them.
  d_plot$feedstock <- factor(d_plot$feedstock,
                             levels = union(levels(d_plot$feedstock), names(header_pal)))
  
  phantom_lvls <- names(header_pal)
  if (length(phantom_lvls)) {
    pad <- d_plot[rep(1L, length(phantom_lvls)), , drop = FALSE]
    pad$feedstock <- factor(phantom_lvls, levels = levels(d_plot$feedstock))
    pad$value     <- 0
    d_plot <- rbind(d_plot, pad)
    d_plot$feedstock <- factor(d_plot$feedstock, levels = levels(pad$feedstock))  # keep factor after rbind
  }
  
  # auto-decide faceting
  n_comm <- length(unique(d_plot$target_comm))
  n_reg  <- length(unique(d_plot$target_continent))
  
  p <- ggplot(d_plot, aes(year, value, fill = feedstock)) +
    geom_col(position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values = pal_full,
                      breaks = legend_breaks,
                      labels = legend_labels,
                      name   = NULL,
                      drop   = FALSE) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    guides(fill = guide_legend(ncol = 1, byrow = TRUE)) +
    labs(x = NULL, y = y_label) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          legend.key.size  = unit(0.4, "cm"),
          legend.text      = ggtext::element_markdown(),
          legend.key       = element_blank(),
          strip.text       = element_text(face = "bold"))
  
  if (n_comm > 1 && n_reg > 1) {
    p <- p + facet_grid(target_comm ~ target_continent,
                        scales   = facet_scales,
                        labeller = labeller(target_comm = relabel_comm))
  } else if (n_comm > 1) {
    p <- p + facet_wrap(~ target_comm,
                        scales   = facet_scales,
                        ncol     = ncol,
                        labeller = labeller(target_comm = relabel_comm))
  } else if (n_reg > 1) {
    p <- p + facet_wrap(~ target_continent, scales = facet_scales, ncol = ncol)
  } else {
    p <- p + labs(title = paste0(relabel_comm(unique(d_plot$target_comm)), " — ",
                                 unique(d_plot$target_continent)))
  }
  p
}

######################################################################################################################
############################## PLOT RESULTS #########################################################################
######################################################################################################################

######################################################################################################################
#0. Plot descriptive statistics
######################################################################################################################

# usage
p <- plot_feedstock_desc(Z_per_year, c("c146", "c147", "c149"), c("EU","ASI","NAM","LAM"))

ggsave(
  filename = file.path("output", "plot", "desc_embodied_feedstock_BF_ASI-EU-NAM.svg"),
  plot = p,
  width = 10, height = 8, dpi = 300)


######################################################################################################################
############################## SOURCING DECOMPOSITION: Domestic / Intra- / Extra-regional ############################
######################################################################################################################
# Classify where consumed biofuel (or its embodied feedstock) comes from,
# relative to the consumer's own continent:
#   - Domestic       : flow_type == "self"
#   - Intra-regional : same continent, but not self
#   - Extra-regional : different continent (incl. unmatched-continent origins)
#
#   plot_sourcing_feedstock()  -> *_material_mass_BF_byComm_byFeed_direct_bilat.csv  (feedstock mass)
#   plot_sourcing_product()    -> FABIO_Ysourcing_*_byComm_inclSelf.csv (final product)

# Stacking order (bottom -> top) and colours
sourcing_levels <- c("Domestic", "Intra-regional", "Extra-regional")
sourcing_colors <- c(
  "Domestic"       = "#08519C",   # dark blue  -> grown/made at home
  "Intra-regional" = "#6BAED6",   # mid blue   -> same continent
  "Extra-regional" = "#FD8D3C"    # orange     -> off-continent
)

# ─── Core: classify, aggregate to continent shares, average over 2 windows ─────
.sourcing_shares <- function(df, regions,
                             commodities  = NULL,
                             continents   = NULL,
                             period_early = 2012:2014,
                             period_late  = 2020:2022,
                             share_method = c("value", "year_mean")) {
  
  share_method <- match.arg(share_method)
  df  <- as.data.table(copy(df))
  reg <- unique(as.data.table(regions)[, .(iso3c, continent)])
  
  ## --- attach origin & consumer continents -------------------------------
  df <- merge(df, reg, by.x = "country_origin",   by.y = "iso3c",
              all.x = TRUE, sort = FALSE)
  setnames(df, "continent", "continent_origin")
  df <- merge(df, reg, by.x = "country_consumer", by.y = "iso3c",
              all.x = TRUE, sort = FALSE)
  setnames(df, "continent", "continent_consumer")
  
  ## --- classify each flow into a source category -------------------------
  df[, category := fifelse(
    flow_type == "self", "Domestic",
    fifelse(continent_origin == continent_consumer,
            "Intra-regional", "Extra-regional"))]
  # imports whose origin has no matched continent -> Extra-regional
  df[is.na(category), category := "Extra-regional"]
  
  ## --- restrict to the two comparison windows ----------------------------
  early_lab <- sprintf("%d\u2013%d", min(as.integer(period_early)),
                       max(as.integer(period_early)))
  late_lab  <- sprintf("%d\u2013%d", min(as.integer(period_late)),
                       max(as.integer(period_late)))
  df <- df[year %in% c(period_early, period_late)]
  df[, period := fifelse(year %in% period_early, early_lab, late_lab)]
  
  ## --- optional subsetting ----------------------------------------------
  if (!is.null(commodities)) df <- df[commodity          %in% commodities]
  if (!is.null(continents))  df <- df[continent_consumer %in% continents]
  df <- df[!is.na(continent_consumer)]
  
  if (nrow(df) == 0)
    stop("No rows left after filtering for the requested commodities / continents / years.")
  
  ## --- continent-level totals (volume-weighted across countries) ---------
  agg <- df[, .(value = sum(value, na.rm = TRUE)),
            by = .(continent_consumer, commodity, period, year, category)]
  
  if (share_method == "value") {
    # volume-weighted average across years in the window
    agg <- agg[, .(value = sum(value, na.rm = TRUE)),
               by = .(continent_consumer, commodity, period, category)]
    agg[, share := value / sum(value),
        by = .(continent_consumer, commodity, period)]
  } else {
    # equal weight per year: yearly share then simple mean across years
    agg[, share_yr := value / sum(value),
        by = .(continent_consumer, commodity, period, year)]
    agg <- agg[, .(share = mean(share_yr, na.rm = TRUE)),
               by = .(continent_consumer, commodity, period, category)]
  }
  
  agg[, category := factor(category, levels = sourcing_levels)]
  agg[]
}

# ─── Core: stacked-bar plot (continent grid x period rows) ────────────────────
.plot_sourcing <- function(agg,
                           items             = items_full_bcp,
                           colors            = sourcing_colors,
                           commodities_order = NULL,
                           continents_order  = NULL,
                           y_lab             = "Share of consumption",
                           title             = NULL) {
  
  agg   <- as.data.table(copy(agg))
  items <- as.data.table(items)
  
  # commodity code -> readable label, ordered as requested (or by code)
  lbl        <- setNames(items$item, items$comm_code)
  comm_codes <- if (!is.null(commodities_order)) commodities_order
  else sort(unique(agg$commodity))
  comm_labs  <- ifelse(comm_codes %in% names(lbl), lbl[comm_codes], comm_codes)
  agg[, commodity_lab := factor(
    ifelse(commodity %in% names(lbl), lbl[commodity], commodity),
    levels = comm_labs)]
  
  # continent ordering (governs left-to-right grouping of the bars)
  cont_levels <- if (!is.null(continents_order)) continents_order
  else sort(unique(agg$continent_consumer))
  agg[, continent_consumer := factor(continent_consumer, levels = cont_levels)]
  
  # period rows: early label sorts before late label ("2012-2014" < "2020-2022")
  agg[, period := factor(period, levels = sort(unique(period)))]
  
  ggplot(agg, aes(commodity_lab, share, fill = category)) +
    geom_col(width = 0.8, position = position_stack(reverse = TRUE)) +
    facet_grid(period ~ continent_consumer,
               scales = "free_x", space = "free_x") +
    scale_fill_manual(values = colors, name = "Source", drop = FALSE) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(x = NULL, y = y_lab, title = title) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x        = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.spacing.x    = unit(0.3, "lines"),
      panel.spacing.y    = unit(1.6, "lines"),
      strip.text.x = element_text(face = "bold"),
      strip.text.y = element_text(face = "bold", size = 15),
      legend.position    = "bottom"
    )
}

######################################################################################################################
# (1) Geographic sourcing of FEEDSTOCK embodied in biofuel consumption (bilateral files)
######################################################################################################################
# Input: rbindlist of FABIO_tradeFeed_<year>_material_mass_BF_byComm_byFeed_direct_bilat.csv
# Columns: country_origin, country_consumer, flow_type, year, indicator, allocation,
#          feedstock, commodity, value   (feedstock is summed over automatically)

plot_sourcing_feedstock <- function(data, regions,
                                    commodities     = NULL,   # e.g. c("c146","c147","c149"); NULL = all
                                    continents      = NULL,   # e.g. c("EU","ASI","NAM"); NULL = all
                                    indicator_keep  = NULL,   # e.g. "material" (guards against double counting)
                                    allocation_keep = NULL,   # e.g. "mass"
                                    period_early    = 2012:2014,
                                    period_late     = 2020:2022,
                                    share_method    = "value",
                                    items           = items_full_bcp,
                                    colors          = sourcing_colors,
                                    y_lab           = "Share of embodied feedstock",
                                    title  = "Sourcing of feedstock embodied in biofuel consumption",
                                    return_data     = FALSE) {
  
  dt <- as.data.table(copy(data))
  if (!is.null(indicator_keep))  dt <- dt[indicator  %in% indicator_keep]
  if (!is.null(allocation_keep)) dt <- dt[allocation %in% allocation_keep]
  
  agg <- .sourcing_shares(dt, regions,
                          commodities  = commodities,
                          continents   = continents,
                          period_early = period_early,
                          period_late  = period_late,
                          share_method = share_method)
  
  p <- .plot_sourcing(agg, items = items, colors = colors,
                      commodities_order = commodities,
                      continents_order  = continents,
                      y_lab = y_lab, title = title)
  
  if (return_data) list(plot = p, data = agg) else p
}

######################################################################################################################
# (2) Geographic sourcing of the FINAL BIOFUEL PRODUCT consumed
######################################################################################################################
# Input: fread(file.path(IN_DIR, "FABIO_Ysourcing_2012-2022_BF_byComm_inclSelf.csv"))
# Columns: country_consumer, year, commodity, country_origin, flow_type, value, share

plot_sourcing_product <- function(data, regions,
                                  commodities  = NULL,
                                  continents   = NULL,
                                  period_early = 2012:2014,
                                  period_late  = 2020:2022,
                                  share_method = "value",
                                  items        = items_full_bcp,
                                  colors       = sourcing_colors,
                                  y_lab        = "Share of biofuel consumption",
                                  title = "Geographic sourcing of biofuel products consumed",
                                  return_data  = FALSE) {
  
  agg <- .sourcing_shares(as.data.table(copy(data)), regions,
                          commodities  = commodities,
                          continents   = continents,
                          period_early = period_early,
                          period_late  = period_late,
                          share_method = share_method)
  
  p <- .plot_sourcing(agg, items = items, colors = colors,
                      commodities_order = commodities,
                      continents_order  = continents,
                      y_lab = y_lab, title = title)
  
  if (return_data) list(plot = p, data = agg) else p
}

######################################################################################################################
# EXAMPLE USAGE
######################################################################################################################

# --- (1) feedstock embodied (bilateral) -------------------------------------
# NB: allocation_keep switched "value" -> "mass" for the material/mass split.
p_feed_src <- plot_sourcing_feedstock(
  dt_material, regions,
  commodities     = c("c146", "c147", "c149"),
  continents      = c("EU", "ASI", "NAM", "LAM"),
  indicator_keep  = "material",
  allocation_keep = "mass")

ggsave(file.path("output", "plot", "sourcing_feedstock_BF.svg"),
       p_feed_src, width = 11, height = 6, dpi = 300, device = svg)

# --- (2) final product origin -----------------------------------------------
p_prod_src <- plot_sourcing_product(
  dt_ysrc, regions,
  commodities = c("c146", "c147", "c149"),
  continents  = c("EU", "ASI", "NAM", "LAM"))

ggsave(file.path("output", "plot", "sourcing_product_BF.svg"),
       p_prod_src, width = 11, height = 6, dpi = 300, device = svg)


######################################################################################################################
################################# FLOW CHART #######################################################################
######################################################################################################################
# NB: sankey_flows (FABIO_bfChain_*.csv) is written by 18a already converted from
# feedstock use to biofuel-output-equivalent (tcf applied there, before saving).
# Do NOT pass `tcf` to bf_sankey_gg here too - it would apply the factor a second
# time and silently double-convert stage-1 values.

bf_sankey_gg <- function(sankey_flows,
                         year_sel    = NULL,
                         window      = 1L,
                         aggregate   = "mean",
                         biofuel_sel = NULL,
                         commodity_label = NULL,
                         normalize   = "per_stage",
                         class_cols  = c(domestic       = "#08519C",
                                         intra_regional = "#6BAED6",
                                         inter_regional = "#FD8D3C"),
                         cont_order  = NULL,
                         node_w      = 0.04,
                         gap_frac    = 0.04,
                         curv_n      = 60,
                         alpha       = 0.8,
                         share_min   = 0.02,   # hide labels below this share
                         share_size  = 2.3,    # font size for share labels
                         # --- TCF conversion of stage-1 (feedstock use -> biofuel supply) ---------------------
                         tcf            = NULL,                    # data.table; NULL = old behaviour (no conversion)
                         tcf_stage      = "feedstock_to_producer", # which `stage` value carries feedstock-use rows
                         tcf_feed_col   = "item",                  # column in `tcf` holding the feedstock id
                         tcf_bf_col     = "biofuel_code",          # column in `tcf` holding the biofuel id (c146/c147/c149)
                         tcf_val_col    = "tcf",                   # column in `tcf` holding the conversion factor  <-- CHECK names(tcf)
                         feed_join      = "feedstock",             # column in `sankey_flows` matched to tcf_feed_col
                         bf_join        = "biofuel",               # column in `sankey_flows` matched to tcf_bf_col
                         tcf_invert     = FALSE,                   # TRUE if tcf = feedstock-per-biofuel (input coeff) -> uses 1/cf
                         tcf_on_missing = "warn") {                # "warn" (keep raw, unbalanced) | "drop" | "error"
  
  req <- c("stage","source_node","target_node","flow_class","value",
           "source_continent","target_continent")
  stopifnot(all(req %in% names(sankey_flows)))
  aggregate <- match.arg(aggregate, c("mean","sum"))
  d <- copy(as.data.table(sankey_flows))
  if (!is.null(biofuel_sel) && "biofuel" %in% names(d)) d <- d[biofuel %in% biofuel_sel]
  
  # --- TCF: feedstock USE -> biofuel SUPPLY on stage-1 rows --------------------------------
  if (!is.null(tcf)) {
    tcf <- as.data.table(tcf)
    need <- c(tcf_feed_col, tcf_bf_col, tcf_val_col)
    if (!all(need %in% names(tcf)))
      stop("`tcf` is missing column(s): ", paste(setdiff(need, names(tcf)), collapse = ", "),
           ". Set tcf_feed_col / tcf_bf_col / tcf_val_col to match names(tcf).")
    if (!all(c(feed_join, bf_join) %in% names(d)))
      stop("`sankey_flows` has no `", feed_join, "` / `", bf_join,
           "` column \u2014 stage-1 rows must carry feedstock & biofuel ids to attach a tcf. ",
           "(bf_supply_chain_flows emits these; check feed_join/bf_join.)")
    
    tcf_lu <- unique(tcf[, .(feed = as.character(get(tcf_feed_col)),
                             bf   = as.character(get(tcf_bf_col)),
                             cf   = as.numeric(get(tcf_val_col)))])
    tcf_lu <- tcf_lu[!is.na(cf) & !is.na(feed) & !is.na(bf)]
    dup <- tcf_lu[, .N, by = .(feed, bf)][N > 1L]
    if (nrow(dup))
      stop(nrow(dup), " (feedstock, biofuel) pair(s) map to more than one tcf value \u2014 ",
           "collapse `tcf` to one factor per pair before passing it in.")
    if (isTRUE(tcf_invert)) tcf_lu[, cf := 1 / cf]
    
    s1_idx <- which(d$stage == tcf_stage)
    if (length(s1_idx)) {
      key    <- data.table(feed = as.character(d[[feed_join]][s1_idx]),
                           bf   = as.character(d[[bf_join]][s1_idx]))
      cf_vec <- tcf_lu[key, on = c("feed", "bf"), cf]   # one cf per stage-1 row, key order; NA = no match
      
      na_i <- is.na(cf_vec)
      if (any(na_i)) {
        bad <- unique(key[na_i])
        msg <- sprintf("TCF: %d stage-1 row(s) across %d feedstock\u00d7biofuel pair(s) have no match",
                       sum(na_i), nrow(bad))
        if (tcf_on_missing == "error") stop(msg, ".")
        if (tcf_on_missing == "drop")  warning(msg, " \u2014 dropped from the diagram.")
        if (tcf_on_missing == "warn")  warning(msg,
                                               " \u2014 left in feedstock units; the producer node will not balance for these.")
      }
      
      keep <- !na_i
      if (any(keep)) {
        d[s1_idx[keep], value := value * cf_vec[keep]]
        if ("unit" %in% names(d)) d[s1_idx[keep], unit := "biofuel output (via tcf)"]
      }
      if (any(na_i) && tcf_on_missing == "drop") d <- d[-s1_idx[na_i]]
    }
  }
  
  # --- year selection / averaging window ---------------------------------------------------
  if ("year" %in% names(d)) {
    if (!is.null(year_sel)) {
      yrs <- if (length(year_sel) == 1L && window > 1L) {
        half <- (window - 1L) %/% 2L
        (year_sel - half):(year_sel + half)
      } else year_sel
      avail <- sort(intersect(yrs, unique(d$year)))
      if (!length(avail)) stop("None of the requested years are present in `chain`.")
      if (length(avail) < length(yrs))
        warning("Window trimmed to available years: ", paste(avail, collapse = ", "))
      d <- d[year %in% avail]
    } else avail <- sort(unique(d$year))
  } else avail <- NA_integer_
  n_years <- if (anyNA(avail)) 1L else length(avail)
  if (!nrow(d)) stop("No rows after filtering.")
  
  link <- d[, .(value = sum(value)),
            by = .(stage, source_node, target_node, flow_class,
                   source_continent, target_continent)]
  if (aggregate == "mean") link[, value := value / n_years]
  
  # --- per-stage normalisation -------------------------------------------------------------
  if (normalize == "per_stage") {
    link[, w := value / sum(value), by = stage]
  } else if (normalize == "match_stage1") {
    tot <- link[, .(s = sum(value)), by = stage]
    s1  <- tot[stage == "feedstock_to_producer", s]
    link <- merge(link, tot, by = "stage"); link[, w := value * (s1 / s)][, s := NULL]
  } else link[, w := value]
  link <- link[is.finite(w) & w > 0]
  link[, lid := .I]
  
  # --- tiers / columns ---------------------------------------------------------------------
  tier_of <- function(n) sub("^.*\\| ", "", n)
  col_x   <- c(origin = 0, producer = 1, consumer = 2)
  link[, `:=`(s_tier = tier_of(source_node), t_tier = tier_of(target_node))]
  
  # --- node table --------------------------------------------------------------------------
  nodes <- unique(rbindlist(list(
    link[, .(node = source_node, tier = s_tier, continent = source_continent)],
    link[, .(node = target_node, tier = t_tier, continent = target_continent)]
  )))
  if (is.null(cont_order)) cont_order <- sort(unique(nodes$continent))
  nodes[, col := col_x[tier]]
  nodes <- merge(nodes, link[, .(ins  = sum(w)), by = .(node = target_node)], by = "node", all.x = TRUE)
  nodes <- merge(nodes, link[, .(outs = sum(w)), by = .(node = source_node)], by = "node", all.x = TRUE)
  nodes[is.na(ins), ins := 0][is.na(outs), outs := 0]
  nodes[, `:=`(val = pmax(ins, outs), ord = match(continent, cont_order))]
  setorder(nodes, col, ord)
  
  cs    <- nodes[, .(need = sum(val) + (.N - 1) * gap_frac), by = col]
  scale <- 1 / max(cs$need)
  g     <- gap_frac * scale
  nodes[, h := val * scale]
  nodes[, ytop := {
    colH <- sum(h) + (.N - 1) * g
    (1 - colH) / 2 + cumsum(c(0, head(h, -1) + g))
  }, by = col]
  nodes[, `:=`(ybot = ytop + h, ymid = ytop + h / 2,
               xl = col - node_w / 2, xr = col + node_w / 2)]
  setkey(nodes, node)
  
  # --- ribbon endpoints --------------------------------------------------------------------
  link[, h := w * scale]
  link[, `:=`(s_mid = nodes[source_node, ymid], t_mid = nodes[target_node, ymid])]
  
  # Source side: sort by target midpoint (unchanged — keeps source-side visually smooth)
  setorder(link, source_node, t_mid)
  link[, sy_top := nodes[source_node, ytop] + cumsum(c(0, head(h, -1))), by = source_node]
  
  # Target side: sort by flow_class FIRST (keeps same-class ribbons contiguous),
  # then by s_mid within each class (minimises crossing within a class band).
  link[, flow_class_f := factor(flow_class, levels = names(class_cols))]
  setorder(link, target_node, flow_class_f, s_mid)
  link[, ty_top := nodes[target_node, ytop] + cumsum(c(0, head(h, -1))), by = target_node]
  link[, `:=`(sx = nodes[source_node, xr], tx = nodes[target_node, xl])]
  
  # --- ribbon polygons ---------------------------------------------------------------------
  smooth <- function(t) 3 * t^2 - 2 * t^3
  tt     <- seq(0, 1, length.out = curv_n)
  polys  <- link[, {
    x    <- sx + (tx - sx) * tt
    ftop <- sy_top + (ty_top - sy_top) * smooth(tt)
    fbot <- (sy_top + h) + ((ty_top + h) - (sy_top + h)) * smooth(tt)
    data.table(x = c(x, rev(x)), y = c(ftop, rev(fbot)), flow_class = flow_class)
  }, by = lid]
  
  # --- flow-class shares per RECEPTOR node -------------------------------------------------
  shares_raw <- link[, .(
    w_cls    = sum(w),
    ty_top   = min(ty_top),   # topmost ribbon edge for this (node x flow_class) band
    band_h_r = sum(h)         # total rendered height of this band at the target node
  ), by = .(node = target_node, flow_class, stage)]
  
  stage_totals <- link[, .(w_stage = sum(w)), by = stage]
  shares_raw   <- merge(shares_raw, stage_totals, by = "stage")
  
  shares_raw[, share_of_stage := w_cls / w_stage]
  shares_raw[, share          := w_cls / sum(w_cls), by = node]
  
  shares_raw <- shares_raw[share_of_stage >= share_min]
  
  if (nrow(shares_raw) > 0) {
    shares_raw <- merge(shares_raw,
                        nodes[, .(node, col, ytop, h, xr, xl)],
                        by = "node")
    shares_raw[, label_y   := ty_top + band_h_r / 2]
    shares_raw[, label_txt := paste0(round(share * 100), "%")]
    shares_raw[, label_x   := xl - node_w * 0.6]
    shares_raw[, label_hjust := 1]
    shares_raw <- shares_raw[col > 0]
  }
  
  # --- self-documenting year label ---------------------------------------------------------
  agg_word <- if (aggregate == "mean") "mean" else "sum"
  
  darken_col <- function(hex, amount = 0.45) {
    rgb_vals <- col2rgb(hex) / 255
    rgb(rgb_vals[1] * (1 - amount),
        rgb_vals[2] * (1 - amount),
        rgb_vals[3] * (1 - amount))
  }
  label_cols <- setNames(sapply(class_cols, darken_col), names(class_cols))
  
  # --- plot --------------------------------------------------------------------------------
  p <- ggplot() +
    geom_polygon(data = polys, aes(x, y, group = lid, fill = flow_class),
                 alpha = alpha, colour = NA) +
    geom_rect(data = nodes, aes(xmin = xl, xmax = xr, ymin = ytop, ymax = ybot),
              fill = "grey60", colour = "grey30", linewidth = 0.2) +
    geom_text(data = nodes[col == 0], aes(xl - node_w, ymid, label = continent),
              hjust = 1, size = 3) +
    geom_text(data = nodes[col > 0],  aes(xr + node_w, ymid, label = continent),
              hjust = 0, size = 3) +
    annotate("text", x = c(0, 1, 2), y = 1.06, fontface = 2, size = 3.4,
             label = c("Feedstock origin", "Biofuel producer", "Biofuel consumer")) +
    scale_fill_manual(values = class_cols, name = "flow class") +
    coord_cartesian(xlim = c(-0.55, 2.55), ylim = c(-0.02, 1.10), clip = "off") +
    theme_void() +
    theme(legend.position = "bottom", plot.margin = margin(0, 0, 0, 0))
  
  if (nrow(shares_raw) > 0) {
    p <- p +
      geom_text(data = shares_raw,
                aes(x = label_x, y = label_y, label = label_txt, colour = flow_class),
                hjust    = shares_raw$label_hjust,
                size     = share_size * 0.87,
                fontface = "bold",
                show.legend = FALSE) +
      scale_colour_manual(values = label_cols)
  }
  
  p
}


codes <- commodity_meta$comm_code   # c146, c147, c149

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

# Build the two Sankey plots (NO ggtitle). Do NOT pass `tcf` (see note above).
p1 <- bf_sankey_gg(
  sankey_flows,
  year_sel    = 2020:2022,
  biofuel_sel = "c146",
  cont_order  = c("LAM", "NAM", "ASI", "EU"),
  normalize = "raw"
)

p2 <- bf_sankey_gg(
  sankey_flows,
  year_sel    = 2020:2022,
  biofuel_sel = c("c147", "c149"),
  cont_order  = c("LAM", "NAM", "ASI", "EU"),
  normalize = "raw"
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
    heights = c(1, 1)  # equal rows — adjust ratio here if panels differ in content height
  ) &
  theme(
    legend.position = "bottom",
    plot.margin     = margin(2, 2, 2, 2)  # 2 pt on all sides — tweak downward to taste
  )

svglite::svglite("output/plot/BF_Sankey_grid_c146_c147+c149_2020-2022_post.svg",
                 width = 6, height = 10); print(grid); dev.off()