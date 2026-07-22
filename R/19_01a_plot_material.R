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
dt_feedsrc <- fread(file.path(IN_DIR, "FABIO_feedSourcing_2012-2022_BF_byComm_inclSelf.csv"))
sankey_flows <- fread(file.path(IN_DIR, "FABIO_bfChain_2012-2022_BF_final_demand_continent.csv"))
trace_flows <- fread(file.path(IN_DIR, "FABIO_bfTrace_2012-2022_BF_continent.csv"))


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
  filter(origin_comm_name != "Other, Unknown")


plot_feedstock_desc <- function(data,
                                target_comm_in      = NULL,
                                target_continent_in = NULL,
                                n_top               = 12,
                                include_other       = TRUE,
                                top_scope           = c("per_comm", "global"),
                                facet_scales        = "free_y",
                                ncol                = NULL,
                                to_supply           = TRUE,          # convert embodied feedstock -> biofuel supply via TCF
                                tcf                 = NULL,          # NULL = load intermediate_data/tcf_table_final.rds
                                tcf_on_missing      = c("drop", "keep", "error"),
                                scale               = 1,             # value / scale (matches plot_country_consumption)
                                y_lab               = NULL,
                                total_labels        = TRUE,
                                accuracy            = 1) {
  
  top_scope      <- match.arg(top_scope)
  tcf_on_missing <- match.arg(tcf_on_missing)
  if (is.null(y_lab))
    y_lab <- if (to_supply) "Biofuel supply (M liters)"
  else           "Feedstock mass embodied in biofuel consumption (ktonnes)"
  
  comm_lookup  <- setNames(as.character(commodity_meta$item),
                           as.character(commodity_meta$comm_code))
  relabel_comm <- function(x) ifelse(x %in% names(comm_lookup), comm_lookup[x], x)
  
  feed_lookup <- setNames(as.character(feedstock_meta$category),
                          as.character(feedstock_meta$feedstock))
  feed_cat    <- function(x) ifelse(x %in% names(feed_lookup), feed_lookup[x], "Other")
  
  if (is.null(target_comm_in))      target_comm_in      <- unique(data$target_comm)
  if (is.null(target_continent_in)) target_continent_in <- unique(data$target_continent)
  
  d <- data %>%
    filter(target_comm      %in% target_comm_in,
           target_continent %in% target_continent_in)
  
  if (nrow(d) == 0) stop("No rows for the requested target_comm / target_continent.")
  
  # ---- convert embodied feedstock MASS -> biofuel SUPPLY (output-equivalent) via TCF ----
  # supply = mass * output_qty, where output_qty [biofuel/feedstock] = 1/multiplier_output_kl_to_input_t.
  # Joined on (feedstock, biofuel) since the factor differs by fuel. Feedstock with no TCF row
  # has no defined supply -> dropped (default) with a warning on the mass share lost.
  if (to_supply) {
    if (is.null(tcf)) {
      tcf <- readRDS("intermediate_data/tcf_table_final.rds")
      setDT(tcf)
      tcf[, item := trimws(item)]
      tcf[, biofuel_code := fcase(
        grepl("Biogasoline",      proc), "c146",
        grepl("Biodiesel",        proc), "c147",
        grepl("Renewable diesel", proc), "c149",
        default = NA_character_)]
      tcf <- tcf[!item %in% c("Oilcrops Oil, Other", "Total")]
    } else setDT(tcf)
    
    
    tcf_lu <- unique(tcf[!is.na(biofuel_code),
                         .(origin_comm_name = trimws(as.character(item)),
                           target_comm      = as.character(biofuel_code),
                           cf               = as.numeric(output_qty))])
    tcf_lu <- tcf_lu[is.finite(cf)]
    dup <- tcf_lu[, .N, by = .(origin_comm_name, target_comm)][N > 1L]
    if (nrow(dup))
      stop(nrow(dup), " (feedstock, biofuel) pair(s) map to >1 output_qty in `tcf` \u2014 collapse first.")
    
    d <- as.data.table(copy(d))
    d[, `:=`(origin_comm_name = trimws(as.character(origin_comm_name)),
             target_comm      = as.character(target_comm))]
    d[tcf_lu, on = .(origin_comm_name, target_comm), cf := i.cf]
    
    na_i <- is.na(d$cf)
    if (any(na_i)) {
      bad  <- unique(d[na_i, .(origin_comm_name, target_comm)])
      tot  <- sum(d$value, na.rm = TRUE)
      lost <- sum(d$value[na_i], na.rm = TRUE)
      msg  <- sprintf("TCF: %d feedstock\u00d7biofuel pair(s) have no output_qty (%.1f%% of mass)",
                      nrow(bad), if (tot > 0) 100 * lost / tot else 0)
      if (tcf_on_missing == "error") stop(msg, ".")
      warning(msg, if (tcf_on_missing == "drop") " \u2014 dropped."
              else                          " \u2014 kept UNCONVERTED (mixed units!).")
    }
    if (tcf_on_missing == "drop") d <- d[!is.na(cf)]
    d[, value := value * fifelse(is.na(cf), 1, cf)][, cf := NULL]
    d <- as.data.frame(d)
  }
  
  # display-only relabel: long HS name -> short label, AFTER the TCF join
  d$origin_comm_name <- ifelse(
    grepl("^Animal or vegetable fats and oils", d$origin_comm_name),
    "Used Cooking Oil", d$origin_comm_name)
  
  rank_top <- function(df, n) {
    df %>%
      group_by(origin_comm_name) %>%
      summarize(total = sum(value), .groups = "drop") %>%
      slice_max(total, n = n) %>%
      arrange(desc(total)) %>%
      pull(origin_comm_name)
  }
  
  if (top_scope == "global") {
    top_candidates <- rank_top(d, n_top)
  } else {
    top_candidates <- d %>%
      group_by(target_comm) %>%
      group_modify(~ tibble(origin_comm_name = rank_top(.x, n_top))) %>%
      ungroup() %>%
      pull(origin_comm_name) %>%
      unique()
  }
  
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
  
  d_plot <- d %>%
    mutate(feedstock = ifelse(origin_comm_name %in% top_feedstocks,
                              origin_comm_name, "Other"))
  
  if (!include_other) d_plot <- d_plot %>% filter(feedstock != "Other")
  
  d_plot <- d_plot %>%
    mutate(feedstock = factor(feedstock,
                              levels = c(top_feedstocks,
                                         if (include_other) "Other"))) %>%
    group_by(year, target_comm, target_continent, feedstock) %>%
    summarize(value = sum(value) / scale, .groups = "drop")
  
  totals <- d_plot %>%
    group_by(year, target_comm, target_continent) %>%
    summarize(total = sum(value), .groups = "drop")
  
  cats <- feed_cat(top_feedstocks)
  pal  <- get_feedstock_palette(top_feedstocks, feedstock_meta)
  
  cat_display <- c("Starchy / Sugar crops" = "Sugar and starch",
                   "Oilcrops"              = "Oils and fats")
  cat_order   <- names(cat_display)
  
  present_levels  <- levels(droplevels(d_plot$feedstock))
  neutral_present <- setdiff(present_levels[feed_cat(present_levels) == "Other"], "Other")
  neutral_cols    <- setNames(unname(feedstock_color_map[neutral_present]), neutral_present)
  
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
    
    for (nm in neutral_present) {
      key <- paste0("__ow_", k, "_", nm)
      legend_breaks   <- c(legend_breaks, key)
      legend_labels   <- c(legend_labels, nm)
      header_pal[key] <- neutral_cols[[nm]]
    }
  }
  
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
  pal_full <- pal_full[!duplicated(names(pal_full))]
  if (include_other) pal_full <- c(pal_full, Other = "grey70")
  
  d_plot$feedstock <- factor(d_plot$feedstock,
                             levels = union(levels(d_plot$feedstock), names(header_pal)))
  
  phantom_lvls <- names(header_pal)
  if (length(phantom_lvls)) {
    pad <- d_plot[rep(1L, length(phantom_lvls)), , drop = FALSE]
    pad$feedstock <- factor(phantom_lvls, levels = levels(d_plot$feedstock))
    pad$value     <- 0
    d_plot <- rbind(d_plot, pad)
    d_plot$feedstock <- factor(d_plot$feedstock, levels = levels(pad$feedstock))
  }
  
  num <- scales::label_number(accuracy = accuracy)
  
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
    scale_y_continuous(labels = num, expand = expansion(mult = c(0, 0.10))) +
    guides(fill = guide_legend(ncol = 1, byrow = TRUE)) +
    labs(x = NULL, y = y_lab) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          legend.key.size  = unit(0.4, "cm"),
          legend.text      = ggtext::element_markdown(),
          legend.key       = element_blank(),
          strip.text       = element_text(face = "bold"))
  
  if (total_labels) {
    p <- p + geom_text(data = totals,
                       aes(year, total, label = num(total)),
                       inherit.aes = FALSE, vjust = -0.4, size = 2.7, fontface = "bold")
  }
  
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
#3. Annual biofuel consumption by country & fuel type  (grid: fuel row x region column, countries stacked)
######################################################################################################################
# Mirrors plot_feedstock_desc's grid layout, but the stacked segments are CONSUMER COUNTRIES instead of feedstocks.
#
#   grid:      row  = biofuel type (comm_code)   ;   column = region (target continent)
#   stack:     one segment per consumer country within the region  (rest -> "Other")
#   highlight: the n_top largest consumers in the region, KEPT only if the country reaches >= share_thresh
#              of the regional total in at least one year (top-N intersect share-gate, per comm x continent)
#   label:     regional annual total printed above each bar (bar height == true regional total, incl. "Other")
#
# NOTE on scales: facet_grid(scales = "free_y") frees y PER ROW (fuel) but SHARES it across the region columns of
#   that row -> a large region squashes a small one. Use comparable regions (EU/ASI/NAM/LAM), or set
#   `independent_y = TRUE` to switch to ggh4x::facet_grid2(independent = "y") for a truly per-panel y-axis.

# distinct qualitative colours by stacking Brewer palettes (self-contained; interpolates if > ~49 needed)
.distinct_country_cols <- function(n) {
  base <- unique(c(
    RColorBrewer::brewer.pal(9,  "Set1"),
    RColorBrewer::brewer.pal(8,  "Dark2"),
    RColorBrewer::brewer.pal(8,  "Set2"),
    RColorBrewer::brewer.pal(12, "Paired"),
    RColorBrewer::brewer.pal(12, "Set3")
  ))
  base <- base[!base %in% c("#999999", "#FFFFB3")]        # drop near-greys that clash with "Other"
  if (n <= length(base)) base[seq_len(n)] else grDevices::colorRampPalette(base)(n)
}

plot_country_consumption <- function(Y_summary,
                                     regions,
                                     comm_in       = NULL,                 # e.g. c("c146","c147","c149"); NULL = all
                                     continent_in  = NULL,                 # e.g. c("EU","ASI","NAM","LAM"); NULL = all
                                     items         = items_full_bcp,       # comm_code -> item (row-strip labels)
                                     n_top         = 10,
                                     share_thresh  = 0.10,
                                     scale         = 1000,                 # value / scale (1000: k-litres -> M litres)
                                     y_lab         = "Biofuel consumption (M liters)",
                                     accuracy      = 1,
                                     facet_scales  = "free_y",
                                     independent_y = FALSE,                # TRUE -> ggh4x per-panel free y
                                     total_labels  = TRUE,
                                     legend_ncol   = 2,
                                     legend_title  = "Consumer country") {
  
  num         <- scales::label_number(accuracy = accuracy)
  comm_lookup <- setNames(as.character(items$item), as.character(items$comm_code))
  relabel     <- function(x) { o <- comm_lookup[x]; o[is.na(o)] <- x[is.na(o)]; unname(o) }
  
  reg <- as.data.frame(regions) |> dplyr::distinct(iso3c, continent)
  
  # ---- 1. attach continent to the CONSUMER, collapse origins -> annual country x fuel consumption ----
  d <- as.data.frame(Y_summary) |>
    dplyr::left_join(reg, by = c("target_country" = "iso3c")) |>
    dplyr::group_by(year, target_country, comm_code, continent) |>
    dplyr::summarize(value = sum(value), .groups = "drop")
  
  if (anyNA(d$continent)) {
    lost <- unique(d$target_country[is.na(d$continent)])
    warning(sprintf("%d consumer country/-ies have no continent in `regions` (dropped): %s",
                    length(lost), paste(lost, collapse = ", ")))
    d <- d[!is.na(d$continent), ]
  }
  
  if (is.null(comm_in))      comm_in      <- sort(unique(d$comm_code))
  if (is.null(continent_in)) continent_in <- unique(d$continent)
  d <- dplyr::filter(d, comm_code %in% comm_in, continent %in% continent_in)
  if (nrow(d) == 0) stop("No rows for the requested comm_in / continent_in.")
  
  # ---- 2. regional totals (year x fuel x region): denominator for shares AND the above-bar label ----
  reg_tot <- d |>
    dplyr::group_by(year, comm_code, continent) |>
    dplyr::summarize(reg_total = sum(value), .groups = "drop")
  
  # ---- 3. highlight set per (fuel, region): top-N by total, gated on >= share_thresh in some year ----
  ctry_stats <- d |>
    dplyr::left_join(reg_tot, by = c("year", "comm_code", "continent")) |>
    dplyr::mutate(share = value / reg_total) |>
    dplyr::group_by(comm_code, continent, target_country) |>
    dplyr::summarize(total = sum(value), max_share = max(share, na.rm = TRUE), .groups = "drop")
  
  highlight <- ctry_stats |>
    dplyr::group_by(comm_code, continent) |>
    dplyr::slice_max(total, n = n_top, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::filter(max_share >= share_thresh) |>
    dplyr::select(comm_code, continent, target_country) |>
    dplyr::mutate(.keep_flag = TRUE)
  
  # ---- 4. label as country or "Other", re-aggregate, scale ----
  d_plot <- d |>
    dplyr::left_join(highlight, by = c("comm_code", "continent", "target_country")) |>
    dplyr::mutate(country = ifelse(is.na(.keep_flag), "Other", target_country)) |>
    dplyr::group_by(year, comm_code, continent, country) |>
    dplyr::summarize(value = sum(value) / scale, .groups = "drop")
  
  totals <- dplyr::mutate(reg_tot, total = reg_total / scale)
  
  # ---- 5. factor order (continent, then desc size) so legend groups by region & big-at-bottom ----
  country_levels <- ctry_stats |>
    dplyr::semi_join(highlight, by = c("comm_code", "continent", "target_country")) |>
    dplyr::group_by(continent, target_country) |>
    dplyr::summarize(tot = sum(total), .groups = "drop") |>
    dplyr::arrange(factor(continent, levels = continent_in), dplyr::desc(tot)) |>
    dplyr::pull(target_country) |>
    unique()
  
  d_plot$country <- factor(d_plot$country, levels = c(country_levels, "Other"))
  
  pal <- c(setNames(.distinct_country_cols(length(country_levels)), country_levels),
           Other = "grey75")
  
  # ---- 6. build ----
  p <- ggplot(d_plot, aes(year, value, fill = country)) +
    geom_col(position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values = pal, name = legend_title, drop = FALSE) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    scale_y_continuous(labels = num, expand = expansion(mult = c(0, 0.10))) +
    guides(fill = guide_legend(ncol = legend_ncol, byrow = TRUE)) +
    labs(x = NULL, y = y_lab) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          legend.key.size   = unit(0.4, "cm"),
          strip.text        = element_text(face = "bold"))
  
  if (total_labels)
    p <- p + geom_text(data = totals,
                       aes(year, total, label = num(total)),
                       inherit.aes = FALSE, vjust = -0.4, size = 2.7, fontface = "bold")
  
  if (independent_y) {
    if (!requireNamespace("ggh4x", quietly = TRUE))
      stop("independent_y = TRUE needs the 'ggh4x' package.")
    p <- p + ggh4x::facet_grid2(comm_code ~ continent, scales = "free_y", independent = "y",
                                labeller = labeller(comm_code = relabel))
  } else {
    p <- p + facet_grid(comm_code ~ continent, scales = facet_scales,
                        labeller = labeller(comm_code = relabel))
  }
  p
}



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

# ─── Core: classify (relative to an anchor region), aggregate to continent shares ─────
.sourcing_shares <- function(df, regions,
                             anchor_col    = "country_consumer", # "country_consumer" (product) | "region" (feedstock)
                             carry         = character(0),       # extra grouping cols to keep, e.g. "basis"
                             commodities   = NULL,
                             continents    = NULL,
                             period_early  = 2012:2014,
                             period_late   = 2020:2022,
                             single_period = NULL,               # if set (year vector) -> one window, no early/late rows
                             share_method  = c("value", "year_mean")) {
  
  share_method <- match.arg(share_method)
  df  <- as.data.table(copy(df))
  reg <- unique(as.data.table(regions)[, .(iso3c, continent)])
  
  ## origin & anchor continents
  df <- merge(df, reg, by.x = "country_origin", by.y = "iso3c", all.x = TRUE, sort = FALSE)
  setnames(df, "continent", "continent_origin")
  df <- merge(df, reg, by.x = anchor_col, by.y = "iso3c", all.x = TRUE, sort = FALSE)
  setnames(df, "continent", "continent_anchor")
  
  ## classify each flow relative to the anchor region
  df[, category := fifelse(
    flow_type == "self", "Domestic",
    fifelse(continent_origin == continent_anchor, "Intra-regional", "Extra-regional"))]
  df[is.na(category), category := "Extra-regional"]   # origin w/ no matched continent -> Extra
  
  ## period window(s)
  if (!is.null(single_period)) {
    lab <- sprintf("%d\u2013%d", min(single_period), max(single_period))
    df  <- df[year %in% single_period][, period := lab]
  } else {
    early_lab <- sprintf("%d\u2013%d", min(period_early), max(period_early))
    late_lab  <- sprintf("%d\u2013%d", min(period_late),  max(period_late))
    df <- df[year %in% c(period_early, period_late)]
    df[, period := fifelse(year %in% period_early, early_lab, late_lab)]
  }
  
  if (!is.null(commodities)) df <- df[commodity        %in% commodities]
  if (!is.null(continents))  df <- df[continent_anchor %in% continents]
  df <- df[!is.na(continent_anchor)]
  if (nrow(df) == 0)
    stop("No rows left after filtering for the requested commodities / continents / years.")
  
  by0 <- c("continent_anchor", "commodity", "period", carry)
  agg <- df[, .(value = sum(value, na.rm = TRUE)), by = c(by0, "year", "category")]
  
  if (share_method == "value") {
    agg <- agg[, .(value = sum(value, na.rm = TRUE)), by = c(by0, "category")]
    agg[, share := value / sum(value), by = by0]
  } else {
    agg[, share_yr := value / sum(value), by = c(by0, "year")]
    agg <- agg[, .(share = mean(share_yr, na.rm = TRUE)), by = c(by0, "category")]
  }
  agg[, category := factor(category, levels = sourcing_levels)]
  agg[]
}

# ─── Core: stacked-bar plot (row facet x continent grid) ──────────────────────
.plot_sourcing <- function(agg,
                           items             = items_full_bcp,
                           colors            = sourcing_colors,
                           commodities_order = NULL,
                           continents_order  = NULL,
                           row_var           = "period",   # "period" (product) | "basis" (feedstock)
                           y_lab             = "Share of consumption",
                           title             = NULL) {
  
  agg   <- as.data.table(copy(agg))
  items <- as.data.table(items)
  
  lbl        <- setNames(items$item, items$comm_code)
  comm_codes <- if (!is.null(commodities_order)) commodities_order else sort(unique(agg$commodity))
  comm_labs  <- ifelse(comm_codes %in% names(lbl), lbl[comm_codes], comm_codes)
  agg[, commodity_lab := factor(
    ifelse(commodity %in% names(lbl), lbl[commodity], commodity), levels = comm_labs)]
  
  cont_levels <- if (!is.null(continents_order)) continents_order else sort(unique(agg$continent_anchor))
  agg[, continent_anchor := factor(continent_anchor, levels = cont_levels)]
  if (row_var == "period") agg[, period := factor(period, levels = sort(unique(period)))]
  
  ggplot(agg, aes(commodity_lab, share, fill = category)) +
    geom_col(width = 0.8, position = position_stack(reverse = TRUE)) +
    facet_grid(reformulate("continent_anchor", row_var),
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
# (1) Geographic sourcing of FEEDSTOCK embodied in biofuel PRODUCTION vs CONSUMPTION
######################################################################################################################
# Input: FABIO_feedSourcing_2012-2022_BF_byComm_inclSelf.csv (from 18a, feedstock_sourcing_shares).
#   Columns: year, basis, commodity, country_origin, region, flow_type, value
#   basis == "production"  -> region is the biofuel PRODUCER (feedstock embodied in output)
#   basis == "consumption" -> region is the biofuel CONSUMER (feedstock embodied in final demand)
# Sourcing is classified relative to `region`: Domestic / Intra-regional / Extra-regional.

plot_sourcing_feedstock <- function(data, regions,
                                    commodities  = NULL,      # e.g. c("c146","c147","c149"); NULL = all
                                    continents   = NULL,      # anchor-region continents; NULL = all
                                    period       = 2020:2022, # single window (Production vs Consumption go in rows)
                                    share_method = "value",
                                    items        = items_full_bcp,
                                    colors       = sourcing_colors,
                                    y_lab        = "Share of embodied feedstock",
                                    return_data  = FALSE) {
  
  agg <- .sourcing_shares(as.data.table(copy(data)), regions,
                          anchor_col    = "region",
                          carry         = "basis",
                          commodities   = commodities,
                          continents    = continents,
                          single_period = period,
                          share_method  = share_method)
  
  agg[, basis := factor(fifelse(basis == "production", "Production", "Consumption"),
                        levels = c("Production", "Consumption"))]
  
  p <- .plot_sourcing(agg, items = items, colors = colors,
                      commodities_order = commodities,
                      continents_order  = continents,
                      row_var = "basis", y_lab = y_lab)
  
  if (return_data) list(plot = p, data = agg) else p
}




######################################################################################################################
################################# FLOW CHART #######################################################################
######################################################################################################################
# NB: sankey_flows (FABIO_bfChain_*.csv) is written by 18a with the two stages in
# DIFFERENT units and deliberately NOT mass-balanced across the conversion:
#   stage 1 (feedstock_to_producer) = embodied feedstock USE, feedstock tonnes
#           (total requirements, L %*% x, restricted to ISIC == "A"), and
#   stage 2 (producer_to_consumer)  = biofuel output.
# There is no TCF step anywhere in the pipeline now (we reverted from
# biofuel-output-equivalent back to embodied feedstock use), so this function no
# longer accepts or applies `tcf`. Because the two stages are in different units,
# render with normalize = "per_stage" (each stage = 100%); "raw" would put
# feedstock tonnes and biofuel tonnes on one shared height scale and mislead.

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
                         merge_regions = NULL,    # continents to fold into ONE display node
                         merge_label   = "Other", # label shown for the merged node
                         node_w      = 0.04,
                         gap_frac    = 0.04,
                         curv_n      = 60,
                         alpha       = 0.8,
                         share_min   = 0.01,   # hide labels below this share
                         share_size  = 2.3) {  # font size for share labels
  
  req <- c("stage","source_node","target_node","flow_class","value",
           "source_continent","target_continent")
  stopifnot(all(req %in% names(sankey_flows)))
  aggregate <- match.arg(aggregate, c("mean","sum"))
  d <- copy(as.data.table(sankey_flows))
  if (!is.null(biofuel_sel) && "biofuel" %in% names(d)) d <- d[biofuel %in% biofuel_sel]
  
  # --- optional: collapse a set of continents into ONE display node ------------------------
  # Purely visual. flow_class is NOT recomputed, so a domestic / intra / inter ribbon keeps
  # its class after its continent is folded into `merge_label`; same-class flows that now
  # share a merged (node -> node) pair are summed, different classes stay separate ribbons.
  # (A domestic AFR->AFR flow and an inter AFR->EUR flow both live in the node but stay
  #  coloured by their original class -- they are not treated as one region.)
  if (!is.null(merge_regions)) {
    d[source_continent %in% merge_regions, source_continent := merge_label]
    d[target_continent %in% merge_regions, target_continent := merge_label]
    d[, source_node := paste(source_continent, sub("^.*\\| ", "", source_node), sep = " | ")]
    d[, target_node := paste(target_continent, sub("^.*\\| ", "", target_node), sep = " | ")]
    if (!is.null(cont_order))
      cont_order <- unique(fifelse(cont_order %in% merge_regions, merge_label, cont_order))
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
  link[, flow_class_f := factor(flow_class, levels = names(class_cols))]
  
  # BOTH sides banded by flow_class in the SAME order (domestic -> intra -> inter),
  # so every ribbon sits in its colour band on each end and blue never overlaps orange.
  # Within a band, sort by the opposite node's midpoint to reduce same-colour crossings.
  # Source side: class band first, then target midpoint within the band.
  setorder(link, source_node, flow_class_f, t_mid)
  link[, sy_top := nodes[source_node, ytop] + cumsum(c(0, head(h, -1))), by = source_node]
  
  # Target side: class band first, then source midpoint within the band.
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
                         y_lab        = "Biofuel supply (B liters)")  # default tcf_on_missing = "drop"

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