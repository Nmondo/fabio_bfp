# 19.01 - Plot footprint results

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
# Read the footprint CSVs produced by 18_01 for the chosen model version:
#   "rescaled" (default) -> results in output/         (the RED-rescaled run)
#   "bypass"             -> results in output/bypass/  (non-rescaled counterfactual)
# Rescaled is the default, so behaviour is unchanged. Keep in sync with 18_01.
model_version <- Sys.getenv("FABIO_RUN_MODE", unset = "rescaled")
model_version <- if (tolower(trimws(model_version)) == "bypass") "bypass" else "rescaled"
IN_DIR <- if (model_version == "bypass") "output/bypass" else "output"
message(sprintf(">>> [19_01] model_version = '%s'  (reading footprints from: %s)",
                model_version, IN_DIR))

source("R/19_plot_definitions.R")





## Clean TCF

setDT(tcf)
tcf[, biofuel_code := fcase(
  grepl("Biogasoline",      proc), "c146",
  grepl("Biodiesel",        proc), "c147",
  grepl("Renewable diesel", proc), "c149",
  default = NA_character_)]
tcf <- tcf[!item %in% c("Oilcrops Oil, Other", "Total")]

######################################################################################################################
############################## HELPER FUNCTION TO EXTRACTS RESULTS FILES FROM SPECIFIC PATTERN #######################
######################################################################################################################

fabio_files <- function(prefix, group = c("BF", "BP", "BF_BP"), dir = IN_DIR) {
  group <- match.arg(group)
  yr    <- "(201[2-9]|202[0-2])"
  all   <- list.files(dir,
                      pattern    = sprintf("^%s_%s_.*\\.csv$", prefix, yr),
                      full.names = TRUE)
  all   <- all[!file.info(all)$isdir]          # drop any stray directories
  switch(group,
         BF    = all[grepl("_BF_", all) & !grepl("_BF_BP_", all)],
         BP    = all[grepl("_BP_", all) & !grepl("_BF_BP_", all)],
         BF_BP = all[grepl("_BF_BP_", all)]
  )
}



######################################################################################################################
############################## READ RESULTS DATA #######################
######################################################################################################################


## Read descriptive stats
Y_summary <- fread(file.path(IN_DIR, "Y_summary_c146_c147_c149.csv"))
Y_summary_BP <- fread(file.path(IN_DIR, "Y_summary_BP.csv"))
Z_summary <- fread(file.path(IN_DIR, "Z_summary_c146_c147_c149.csv"))
dt_ysrc <- fread(file.path(IN_DIR, "FABIO_Ysourcing_2012-2022_BF_byComm_inclSelf.csv"))
sankey_flows <- fread(file.path(IN_DIR, "FABIO_bfChain_2012-2022_BF_final_demand_continent.csv"))

files_tradeFeed_BF <- fabio_files("FABIO_tradeFeed", "BF")
files_tradeFeed_BP <- fabio_files("FABIO_tradeFeed", "BP")
files_feedstock_BF <- fabio_files("FABIO_feedstock", "BF")
files_totalreq_BF  <- fabio_files("FABIO_totalreq",  "BF")
# files_totalreq_BP  <- fabio_files("FABIO_totalreq",  "BP")

dt_tradeFeed    <- rbindlist(lapply(files_tradeFeed_BF, fread))
dt_material     <- subset(dt_tradeFeed, indicator == "material")
dt_tradeFeed_BP <- rbindlist(lapply(files_tradeFeed_BP, fread))
dt_feedstock    <- rbindlist(lapply(files_feedstock_BF, fread))
dt_totalreq     <- rbindlist(lapply(files_totalreq_BF,  fread))

# Build combined LC Impact terrestrial indicator (climate + acidification; ruling out "land use") ----------------

id_cols_trade <- c("country_origin", "country_consumer", "flow_type", "year",
             "allocation", "feedstock", "commodity")

lcim_terr <- dt_tradeFeed[
  indicator %in% c("LCIM_EQ_terrestrial_climate", "LCIM_EQ_terrestrial_acidification"),
  .(value = sum(value, na.rm = TRUE)),
  by = id_cols_trade
][, indicator := "LCIM_EQ_terrestrial"]

# Drop any pre-existing LCIM_EQ_terrestrial rows, bind in the recomputed ones
dt_tradeFeed <- rbindlist(
  list(dt_tradeFeed[indicator != "LCIM_EQ_terrestrial"], lcim_terr),
  use.names = TRUE
)

id_cols_feed <- c("country_consumer", "continent_origin", "year",
                  "allocation", "feedstock", "commodity")

# Build combined LC Impact terrestrial indicator (climate + acidification) in dt_feedstock ----------------
lcim_terr_feedstock <- dt_feedstock[
  indicator %in% c("LCIM_EQ_terrestrial_climate", "LCIM_EQ_terrestrial_acidification"),
  .(value = sum(value, na.rm = TRUE)),
  by = id_cols_feed
][, indicator := "LCIM_EQ_terrestrial"]

# Drop any pre-existing LCIM_EQ_terrestrial rows, bind in the recomputed ones
dt_feedstock <- rbindlist(
  list(dt_feedstock[indicator != "LCIM_EQ_terrestrial"], lcim_terr_feedstock),
  use.names = TRUE
)


######################################################################################################################
############################## DESCRIPTIVE STATS #######################
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
############################## FUNCTIONS TO PLOT RESULTS #######################
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
  
  # build the set of "top" feedstocks
  # build the candidate set of "top" feedstocks
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
  # Feedstocks whose category is "Other" (the neutrals in other_pool, e.g.
  # "Other, Waste") belong to no single crop group, so they're shown once under
  # EACH header. Detect them from what's actually present rather than testing one
  # hard-coded name, and add their colours to pal_full explicitly so they're
  # coloured even when they aren't top-N (hence absent from `pal`). The real
  # neutral level is kept out of `breaks` so it doesn't also get a third key.
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
#1. Plot countries' impact balance (exported, self, imported)
######################################################################################################################

# Helper: extract tidy plot data for one year
.balance_data <- function(data, Y_summary, year_select,
                          top_pos_n = 20, top_exp_n = 5) {
  dt <- as.data.table(data)
  Y  <- as.data.table(Y_summary)
  
  dt_year <- dt[year == year_select]
  
  origin_summary <- dt_year[
    , .(
      self    = sum(value[flow_type == "self"],  na.rm = TRUE) / 1e3,
      exports = sum(value[flow_type == "trade"], na.rm = TRUE) / 1e3
    ),
    by = country_origin
  ]
  
  import_summary <- dt_year[
    flow_type == "trade",
    .(imports = sum(value, na.rm = TRUE) / 1e3),
    by = country_consumer
  ]
  
  final <- merge(
    origin_summary, import_summary,
    by.x = "country_origin", by.y = "country_consumer",
    all = TRUE
  )
  final[is.na(final)] <- 0
  
  final[, pos := self + imports]
  setorder(final, -pos)
  top_pos <- final[1:top_pos_n]
  
  remaining <- final[!country_origin %in% top_pos$country_origin]
  setorder(remaining, -exports)
  top_exp   <- remaining[1:top_exp_n][order(exports)]
  
  plot_data <- rbind(top_pos, top_exp)
  countries <- c(top_pos$country_origin, top_exp$country_origin)
  
  plot_data[, country_origin := factor(country_origin, levels = countries)]
  
  plot_long <- melt(
    plot_data,
    id.vars      = "country_origin",
    measure.vars = c("imports", "self", "exports"),
    variable.name = "type",
    value.name   = "value"
  )
  plot_long[type == "exports", value := -value]
  plot_long[, type := factor(type, levels = c("exports", "imports", "self"))]
  
  # --- consumption line
  Y_yr <- Y[year == year_select & target_country %in% countries]
  Y_yr[, value := value / 1e3]
  prod <- Y_yr[, .(consumption = sum(value, na.rm = TRUE)),
               by = .(country_origin = target_country)]
  prod[, country_origin := factor(country_origin, levels = countries)]
  
  # scale factor for dual axis (per-year, so axes are independent)
  lu_max   <- plot_long[value > 0, sum(value), by = country_origin][, max(V1)]
  prod_max <- prod[, max(consumption, na.rm = TRUE)]
  scale_factor <- lu_max / prod_max
  
  prod[, value_scaled := consumption * scale_factor]
  
  list(
    bars         = copy(plot_long)[, year := year_select],
    lines        = copy(prod)[, year := year_select],
    scale_factor = scale_factor,
    countries    = countries
  )
}

# ── Main function ────────────────────────────────────────────────────────────
plot_balance <- function(data, Y_summary,
                         indicator = "land_harv",
                         years     = c(2012, 2022),
                         top_pos_n = 20,
                         top_exp_n = 5,
                         meta      = indicator_meta) {
  
  # ── Indicator lookup ─────────────────────────────────────────────────────
  indicator_select <- indicator           # avoid name collision with DT column
  m <- meta[indicator == indicator_select]
  if (nrow(m) == 0) {
    m <- data.table(indicator    = indicator_select,
                    scale_factor = 1,
                    y_label      = indicator_select,
                    short_label  = indicator_select)
  }
  sf_ind <- m$scale_factor[1]
  y_lab  <- m$y_label[1]
  
  dt <- as.data.table(data)
  Y  <- as.data.table(Y_summary)
  
  # ── Step 1: derive country list & ordering from the FIRST year ONLY ──────
  ref_year <- years[1]
  
  .get_plot_data <- function(year_select, countries_ref = NULL) {
    
    dt_year <- dt[year == year_select & indicator == indicator_select]
    
    origin_summary <- dt_year[
      , .(
        self    = sum(value[flow_type == "self"],  na.rm = TRUE) / sf_ind,
        exports = sum(value[flow_type == "trade"], na.rm = TRUE) / sf_ind
      ),
      by = country_origin
    ]
    
    import_summary <- dt_year[
      flow_type == "trade",
      .(imports = sum(value, na.rm = TRUE) / sf_ind),
      by = country_consumer
    ]
    
    final <- merge(
      origin_summary, import_summary,
      by.x = "country_origin", by.y = "country_consumer",
      all = TRUE
    )
    final[is.na(final)] <- 0
    
    if (is.null(countries_ref)) {
      # ── Reference year: determine the country list ──────────────────
      final[, pos := self + imports]
      setorder(final, -pos)
      top_pos <- final[1:top_pos_n]
      
      remaining <- final[!country_origin %in% top_pos$country_origin]
      setorder(remaining, -exports)
      top_exp   <- remaining[1:top_exp_n][order(exports)]
      
      countries <- c(top_pos$country_origin, top_exp$country_origin)
      plot_data <- rbind(top_pos, top_exp)
      
    } else {
      # ── Subsequent years: USE the reference country list ────────────
      countries <- countries_ref
      plot_data <- final[country_origin %in% countries]
      
      missing <- setdiff(countries, plot_data$country_origin)
      if (length(missing) > 0) {
        plot_data <- rbind(
          plot_data,
          data.table(country_origin = missing,
                     self = 0, exports = 0, imports = 0, pos = 0)
        )
      }
    }
    
    plot_data[, country_origin := factor(country_origin, levels = countries)]
    
    plot_long <- melt(
      plot_data,
      id.vars       = "country_origin",
      measure.vars  = c("imports", "self", "exports"),
      variable.name = "type",
      value.name    = "value"
    )
    plot_long[type == "exports", value := -value]
    plot_long[, type := factor(type, levels = c("exports", "imports", "self"))]
    
    # ── Consumption line (independent of the indicator) ─────────────────
    Y_yr <- Y[year == year_select & target_country %in% countries]
    Y_yr[, value := value / 1e3]
    prod <- Y_yr[, .(consumption = sum(value, na.rm = TRUE)),
                 by = .(country_origin = target_country)]
    
    missing_prod <- setdiff(countries, prod$country_origin)
    if (length(missing_prod) > 0) {
      prod <- rbind(prod,
                    data.table(country_origin = missing_prod, consumption = 0))
    }
    prod[, country_origin := factor(country_origin, levels = countries)]
    
    lu_max   <- plot_long[value > 0, sum(value), by = country_origin][, max(V1)]
    prod_max <- prod[, max(consumption, na.rm = TRUE)]
    scale_factor <- if (prod_max > 0) lu_max / prod_max else 1
    
    prod[, value_scaled := consumption * scale_factor]
    
    list(bars = plot_long, lines = prod,
         scale_factor = scale_factor, countries = countries)
  }
  
  # ── Step 2: build data for both years ────────────────────────────────────
  ref_data  <- .get_plot_data(ref_year, countries_ref = NULL)
  countries <- ref_data$countries
  
  other_data <- lapply(years[-1], .get_plot_data, countries_ref = countries)
  all_data   <- c(list(ref_data), other_data)
  names(all_data) <- as.character(years)
  
  # ── Step 3: stack & annotate with year ───────────────────────────────────
  bars_all  <- rbindlist(Map(function(d, y) d$bars[,  year := y], all_data, years))
  lines_all <- rbindlist(Map(function(d, y) d$lines[, year := y], all_data, years))
  
  bars_all[,  year := factor(year,  levels = years)]
  lines_all[, year := factor(year,  levels = years)]
  
  sf <- vapply(all_data, `[[`, numeric(1), "scale_factor")
  sf_global <- mean(sf)
  lines_all[, value_scaled_global := consumption * sf_global]
  
  # ── Step 4: single ggplot ─────────────────────────────────────────────────
  ggplot() +
    geom_bar(
      data = bars_all,
      aes(x = country_origin, y = value, fill = type),
      stat = "identity"
    ) +
    geom_segment(
      data = lines_all,
      aes(x = country_origin, xend = country_origin,
          y = 0,              yend = value_scaled_global),
      color = "black", linewidth = 0.6
    ) +
    facet_wrap(
      ~ year,
      nrow   = 2,
      axes   = "all",
      scales = "fixed"
    ) +
    scale_y_continuous(
      name     = y_lab,
      labels   = label_number(),
      sec.axis = sec_axis(~ . / sf_global,
                          name   = "Biofuel consumption (M liters)",
                          labels = label_number())
    ) +
    scale_x_discrete(name = "Country") +
    scale_fill_manual(
      values = c(imports = "#2196F3", self = "#4CAF50", exports = "#F44336"),
      name   = "Flow type"
    ) +
    theme_minimal() +
    theme(
      strip.text          = element_text(size = 11, face = "bold", hjust = 0.5),
      axis.text.x         = element_text(angle = 45, hjust = 1),
      axis.title.x.bottom = element_text(margin = margin(t = 6)),
      legend.position     = "bottom",
      legend.direction    = "horizontal",
      legend.title        = element_text(face = "bold"),
      plot.title          = element_blank(),
      plot.margin         = margin(8, 8, 8, 8)
    )
}



######################################################################################################################
#2. Plot impacts by commodity, by feedstock over time. 
######################################################################################################################

# ─── Helper: build a global feedstock palette ──────────────────────────
build_feedstock_palette <- function(data,
                                    commodities     = NULL,
                                    indicators      = NULL,
                                    top_n_feedstock = 10,
                                    other_color     = unname(feedstock_color_map[["Other"]])) {
  
  dt <- as.data.table(data)
  if (!is.null(commodities)) dt <- dt[commodity %in% commodities]
  if (!is.null(indicators))  dt <- dt[indicator %in% indicators]
  
  dt[grepl("^Animal or vegetable fats and oils", feedstock),
     feedstock := "Used Cooking Oil"]
  
  # which feedstocks make each commodity x indicator top-N (union)
  top_by_combo <- dt[, {
    rk <- .SD[, .(total = sum(value, na.rm = TRUE)), by = feedstock][order(-total)]
    list(feedstock = head(rk$feedstock, top_n_feedstock))
  }, by = .(commodity, indicator)]
  
  # order the union by overall magnitude (preserves prior stacking order)
  feedstocks <- dt[feedstock %in% unique(top_by_combo$feedstock),
                   .(total = sum(value, na.rm = TRUE)),
                   by = feedstock][order(-total), feedstock]
  
  # COLOURS come from the fixed map, not a generator/override
  pal <- get_feedstock_palette(feedstocks, feedstock_meta, other_color = other_color)
  c(pal, Other = other_color)
}

# ─── Main plotting function ────────────────────────────────────────────
plot_commodity_feedstock <- function(data,
                                     commodity_select,
                                     indicator_select,
                                     top_n_feedstock = 10,
                                     year_range      = c(2012, 2022),
                                     meta            = indicator_meta,
                                     palette         = "Set3",
                                     level_order     = NULL,
                                     Y_summary       = NULL,        # consumption data
                                     cons_scale      = 1000,        # liters -> M liters
                                     cons_color      = "black",
                                     cons_lab        = "Consumption (M liters)") {
  
  dt <- as.data.table(data)
  
  m <- meta[indicator == indicator_select]
  if (nrow(m) == 0) {
    m <- data.table(indicator    = indicator_select,
                    scale_factor = 1,
                    y_label      = indicator_select,
                    short_label  = indicator_select)
  }
  
  dt_sub <- dt[commodity == commodity_select &
                 indicator == indicator_select &
                 year %between% year_range]
  
  if (nrow(dt_sub) == 0) {
    warning("No rows for commodity '", commodity_select,
            "' and indicator '", indicator_select, "'.")
    return(invisible(NULL))
  }
  
  dt_sub[grepl("^Animal or vegetable fats and oils", feedstock),
         feedstock := "Used Cooking Oil"]
  
  is_named_palette <- is.character(palette) && !is.null(names(palette))
  
  if (is_named_palette) {
    top_feedstocks <- setdiff(names(palette), "Other")
  } else {
    top_feedstocks <- dt_sub[, .(total = sum(value, na.rm = TRUE)),
                             by = feedstock
    ][order(-total)
    ][seq_len(min(top_n_feedstock, .N)), feedstock]
  }
  
  dt_sub[, feedstock_group := fifelse(feedstock %in% top_feedstocks,
                                      feedstock, "Other")]
  
  agg <- dt_sub[, .(value = sum(value, na.rm = TRUE) / m$scale_factor),
                by = .(year, feedstock_group)]
  
  if (is_named_palette) {
    levels_use <- if (!is.null(level_order)) level_order else names(palette)
    agg[, feedstock_group := factor(feedstock_group, levels = levels_use)]
    fill_scale <- scale_fill_manual(values = palette, name = "Feedstock",
                                    drop  = TRUE,
                                    guide = guide_legend(reverse = FALSE))
  } else {
    feedstock_order <- agg[, .(total = sum(value)), by = feedstock_group
    ][rev(order(-total))]$feedstock_group
    agg[, feedstock_group := factor(feedstock_group, levels = feedstock_order)]
    fill_scale <- scale_fill_brewer(palette = palette, name = "Feedstock")
  }
  
  # ─── Consumption time-series for the secondary axis ──────────────────
  cons <- NULL
  if (!is.null(Y_summary)) {
    yc <- as.data.table(Y_summary)[comm_code == commodity_select &
                                     year %between% year_range,
                                   .(cons = sum(value, na.rm = TRUE) / cons_scale),
                                   by = year]
    if (nrow(yc)) {
      # Scale consumption onto the bar (primary) axis range so sec_axis can
      # invert it. ratio = max(stacked total) / max(consumption).
      bar_max <- agg[, .(t = sum(value)), by = year][, max(t, na.rm = TRUE)]
      cons_max <- yc[, max(cons, na.rm = TRUE)]
      ratio    <- if (cons_max > 0) bar_max / cons_max else 1
      yc[, cons_scaled := cons * ratio]
      yc[, year := factor(year, levels = sort(unique(agg$year)))]
      cons <- yc
    }
  }
  
  agg[, year := factor(year, levels = sort(unique(agg$year)))]
  
  commodity_name <- items_full_bcp[comm_code == commodity_select, item]
  
  p <- ggplot(agg, aes(x = year, y = value, fill = feedstock_group)) +
    geom_bar(stat = "identity") +
    fill_scale +
    labs(
      x     = "Year",
      y     = m$y_label,
      title = paste0(m$short_label, " by feedstock — ", commodity_name)
    ) +
    theme_minimal()
  
  if (!is.null(cons)) {
    p <- p +
      geom_line(data = cons, aes(x = year, y = cons_scaled, group = 1),
                inherit.aes = FALSE, colour = cons_color, linewidth = 0.4) +
      geom_point(data = cons, aes(x = year, y = cons_scaled),
                 inherit.aes = FALSE, colour = cons_color, size = 0.9) +
      scale_y_continuous(
        labels   = scales::label_number(),
        sec.axis = sec_axis(~ . / ratio, name = cons_lab,
                            labels = scales::label_number())
      )
  } else {
    p <- p + scale_y_continuous(labels = scales::label_number())
  }
  
  p
}

# ─── Convenience wrapper for batch plotting ────────────────────────────

plot_commodity_feedstock_grid <- function(data, commodities, indicators,
                                          palette         = NULL,
                                          top_n_feedstock = 10,
                                          Y_summary       = NULL,
                                          ...) {
  
  if (is.null(palette) || is.null(names(palette))) {
    palette <- build_feedstock_palette(
      data,
      commodities     = commodities,
      indicators      = indicators,
      top_n_feedstock = top_n_feedstock
    )
  }
  
  leftmost_ind  <- indicators[1]
  palette_feeds <- setdiff(names(palette), "Other")
  
  # One horizontal row of plots per commodity.
  row_stacks <- lapply(commodities, function(comm) {
    
    # Anchor stack order to the leftmost indicator's 2012 totals.
    dt_anchor <- as.data.table(data)[
      commodity == comm & indicator == leftmost_ind & year == 2012
    ]
    if (nrow(dt_anchor)) {
      dt_anchor[grepl("^Animal or vegetable fats and oils", feedstock),
                feedstock := "Used Cooking Oil"]
      dt_anchor[, fg := fifelse(feedstock %in% palette_feeds,
                                feedstock, "Other")]
      anchor_order <- dt_anchor[, .(total = sum(value, na.rm = TRUE)),
                                by = fg][order(total), fg]
    } else {
      anchor_order <- character(0)
    }
    
    # Pad with palette names absent from the anchor so every plot in the
    # row shares the same complete factor; `drop = TRUE` hides absentees
    # from the collected row legend.
    level_order <- c(anchor_order,
                     setdiff(c(palette_feeds, "Other"), anchor_order))
    
    plist <- lapply(indicators, function(ind) {
      plot_commodity_feedstock(data, comm, ind,
                               palette         = palette,
                               top_n_feedstock = top_n_feedstock,
                               level_order     = level_order,
                               Y_summary       = Y_summary,
                               ...)
    })
    patchwork::wrap_plots(plist, nrow = 1) +
      patchwork::plot_layout(guides = "collect") &
      ggplot2::theme(legend.position = "left")
  })
  
  patchwork::wrap_plots(row_stacks, ncol = 1)
}





######################################################################################################################
# Plot impacts by commodity x indicator over time (no feedstock detail).
######################################################################################################################

plot_commodity <- function(data,
                           commodity_select,
                           indicator_select,
                           year_range = c(2012, 2022),
                           meta       = indicator_meta,
                           bar_color  = "#4575B4") {
  
  dt <- as.data.table(data)
  
  m <- meta[indicator == indicator_select]
  if (nrow(m) == 0) {
    m <- data.table(indicator    = indicator_select,
                    scale_factor = 1,
                    y_label      = indicator_select,
                    short_label  = indicator_select)
  }
  
  dt_sub <- dt[commodity == commodity_select &
                 indicator == indicator_select &
                 year %between% year_range]
  
  if (nrow(dt_sub) == 0) return(invisible(NULL))
  
  agg <- dt_sub[, .(value = sum(value, na.rm = TRUE) / m$scale_factor),
                by = .(year)]
  agg[, year := factor(year, levels = sort(unique(year)))]
  
  commodity_name <- items_full_bcp[comm_code == commodity_select, item]
  
  ggplot(agg, aes(x = year, y = value)) +
    geom_bar(stat = "identity", fill = bar_color) +
    scale_y_continuous(labels = scales::label_number()) +
    labs(
      x     = "Year",
      y     = m$y_label,
      title = paste0(m$short_label, " — ", commodity_name)
    ) +
    theme_minimal()
}

# ─── Grid wrapper ──────────────────────────────────────────────────────
# Rows = commodities, columns = indicators. Commodities with no data for
# any indicator in `year_range` are dropped (no empty row).
plot_commodity_grid <- function(data, commodities, indicators,
                                year_range = c(2012, 2022),
                                bar_color  = "#4575B4",
                                ...) {
  
  dt <- as.data.table(data)
  
  # Keep only commodities that have at least one row across the indicators.
  has_data <- vapply(commodities, function(comm) {
    nrow(dt[commodity == comm & indicator %in% indicators &
              year %between% year_range]) > 0
  }, logical(1))
  
  commodities <- commodities[has_data]
  if (length(commodities) == 0) {
    warning("No commodities with data in the requested range.")
    return(invisible(NULL))
  }
  
  row_stacks <- lapply(commodities, function(comm) {
    plist <- lapply(indicators, function(ind) {
      plot_commodity(data, comm, ind,
                     year_range = year_range,
                     bar_color  = bar_color,
                     ...)
    })
    # Replace NULL panels (commodity x indicator with no data) with spacers.
    plist <- lapply(plist, function(p) if (is.null(p)) patchwork::plot_spacer() else p)
    patchwork::wrap_plots(plist, nrow = 1)
  })
  
  patchwork::wrap_plots(row_stacks, ncol = 1)
}


######################################################################################################################
############################## INTER-REGIONAL IMPACT FLOWS #######################
######################################################################################################################

plot_continent_heatmap <- function(dt_feedstock,
                                   years_base,
                                   years_curr,
                                   indicator,
                                   ref_max   = NULL,  # reference max defining the colour curve; SHARE across indicators
                                   gamma     = 1,     # >1 keeps darker colours until larger values (rightward shift)
                                   region_order = NULL, # fix continent axis order (e.g. land_harv's); NULL = self self-flow
                                   save_dir  = file.path("output", "plot"),
                                   save      = TRUE,
                                   breaks    = NULL,  # override auto breaks
                                   limits    = NULL) {
  
  # --- Map consumer country -> continent, aggregate ------------------------
  dt_feedstock <- left_join(
    dt_feedstock,
    regions %>% select(country_consumer = iso3c, continent_target = continent),
    by = "country_consumer"
  ) %>%
    mutate(continent_target = if_else(country_consumer == "EU27", "EU", continent_target)) %>%
    group_by(continent_target, continent_origin, commodity, year, feedstock, indicator) %>%
    summarize(value = sum(value), .groups = "drop") %>%
    setDT()
  
  # --- Per-indicator config ------------------------------------------------
  cfg_all <- list(
    ibif_total = list(
      div          = 1e3,
      legend_title = "Means species abundance loss (1000 MSA·km²·yr, annual mean)",
      file_suffix  = "IBIF_flows_continent"
    ),
    LCIM_EQ_terrestrial = list(
      div          = 1e-6,
      legend_title = "Terrestrial ecosystem damage (1/1,000,000 PDF·yr, annual mean)",
      file_suffix  = "LCIM_terrestrial_flows_continent"
    ),
    land_harv = list(
      div          = 1e3,
      legend_title = "Land use (1000 ha, annual mean)",
      file_suffix  = "LU_flows_continent"
    )
  )
  
  if (!indicator %in% names(cfg_all)) {
    stop("Unknown indicator '", indicator, "'. Known: ",
         paste(names(cfg_all), collapse = ", "))
  }
  cfg <- cfg_all[[indicator]]
  
  # --- Filter & aggregate --------------------------------------------------
  ind    <- indicator                              # avoid i-expr name clash
  dt_sub <- dt_feedstock[indicator == ind]
  
  dt_heatmap <- dt_sub[, .(value = sum(value) / cfg$div),
                       by = .(continent_origin, continent_target, year)]
  dt_heatmap <- dt_heatmap[
    CJ(continent_origin, continent_target, year, unique = TRUE),
    on = .(continent_origin, continent_target, year)
  ][is.na(value), value := 0]
  
  # --- Smoothed endpoints --------------------------------------------------
  dt_heatmap_base <- dt_heatmap[year %in% years_base,
                                .(value = mean(value)),
                                by = .(continent_origin, continent_target)]
  dt_heatmap_curr <- dt_heatmap[year %in% years_curr,
                                .(value = mean(value)),
                                by = .(continent_origin, continent_target)]
  
  label_base <- paste0(min(years_base), "-", max(years_base))
  label_curr <- paste0(min(years_curr), "-", max(years_curr))
  
  dt_heatmap_base[, period := label_base]
  dt_heatmap_curr[, period := label_curr]
  
  dt_both <- rbind(dt_heatmap_base, dt_heatmap_curr)
  
  # --- Reference-anchored colour scale (SHARED across indicators) ----------
  # Reproduces land_harv's exact log1p curve, but keyed to a REFERENCE max so
  # the SAME curve can be reused for other indicators. A cell at ratio
  # r = value / val_max is coloured at
  #
  #     t(r) = log1p(r * ref_max) / log1p(ref_max)     (clamped to [0,1])
  #
  # which depends only on r and the shared ref_max -> "30x below max" gets the
  # same colour for every indicator. When ref_max == val_max (the default, i.e.
  # each indicator against itself), this is BYTE-FOR-BYTE the original log1p
  # appearance: low values stay in the blues, exactly as before. Larger ref_max
  # -> lows brighter; smaller ref_max -> lows darker. Pass land_harv's val_max
  # as ref_max to make ibif_total / LCIM_EQ_terrestrial inherit its look.
  #
  # `gamma` then distorts that curve: t -> t^gamma. gamma > 1 (e.g. 1.5) holds
  # the dark blues until larger values before ramping into the greens/yellows;
  # gamma < 1 does the opposite. gamma = 1 leaves the reference curve untouched.
  pos_vals <- dt_both$value[dt_both$value > 0 & is.finite(dt_both$value)]
  val_max  <- max(pos_vals, na.rm = TRUE)
  if (is.null(ref_max)) ref_max <- val_max        # default: self -> original per-indicator look
  
  if (is.null(limits)) limits <- c(0, val_max)
  if (is.null(breaks)) {
    high_pow <- ceiling(log10(val_max))
    low_pow  <- max(0, high_pow - 4)
    breaks   <- c(0, 10^(low_pow:high_pow))
  }
  
  ratio_rescaler <- function(x, to = c(0, 1), from = c(0, val_max)) {
    r <- x / from[2]                              # ratio to this indicator's max
    t <- log1p(r * ref_max) / log1p(ref_max)      # land_harv's curve, keyed to ref_max
    pmin(pmax(t, 0), 1) ^ gamma                   # gamma > 1 -> darker until larger values
  }
  
  # --- Continent ordering ---------------------------------------------------
  # Default: order by base-period self-flow (this indicator). If `region_order`
  # is supplied (e.g. land_harv's order), use it verbatim so every indicator
  # shares one axis order; any continents present but missing from it are
  # appended at the end rather than dropped.
  present <- setdiff(union(unique(dt_both$continent_origin),
                           unique(dt_both$continent_target)), "Total")
  if (is.null(region_order)) {
    diag_base  <- dt_heatmap_base[continent_origin == continent_target,
                                  .(diag_val = sum(value)),
                                  by = continent_origin][order(-diag_val)]
    cont_order <- diag_base$continent_origin
  } else {
    cont_order <- as.character(region_order)
  }
  cont_order <- c(intersect(cont_order, present), setdiff(present, cont_order))
  
  # --- Marginal sums -------------------------------------------------------
  rs <- dt_both[, .(value = sum(value)), by = .(continent_origin, period)]
  rs[, continent_target := "Total"]
  
  cs <- dt_both[, .(value = sum(value)), by = .(continent_target, period)]
  cs[, continent_origin := "Total"]
  
  gt <- dt_both[, .(value = sum(value)), by = period]
  gt[, `:=`(continent_origin = "Total", continent_target = "Total")]
  
  to_char <- function(d) {
    d[, continent_origin := as.character(continent_origin)]
    d[, continent_target := as.character(continent_target)]
    d[, period           := as.character(period)]
    invisible(d)
  }
  to_char(dt_both); to_char(rs); to_char(cs); to_char(gt)
  
  target_levels <- c(cont_order, "Total")
  origin_levels <- c("Total", rev(cont_order))
  
  # --- Magnitude-aware cell-label formatter --------------------------------
  fmt_cell <- function(x) {
    if      (val_max < 1)   sprintf("%.3f", x)
    else if (val_max < 10)  sprintf("%.2f", x)
    else if (val_max < 100) sprintf("%.1f", x)
    else                    formatC(round(x), big.mark = ",", format = "d")
  }
  
  # --- Plot ----------------------------------------------------------------
  p <- ggplot(dt_both, aes(continent_target, continent_origin, fill = value)) +
    geom_tile() +
    geom_text(aes(label = fmt_cell(value)), size = 3) +
    geom_text(data = rs, aes(x = continent_target, y = continent_origin,
                             label = fmt_cell(value)),
              inherit.aes = FALSE, size = 3, fontface = "bold") +
    geom_text(data = cs, aes(x = continent_target, y = continent_origin,
                             label = fmt_cell(value)),
              inherit.aes = FALSE, size = 3, fontface = "bold") +
    geom_text(data = gt, aes(x = continent_target, y = continent_origin,
                             label = fmt_cell(value)),
              inherit.aes = FALSE, size = 3, fontface = "bold") +
    scale_fill_viridis_c(
      limits   = limits,
      breaks   = breaks,
      labels   = scales::comma,
      oob      = scales::squish,
      rescaler = ratio_rescaler
    ) +
    scale_x_discrete(limits = target_levels, position = "top") +
    scale_y_discrete(limits = origin_levels) +
    facet_wrap(~ period, axes = "all") +
    labs(x     = NULL,
         y     = "Impacted region",
         title = "Biofuel consumer region",
         fill  = cfg$legend_title) +
    theme_minimal() +
    theme(plot.title       = element_text(hjust = 0.5, size = 10,
                                          margin = margin(b = 18)),
          axis.title.y     = element_text(margin = margin(r = 10), size = 10),
          legend.position  = "bottom",
          legend.key.width = unit(2, "cm"),
          panel.spacing    = unit(1, "cm"),
          strip.placement  = "outside",
          panel.grid       = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    guides(fill = guide_colorbar(direction      = "horizontal",
                                 title.position = "bottom",
                                 title.hjust    = 0.5,
                                 title.theme    = element_text(size = 9)))
  
  # --- Save ----------------------------------------------------------------
  if (save) {
    fname <- paste0(label_base, "_vs_", label_curr, "_", cfg$file_suffix, ".svg")
    ggsave(filename = file.path(save_dir, fname),
           plot = p, width = 10, height = 6, dpi = 300)
  }
  
  attr(p, "cont_order") <- cont_order   # read back with attr(p, "cont_order")
  p
}


######################################################################################################################
############################## PLOT RESULTS #######################
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
#1. Plot countries' impact balance (exported, self, imported)
######################################################################################################################

plot_netLU_2012_2022 <- plot_balance(dt_tradeFeed, Y_summary)      

plot_ibif_2012_2022 <- plot_balance(dt_tradeFeed, Y_summary, indicator = "ibif_total")

plot_lcim_terrestrial_2012_2022 <- plot_balance(dt_tradeFeed, Y_summary, indicator = "LCIM_EQ_terrestrial")



######################################################################################################################
#2. Plot impacts by commodity, by feedstock over time. 
######################################################################################################################

# 
# str(dt_totalreq)
# 
# 
# source("R/00_system_variables.R")
# 
# setwd(fabio_root)
# # Read labels ------------------------------------------------------------------
# input_path <- "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/"
# L22 <- readRDS(paste0(input_path,"2022_", "L_", "value", ".rds"))
# L12 <- readRDS(paste0(input_path,"2012_", "L_", "value", ".rds"))
# input_path <- "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/"
# io <- fread(paste0(input_path,"io_labels.csv"))
# io_names <- io[, paste0(iso3c, "_", comm_code)]
# 
# library(Matrix)
# colnames(L22) <- rownames(L22) <- colnames(L12) <- rownames(L12) <- io_names
# 
# contrast_country_bfp <- function(L_late, L_early,
#                                  iso3,
#                                  comm_codes = c(146, 147, 149),
#                                  threshold  = 0.01,
#                                  fill       = 0) {
#   
#   pattern <- paste0("^", iso3, "_c(", paste(comm_codes, collapse = "|"), ")$")
#   
#   extract <- function(L) {
#     col_idx <- grep(pattern, colnames(L))
#     if (!length(col_idx)) stop("No columns match pattern: ", pattern)
#     sub  <- L[, col_idx, drop = FALSE]
#     keep <- sub@x >= threshold
#     rows <- sort.int(unique(sub@i[keep])) + 1L
#     as.matrix(sub[rows, , drop = FALSE])
#   }
#   
#   agg <- function(M) {
#     suffix <- substring(rownames(M), nchar(rownames(M)) - 4L)
#     rowsum(M, group = suffix)
#   }
#   
#   A <- agg(extract(L_late))
#   B <- agg(extract(L_early))
#   
#   rows <- union(rownames(A), rownames(B))
#   align <- function(M) {
#     out <- matrix(fill, length(rows), ncol(M),
#                   dimnames = list(rows, colnames(M)))
#     out[rownames(M), ] <- M
#     out
#   }
#   
#   align(A) - align(B)
# }
# 
# # usage
# L_diff_USA <- contrast_country_bfp(L22, L12, iso3 = "USA")
# L_diff_BRA <- contrast_country_bfp(L22, L12, iso3 = "BRA")
# L_diff_ARG <- contrast_country_bfp(L22, L12, iso3 = "ARG")
# L_diff_DEU <- contrast_country_bfp(L22, L12, iso3 = "DEU")
# 
# 
# 
# 

p_ibif <- plot_commodity_feedstock_grid(
  dt_tradeFeed,
  commodities = c("c146", "c147", "c149"),
  indicators  = "ibif_total",
  Y_summary = Y_summary,
  top_n_feedstock = 7
)

p_lcim <- plot_commodity_feedstock_grid(
  dt_tradeFeed,
  commodities = c("c146", "c147", "c149"),
  indicators  = "LCIM_EQ_terrestrial",
  Y_summary = Y_summary,
  top_n_feedstock = 7
)

p_ibif_lcim <- plot_commodity_feedstock_grid(
  dt_tradeFeed,
  commodities = c("c146", "c147", "c149"),
  indicators  = c("ibif_total","LCIM_EQ_terrestrial"),
  Y_summary = Y_summary,
  top_n_feedstock = 7
)

p_grid <- plot_commodity_feedstock_grid(
  dt_tradeFeed,
  commodities = c("c146", "c147", "c149"),
  indicators  = c("ibif_total", "land_harv"),
  Y_summary = Y_summary,
  top_n_feedstock = 7
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
  indicators  = "ibif_total"
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

######################################################################################################################
############################## SAVE PLOTS #######################
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


# ggsave(filename = file.path("output", "plot", "c146_feedstock.svg"),
#        plot = plot_commodity_feedstock(tab_land_use, "c146"),
#        width = 10, height = 6, dpi = 300)
# 
# ggsave(filename = file.path("output", "plot", "c147_feedstock.svg"),
#        plot = plot_commodity_feedstock(tab_land_use, "c147"),
#        width = 10, height = 6, dpi = 300)
# 
# ggsave(filename = file.path("output", "plot", "c149_feedstock.svg"),
#        plot = plot_commodity_feedstock(tab_land_use, "c149"),
#        width = 10, height = 6, dpi = 300)









######################################################################################################################
############################## READ RESULTS DATA #######################
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


# ######## ######## ######## ######## ######## ######## ######## 
# ######## Correlations between main biodiv indicators ######## 
# ######## ######## ######## ######## ######## ######## ######## 
# # global totals per indicator × year
# dt_global <- dt_tradeFeed[indicator %in% indicators_sel,
#                           .(value = sum(value, na.rm = TRUE)),
#                           by = .(indicator, year)]
# 
# # wide format: one column per indicator
# dt_wide <- dcast(dt_global, year ~ indicator, value.var = "value")
# 
# # correlation matrix (across years, i.e. each row = one year)
# cor_mat <- cor(dt_wide[, -"year"], use = "pairwise.complete.obs")
# 
# ######## ######## ######## ######## ######## ######## ######## 
# ######## Correlations between subindicators ######## 
# ######## ######## ######## ######## ######## ######## ######## 
# 
# # ── 0. Helper: expand each indicator to its sub-indicators ──────────────────
# # Indicators absent from sub_map$parent are their own sub-indicator
# indicator_subs <- lapply(setNames(indicators_sel, indicators_sel), function(ind) {
#   subs <- sub_map[parent == ind, indicator]
#   if (length(subs) == 0L) ind else subs          # self if no children
# })
# 
# all_subs <- unique(unlist(indicator_subs))        # every leaf we need
# 
# # ── 1. Global totals for sub-indicators ─────────────────────────────────────
# dt_sub_global <- dt_tradeFeed[
#   indicator %in% all_subs,
#   .(value = sum(value, na.rm = TRUE)),
#   by = .(indicator, year)
# ]
# 
# dt_sub_wide <- dcast(dt_sub_global, year ~ indicator, value.var = "value")
# sub_cols    <- setdiff(names(dt_sub_wide), "year")
# 
# # ── 2. Correlation matrix between sub-indicators ─────────────────────────────
# cor_sub <- cor(dt_sub_wide[, ..sub_cols],
#                use = "pairwise.complete.obs")   # or "complete.obs"
# # ── 3. SD of each parent indicator (for normalisation) ──────────────────────
# # Re-use dt_wide already computed from your snippet above
# sd_parent <- sapply(indicators_sel, function(ind) {
#   sd(dt_wide[[ind]], na.rm = TRUE)
# })
# 
# # ── 4. Build pairwise sub-indicator decomposition table ─────────────────────
# decomp <- rbindlist(lapply(indicators_sel, function(ind_a) {
#   rbindlist(lapply(indicators_sel, function(ind_b) {
#     subs_a <- indicator_subs[[ind_a]]
#     subs_b <- indicator_subs[[ind_b]]
#     CJ(sub_a = subs_a, sub_b = subs_b)[,
#                                        `:=`(
#                                          indicator_a  = ind_a,
#                                          indicator_b  = ind_b,
#                                          cor_sub      = mapply(function(sa, sb) cor_sub[sa, sb], sub_a, sub_b),
#                                          contribution = mapply(function(sa, sb)
#                                            cor_sub[sa, sb] / (length(subs_a) * length(subs_b)),   # see note
#                                            sub_a, sub_b)
#                                        )]
#   }))
# }))
# 
# setcolorder(decomp, c("indicator_a", "indicator_b", "sub_a", "sub_b",
#                       "cor_sub", "contribution"))
# 
# 
# 
# 
# library(corrplot)
# 
# # Build ordered index: ibif_ first, then LCIM_EQ_, then FD_EQ_
# parent_order <- c(
#   indicator_subs[["ibif_total"]],
#   indicator_subs[["LCIM_EQ_terrestrial"]],
#   indicator_subs[["FD_EQ_ric_terrestrial"]]
# )
# 
# # Subset & reorder the matrix
# cor_sub_ord <- cor_sub[parent_order, parent_order]
# 
# pdf("output/plot/cor_indicators.svg", width = 10, height = 9)
# 
# cor_subindicators <- corrplot(
#   cor_sub_ord,
#   method      = "color",
#   type        = "upper",
#   order       = "original",        # ← respect our manual ordering
#   addCoef.col = "black",
#   number.cex  = 0.7,
#   tl.cex      = 0.8,
#   tl.col      = "black",
#   tl.srt      = 45,
#   col         = colorRampPalette(c("#d73027", "#f7f7f7", "#1a9850"))(200),
#   title       = "Sub-indicator correlation matrix",
#   mar         = c(0, 0, 2, 0)
# )
# 
# dev.off() 
# 
# 
# 



# plot_feedstock_with_impact <- function(bar_data, impact_data, regions,
#                                        target_comm_in, target_continent_in,
#                                        indicator_select,
#                                        n_top = 12, include_other = TRUE,
#                                        meta = indicator_meta) {
#   
#   # ── bars: feedstock mass (top-N + Other) ─────────────────────────────
#   d <- bar_data %>%
#     filter(target_comm == target_comm_in,
#            target_continent == target_continent_in)
#   if (nrow(d) == 0) stop("No feedstock rows for that target_comm / target_continent.")
#   
#   top_feedstocks <- d %>%
#     group_by(origin_comm_name) %>%
#     summarize(total = sum(value), .groups = "drop") %>%
#     slice_max(total, n = n_top) %>%
#     arrange(desc(total)) %>%
#     pull(origin_comm_name)
#   
#   d_plot <- d %>%
#     mutate(feedstock = ifelse(origin_comm_name %in% top_feedstocks,
#                               origin_comm_name, "Other")) %>%
#     { if (!include_other) filter(., feedstock != "Other") else . } %>%
#     mutate(feedstock = factor(feedstock,
#                               levels = c(top_feedstocks,
#                                          if (include_other) "Other"))) %>%
#     group_by(year, feedstock) %>%
#     summarize(value = sum(value), .groups = "drop")
#   
#   # ── line: total impact across feedstocks for the continent ──────────
#   m <- meta[indicator == indicator_select]
#   if (nrow(m) == 0) {
#     m <- data.table(indicator    = indicator_select,
#                     scale_factor = 1,
#                     y_label      = indicator_select,
#                     short_label  = indicator_select)
#   }
#   
#   impact_line <- impact_data %>%
#     left_join(regions %>% select(iso3c, continent),
#               by = c("country_consumer" = "iso3c")) %>%
#     rename(target_continent = continent) %>%
#     filter(commodity == target_comm_in,
#            indicator == indicator_select,
#            target_continent == target_continent_in) %>%
#     group_by(year) %>%
#     summarize(impact = sum(value, na.rm = TRUE) / m$scale_factor,
#               .groups = "drop")
#   
#   if (nrow(impact_line) == 0) {
#     warning("No impact rows for commodity '", target_comm_in,
#             "', indicator '", indicator_select,
#             "', continent '", target_continent_in, "'.")
#   }
#   
#   # ── dual y-axis: scale impact line into bar y-range ─────────────────
#   bar_max  <- d_plot %>% group_by(year) %>%
#     summarize(t = sum(value), .groups = "drop") %>%
#     pull(t) %>% max(na.rm = TRUE)
#   line_max <- if (nrow(impact_line)) max(impact_line$impact, na.rm = TRUE) else 1
#   k <- if (is.finite(line_max) && line_max > 0) bar_max / line_max else 1
#   
#   # ── palette (no greens, grey for Other) ─────────────────────────────
#   pal <- c("#1f78b4", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3",
#            "#fb9a99", "#fdbf6f", "#cab2d6", "#b15928", "#f781bf",
#            "#8da0cb", "#bc80bd")[seq_len(length(top_feedstocks))]
#   names(pal) <- top_feedstocks
#   if (include_other) pal <- c(pal, Other = "grey70")
#   
#   ggplot() +
#     geom_col(data = d_plot,
#              aes(year, value, fill = feedstock),
#              position = position_stack(reverse = TRUE)) +
#     geom_line(data  = impact_line,
#               aes(year, impact * k),
#               colour = "black", linewidth = 0.7) +
#     geom_point(data = impact_line,
#                aes(year, impact * k),
#                colour = "black", size = 1.6) +
#     scale_fill_manual(values = pal, name = "Feedstock") +
#     scale_x_continuous(breaks = scales::pretty_breaks()) +
#     scale_y_continuous(
#       name     = "Feedstock mass embodied in biofuel consumption (ktonnes)",
#       sec.axis = sec_axis(~ . / k, name = paste0(m$y_label, " (total)"))
#     ) +
#     guides(fill = guide_legend(reverse = TRUE)) +
#     labs(title = paste0(target_comm_in, " — ", target_continent_in,
#                         " · ", m$short_label),
#          x = NULL) +
#     theme_minimal(base_size = 11) +
#     theme(panel.grid.minor = element_blank(),
#           legend.key.size  = unit(0.4, "cm"))
# }
# 
# # usage
# plot_feedstock_with_impact(
#   bar_data            = Z_per_year,
#   impact_data         = dt_tradeFeed,             # has target_country, commodity, indicator, year, value (and feedstock)
#   regions             = regions,
#   target_comm_in      = "c146",
#   target_continent_in = "EU",
#   indicator_select    = "ibif_total"
# )
# 
# plot_feedstock_with_impact(
#   bar_data            = Z_per_year,
#   impact_data         = dt_tradeFeed,             # has target_country, commodity, indicator, year, value (and feedstock)
#   regions             = regions,
#   target_comm_in      = "c147",
#   target_continent_in = "EU",
#   indicator_select    = "ibif_total"
# )
# 
# plot_feedstock_with_impact(
#   bar_data            = Z_per_year,
#   impact_data         = dt_tradeFeed,             # has target_country, commodity, indicator, year, value (and feedstock)
#   regions             = regions,
#   target_comm_in      = "c149",
#   target_continent_in = "EU",
#   indicator_select    = "ibif_total"
# )
# 
# 
# plot_feedstock_with_impact(
#   bar_data            = Z_per_year,
#   impact_data         = dt_tradeFeed,             # has target_country, commodity, indicator, year, value (and feedstock)
#   regions             = regions,
#   target_comm_in      = "c147",
#   target_continent_in = "EU",
#   indicator_select    = "LCIM_EQ_terrestrial"
# )
# 
# plot_feedstock_with_impact(
#   bar_data            = Z_per_year,
#   impact_data         = dt_tradeFeed,             # has target_country, commodity, indicator, year, value (and feedstock)
#   regions             = regions,
#   target_comm_in      = "c149",
#   target_continent_in = "EU",
#   indicator_select    = "LCIM_EQ_terrestrial"
# )




######################################################################################################################
############################## SOURCING DECOMPOSITION: Domestic / Intra- / Extra-regional #############################
######################################################################################################################
# Two plotting functions that classify where consumed biofuel (or its embodied
# feedstock) comes from, relative to the consumer's own continent:
#   - Domestic       : flow_type == "self"
#   - Intra-regional : same continent, but not self
#   - Extra-regional : different continent (incl. unmatched-continent origins)
#
#   plot_sourcing_feedstock()  -> *_byComm_byFeed_direct_bilat.csv  (feedstock mass)
#   plot_sourcing_product()    -> FABIO_Ysourcing_*_byComm_inclSelf.csv (final product)
#
# Both share the same core (.sourcing_shares + .plot_sourcing).

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
  # Summing the absolute `value` across all consumer countries in a continent
  # automatically weights each country by its own volume, so large consumers
  # dominate the continent share. (Deliberately use `value`, NOT any per-country
  # `share` column, otherwise every country would count equally.)
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
# Input: rbindlist of FABIO_tradeFeed_<year>_material_value_BF_byComm_byFeed_direct_bilat.csv
# Columns: country_origin, country_consumer, flow_type, year, indicator, allocation,
#          feedstock, commodity, value   (feedstock is summed over automatically)

plot_sourcing_feedstock <- function(data, regions,
                                    commodities     = NULL,   # e.g. c("c146","c147","c149"); NULL = all
                                    continents      = NULL,   # e.g. c("EU","ASI","NAM"); NULL = all
                                    indicator_keep  = NULL,   # e.g. "material" (guards against double counting)
                                    allocation_keep = NULL,   # e.g. "value"
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
#          (uses `value`, not `share`, for the cross-country volume weighting)

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
p_feed_src <- plot_sourcing_feedstock(
  dt_material, regions,
  commodities    = c("c146", "c147", "c149"),
  continents     = c("EU", "ASI", "NAM", "LAM"),
  indicator_keep = "material",
  allocation_keep = "value")

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
################################# FLOW CHART #################################
######################################################################################################################




# NEW (tcf):  stage-1 flows can be converted from FEEDSTOCK USE (tonnes of feedstock) to
#             BIOFUEL SUPPLY (biofuel output the feedstock yields) via a technical conversion
#             factor table. Applied PER (feedstock x biofuel) on the un-aggregated rows, so a
#             producer running several feedstocks with different yields is handled correctly.
#             With balanced units, the producer node's inflow == outflow (mass conservation),
#             so the feedstock/biofuel "gap" at the conversion node closes.
#
#             biofuel_supply = feedstock_use * cf        (cf = biofuel output per unit feedstock,
#                                                          i.e. an extraction/yield rate, output/input)
#             If your tcf is stored the other way round (feedstock per unit biofuel, an input
#             coefficient), pass tcf_invert = TRUE.
#
#             To actually SEE the balance, render with normalize = "raw" (or "match_stage1",
#             which is now equivalent). normalize = "per_stage" re-normalises each stage to 1
#             independently and will hide the per-producer balance.

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
  # Done BEFORE the link aggregation below, while each row is still a single feedstock,
  # so heterogeneous feedstock mixes per producer convert with their own factor.
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

# Build the two Sankey plots (NO ggtitle)
p1 <- bf_sankey_gg(
  sankey_flows,
  year_sel    = 2020:2022,
  biofuel_sel = "c146",
  cont_order  = c("LAM", "NAM", "ASI", "EU"),
  tcf = tcf, 
  tcf_val_col = "output_qty",
  normalize = "raw"
)

p2 <- bf_sankey_gg(
  sankey_flows,
  year_sel    = 2020:2022,
  biofuel_sel = c("c147", "c149"),
  cont_order  = c("LAM", "NAM", "ASI", "EU"),
  tcf = tcf, 
  tcf_val_col = "output_qty",
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








# 
# plot_consumption_vs_impact_by_continent <- function(
    #     Y_data, impact_data, regions,
#     target_comm_in, indicator_select,
#     meta      = indicator_meta,
#     y_label   = "Biofuel consumption (M liters)",
#     grid_nrow = NULL,
#     grid_ncol = NULL) {
#   
#   # ── grid path: vectorised target_comm_in ───────────────────────────
#   if (length(target_comm_in) > 1) {
#     plots <- lapply(target_comm_in, function(cc) {
#       plot_consumption_vs_impact_by_continent(
#         Y_data           = Y_data,
#         impact_data      = impact_data,
#         regions          = regions,
#         target_comm_in   = cc,
#         indicator_select = indicator_select,
#         meta             = meta,
#         y_label          = y_label
#       )
#     })
#     if (is.null(grid_nrow) && is.null(grid_ncol)) grid_nrow <- 1
#     
#     return(
#       patchwork::wrap_plots(plots, nrow = grid_nrow, ncol = grid_ncol) +
#         patchwork::plot_layout(guides = "collect") &
#         ggplot2::theme(
#           legend.position      = "bottom",
#           legend.box           = "horizontal",
#           legend.justification = "center",
#           legend.box.just      = "center"
#         )
#     )
#   }
#   
#   # ── single-commodity path ──────────────────────────────────────────
#   cons <- Y_data %>%
#     filter(comm_code == target_comm_in) %>%
#     group_by(year, target_continent) %>%
#     summarize(value = sum(value, na.rm = TRUE), .groups = "drop")
#   if (nrow(cons) == 0) stop("No Y rows for that comm_code.")
#   
#   item_name <- Y_data %>%
#     filter(comm_code == target_comm_in) %>%
#     pull(item) %>% unique() %>% paste(collapse = "/")
#   
#   m <- meta[indicator == indicator_select]
#   if (nrow(m) == 0) {
#     m <- data.table(indicator    = indicator_select,
#                     scale_factor = 1,
#                     y_label      = indicator_select,
#                     short_label  = indicator_select)
#   }
#   
#   imp <- impact_data %>%
#     left_join(regions %>% select(iso3c, continent),
#               by = c("country_consumer" = "iso3c")) %>%
#     rename(target_continent = continent) %>%
#     filter(commodity == target_comm_in,
#            indicator == indicator_select) %>%
#     group_by(year, target_continent) %>%
#     summarize(value = sum(value, na.rm = TRUE) / m$scale_factor,
#               .groups = "drop")
#   
#   common <- intersect(cons$target_continent, imp$target_continent)
#   cons <- cons %>% filter(target_continent %in% common)
#   imp  <- imp  %>% filter(target_continent %in% common)
#   
#   cons_max <- max(cons$value, na.rm = TRUE)
#   imp_max  <- if (nrow(imp)) max(imp$value, na.rm = TRUE) else 1
#   k <- if (is.finite(imp_max) && imp_max > 0) cons_max / imp_max else 1
#   
#   flow_imp <- m$short_label
#   plot_df <- bind_rows(
#     cons %>% mutate(flow = "Consumption", y = value),
#     imp  %>% mutate(flow = flow_imp,      y = value * k)
#   ) %>%
#     mutate(flow = factor(flow, levels = c("Consumption", flow_imp)),
#            target_continent = factor(target_continent,
#                                      levels = names(continent_palette)))
#   
#   ggplot(plot_df, aes(year, y,
#                       colour   = target_continent,
#                       linetype = flow,
#                       group    = interaction(target_continent, flow))) +
#     geom_line(linewidth = 0.7) +
#     scale_x_continuous(breaks = scales::pretty_breaks()) +
#     scale_y_continuous(
#       name     = y_label,
#       sec.axis = sec_axis(~ . / k, name = m$y_label)
#     ) +
#     scale_linetype_manual(values = setNames(c("solid", "longdash"),
#                                             c("Consumption", flow_imp))) +
#     scale_colour_manual(values   = continent_palette,
#                         name     = "Continent",
#                         na.value = "grey60",
#                         drop     = FALSE) +
#     labs(title    = paste0(item_name, " · ", m$short_label),
#          x        = NULL,
#          colour   = "Continent",
#          linetype = "Flow") +
#     theme_minimal(base_size = 11) +
#     theme(panel.grid.minor = element_blank(),
#           legend.key.size  = unit(0.6, "cm"))
# }
# 
# 
# 
# plot_consumption_vs_impact_by_continent(
#   Y_data           = Y_per_year_continent,
#   impact_data      = dt_tradeFeed,
#   regions          = regions,
#   target_comm_in   = c("c146", "c147", "c149"),
#   indicator_select = "ibif_total"
# )
# 
# 
# 
# 
# 
# 
# 
# plot_decoupling_indexed <- function(
    #     Y_data, impact_data, regions,
#     target_comm_in, indicator_select,
#     meta = indicator_meta,
#     base_year = NULL) {
#   
#   cons <- Y_data %>%
#     filter(comm_code %in% target_comm_in) %>%
#     group_by(year, target_continent, comm_code, item) %>%
#     summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
#     mutate(flow = "Consumption")
#   
#   imp <- impact_data %>%
#     left_join(regions %>% select(iso3c, continent),
#               by = c("country_consumer" = "iso3c")) %>%
#     rename(target_continent = continent) %>%
#     filter(commodity %in% target_comm_in,
#            indicator == indicator_select) %>%
#     group_by(year, target_continent, commodity) %>%
#     summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
#     rename(comm_code = commodity) %>%
#     left_join(distinct(cons, comm_code, item), by = "comm_code") %>%
#     mutate(flow = meta[indicator == indicator_select, short_label])
#   
#   df <- bind_rows(cons, imp)
#   if (is.null(base_year)) base_year <- min(df$year)
#   
#   df <- df %>%
#     group_by(flow, target_continent, comm_code, item) %>%
#     arrange(year) %>%
#     mutate(
#       base_val = value[year == base_year][1],
#       index    = ifelse(is.finite(base_val) & base_val > 0,
#                         100 * value / base_val, NA_real_)
#     ) %>%
#     ungroup() %>%
#     filter(is.finite(index))
#   
#   # Wide form for ribbon between the two flows
#   df_w <- df %>%
#     select(year, target_continent, item, flow, index) %>%
#     tidyr::pivot_wider(names_from = flow, values_from = index) %>%
#     rename(cons_idx = Consumption,
#            imp_idx  = !!meta[indicator == indicator_select, short_label])
#   
#   ggplot() +
#     geom_hline(yintercept = 100, colour = "grey80", linewidth = 0.3) +
#     geom_ribbon(data = df_w,
#                 aes(x = year, ymin = pmin(cons_idx, imp_idx),
#                     ymax = pmax(cons_idx, imp_idx),
#                     fill = cons_idx > imp_idx),
#                 alpha = 0.15) +
#     geom_line(data = df,
#               aes(year, index, colour = flow, linetype = flow),
#               linewidth = 0.7) +
#     facet_grid(rows = vars(target_continent), cols = vars(item),
#                scales = "free_y") +
#     scale_colour_manual(values = setNames(c("#444444", "#D55E00"),
#                                           c("Consumption",
#                                             meta[indicator == indicator_select, short_label]))) +
#     scale_linetype_manual(values = setNames(c("solid", "longdash"),
#                                             c("Consumption",
#                                               meta[indicator == indicator_select, short_label]))) +
#     scale_fill_manual(values = c(`TRUE` = "#0072B2", `FALSE` = "#D55E00"),
#                       labels = c(`TRUE` = "Decoupling gap", `FALSE` = "Recoupling gap"),
#                       name   = NULL) +
#     labs(title = paste0("Decoupling: consumption vs ",
#                         meta[indicator == indicator_select, short_label]),
#          subtitle = paste0("Indexed to ", base_year, " = 100"),
#          x = NULL, y = paste0("Index (", base_year, " = 100)"),
#          colour = NULL, linetype = NULL) +
#     theme_minimal(base_size = 11) +
#     theme(panel.grid.minor = element_blank(),
#           legend.position  = "bottom")
# }
# 
# plot_decoupling_indexed(
#   Y_data           = Y_per_year_continent,
#   impact_data      = dt_tradeFeed,
#   regions          = regions,
#   target_comm_in   = c("c146", "c147"),
#   indicator_select = "ibif_total"
# 
