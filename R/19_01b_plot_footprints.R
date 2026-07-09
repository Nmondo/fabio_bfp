# 19b - Plot ENVIRONMENTAL-FOOTPRINT results  (reads VALUE-allocated stressor files)
# ------------------------------------------------------------------------------
# Consumption-based impact plots, driven by stressors (ibif*, LCIM_*, land_harv):
#   - country impact balance (self / import / export)         plot_balance
#   - impact by commodity x feedstock over time               plot_commodity_feedstock(_grid)
#   - impact by commodity x indicator over time               plot_commodity(_grid)
#   - inter-regional impact heatmaps                          plot_continent_heatmap
#   - focal-source share plots (IDN palm / BRA cane / USA maize)
#
# Impact tables read here are the VALUE-allocated stressor CSVs written by
# 18b_extension_footprints.R, selected by the "_value_" tag in their filename.
# Several plots ALSO overlay biofuel consumption (Y_summary) as reference — that
# is the "material flows as additional information" case — so Y_summary is loaded
# here too (it is Y-based, allocation-invariant).
#
# Companion: 19a_plot_material.R (reads the "_mass_" material files).
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
# Keep in sync with 18b.
model_version <- Sys.getenv("FABIO_RUN_MODE", unset = "rescaled")
model_version <- if (tolower(trimws(model_version)) == "bypass") "bypass" else "rescaled"
IN_DIR <- if (model_version == "bypass") "output/bypass" else "output"
message(sprintf(">>> [19b environmental] model_version = '%s'  (reading footprints from: %s)",
                model_version, IN_DIR))

source("R/19_plot_definitions.R")

dir.create(file.path("output", "plot"), recursive = TRUE, showWarnings = FALSE)

## Clean TCF (kept for parity with the shared setup; not used by env plots) ----
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
# `must_match` is an optional vector of extra regexes that ALL must match.

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
############################## READ RESULTS DATA (VALUE) ############################################################
######################################################################################################################

## Consumption reference (Y-based; overlaid on several impact plots) ----------
Y_summary <- fread(file.path(IN_DIR, "Y_summary_c146_c147_c149.csv"))

## Stressor footprints -- VALUE-allocated files from 18b ----------------------
# The "_value_" tag now separates these cleanly from the material "_mass_" run.
files_tradeFeed_BF <- fabio_files("FABIO_tradeFeed", "BF", alloc = "value")
files_tradeFeed_BP <- fabio_files("FABIO_tradeFeed", "BP", alloc = "value")
files_feedstock_BF <- fabio_files("FABIO_feedstock", "BF", alloc = "value")

dt_tradeFeed    <- rbindlist(lapply(files_tradeFeed_BF, fread))
dt_tradeFeed_BP <- rbindlist(lapply(files_tradeFeed_BP, fread))
dt_feedstock    <- rbindlist(lapply(files_feedstock_BF, fread))

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
############################## FUNCTIONS TO PLOT RESULTS #############################################################
######################################################################################################################

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
############################## INTER-REGIONAL IMPACT FLOWS ##########################################################
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