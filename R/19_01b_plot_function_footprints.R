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
svg_device <- if (requireNamespace("svglite", quietly = TRUE)) svglite::svglite else grDevices::svg
library(openxlsx)


items_full_bcp <- read_csv("inst/items_full_bcp.csv")
items_full_bcp <- as.data.table(items_full_bcp)
regions <- setDT(read_csv("inst/regions_full.csv"))[current==TRUE]
tcf <- readRDS("intermediate_data/tcf_table_final.rds")

# ---- MODEL VERSION -----------------------------------------------------------
#   "rescaled" (default) -> results in output/         (RED-rescaled run)
#   "bypass"             -> results in output/bypass/  (non-rescaled counterfactual)
# Keep in sync with 18b.
model_version <- Sys.getenv("FABIO_RUN_MODE", unset = "rescaled")
model_version <- if (tolower(trimws(model_version)) == "bypass") "bypass" else "rescaled"

# ---- CAPPING VARIANT (default = capped) --------------------------------------
# FABIO_VARIANT = "capped" (default) or "uncapped". Environmental footprint CSVs
# come from 18_01b and are variant-dependent (output_capped/). MATERIAL summaries
# (Y/Z/X_summary from 18_01a) are capping-invariant and always read from output/.
VARIANT   <- tolower(trimws(Sys.getenv("FABIO_VARIANT", unset = "capped")))
is_capped <- VARIANT == "capped"
vsuf      <- if (is_capped) "_capped" else ""
base_out  <- if (model_version == "bypass") "output/bypass" else "output"
ENV_DIR   <- paste0(base_out, vsuf)          # 18_01b footprint CSVs (capped-aware)
MAT_DIR   <- base_out                         # 18_01a material CSVs (baseline only)
PLOT_DIR  <- file.path(ENV_DIR, "plot")       # plots separated per variant
message(sprintf(">>> [19b environmental] model_version = '%s' | variant = '%s'  (env: %s | material: %s | plots: %s)",
                model_version, VARIANT, ENV_DIR, MAT_DIR, PLOT_DIR))

source("R/19_plot_definitions.R")

dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

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
                        dir = ENV_DIR) {
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
Y_summary <- fread(file.path(MAT_DIR, "Y_summary_c146_c147_c149.csv"))

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

## Producer-located end-use decomposition -- combined per-indicator files (18b) 
# Columns: country, continent, year, indicator, allocation,
#          end_group, enduse_impact, total_ag_impact, share
load_endUseOrigin <- function(dir = ENV_DIR, alloc = "value") {
  f <- list.files(dir, pattern = "^FABIO_endUseOrigin_.*\\.csv$", full.names = TRUE)
  f <- f[!file.info(f)$isdir]
  if (!is.null(alloc)) f <- f[grepl(paste0("_", alloc), f)]
  if (length(f) == 0) {
    warning("No FABIO_endUseOrigin_* files found in ", dir)
    return(data.table())
  }
  rbindlist(lapply(f, fread), use.names = TRUE, fill = TRUE)
}

dt_endUseOrigin <- load_endUseOrigin()

# Build combined LC Impact terrestrial indicator (climate + acidification) for
# endUseOrigin, exactly as for dt_feedstock/dt_tradeFeed. LCIM_EQ_terrestrial_land_use
# is left UNTOUCHED (kept as its own separate indicator). Unlike the feedstock
# table, endUseOrigin carries per-country totals + shares, so recompute those.
id_cols_enduse <- c("country", "continent", "year", "allocation", "end_group")

lcim_terr_enduse <- dt_endUseOrigin[
  indicator %in% c("LCIM_EQ_terrestrial_climate", "LCIM_EQ_terrestrial_acidification"),
  .(enduse_impact = sum(enduse_impact, na.rm = TRUE)),
  by = id_cols_enduse
][, indicator := "LCIM_EQ_terrestrial"]

# recompute the per-country-year total and share for the aggregated indicator
lcim_terr_enduse[, total_ag_impact := sum(enduse_impact), by = .(country, year)]
lcim_terr_enduse[, share := fifelse(total_ag_impact != 0,
                                    enduse_impact / total_ag_impact, NA_real_)]

# drop the two sub-indicators (and any stale combined rows), bind in the combined one
dt_endUseOrigin <- rbindlist(
  list(dt_endUseOrigin[!indicator %in% c("LCIM_EQ_terrestrial_climate",
                                         "LCIM_EQ_terrestrial_acidification",
                                         "LCIM_EQ_terrestrial")],
       lcim_terr_enduse),
  use.names = TRUE, fill = TRUE
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
# ─── Shared helpers: grid strip labelling ──────────────────────────────
# The *_grid wrappers below assemble one ggplot per (commodity x indicator)
# cell with patchwork. Instead of giving every cell its own title, they label
# the assembled grid the way 19_01a's `facet_grid(target_comm ~ target_continent)`
# does: COLUMN name on top of the first row, ROW name on the right of the last
# column. Each cell carries its own single-level facet, so the strips are real
# ggplot strips (same look as 19_01a) while every panel keeps its own y axis
# and secondary consumption axis -- which a single facet_grid could not do.

.resolve_indicator_labels <- function(indicators, indicator_labels = NULL,
                                      meta = indicator_meta) {
  m   <- as.data.table(meta)
  out <- vapply(indicators, function(ind) {
    v <- m[indicator == ind, short_label]
    if (length(v) && !is.na(v[1])) as.character(v[1]) else ind
  }, character(1))
  names(out) <- indicators
  if (!is.null(indicator_labels)) {
    hit <- intersect(names(indicator_labels), indicators)
    out[hit] <- as.character(indicator_labels[hit])
  }
  out
}

.resolve_commodity_labels <- function(commodities, commodity_labels = NULL,
                                      items = items_full_bcp) {
  it  <- as.data.table(items)
  out <- vapply(commodities, function(cc) {
    v <- it[comm_code == cc, item]
    if (length(v) && !is.na(v[1])) as.character(v[1]) else cc
  }, character(1))
  names(out) <- commodities
  if (!is.null(commodity_labels)) {
    hit <- intersect(names(commodity_labels), commodities)
    out[hit] <- as.character(commodity_labels[hit])
  }
  out
}

# Attach the constant facet key(s) to a panel's plotting data.
.attach_strip_keys <- function(agg, col_strip = NULL, row_strip = NULL) {
  if (!is.null(col_strip)) agg[, `.col_strip` := as.character(col_strip)]
  if (!is.null(row_strip)) agg[, `.row_strip` := as.character(row_strip)]
  agg
}

# Add the matching single-level facet to a finished panel.
.apply_strips <- function(p, col_strip = NULL, row_strip = NULL) {
  if (is.null(col_strip) && is.null(row_strip)) return(p)
  p <- p + if (!is.null(col_strip) && !is.null(row_strip)) {
    ggplot2::facet_grid(rows = ggplot2::vars(.row_strip),
                        cols = ggplot2::vars(.col_strip))
  } else if (!is.null(col_strip)) {
    ggplot2::facet_grid(cols = ggplot2::vars(.col_strip))
  } else {
    ggplot2::facet_grid(rows = ggplot2::vars(.row_strip))
  }
  p + ggplot2::theme(strip.text       = ggplot2::element_text(face = "bold"),
                     strip.background = ggplot2::element_blank())
}


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
                                     cons_lab        = "Consumption (M liters)",
                                     title           = NA,      # NA = auto ("IBIF by feedstock — Biodiesel"); NULL = none
                                     x_lab           = "Year",  # NULL = no x-axis title
                                     col_strip       = NULL,    # top strip text   (NULL = none)
                                     row_strip       = NULL) {  # right strip text (NULL = none)
  
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
  
  if (length(title) == 1L && is.na(title))
    title <- paste0(m$short_label, " by feedstock — ", commodity_name)
  
  agg <- .attach_strip_keys(agg, col_strip, row_strip)
  
  p <- ggplot(agg, aes(x = year, y = value, fill = feedstock_group)) +
    geom_bar(stat = "identity") +
    fill_scale +
    labs(
      x     = x_lab,
      y     = m$y_label,
      title = title
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
  
  .apply_strips(p, col_strip, row_strip)
}

# ─── Convenience wrapper for batch plotting ────────────────────────────

plot_commodity_feedstock_grid <- function(data, commodities, indicators,
                                          palette         = NULL,
                                          top_n_feedstock = 10,
                                          Y_summary       = NULL,
                                          strip_labels     = TRUE,  # FALSE = old per-panel titles
                                          indicator_labels = NULL,  # named override, e.g. c(ibif_total = "IBIF")
                                          commodity_labels = NULL,  # named override, e.g. c(c146 = "Bioethanol")
                                          compact_axes     = TRUE,  # x-axis title on the bottom row only
                                          ...) {
  
  ind_lab  <- .resolve_indicator_labels(indicators, indicator_labels)
  comm_lab <- .resolve_commodity_labels(commodities, commodity_labels)
  n_ind    <- length(indicators)
  n_comm   <- length(commodities)
  
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
  row_stacks <- lapply(seq_along(commodities), function(i) {
    
    comm <- commodities[i]
    
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
    
    plist <- lapply(seq_along(indicators), function(j) {
      ind <- indicators[j]
      # Column strip on the first row, row strip on the last column -- the
      # facet_grid(rows ~ cols) convention used by 19_01a.
      strip_args <- if (strip_labels) {
        list(title     = NULL,
             x_lab     = if (compact_axes && i < n_comm) NULL else "Year",
             col_strip = if (i == 1L)     unname(ind_lab[ind])   else NULL,
             row_strip = if (j == n_ind)  unname(comm_lab[comm]) else NULL)
      } else list()
      
      do.call(plot_commodity_feedstock,
              c(list(data, comm, ind,
                     palette         = palette,
                     top_n_feedstock = top_n_feedstock,
                     level_order     = level_order,
                     Y_summary       = Y_summary),
                strip_args, list(...)))
    })
    
    # Replace NULL panels (commodity x indicator with no data) with spacers.
    plist <- lapply(plist, function(p) if (is.null(p)) patchwork::plot_spacer() else p)
    
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
                           bar_color  = "#4575B4",
                           title      = NA,      # NA = auto; NULL = none
                           x_lab      = "Year",
                           col_strip  = NULL,
                           row_strip  = NULL) {
  
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
  
  if (length(title) == 1L && is.na(title))
    title <- paste0(m$short_label, " — ", commodity_name)
  
  agg <- .attach_strip_keys(agg, col_strip, row_strip)
  
  p <- ggplot(agg, aes(x = year, y = value)) +
    geom_bar(stat = "identity", fill = bar_color) +
    scale_y_continuous(labels = scales::label_number()) +
    labs(
      x     = x_lab,
      y     = m$y_label,
      title = title
    ) +
    theme_minimal()
  
  .apply_strips(p, col_strip, row_strip)
}

# ─── Grid wrapper ──────────────────────────────────────────────────────
# Rows = commodities, columns = indicators. Commodities with no data for
# any indicator in `year_range` are dropped (no empty row).
plot_commodity_grid <- function(data, commodities, indicators,
                                year_range = c(2012, 2022),
                                bar_color  = "#4575B4",
                                strip_labels     = TRUE,
                                indicator_labels = NULL,
                                commodity_labels = NULL,
                                compact_axes     = TRUE,
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
  
  ind_lab  <- .resolve_indicator_labels(indicators, indicator_labels)
  comm_lab <- .resolve_commodity_labels(commodities, commodity_labels)
  n_ind    <- length(indicators)
  n_comm   <- length(commodities)
  
  # Per-cell data availability: a row strip must land on a panel that exists,
  # so it goes on the RIGHTMOST cell of the row that actually has data.
  cell_ok <- outer(commodities, indicators, Vectorize(function(comm, ind) {
    nrow(dt[commodity == comm & indicator == ind & year %between% year_range]) > 0
  }))
  
  row_stacks <- lapply(seq_along(commodities), function(i) {
    
    comm      <- commodities[i]
    row_strip_col <- if (any(cell_ok[i, ])) max(which(cell_ok[i, ])) else NA_integer_
    
    plist <- lapply(seq_along(indicators), function(j) {
      ind <- indicators[j]
      strip_args <- if (strip_labels) {
        list(title     = NULL,
             x_lab     = if (compact_axes && i < n_comm) NULL else "Year",
             col_strip = if (i == 1L) unname(ind_lab[ind]) else NULL,
             row_strip = if (!is.na(row_strip_col) && j == row_strip_col)
               unname(comm_lab[comm]) else NULL)
      } else list()
      
      do.call(plot_commodity,
              c(list(data, comm, ind,
                     year_range = year_range,
                     bar_color  = bar_color),
                strip_args, list(...)))
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
                                   commodities  = NULL, # NULL = all commodities in `dt_feedstock`; else subset + sum
                                   title        = "Biofuel consumer region",
                                   legend_title = NULL, # NULL = per-indicator default from cfg_all
                                   file_tag     = NULL, # extra token in the filename (e.g. "BP") to avoid collisions
                                   save_dir  = PLOT_DIR,
                                   save      = TRUE,
                                   breaks    = NULL,  # override auto breaks
                                   limits    = NULL) {
  
  # --- Optional commodity subset (summed over, e.g. the whole bp_set) -------
  if (!is.null(commodities)) {
    dt_feedstock <- as.data.table(dt_feedstock)[commodity %in% commodities]
    if (nrow(dt_feedstock) == 0L)
      stop("No rows left after filtering to commodities: ",
           paste(commodities, collapse = ", "))
  }
  
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
  
  # single-year windows -> "2012" rather than "2012-2012"
  .period_label <- function(y) {
    if (min(y) == max(y)) as.character(min(y)) else paste0(min(y), "-", max(y))
  }
  label_base <- .period_label(years_base)
  label_curr <- .period_label(years_curr)
  
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
         title = title,
         fill  = if (is.null(legend_title)) cfg$legend_title else legend_title) +
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
    fname <- paste0(label_base, "_vs_", label_curr, "_", cfg$file_suffix,
                    if (is.null(file_tag)) "" else paste0("_", file_tag), ".svg")
    ggsave(filename = file.path(save_dir, fname),
           plot = p, width = 10, height = 6, dpi = 300)
  }
  
  attr(p, "cont_order") <- cont_order   # read back with attr(p, "cont_order")
  p
}


######################################################################################################################
############################## PRODUCER-LOCATED IMPACT BY END-PRODUCT (symmetric Ly footprint) #######################
######################################################################################################################
# Grouped-bar layout: two adjacent year bars per country (left = first year,
# right = last), biofuels vivid at the BOTTOM, other segments dulled. Segments are
# END-PRODUCT groups from fp_enduse_origin - each producing country's territorial
# agricultural impact split by the end-product whose global final demand drives it.
# Bars are shown as RELATIVE shares: every bar fills 0-100% of that country-year's
# OWN total, so composition is comparable across countries regardless of size. The
# exact % is printed for the biofuels segment ONLY. `top_n` countries are selected
# by `rank_by`: "biofuel_total" (absolute biofuel-driven impact) or "biofuel_share"
# (biofuel as a share of the country's total). The top `n_groups` NON-biofuel
# end-groups are shown; the rest fold into "Other". Reads FABIO_endUseOrigin_*.

plot_enduse_origin <- function(data,
                               indicator_select,
                               years       = c(2012, 2022),
                               ref_year    = years[1],
                               top_n       = 10,
                               rank_by     = c("biofuel_total", "biofuel_share"),  # top_n by absolute vs share
                               n_groups    = 5,                # largest NON-biofuel end-groups; rest -> "Other"
                               meta        = indicator_meta,
                               items       = items_full_bcp,
                               bf_codes    = c("c146", "c147", "c149"),
                               bf_fill     = "#B2182B",
                               other_fill  = "grey75",
                               group_colors = c(   # fixed base colour per commodity group
                                 "Cereals"                                 = "#E6A817",
                                 "Vegetables, fruit, nuts, pulses, spices" = "#4DAF4A",
                                 "Vegetable oils"                          = "#A2A62E",
                                 "Sugar, sweeteners"                       = "#984EA3",
                                 "Fibre crops"                             = "#17A2A2",
                                 "Milk"                                    = "#377EB8",
                                 "Meat"                                    = "#8C564B",
                                 "Live animals"                            = "#E377C2"),
                               dull_amt    = 0.6,
                               bar_width   = 0.36,
                               label_share = TRUE,
                               legend_labels = c(   # compact legend-only relabels (data untouched)
                                 "Vegetables, fruit, nuts, pulses, spices" = "Produce"),
                               save        = FALSE,
                               save_dir    = PLOT_DIR,
                               save_name   = NULL,   # NULL -> auto filename
                               width       = 12,
                               height      = 3,
                               dpi         = 300) {
  
  rank_by <- match.arg(rank_by)
  dt <- as.data.table(data)
  if (nrow(dt) == 0) {
    warning("Empty endUseOrigin table - did 18b write FABIO_endUseOrigin_* files?")
    return(invisible(NULL))
  }
  need <- c("end_group", "enduse_impact", "total_ag_impact")
  miss <- setdiff(need, names(dt))
  if (length(miss))
    stop("endUseOrigin table missing column(s): ", paste(miss, collapse = ", "),
         " - re-run 18b fp_enduse_origin.")
  items <- as.data.table(items)
  
  m <- meta[indicator == indicator_select]
  if (nrow(m) == 0)
    m <- data.table(indicator = indicator_select, scale_factor = 1,
                    y_label = indicator_select, short_label = indicator_select)
  sf <- m$scale_factor[1]; ttl <- m$short_label[1]   # sf unused for shares; kept for parity
  
  yrs <- sort(unique(years))
  d <- dt[indicator == indicator_select & year %in% yrs]
  if (nrow(d) == 0) {
    warning("No data for ", indicator_select, " in years ", paste(yrs, collapse = "/"))
    return(invisible(NULL))
  }
  
  # biofuel end-group(s) = comm_group(s) of the biofuel commodities
  bf_group <- unique(items[comm_code %in% bf_codes, comm_group])
  bf_group <- bf_group[!is.na(bf_group)]
  if (!length(bf_group)) warning("No biofuel comm_group found via bf_codes.")
  
  # country set: top-n in ref_year, ranked by rank_by
  #   "biofuel_total" -> absolute biofuel-driven impact
  #   "biofuel_share" -> biofuel as a share of the country's territorial total
  ref_bf  <- d[year == ref_year & end_group %in% bf_group,
               .(bf_val = sum(enduse_impact, na.rm = TRUE)), by = country]
  ref_tot <- unique(d[year == ref_year, .(country, total_ag_impact)])
  ref <- merge(ref_tot, ref_bf, by = "country", all.x = TRUE)
  ref[is.na(bf_val), bf_val := 0]
  ref[, bf_share := fifelse(total_ag_impact != 0, bf_val / total_ag_impact, NA_real_)]
  rank_col <- if (rank_by == "biofuel_share") "bf_share" else "bf_val"
  setorderv(ref, rank_col, order = -1L)
  keep <- ref[get(rank_col) > 0, country][seq_len(min(top_n, .N))]
  keep <- keep[!is.na(keep)]
  if (!length(keep)) {
    warning("No positive biofuel ", rank_by, " for ", indicator_select, " in ", ref_year)
    return(invisible(NULL))
  }
  d <- d[country %in% keep]
  
  # largest NON-biofuel end-groups globally; biofuels kept separate, rest -> Other
  glob <- d[!end_group %in% bf_group,
            .(v = sum(enduse_impact, na.rm = TRUE)), by = end_group][order(-v)]
  keep_groups <- head(glob$end_group, n_groups)                 # largest first
  d[, seg := fifelse(end_group %in% bf_group, "Biofuels",
                     fifelse(end_group %in% keep_groups, end_group, "Other"))]
  
  # segments as SHARES of each country-year's own total (bars fill 0-100%)
  seg <- d[, .(value = sum(enduse_impact, na.rm = TRUE)),
           by = .(country, year, segment = seg)]
  seg[, tot := sum(value), by = .(country, year)]
  seg[, sh  := fifelse(tot != 0, value / tot, 0)]
  
  # biofuel share per country-year (the ONLY % printed); ensure all bars present
  cty <- merge(CJ(country = keep, year = yrs, unique = TRUE),
               seg[segment == "Biofuels", .(country, year, bf_share = sh)],
               by = c("country", "year"), all.x = TRUE)
  cty[is.na(bf_share), bf_share := 0]
  
  # ordering: Biofuels at BOTTOM, then largest end-groups, "Other" on top
  country_order <- ref[country %in% keep][order(-get(rank_col)), country]
  ci <- setNames(seq_along(country_order), country_order)
  seg_order <- c("Biofuels", keep_groups, "Other")
  seg[, segment := factor(segment, levels = seg_order)]
  
  dull <- function(cols, amt = dull_amt, toward = "grey72") {
    mm <- grDevices::col2rgb(toward) / 255
    vapply(cols, function(cc) {
      v <- grDevices::col2rgb(cc) / 255
      grDevices::rgb(t(v * (1 - amt) + mm * amt))
    }, character(1), USE.NAMES = FALSE)
  }
  # Fixed colour per commodity group (consistent across plots/indicators), dulled
  # to keep the vivid Biofuels segment dominant. Any group absent from
  # `group_colors` falls back to a generated colour.
  base_cols <- unname(group_colors[keep_groups])
  na_i <- which(is.na(base_cols))
  if (length(na_i)) {
    fb <- suppressWarnings(RColorBrewer::brewer.pal(max(3, length(na_i)), "Set3"))[seq_along(na_i)]
    base_cols[na_i] <- fb
  }
  pal <- c(Biofuels = bf_fill,
           setNames(dull(base_cols), keep_groups),
           Other = dull(other_fill))
  
  step <- bar_width + 0.04
  offs <- (seq_along(yrs) - (length(yrs) + 1) / 2) * step
  names(offs) <- as.character(yrs)
  seg[, xpos := ci[country] + offs[as.character(year)]]
  cty[, xpos := ci[country] + offs[as.character(year)]]
  
  p <- ggplot() +
    geom_col(data = seg, aes(x = xpos, y = sh, fill = segment),
             width = bar_width, position = position_stack(reverse = TRUE)) +
    geom_text(data = cty, aes(x = xpos, y = 0, label = substr(year, 3, 4)),
              vjust = 1.6, size = 3.9, colour = "grey35") +
    scale_fill_manual(values = pal, breaks = rev(seg_order),
                      labels = function(x) { r <- unname(legend_labels[x]); ifelse(is.na(r), x, r) },
                      name = "Commodity type") +
    scale_x_continuous(breaks = ci, labels = names(ci), expand = expansion(add = 0.55)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       breaks = seq(0, 1, 0.25),
                       expand = expansion(mult = c(0.03, 0.06))) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = "Share of agriculture-driven impact") +
    theme_minimal(base_size = 13) +       
    theme(panel.grid.major.x = element_blank(),
          axis.text.x        = element_text(face = "bold"),
          legend.position    = "right")
  
  if (label_share) {
    p <- p + geom_text(data = cty[bf_share > 0],
                       aes(x = xpos, y = bf_share,
                           label = scales::percent(bf_share, accuracy = 0.1)),
                       vjust = -0.4, size = 3.8, fontface = "bold", colour = "grey10")
  }
  
  if (save) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
    fn <- save_name
    if (is.null(fn))
      fn <- paste0("enduse_origin_", indicator_select, "_", rank_by, "_",
                   min(yrs), "_", max(yrs), ".svg")
    ggsave(file.path(save_dir, fn), plot = p, width = width, height = height, dpi = dpi)
    message("Saved ", file.path(save_dir, fn))
  }
  
  p
}

## Printing global shares 
compute_global_biofuel_share <- function(data, indicator_select, year_select) {
  dt <- as.data.table(data)[indicator == indicator_select & year == year_select]
  
  global_biofuel <- dt[end_group == "Biofuels", sum(enduse_impact, na.rm = TRUE)]
  
  global_total <- unique(dt[, .(country, total_ag_impact)])[
    , sum(total_ag_impact, na.rm = TRUE)]
  
  data.table(indicator = indicator_select, year = year_select,
             global_biofuel_impact = global_biofuel,
             global_total_impact   = global_total,
             global_share = global_biofuel / global_total)
}



