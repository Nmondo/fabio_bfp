# 19.02 - Plot SDA results

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


items_full_bcp <- read_csv("inst/items_full_bcp.csv")
items_full_bcp <- as.data.table(items_full_bcp)
regions <- setDT(read_csv("inst/regions.csv"))[current==TRUE]

# ---- MODEL VERSION -----------------------------------------------------------
# Read the SDA CSVs produced by 18_02 for the chosen model version:
#   "rescaled" (default) -> results in output/         (the RED-rescaled run)
#   "bypass"             -> results in output/bypass/  (non-rescaled counterfactual)
# Rescaled is the default, so behaviour is unchanged. Keep in sync with 18_02.
model_version <- Sys.getenv("FABIO_RUN_MODE", unset = "rescaled")
model_version <- if (tolower(trimws(model_version)) == "bypass") "bypass" else "rescaled"
IN_DIR <- if (model_version == "bypass") "output/bypass" else "output"
message(sprintf(">>> [19_02] model_version = '%s'  (reading SDA from: %s)",
                model_version, IN_DIR))


######################################################################################################################
############################## Setting colors #######################
######################################################################################################################

fuel_colors <- c(
  "Biogasoline"       = "#D4A017",
  "Biodiesel"        = "#5B2C6F",
  "Renewable diesel" = "#9B59B6"
)


######################################################################################################################
############################## Setting indicator-specific labels #######################
######################################################################################################################

indicator_meta <- data.table(
  indicator    = c("land_harv", "ibif_total",
                   "LCIM_EQ_freshwater", "LCIM_EQ_marine", "LCIM_EQ_terrestrial"),
  scale_factor = c(1e3, 1, 1, 1, 1),
  y_label      = c("Land use (1000 ha)",
                   "IBIF (MSA·km²·yr)",
                   "Freshwater ecotoxicity (PDF·yr)",
                   "Marine ecotoxicity (PDF·yr)",
                   "Terrestrial ecotoxicity (PDF·yr)"),
  short_label  = c("Land use", "IBIF",
                   "Freshwater ecotoxicity",
                   "Marine ecotoxicity",
                   "Terrestrial ecotoxicity")
)




######################################################################################################################
############################## LOADING SDA RESULTS #######################
######################################################################################################################

SDA_chain_ibif <- fread(file.path(IN_DIR, "FABIO_SDA_chained_2012_2022_ibif_total_value_BF.csv"))
SDA_chain_ibif_drivers <- fread(file.path(IN_DIR, "FABIO_SDA_chained_2012_2022_ibif_total_value_BF_drivers.csv"))
# SDA_chain_lcim <- fread(file.path(IN_DIR, "FABIO_SDA_chained_2012_2022_LCIM_EQ_terrestrial_value_BF.csv"))
# SDA_chain_lcim_drivers <- fread(file.path(IN_DIR, "FABIO_SDA_chained_2012_2022_LCIM_EQ_terrestrial_value_BF_drivers.csv"))
# SDA_smooth <- fread(file.path(IN_DIR, "FABIO_SDA_smoothed_2012-2014_vs_2020-2022_land_harv_value_BF.csv"))
# SDA_smooth_drivers <- fread(file.path(IN_DIR, "FABIO_SDA_smoothed_2012-2014_vs_2020-2022_land_harv_value_BF_drivers.csv"))


# View(SDA_chain_ibif_drivers[effect == "feedstock_mix"])
# View(SDA_chain_ibif_drivers[effect == "composition"])


######################################################################################################################
############################## PLOTS / PLOT FUNCTIONS #######################
######################################################################################################################

# ######################################################################################################################
# #1. Plotting drivers by continent
# ######################################################################################################################
# 
# # --- Aggregate by continent ------------------------------------------------
# plot_dt <- SDA_smooth[
#   effect %in% c("intensity", "feedstock_mix", "sourcing",
#                 "scale", "composition", "origin")
# ][regions[, .(iso3c, continent)],
#   on = c(country_consumer = "iso3c"),
#   nomatch = NULL
# ][, .(value = sum(value, na.rm = TRUE)),
#   by = .(continent, effect)]
# 
# # Order continents by total Δ footprint, descending
# continent_order <- plot_dt[, .(total = sum(value, na.rm = TRUE)),
#                            by = continent][order(-total), continent]
# 
# plot_dt[, continent := factor(continent, levels = continent_order)]
# plot_dt[, effect    := factor(effect,
#                               levels = c("intensity", "feedstock_mix", "sourcing",
#                                          "scale", "composition", "origin"))]
# 
# # --- Plot ------------------------------------------------------------------
# plot_sda_continent <- ggplot(plot_dt, aes(x = continent, y = value, fill = effect)) +
#   geom_col(position = position_dodge(width = 0.8), width = 0.75) +
#   geom_hline(yintercept = 0, linewidth = 0.3) +
#   scale_fill_brewer(palette = "Set2") +
#   labs(
#     title    = "SDA effects, 2012-2014 average versus 2020–2022 average — by continent",
#     subtitle = "Continents ordered left-to-right by total change",
#     x        = NULL,
#     y        = "Contribution to Δ footprint",
#     fill     = "Effect"
#   ) +
#   theme_minimal(base_size = 12) +
#   theme(
#     axis.text.x        = element_text(angle = 30, hjust = 1),
#     panel.grid.major.x = element_blank(),
#     legend.position    = "bottom"
#   )
# 
# ggsave(filename = file.path("output", "plot", "2012_2022_SDA_continent.pdf"),
#        device = cairo_pdf,
#        plot   = plot_sda_continent,
#        width  = 10, height = 6, dpi = 300)
# 
# 


######################################################################################################################
#2. Plotting SDA time series for top countries
######################################################################################################################

plot_SDA_chain <- function(first_year,
                           last_year,
                           extension,
                           top_n      = 9,
                           input_dir  = IN_DIR,
                           output_dir = file.path("output", "plot"),
                           width      = 12,
                           height     = 12) {
  
  # --- Build filename and read ----------------------------------------------
  in_file  <- file.path(
    input_dir,
    sprintf("FABIO_SDA_chained_%d_%d_%s_value_BF.csv",
            first_year, last_year, extension)
  )
  SDA_chain <- fread(in_file)
  
  # --- Cumulative totals per country ----------------------------------------
  SDA_cumul <- SDA_chain[
    effect %in% c("intensity", "feedstock_mix", "sourcing",
                  "scale", "composition", "origin", "delta"),
    .(value = sum(value)),
    by = .(country_consumer, effect)
  ]
  
  # --- Top N countries by signed cumulative delta ---------------------------
  topN          <- SDA_cumul[effect == "delta"][order(-value)][seq_len(top_n)]
  country_order <- topN$country_consumer
  
  traj <- SDA_chain[
    country_consumer %in% country_order &
      effect %in% c("intensity", "feedstock_mix", "sourcing",
                    "scale", "composition", "origin")
  ]
  
  year_levels <- sort(unique(traj$year_current))
  traj[, country_consumer := factor(country_consumer, levels = country_order)]
  traj[, effect           := factor(effect, levels = c("intensity", "feedstock_mix", "sourcing",
                                                       "scale", "composition", "origin"))]
  traj[, year_current     := factor(year_current, levels = year_levels)]
  
  # --- Running cumulative Δ per country -------------------------------------
  cum_delta <- SDA_chain[
    country_consumer %in% country_order & effect == "delta"
  ][order(country_consumer, year_current),
    .(year_current, value, cum_value = cumsum(value)),
    by = country_consumer]
  cum_delta[, country_consumer := factor(country_consumer, levels = country_order)]
  cum_delta[, year_current     := factor(year_current, levels = year_levels)]
  
  # --- Plot -----------------------------------------------------------------
  p <- ggplot(traj, aes(x = year_current, y = value, fill = effect)) +
    geom_col(position = position_stack(), width = 0.7) +
    geom_line(data = cum_delta,
              aes(x = year_current, y = cum_value, group = 1),
              inherit.aes = FALSE, linewidth = 0.6, colour = "black") +
    geom_point(data = cum_delta,
               aes(x = year_current, y = cum_value),
               inherit.aes = FALSE, size = 1.2, colour = "black") +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    facet_wrap(~ country_consumer, scales = "free_y") +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title    = sprintf("SDA effects, annual chain %d-%d — top %d countries (%s)",
                         first_year, last_year, top_n, extension),
      subtitle = "Stacked annual contributions per year-pair; black line = running cumulative Δ",
      x        = "Year",
      y        = "Annual contribution to Δ footprint",
      fill     = "Effect"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          axis.text.x     = element_text(angle = 45, hjust = 1))
  
  # --- Save -----------------------------------------------------------------
  out_file <- file.path(
    output_dir,
    sprintf("%d_%d_SDA_chain_ts_%s_BF.pdf",
            first_year, last_year, extension)
  )
  ggsave(filename = out_file,
         device   = cairo_pdf,
         plot     = p,
         width    = width, height = height, dpi = 300)
  
  invisible(p)
}


######################################################################################################################
#2b. Plotting SDA time series for top continents
######################################################################################################################

plot_SDA_chain_continent <- function(first_year,
                                     last_year,
                                     extension,
                                     top_n          = 4,
                                     continents     = NULL,   # explicit continent selection; overrides top_n if set
                                     include_global = FALSE,  # add a "World" panel = sum over ALL continents
                                     input_dir      = IN_DIR,
                                     output_dir     = file.path("output", "plot"),
                                     width          = 12,
                                     height         = 12) {
  
  # --- Build filename and read ----------------------------------------------
  in_file  <- file.path(
    input_dir,
    sprintf("FABIO_SDA_chained_%d_%d_%s_value_BF.csv",
            first_year, last_year, extension)
  )
  SDA_chain_country <- fread(in_file)
  
  # --- Aggregate to continent level, preserving the year/effect structure ---
  SDA_chain <- SDA_chain_country[
    regions[, .(iso3c, continent)],
    on = c(country_consumer = "iso3c"),
    nomatch = NULL
  ][, .(value = sum(value, na.rm = TRUE)),
    by = .(continent, effect, year_current)]
  
  # --- Select continents: explicit choice, or top-N by cumulative delta -----
  if (!is.null(continents)) {
    
    available <- unique(SDA_chain$continent)
    missing   <- setdiff(continents, available)
    if (length(missing) > 0) {
      warning(sprintf("Continent(s) not found in data and will be dropped: %s",
                      paste(missing, collapse = ", ")))
    }
    continent_order <- continents[continents %in% available]  # keeps user-specified order
    if (length(continent_order) == 0 && !include_global) {
      stop("None of the requested continents were found in the data.")
    }
    
  } else {
    
    SDA_cumul <- SDA_chain[
      effect %in% c("intensity", "feedstock_mix", "sourcing",
                    "scale", "composition", "origin", "delta"),
      .(value = sum(value)),
      by = .(continent, effect)
    ]
    topN            <- SDA_cumul[effect == "delta"][order(-value)][seq_len(top_n)]
    continent_order <- topN$continent
    
  }
  
  # --- Optionally append a "World" panel = sum across ALL continents --------
  if (include_global) {
    global_chain <- SDA_chain[, .(value = sum(value, na.rm = TRUE)),
                              by = .(effect, year_current)][, continent := "World"]
    SDA_chain       <- rbindlist(list(SDA_chain, global_chain), use.names = TRUE, fill = TRUE)
    continent_order <- c(continent_order, "World")
  }
  
  traj <- SDA_chain[
    continent %in% continent_order &
      effect %in% c("intensity", "feedstock_mix", "sourcing",
                    "scale", "composition", "origin")
  ]
  
  year_levels <- sort(unique(traj$year_current))
  traj[, continent    := factor(continent, levels = continent_order)]
  traj[, effect       := factor(effect, levels = c("intensity", "feedstock_mix", "sourcing",
                                                   "scale", "composition", "origin"))]
  traj[, year_current := factor(year_current, levels = year_levels)]
  
  # --- Running cumulative Δ per continent -------------------------------------
  cum_delta <- SDA_chain[
    continent %in% continent_order & effect == "delta"
  ][order(continent, year_current),
    .(year_current, value, cum_value = cumsum(value)),
    by = continent]
  cum_delta[, continent    := factor(continent, levels = continent_order)]
  cum_delta[, year_current := factor(year_current, levels = year_levels)]
  
  # --- Title reflects the selection mode -------------------------------------
  selection_label <- if (!is.null(continents)) {
    paste(setdiff(continent_order, "World"), collapse = ", ")
  } else {
    sprintf("top %d", top_n)
  }
  if (include_global) selection_label <- paste0(selection_label, " + World")
  
  # --- Plot -----------------------------------------------------------------
  p <- ggplot(traj, aes(x = year_current, y = value, fill = effect)) +
    geom_col(position = position_stack(), width = 0.7) +
    geom_line(data = cum_delta,
              aes(x = year_current, y = cum_value, group = 1),
              inherit.aes = FALSE, linewidth = 0.6, colour = "black") +
    geom_point(data = cum_delta,
               aes(x = year_current, y = cum_value),
               inherit.aes = FALSE, size = 1.2, colour = "black") +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    facet_wrap(~ continent, scales = "free_y") +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title    = sprintf("SDA effects, annual chain %d-%d — %s (%s)",
                         first_year, last_year, selection_label, extension),
      subtitle = "Stacked annual contributions per year-pair; black line = running cumulative Δ",
      x        = "Year",
      y        = "Annual contribution to Δ footprint",
      fill     = "Effect"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          axis.text.x     = element_text(angle = 45, hjust = 1))
  
  # --- Save -----------------------------------------------------------------
  out_file <- file.path(
    output_dir,
    sprintf("%d_%d_SDA_chain_ts_%s_continent_BF.pdf",
            first_year, last_year, extension)
  )
  ggsave(filename = out_file,
         device   = cairo_pdf,
         plot     = p,
         width    = width, height = height, dpi = 300)
  
  invisible(p)
}

########### Plotting for various extensions

plot_SDA_chain(2012, 2022, "ibif_total")
# plot_SDA_chain(2012, 2022, "LCIM_EQ_terrestrial")

plot_SDA_chain_continent(2012, 2022, "ibif_total",
                         continents = c("EU", "ASI", "LAM", "NAM"))

# plot_SDA_chain_continent(2012, 2022, "ibif_total",
#                          continents     = character(0),
#                          include_global = TRUE)


######################################################################################################################
#3. Plot SDA chained driver decomposition by effect by consumer continent
######################################################################################################################

plot_SDA_chained_drivers <- function(indicator,
                                     output_dir = file.path("output", "plot"),
                                     width = 11, height = 6.5, dpi = 300) {
  
  # --- Indicator metadata ----------------------------------------------------
  ind  <- indicator
  meta <- indicator_meta[indicator == ind]
  if (nrow(meta) == 0L) stop("Unknown indicator: ", ind,
                             ". Add it to indicator_meta.")
  scale_factor <- meta$scale_factor
  y_axis_label <- paste0("Contribution to Δ ", meta$y_label)
  short_lbl    <- meta$short_label
  
  # --- 0. Read, rescale, and map to continent BEFORE aggregating -------------
  path <- sprintf(file.path(IN_DIR, "FABIO_SDA_chained_2012_2022_%s_value_BF_drivers.csv"),
                  indicator)
  raw <- fread(path)
  raw[, value := value / scale_factor]
  
  # Map country_consumer / country_origin to continent up front, so every
  # downstream sum groups directly by continent — no country-level
  # intermediate, no late-stage re-collapse inside make_sda_driver_plot.
  raw[regions, on = c(country_consumer = "iso3c"), continent_consumer := i.continent]
  raw[regions, on = c(country_origin   = "iso3c"), continent_origin   := i.continent]
  
  effect_groups <- list(
    intensity     = c("continent_consumer", "continent_origin", "item_origin"),
    feedstock_mix = c("continent_consumer", "item_origin"),
    sourcing      = c("continent_consumer", "continent_origin", "item_origin"),
    scale         = c("continent_consumer", "commodity"),
    origin        = c("continent_consumer", "continent_origin", "item_origin", "commodity"),
    composition   = c("continent_consumer", "commodity")
  )
  
  plot_base <- rbindlist(
    lapply(names(effect_groups), function(eff) {
      gv <- effect_groups[[eff]]
      raw[effect == eff,
          .(value = sum(value, na.rm = TRUE)),
          by = gv][, effect := eff][]
    }),
    use.names = TRUE, fill = TRUE
  )
  
  # --- 1. Helper: aggregate + plot one effect --------------------------------
  # (unchanged — agg's group-by continent_consumer is now a no-op re-sum
  #  since plot_base is already at continent granularity, but kept for safety)
  make_sda_driver_plot <- function(dt, eff, group_var,
                                   palette    = NULL,
                                   cont_order = NULL,
                                   fill_order = NULL,
                                   top_n      = NULL) {
    sub <- dt[effect == eff]
    
    if (!is.null(top_n)) {
      ranked <- sub[, .(tot = sum(abs(value), na.rm = TRUE)),
                    by = c(group_var)][order(-tot)]
      keep   <- ranked[seq_len(min(top_n, nrow(ranked))), get(group_var)]
      sub[, (group_var) := fifelse(get(group_var) %in% keep,
                                   get(group_var), "Other")]
    }
    
    # NEW: anything without a defined level -> "Other", so factor() never NA-coerces
    if (!is.null(fill_order)) {
      sub[!(get(group_var) %in% fill_order), (group_var) := "Other"]
    }
    
    agg <- sub[, .(value = sum(value, na.rm = TRUE)),
               by = c("continent_consumer", group_var)]
    
    if (is.null(cont_order)) {
      cont_order <- agg[, .(total = sum(pmax(value, 0), na.rm = TRUE)),
                        by = continent_consumer][order(-total), continent_consumer]
    }
    agg[, continent_consumer := factor(continent_consumer, levels = cont_order)]
    
    if (is.null(fill_order)) {
      fill_order <- agg[, .(tot = sum(abs(value))), by = c(group_var)][
        order(-tot), get(group_var)]
    }
    if ("Other" %in% fill_order) {
      fill_order <- c("Other", setdiff(fill_order, "Other"))
    }
    agg[, (group_var) := factor(get(group_var), levels = fill_order)]
    
    p <- ggplot(agg,
                aes(x = continent_consumer, y = value,
                    fill = .data[[group_var]])) +
      geom_col(position = position_stack(reverse = FALSE)) +
      geom_hline(yintercept = 0, linewidth = 0.3) +
      labs(
        title    = paste0("SDA driver decomposition (chained 2012–2022, ",
                          short_lbl, ") — ", eff),
        subtitle = paste0("Stacked by ", group_var, ", per consumer continent"),
        x        = NULL,
        y        = y_axis_label,
        fill     = group_var
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x        = element_text(angle = 30, hjust = 1),
        panel.grid.major.x = element_blank(),
        legend.position    = "right"
      )
    
    if (!is.null(palette)) p <- p + scale_fill_manual(values = palette)
    p
  }
  
  # --- 2. Crop palette -------------------------------------------------------
  item_palette <- c(
    "Sunflower seed"       = "#F59E0B",
    "Rape and Mustardseed" = "#B7791F",
    "Oil, palm fruit"      = "#DC2626",
    "Soyabeans"            = "#B45309",
    "Fodder crops"         = "#78350F",
    "Wheat and products"   = "#A3E635",
    "Maize and products"   = "#65A30D",
    "Sorghum and products" = "#166534",
    "Rice and products"    = "#059669",
    "Sugar cane"           = "#0D9488",
    "Cassava and products" = "#155E75",
    "Other"                = "#9CA3AF"
  )
  
  item_order <- c(
    "Other",
    "Wheat and products", "Maize and products", "Sorghum and products",
    "Rice and products",  "Sugar cane",         "Cassava and products",
    "Fodder crops",       "Soyabeans",          "Oil, palm fruit",
    "Rape and Mustardseed", "Sunflower seed"
  )
  
  commodity_order <- plot_base[
    effect %in% c("scale", "composition"),
    .(tot = sum(abs(value), na.rm = TRUE)), by = commodity
  ][order(-tot), commodity]
  
  cont_order <- plot_base[
    effect %in% c("intensity", "feedstock_mix", "sourcing",
                  "scale", "composition", "origin"),
    .(total = sum(pmax(value, 0), na.rm = TRUE)),
    by = continent_consumer
  ][order(-total), continent_consumer]
  
  # --- 3. Specs --------------------------------------------------------------
  specs <- list(
    intensity     = list(group_var = "item_origin", top_n = NULL,
                         palette = item_palette, fill_order = item_order),
    feedstock_mix = list(group_var = "item_origin", top_n = NULL,
                         palette = item_palette, fill_order = item_order),
    sourcing      = list(group_var = "continent_origin", top_n = NULL,
                         palette = NULL,         fill_order = cont_order),
    scale         = list(group_var = "commodity",        top_n = NULL,
                         palette = NULL,         fill_order = commodity_order),
    composition   = list(group_var = "commodity",        top_n = NULL,
                         palette = NULL,         fill_order = commodity_order),
    origin        = list(group_var = "continent_origin", top_n = NULL,
                         palette = NULL,         fill_order = cont_order)
  )
  
  # --- 4. Build all six plots -------------------------------------------------
  plots <- lapply(names(specs), function(eff) {
    s <- specs[[eff]]
    make_sda_driver_plot(plot_base, eff,
                         group_var  = s$group_var,
                         top_n      = s$top_n,
                         palette    = s$palette,
                         cont_order = cont_order,
                         fill_order = s$fill_order)
  })
  names(plots) <- names(specs)
  
  # --- 5. Save ---------------------------------------------------------------
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  for (eff in names(plots)) {
    ggsave(
      filename = file.path(output_dir,
                           sprintf("FABIO_SDA_chained_2012_2022_%s_drivers_%s.pdf",
                                   indicator, eff)),
      device = cairo_pdf,
      plot   = plots[[eff]],
      width  = width, height = height, dpi = dpi
    )
  }
  
  invisible(list(plots = plots, plot_base = plot_base))
}

# --- Usage -----------------------------------------------------------------
plot_SDA_chained_drivers("ibif_total")



######################################################################################################################
#3b. SDA chained driver decomposition — summed across one consumer continent (default EU)
######################################################################################################################

plot_SDA_chained_drivers_continent <- function(indicator,
                                               consumer_continent = "EU",
                                               output_dir = file.path("output", "plot"),
                                               width = 9, height = 6, dpi = 300) {
  
  # --- Indicator metadata ----------------------------------------------------
  ind  <- indicator
  meta <- indicator_meta[indicator == ind]
  if (nrow(meta) == 0L) stop("Unknown indicator: ", ind, ". Add it to indicator_meta.")
  scale_factor <- meta$scale_factor
  y_axis_label <- paste0("Contribution to Δ ", meta$y_label)
  short_lbl    <- meta$short_label
  cons_lbl     <- paste(consumer_continent, collapse = " + ")
  
  # --- 0. Read, rescale, map continents, then FILTER consumers ---------------
  path <- sprintf(file.path(IN_DIR, "FABIO_SDA_chained_2012_2022_%s_value_BF_drivers.csv"),
                  indicator)
  raw <- fread(path)
  raw[, value := value / scale_factor]
  
  raw[regions, on = c(country_consumer = "iso3c"), continent_consumer := i.continent]
  raw[regions, on = c(country_origin   = "iso3c"), continent_origin   := i.continent]
  
  raw <- raw[continent_consumer %in% consumer_continent]
  if (nrow(raw) == 0L) stop("No rows with continent_consumer in: ", cons_lbl)
  
  # Consumer dimension is collapsed: group only by the driver dims -------------
  effect_groups <- list(
    intensity     = c("continent_origin", "item_origin"),
    feedstock_mix = c("item_origin"),
    sourcing      = c("continent_origin", "item_origin"),
    scale         = c("commodity"),
    origin        = c("continent_origin", "item_origin", "commodity"),
    composition   = c("commodity")
  )
  
  plot_base <- rbindlist(
    lapply(names(effect_groups), function(eff) {
      gv <- effect_groups[[eff]]
      raw[effect == eff, .(value = sum(value, na.rm = TRUE)), by = gv][, effect := eff][]
    }),
    use.names = TRUE, fill = TRUE
  )
  
  # --- 1. Helper: one effect, drivers on the x-axis ---------------------------
  make_sda_driver_plot <- function(dt, eff, group_var,
                                   palette    = NULL,
                                   fill_order = NULL,
                                   top_n      = NULL) {
    sub <- dt[effect == eff]
    
    if (!is.null(top_n)) {
      ranked <- sub[, .(tot = sum(abs(value), na.rm = TRUE)), by = c(group_var)][order(-tot)]
      keep   <- ranked[seq_len(min(top_n, nrow(ranked))), get(group_var)]
      sub[, (group_var) := fifelse(get(group_var) %in% keep, get(group_var), "Other")]
    }
    
    # NEW: fold anything without a defined level into "Other" so factor() never NA-coerces
    if (!is.null(fill_order) && "Other" %in% fill_order) {
      sub[!(get(group_var) %in% fill_order), (group_var) := "Other"]
    }
    
    agg <- sub[, .(value = sum(value, na.rm = TRUE)), by = c(group_var)]
    
    if (is.null(fill_order)) {
      fill_order <- agg[order(-abs(value)), get(group_var)]
    }
    fill_order <- fill_order[fill_order %in% agg[[group_var]]]
    if ("Other" %in% fill_order) fill_order <- c(setdiff(fill_order, "Other"), "Other")
    agg[, (group_var) := factor(get(group_var), levels = fill_order)]
    
    p <- ggplot(agg, aes(x = .data[[group_var]], y = value, fill = .data[[group_var]])) +
      geom_col(width = 0.75) +
      geom_hline(yintercept = 0, linewidth = 0.3) +
      labs(
        title    = paste0("SDA driver decomposition (chained 2012–2022, ",
                          short_lbl, ") — ", eff),
        subtitle = paste0("Summed across all ", cons_lbl, " consumer countries; broken down by ", group_var),
        x        = NULL,
        y        = y_axis_label,
        fill     = group_var
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x        = element_text(angle = 30, hjust = 1),
        panel.grid.major.x = element_blank(),
        legend.position    = "none"
      )
    
    if (!is.null(palette)) p <- p + scale_fill_manual(values = palette)
    p
  }
  
  # --- 2. Crop palette (unchanged) -------------------------------------------
  item_palette <- c(
    "Sunflower seed"       = "#F59E0B",
    "Rape and Mustardseed" = "#B7791F",
    "Oil, palm fruit"      = "#DC2626",
    "Soyabeans"            = "#B45309",
    "Fodder crops"         = "#78350F",
    "Wheat and products"   = "#A3E635",
    "Maize and products"   = "#65A30D",
    "Sorghum and products" = "#166534",
    "Rice and products"    = "#059669",
    "Sugar cane"           = "#0D9488",
    "Cassava and products" = "#155E75",
    "Other"                = "#9CA3AF"
  )
  
  item_order <- c(
    "Wheat and products", "Maize and products", "Sorghum and products",
    "Rice and products",  "Sugar cane",         "Cassava and products",
    "Fodder crops",       "Soyabeans",          "Oil, palm fruit",
    "Rape and Mustardseed", "Sunflower seed",   "Other"
  )
  
  commodity_order <- plot_base[
    effect %in% c("scale", "composition"),
    .(tot = sum(abs(value), na.rm = TRUE)), by = commodity
  ][order(-tot), commodity]
  
  origin_order <- plot_base[
    effect %in% c("intensity", "sourcing", "origin"),
    .(tot = sum(abs(value), na.rm = TRUE)), by = continent_origin
  ][order(-tot), continent_origin]
  
  # --- 3. Specs --------------------------------------------------------------
  specs <- list(
    intensity     = list(group_var = "item_origin", top_n = NULL,
                         palette = item_palette, fill_order = item_order),
    feedstock_mix = list(group_var = "item_origin", top_n = NULL,
                         palette = item_palette, fill_order = item_order),
    sourcing      = list(group_var = "continent_origin", top_n = NULL,
                         palette = NULL,         fill_order = origin_order),
    scale         = list(group_var = "commodity",        top_n = NULL,
                         palette = NULL,         fill_order = commodity_order),
    composition   = list(group_var = "commodity",        top_n = NULL,
                         palette = NULL,         fill_order = commodity_order),
    origin        = list(group_var = "continent_origin", top_n = NULL,
                         palette = NULL,         fill_order = origin_order)
  )
  
  plots <- lapply(names(specs), function(eff) {
    s <- specs[[eff]]
    make_sda_driver_plot(plot_base, eff,
                         group_var  = s$group_var,
                         top_n      = s$top_n,
                         palette    = s$palette,
                         fill_order = s$fill_order)
  })
  names(plots) <- names(specs)
  
  # --- 4. Save ---------------------------------------------------------------
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  tag <- paste(consumer_continent, collapse = "-")
  for (eff in names(plots)) {
    ggsave(
      filename = file.path(output_dir,
                           sprintf("FABIO_SDA_chained_2012_2022_%s_drivers_%s_%s.pdf",
                                   indicator, eff, tag)),
      device = cairo_pdf,
      plot   = plots[[eff]],
      width  = width, height = height, dpi = dpi
    )
  }
  
  invisible(list(plots = plots, plot_base = plot_base))
}

# --- Usage -----------------------------------------------------------------
plot_SDA_chained_drivers_continent("ibif_total", consumer_continent = "EU")
