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

# ---- CAPPING VARIANT (default = capped) --------------------------------------
# FABIO_VARIANT = "capped" (default) or "uncapped". SDA CSVs come from 18_02 and
# are variant-dependent; plots are written to a per-variant dir.
VARIANT   <- tolower(trimws(Sys.getenv("FABIO_VARIANT", unset = "capped")))
is_capped <- VARIANT == "capped"
vsuf      <- if (is_capped) "_capped" else ""
IN_DIR    <- paste0(if (model_version == "bypass") "output/bypass" else "output", vsuf)
PLOT_DIR  <- file.path(IN_DIR, "plot")
message(sprintf(">>> [19_02] model_version = '%s' | variant = '%s'  (SDA in: %s | plots: %s)",
                model_version, VARIANT, IN_DIR, PLOT_DIR))
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)


######################################################################################################################
############################## Output device #######################
######################################################################################################################
# Every figure is written as SVG.
#
# grDevices::svg() is the cairo SVG device: it converts every glyph to an outline
# path, so the resulting file has no real text - labels cannot be selected,
# searched, restyled or edited downstream, and the file is larger. svglite writes
# text as <text> elements and produces much smaller files. Prefer it; fall back to
# cairo only if svglite is not installed.
svg_device <- if (requireNamespace("svglite", quietly = TRUE)) {
  svglite::svglite
} else {
  message(">>> [19_02] svglite not installed - falling back to grDevices::svg(); ",
          "text in the output will be glyph outlines, not editable text. ",
          "install.packages(\"svglite\") to fix.")
  grDevices::svg
}


######################################################################################################################
############################## Setting colors #######################
######################################################################################################################

fuel_colors <- c(
  "Biogasoline"       = "#D4A017",
  "Biodiesel"        = "#5B2C6F",
  "Renewable diesel" = "#9B59B6"
)


# ---- Max-contrast pool: Starchy / Sugar crops (12 slots) -------------------
# min pairwise dE2000 ~ 20.1
starchy_pool <- c(
  "#61C748",  # leaf green
  "#9E1597",  # magenta
  "#00441B",  # dark forest green
  "#F53666",  # raspberry / pink-red
  "#1DC6AF",  # teal
  "#874A18",  # brown
  "#3EBBFF",  # sky blue
  "#5A5D05",  # dark olive
  "#CF9EEB",  # light lavender
  "#CAAE5D",  # khaki / gold
  "#3257A0",  # royal blue
  "#F69978"   # salmon (reserve slot)
)

# ---- Max-contrast pool: Oilcrops (12 slots) --------------------------------
# min pairwise dE2000 ~ 18.1
oilcrop_pool <- c(
  "#8EC029",  # yellow-green
  "#7E34B1",  # purple
  "#EEA119",  # amber / orange
  "#E63DA7",  # hot pink
  "#016833",  # deep green
  "#FD938A",  # light coral
  "#543005",  # espresso
  "#B4A6F8",  # periwinkle
  "#878834",  # olive
  "#3588D1",  # azure blue
  "#9B3649",  # maroon (reserve slot)
  "#B87340"   # tan / ochre (reserve slot)
)

# ---- Neutral pool: Other / Other, Waste ------------------------------------
other_pool <- c(
  "Other, Waste" = "#4D4D4D",  # dark grey
  "Other"        = "#999999"   # light grey
)

# ---- Fixed name -> colour assignment ---------------------------------------
# Ordering follows the legend: starchy/sugar crops first, then oilcrops,
# then the two neutrals. Names match feedstock_meta$feedstock exactly.
starchy_names <- c(
  "Sugar cane",
  "Maize and products",
  "Molasses",
  "Cassava and products",
  "Sugar beet",
  "Wheat and products",
  "Rice and products",
  "Sorghum and products",
  "Rye and products",
  "Triticale",
  "Barley and products"
)

oilcrop_names <- c(
  "Palm Oil",
  "Soyabean Oil",
  "Rape and Mustard Oil",
  "Used Cooking Oil",
  "Fats, Animals, Raw",
  "Maize Germ Oil",
  "Sunflowerseed Oil",
  "Coconut Oil",
  "Palmkernel Oil",
  "Cottonseed Oil"
)

feedstock_color_map <- c(
  setNames(starchy_pool[seq_along(starchy_names)], starchy_names),
  setNames(oilcrop_pool[seq_along(oilcrop_names)], oilcrop_names),
  other_pool
)


# ---- item (total requirements) -> feedstock (direct requirements) ----------
# The SDA driver tables are expressed in TOTAL requirements: `item_origin` is
# whatever the Leontief inverse resolves to upstream (crops, live animals,
# grazing land, crushed oils...). The feedstock palette is defined on DIRECT
# requirements, i.e. the first-degree biofuel feedstocks. The map below walks
# each item FORWARD along the supply chain to the direct feedstock it
# terminates in, so the two figures speak the same colour language.
#
# The relation is many-to-one by construction: e.g. Grazing -> Fodder crops ->
# Cattle/Buffaloes/Goats all terminate in tallow ("Fats, Animals, Raw"), and
# oilseed + crushed oil pairs collapse onto the oil. Items absent from the map
# fall through to "Other" (audited residual < 0.6% of any continent/effect).
item_to_feedstock <- c(
  # --- starchy / sugar -> bioethanol feedstocks (identity) ------------------
  "Sugar cane"                   = "Sugar cane",
  "Molasses"                     = "Molasses",
  "Sugar beet"                   = "Sugar beet",
  "Maize and products"           = "Maize and products",
  "Wheat and products"           = "Wheat and products",
  "Rice and products"            = "Rice and products",
  "Sorghum and products"         = "Sorghum and products",
  "Rye and products"             = "Rye and products",
  "Triticale"                    = "Triticale",
  "Barley and products"          = "Barley and products",
  "Cassava and products"         = "Cassava and products",
  # --- oilcrops -> the crushed oil that is the actual feedstock ------------
  "Oil, palm fruit"              = "Palm Oil",
  "Palm Oil"                     = "Palm Oil",
  "Palm kernels"                 = "Palmkernel Oil",
  "Palmkernel Oil"               = "Palmkernel Oil",
  "Soyabeans"                    = "Soyabean Oil",
  "Soyabean Oil"                 = "Soyabean Oil",
  "Rape and Mustardseed"         = "Rape and Mustard Oil",
  "Rape and Mustard Oil"         = "Rape and Mustard Oil",
  "Sunflower seed"               = "Sunflowerseed Oil",
  "Sunflowerseed Oil"            = "Sunflowerseed Oil",
  "Maize Germ Oil"               = "Maize Germ Oil",
  "Coconuts - Incl Copra"        = "Coconut Oil",
  "Coconut Oil"                  = "Coconut Oil",
  "Seed cotton"                  = "Cottonseed Oil",
  "Cottonseed Oil"               = "Cottonseed Oil",
  # --- livestock + their feed base -> tallow / Cat 1-3 ---------------------
  "Cattle"                       = "Fats, Animals, Raw",
  "Buffaloes"                    = "Fats, Animals, Raw",
  "Goats"                        = "Fats, Animals, Raw",
  "Sheep"                        = "Fats, Animals, Raw",
  "Pigs"                         = "Fats, Animals, Raw",
  "Poultry Birds"                = "Fats, Animals, Raw",
  "Milk - Excluding Butter"      = "Fats, Animals, Raw",
  "Grazing"                      = "Fats, Animals, Raw",
  "Fodder crops"                 = "Fats, Animals, Raw",
  "Fats, Animals, Raw"           = "Fats, Animals, Raw",
  # --- food oils reaching the fuel chain as spent cooking oil --------------
  "Olives (including preserved)" = "Used Cooking Oil",
  "Olive Oil"                    = "Used Cooking Oil",
  "Sesame seed"                  = "Used Cooking Oil",
  "Sesameseed Oil"               = "Used Cooking Oil",
  "Groundnut Oil"                = "Used Cooking Oil"
)

map_to_feedstock <- function(x, map = item_to_feedstock, fallback = "Other") {
  out <- unname(map[x])
  out[is.na(out)] <- fallback
  out
}


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
############################## SDA EFFECT VOCABULARY #######################
######################################################################################################################
# Single source of truth for the effect names written by 18_02. The decomposition
# reports SEVEN terms over six factors: Delta_Phi is split into feedstock_conv and
# feedstock_mix, which sum to it. Any function that filters or factors `effect`
# must use this vector - a hard-coded subset silently drops a term, and the stacked
# bars then no longer sum to the delta line.

SDA_EFFECT_LEVELS <- c("intensity", "sourcing", "feedstock_conv", "feedstock_mix",
                       "scale", "composition", "origin")

# Colours pinned BY NAME rather than by position, so adding or reordering a term
# does not silently recolour every previous figure.
SDA_EFFECT_COLORS <- c(
  intensity      = "#66C2A5",
  sourcing       = "#FC8D62",
  feedstock_conv = "#8DA0CB",
  feedstock_mix  = "#E78AC3",
  scale          = "#A6D854",
  composition    = "#FFD92F",
  origin         = "#B3B3B3"
)

# Fail loudly if the SDA file carries an effect this script does not know about.
check_sda_effects <- function(dt) {
  extra <- setdiff(unique(dt$effect),
                   c(SDA_EFFECT_LEVELS, "total_t0", "total_t1", "delta"))
  if (length(extra))
    stop("SDA file contains effect(s) not in SDA_EFFECT_LEVELS: ",
         paste(extra, collapse = ", "),
         ". Update SDA_EFFECT_LEVELS, or the stacked bars will not sum to delta.")
  invisible(TRUE)
}


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
# ggsave(filename = file.path("output", "plot", "2012_2022_SDA_continent.svg"),
#        device = svg_device,
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
                           output_dir = PLOT_DIR,
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
  check_sda_effects(SDA_chain)
  
  SDA_cumul <- SDA_chain[
    effect %in% c(SDA_EFFECT_LEVELS, "delta"),
    .(value = sum(value)),
    by = .(country_consumer, effect)
  ]
  
  # --- Top N countries by signed cumulative delta ---------------------------
  topN          <- SDA_cumul[effect == "delta"][order(-value)][seq_len(top_n)]
  country_order <- topN$country_consumer
  
  traj <- SDA_chain[
    country_consumer %in% country_order &
      effect %in% SDA_EFFECT_LEVELS
  ]
  
  year_levels <- sort(unique(traj$year_current))
  traj[, country_consumer := factor(country_consumer, levels = country_order)]
  traj[, effect           := factor(effect, levels = SDA_EFFECT_LEVELS)]
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
    scale_fill_manual(values = SDA_EFFECT_COLORS, drop = FALSE) +
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
    sprintf("%d_%d_SDA_chain_ts_%s_BF.svg",
            first_year, last_year, extension)
  )
  ggsave(filename = out_file,
         device   = svg_device,
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
                                     anchor_zero    = TRUE,   # cumulate from 0 in the base year (= first_year)
                                     show_total     = TRUE,   # overlay the NET cumulative delta (see note below)
                                     input_dir      = IN_DIR,
                                     output_dir     = PLOT_DIR,
                                     width          = 12,
                                     height         = 12) {
  
  effect_levels <- SDA_EFFECT_LEVELS
  
  # --- Build filename and read ----------------------------------------------
  in_file  <- file.path(
    input_dir,
    sprintf("FABIO_SDA_chained_%d_%d_%s_value_BF.csv",
            first_year, last_year, extension)
  )
  SDA_chain_country <- fread(in_file)
  check_sda_effects(SDA_chain_country)
  
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
      effect %in% c(effect_levels, "delta"),
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
    continent %in% continent_order & effect %in% effect_levels
  ]
  if (nrow(traj) == 0L) stop("No rows left after continent/effect filtering.")
  
  # year_current stays NUMERIC: cumulative paths are levels through time, so an
  # interval x-axis keeps the year spacing (and any gap) honest.
  year_levels <- sort(unique(as.integer(traj$year_current)))
  base_year   <- min(year_levels) - 1L   # the t0 of the first year-pair
  
  # --- Running cumulative contribution PER EFFECT ---------------------------
  cum_eff <- traj[order(continent, effect, year_current),
                  .(year_current = as.integer(year_current),
                    cum_value    = cumsum(value)),
                  by = .(continent, effect)]
  
  # --- Running cumulative Δ, total ------------------------------------------
  cum_delta <- SDA_chain[
    continent %in% continent_order & effect == "delta"
  ][order(continent, year_current),
    .(year_current = as.integer(year_current), cum_value = cumsum(value)),
    by = continent]
  
  # --- Anchor the base year at 0 (bars are drawn from base_year + 1) --------
  if (anchor_zero) {
    anchor_eff <- CJ(continent = continent_order, effect = effect_levels, sorted = FALSE)
    anchor_eff[, `:=`(year_current = base_year, cum_value = 0)]
    cum_eff <- rbindlist(list(cum_eff, anchor_eff), use.names = TRUE)
    
    anchor_tot <- data.table(continent    = continent_order,
                             year_current = base_year,
                             cum_value    = 0)
    cum_delta <- rbindlist(list(cum_delta, anchor_tot), use.names = TRUE)
  }
  
  cum_eff[,   continent := factor(continent, levels = continent_order)]
  cum_delta[, continent := factor(continent, levels = continent_order)]
  cum_eff[,   effect    := factor(effect,    levels = effect_levels)]
  setorder(cum_eff,   continent, effect, year_current)
  setorder(cum_delta, continent, year_current)
  
  bars  <- cum_eff[year_current > base_year]
  tot   <- cum_delta[year_current > base_year]
  
  # --- Title reflects the selection mode -------------------------------------
  selection_label <- if (!is.null(continents)) {
    paste(setdiff(continent_order, "World"), collapse = ", ")
  } else {
    sprintf("top %d", top_n)
  }
  if (include_global) selection_label <- paste0(selection_label, " + World")
  
  subtitle_txt <- paste0(
    "Stacked cumulative contribution of each effect since ", base_year,
    if (show_total) "; black diamonds = net cumulative Δ (positive and negative effects stack on opposite sides of 0)" else ""
  )
  
  # --- Plot -----------------------------------------------------------------
  p <- ggplot(bars, aes(x = year_current, y = cum_value, fill = effect)) +
    geom_col(position = position_stack(), width = 0.7) +
    geom_hline(yintercept = 0, linewidth = 0.3)
  
  if (show_total) {
    # Kept on purpose: with mixed signs, position_stack() splits positives above and
    # negatives below zero, so the top of the stack is NOT the net cumulative Δ.
    p <- p +
      geom_line(data = tot, aes(x = year_current, y = cum_value, group = 1),
                inherit.aes = FALSE, linewidth = 0.5, colour = "black") +
      geom_point(data = tot, aes(x = year_current, y = cum_value),
                 inherit.aes = FALSE, size = 1.6, shape = 18, colour = "black")
  }
  
  p <- p +
    facet_wrap(~ continent, scales = "free_y") +
    scale_x_continuous(breaks = year_levels) +
    scale_fill_manual(values = SDA_EFFECT_COLORS, drop = FALSE) +
    labs(
      title    = sprintf("Cumulative SDA effects, annual chain %d-%d — %s (%s)",
                         first_year, last_year, selection_label, extension),
      subtitle = subtitle_txt,
      x        = "Year",
      y        = sprintf("Cumulative contribution to Δ footprint since %d", base_year),
      fill     = "Effect"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position    = "bottom",
          axis.text.x        = element_text(angle = 45, hjust = 1),
          panel.grid.minor.x = element_blank())
  
  # --- Save -----------------------------------------------------------------
  out_file <- file.path(
    output_dir,
    sprintf("%d_%d_SDA_chain_ts_%s_continent_BF.svg",
            first_year, last_year, extension)
  )
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  ggsave(filename = out_file,
         device   = svg_device,
         plot     = p,
         width    = width, height = height, dpi = 300)
  
  invisible(p)
}



######################################################################################################################
#3. Plot SDA chained driver decomposition by effect by consumer continent
######################################################################################################################

plot_SDA_chained_drivers <- function(indicator,
                                     top_n_feedstock = 12,
                                     output_dir = PLOT_DIR,
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
  
  # Collapse total-requirement items onto their direct biofuel feedstock, so the
  # driver figures share the feedstock palette. Many-to-one; see item_to_feedstock.
  raw[, feedstock := map_to_feedstock(item_origin)]
  
  effect_groups <- list(
    intensity      = c("continent_consumer", "continent_origin", "feedstock"),
    feedstock_conv = c("continent_consumer", "commodity"),
    feedstock_mix  = c("continent_consumer", "feedstock"),
    sourcing       = c("continent_consumer", "continent_origin", "feedstock"),
    scale         = c("continent_consumer", "commodity"),
    origin        = c("continent_consumer", "continent_origin", "feedstock", "commodity"),
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
  make_sda_driver_plot <- function(dt, eff, group_var,
                                   palette    = NULL,
                                   cont_order = NULL,
                                   fill_order = NULL,
                                   top_n      = NULL) {
    sub <- copy(dt[effect == eff])
    
    # Keep the top_n categories by NET magnitude (= the bar height actually
    # drawn). "Other" never competes for a slot; it absorbs the remainder.
    if (!is.null(top_n)) {
      ranked <- sub[get(group_var) != "Other",
                    .(tot = abs(sum(value, na.rm = TRUE))),
                    by = c(group_var)][order(-tot)]
      keep <- ranked[seq_len(min(top_n, nrow(ranked))), get(group_var)]
      sub[!(get(group_var) %in% keep), (group_var) := "Other"]
    }
    
    # Safety net: any category without a colour -> "Other", so factor() below
    # can never NA-coerce and scale_fill_manual() can never miss a key.
    if (!is.null(palette)) {
      sub[!(get(group_var) %in% names(palette)), (group_var) := "Other"]
    }
    
    agg <- sub[, .(value = sum(value, na.rm = TRUE)),
               by = c("continent_consumer", group_var)]
    
    if (is.null(cont_order)) {
      cont_order <- agg[, .(total = sum(pmax(value, 0), na.rm = TRUE)),
                        by = continent_consumer][order(-total), continent_consumer]
    }
    agg[, continent_consumer := factor(continent_consumer, levels = cont_order)]
    
    # Legend order follows the palette's declared order, so it matches the
    # feedstock figures; magnitude fallback when no palette is supplied.
    if (is.null(fill_order)) {
      fill_order <- if (!is.null(palette)) names(palette)
      else agg[, .(tot = sum(abs(value))), by = c(group_var)][
        order(-tot), get(group_var)]
    }
    fill_order <- fill_order[fill_order %in% agg[[group_var]]]
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
  
  # --- 2. Orders -------------------------------------------------------------
  commodity_order <- plot_base[
    effect %in% c("scale", "composition"),
    .(tot = sum(abs(value), na.rm = TRUE)), by = commodity
  ][order(-tot), commodity]
  
  cont_order <- plot_base[
    effect %in% SDA_EFFECT_LEVELS,
    .(total = sum(pmax(value, 0), na.rm = TRUE)),
    by = continent_consumer
  ][order(-total), continent_consumer]
  
  # --- 3. Specs --------------------------------------------------------------
  # Item-based effects group by FEEDSTOCK (many-to-one collapse of item_origin)
  # and show the top_n_feedstock categories by net magnitude, rest in "Other".
  specs <- list(
    intensity     = list(group_var = "feedstock", top_n = top_n_feedstock,
                         palette = feedstock_color_map, fill_order = NULL),
    feedstock_conv = list(group_var = "commodity", top_n = NULL,
                          palette = NULL,         fill_order = commodity_order),
    feedstock_mix = list(group_var = "feedstock", top_n = top_n_feedstock,
                         palette = feedstock_color_map, fill_order = NULL),
    sourcing      = list(group_var = "continent_origin", top_n = NULL,
                         palette = NULL,         fill_order = cont_order),
    scale         = list(group_var = "commodity",        top_n = NULL,
                         palette = NULL,         fill_order = commodity_order),
    composition   = list(group_var = "commodity",        top_n = NULL,
                         palette = NULL,         fill_order = commodity_order),
    origin        = list(group_var = "continent_origin", top_n = NULL,
                         palette = NULL,         fill_order = cont_order)
  )
  
  # --- 4. Build one plot per reported effect ----------------------------------
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
                           sprintf("FABIO_SDA_chained_2012_2022_%s_drivers_%s.svg",
                                   indicator, eff)),
      device = svg_device,
      plot   = plots[[eff]],
      width  = width, height = height, dpi = dpi
    )
  }
  
  invisible(list(plots = plots, plot_base = plot_base))
}




######################################################################################################################
#3b. SDA chained driver decomposition — summed across one consumer continent (default EU)
######################################################################################################################

plot_SDA_chained_drivers_continent <- function(indicator,
                                               consumer_continent = "EU",
                                               top_n_feedstock = 12,
                                               output_dir = PLOT_DIR,
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
  
  # Collapse total-requirement items onto their direct biofuel feedstock.
  raw[, feedstock := map_to_feedstock(item_origin)]
  
  # Consumer dimension is collapsed: group only by the driver dims -------------
  effect_groups <- list(
    intensity      = c("continent_origin", "feedstock"),
    feedstock_conv = c("commodity"),
    feedstock_mix  = c("feedstock"),
    sourcing       = c("continent_origin", "feedstock"),
    scale         = c("commodity"),
    origin        = c("continent_origin", "feedstock", "commodity"),
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
    sub <- copy(dt[effect == eff])
    
    # Keep the top_n categories by NET magnitude; "Other" absorbs the rest.
    if (!is.null(top_n)) {
      ranked <- sub[get(group_var) != "Other",
                    .(tot = abs(sum(value, na.rm = TRUE))),
                    by = c(group_var)][order(-tot)]
      keep <- ranked[seq_len(min(top_n, nrow(ranked))), get(group_var)]
      sub[!(get(group_var) %in% keep), (group_var) := "Other"]
    }
    
    # Safety net: any category without a colour -> "Other".
    if (!is.null(palette)) {
      sub[!(get(group_var) %in% names(palette)), (group_var) := "Other"]
    }
    
    agg <- sub[, .(value = sum(value, na.rm = TRUE)), by = c(group_var)]
    
    if (is.null(fill_order)) {
      fill_order <- if (!is.null(palette)) names(palette)
      else agg[order(-abs(value)), get(group_var)]
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
  
  # --- 2. Orders -------------------------------------------------------------
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
    intensity     = list(group_var = "feedstock", top_n = top_n_feedstock,
                         palette = feedstock_color_map, fill_order = NULL),
    feedstock_conv = list(group_var = "commodity", top_n = NULL,
                          palette = NULL,         fill_order = commodity_order),
    feedstock_mix = list(group_var = "feedstock", top_n = top_n_feedstock,
                         palette = feedstock_color_map, fill_order = NULL),
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
                           sprintf("FABIO_SDA_chained_2012_2022_%s_drivers_%s_%s.svg",
                                   indicator, eff, tag)),
      device = svg_device,
      plot   = plots[[eff]],
      width  = width, height = height, dpi = dpi
    )
  }
  
  invisible(list(plots = plots, plot_base = plot_base))
}