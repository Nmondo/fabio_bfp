# 19 - Plot results

setwd("/home/mmondolfo/fabio_bfp/")

# Setup ------------------------------------------------------------------------
library(data.table)
library(Matrix)
library(tidyverse)
library(ggplot2)
library(paletteer)
library(RColorBrewer)
library(gridExtra)


items_full_bcp <- read_csv("inst/items_full_bcp.csv")
items_full_bcp <- as.data.table(items_full_bcp)
regions <- setDT(read_csv("inst/regions.csv"))[current==TRUE]


######################################################################################################################
############################## Setting colors #######################
######################################################################################################################

fuel_colors <- c(
  "Biogasoline"       = "#D4A017",
  "Biodiesel"        = "#5B2C6F",
  "Renewable diesel" = "#9B59B6"
)


######################################################################################################################
############################## DESCRIPTIVE STATS #######################
######################################################################################################################

######################################################################################################################
#1. Descriptive statistics for consumption / fuel types.
######################################################################################################################

Y_summary <- fread("output/Y_summary_c146_c147_c149.csv")
Z_summary <- fread("output/Z_summary_c146_c147_c149.csv")



Y_per_year_global <- Y_summary %>%
  group_by(year, comm_code) %>%
  summarize(value = sum(value)/1000,
            .groups = "drop") %>%
  left_join(items_full_bcp %>% select(comm_code, item), by = "comm_code")

# order by 2012 value, descending (largest at bottom)
item_order <- Y_per_year_global[
  Y_per_year_global$year == min(Y_per_year_global$year),
] |>
  (\(x) x[order(-x$value), "item", drop = TRUE])()

Y_per_year_global$item <- factor(Y_per_year_global$item, levels = rev(item_order))

totals <- Y_per_year_global |>
  dplyr::group_by(year) |>
  dplyr::summarize(total = sum(value), .groups = "drop")


## Making plot 
Y_global_plot <- ggplot(Y_per_year_global, aes(x = year, y = value, fill = item)) +
  geom_bar(stat = "identity") +
  # per-segment values, centered, in white
  geom_text(aes(label = scales::label_number(accuracy = 1)(value)),
            position = position_stack(vjust = 0.5),
            size = 2.8, color = "white") +
  # annual totals on top
  geom_text(data = totals,
            aes(x = year, y = total, label = scales::label_number(accuracy = 1)(total)),
            inherit.aes = FALSE,
            vjust = -0.5, size = 3, fontface = "bold") +
  scale_fill_manual(values = fuel_colors) +
  scale_x_continuous(breaks = unique(Y_per_year_global$year)) +
  scale_y_continuous(labels = scales::label_number(),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Year", y = "Biofuel consumption (M liters)",
       fill = "Fuel type",
       title = "Global biofuel consumption by fuel type") +
  theme_minimal()

## Saving
ggsave(filename = file.path("output", "plot", "Y_global_2012_2022.pdf"),
       plot = Y_global_plot,
       width = 10, height = 6, dpi = 300)

######################################################################################################################
#2. Descriptive statistics for production technologies
######################################################################################################################

# # Is there any point in this? It's implicit and more detailed in the later _c146, ... tables.
# Z_per_year_global <- Z_summary %>%
#   group_by(origin_comm_name, year) %>%
#   summarize(value = sum(value)/1000,
#             .groups = "drop") %>%
#   filter(origin_comm_name != "Other, Unknown") %>%
#   mutate(origin_comm_name = ifelse(grepl("^Animal or vegetable fats and oils", origin_comm_name), "Used Cooking Oil", origin_comm_name))



######################################################################################################################
############################## READ RESULTS DATA #######################
######################################################################################################################

# Match years 2012-2022
files_tradeFeed <- list.files(
  "output",
  pattern    = "^FABIO_tradeFeed_(201[2-9]|202[0-2])_.*\\.csv$",
  full.names = TRUE
)

files_totalreq <- list.files(
  "output",
  pattern    = "^FABIO_totalreq_(201[2-9]|202[0-2])_.*\\.csv$",
  full.names = TRUE
)

# Read and combine into one data.table
dt_tradeFeed <- rbindlist(lapply(files_tradeFeed, fread))
dt_totalreq <- rbindlist(lapply(files_totalreq, fread))




######################################################################################################################
############################## FUNCTIONS TO PLOT RESULTS #######################
######################################################################################################################

tab_land_use <- subset(dt_tradeFeed, indicator == "land_harv")
tab_ibif <- subset(dt_tradeFeed, indicator == "ibif_total")
tab_lcim_freshwater <- subset(dt_tradeFeed, indicator == "LCIM_EQ_freshwater")
tab_lcim_marine <- subset(dt_tradeFeed, indicator == "LCIM_EQ_marine")
tab_lcim_terrestrial <- subset(dt_tradeFeed, indicator == "LCIM_EQ_terrestrial")


######################################################################################################################
#1. Plot countries' impact balance (exported, self, imported)
######################################################################################################################

# Helper: extract tidy plot data for one year
.balance_data <- function(data, Y_summary, year_select,
                          top_pos_n = 20, top_exp_n = 10) {
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
                         years     = c(2012, 2022),
                         top_pos_n = 20,
                         top_exp_n = 10) {
  
  dt <- as.data.table(data)
  Y  <- as.data.table(Y_summary)
  
  # ── Step 1: derive country list & ordering from the FIRST year ONLY ──────
  ref_year <- years[1]
  
  .get_plot_data <- function(year_select, countries_ref = NULL) {
    
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
      
      # Keep only the reference countries; add zero rows for missing ones
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
    
    # Apply the fixed factor ordering
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
    
    # ── Consumption line ────────────────────────────────────────────────
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
  countries <- ref_data$countries          # <── fixed, from ref year only
  
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
      axes = "all",
      scales = "fixed"        # ← fixed: same x-axis, same ordering, both panels
    ) +
    scale_y_continuous(
      name     = "Land use (1 000 ha)",
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

plot_commodity_feedstock <- function(data, commodity_select, top_n_feedstock = 10) {
    
    dt <- as.data.table(data)
    
    # --- filter commodity (all years)
    dt_sub <- dt[commodity == commodity_select & year %between% c(2012, 2022)]
    
    # --- recode feedstock
    dt_sub[grepl("^Animal or vegetable fats and oils", feedstock),
           feedstock := "Used Cooking Oil"]
    
    # --- global feedstock ranking across all years
    feedstock_rank <- dt_sub[
      , .(total = sum(value, na.rm = TRUE)),
      by = feedstock
    ][order(-total)]
    
    top_feedstocks <- head(feedstock_rank$feedstock, top_n_feedstock)
    
    # --- group feedstocks
    dt_sub[, feedstock_group := fifelse(feedstock %in% top_feedstocks,
                                        feedstock, "Other")]
    
    # --- aggregate by year × feedstock group
    agg <- dt_sub[
      , .(value = sum(value, na.rm = TRUE) / 1e3),
      by = .(year, feedstock_group)
    ]
    
    # --- order feedstocks (descending importance)
    feedstock_order <- agg[
      , .(total = sum(value)),
      by = feedstock_group
    ][rev(order(-total))]$feedstock_group
    
    agg[, feedstock_group := factor(feedstock_group, levels = feedstock_order)]
    
    # --- ensure year ordering
    agg[, year := factor(year, levels = sort(unique(year)))]
    
    commodity_name <- items_full_bcp[
      comm_code == commodity_select,
      item
    ]
    
    # --- plot
    p <- ggplot(agg, aes(x = year, y = value, fill = feedstock_group)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set3") +
      scale_y_continuous(labels = scales::label_number()) +
      labs(
        x = "Year",
        y = "Land use (1000 ha)",
        fill = "Feedstock",
        title = paste("Land use by feedstock for", commodity_name)
      ) +
      theme_minimal()
    
    return(p)
  }



# ── 1. Subset & aggregate ────────────────────────────────────────────────────
dt_ibif <- as.data.table(dt_tradeFeed)[
  grepl("ibif", indicator, ignore.case = TRUE) &
    indicator != "ibif_total" &
    year %in% c(2012, 2022)
]

dt_grouped <- dt_ibif[
  , .(value = sum(value, na.rm = TRUE)),
  by = .(country_consumer, indicator, year)
]

# ── 2. Identify top-20 countries by total across both years ──────────────────
top20 <- dt_grouped[
  , .(total = sum(value, na.rm = TRUE)),
  by = country_consumer
][order(-total)][1:20, country_consumer]

dt_plot <- dt_grouped[country_consumer %in% top20]
dt_plot[, country_consumer := factor(country_consumer, levels = top20)]
dt_plot[, year := factor(year, levels = c(2012, 2022))]

# ── 3. Consistent indicator colour palette ───────────────────────────────────
n_ind   <- length(unique(dt_plot$indicator))
pal     <- scales::hue_pal()(n_ind)         # swap for manual colours if desired
names(pal) <- sort(unique(dt_plot$indicator))

# ── 4. Plot ──────────────────────────────────────────────────────────────────
p <- ggplot(
  dt_plot,
  aes(
    x    = country_consumer,
    y    = value,
    fill = indicator
  )
) +
  geom_bar(
    stat     = "identity",
    position = "stack",
    colour   = NA          # no bar outlines – cleaner with many colours
  ) +
  facet_wrap(
    ~ year,
    nrow  = 2,
    axes  = "all",         # repeat x-axis on both panels
    scales = "fixed"       # identical y-scale for direct comparison
  ) +
  scale_y_continuous(
    name   = "Value",
    labels = label_number(big.mark = ",")
  ) +
  scale_x_discrete(name = "Country") +
  scale_fill_manual(
    values = pal,
    name   = "Indicator"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text          = element_text(size = 11, face = "bold", hjust = 0.5),
    axis.text.x         = element_text(angle = 45, hjust = 1),
    axis.title.x.bottom = element_text(margin = margin(t = 6)),
    legend.position     = "bottom",
    legend.direction    = "horizontal",
    legend.title        = element_text(face = "bold"),
    plot.margin         = margin(8, 8, 8, 8)
  )

print(p)

# optional save
# ggsave("ibif_top20_2012_2022.png", p, width = 14, height = 9, dpi = 150)






######################################################################################################################
############################## PLOT RESULTS #######################
######################################################################################################################

######################################################################################################################
#1. Plot countries' impact balance (exported, self, imported)
######################################################################################################################

plot_netLU_2012_2022 <- plot_balance(tab_land_use, Y_summary, years = c(2012, 2022))

plot_ibif_2012_2022 <- plot_balance(tab_ibif, Y_summary, years = c(2012, 2022)) ## Need to adapt the name of the axes. 

plot_lcim_fresh_2012_2022 <- plot_balance(tab_lcim_freshwater, Y_summary, years = c(2012, 2022)) ## Need to adapt the name of the axes. 
plot_lcim_marine_2012_2022 <- plot_balance(tab_lcim_marine, Y_summary, years = c(2012, 2022)) ## Need to adapt the name of the axes. 
plot_lcim_terrestrial_2012_2022 <- plot_balance(tab_lcim_terrestrial, Y_summary, years = c(2012, 2022)) ## Need to adapt the name of the axes. 


######################################################################################################################
#2. Plot impacts by commodity, by feedstock over time. 
######################################################################################################################

plot_commodity_feedstock(tab_land_use, "c146")
plot_commodity_feedstock(tab_land_use, "c147")
plot_commodity_feedstock(tab_land_use, "c149")

plot_commodity_feedstock(tab_lcim_terrestrial, "c147")
plot_commodity_feedstock(tab_lcim_terrestrial, "c146")




######################################################################################################################
############################## SAVE PLOTS #######################
######################################################################################################################

dir.create(file.path("output", "plot"), recursive = TRUE, showWarnings = FALSE)

ggsave(filename = file.path("output", "plot", "netLU_2012_2022.pdf"),
       plot = plot_netLU_2012_2022,
       width = 10, height = 10, dpi = 300)

ggsave(filename = file.path("output", "plot", "ibif_2012_2022.pdf"),
       plot = plot_ibif_2012_2022,
       width = 10, height = 10, dpi = 300)

ggsave(filename = file.path("output", "plot", "c146_feedstock.pdf"),
       plot = plot_commodity_feedstock(tab_land_use, "c146"),
       width = 10, height = 6, dpi = 300)

ggsave(filename = file.path("output", "plot", "c147_feedstock.pdf"),
       plot = plot_commodity_feedstock(tab_land_use, "c147"),
       width = 10, height = 6, dpi = 300)

ggsave(filename = file.path("output", "plot", "c149_feedstock.pdf"),
       plot = plot_commodity_feedstock(tab_land_use, "c149"),
       width = 10, height = 6, dpi = 300)






######################################################################################################################
############################## READ RESULTS DATA #######################
######################################################################################################################

# Match years 2012-2022
files <- list.files(
  "output",
  pattern    = "^FABIO_feedstock_(201[2-9]|202[0-2])_.*\\.csv$",
  full.names = TRUE
)
# Read and combine into one data.table
dt <- rbindlist(lapply(files, fread))
dt <- left_join(dt, regions %>% select(country_consumer = iso3c, continent_target = continent),
                by = "country_consumer") %>%
  mutate(continent_target = if_else(country_consumer == "EU27", "EU", continent_target))
dt <- dt %>%
  group_by(continent_target, continent_origin, commodity, year, feedstock, indicator) %>%
  summarize(value = sum(value),
            .groups = "drop") %>%
  setDT()

dt_heatmap <- dt %>%
  group_by(continent_origin, continent_target, year, indicator) %>%
  summarize(value = sum(value),
            .groups = "drop") %>%
  setDT()
dt_heatmap[, value := value / 1e3]

# --- Smoothed endpoints: 3-year averages --------------------------------
years_base <- 2012:2014
years_curr <- 2020:2022

dt_heatmap_base <- dt_heatmap[year %in% years_base,
                              .(value = mean(value)),
                              by = .(continent_origin, continent_target, indicator)]
dt_heatmap_curr <- dt_heatmap[year %in% years_curr,
                              .(value = mean(value)),
                              by = .(continent_origin, continent_target, indicator)]

label_base <- paste0(min(years_base), "-", max(years_base))
label_curr <- paste0(min(years_curr), "-", max(years_curr))

dt_heatmap_base[, period := label_base]
dt_heatmap_curr[, period := label_curr]

dt_both   <- rbind(dt_heatmap_base, dt_heatmap_curr)
fill_lims <- range(dt_both$value[dt_both$value > 0])

# --- Order continents by base-period self-to-self flow (descending) -----
diag_base <- dt_heatmap_base[continent_origin == continent_target,
                             .(diag_val = sum(value)),
                             by = continent_origin][order(-diag_val)]
cont_order <- diag_base$continent_origin

# === Marginal sums =======================================================
rs <- dt_both[, .(value = sum(value)), by = .(continent_origin, period)]
rs[, continent_target := "Total"]

cs <- dt_both[, .(value = sum(value)), by = .(continent_target, period)]
cs[, continent_origin := "Total"]

gt <- dt_both[, .(value = sum(value)), by = period]
gt[, `:=`(continent_origin = "Total", continent_target = "Total")]

# Strip any pre-existing factors so we control levels uniformly downstream
to_char <- function(d) {
  d[, continent_origin := as.character(continent_origin)]
  d[, continent_target := as.character(continent_target)]
  d[, period           := as.character(period)]
  invisible(d)
}
to_char(dt_both); to_char(rs); to_char(cs); to_char(gt)

# Axis orderings — these will be enforced via scale limits, not factor levels
target_levels <- c(cont_order,    "Total")          # left -> right
origin_levels <- c("Total", rev(cont_order))        # bottom -> top
period_levels <- c(label_base, label_curr)

# === Plot ================================================================
p_combined <- ggplot(dt_both,
                     aes(continent_target, continent_origin, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value)), size = 3) +
  geom_text(data = rs, aes(x = continent_target, y = continent_origin, label = round(value)),
            inherit.aes = FALSE, size = 3, fontface = "bold") +    # right column
  geom_text(data = cs, aes(x = continent_target, y = continent_origin, label = round(value)),
            inherit.aes = FALSE, size = 3, fontface = "bold") +    # bottom row
  geom_text(data = gt, aes(x = continent_target, y = continent_origin, label = round(value)),
            inherit.aes = FALSE, size = 3, fontface = "bold") +    # bottom-right
  scale_fill_viridis_c(
    trans  = "log1p",
    breaks = c(0, 10, 100, 1000, 10000),
    labels = scales::comma
  ) +
  scale_x_discrete(limits = target_levels, position = "top") +     # x at TOP, "Total" at right
  scale_y_discrete(limits = origin_levels) +                        # y forced: "Total" at BOTTOM
  facet_wrap(~ period, axes = "all") +
  labs(x     = NULL,                                
       y     = "Impacted region",
       title = "Biofuel consumer region",        
       fill  = "Land use (1000 ha, annual mean)") +
  theme_minimal() +
  theme(plot.title       = element_text(hjust = 0.5, size = 10,
                                        margin = margin(b = 18)),
        axis.title.y     = element_text(margin = margin(r = 10), size = 10),
        legend.position  = "bottom",
        legend.key.width = unit(2, "cm"),
        panel.spacing    = unit(1, "cm"),
        strip.placement  = "outside",
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_colorbar(direction       = "horizontal",
                               title.position  = "bottom",
                               title.hjust     = 0.5,
                               title.theme     = element_text(size = 9)))

ggsave(filename = file.path("output", "plot",
                            paste0(label_base, "_vs_", label_curr,
                                   "_induced_LU_flows_continent.pdf")),
       plot   = p_combined,
       width  = 10, height = 6, dpi = 300)


######################################################################################################################
############################## STRUCTURAL DECOMPOSITION ANALYSIS #######################
######################################################################################################################

## Loading data

SDA_chain <- fread("output/FABIO_SDA_chained_2012_2022_land_harv_value_c146-c147-c149.csv")
SDA_chain_drivers <- fread("output/FABIO_SDA_chained_2012_2022_land_harv_value_c146-c147-c149_drivers.csv")
SDA_smooth <- fread("output/FABIO_SDA_smoothed_2012-2014_vs_2020-2022_land_harv_value_c146-c147-c149.csv")
SDA_smooth_drivers <- fread("output/FABIO_SDA_smoothed_2012-2014_vs_2020-2022_land_harv_value_c146-c147-c149_drivers.csv")


######################################################################################################################
#1. Plotting drivers by continent
######################################################################################################################

# --- Aggregate by continent ------------------------------------------------
plot_dt <- SDA_smooth[
  effect %in% c("intensity", "feedstock_mix", "sourcing",
                "scale", "composition", "origin")
][regions[, .(iso3c, continent)],
  on = c(country_consumer = "iso3c"),
  nomatch = NULL
][, .(value = sum(value, na.rm = TRUE)),
  by = .(continent, effect)]

# Order continents by total Δ footprint, descending
continent_order <- plot_dt[, .(total = sum(value, na.rm = TRUE)),
                           by = continent][order(-total), continent]

plot_dt[, continent := factor(continent, levels = continent_order)]
plot_dt[, effect    := factor(effect,
                              levels = c("intensity", "feedstock_mix", "sourcing",
                                         "scale", "composition", "origin"))]

# --- Plot ------------------------------------------------------------------
plot_sda_continent <- ggplot(plot_dt, aes(x = continent, y = value, fill = effect)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title    = "SDA effects, 2012-2014 average versus 2020–2022 average — by continent",
    subtitle = "Continents ordered left-to-right by total change",
    x        = NULL,
    y        = "Contribution to Δ footprint",
    fill     = "Effect"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x        = element_text(angle = 30, hjust = 1),
    panel.grid.major.x = element_blank(),
    legend.position    = "bottom"
  )

ggsave(filename = file.path("output", "plot", "2012_2022_SDA_continent.pdf"),
       device = cairo_pdf,
       plot   = plot_sda_continent,
       width  = 10, height = 6, dpi = 300)




######################################################################################################################
#2. Plotting SDA time series for top countries
######################################################################################################################

SDA_cumul <- SDA_chain[
  effect %in% c("intensity", "feedstock_mix", "sourcing",
                "scale", "composition", "origin", "delta"),
  .(value = sum(value)),
  by = .(country_consumer, effect)
]

# --- Top 9 countries by signed cumulative delta ---------------------------
top9 <- SDA_cumul[
  effect == "delta"
][order(-value)][1:9]

country_order <- top9$country_consumer

traj <- SDA_chain[
  country_consumer %in% country_order &
    effect %in% c("intensity", "feedstock_mix", "sourcing",
                  "scale", "composition", "origin")
]
year_levels <- sort(unique(traj$year_current))
traj[, country_consumer := factor(country_consumer, levels = country_order)]
traj[, effect := factor(effect, levels = c("intensity", "feedstock_mix", "sourcing",
                                           "scale", "composition", "origin"))]
traj[, year_current := factor(year_current, levels = year_levels)]

# Running cumulative Δ per country for the overlay line
cum_delta <- SDA_chain[
  country_consumer %in% country_order & effect == "delta"
][order(country_consumer, year_current),
  .(year_current, value, cum_value = cumsum(value)),
  by = country_consumer]
cum_delta[, country_consumer := factor(country_consumer, levels = country_order)]
cum_delta[, year_current := factor(year_current, levels = year_levels)]

SDA_chain_ts <- ggplot(traj, aes(x = year_current, y = value, fill = effect)) +
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
    title    = "SDA effects, annual chain 2012-2022 — top 9 countries",
    subtitle = "Stacked annual contributions per year-pair; black line = running cumulative Δ",
    x        = "Year",
    y        = "Annual contribution to Δ footprint",
    fill     = "Effect"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(filename = file.path("output", "plot", "2012_2022_SDA_chain_ts.pdf"),
       device = cairo_pdf,
       plot = SDA_chain_ts,
       width = 12, height = 12, dpi = 300)


######################################################################################################################
#3. Plotting SDA by effect by continent
######################################################################################################################

# --- 1. Add continent labels via update-on-join ----------------------------
plot_base <- copy(SDA_smooth_drivers)
plot_base[regions, on = c(country_consumer = "iso3c"),
          continent_consumer := i.continent]
plot_base[regions, on = c(country_origin   = "iso3c"),
          continent_origin   := i.continent]

# --- 2. Helper: aggregate + plot one effect --------------------------------
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
  # Other goes to the HEAD so it ends up at the top of the stack
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
      title    = paste0("SDA driver decomposition — ", eff),
      subtitle = paste0("Stacked by ", group_var, ", per consumer continent"),
      x        = NULL,
      y        = "Contribution to Δ footprint",
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

# --- Crop palette: biodiesel (warm) vs bioethanol (cool), Other gray -------
item_palette <- c(
  # --- Biodiesel-related (warm: amber → red → brown) ---------------------
  "Sunflower seed"       = "#F59E0B",  # golden / sunflower yellow
  "Rape and Mustardseed" = "#B7791F",  # mustard ochre
  "Oil, palm fruit"      = "#DC2626",  # palm-fruit red
  "Soyabeans"            = "#B45309",  # burnt amber
  "Fodder crops"         = "#78350F",  # dark brown (hay / animal-fat link)
  
  # --- Bioethanol-related (cool: lime → green → teal) --------------------
  "Wheat and products"   = "#A3E635",  # light lime
  "Maize and products"   = "#65A30D",  # lime green
  "Sorghum and products" = "#166534",  # forest green
  "Rice and products"    = "#059669",  # emerald
  "Sugar cane"           = "#0D9488",  # teal
  "Cassava and products" = "#155E75",  # dark teal / cyan
  
  # --- Catch-all ---------------------------------------------------------
  "Other"                = "#9CA3AF"   # neutral gray
)

# --- Global fill orderings -------------------------------------------------

# item_origin: union of top-10 in intensity + feedstock_mix (the τ-side effects)
top_items <- unique(unlist(lapply(c("intensity", "feedstock_mix"), function(eff) {
  plot_base[effect == eff,
            .(tot = sum(abs(value), na.rm = TRUE)),
            by = item_origin][order(-tot)][seq_len(min(10, .N)), item_origin]
})))

# Gradient-based item ordering (gray → cool → warm)
item_order <- c(
  "Other",                  # gray, top of stack
  # Bioethanol — light to dark cool
  "Wheat and products",
  "Maize and products",
  "Sorghum and products",
  "Rice and products",
  "Sugar cane",
  "Cassava and products",
  # Biodiesel — dark to light warm
  "Fodder crops",
  "Soyabeans",
  "Oil, palm fruit",
  "Rape and Mustardseed",
  "Sunflower seed"
)

# commodity ordering: across scale + composition
commodity_order <- plot_base[
  effect %in% c("scale", "composition"),
  .(tot = sum(abs(value), na.rm = TRUE)),
  by = commodity
][order(-tot), commodity]

# Continent (consumer) ordering: by sum of POSITIVE contributions across all six effects
cont_order <- plot_base[
  effect %in% c("intensity", "feedstock_mix", "sourcing",
                "scale", "composition", "origin"),
  .(total = sum(pmax(value, 0), na.rm = TRUE)),
  by = continent_consumer
][order(-total), continent_consumer]

# --- Specs: six effects ----------------------------------------------------
# - intensity, feedstock_mix → grouped by item_origin (the τ-side: "which feedstocks?")
# - sourcing, origin         → grouped by continent_origin (the geographic-shift dimension)
# - scale, composition       → grouped by commodity (biofuel commodity dimension)
specs <- list(
  intensity     = list(group_var = "item_origin",      top_n = 10,
                       palette = item_palette, fill_order = item_order),
  feedstock_mix = list(group_var = "item_origin",      top_n = 10,
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

# --- Build all six plots ---------------------------------------------------
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
for (eff in names(plots)) {
  ggsave(
    filename = file.path("output", "plot",
                         sprintf("2012_2022_SDA_drivers_%s.pdf", eff)),
    device = cairo_pdf,
    plot   = plots[[eff]],
    width  = 11, height = 6.5, dpi = 300
  )
}



######################################################################################################################
############################## COMPLEMENTARY, TEMPORARY CHECKS #######################
######################################################################################################################

str(dt_totalreq)


source("R/00_system_variables.R")

setwd("/home/mmondolfo/fabio_bfp/")
# Read labels ------------------------------------------------------------------
input_path <- "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/"
L22 <- readRDS(paste0(input_path,"2022_", "L_", "value", ".rds"))
L12 <- readRDS(paste0(input_path,"2012_", "L_", "value", ".rds"))
input_path <- "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/"
io <- fread(paste0(input_path,"io_labels.csv"))
io_names <- io[, paste0(iso3c, "_", comm_code)]

library(Matrix)
colnames(L22) <- rownames(L22) <- colnames(L12) <- rownames(L12) <- io_names

contrast_country_bfp <- function(L_late, L_early,
                                 iso3,
                                 comm_codes = c(146, 147, 149),
                                 threshold  = 0.01,
                                 fill       = 0) {
  
  pattern <- paste0("^", iso3, "_c(", paste(comm_codes, collapse = "|"), ")$")
  
  extract <- function(L) {
    col_idx <- grep(pattern, colnames(L))
    if (!length(col_idx)) stop("No columns match pattern: ", pattern)
    sub  <- L[, col_idx, drop = FALSE]
    keep <- sub@x >= threshold
    rows <- sort.int(unique(sub@i[keep])) + 1L
    as.matrix(sub[rows, , drop = FALSE])
  }
  
  agg <- function(M) {
    suffix <- substring(rownames(M), nchar(rownames(M)) - 4L)
    rowsum(M, group = suffix)
  }
  
  A <- agg(extract(L_late))
  B <- agg(extract(L_early))
  
  rows <- union(rownames(A), rownames(B))
  align <- function(M) {
    out <- matrix(fill, length(rows), ncol(M),
                  dimnames = list(rows, colnames(M)))
    out[rownames(M), ] <- M
    out
  }
  
  align(A) - align(B)
}

# usage
L_diff_USA <- contrast_country_bfp(L22, L12, iso3 = "USA")
L_diff_BRA <- contrast_country_bfp(L22, L12, iso3 = "BRA")
L_diff_ARG <- contrast_country_bfp(L22, L12, iso3 = "ARG")
L_diff_DEU <- contrast_country_bfp(L22, L12, iso3 = "DEU")









# ============================================================================
# Δ Material footprint vs Technique effect — by continent × origin item
# ============================================================================

# --- A. Add continent_consumer to dt_totalreq -----------------------------
dt_totalreq <- copy(dt_totalreq)
dt_totalreq[regions, on = c(country_consumer = "iso3c"),
            continent_consumer := i.continent]

# --- B. Sum by (continent_consumer, year, item_origin) --------------------
totalreq_yr <- dt_totalreq[
  , .(value = sum(value, na.rm = TRUE)),
  by = .(continent_consumer, year, item_origin)
]

# --- C. Filter to the same top_items used in the SDA plot -----------------
missing <- setdiff(top_items, item_order)
if (length(missing) > 0) {
  warning("top_items not in item_order (no color/level): ",
          paste(missing, collapse = ", "))
}
totalreq_yr <- totalreq_yr[item_origin %in% top_items]

# --- D. Convert to within-continent shares, then contrast late − early ----
totalreq_yr[, period := fifelse(year %in% 2012:2014, "early",
                                fifelse(year %in% 2020:2022, "late", NA_character_))]

period_means <- totalreq_yr[!is.na(period),
                            .(value = mean(value, na.rm = TRUE)),
                            by = .(continent_consumer, item_origin, period)]

period_means[, share := value / sum(value, na.rm = TRUE),
             by = .(continent_consumer, period)]

delta_material <- dcast(period_means,
                        continent_consumer + item_origin ~ period,
                        value.var = "share", fill = 0)
delta_material[, value := late - early]
delta_material <- delta_material[, .(continent_consumer, item_origin, value)]

# --- E. Technique effect from plot_base, restricted to the same items -----
technique_effect <- plot_base[effect == "technique" & item_origin %in% top_items,
                              .(value = sum(value, na.rm = TRUE)),
                              by = .(continent_consumer, item_origin)]

# --- F. Stack the two metrics into one long table -------------------------
combined <- rbindlist(list(
  technique_effect[, .(continent_consumer, item_origin, value,
                       type = "technique")],
  delta_material  [, .(continent_consumer, item_origin, value,
                       type = "delta_material")]
))

combined[, item_origin        := factor(item_origin, levels = item_order)]
combined[, continent_consumer := factor(continent_consumer, levels = cont_order)]
combined[, type               := factor(type,
                                        levels = c("technique", "delta_material"))]

# --- G. Dual-axis scale factor --------------------------------------------
# Match the tallest stacked bar on each side so the two side-by-side bars
# look comparable; each metric keeps its own y-axis label.
visual_height <- function(x)
  sum(pmax(x, 0), na.rm = TRUE) - sum(pmin(x, 0), na.rm = TRUE)

range_tech  <- combined[type == "technique",
                        visual_height(value), by = continent_consumer][, max(V1)]
range_delta <- combined[type == "delta_material",
                        visual_height(value), by = continent_consumer][, max(V1)]
scale_factor <- range_tech / range_delta

combined[, value_scaled := fifelse(type == "delta_material",
                                   value * scale_factor, value)]

# --- H. Numeric x positions: dodge within continent, stack within bar -----
combined[, cont_idx := as.numeric(continent_consumer)]
combined[, x_pos    := cont_idx + fifelse(type == "technique", -0.22, 0.22)]

# --- I. Plot --------------------------------------------------------------
p_compare <- ggplot(combined,
                    aes(x = x_pos, y = value_scaled, fill = item_origin)) +
  geom_col(width = 0.4, position = position_stack(reverse = FALSE)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_x_continuous(
    breaks = seq_along(levels(combined$continent_consumer)),
    labels = levels(combined$continent_consumer),
    expand = expansion(add = 0.5)
  ) +
  scale_y_continuous(
    name     = "Technique effect (SDA, Δ land-harvested)",
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "Δ feedstock share  (mean 2020–2022 − mean 2012–2014)")
  ) +
  scale_fill_manual(values = item_palette, drop = FALSE) +
  labs(
    title    = "Technique effect vs Δ material footprint",
    subtitle = "Per continent: left bar = technique effect (SDA), right bar = Δ material",
    x        = NULL,
    fill     = "Origin item"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x        = element_text(angle = 30, hjust = 1),
    panel.grid.major.x = element_blank(),
    legend.position    = "right"
  )

# --- J. Save --------------------------------------------------------------
ggsave(
  filename = file.path("output", "plot",
                       "2012_2022_technique_vs_delta_material.pdf"),
  device   = cairo_pdf,
  plot     = p_compare,
  width    = 12, height = 6.5, dpi = 300
)


SDA_chain_drivers[effect == "feedstock_mix" 
                 & item_origin == "Rice and products" 
                 & country_consumer == "CHN",
                 .(cumul = sum(value)), by = country_consumer]

# Smoothed: same query
SDA_smooth_drivers[effect == "feedstock_mix" 
                   & item_origin == "Rice and products" 
                   & country_consumer == "CHN",
                   .(cumul = sum(value)), by = country_consumer]

E12 <- E_bar[["2012"]]
E15 <- E_bar[["2015"]]
E18 <- E_bar[["2018"]]
E21 <- E_bar[["2021"]]
E22 <- E_bar[["2022"]]
print(E12[grepl("gwp_total|^land_crop|^gwp_co2_deforestation", rownames(E12)), grepl("IDN_c028", colnames(E12))])
print(E15[grepl("gwp_total|^land_crop|^gwp_co2_deforestation", rownames(E18)), grepl("IDN_c028", colnames(E15))])
print(E18[grepl("gwp_total|^land_crop|^gwp_co2_deforestation", rownames(E18)), grepl("IDN_c028", colnames(E18))])
print(E21[grepl("gwp_total|^land_crop|^gwp_co2_deforestation", rownames(E21)), grepl("IDN_c028", colnames(E21))])
print(E22[grepl("gwp_total|^land_crop|^land_grass|^gwp_co2_deforestation", rownames(E22)), grepl("IDN_c028", colnames(E22))])

print(E22[grepl("ibif", rownames(E22)), grepl("IDN_c028", colnames(E22))])
print(E22[, "IDN_c028"])

print(E22[, "IDN_c028"])
subset(dt_tradeFeed, 
             country_consumer == "IDN" & 
             country_origin == "IDN" &
               grepl("ibif", indicator) &
               year == 2022))
write.csv(
  subset(
    dt_tradeFeed,
    country_consumer == "IDN" & 
      country_origin == "IDN" &
      grepl("ibif", indicator) &
      year == 2022
  ),
  "filtered_tradefeed.csv",
  row.names = FALSE
)
      