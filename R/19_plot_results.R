# 19 - Plot results

setwd("/home/mmondolfo/fabio_bfp/")

# Setup ------------------------------------------------------------------------
library(data.table)
library(Matrix)
library(tidyverse)
library(ggplot2)
library(scales)
library(paletteer)


######################################################################################################################
############################## READ RESULTS DATA #######################
######################################################################################################################

# Match years 2012-2022
files <- list.files(
  "output",
  pattern    = "^FABIO_tradeFeed_(201[2-9]|202[0-2])_.*\\.csv$",
  full.names = TRUE
)

# Read and combine into one data.table
dt <- rbindlist(lapply(files, fread))

items_full_bcp <- read_csv("inst/items_full_bcp.csv")
items_full_bcp <- as.data.table(items_full_bcp)


######################################################################################################################
############################## FUNCTIONS TO PLOT RESULTS #######################
######################################################################################################################

tab_land_use <- subset(dt, indicator == "land_harv")


######################################################################################################################
#1. Plot countries' impact balance (exported, self, imported)
######################################################################################################################


plot_balance <- function(data, year_select, top_pos_n = 20, top_exp_n = 10) {
  dt <- as.data.table(data)
  
  # --- filter year
  dt_year <- dt[year == year_select]
  
  # --- aggregate
  origin_summary <- dt_year[
    , .(
      self    = sum(value[flow_type == "self"],  na.rm = TRUE),
      exports = sum(value[flow_type == "trade"], na.rm = TRUE)
    ),
    by = country_origin
  ]
  
  import_summary <- dt_year[
    flow_type == "trade",
    .(imports = sum(value, na.rm = TRUE)),
    by = country_consumer
  ]
  
  # --- merge
  final <- merge(
    origin_summary,
    import_summary,
    by.x = "country_origin",
    by.y = "country_consumer",
    all = TRUE
  )
  
  final[is.na(final)] <- 0
  

  # --- select top & bottom
  # --- compute positive side
  final[, pos := self + imports]
  
  # --- top N by self + imports
  setorder(final, -pos)
  top_pos <- final[1:top_pos_n]
  
  # --- exclude already selected
  remaining <- final[!country_origin %in% top_pos$country_origin]
  
  # --- top N exports from remaining
  setorder(remaining, -exports)
  top_exp <- remaining[1:top_exp_n]
  top_exp <- top_exp[order(exports)] 
  
  # --- combine
  plot_data <- rbind(top_pos, top_exp)
  
  # --- ordering
  plot_data[, country_origin := factor(
    country_origin,
    levels = c(
      top_pos$country_origin,
      top_exp$country_origin
    )
  )]
  
  # --- reshape
  plot_long <- melt(
    plot_data,
    id.vars = c("country_origin"),
    measure.vars = c("imports", "self", "exports"),
    variable.name = "type",
    value.name = "value"
  )
  
  plot_long[type == "exports", value := -value]
  plot_long[, type := factor(type, levels = c("exports", "imports", "self"))]  
  # --- plot
  p <- ggplot(plot_long, aes(x = country_origin, y = value, fill = type)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c(
      "imports" = "#2196F3",
      "self" = "#4CAF50",
      "exports" = "#F44336"
    )) +
    scale_y_continuous(
      labels = label_number()
    ) +
    labs(
      x = "Country",
      y = "Land use",
      fill = "Flow type",
      title = paste("Net Land Use by country (",year_select,")")
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(p)
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
    , .(value = sum(value, na.rm = TRUE)),
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
    scale_y_continuous(labels = label_number()) +
    labs(
      x = "Year",
      y = "Land use (ha)",
      fill = "Feedstock",
      title = paste("Land use by feedstock for", commodity_name)
    ) +
    theme_minimal()
  
  return(p)
}



######################################################################################################################
############################## PLOT RESULTS #######################
######################################################################################################################

######################################################################################################################
#1. Plot countries' impact balance (exported, self, imported)
######################################################################################################################

plot_balance(tab_land_use, 2012)
plot_balance(tab_land_use, 2022)


######################################################################################################################
#2. Plot impacts by commodity, by feedstock over time. 
######################################################################################################################

plot_commodity_feedstock(tab_land_use, "c146")
plot_commodity_feedstock(tab_land_use, "c147")
plot_commodity_feedstock(tab_land_use, "c149")




######################################################################################################################
############################## SAVE PLOTS #######################
######################################################################################################################

dir.create(file.path("output", "plot"), recursive = TRUE, showWarnings = FALSE)

ggsave(filename = file.path("output", "plot", "2012_netLU.png"),
       plot = plot_balance(tab_land_use, 2012),
       width = 10, height = 6, dpi = 300)

ggsave(filename = file.path("output", "plot", "2022_netLU.png"),
       plot = plot_balance(tab_land_use, 2022),
       width = 10, height = 6, dpi = 300)

ggsave(filename = file.path("output", "plot", "c146_feedstock.png"),
       plot = plot_commodity_feedstock(tab_land_use, "c146"),
       width = 10, height = 6, dpi = 300)

ggsave(filename = file.path("output", "plot", "c147_feedstock.png"),
       plot = plot_commodity_feedstock(tab_land_use, "c147"),
       width = 10, height = 6, dpi = 300)

ggsave(filename = file.path("output", "plot", "c149_feedstock.png"),
       plot = plot_commodity_feedstock(tab_land_use, "c149"),
       width = 10, height = 6, dpi = 300)