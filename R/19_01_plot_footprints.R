# 19.01 - Plot footprint results

setwd("/home/mmondolfo/fabio_bfp/")

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


######################################################################################################################
############################## Making general vectors #######################
######################################################################################################################

# Colors for biofuels
fuel_colors <- c(
  "Biogasoline"       = "#D4A017",
  "Biodiesel"        = "#5B2C6F",
  "Renewable diesel" = "#9B59B6"
)


continent_palette <- c(
  AFR = "#D55E00",  # vermillion
  ASI = "#E69F00",  # orange
  EU  = "#0072B2",  # blue
  EUR = "#56B4E9",  # sky blue
  LAM = "#009E73",  # green
  NAM = "#CC79A7",  # pink
  OCE = "#F0E442",  # yellow
  ROW = "#000000"   # black
)


# List of commodities by category
bf_set <- c("c146", "c147", "c149", "c150", "c151")
bp_set <- paste0("c", 152:170)

######################################################################################################################
############################## Setting indicator-specific labels #######################
######################################################################################################################

indicator_meta <- data.table(
  indicator    = c("land_harv", "ibif_total",
                   "LCIM_EQ_freshwater", "LCIM_EQ_marine", "LCIM_EQ_terrestrial"),
  scale_factor = c(1e3, 1, 1, 1, 1),
  y_label      = c("Land use (1000 ha)",
                   "Species abundance loss (MSA·km²·yr)",
                   "Freshwater ecotoxicity (PDF·yr)",
                   "Marine ecotoxicity (PDF·yr)",
                   "Terrestrial ecotoxicity (PDF·yr)"),
  short_label  = c("Land use", "IBIF",
                   "Freshwater ecotoxicity",
                   "Marine ecotoxicity",
                   "Terrestrial ecotoxicity")
)

commodity_meta <- data.table(
  item = c("Biogasoline", "Biodiesel", "Renewable diesel"), 
  comm_code = c("c146", "c147", "c149")
)

feedstock_meta <- data.table(
  feedstock = c(
    # Oils & fats (biodiesel / HVO feedstocks)
    "Palm Oil", "Palmkernel Oil", "Coconut Oil",
    "Soyabean Oil", "Rape and Mustard Oil", "Sunflowerseed Oil",
    "Cottonseed Oil", "Maize Germ Oil",
    "Used Cooking Oil", "Fats, Animals, Raw",
    # Sugar / starch (bioethanol feedstocks)
    "Sugar cane", "Sugar beet", "Molasses",
    "Maize and products", "Wheat and products", "Rice and products",
    "Barley and products", "Rye and products", "Triticale",
    "Sorghum and products", "Cassava and products",
    # Residual
    "Other, Waste"
  ),
  category = c(
    rep("Oilcrops",   10),
    rep("Starchy / Sugar crops", 11),
    "Other"
  )
)





######################################################################################################################
############################## HELPER FUNCTION TO EXTRACTS RESULTS FILES FROM SPECIFIC PATTERN #######################
######################################################################################################################

fabio_files <- function(prefix, group = c("BF", "BP", "BF_BP"), dir = "output") {
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
Y_summary <- fread("output/Y_summary_c146_c147_c149.csv")
Y_summary_BP <- fread("output/Y_summary_BP.csv")
Z_summary <- fread("output/Z_summary_c146_c147_c149.csv")

files_tradeFeed_BF <- fabio_files("FABIO_tradeFeed", "BF")
files_tradeFeed_BP <- fabio_files("FABIO_tradeFeed", "BP")
files_feedstock_BF <- fabio_files("FABIO_feedstock", "BF")
files_totalreq_BF  <- fabio_files("FABIO_totalreq",  "BF")
# files_totalreq_BP  <- fabio_files("FABIO_totalreq",  "BP")

dt_tradeFeed    <- rbindlist(lapply(files_tradeFeed_BF, fread))
dt_tradeFeed_BP <- rbindlist(lapply(files_tradeFeed_BP, fread))
dt_feedstock    <- rbindlist(lapply(files_feedstock_BF, fread))
dt_totalreq     <- rbindlist(lapply(files_totalreq_BF,  fread))




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
ggsave(filename = file.path("output", "plot", "Y_global_2012_2022_BF.pdf"),
       plot = Y_global_plot,
       width = 10, height = 6, dpi = 300)

ggsave(filename = file.path("output", "plot", "Y_global_2012_2022_BP.pdf"),
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
  
  # category-aware palette (no greens), ordered for within-group contrast
  pal_by_cat <- list(
    # Cool jewel tones — alternating blues ↔ purples, dark ↔ light
    "Oilcrops"              = c("#08306b",  # deep navy
                                "#c994c7",  # orchid
                                "#2171b5",  # steel blue
                                "#4a1486",  # dark violet
                                "#9ecae1",  # light sky
                                "#807dba",  # medium purple
                                "#08519c",  # royal blue
                                "#d8b4f8",  # lavender (light)
                                "#54278f",  # deep purple
                                "#6baed6",  # sky blue
                                "#3f007d",  # indigo
                                "#bcbddc"), # lavender
    
    # Warm earth / sunset tones — alternating red ↔ orange ↔ brown ↔ gold
    "Starchy / Sugar crops" = c("#67000d",  # deep wine
                                "#fdae6b",  # peach
                                "#cb181d",  # vermillion
                                "#543005",  # espresso
                                "#fb6a4a",  # coral
                                "#bf812d",  # tan
                                "#d94801",  # burnt orange
                                "#fcae91",  # salmon
                                "#8c510a",  # sienna
                                "#fd8d3c",  # tangerine
                                "#b8860b",  # dark goldenrod
                                "#fee5d9"), # peach cream
    
    # Neutrals — stays grey, but with more steps for differentiation
    "Other"                 = c("#525252")
  )
  
  cats <- feed_cat(top_feedstocks)
  
  # fill data-row palette
  pal <- character(length(top_feedstocks))
  for (k in unique(cats)) {
    pos <- which(cats == k)
    if (length(pos) > length(pal_by_cat[[k]]))
      warning("More '", k, "' feedstocks than palette colours; recycling.")
    pal[pos] <- rep_len(pal_by_cat[[k]], length(pos))
  }
  names(pal) <- top_feedstocks
  
  # legend breaks/labels: insert header pseudo-entries above each group
  legend_breaks <- character()
  legend_labels <- character()
  header_pal    <- character()
  
  for (k in unique(cats)) {
    hdr <- paste0("__hdr_", k)
    fs  <- top_feedstocks[cats == k]
    legend_breaks <- c(legend_breaks, hdr, fs)
    legend_labels <- c(legend_labels, paste0("<b>", k, "</b>"), fs)
    header_pal[hdr] <- "transparent"
  }
  if (include_other) {
    legend_breaks <- c(legend_breaks, "Other")
    legend_labels <- c(legend_labels, "Other")
  }
  
  pal_full <- c(header_pal, pal)
  if (include_other) pal_full <- c(pal_full, Other = "grey70")
  
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
                                    other_color     = "grey70",
                                    palette_fn      = NULL,
                                    override_colors = c(
                                      "Fats, Animals, Raw" = "#762A83",
                                      "Maize Germ Oil"     = "#F1A340"
                                    )) {
  
  dt <- as.data.table(data)
  if (!is.null(commodities)) dt <- dt[commodity %in% commodities]
  if (!is.null(indicators))  dt <- dt[indicator %in% indicators]
  
  dt[grepl("^Animal or vegetable fats and oils", feedstock),
     feedstock := "Used Cooking Oil"]
  
  top_by_combo <- dt[, {
    rk <- .SD[, .(total = sum(value, na.rm = TRUE)), by = feedstock][order(-total)]
    list(feedstock = head(rk$feedstock, top_n_feedstock))
  }, by = .(commodity, indicator)]
  
  feedstocks <- dt[feedstock %in% unique(top_by_combo$feedstock),
                   .(total = sum(value, na.rm = TRUE)),
                   by = feedstock][order(-total), feedstock]
  
  n <- length(feedstocks)
  if (is.null(palette_fn)) {
    palette_fn <- function(k) {
      pool <- unique(c(
        RColorBrewer::brewer.pal(8, "Dark2"),
        setdiff(RColorBrewer::brewer.pal(9, "Set1"), c("#ffff33", "#999999")),
        RColorBrewer::brewer.pal(12, "Paired")
      ))
      if (k <= length(pool)) pool[seq_len(k)]
      else scales::hue_pal(l = 55, c = 90)(k)
    }
  }
  
  base <- setNames(palette_fn(n), feedstocks)
  
  # Pin specific feedstocks to chosen hues. Default forces
  # "Fats, Animals, Raw" (deep purple) and "Maize Germ Oil" (warm orange)
  # to clearly different colours.
  if (length(override_colors)) {
    ovr <- intersect(names(override_colors), names(base))
    base[ovr] <- override_colors[ovr]
  }
  
  c(base, Other = other_color)
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
############################## PLOT RESULTS #######################
######################################################################################################################

######################################################################################################################
#0. Plot descriptive statistics
######################################################################################################################

# usage
p <- plot_feedstock_desc(Z_per_year, c("c146", "c147", "c149"), c("EU","ASI","NAM","LAM"))

ggsave(
filename = file.path("output", "plot", "desc_embodied_feedstock_BF_ASI-EU-NAM.pdf"),
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

p_ibif <- plot_commodity_feedstock_grid(
  dt_tradeFeed,
  commodities = c("c146", "c147", "c149"),
  indicators  = "ibif_total",
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


######################################################################################################################
############################## SAVE PLOTS #######################
######################################################################################################################

dir.create(file.path("output", "plot"), recursive = TRUE, showWarnings = FALSE)

ggsave(filename = file.path("output", "plot", "balance_LU_2012_2022.pdf"),
       plot = plot_netLU_2012_2022,
       width = 10, height = 10, dpi = 300)

ggsave(filename = file.path("output", "plot", "balance_ibif_2012_2022.pdf"),
       plot = plot_ibif_2012_2022,
       width = 10, height = 10, dpi = 300)

ggsave(filename = file.path("output", "plot", "balance_lcim_terrestrial_2012_2022.pdf"),
       plot = plot_lcim_terrestrial_2012_2022,
       width = 10, height = 10, dpi = 300)

# ggsave(filename = file.path("output", "plot", "feedstock_impact_by_indicator_grid.pdf"), 
#        plot = p_grid_full,
#        device = cairo_pdf,
#        width = 19, height = 12 , dpi = 300)

ggsave(filename = file.path("output", "plot", "feedstock_impact_ibif_lu.pdf"), 
       plot = p_grid,
       device = cairo_pdf,
       width = 13, height = 12 , dpi = 300)

ggsave(filename = file.path("output", "plot", "feedstock_impact_ibif.pdf"), 
       plot = p_ibif,
       device = cairo_pdf,
       width = 7, height = 12 , dpi = 300)



# ggsave(filename = file.path("output", "plot", "c146_feedstock.pdf"),
#        plot = plot_commodity_feedstock(tab_land_use, "c146"),
#        width = 10, height = 6, dpi = 300)
# 
# ggsave(filename = file.path("output", "plot", "c147_feedstock.pdf"),
#        plot = plot_commodity_feedstock(tab_land_use, "c147"),
#        width = 10, height = 6, dpi = 300)
# 
# ggsave(filename = file.path("output", "plot", "c149_feedstock.pdf"),
#        plot = plot_commodity_feedstock(tab_land_use, "c149"),
#        width = 10, height = 6, dpi = 300)




######################################################################################################################
############################## READ RESULTS DATA #######################
######################################################################################################################

plot_continent_heatmap <- function(dt_feedstock,
                                   years_base,
                                   years_curr,
                                   indicator,
                                   save_dir = file.path("output", "plot"),
                                   save     = TRUE,
                                   trans    = NULL,   # override default
                                   breaks   = NULL,   # override default
                                   limits   = NULL) { # override default
  
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
  # Add a new indicator here = supported everywhere downstream.
  cfg_all <- list(
    ibif_total = list(
      div          = 1e3,
      legend_title = "Means species abundance loss (1000 MSA·km²·yr, annual mean)",
      file_suffix  = "IBIF_flows_continent",
      trans        = "log1p"
    ),
    LCIM_EQ_terrestrial = list(
      div          = 1e-2,
      legend_title = "Terrestrial ecosystem damage (PDF·yr, annual mean)",
      file_suffix  = "LCIM_terrestrial_flows_continent",
      trans        = "log1p"
    )
  )
  
  if (!indicator %in% names(cfg_all)) {
    stop("Unknown indicator '", indicator, "'. Known: ",
         paste(names(cfg_all), collapse = ", "))
  }
  cfg <- cfg_all[[indicator]]
  if (is.null(trans)) trans <- cfg$trans
  
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
  
  # --- Auto-derive limits & breaks from THIS indicator's range -------------
  pos_vals <- dt_both$value[dt_both$value > 0 & is.finite(dt_both$value)]
  val_max  <- max(pos_vals, na.rm = TRUE)
  
  if (is.null(limits)) limits <- c(0, val_max)
  if (is.null(breaks)) {
    breaks <- if (grepl("^log", trans)) {
      high_pow <- ceiling(log10(val_max))
      low_pow  <- max(0, high_pow - 4)
      c(0, 10^(low_pow:high_pow))
    } else {
      ggplot2::waiver()   # let scale_fill_viridis_c pick linear breaks
    }
  }
  
  # --- Continent ordering by base self-flow --------------------------------
  diag_base <- dt_heatmap_base[continent_origin == continent_target,
                               .(diag_val = sum(value)),
                               by = continent_origin][order(-diag_val)]
  cont_order <- diag_base$continent_origin
  
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
      trans  = trans,
      breaks = breaks,
      limits = limits,
      labels = scales::comma
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
    fname <- paste0(label_base, "_vs_", label_curr, "_", cfg$file_suffix, ".pdf")
    ggsave(filename = file.path(save_dir, fname),
           plot = p, width = 10, height = 6, dpi = 300)
  }
  
  p
}


plot_continent_heatmap(dt_feedstock, 2012:2014, 2020:2022, "ibif_total")
plot_continent_heatmap(dt_feedstock, 2012:2014, 2020:2022, "LCIM_EQ_terrestrial")





######################################################################################################################
############################## COMPLEMENTARY, TEMPORARY CHECKS #######################
######################################################################################################################
# 
# str(dt_totalreq)
# 
# 
# source("R/00_system_variables.R")
# 
# setwd("/home/mmondolfo/fabio_bfp/")
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





# # ============================================================================
# # Δ Material footprint vs Technique effect — by continent × origin item
# # ============================================================================
# 
# # --- A. Add continent_consumer to dt_totalreq -----------------------------
# dt_totalreq <- copy(dt_totalreq)
# dt_totalreq[regions, on = c(country_consumer = "iso3c"),
#             continent_consumer := i.continent]
# 
# # --- B. Sum by (continent_consumer, year, item_origin) --------------------
# totalreq_yr <- dt_totalreq[
#   , .(value = sum(value, na.rm = TRUE)),
#   by = .(continent_consumer, year, item_origin)
# ]
# 
# # --- C. Filter to the same top_items used in the SDA plot -----------------
# missing <- setdiff(top_items, item_order)
# if (length(missing) > 0) {
#   warning("top_items not in item_order (no color/level): ",
#           paste(missing, collapse = ", "))
# }
# totalreq_yr <- totalreq_yr[item_origin %in% top_items]
# 
# # --- D. Convert to within-continent shares, then contrast late − early ----
# totalreq_yr[, period := fifelse(year %in% 2012:2014, "early",
#                                 fifelse(year %in% 2020:2022, "late", NA_character_))]
# 
# period_means <- totalreq_yr[!is.na(period),
#                             .(value = mean(value, na.rm = TRUE)),
#                             by = .(continent_consumer, item_origin, period)]
# 
# period_means[, share := value / sum(value, na.rm = TRUE),
#              by = .(continent_consumer, period)]
# 
# delta_material <- dcast(period_means,
#                         continent_consumer + item_origin ~ period,
#                         value.var = "share", fill = 0)
# delta_material[, value := late - early]
# delta_material <- delta_material[, .(continent_consumer, item_origin, value)]
# 
# # --- E. Technique effect from plot_base, restricted to the same items -----
# technique_effect <- plot_base[effect == "technique" & item_origin %in% top_items,
#                               .(value = sum(value, na.rm = TRUE)),
#                               by = .(continent_consumer, item_origin)]
# 
# # --- F. Stack the two metrics into one long table -------------------------
# combined <- rbindlist(list(
#   technique_effect[, .(continent_consumer, item_origin, value,
#                        type = "technique")],
#   delta_material  [, .(continent_consumer, item_origin, value,
#                        type = "delta_material")]
# ))
# 
# combined[, item_origin        := factor(item_origin, levels = item_order)]
# combined[, continent_consumer := factor(continent_consumer, levels = cont_order)]
# combined[, type               := factor(type,
#                                         levels = c("technique", "delta_material"))]
# 
# # --- G. Dual-axis scale factor --------------------------------------------
# # Match the tallest stacked bar on each side so the two side-by-side bars
# # look comparable; each metric keeps its own y-axis label.
# visual_height <- function(x)
#   sum(pmax(x, 0), na.rm = TRUE) - sum(pmin(x, 0), na.rm = TRUE)
# 
# range_tech  <- combined[type == "technique",
#                         visual_height(value), by = continent_consumer][, max(V1)]
# range_delta <- combined[type == "delta_material",
#                         visual_height(value), by = continent_consumer][, max(V1)]
# scale_factor <- range_tech / range_delta
# 
# combined[, value_scaled := fifelse(type == "delta_material",
#                                    value * scale_factor, value)]
# 
# # --- H. Numeric x positions: dodge within continent, stack within bar -----
# combined[, cont_idx := as.numeric(continent_consumer)]
# combined[, x_pos    := cont_idx + fifelse(type == "technique", -0.22, 0.22)]
# 
# # --- I. Plot --------------------------------------------------------------
# p_compare <- ggplot(combined,
#                     aes(x = x_pos, y = value_scaled, fill = item_origin)) +
#   geom_col(width = 0.4, position = position_stack(reverse = FALSE)) +
#   geom_hline(yintercept = 0, linewidth = 0.3) +
#   scale_x_continuous(
#     breaks = seq_along(levels(combined$continent_consumer)),
#     labels = levels(combined$continent_consumer),
#     expand = expansion(add = 0.5)
#   ) +
#   scale_y_continuous(
#     name     = "Technique effect (SDA, Δ land-harvested)",
#     sec.axis = sec_axis(~ . / scale_factor,
#                         name = "Δ feedstock share  (mean 2020–2022 − mean 2012–2014)")
#   ) +
#   scale_fill_manual(values = item_palette, drop = FALSE) +
#   labs(
#     title    = "Technique effect vs Δ material footprint",
#     subtitle = "Per continent: left bar = technique effect (SDA), right bar = Δ material",
#     x        = NULL,
#     fill     = "Origin item"
#   ) +
#   theme_minimal(base_size = 12) +
#   theme(
#     axis.text.x        = element_text(angle = 30, hjust = 1),
#     panel.grid.major.x = element_blank(),
#     legend.position    = "right"
#   )
# 
# # --- J. Save --------------------------------------------------------------
# ggsave(
#   filename = file.path("output", "plot",
#                        "2012_2022_technique_vs_delta_material.pdf"),
#   device   = cairo_pdf,
#   plot     = p_compare,
#   width    = 12, height = 6.5, dpi = 300
# )
# 
# 
# 
# 
# 



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
       filename = "/home/mmondolfo/fabio_bfp/output/plot/p_indicator_BRA_IDN_USA.pdf",
       width = 12, 
       height = 9,
       dpi = 300)

######## ######## ######## ######## ######## ######## ######## 
######## Correlations between main biodiv indicators ######## 
######## ######## ######## ######## ######## ######## ######## 
# global totals per indicator × year
dt_global <- dt_tradeFeed[indicator %in% indicators_sel,
                          .(value = sum(value, na.rm = TRUE)),
                          by = .(indicator, year)]

# wide format: one column per indicator
dt_wide <- dcast(dt_global, year ~ indicator, value.var = "value")

# correlation matrix (across years, i.e. each row = one year)
cor_mat <- cor(dt_wide[, -"year"], use = "pairwise.complete.obs")

######## ######## ######## ######## ######## ######## ######## 
######## Correlations between subindicators ######## 
######## ######## ######## ######## ######## ######## ######## 

# ── 0. Helper: expand each indicator to its sub-indicators ──────────────────
# Indicators absent from sub_map$parent are their own sub-indicator
indicator_subs <- lapply(setNames(indicators_sel, indicators_sel), function(ind) {
  subs <- sub_map[parent == ind, indicator]
  if (length(subs) == 0L) ind else subs          # self if no children
})

all_subs <- unique(unlist(indicator_subs))        # every leaf we need

# ── 1. Global totals for sub-indicators ─────────────────────────────────────
dt_sub_global <- dt_tradeFeed[
  indicator %in% all_subs,
  .(value = sum(value, na.rm = TRUE)),
  by = .(indicator, year)
]

dt_sub_wide <- dcast(dt_sub_global, year ~ indicator, value.var = "value")
sub_cols    <- setdiff(names(dt_sub_wide), "year")

# ── 2. Correlation matrix between sub-indicators ─────────────────────────────
cor_sub <- cor(dt_sub_wide[, ..sub_cols],
               use = "pairwise.complete.obs")   # or "complete.obs"
# ── 3. SD of each parent indicator (for normalisation) ──────────────────────
# Re-use dt_wide already computed from your snippet above
sd_parent <- sapply(indicators_sel, function(ind) {
  sd(dt_wide[[ind]], na.rm = TRUE)
})

# ── 4. Build pairwise sub-indicator decomposition table ─────────────────────
decomp <- rbindlist(lapply(indicators_sel, function(ind_a) {
  rbindlist(lapply(indicators_sel, function(ind_b) {
    subs_a <- indicator_subs[[ind_a]]
    subs_b <- indicator_subs[[ind_b]]
    CJ(sub_a = subs_a, sub_b = subs_b)[,
                                       `:=`(
                                         indicator_a  = ind_a,
                                         indicator_b  = ind_b,
                                         cor_sub      = mapply(function(sa, sb) cor_sub[sa, sb], sub_a, sub_b),
                                         contribution = mapply(function(sa, sb)
                                           cor_sub[sa, sb] / (length(subs_a) * length(subs_b)),   # see note
                                           sub_a, sub_b)
                                       )]
  }))
}))

setcolorder(decomp, c("indicator_a", "indicator_b", "sub_a", "sub_b",
                      "cor_sub", "contribution"))




library(corrplot)

# Build ordered index: ibif_ first, then LCIM_EQ_, then FD_EQ_
parent_order <- c(
  indicator_subs[["ibif_total"]],
  indicator_subs[["LCIM_EQ_terrestrial"]],
  indicator_subs[["FD_EQ_ric_terrestrial"]]
)

# Subset & reorder the matrix
cor_sub_ord <- cor_sub[parent_order, parent_order]

pdf("output/plot/cor_indicators.pdf", width = 10, height = 9)

cor_subindicators <- corrplot(
  cor_sub_ord,
  method      = "color",
  type        = "upper",
  order       = "original",        # ← respect our manual ordering
  addCoef.col = "black",
  number.cex  = 0.7,
  tl.cex      = 0.8,
  tl.col      = "black",
  tl.srt      = 45,
  col         = colorRampPalette(c("#d73027", "#f7f7f7", "#1a9850"))(200),
  title       = "Sub-indicator correlation matrix",
  mar         = c(0, 0, 2, 0)
)

dev.off() 






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
      strip.text         = element_text(face = "bold"),
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
# Input: fread("output/FABIO_Ysourcing_2012-2022_BF_byComm_inclSelf.csv")
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
files_material <- list.files(
  "output",
  pattern = "^FABIO_tradeFeed_(201[2-9]|202[0-2])_material_value_BF_byComm_byFeed_direct_bilat\\.csv$",
  full.names = TRUE)
dt_material <- rbindlist(lapply(files_material, fread))

p_feed_src <- plot_sourcing_feedstock(
  dt_material, regions,
  commodities    = c("c146", "c147", "c149"),
  continents     = c("EU", "ASI", "NAM", "LAM"),
  indicator_keep = "material",
  allocation_keep = "value")

ggsave(file.path("output", "plot", "sourcing_feedstock_BF.pdf"),
       p_feed_src, width = 11, height = 6, dpi = 300, device = cairo_pdf)

# --- (2) final product origin -----------------------------------------------
dt_ysrc <- fread("output/FABIO_Ysourcing_2012-2022_BF_byComm_inclSelf.csv")

p_prod_src <- plot_sourcing_product(
  dt_ysrc, regions,
  commodities = c("c146", "c147", "c149"),
  continents  = c("EU", "ASI", "NAM", "LAM"))

ggsave(file.path("output", "plot", "sourcing_product_BF.pdf"),
       p_prod_src, width = 11, height = 6, dpi = 300, device = cairo_pdf)







plot_consumption_vs_impact_by_continent <- function(
    Y_data, impact_data, regions,
    target_comm_in, indicator_select,
    meta      = indicator_meta,
    y_label   = "Biofuel consumption (M liters)",
    grid_nrow = NULL,
    grid_ncol = NULL) {
  
  # ── grid path: vectorised target_comm_in ───────────────────────────
  if (length(target_comm_in) > 1) {
    plots <- lapply(target_comm_in, function(cc) {
      plot_consumption_vs_impact_by_continent(
        Y_data           = Y_data,
        impact_data      = impact_data,
        regions          = regions,
        target_comm_in   = cc,
        indicator_select = indicator_select,
        meta             = meta,
        y_label          = y_label
      )
    })
    if (is.null(grid_nrow) && is.null(grid_ncol)) grid_nrow <- 1
    
    return(
      patchwork::wrap_plots(plots, nrow = grid_nrow, ncol = grid_ncol) +
        patchwork::plot_layout(guides = "collect") &
        ggplot2::theme(
          legend.position      = "bottom",
          legend.box           = "horizontal",
          legend.justification = "center",
          legend.box.just      = "center"
        )
    )
  }
  
  # ── single-commodity path ──────────────────────────────────────────
  cons <- Y_data %>%
    filter(comm_code == target_comm_in) %>%
    group_by(year, target_continent) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop")
  if (nrow(cons) == 0) stop("No Y rows for that comm_code.")
  
  item_name <- Y_data %>%
    filter(comm_code == target_comm_in) %>%
    pull(item) %>% unique() %>% paste(collapse = "/")
  
  m <- meta[indicator == indicator_select]
  if (nrow(m) == 0) {
    m <- data.table(indicator    = indicator_select,
                    scale_factor = 1,
                    y_label      = indicator_select,
                    short_label  = indicator_select)
  }
  
  imp <- impact_data %>%
    left_join(regions %>% select(iso3c, continent),
              by = c("country_consumer" = "iso3c")) %>%
    rename(target_continent = continent) %>%
    filter(commodity == target_comm_in,
           indicator == indicator_select) %>%
    group_by(year, target_continent) %>%
    summarize(value = sum(value, na.rm = TRUE) / m$scale_factor,
              .groups = "drop")
  
  common <- intersect(cons$target_continent, imp$target_continent)
  cons <- cons %>% filter(target_continent %in% common)
  imp  <- imp  %>% filter(target_continent %in% common)
  
  cons_max <- max(cons$value, na.rm = TRUE)
  imp_max  <- if (nrow(imp)) max(imp$value, na.rm = TRUE) else 1
  k <- if (is.finite(imp_max) && imp_max > 0) cons_max / imp_max else 1
  
  flow_imp <- m$short_label
  plot_df <- bind_rows(
    cons %>% mutate(flow = "Consumption", y = value),
    imp  %>% mutate(flow = flow_imp,      y = value * k)
  ) %>%
    mutate(flow = factor(flow, levels = c("Consumption", flow_imp)),
           target_continent = factor(target_continent,
                                     levels = names(continent_palette)))
  
  ggplot(plot_df, aes(year, y,
                      colour   = target_continent,
                      linetype = flow,
                      group    = interaction(target_continent, flow))) +
    geom_line(linewidth = 0.7) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    scale_y_continuous(
      name     = y_label,
      sec.axis = sec_axis(~ . / k, name = m$y_label)
    ) +
    scale_linetype_manual(values = setNames(c("solid", "longdash"),
                                            c("Consumption", flow_imp))) +
    scale_colour_manual(values   = continent_palette,
                        name     = "Continent",
                        na.value = "grey60",
                        drop     = FALSE) +
    labs(title    = paste0(item_name, " · ", m$short_label),
         x        = NULL,
         colour   = "Continent",
         linetype = "Flow") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          legend.key.size  = unit(0.6, "cm"))
}



plot_consumption_vs_impact_by_continent(
  Y_data           = Y_per_year_continent,
  impact_data      = dt_tradeFeed,
  regions          = regions,
  target_comm_in   = c("c146", "c147", "c149"),
  indicator_select = "ibif_total"
)







plot_decoupling_indexed <- function(
    Y_data, impact_data, regions,
    target_comm_in, indicator_select,
    meta = indicator_meta,
    base_year = NULL) {
  
  cons <- Y_data %>%
    filter(comm_code %in% target_comm_in) %>%
    group_by(year, target_continent, comm_code, item) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(flow = "Consumption")
  
  imp <- impact_data %>%
    left_join(regions %>% select(iso3c, continent),
              by = c("country_consumer" = "iso3c")) %>%
    rename(target_continent = continent) %>%
    filter(commodity %in% target_comm_in,
           indicator == indicator_select) %>%
    group_by(year, target_continent, commodity) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    rename(comm_code = commodity) %>%
    left_join(distinct(cons, comm_code, item), by = "comm_code") %>%
    mutate(flow = meta[indicator == indicator_select, short_label])
  
  df <- bind_rows(cons, imp)
  if (is.null(base_year)) base_year <- min(df$year)
  
  df <- df %>%
    group_by(flow, target_continent, comm_code, item) %>%
    arrange(year) %>%
    mutate(
      base_val = value[year == base_year][1],
      index    = ifelse(is.finite(base_val) & base_val > 0,
                        100 * value / base_val, NA_real_)
    ) %>%
    ungroup() %>%
    filter(is.finite(index))
  
  # Wide form for ribbon between the two flows
  df_w <- df %>%
    select(year, target_continent, item, flow, index) %>%
    tidyr::pivot_wider(names_from = flow, values_from = index) %>%
    rename(cons_idx = Consumption,
           imp_idx  = !!meta[indicator == indicator_select, short_label])
  
  ggplot() +
    geom_hline(yintercept = 100, colour = "grey80", linewidth = 0.3) +
    geom_ribbon(data = df_w,
                aes(x = year, ymin = pmin(cons_idx, imp_idx),
                    ymax = pmax(cons_idx, imp_idx),
                    fill = cons_idx > imp_idx),
                alpha = 0.15) +
    geom_line(data = df,
              aes(year, index, colour = flow, linetype = flow),
              linewidth = 0.7) +
    facet_grid(rows = vars(target_continent), cols = vars(item),
               scales = "free_y") +
    scale_colour_manual(values = setNames(c("#444444", "#D55E00"),
                                          c("Consumption",
                                            meta[indicator == indicator_select, short_label]))) +
    scale_linetype_manual(values = setNames(c("solid", "longdash"),
                                            c("Consumption",
                                              meta[indicator == indicator_select, short_label]))) +
    scale_fill_manual(values = c(`TRUE` = "#0072B2", `FALSE` = "#D55E00"),
                      labels = c(`TRUE` = "Decoupling gap", `FALSE` = "Recoupling gap"),
                      name   = NULL) +
    labs(title = paste0("Decoupling: consumption vs ",
                        meta[indicator == indicator_select, short_label]),
         subtitle = paste0("Indexed to ", base_year, " = 100"),
         x = NULL, y = paste0("Index (", base_year, " = 100)"),
         colour = NULL, linetype = NULL) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          legend.position  = "bottom")
}

plot_decoupling_indexed(
  Y_data           = Y_per_year_continent,
  impact_data      = dt_tradeFeed,
  regions          = regions,
  target_comm_in   = c("c146", "c147"),
  indicator_select = "ibif_total"
)
