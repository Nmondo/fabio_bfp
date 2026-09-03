# 20_3 - make EU-specific plots

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

source("R/19_01a_plot_function_material.R")
source("R/19_01b_plot_function_footprints.R")

# ENV_DIR / MAT_DIR come from 19_01b (default variant = capped; set
# FABIO_VARIANT=uncapped for the baseline). EU environmental plots are written to
# a per-variant dir; material inputs (Y/Z/X_summary, EU feedstock mix) stay on the
# capping-invariant baseline MAT_DIR.
EU_PLOT_DIR <- file.path(ENV_DIR, "EU", "plot")
dir.create(EU_PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

################################################################################
##  1.  LOAD INPUTS
################################################################################
## Y_summary (consumption) and Z_summary (feedstock) are the 18_01a dumps read
## exactly as in 19_01a; dt_tradeFeed (value-allocated footprints) is read
## exactly as in 19_01b. dt_tradeFeed (environmental) and fabio_files() come from
## the sourced 19_01b (ENV_DIR); Y/Z/X_summary are material (MAT_DIR, baseline).
Y_summary <- fread(file.path(MAT_DIR, "Y_summary_c146_c147_c149.csv"))
Z_summary <- fread(file.path(MAT_DIR, "Z_summary_c146_c147_c149.csv"))

## NEW: X_summary = biofuel gross output (production), written by 18_01a
X_summary <- fread(file.path(MAT_DIR, "X_summary_c146_c147_c149.csv"))

## dt_tradeFeed as in 19_01b (value allocation, BF group)
files_tradeFeed_BF <- fabio_files("FABIO_tradeFeed", "BF", alloc = "value")
dt_tradeFeed       <- rbindlist(lapply(files_tradeFeed_BF, fread))

files_tradeFeed_BP <- fabio_files("FABIO_tradeFeed", "BP", alloc = "value")
dt_tradeFeed_BP <- rbindlist(lapply(files_tradeFeed_BP, fread))

## Build combined LC-Impact terrestrial indicator (climate + acidification;
## ruling out "land use") — same recomputation as 19_01b, so LCIM_EQ_terrestrial
## is available to the plots below alongside its sub-indicators.
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

## EU embodied-feedstock mix (origin-resolved), from 18_00_validation_FABIO_RED
fabio_eu_mix <- fread(file.path(MAT_DIR, "EU", "FABIO_EU_embodied_feedstock_mix.csv"))

## EU restriction (consumer AND producer) + biofuel commodities used throughout
EU_iso3 <- regions[continent == "EU", iso3c]
GBR <- "GBR"
EU_iso3 <- setdiff(EU_iso3, GBR)                 # belt-and-suspenders for iso_keep = EU_iso3 defaults
BF_COMM <- c("c146", "c147", "c149")


Y_summary    <- Y_summary[target_country    != GBR]
X_summary    <- X_summary[producer_country  != GBR]
Z_summary    <- Z_summary[target_country != GBR]  # verify col names
dt_tradeFeed <- dt_tradeFeed[country_consumer != GBR]
fabio_eu_mix <- fabio_eu_mix[country != GBR] # verify col names


################################################################################
##  DESCRIPTIVE STATS
################################################################################
fabio_eu_mix[, feedstock_cat := fifelse(
  comm_code == "c901", "Annex IX part A",
  fifelse(comm_code %in% c("c119", "c145"), "UCO & Animal fats",
          "Conventional")
)]

## total fabio_resc by (year, biofuel_code, feedstock_cat)
tab_by_biofuel <- fabio_eu_mix[
  , .(fabio_resc = sum(fabio_resc, na.rm = TRUE)),
  by = .(year, biofuel_code, feedstock_cat)
][order(year, biofuel_code, feedstock_cat)]

## total fabio_resc by (year, feedstock_cat) only
tab_by_cat <- fabio_eu_mix[
  , .(fabio_resc = sum(fabio_resc, na.rm = TRUE)),
  by = .(year, feedstock_cat)
][order(year, feedstock_cat)]

tab_wide <- dcast(
  tab_by_biofuel,
  feedstock_cat + year ~ biofuel_code,
  value.var = "fabio_resc",
  fill = 0
)

## keep only c146 / c147 / c149 (create as 0 if entirely absent from the data)
for (bf in c("c146", "c147", "c149")) {
  if (!bf %in% names(tab_wide)) tab_wide[, (bf) := 0]
}
tab_wide <- tab_wide[, .(feedstock_cat, year, c146, c147, c149)]

## attach the (year, feedstock_cat) total from tab_by_cat
tab_wide <- merge(tab_wide, tab_by_cat, by = c("feedstock_cat", "year"), all.x = TRUE)
setnames(tab_wide, "fabio_resc", "total")
tab_wide[is.na(total), total := 0]

setorder(tab_wide, feedstock_cat, year)

print(tab_wide)

## share of total fabio_resc per feedstock_cat, within each year
tab_wide[, year_total := sum(total), by = year]
tab_wide[, share := total / year_total]

ggplot(tab_wide, aes(year, share, color = feedstock_cat)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Share of total", color = NULL) +
  theme_minimal()

################################################################################
##  Evolution of specific feedstocks (comm_code c074 / c119 / c145)
################################################################################

## comm_code -> item lookup (one item name per code)
comm_lookup <- unique(fabio_eu_mix[, .(comm_code, item)])

## total fabio_resc by (year, comm_code) — all codes, no filtering
tab_by_comm_all <- fabio_eu_mix[
  , .(fabio_resc = sum(fabio_resc, na.rm = TRUE)),
  by = .(year, comm_code)
][order(year, comm_code)]

tab_comm_wide_all <- dcast(
  tab_by_comm_all,
  year ~ comm_code,
  value.var = "fabio_resc",
  fill = 0
)

## back to long for plotting, as shares of each year's total
tab_comm_long <- melt(tab_comm_wide_all, id.vars = "year",
                      variable.name = "comm_code", value.name = "fabio_resc")
tab_comm_long[, year_total := sum(fabio_resc), by = year]
tab_comm_long[, share := fabio_resc / year_total]

## attach the readable item name for the legend
tab_comm_long <- merge(tab_comm_long, comm_lookup, by = "comm_code", all.x = TRUE)

compact_names <- c(
  "Rape and Mustard Oil"  = "Rapeseed Oil",
  "Palm Oil"               = "Palm Oil",
  "Maize and products"     = "Maize",
  "Wheat and products"     = "Wheat",
  "Soyabean Oil"           = "Soybean Oil",
  "Fats, Animals, Raw"     = "Animal Fats",
  "Animal or vegetable fats and oils and their fractions, chemically modified, except those hydrogenated, inter-esterified, re-esterified or elaidinized; inedible mixtures or preparations of animal or vegetable fats or oils" = "Used Cooking Oil",
  "Sugar beet"             = "Sugar Beet",
  "Sugar cane"             = "Sugar Cane",
  "Molasses"               = "Molasses",
  "Rice and products"      = "Rice",
  "Barley and products"    = "Barley",
  "Rye and products"       = "Rye",
  "Triticale"              = "Triticale",
  "Sorghum and products"   = "Sorghum",
  "Cassava and products"   = "Cassava",
  "Palmkernel Oil"         = "Palmkernel Oil",
  "Coconut Oil"            = "Coconut Oil",
  "Cottonseed Oil"         = "Cottonseed Oil",
  "Maize Germ Oil"         = "Maize Germ Oil",
  "Sunflowerseed Oil"      = "Sunflower Oil",
  "Other, Waste"           = "Annex IX Part A",
  "Other, Unknown"         = "Unknown",
  "Other"                  = "Other"
)
compact_lab <- function(x) {
  out <- compact_names[x]
  out[is.na(out)] <- x[is.na(out)]
  unname(out)
}

## apply to the item names before plotting
tab_comm_long[, item_label := compact_lab(item)]

################################################################################
##  EU IBIF, aggregated across all biofuels, broken down by FEEDSTOCK (top-8)
################################################################################
imp_dt_feed <- dt_tradeFeed[
  indicator == "ibif_total" & commodity %in% BF_COMM & country_consumer %in% EU_iso3,
  .(value = sum(value, na.rm = TRUE)),
  by = .(year, feedstock)
]

## top-8 feedstocks by total impact over the full period, rest -> "Other"
feed_tot <- imp_dt_feed[, .(total = sum(value)), by = feedstock][order(-total)]

## rename to match feedstock_meta / feedstock_color_map's naming exactly
feedstock_relabel <- c(
  "Animal or vegetable fats and oils and their fractions, chemically modified, except those hydrogenated, inter-esterified, re-esterified or elaidinized; inedible mixtures or preparations of animal or vegetable fats or oils" = "Used Cooking Oil"
)
relabel_feed <- function(x) {
  out <- feedstock_relabel[x]
  out[is.na(out)] <- x[is.na(out)]
  unname(out)
}
feed_tot[, feedstock := relabel_feed(feedstock)]
imp_dt_feed[, feedstock := relabel_feed(feedstock)]

top_feed <- feed_tot[seq_len(min(8, .N)), feedstock]
imp_dt_feed[, feedstock := fifelse(feedstock %in% top_feed, feedstock, "Other")]
imp_dt_feed <- imp_dt_feed[, .(value = sum(value)), by = .(year, feedstock)]

## scale from indicator_meta (raw -> base IBIF units), THEN an extra /1000 so
## the figure reports "1,000 MSA-loss·km²" rather than raw MSA-loss·km²;
## y_lab is set manually here rather than taken from m_ibif$y_label, since
## that label reflects the base (unscaled-by-1000) unit.
m_ibif <- indicator_meta[indicator == "ibif_total"]
imp_dt_feed[, value := value / m_ibif$scale_factor[1] / 1000]
ibif_y_lab <- "Pristine area loss equivalents (1,000 MSA-loss\u00b7km\u00b2)"

## order: biggest feedstock at the bottom of the stack (reverse position_stack)
feed_levels <- feed_tot[feedstock %in% top_feed, feedstock]
imp_dt_feed[, feedstock := factor(feedstock, levels = c(feed_levels, "Other"))]

tot_feed <- imp_dt_feed[, .(total = sum(value)), by = year]

## reuse the same categorised feedstock palette as plot_feedstock_desc if it's
## in scope, KEYED BY THE ORIGINAL (feedstock_meta) NAMES so the color lookup
## still matches; legend TEXT is compacted separately via scale_fill_manual(labels=...)
if (exists("feedstock_color_map")) {
  feed_pal <- feedstock_color_map[levels(imp_dt_feed$feedstock)]
  feed_pal[is.na(feed_pal)] <- "grey75"
  names(feed_pal) <- levels(imp_dt_feed$feedstock)
} else {
  feed_pal <- c(setNames(.distinct_country_cols(length(feed_levels)), feed_levels),
                Other = "grey75")
}

## compact legend labels (display only — factor levels / palette keys unchanged)
compact_names <- c(
  "Rape and Mustard Oil" = "Rapeseed Oil",
  "Palm Oil"              = "Palm Oil",
  "Maize and products"    = "Maize",
  "Wheat and products"    = "Wheat",
  "Soyabean Oil"          = "Soybean Oil",
  "Fats, Animals, Raw"    = "Animal Fats",
  "Used Cooking Oil"      = "Used Cooking Oil",
  "Sugar beet"            = "Sugar Beet",
  "Sugar cane"            = "Sugar Cane",
  "Molasses"              = "Molasses",
  "Rice and products"     = "Rice",
  "Barley and products"   = "Barley",
  "Rye and products"      = "Rye",
  "Triticale"             = "Triticale",
  "Sorghum and products"  = "Sorghum",
  "Cassava and products"  = "Cassava",
  "Palmkernel Oil"        = "Palmkernel Oil",
  "Coconut Oil"           = "Coconut Oil",
  "Cottonseed Oil"        = "Cottonseed Oil",
  "Maize Germ Oil"        = "Maize Germ Oil",
  "Sunflowerseed Oil"     = "Sunflower Oil",
  "Other"                 = "Other",
  "Other, Waste"          = "Annex IX Part A"
)
compact_lab <- function(x) {
  out <- compact_names[x]
  out[is.na(out)] <- x[is.na(out)]
  unname(out)
}

## rounded to the unit (i.e. whole thousands) everywhere it's displayed —
## bar totals AND the primary axis ticks
num <- scales::label_number(accuracy = 1)

p_ibif_feedstock <- ggplot(imp_dt_feed, aes(year, value, fill = feedstock)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = feed_pal, name = NULL, drop = FALSE,
                    labels = compact_lab(levels(imp_dt_feed$feedstock))) +
  scale_x_continuous(breaks = scales::pretty_breaks(),
                     expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(labels = num, expand = expansion(mult = c(0, 0.10))) +
  labs(x = NULL, y = ibif_y_lab) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_minimal(base_size = 8) +
  theme(panel.grid.minor  = element_blank(),
        legend.position   = "bottom",
        legend.direction  = "horizontal",
        legend.key.size   = unit(0.28, "cm"),
        legend.text       = element_text(size = 6.5),
        legend.spacing.x  = unit(0.15, "cm"),
        legend.margin     = margin(t = 2, b = 2),
        legend.box.margin = margin(t = 0),
        plot.margin       = margin(t = 5, r = 10, b = 5, l = 10))

ggsave(file.path(EU_PLOT_DIR, "EU_ibif_by_feedstock_BF_aggregated.png"),
       p_ibif_feedstock, width = 4, height = 3.5, dpi = 300)


################################################################################
##  5b. EU CONSUMPTION feedstock mix by ITEM (top-8), stacked bars
################################################################################
## Standalone: fabio_resc (biofuel-output-equivalent, k-litres) from
## fabio_eu_mix, summed by (year, item). Top-8 items over the full period +
## "Other", stacked bars. Self-contained — no second axis and no reliance on the
## IBIF plot's build-up: the UCO relabel and the compact legend map are inlined
## below; the feedstock palette is used if present, else a grey/hue fallback.

## --- inlined helpers (no dependence on the IBIF section) --------------------
## long UCO item name -> "Used Cooking Oil", to match feedstock_color_map keys
.uco_long <- paste0(
  "Animal or vegetable fats and oils and their fractions, chemically modified, ",
  "except those hydrogenated, inter-esterified, re-esterified or elaidinized; ",
  "inedible mixtures or preparations of animal or vegetable fats or oils")
mix_relabel <- function(x) fifelse(x == .uco_long, "Used Cooking Oil", x)

## compact display labels for the legend (palette keys stay = original names)
mix_compact_names <- c(
  "Rape and Mustard Oil" = "Rapeseed Oil", "Palm Oil" = "Palm Oil",
  "Maize and products" = "Maize", "Wheat and products" = "Wheat",
  "Soyabean Oil" = "Soybean Oil", "Fats, Animals, Raw" = "Animal Fats",
  "Used Cooking Oil" = "Used Cooking Oil", "Sugar beet" = "Sugar Beet",
  "Sugar cane" = "Sugar Cane", "Molasses" = "Molasses",
  "Rice and products" = "Rice", "Barley and products" = "Barley",
  "Rye and products" = "Rye", "Triticale" = "Triticale",
  "Sorghum and products" = "Sorghum", "Cassava and products" = "Cassava",
  "Palmkernel Oil" = "Palmkernel Oil", "Coconut Oil" = "Coconut Oil",
  "Cottonseed Oil" = "Cottonseed Oil", "Maize Germ Oil" = "Maize Germ Oil",
  "Sunflowerseed Oil" = "Sunflower Oil", "Other" = "Other", "Other, Waste" = "Annex IX Part A")
mix_compact_lab <- function(x) {
  out <- mix_compact_names[x]; out[is.na(out)] <- x[is.na(out)]; unname(out)
}

## --- data: sum fabio_resc by (year, item), top-8 + "Other" ------------------
mix_dt_item <- as.data.table(fabio_eu_mix)[
  , .(value = sum(fabio_resc, na.rm = TRUE)), by = .(year, item)]
mix_dt_item[, item := mix_relabel(item)]

item_tot  <- mix_dt_item[, .(total = sum(value)), by = item][order(-total)]
top_items <- item_tot[seq_len(min(8, .N)), item]
mix_dt_item[, item := fifelse(item %in% top_items, item, "Other")]
mix_dt_item <- mix_dt_item[, .(value = sum(value) / 1e6), by = .(year, item)]  # k-litres -> B litres
mix_y_lab <- "Biofuels from feedstocks in EU consumption (B litres)"

## biggest item at the bottom of the stack (reverse position_stack)
item_levels <- item_tot[item %in% top_items, item]
mix_dt_item[, item := factor(item, levels = c(item_levels, "Other"))]

## --- palette: feedstock_color_map if available, else fallback ---------------
if (exists("feedstock_color_map")) {
  mix_pal <- feedstock_color_map[levels(mix_dt_item$item)]
  mix_pal[is.na(mix_pal)] <- "grey75"
  names(mix_pal) <- levels(mix_dt_item$item)
} else {
  mix_pal <- setNames(scales::hue_pal()(nlevels(mix_dt_item$item)),
                      levels(mix_dt_item$item))
  mix_pal["Other"] <- "grey75"
}

num <- scales::label_number(accuracy = 1)

p_mix_item <- ggplot(mix_dt_item, aes(year, value, fill = item)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = mix_pal, name = NULL, drop = FALSE,
                    labels = mix_compact_lab(levels(mix_dt_item$item))) +
  scale_x_continuous(breaks = scales::pretty_breaks(),
                     expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(labels = num, expand = expansion(mult = c(0, 0.10))) +
  labs(x = NULL, y = mix_y_lab) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_minimal(base_size = 8) +
  theme(panel.grid.minor  = element_blank(),
        legend.position   = "bottom",
        legend.direction  = "horizontal",
        legend.key.size   = unit(0.28, "cm"),
        legend.text       = element_text(size = 6.5),
        legend.spacing.x  = unit(0.15, "cm"),
        legend.margin     = margin(t = 2, b = 2),
        legend.box.margin = margin(t = 0),
        plot.margin       = margin(t = 5, r = 10, b = 5, l = 10))

ggsave(file.path(EU_PLOT_DIR, "EU_consumption_feedstock_mix_item_top8_BF.png"),
       p_mix_item, width = 4, height = 3.5, dpi = 300)


################################################################################
##  6.  EU CONSUMER SHARE within each origin continent
################################################################################

plot_eu_origin_share <- function(dt_tradeFeed, regions,
                                 years1              = 2012:2014,
                                 years2              = 2020:2022,
                                 indicator_select    = "ibif_total",
                                 meta                = indicator_meta,
                                 top_n               = 6,
                                 highlight_continent = "EU",
                                 scale               = NULL,
                                 y_lab               = NULL) {
  
  m <- meta[indicator == indicator_select]
  if (nrow(m) == 0)
    m <- data.table(indicator = indicator_select, scale_factor = 1,
                    y_label = indicator_select, short_label = indicator_select)
  if (is.null(scale)) scale <- m$scale_factor[1]
  ## ibif_total: same extra /1000 + label as the feedstock plot above, so
  ## "Pristine area loss equivalents" is reported in 1,000 MSA-loss·km² here too
  if (indicator_select == "ibif_total") {
    scale <- scale * 1000
    if (is.null(y_lab)) y_lab <- "Pristine area loss equivalents (1,000 MSA-loss\u00b7km\u00b2)"
  }
  if (is.null(y_lab)) y_lab  <- m$y_label[1]
  short_lab <- m$short_label[1]
  
  ## --- local continent colour overrides (layered on continent_palette) -------
  ## EU: highlight, vivid dark green. EUR (rest of Europe): a lighter green but
  ## kept saturated so it survives the dulling applied to non-highlight fills.
  ## NAM blue, LAM sky blue (distinct from NAM's blue), ASI brown, OCE purple.
  cont_pal <- continent_palette
  cont_pal[c("EU", "EUR", "NAM", "LAM", "ASI", "OCE")] <- c(
    "#2f7e07",   # EU  — vivid dark green (highlight)
    "#57a639",   # EUR — lighter green, still saturated
    "#1f78b4",   # NAM — blue
    "#56c1e6",   # LAM — sky blue
    "#8c510a",   # ASI — brown
    "#6a3d9a")   # OCE — purple
  
  ## period labels (e.g. "2012-2014" / "2020-2022")
  lab1 <- sprintf("%d\u2013%d", min(years1), max(years1))
  lab2 <- sprintf("%d\u2013%d", min(years2), max(years2))
  nmap <- c(length(years1), length(years2)); names(nmap) <- c(lab1, lab2)
  
  cont_vec <- setNames(as.character(regions$continent), regions$iso3c)
  d <- as.data.table(dt_tradeFeed)[indicator == indicator_select &
                                     year %in% c(years1, years2)]
  if (!nrow(d)) stop("No ", indicator_select, " rows for the requested years")
  
  ## collapse to (year, origin, consumer), attach BOTH continents, scale
  d <- d[, .(value = sum(value, na.rm = TRUE)),
         by = .(year, country_origin, country_consumer)]
  d[, origin_cont := cont_vec[country_origin]]
  d[is.na(origin_cont), origin_cont := "ROW"]
  d[, cons_cont := cont_vec[country_consumer]]
  d[is.na(cons_cont), cons_cont := "ROW"]
  d[, period := fifelse(year %in% years1, lab1, lab2)]
  
  ## sum within each period, THEN divide by window length -> 3-year AVERAGE
  agg0 <- d[, .(value = sum(value) / scale), by = .(period, origin_cont, cons_cont)]
  agg0[, value := value / nmap[period]]
  
  ## shared fill palette from the UNION of consumer continents across both periods
  conts <- c(highlight_continent,
             sort(setdiff(unique(agg0$cons_cont), highlight_continent)))
  dull <- function(hex, amt = 0.7, toward = "grey85") {
    m2 <- grDevices::col2rgb(toward) / 255
    vapply(hex, function(cc) { v <- grDevices::col2rgb(cc) / 255
    grDevices::rgb(t(v * (1 - amt) + m2 * amt)) }, character(1), USE.NAMES = FALSE)
  }
  base_pal <- cont_pal[conts]
  base_pal[is.na(base_pal)] <- "grey50"
  pal          <- base_pal
  non_hl       <- setdiff(conts, highlight_continent)
  pal[non_hl]  <- dull(base_pal[non_hl])
  pal[highlight_continent] <- unname(cont_pal[highlight_continent])
  
  ## --- top-N set AND order fixed by PERIOD 1 highlight-driven impact ---------
  hl_p1     <- agg0[period == lab1 & cons_cont == highlight_continent,
                    .(hl_val = sum(value)), by = origin_cont][order(-hl_val)]
  top_conts <- hl_p1[seq_len(min(top_n, .N)), origin_cont]
  ord       <- c(hl_p1[origin_cont %in% top_conts, origin_cont], "Other")
  
  ## bucket every period into the same top set; re-aggregate
  a <- agg0[, .(value = sum(value)),
            by = .(period,
                   origin_cont = fifelse(origin_cont %in% top_conts, origin_cont, "Other"),
                   cons_cont)]
  
  ## --- numeric x-positions: per origin group, period1 then period2 ----------
  ## group_stride tightened (was 2.8) so continent groups sit as close together
  ## as possible; the gap left between adjacent groups is just enough for the
  ## vertical separator line added below (bar width stays 0.9).
  group_stride <- 2.05
  ord_dt <- data.table(origin_cont = ord, gi = seq_len(length(ord)) - 1L)
  per_dt <- data.table(period = c(lab1, lab2), pj = c(0, 1))
  
  ## x-position of a vertical separator between each pair of adjacent origin
  ## groups, placed at the midpoint of the (now narrow) inter-group gap
  n_grp <- length(ord)
  sep_x <- if (n_grp > 1) {
    gi_left <- seq_len(n_grp - 1) - 1L
    (gi_left * group_stride + 1 + (gi_left + 1) * group_stride) / 2
  } else numeric(0)
  
  a <- merge(merge(a, ord_dt, by = "origin_cont"), per_dt, by = "period")
  a[, xpos := gi * group_stride + pj]
  
  a[, cons_cont := factor(cons_cont, levels = conts)]
  
  ## per-bar colour = ORIGIN continent (used for the manual year labels below)
  bar_pos <- unique(a[, .(xpos, origin_cont, period)])[order(xpos)]
  bar_pos[, col := cont_pal[origin_cont]]
  bar_pos[is.na(col), col := "grey30"]
  
  ## bold origin-continent label, centred under each pair, hugging the 0 axis
  bar_tot <- a[, .(bar_total = sum(value)), by = .(gi, origin_cont, xpos)]
  grp <- bar_tot[, .(x = gi[1] * group_stride + 0.5), by = .(gi, origin_cont)]
  grp[, col := cont_pal[origin_cont]]
  grp[is.na(col), col := "grey30"]
  
  ## Label y-positions as fractions of the tallest bar, so the gaps stay
  ## proportional across indicators. BOTH label rows are drawn as geom_text (not
  ## axis text): the period labels used to be axis.text.x with a vectorised
  ## colour AND a top margin, but ggplot >= 3.5 drops the margin when the colour
  ## is vectorised, so the year labels collapsed back onto the continent labels.
  ## Drawing them manually makes the vertical gap explicit and version-proof.
  max_bt <- max(bar_tot$bar_total)
  cont_y <- -0.02  * max_bt   # continent labels: hug the 0 axis
  year_y <- -0.075 * max_bt   # year labels: clearly below the continent labels
  
  ggplot(a, aes(xpos, value, fill = cons_cont)) +
    geom_vline(xintercept = sep_x, colour = "grey55", linewidth = 0.4) +
    geom_col(position = position_stack(reverse = TRUE), width = 0.9) +
    ## continent labels (bold, hugging the 0 axis)
    geom_text(data = grp,
              aes(x = x, y = cont_y, label = origin_cont),
              inherit.aes = FALSE, vjust = 1, size = 5, fontface = "bold",
              colour = grp$col) +
    ## year/period labels, drawn manually below the continent labels, rotated
    ## vertical and coloured by origin continent (hjust = 1 -> hang downward).
    ## Increase the gap between the two rows via year_y; give plot.margin bottom
    ## enough room (below) so these are not clipped.
    geom_text(data = bar_pos,
              aes(x = xpos, y = year_y, label = period),
              inherit.aes = FALSE, angle = 90, hjust = 1, vjust = 0.5,
              size = 3.9, colour = bar_pos$col) +
    scale_fill_manual(values = pal, name = "Continent", drop = FALSE) +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(labels = scales::label_number(),
                       expand = expansion(mult = c(0, 0.12))) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = y_lab) +
    guides(fill = guide_legend(ncol = 1, byrow = TRUE)) +
    theme_minimal(base_size = 16) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank(),
          axis.text.x        = element_blank(),
          axis.ticks.x       = element_blank(),
          ## bottom margin sized for the manually-drawn vertical year labels
          plot.margin        = margin(t = 5, r = 5, b = 95, l = 5),
          legend.position    = "right")
}

for (share_indicator in c("ibif_total", "LCIM_EQ_terrestrial", "LCIM_EQ_terrestrial_land_use")) {
  p_share <- plot_eu_origin_share(dt_tradeFeed, regions,
                                  years1 = 2012:2014, years2 = 2020:2022,
                                  indicator_select = share_indicator)
  ggsave(file.path(EU_PLOT_DIR,
                   paste0("EU_origin_continent_share_", share_indicator, "_avg_2012-14_2020-22.png")),
         p_share, width = 9, height = 9, dpi = 300)
}

################################################################################
##  7.  EU CONSUMPTION: biofuels from feedstocks (embodied feedstock mix)
################################################################################
## fabio_eu_mix = origin-resolved feedstock embodied in EU biofuel CONSUMPTION,
## already in biofuel-output-equivalent (fabio_resc, k-litres). Sum over EU
## consumer countries + origins to (year, biofuel_code, item), then reuse 19_01a's
## plot_feedstock_desc (same categorised feedstock palette as 20_01a): 3 rows
## (Biogasoline / Biodiesel / Renewable diesel) x 1 column, top-8 feedstocks per
## biofuel by the sum over ALL years + "Other", time series 2012-2022.
##
## to_supply = FALSE: fabio_resc is ALREADY biofuel-output-equivalent, so no
## second TCF conversion. top_scope = "per_comm" mirrors 20_01a's call (top-8 per
## biofuel); switch to "global" for a single shared set of 8 across all biofuels.
eu_mix_desc <- as.data.table(fabio_eu_mix)[
  biofuel_code %in% BF_COMM,
  .(value = sum(fabio_resc, na.rm = TRUE)),
  by = .(year, target_comm = biofuel_code, origin_comm_name = item)]
eu_mix_desc[, target_continent := "EU"]

p_eu_feed <- plot_feedstock_desc(
  as.data.frame(eu_mix_desc),
  target_comm_in      = BF_COMM,
  target_continent_in = "EU",
  n_top               = 8,
  top_scope           = "per_comm",   # top-8 per biofuel (as in 20_01a)
  to_supply           = FALSE,        # fabio_resc already biofuel-output-equivalent
  ncol                = 1,            # 3 rows x 1 column
  scale               = 1e6,          # fabio_resc (k-litres) -> B litres
  y_lab               = "Biofuels from feedstocks in EU consumption (B litres)",
  accuracy            = 1)

ggsave(file.path(EU_PLOT_DIR, "EU_consumption_feedstock_mix_BF.svg"),
       p_eu_feed, width = 8, height = 10, dpi = 300)

################################################################################
##  8.  EU: base-100 index — ibif_total, LCIM_EQ_terrestrial,
##      LCIM_EQ_terrestrial_land_use, and biofuel consumption
################################################################################
## Same scope/aggregation logic as section 5 above ("EU IBIF ... broken down
## by FEEDSTOCK"): dt_tradeFeed filtered to commodity %in% BF_COMM and
## country_consumer %in% EU_iso3, summed across ALL biofuels and origins to a
## single EU (year) series per indicator. Each series is first put into its
## natural unit via indicator_meta$scale_factor (as m_ibif does for IBIF
## above), then indexed to base year = 100 so the 3 environmental indicators
## and biofuel consumption (very different units/magnitudes) can share one
## axis.
##
## NOTE: LCIM_EQ_terrestrial is the recomputed climate+acidification
## indicator built at the top of this script. LCIM_EQ_terrestrial_land_use is
## assumed to already exist as its own indicator in dt_tradeFeed (the
## sub-indicator explicitly excluded from the LCIM_EQ_terrestrial
## recombination) — adjust the string below if it's named differently in your
## data.

idx_indicators <- c("ibif_total", "LCIM_EQ_terrestrial", "LCIM_EQ_terrestrial_land_use")

idx_env <- dt_tradeFeed[
  indicator %in% idx_indicators & commodity %in% BF_COMM & country_consumer %in% EU_iso3,
  .(value = sum(value, na.rm = TRUE)),
  by = .(year, indicator)
]
if (!nrow(idx_env)) stop("No rows found for ", paste(idx_indicators, collapse = ", "),
                         " — check indicator names against dt_tradeFeed.")

## scale each indicator to its natural unit via indicator_meta (as m_ibif above)
scale_map <- setNames(indicator_meta$scale_factor, indicator_meta$indicator)
idx_env[, value := value / fifelse(is.na(scale_map[indicator]), 1, scale_map[indicator])]

## explicit legend labels for these 3 series (overrides indicator_meta$short_label)
idx_label_map <- c(
  "ibif_total"                   = "Pristine area loss equivalents",
  "LCIM_EQ_terrestrial"          = "Richness loss (excl. land use)",
  "LCIM_EQ_terrestrial_land_use" = "Richness loss from land use"
)
idx_env[, series := idx_label_map[indicator]]
idx_env <- idx_env[, .(year, series, value)]

## biofuel consumption, same EU / BF_COMM scope, B litres (as cons_totals in section 5)
idx_cons <- as.data.table(Y_summary)[
  comm_code %in% BF_COMM & target_country %in% EU_iso3,
  .(value = sum(value, na.rm = TRUE)), by = year
][, `:=`(value = value / 1e6, series = "Biofuel consumption")]
idx_cons <- idx_cons[, .(year, series, value)]

idx_dt <- rbindlist(list(idx_env, idx_cons), use.names = TRUE)

## index each series to base year = 100 (earliest year common to the data)
base_year <- min(idx_dt$year)
idx_dt[, base_val := value[year == base_year], by = series]
idx_dt <- idx_dt[!is.na(base_val) & base_val != 0]
idx_dt[, index := 100 * value / base_val]

## series order (consumption first, then env. indicators in idx_indicators order)
series_levels <- c("Biofuel consumption", unname(idx_label_map[idx_indicators]))
series_levels <- intersect(series_levels, unique(idx_dt$series))  # drop any missing series
idx_dt[, series := factor(series, levels = series_levels)]

idx_table <- dcast(idx_dt, year ~ series, value.var = "index")
setcolorder(idx_table, c("year", series_levels))
idx_table

idx_pal <- c(
  "Biofuel consumption"              = "black",
  "Pristine area loss equivalents"   = "#8c510a",
  "Richness loss (excl. land use)"   = "#2f7e07",
  "Richness loss from land use"      = "#8bc34a"
)
idx_pal <- idx_pal[series_levels]

p_eu_index <- ggplot(idx_dt, aes(year, index, color = series)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  geom_hline(yintercept = 100, linetype = "dotted", color = "grey50") +
  scale_color_manual(values = idx_pal, name = NULL) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(x = NULL, y = paste0("Index (", base_year, " = 100)")) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_minimal(base_size = 16) +
  theme(panel.grid.minor = element_blank(),
        legend.position  = "bottom",
        plot.title       = element_text(face = "bold", hjust = 0.5))

ggsave(file.path(EU_PLOT_DIR, "EU_index_footprint_vs_consumption_BF.png"),
       p_eu_index, width = 7, height = 6, dpi = 300)


################################################################################
##  8b. ONE-ROW GRID
################################################################################
##   (a) = p_eu_index (base-100 index), with ITS OWN legend below it.
##   (b) = EU consumption feedstock mix  |  (c) = EU IBIF by feedstock.
## (b) and (c) SHARE one item set (10 top + "Other" = 11) and ONE common legend.
library(cowplot)

## robust legend extractor (handles ggplot >= 3.5 guide-box naming)
extract_legend <- function(p) {
  leg <- tryCatch(cowplot::get_plot_component(p, "guide-box-bottom", return_all = FALSE),
                  error = function(e) NULL)
  if (is.null(leg) || inherits(leg, "zeroGrob")) leg <- cowplot::get_legend(p)
  leg
}

## --- shared 10-item set: top-6 IBIF feedstocks + next-4 consumption items -----
ibif_top6  <- feed_tot[seq_len(min(6, .N)), feedstock]
cons_rank  <- item_tot[order(-total), item]
cons_next4 <- setdiff(cons_rank, ibif_top6)
cons_next4 <- cons_next4[seq_len(min(4, length(cons_next4)))]

shared_items  <- c(ibif_top6, cons_next4)     # 10 named items
shared_levels <- c(shared_items, "Other")     # 11 with "Other"

## shared palette (keyed by ORIGINAL names) + compact display labels
shared_pal <- feedstock_color_map[shared_items]
shared_pal[is.na(shared_pal)] <- "grey75"
names(shared_pal) <- shared_items
shared_pal <- c(shared_pal, Other = "grey75")
shared_lab <- compact_lab(shared_levels)

## --- (b) IBIF by feedstock, rebuilt with the SHARED item set ------------------
b_dt <- dt_tradeFeed[
  indicator == "ibif_total" & commodity %in% BF_COMM & country_consumer %in% EU_iso3,
  .(value = sum(value, na.rm = TRUE)), by = .(year, feedstock)]
b_dt[, feedstock := relabel_feed(feedstock)]
b_dt[, feedstock := fifelse(feedstock %in% shared_items, feedstock, "Other")]
b_dt <- b_dt[, .(value = sum(value)), by = .(year, feedstock)]
b_dt[, value := value / m_ibif$scale_factor[1] / 1000]      # -> 1,000 MSA-loss·km²
b_dt[, feedstock := factor(feedstock, levels = shared_levels)]

## --- (c) consumption feedstock mix, rebuilt with the SHARED item set ----------
c_dt <- as.data.table(fabio_eu_mix)[
  , .(value = sum(fabio_resc, na.rm = TRUE)), by = .(year, item)]
c_dt[, item := mix_relabel(item)]
c_dt[, item := fifelse(item %in% shared_items, item, "Other")]
c_dt <- c_dt[, .(value = sum(value) / 1e6), by = .(year, item)]   # k-litres -> B litres
c_dt[, item := factor(item, levels = shared_levels)]

## --- shared typography / legend styling --------------------------------------
BASE_SZ <- 12                                  # single knob for all panel text
num <- scales::label_number(accuracy = 1)

theme_panel <- theme_minimal(base_size = BASE_SZ) +
  theme(panel.grid.minor = element_blank(),
        axis.text        = element_text(size = BASE_SZ, colour = "grey20"),
        axis.title       = element_text(size = BASE_SZ + 1),
        legend.position  = "none",
        plot.background  = element_rect(fill = "white", colour = NA),
        plot.margin      = margin(t = 22, r = 10, b = 5, l = 10))

theme_leg <- theme(
  legend.position       = "bottom",
  legend.direction      = "horizontal",
  legend.text           = element_text(size = BASE_SZ - 1),
  legend.key.size       = unit(0.55, "cm"),
  legend.spacing.x      = unit(0.30, "cm"),
  legend.background     = element_rect(fill = "white", colour = NA),
  legend.box.background = element_rect(fill = "white", colour = NA),
  legend.key            = element_rect(fill = "white", colour = NA))

## --- (b) and (c) panel bodies (no legend; identical shared fill scale) --------
p_b <- ggplot(b_dt, aes(year, value, fill = feedstock)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = shared_pal, name = NULL, drop = FALSE,
                    breaks = shared_levels, labels = shared_lab) +
  scale_x_continuous(breaks = scales::pretty_breaks(),
                     expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(labels = num, expand = expansion(mult = c(0, 0.10))) +
  labs(x = NULL, y = ibif_y_lab) +
  theme_panel

p_c <- ggplot(c_dt, aes(year, value, fill = item)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = shared_pal, name = NULL, drop = FALSE,
                    breaks = shared_levels, labels = shared_lab) +
  scale_x_continuous(breaks = scales::pretty_breaks(),
                     expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(labels = num, expand = expansion(mult = c(0, 0.10))) +
  labs(x = NULL, y = mix_y_lab) +
  theme_panel

## common (b)+(c) legend. IMPORTANT: extract it from a SYNTHETIC frame carrying
## one row per shared level, NOT from p_b. "Other, Waste" ("Annex IX Part A") has
## EU consumption volume but ~no IBIF footprint, so it never appears in plot (b)'s
## data (b_dt). Extracting the legend from p_b then leaves that key's fill
## aesthetic unpopulated: drop = FALSE keeps the break (so the LABEL shows) but the
## layer has no row to colour the swatch -> "Annex IX Part A" appears with a BLANK
## colour block. A synthetic frame with every level present guarantees each key is
## drawn with its shared_pal colour (Other, Waste -> #4D4D4D).
legend_dt <- data.table(x = 1L, y = 0,
                        lvl = factor(shared_levels, levels = shared_levels))
p_bc_legendsrc <- ggplot(legend_dt, aes(x, y, fill = lvl)) +
  geom_col() +
  scale_fill_manual(values = shared_pal, name = NULL, drop = FALSE,
                    breaks = shared_levels, labels = shared_lab) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  theme_leg
leg_bc <- extract_legend(p_bc_legendsrc)

## --- (a) index panel: match the (b)/(c) scale, keep its OWN legend below ------
## (a) has 4 series; a single column keeps the legend narrow (one series per row)
## so it no longer overflows its third of the row. `nrow = 3` was only a cap and,
## with 4 items, still rendered as 2 rows x 2 columns (the "too large" width).
p_a_full <- p_eu_index + theme_panel +
  guides(color = guide_legend(ncol = 1)) +
  theme_leg
leg_a <- extract_legend(p_a_full)
p_a   <- p_a_full + theme(legend.position = "none")

## --- assemble ----------------------------------------------------------------
top_row <- plot_grid(p_a, p_c, p_b, nrow = 1,
                     labels = c("(a)", "(b)", "(c)"), label_size = 16,
                     align = "h", axis = "tb")
legend_row <- plot_grid(leg_a, leg_bc, nrow = 1, rel_widths = c(1, 2))
grid_abc   <- plot_grid(top_row, legend_row, ncol = 1, rel_heights = c(1, 0.32)) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggsave(file.path(EU_PLOT_DIR, "EU_grid_index_ibif_mix_abc.png"),
       grid_abc, width = 14, height = 6.5, dpi = 300, bg = "white")




################### Extracting a table with EU results (by commodity, feedstock, indicators across IBIF, year, origin country) ################### 

dt_tradeFeed <- dt_tradeFeed[country_consumer != GBR & country_consumer %in% EU_iso3]
dt_tradeFeed <- dt_tradeFeed[indicator %like% "ibif" & year == 2022]
dt_tradeFeed <- dt_tradeFeed[, .(value = sum(value, na.rm = TRUE)), 
                             by = .(country_origin, indicator, feedstock, commodity, year)]
dt_tradeFeed_BP <- dt_tradeFeed_BP[country_consumer != GBR & country_consumer %in% EU_iso3]
dt_tradeFeed_BP <- dt_tradeFeed_BP[indicator %like% "ibif" & year == 2022]
dt_tradeFeed_BP <- dt_tradeFeed_BP[, .(value = sum(value, na.rm = TRUE)), 
                                   by = .(country_origin, indicator, year)]
dt_tradeFeed_BP[, commodity := "Bio-based polymers and building blocks"]
dt_tradeFeed_BP[, feedstock := "All"]

dt_result <- rbind(dt_tradeFeed, dt_tradeFeed_BP)

dt_result[regions, origin_continent := i.continent, on = .(country_origin = iso3c)]
dt_result[regions, origin_area := i.name, on = .(country_origin = iso3c)]
dt_result[country_origin == "GBR", origin_continent := "EUR"]
dt_result[, commodity := fcase(
  commodity == "c146", "Biogasoline",
  commodity == "c147", "Biodiesel",
  commodity == "c149", "Renewable diesel",
  default = commodity
)]

setcolorder(dt_result, c("country_origin", "origin_area", "origin_continent", "year"))

writexl::write_xlsx(
  dt_result,
  file.path("/home/mmondolfo/fabio_bfp/output/table", "dt_result.xlsx")
)




################################################################################
##  TEMP — German versions of (i) the (a)(b)(c) grid and (ii) the origin-continent
##  share figure. Paste at the end, run once, then comment out.
##  Reuses: idx_dt, idx_pal, b_dt, c_dt, shared_pal, shared_levels,
##          theme_panel, theme_leg, extract_legend, plot_eu_origin_share.
################################################################################

## --- German label maps --------------------------------------------------------
idx_lab_de <- c(
  "Biofuel consumption"            = "Verbrauch von Biokraftstoffen",
  "Pristine area loss equivalents" = "Verlust unber\u00fchrter Naturfl\u00e4chen",
  "Richness loss (excl. land use)" = "Verlust an Artenvielfalt (excl. Landnutzung)",
  "Richness loss from land use"    = "Verlust an Artenvielfalt durch Landnutzung"
)

feed_lab_de <- c(
  "Rapeseed Oil"     = "Raps\u00f6l",
  "Soybean Oil"      = "Soja\u00f6l",
  "Sugar Beet"       = "Zuckerr\u00fcben",
  "Palm Oil"         = "Palm\u00f6l",
  "Animal Fats"      = "Tierische Fette",
  "Sugar Cane"       = "Zuckerrohr",
  "Maize"            = "Mais",
  "Used Cooking Oil" = "Altspeise\u00f6l",
  "Other"            = "Sonstige",
  "Wheat"            = "Weizen",
  "Annex IX Part A"  = "Annex IX Part A"
)

de_lab <- function(x) {                       # English compact label -> German
  out <- feed_lab_de[x]; out[is.na(out)] <- x[is.na(out)]; unname(out)
}
shared_lab_de <- de_lab(compact_lab(shared_levels))   # keyed on shared_levels order

mix_y_lab_de  <- "Verbrauch von Biokraftstoffen (Mrd Liter)"
ibif_y_lab_de <- "Verlust unber\u00fchrter Naturfl\u00e4chen (1.000 MSA-loss\u00b7km\u00b2)"

## marginally smaller y-axis titles for the 3-panel grid only — panel (c)'s longer
## German label was touching the top of the axis area at theme_panel's BASE_SZ+1 (13pt)
theme_panel_de <- theme_panel + theme(axis.title = element_text(size = BASE_SZ + 0.3))

## --- (a) base-100 index, German legend ---------------------------------------
p_eu_index_de <- ggplot(idx_dt, aes(year, index, color = series)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  geom_hline(yintercept = 100, linetype = "dotted", color = "grey50") +
  scale_color_manual(values = idx_pal, name = NULL,
                     labels = unname(idx_lab_de[levels(idx_dt$series)])) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(x = NULL, y = paste0("Index (", base_year, " = 100)")) +
  theme_minimal(base_size = 16) +
  theme(panel.grid.minor = element_blank())

p_a_full_de <- p_eu_index_de + theme_panel_de +
  guides(color = guide_legend(ncol = 1)) + theme_leg
leg_a_de <- extract_legend(p_a_full_de)
p_a_de   <- p_a_full_de + theme(legend.position = "none")

## --- (b) consumption mix and (c) IBIF, German axis titles + legend -----------
p_c_de <- ggplot(c_dt, aes(year, value, fill = item)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = shared_pal, name = NULL, drop = FALSE,
                    breaks = shared_levels, labels = shared_lab_de) +
  scale_x_continuous(breaks = scales::pretty_breaks(),
                     expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(labels = num, expand = expansion(mult = c(0, 0.10))) +
  labs(x = NULL, y = mix_y_lab_de) + theme_panel_de

p_b_de <- ggplot(b_dt, aes(year, value, fill = feedstock)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = shared_pal, name = NULL, drop = FALSE,
                    breaks = shared_levels, labels = shared_lab_de) +
  scale_x_continuous(breaks = scales::pretty_breaks(),
                     expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(labels = num, expand = expansion(mult = c(0, 0.10))) +
  labs(x = NULL, y = ibif_y_lab_de) + theme_panel_de

legend_dt_de <- data.table(x = 1L, y = 0,
                           lvl = factor(shared_levels, levels = shared_levels))
leg_bc_de <- extract_legend(
  ggplot(legend_dt_de, aes(x, y, fill = lvl)) +
    geom_col() +
    scale_fill_manual(values = shared_pal, name = NULL, drop = FALSE,
                      breaks = shared_levels, labels = shared_lab_de) +
    guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
    theme_leg)

grid_abc_de <- plot_grid(
  plot_grid(p_a_de, p_c_de, p_b_de, nrow = 1,
            labels = c("(a)", "(b)", "(c)"), label_size = 16,
            align = "h", axis = "tb"),
  plot_grid(leg_a_de, leg_bc_de, nrow = 1, rel_widths = c(1, 2)),
  ncol = 1, rel_heights = c(1, 0.32)) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggsave(file.path(EU_PLOT_DIR, "EU_grid_index_ibif_mix_abc_DE.png"),
       grid_abc_de, width = 14, height = 6.5, dpi = 300, bg = "white")

## --- origin-continent share, German y title and "Other" -> "Sonstige" --------
## dt_tradeFeed is consumed by the table-export section above; reload if needed.
if (!"country_consumer" %in% names(dt_tradeFeed) ||
    !any(dt_tradeFeed$year %in% 2012:2014)) {
  dt_tradeFeed <- rbindlist(lapply(files_tradeFeed_BF, fread))
  .lc <- dt_tradeFeed[
    indicator %in% c("LCIM_EQ_terrestrial_climate", "LCIM_EQ_terrestrial_acidification"),
    .(value = sum(value, na.rm = TRUE)), by = id_cols_trade
  ][, indicator := "LCIM_EQ_terrestrial"]
  dt_tradeFeed <- rbindlist(list(dt_tradeFeed[indicator != "LCIM_EQ_terrestrial"], .lc),
                            use.names = TRUE)
  dt_tradeFeed <- dt_tradeFeed[country_consumer != GBR]
}

## rewrite the x-axis group labels in the manually drawn geom_text layers
de_origin_lab <- function(p, map = c("Other" = "Sonstige")) {
  for (i in seq_along(p$layers)) {
    d <- p$layers[[i]]$data
    if (is.data.frame(d) && "origin_cont" %in% names(d)) {
      v <- as.character(d$origin_cont); hit <- v %in% names(map)
      v[hit] <- unname(map[v[hit]]); d$origin_cont <- v
      p$layers[[i]]$data <- d
    }
  }
  p
}

p_share_de <- de_origin_lab(
  plot_eu_origin_share(dt_tradeFeed, regions,
                       years1 = 2012:2014, years2 = 2020:2022,
                       indicator_select = "ibif_total",
                       y_lab = ibif_y_lab_de))

ggsave(file.path(EU_PLOT_DIR,
                 "EU_origin_continent_share_ibif_total_avg_2012-14_2020-22_DE.png"),
       p_share_de, width = 9, height = 9, dpi = 300)
################################################################################