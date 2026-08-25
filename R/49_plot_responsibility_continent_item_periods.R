# =============================================================================
# 49_plot_responsibility_continent_item_periods.R
# 48's figure with 44's fill: the production- (PBA), consumption- (CBA) and
# value-added-based (VA) accounts as three ADJACENT stacked bars per region,
# stacked BY ITEM rather than by the origin x destination of the charged flow,
# and drawn for 43's TWO PERIODS instead of a single year.
#
#   [1] responsibility_<unit>_item_periods_<ind>_<vabase>_<alloc>.svg
#       facet_grid(period ~ panel): x = panel, three bars each | y = impact |
#       fill = item | rows = 2012-2014 and 2020-2022, on a FIXED y axis. This is
#       the headline figure and the only place the two periods are directly
#       comparable. Chains are POOLED here (40's `total`).
#
#   [2] responsibility_<unit>_item_<period>_<ind>_<vabase>_<alloc>.svg
#       one file PER PERIOD: facet_grid(fuel ~ panel), Total on top then one row
#       per chain, y FREE PER ROW -- i.e. exactly 44's grid, for a period mean
#       instead of a single year.
#
# WHY THE SPLIT INTO TWO FILES, AND NOT ONE fuel + period GRID -----------------
# 48 hit this first (see its note at [2]) and the reasoning carries over
# unchanged: `scales = "free_y"` frees an axis per ROW, so a grid with chain and
# period both on the rows would silently give the two periods of one chain
# DIFFERENT axes -- and "how did this move between the periods" is the entire
# question the figure exists to answer. Period on the rows at a fixed scale, or
# chain on the rows at a free scale; never both on the rows.
# The pooled figure therefore survives here as its own file, exactly as it does
# in 48 and unlike in 43/44/45 where it became the top row of the grid.
#
# WHY THIS COSTS NOTHING UPSTREAM, WHERE 48 COST TWO CSVs ----------------------
# 48 needs 41 and 42 to fold D[p, c] and H*[r, p] onto regions AT SOURCE,
# because a regional TRADE SPLIT cannot be rebuilt from country margins: summing
# `justice_domestic` over the countries of a region keeps the COUNTRY definition
# of domestic, and an intra-EU flow stays an export inside an EU panel.
# This figure has no diagonal term. PBA and CBA are the two margins of D and VA
# is a plain sum off 42's CSV, and summing a margin over a group of countries IS
# the margin of the folded matrix -- so the continental item figure is an exact
# aggregation of the country one, with nothing recomputed and nothing lost.
# That is 44's argument (THE UNIT, in its header) and it is why `unit` is a
# switch here rather than a new pair of CSVs in 41/42.
#
# WHAT THE PERIODS COST ------------------------------------------------------
# 44 reads ONE Leontief inverse. This script reads one PER YEAR of PERIODS --
# six, for 2012-2014 vs 2020-2022 -- because D must be rebuilt at node
# resolution in each year before it can be averaged. They are read, used and
# dropped one at a time, so the peak memory is 44's, but the runtime is roughly
# six times 44's. Nothing else changes: the VA account, the accounts CSV and the
# cross-checks all come from files that already carry `year`.
#
# PERIOD MEANS, NOT SUMS ------------------------------------------------------
# 43's PERIODS and 40's period_mean(), so a period short of a year stays
# comparable with a complete one. Every quantity here is additive in the year,
# so averaging the item rows and averaging the accounts are the same operation
# and the conservation identity survives the average. The identity checks are
# nevertheless run BEFORE it, per year, where a defect cannot be diluted by two
# clean years next to it.
#
# THE KEEP-SET SPANS BOTH PERIODS --------------------------------------------
# An item is kept (given its own colour) if it clears the threshold in ANY
# account x fuel x PERIOD cell, and the keep-set is then applied to the whole
# table. Taking the threshold on the period-pooled total instead would let an
# item that is 2.5% in 2020-2022 and 1.5% in 2012-2014 be coloured in one row
# and grey in the other, and the reader would see a compositional shift that is
# really a threshold artefact -- in the figure whose only subject IS the shift
# between the two rows. Including `period` among the cell keys makes the
# keep-set the UNION over periods, which is the conservative direction: it can
# only lengthen the legend, never move a segment between a colour and the grey.
#
# ITEM SEMANTICS -- read before interpreting the stacks -----------------------
#   PBA / CBA   the PRODUCING node -- the item whose production exerts the
#               pressure. PBA books it to the producing country, CBA to the
#               consuming one.
#   VA          the sector that CAPTURES the value -- the item that earns, not
#               the item that emits.
# Each account sums to the SAME grand total; only its distribution moves.
#
# THE ITEM GRAMMAR IS COPIED FROM 44 ------------------------------------------
# The chain map, the stage classification, the shading and the "-> Other"
# pooling are 44's, duplicated in the block below rather than shared, on the
# same grounds that 48 duplicates 45's rather than abstracting it. THE COST:
# recolour a feedstock or move a comm_code between chains in 44 and the same
# edit must be made here, or one item appears in two colours across the two
# figures. The block is kept byte-identical to 44's so that `diff` catches it.
# The HUES themselves are not copied -- they come from 19 via 40, in both.
#
# READS  <MRIO>/losses/{X.rds, Y.rds, fd_labels.csv, <yr>_L_<alloc>.rds}  (13,14)
#          -- one L per year of PERIODS
#        <base>/{E.rds, io_labels.csv}                                    (16,12_b)
#        inst/items_full_bcp.csv                                          (item, comm_group)
#        VA_RESP_CSV (42) | HDI_CSV (41, cross-check)
# WRITES output/plot/  the two stems above, plus
#        captions_<unit>_item_periods_<ind>_<alloc>_periods.txt
#        No ORDER_CSV: that file is country-keyed and stamped with PLOT_YEAR,
#        and 45 reads it for a single-year figure. Nothing here belongs in it.
#
# RUN: Rscript R/49_plot_responsibility_continent_item_periods.R   (after 42)
# =============================================================================

# --- portable repo root: FABIO_BFP_ROOT override, else walk up to the marker -
fabio_root <- Sys.getenv("FABIO_BFP_ROOT", unset = "")
if (!nzchar(fabio_root)) {
  fabio_root <- getwd()
  while (!file.exists(file.path(fabio_root, "R", "00_system_variables.R")) &&
         dirname(fabio_root) != fabio_root) fabio_root <- dirname(fabio_root)
  if (!file.exists(file.path(fabio_root, "R", "00_system_variables.R")))
    stop("Repo root not found above ", getwd(), " - set FABIO_BFP_ROOT or run from inside the repo.")
}
setwd(fabio_root)

library(data.table)
library(Matrix)
library(ggplot2)
library(scales)
library(svglite)
# ggpattern hatches the fuel segments. Fail with a useful message rather than an
# opaque "could not find function geom_col_pattern".
if (!requireNamespace("ggpattern", quietly = TRUE))
  stop("[49] package 'ggpattern' is required to hatch the fuel segments.\n",
       "     install.packages('ggpattern')   # pulls in 'gridpattern'")
library(ggpattern)

source("R/40_responsibility_shared.R")      # the run definition, PERIODS, period_mean
source("R/00_responsibility_helpers.R")     # country_grid(), build_D()
SCRIPT <- "49"
run_banner()
message(sprintf(">>> [49] periods = %s",
                paste(sprintf("%s (%s)", names(PERIODS),
                              vapply(PERIODS, function(y) paste(range(y), collapse = "-"),
                                     character(1))), collapse = " vs ")))

# =============================================================================
# figure switches
# =============================================================================
# THE PANEL UNIT. "continent" is what this script was written for -- it is 48's
# figure re-filled. "country" is left available because every step below takes
# the panel column as an argument and never learns which unit it is working at,
# exactly as 44 does; set TOP_N to something finite if you switch, or the figure
# grows a panel per country in the model.
UNIT   <- "continent"     # "continent" | "country"
TOP_N  <- Inf             # Inf = every panel of the unit. Ranked on PANEL_ORDER.
DROP_PANELS <- "Unknown"  # continent_of()'s fallback bucket; should be empty

# Left-to-right order of the panels, scored on the LAST period and pooled over
# chains, so the two period rows and the chain rows all use ONE order and a
# region sits in the same column in every figure of the set.
# "PBA" matches 48's REGION_ORDER default. Two continent figures in one document
# with different left-to-right axes is a reading error waiting to happen: change
# this and 48's together, or neither.
PANEL_ORDER <- "PBA"      # "PBA" | "CBA" | "VA" | "max" | "mean" | "alpha"

BY_PERIOD_CHAINS <- TRUE  # also write figure [2], one file per period

# What falls into the grey "Other". Two crowding budgets, kept SEPARATE on
# purpose: 3% of one chain is a different bar from 2% of the pool. Raise
# MIN_ITEM_SHARE_BYFUEL to thin the chain rows; do not push it past ~0.03-0.04.
MIN_ITEM_SHARE_POOLED <- 0.02    # Total row: below this share of a cell        -> "Other"
MIN_ITEM_SHARE_BYFUEL <- 0.03    # chain rows: below this share of a cell       -> "Other"

# What the "below x% -> Other" share is a share OF. For a set that shows EVERY
# panel of its unit -- which is what TOP_N = Inf gives -- "world" and "set" are
# the SAME denominator, because the shares group by the cell and sum over panels
# either way. Keep "world" there so the caption says the simpler true thing and
# the grey "Other" stays the same category as 44's world-scoped figures'.
ITEM_SCOPE <- "world"     # "world" | "set"

# --- type size and furniture -------------------------------------------------
# Same value as 44/45/48: the eight region columns are read at panel width, and
# the item legend below them scales with this through KEY_WIDTH_IN.
BASE_SIZE    <- 16
SHOW_TITLES  <- TRUE      # FALSE -> the text goes to captions_*.txt instead
SHOW_CAPTION <- TRUE

# Same separation devices and defaults as 44/45/48.
SEPARATOR  <- "border"    # "border" | "band" | "both" | "none"
SEP_COLOUR <- "grey89"    # ABOVE every fill in lightness
BAND_FILL  <- "grey96"
PANEL_GAP  <- 5           # pt between panels

# One legend for every figure this script writes, or one per figure? FALSE trims
# each legend to the items its own figure draws -- the pooled figure [1] and the
# per-period grids [2] then differ by the keys only the chain rows need.
LEGEND_KEEP_ALL <- FALSE

ACCOUNTS <- c("PBA", "CBA", "VA")     # bar order within each panel

UNIT_COL  <- c(country = "iso3c",   continent = "continent")
UNIT_ONE  <- c(country = "country", continent = "continent")
UNIT_MANY <- c(country = "countries", continent = "continents")
if (!UNIT %in% names(UNIT_COL))
  stop("[49] UNIT must be 'country' or 'continent', not '", UNIT, "'.")
PCOL <- UNIT_COL[[UNIT]]

TOTAL_LAB <- chain_labeller(TOTAL_KEY)          # "Total", the pooled chain's label
LAST      <- names(PERIODS)[length(PERIODS)]
YEARS     <- sort(unique(unlist(PERIODS, use.names = FALSE)))

# =============================================================================
# THE ITEM GRAMMAR -- COPIED FROM 44. KEEP THE TWO IN SYNC.
# =============================================================================
# Everything from here to the end of this block is 44's, VERBATIM apart from the
# script number in its messages. It is copied rather than shared because the
# alternative -- a third file, or a new section of 40 -- was judged not to earn
# itself for one extra figure; 48 stands in the same relation to 45 and duplicates
# a comparable amount. The cost is real and is stated here rather than discovered
# later: if a feedstock is recoloured, a comm_code moves chain, or a stage
# classification changes IN 44, the same edit must be made HERE, or the two
# figures will show one item in two colours -- which 44's own header calls out as
# the failure that makes a pair of figures unreadable against each other.
# The block is kept byte-identical to 44's so `diff` on the two files is a
# reliable way to check that. Do not reformat it, and do not "improve" one copy.
#
# NOTE ON 19. The HUES are not copied: fuel_colors, feedstock_color_map,
# oilcrop_reserve and starchy_reserve all come from 19_plot_definitions.R via 40,
# in this script exactly as in 44. What is copied is the derivation from a
# feedstock hue to a SEGMENT colour -- the chain map, the stage classification and
# the shading -- which exists only in 44.
# =============================================================================
`%||%` <- function(a, b) if (is.null(a)) b else a

# --- chains: group the stages of one feedstock so they stack together --------
# comm_codes per inst/items_full_bcp.csv; anything unlisted falls to "Other".
CHAIN_CODES <- list(
  "Biofuels"            = c("c146", "c147", "c149", "c150", "c151"),
  "Palm"                = c("c028", "c064", "c073", "c074", "c086"),   # fruit, kernels, oils, cake
  "Soy"                 = c("c021", "c068", "c081"),
  "Rapeseed"            = c("c024", "c071", "c084"),
  "Sunflower"           = c("c023", "c070", "c083"),
  "Maize"               = c("c004", "c079"),                           # grain + germ oil
  "Wheat"               = c("c002"),
  "Sugar"               = c("c015", "c016", "c065", "c066", "c142"),   # cane, beet, sugar, molasses
  "Cereals, other"      = c("c001", "c003", "c005", "c006", "c007", "c008", "c078", "c141"),
  "Roots & tubers"      = c("c010", "c011", "c012", "c013", "c014"),
  "Coconut"             = c("c026", "c075", "c087"),
  "Groundnut"           = c("c022", "c069", "c082"),
  "Cotton"              = c("c025", "c063", "c072", "c085", "c095"),
  "Other oil crops"     = c("c027", "c029", "c076", "c077", "c088", "c089", "c143", "c144"),
  "Livestock & grazing" = c("c061", "c062", sprintf("c%03d", 96:108)),
  "Waste & UCO"         = c("c119", "c145", "c901")
)

# Items with no colour of their own in feedstock_color_map borrow their chain's
# feedstock colour (and are shaded by stage). Keyed by comm_code.
ANCHOR_FEEDSTOCK <- c(
  c028 = "Palm Oil",             c064 = "Palmkernel Oil",       c086 = "Palmkernel Oil",
  c021 = "Soyabean Oil",         c081 = "Soyabean Oil",
  c024 = "Rape and Mustard Oil", c084 = "Rape and Mustard Oil",
  c023 = "Sunflowerseed Oil",    c083 = "Sunflowerseed Oil",
  c026 = "Coconut Oil",          c087 = "Coconut Oil",
  c025 = "Cottonseed Oil",       c063 = "Cottonseed Oil",       c085 = "Cottonseed Oil",
  c065 = "Sugar cane",           c066 = "Sugar cane"            # raw sugar: cane-dominated
)

# Items that are neither a feedstock nor a stage of one -- drawn from 19's reserve
# slots so they cannot collide with anything already fixed.
# COLLISION NOTE (CIEDE2000, the metric 19's palette was built on):
#   * Grazing sits in oilcrop_reserve[1] (maroon #9B3649), not [2] (tan #B87340):
#     the tan is dE 11.7 from DARKENED Rape and Mustardseed (#9B6910), below the
#     ~15 dE that keeps two keys apart at 10pt, and rape is a lead biodiesel
#     feedstock so the two are frequently adjacent. Maroon is dE 34.8 from it,
#     nearest neighbour anywhere 18.4 (Cassava).
#   * Cattle needs an entry here or build_palette() falls it back to the grey
#     "Other" catch-all -- an EXACT match (dE 0), i.e. Cattle reads as "pooled".
#     Its hide-brown is kept LOCAL to this script rather than added to 19's
#     shared pool, which get_feedstock_palette indexes into for other figures:
#     dE 13.6 from the grey Other, 27.0 from Grazing (both livestock, so a
#     related-but-distinct warm pair is intentional).
EXTRA_ITEM_COLORS <- c(Grazing        = unname(oilcrop_reserve[1]),
                       Cattle         = "#8C7B6B",
                       `Fodder crops` = unname(starchy_reserve[1]))

# 19_01b's relabel: c145's FAOSTAT name is unreadable, and it IS the UCO node.
UCO_PATTERN <- "^Animal or vegetable fats and oils"

# processing stage, from comm_group: 1 = primary (where the impact lands),
# 2 = processed (where the value lands), 3 = residual. Sets the shade.
STAGE_1 <- c("Oil crops", "Cereals", "Sugar crops", "Roots and tubers",
             "Fibre crops", "Fodder crops", "Grazing", "Live animals")
STAGE_2 <- c("Vegetable oils", "Sugar, sweeteners", "Alcohol", "Meat", "Milk", "Biofuels")

SHADE <- 0.35                    # how far a primary crop is darkened / a by-product lightened

# --- the fuel hatch (ggpattern) ---------------------------------------------
# pattern_spacing is the parameter to touch. 0.03 puts ~10 stripes across a fuel
# segment and renders in seconds. Do NOT reach for a much finer value: at 0.0025
# ggpattern generates an enormous stripe field per patterned grob, clips each to
# its segment, and ggsave() grinds for minutes before svglite has to serialise it
# all. If the stripes look too coarse, step DOWN gently (0.03 -> 0.02 -> 0.015)
# and watch the render time.
PATTERN_SPACING <- 0.03          # ~10 stripes per fuel segment
PATTERN_DENSITY <- 0.40          # fraction of each stripe period that is ink
PATTERN_ANGLE   <- 45            # no feedstock has any texture, so any angle works
PATTERN_INK     <- "white"       # reads on all three fuel fills

# --- the line between stacked segments ---------------------------------------
# NA = none, the segments meet directly. The white hairline was doing two jobs:
# separating adjacent segments, and separating a segment from the panel
# background. Only the first is lost, and the palette was built for it -- every
# pair within a category is >= 18 dE apart. The exception is a chain's OWN two
# stages: they are the same hue by design, one darkened and one lightened by
# SHADE, and the stack puts them next to each other (crop -> processed). If a
# soy or palm pair now reads as one block, raise SHADE (0.35 -> 0.45) rather than
# bringing the hairline back.
SEG_BORDER     <- NA             # e.g. "white" or "grey30" to restore it
SEG_BORDER_LWD <- 0.25

# --- how wide ONE panel has to be, per unit ----------------------------------
# A continent name is not a 3-character ISO code: "South America" needs either a
# wider panel or a wrapped strip, and at the country allowance it collides with
# its neighbour. So the per-panel allowance follows the UNIT, and the strip label
# is wrapped as well.
PANEL_W_BYFUEL <- c(country = 0.78, continent = 1.25)
# Characters per line of a strip label before it wraps. Countries are never
# wrapped (a 3-letter code has nothing to wrap), continents are.
STRIP_WRAP <- c(country = 40L, continent = 11L)

# Strip labels: label_wrap_gen() rather than an abbreviation table -- an
# abbreviation is one more thing that can disagree with 48's panel names.
strip_labeller <- function(unit) label_wrap_gen(width = STRIP_WRAP[[unit]])

# legend_rows: ~2.4in per key is measured off the longest labels ("Rape and
# Mustard Oil", "Fats, Animals, Raw") AT 11pt, so it scales with BASE_SIZE --
# otherwise raising the type quietly overflows the legend row instead of adding
# a row.
KEY_WIDTH_IN <- 2.4 * BASE_SIZE / 11
legend_rows <- function(n_keys, width_in)
  max(3L, as.integer(ceiling(n_keys / max(3, floor(width_in / KEY_WIDTH_IN)))))

# --- colours: 19's feedstock palette, shaded by processing stage -------------
#   item in feedstock_color_map      -> that colour, EXACTLY (Palm Oil, Sugar cane)
#   primary crop of such a feedstock -> the same colour, DARKENED
#   cake / by-product                -> the same colour, LIGHTENED
#   non-feedstock items (Grazing)    -> 19's reserve slots
# One hue = one chain; the shade gives the processing stage: dark = where the
# impact is, light = where the value is.
shade <- function(hex, amount) {          # amount < 0 darkens, > 0 lightens
  m <- col2rgb(hex) / 255
  m <- m + (ifelse(amount < 0, 0, 1) - m) * abs(amount)
  rgb(m[1, ], m[2, ], m[3, ])
}
build_palette <- function(lvl) {
  cols <- vapply(seq_len(nrow(lvl)), function(k) {
    it <- as.character(lvl$item[k]); cc <- lvl$comm_code[k]
    if (it %in% names(fuel_colors))          return(unname(fuel_colors[it]))
    if (it %in% names(feedstock_color_map))  return(unname(feedstock_color_map[it]))
    if (it %in% names(EXTRA_ITEM_COLORS))    return(unname(EXTRA_ITEM_COLORS[it]))
    if (cc %in% names(ANCHOR_FEEDSTOCK)) {   # a stage of a mapped feedstock: same hue, shaded
      base <- unname(feedstock_color_map[ANCHOR_FEEDSTOCK[[cc]]])
      return(shade(base, if (lvl$stage[k] == 1L) -SHADE else SHADE))
    }
    warning("[49] no colour for '", it, "' (", cc, ") -- grey. Map it in ANCHOR_FEEDSTOCK ",
            "or EXTRA_ITEM_COLORS.")
    unname(feedstock_color_map[["Other"]])
  }, character(1))
  setNames(cols, as.character(lvl$item))
}

# --- stack + legend order: fuels at the base, then chain, then stage ---------
make_levels <- function(p) {
  lvl <- unique(p[, .(item, comm_code, chain, stage)])
  lvl[p[, .(size = sum(abs(value))), by = item], size := i.size, on = "item"]
  lvl[lvl[, .(cs = sum(size)), by = chain], cs := i.cs, on = "chain"]  # big chains first
  lvl[, chain_rank := fifelse(chain == "Biofuels", Inf,                # fuels always at the base
                              fifelse(chain == "Other", -Inf, cs))]    # grey catch-all always last
  setorder(lvl, -chain_rank, stage, -size)                             # within a chain: crop -> processed
  lvl[]
}

# --- pool the small items into "Other" ---------------------------------------
# An item is KEPT if it reaches the threshold in ANY CELL of the figure; `cell`
# names what a cell is. The keep-set is therefore a UNION over cells, which is
# what makes the relabelling consistent across every panel: an item is coloured
# everywhere or grey everywhere, never coloured in one facet and pooled in the
# next.
#
# 44 CALLS THIS WITH cell = (account, fuel). THIS SCRIPT ADDS `period` -- see
# THE KEEP-SET SPANS BOTH PERIODS in the header. That call-site difference is
# intended and is the one place the two scripts' use of this function diverges;
# the function itself is unchanged.
#
# `scope` is the data the SHARES are computed on; `dat` is the data relabelled.
# Keeping them separate is what lets one figure take its shares against the world
# and another against the panels it shows, without either one deciding what the
# other draws.
# `min_share` is either ONE number, applied to every cell, or a vector NAMED by
# the levels of `fuel` -- the second form gives the pooled Total row its own
# threshold. It is carried as a column rather than recycled, so a cell can never
# be matched against the wrong entry by position.
pool_items <- function(dat, cell, min_share, scope = dat, tag = "") {
  share <- scope[, .(v = sum(abs(value))), by = c(cell, "item")][, share := v / sum(v), by = cell]
  if (length(min_share) == 1L) {
    share[, thr := as.numeric(min_share)]
  } else {
    if (!"fuel" %in% cell)
      stop("[49] pool_items(): a per-row min_share needs `fuel` among the cell keys.")
    share[, thr := unname(min_share[as.character(fuel)])]
    if (anyNA(share$thr))
      stop("[49] pool_items(): no threshold for fuel level(s) ",
           paste(sort(unique(as.character(share$fuel[is.na(share$thr)]))), collapse = ", "))
  }
  keep <- share[share >= thr, unique(item)]
  out  <- copy(dat)
  out[!item %in% keep, `:=`(item = "Other", chain = "Other", stage = 3L, comm_code = "Other")]
  message(sprintf("[49] %-11s %-18s %2d items kept (>= %s of a cell), %d pooled as 'Other'.",
                  tag, paste0("[", paste(cell, collapse = " x "), "]"), length(keep),
                  paste(sprintf("%.0f%%", 100 * sort(unique(share$thr))), collapse = "/"),
                  uniqueN(share$item) - length(keep)))
  out
}

# --- WHY WE DRAW THE LEGEND KEY OURSELVES (key_glyph) ------------------------
# ggpattern does not render its hatch into the legend keys under
# geom_col_pattern + svglite: routing the pattern to the fill guide via
# guide_legend(override.aes = list(pattern = ..)) still emits SOLID keys, no
# stripe geometry at all, which leaves the three fuels indistinguishable from the
# feedstocks they sit dE 4-7 from. So the swatch is painted with plain grid grobs,
# which svglite serialises cleanly: every key is a filled rect edged like the bars
# (SEG_BORDER, so NA leaves it unedged), and the three FUEL keys (detected by
# their exact fill -- the collisions are close but never identical hexes) get the
# same 45-deg white hatch as the bars.
FUEL_HEX <- toupper(unname(fuel_colors))          # the three fuel fills

draw_key_fuelhatch <- function(data, params, size) {
  fill   <- data$fill %||% "grey50"
  swatch <- grid::rectGrob(gp = grid::gpar(fill = fill, col = SEG_BORDER,
                                           lwd = SEG_BORDER_LWD * .pt))  # follows the bars
  if (toupper(fill) %in% FUEL_HEX) {                                  # fuels only -> hatch
    off   <- seq(-1, 1, by = 0.28)                                   # ~7 stripes across the key
    hatch <- grid::segmentsGrob(                                     # 45 deg, as PATTERN_ANGLE
      x0 = grid::unit(off,     "npc"), y0 = grid::unit(0, "npc"),
      x1 = grid::unit(off + 1, "npc"), y1 = grid::unit(1, "npc"),
      gp = grid::gpar(col = PATTERN_INK, lwd = 0.9),
      vp = grid::viewport(clip = "on"))                              # keep stripes inside the cell
    grid::grobTree(swatch, hatch)
  } else swatch
}
# =============================================================================
# END OF THE BLOCK COPIED FROM 44
# =============================================================================

# =============================================================================
# 1. inputs that do not depend on the year
# =============================================================================
X     <- readRDS(need(file.path(MRIO_PATH, "losses", "X.rds"), "13/14"))
Y     <- readRDS(need(file.path(MRIO_PATH, "losses", "Y.rds"), "13"))
E     <- readRDS(need(file.path(base_path, "E.rds"), "16"))
io    <- fread(need(file.path(base_path, "io_labels.csv"), "12_b"))
fd    <- fread(need(file.path(MRIO_PATH, "losses", "fd_labels.csv"), "13"))
items <- fread(need("inst/items_full_bcp.csv", "00_update_items_list.R"))[
  , .(comm_code, item, comm_group)]
stopifnot(all(c("iso3c", "comm_code", "item") %in% names(io)), "iso3c" %in% names(fd))

# item table: readable UCO name, chain, stage -- the whole plot keys off these
items[grepl(UCO_PATTERN, item), item := "Used Cooking Oil"]
items[, chain := "Other"]
for (ch in names(CHAIN_CODES)) items[comm_code %in% CHAIN_CODES[[ch]], chain := ch]
items[, stage := fifelse(comm_group %in% STAGE_1, 1L,
                         fifelse(comm_group %in% STAGE_2, 2L, 3L))]

# The country universe of BOTH axes of D. io and fd are year-invariant, so this
# is built once and reused by every year -- and that is also what guarantees the
# six years share one grid and can be averaged cell by cell.
grid   <- country_grid(io, fd)
io_key <- paste0(io$iso3c, "_", io$comm_code)

# ALIGNMENT GUARD. The math below is positional: as.numeric() strips names, so R
# lines up E, X, B and `io` by INDEX. If any artefact was rebuilt against a
# differently-ordered io_labels.csv, one item's extension is divided by another
# item's output and the impact lands on the WRONG item -- silently, and this is a
# by-item figure. Check it loudly before computing anything. (Mirrors 41 and 44.)
if (!is.null(rownames(X)) && !identical(rownames(X), io_key))
  stop("[49] X.rds rows != io_labels grid -- extensions would be misattributed. Re-run 16.")

# =============================================================================
# 2. the bilateral matrices, ONE YEAR AT A TIME
# =============================================================================
# ONE D PER FUEL PER YEAR. D is linear in Y_bf, so the pooled matrix is sum_g D_g
# and nothing is computed twice. build_D masks Y to the group's own rows
# (diag(sel) %*% Yc) rather than subsetting B; because B is sparse the product
# only ever touches the columns of B at those few biofuel nodes.
#
# B is read INSIDE the loop and dropped at the end of each iteration. A list of
# six Leontief inverses held at once is the one thing that would make this script
# fail where 44 succeeds, and it buys nothing: each year's D depends on that
# year's B alone.
acc_y   <- list()
have_yr <- integer(0)
for (yr in YEARS) {
  yc    <- as.character(yr)
  Lfile <- file.path(MRIO_PATH, "losses", paste0(yc, "_L_", allocation, ".rds"))
  # A year missing from the run is a MESSAGE, not an error: period_mean() divides
  # by the years actually present, so a short period still yields a comparable
  # mean and says so on the console. A period with NO years is caught below.
  if (!yc %in% colnames(X) || is.null(Y[[yc]]) || is.null(E[[yc]])) {
    message("[49] year ", yc, " absent from X / Y / E -- skipped."); next
  }
  if (!file.exists(Lfile)) {
    message("[49] no Leontief inverse for ", yc, " (", basename(Lfile), ") -- skipped."); next
  }
  if (!STRESSOR %in% rownames(E[[yc]]))
    stop("[49] E[['", yc, "']] has no '", STRESSOR, "' row -- re-run 16.")
  if (!is.null(colnames(E[[yc]])) && !identical(colnames(E[[yc]]), io_key))
    stop("[49] E cols (", yc, ") != io_labels grid -- extensions would be misattributed. Re-run 16.")
  
  message(sprintf("[49] %s: building D ...", yc))
  n_before <- length(acc_y)
  B  <- readRDS(Lfile)
  Xi <- as.vector(X[, yc])
  f  <- as.numeric(E[[yc]][STRESSOR, ]) / Xi; f[!is.finite(f)] <- 0
  Yc <- Y[[yc]] %*% grid$S_fd                                    # N x consumer
  
  for (g in names(biofuel_groups)) {
    sel <- as.numeric(io$comm_code %in% biofuel_groups[[g]])
    if (!sum(sel)) { warning("[49] no node for fuel '", g, "' -- skipped."); next }
    
    # producing node x consuming country, at the node resolution the stacks need
    D <- build_D(f, B, Yc, sel, countries = grid$countries)
    if (sum(D) == 0) {
      warning("[49] '", g, "' carries no ", STRESSOR, " in ", yc, " -- skipped."); next
    }
    
    # PBA: impact at the producing node i -- rowSums(D_g)
    pba <- data.table(iso3c = io$iso3c, comm_code = io$comm_code, value = rowSums(D)
    )[value != 0, .(value = sum(value)), by = .(iso3c, comm_code)][, account := "PBA"]
    
    # CBA: same impact, routed to the consumer of the fuel -- colSums per source item
    Dc  <- rowsum(D, group = io$comm_code)                       # source item x consumer
    cba <- data.table(comm_code = rep(rownames(Dc), times = ncol(Dc)),
                      iso3c     = rep(colnames(Dc), each  = nrow(Dc)),
                      value     = as.vector(Dc))[value != 0][, account := "CBA"]
    
    acc_y[[paste(yc, g)]] <- rbindlist(list(pba, cba), use.names = TRUE)[
      , `:=`(fuel = g, year = yr)]
    rm(D, Dc, pba, cba)
  }
  # Only a year that actually CONTRIBUTED rows counts as present. period_mean()
  # divides by the years it finds in the data, so a year whose every chain was
  # skipped is not in that divisor either -- and the period note printed under
  # the figure must name the same years the arithmetic used.
  if (length(acc_y) > n_before) have_yr <- c(have_yr, yr)
  rm(B, Yc, f, Xi); gc(verbose = FALSE)
}
if (!length(acc_y))
  stop("[49] the biofuel chains carry no ", STRESSOR, " in any year of ",
       paste(names(PERIODS), collapse = " / "), ".")
pc <- rbindlist(acc_y, use.names = TRUE)
rm(acc_y)

# every period must retain at least one year, or a row of the figure is a lie
for (p in names(PERIODS))
  if (!length(intersect(PERIODS[[p]], have_yr)))
    stop("[49] period '", p, "' has no usable year -- it would be drawn empty.")

# =============================================================================
# 3. VA: read per fuel and year from 42 (its CSV already carries both)
# =============================================================================
# 42 writes one file per VA variant AND tags the rows with `va_variant`. 40's
# VA_RESP_CSV already resolves to the variant VA_VARIANT names, so the filter is
# normally a no-op -- but 48 filters it explicitly and this script does too: if a
# file ever carries both variants, an unfiltered read DOUBLES the VA account
# silently, and the conservation check below would then blame the PBA/CBA side.
# (44 reads the same file without this filter. If the check at [4] ever fires on
# VA alone, that is the first thing to look at.)
va <- fread(need(VA_RESP_CSV, "42"))
if ("va_variant" %in% names(va)) {
  nv <- uniqueN(va$va_variant)
  va <- va[va_variant == VA_VARIANT]
  if (nv > 1)
    message("[49] ", basename(VA_RESP_CSV), " carries ", nv,
            " VA variants; filtered to '", VA_VARIANT, "'.")
}
va <- va[year %in% YEARS & biofuel_group %in% names(biofuel_groups),
         .(value = sum(va_resp)),
         by = .(year, iso3c = va_iso3c, comm_code = va_comm_code,
                fuel = biofuel_group)][value != 0][, account := "VA"]
if (!nrow(va))
  stop("[49] no VA responsibility rows for ", paste(names(PERIODS), collapse = " / "),
       " in ", VA_RESP_CSV)

d_y <- rbindlist(list(pc, va), use.names = TRUE)
rm(pc, va)

# =============================================================================
# 4. consistency checks -- PER YEAR, before the period average
# =============================================================================
# Averaging is a sum, so a defect in one year survives into its period diluted by
# the other two. Both checks therefore run at YEAR resolution, where a single bad
# year is visible as itself.

# (1) the three accounts re-attribute ONE total -- per year and fuel.
# BRACED, though each body is a single call: an unbraced top-level `else` parses
# under source()/Rscript but NOT when the lines are pasted at the console, where
# R evaluates the `if` the moment it is complete and the `else` then begins a
# statement of its own. 44 gets away with the unbraced form at its line 681
# because that one sits inside `if (file.exists(HDI_CSV)) { ... }`. These checks
# are the part of the script you most want to paste while debugging a run, so
# they are braced even where the body does not need it.
tot <- d_y[, .(total = sum(value)), by = .(year, fuel, account)]
bad <- tot[, .(dev = (max(total) - min(total)) / max(abs(total))), by = .(year, fuel)][dev > TOL]
if (nrow(bad)) {
  warning("[49] the accounts do not share one total within: ",
          paste(sprintf("%d/%s (rel. %.3g)", bad$year, bad$fuel, bad$dev), collapse = ", "))
} else {
  message("[49] the three accounts share one total in every year x fuel.")
}

# (2) PBA/CBA rebuilt here must reproduce the accounts CSV, at
#     (year x account x iso3c x fuel) resolution.
#     This stays at COUNTRY resolution even though the figure draws continents,
#     and it is the stronger test for it: the continental bars are sums of
#     exactly these rows, so a country-level agreement implies a continental one
#     while the reverse does not -- two countries wrong in opposite directions
#     inside one continent cancel in a continent-level check. Do not "upgrade"
#     this to the drawn unit; that would weaken it.
if (file.exists(HDI_CSV)) {
  acc <- fread(HDI_CSV)[year %in% YEARS & biofuel_group %in% names(biofuel_groups)]
  ref <- rbindlist(list(
    acc[, .(ref = sum(production_based)),  by = .(year, iso3c, fuel = biofuel_group)][, account := "PBA"],
    acc[, .(ref = sum(consumption_based)), by = .(year, iso3c, fuel = biofuel_group)][, account := "CBA"]
  ), use.names = TRUE)
  cmp <- merge(d_y[account != "VA", .(new = sum(value)),
                   by = .(year, account, iso3c, fuel)],
               ref, by = c("year", "account", "iso3c", "fuel"), all = TRUE)
  cmp[is.na(new), new := 0][is.na(ref), ref := 0]
  dev <- max(abs(cmp$new - cmp$ref)) / max(abs(cmp$ref), 1)
  if (dev > TOL) {
    warning(sprintf("[49] PBA/CBA differ from the accounts CSV per year x fuel (max rel. %.3g) -- same switches?", dev))
  } else {
    message("[49] PBA/CBA match the accounts CSV, per year and fuel.")
  }
} else {
  message("[49] no accounts CSV to cross-check against: ", HDI_CSV)
}

# =============================================================================
# 5. the period means
# =============================================================================
# 40's period_mean(): the MEAN over the years of a period, not the sum, so a
# period short of a year stays comparable. Every column here is additive in the
# year, so this commutes with every aggregation below it -- the identity checked
# above survives it, which is why it is safe to check first and average after.
d <- period_mean(d_y, "value", c("iso3c", "comm_code", "fuel", "account"))
if (!nrow(d))
  stop("[49] period_mean() returned nothing -- no year of PERIODS survived section 2.")
rm(d_y)

d[, account := factor(account, levels = ACCOUNTS)]
d[, fuel    := factor(fuel,    levels = names(biofuel_groups))]
d <- merge(d, items, by = "comm_code", all.x = TRUE)
if (anyNA(d$item))
  stop("[49] comm_code(s) absent from items_full_bcp.csv: ",
       paste(unique(d[is.na(item), comm_code]), collapse = ", "))

# 40's continent_of(), NOT a local read of regions_full.csv: one answer to "which
# region is this country in" in the whole block. Attached once, here, so every
# downstream expression can key on either column without knowing which it got.
d[, continent := continent_of(iso3c)]
{
  cn <- sort(unique(d$continent))
  message(sprintf("[49] continents present (%d): %s", length(cn), paste(cn, collapse = ", ")))
  if ("Unknown" %in% cn)
    message(sprintf("[49]   'Unknown' carries %.2f%% of the pooled world total.",
                    100 * d[continent == "Unknown", sum(value)] / d[account == "PBA", sum(value)]))
}

# =============================================================================
# 6. the panels: which, and in what order
# =============================================================================
# ONE ranking, taken on the LAST period and pooled over chains, shared by every
# row of every figure this script writes: the same panels, in the same order, in
# both period rows and in all four chain rows. Ranking each row separately would
# optimise each panel and make the grid uncomparable, which is the whole point of
# the grid.
panel_totals <- {
  pt <- dcast(d[period == LAST, .(value = sum(value)), by = c(PCOL, "account")],
              stats::as.formula(paste(PCOL, "~ account")), value.var = "value", fill = 0)
  setnames(pt, PCOL, "panel")
  pt[, panel := as.character(panel)]
  # An account with no row at all in the last period yields no dcast column, and
  # pmax()/get() below would fail on a missing name rather than on a zero.
  for (a in ACCOUNTS) if (!a %in% names(pt)) set(pt, j = a, value = 0)
  pt[!panel %in% DROP_PANELS]
}
if (!nrow(panel_totals))
  stop("[49] no ", UNIT_ONE[[UNIT]], " carries any ", STRESSOR, " in ", LAST, ".")

if (identical(PANEL_ORDER, "alpha")) {
  setorder(panel_totals, panel)
} else {
  # NOT a name with a leading dot: data.table reserves that namespace for its own
  # symbols, and `-.ord` in setorder() is an expression a reader has to squint at.
  panel_totals[, ord_by := switch(PANEL_ORDER,
                                  max  = pmax(PBA, CBA, VA),
                                  mean = (PBA + CBA + VA) / 3,
                                  {
                                    if (!PANEL_ORDER %in% ACCOUNTS)
                                      stop("[49] PANEL_ORDER must be one of ",
                                           paste(c(ACCOUNTS, "max", "mean", "alpha"), collapse = " / "),
                                           ", not '", PANEL_ORDER, "'.")
                                    get(PANEL_ORDER)
                                  })]
  setorder(panel_totals, -ord_by)
}
SEL <- head(panel_totals$panel, if (is.finite(TOP_N)) TOP_N else nrow(panel_totals))
if (is.finite(TOP_N) && length(SEL) < TOP_N)
  warning("[49] only ", length(SEL), " of the ", TOP_N, " ", UNIT_MANY[[UNIT]],
          " asked for carry any ", STRESSOR, ".")
message("[49] panels (", PANEL_ORDER, ", scored on ", LAST, "): ", paste(SEL, collapse = " "))

# Coverage is reported PER PERIOD and PER FUEL, not just pooled: a pooled "90%
# covered" can sit on top of a renewable-diesel row covering barely half its own
# total. With TOP_N = Inf and an empty DROP_PANELS every line should read 100%;
# if one does not, a panel went missing between the ranking and the selection --
# that is what this report is FOR at that unit, and the reason it runs for a
# complete set at all instead of being skipped as trivially true.
{
  cov <- merge(d[get(PCOL) %in% SEL, .(shown = sum(value)), by = .(period, fuel, account)],
               d[, .(all = sum(value)), by = .(period, fuel, account)],
               by = c("period", "fuel", "account"))
  cov[, pct := 100 * shown / all]
  message("[49] coverage of each period x fuel world total:")
  for (pp in levels(cov$period)) for (ff in levels(d$fuel)) {
    x <- cov[period == pp & fuel == ff]
    if (!nrow(x)) next
    message(sprintf("         %-11s %-17s %s", pp, chain_labeller(ff),
                    paste(sprintf("%s %.0f%%", x$account, x$pct), collapse = " | ")))
  }
}

# =============================================================================
# 7. the pooled chain, then the item pooling
# =============================================================================
# WHERE with_total() SITS IS THE WHOLE POINT. Everything above -- the
# conservation checks, the panel ranking, the coverage denominators -- runs on
# the three chains alone and is untouched by it. Move the call any earlier and
# every one of those would see the world twice.
d <- with_total(d, "fuel")
d[, fuel := factor(chain_labeller(fuel), levels = chain_labeller(levels(fuel)))]

# `panel` is assigned BEFORE the pooling, so pool_items() and everything after it
# never learn which unit they are working at -- and the aggregation to continents
# happens in the same group-by that already summed the item rows, not in a
# separate pass that could disagree with it.
rows  <- d[get(PCOL) %in% SEL][, panel := get(PCOL)]
scope <- switch(ITEM_SCOPE,
                world = d,
                set   = rows,
                stop("[49] ITEM_SCOPE must be 'world' or 'set', not '", ITEM_SCOPE, "'."))

# the chains take MIN_ITEM_SHARE_BYFUEL, the pooled Total row keeps the other
min_by <- setNames(rep(MIN_ITEM_SHARE_BYFUEL, nlevels(d$fuel)), levels(d$fuel))
min_by[[TOTAL_LAB]] <- MIN_ITEM_SHARE_POOLED

message("[49] item pooling:")
# `period` among the cell keys -- see THE KEEP-SET SPANS BOTH PERIODS, above.
q <- pool_items(rows, c("account", "fuel", "period"), min_by, scope, UNIT)[
  , .(value = sum(value)),
  by = .(panel, account, fuel, period, item, comm_code, chain, stage)]
q[, panel := factor(panel, levels = SEL)]

# --- stack order and colours, fixed ONCE over everything drawn ---------------
# Both figures come out of `q`, so this is automatically shared between them: an
# item keeps its colour and its height in the stack in figure [1] and in every
# per-period file of figure [2].
lvl <- make_levels(q)
q[, item := factor(item, levels = as.character(lvl$item))]
pal <- build_palette(unique(lvl[, .(item, comm_code, chain, stage)], by = "item"))

# =============================================================================
# 8. the shared plot skeleton
# =============================================================================
# How many keys a figure actually shows: every level under LEGEND_KEEP_ALL, only
# the items it draws otherwise. Counting levels in the second case would reserve
# rows for keys that are never printed.
n_keys <- function(z) if (LEGEND_KEEP_ALL) nrow(lvl) else uniqueN(as.character(z$item))

base_plot <- function(p, n_legend_rows = 3L) {
  ggplot(p, aes(x = account, y = value / META$scale_factor,
                fill = item, pattern = item %in% names(fuel_colors))) +
    panel_bands(p, "panel", SEPARATOR, BAND_FILL) +   # BENEATH the bars: added first
    geom_col_pattern(
      position                 = position_stack(reverse = TRUE),  # first level at the BASE
      width                    = 0.85,
      colour                   = SEG_BORDER,
      linewidth                = SEG_BORDER_LWD,
      pattern_colour           = NA,                 # no outline on the stripes themselves
      pattern_fill             = PATTERN_INK,
      pattern_angle            = PATTERN_ANGLE,
      pattern_density          = PATTERN_DENSITY,
      pattern_spacing          = PATTERN_SPACING,
      key_glyph                = draw_key_fuelhatch) +
    scale_pattern_manual(values = c(`FALSE` = "none", `TRUE` = "stripe"), guide = "none") +
    scale_fill_manual(values = pal, drop = !LEGEND_KEEP_ALL) +
    scale_y_continuous(labels = scales::label_number(big.mark = ","),
                       expand = expansion(mult = c(0, 0.04))) +
    guides(fill = guide_legend(nrow = n_legend_rows, byrow = TRUE)) +
    labs(x = NULL, y = META$y_label, fill = NULL) +
    theme_minimal(base_size = BASE_SIZE) +
    theme(legend.position    = "bottom",
          legend.key.size    = unit(BASE_SIZE - 1, "pt"),
          legend.text        = element_text(size = BASE_SIZE - 3),
          axis.title         = element_text(size = BASE_SIZE - 1),
          axis.text.y        = element_text(size = BASE_SIZE - 3),
          strip.placement    = "outside",
          strip.background   = element_blank(),
          strip.text         = element_text(face = "bold", size = BASE_SIZE - 1),
          panel.border       = if (SEPARATOR %in% c("border", "both"))
            element_rect(fill = NA, colour = SEP_COLOUR, linewidth = 0.3)
          else element_blank(),
          panel.spacing.x    = unit(PANEL_GAP, "pt"),
          panel.spacing.y    = unit(8, "pt"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank(),
          axis.text.x        = element_text(size = BASE_SIZE - 3, angle = 90,
                                            hjust = 1, vjust = 0.5),
          strip.text.y.left  = element_text(face = "bold", size = BASE_SIZE, angle = 90),
          plot.title         = element_text(size = BASE_SIZE + 2, face = "bold"),
          plot.subtitle      = element_text(size = BASE_SIZE - 4, colour = "grey30"),
          plot.caption       = element_text(size = BASE_SIZE - 5, colour = "grey40"))
}

CAPTION <- sprintf("%s allocation, %s value-added base; biogasoline + biodiesel + renewable diesel.",
                   allocation, VA_BASE)
# "where the pressure is exerted", not "where the impact occurs": the items name
# the PRODUCTION an impact is attributed to. Where the damage lands is a property
# of the indicator -- see 40's NONLOCAL_PATHWAYS, which provenance_note() renders.
LEGEND_NOTE <- paste0("One hue per feedstock chain (as in the footprint figures), shaded dark for the ",
                      "primary commodity and light for the processed one:\nPBA/CBA items are where the ",
                      "pressure is exerted, VA items are where the value is captured. Fuel segments are hatched.")

DROPPED    <- intersect(DROP_PANELS, unique(as.character(d[[PCOL]])))
DROP_SHARE <- if (length(DROPPED)) {
  s <- merge(d[get(PCOL) %in% DROPPED, .(x = sum(value)), by = account],
             d[, .(a = sum(value)), by = account], by = "account")
  max(s$x / s$a)
} else 0
# The completeness claim is CHECKED, not asserted: a figure that describes itself
# as summing to the world total when a dropped panel actually carries impact is
# the failure this clause exists to prevent.
PICK_NOTE <- if (is.finite(TOP_N)) {
  sprintf("The %d largest %s by pooled %s in %s", length(SEL), UNIT_MANY[[UNIT]],
          PANEL_ORDER, LAST)
} else if (!length(DROPPED)) {
  sprintf("All %d %s, so the panels sum to the world total", length(SEL), UNIT_MANY[[UNIT]])
} else {
  sprintf(paste0("All %d %s except %s, which carries up to %.2f%% of an account and is NOT ",
                 "drawn -- the panels therefore fall short of the world total by that much"),
          length(SEL), UNIT_MANY[[UNIT]], paste(DROPPED, collapse = ", "), 100 * DROP_SHARE)
}
ORDER_NOTE <- sprintf("ordered by %s in %s", PANEL_ORDER, LAST)
SCOPE_NOTE <- switch(ITEM_SCOPE,
                     world = "of the world total",
                     set   = sprintf("across the %s shown", UNIT_MANY[[UNIT]]))
PERIOD_NOTE <- sprintf("Each period is the MEAN over its years (%s), not the sum, so a period short of a year stays comparable.",
                       paste(vapply(names(PERIODS),
                                    function(p) sprintf("%s = %s", p,
                                                        paste(intersect(PERIODS[[p]], have_yr), collapse = "/")),
                                    character(1)), collapse = "; "))

W  <- max(if (SHOW_TITLES) 11 else 10, 5 + PANEL_W_BYFUEL[[UNIT]] * length(SEL))
caps <- character(0)

# =============================================================================
# 9. [1] the pooled figure: rows = period, FIXED y
# =============================================================================
# THE ONE PLACE THE PERIODS ARE DIRECTLY COMPARABLE. scales = "fixed", not
# "free_y": the whole figure is the movement between the two rows, and a free
# axis per row would rescale each period to fill its own row and erase exactly
# that. Do not "fix" a squat lower row by freeing the scale -- if 2012-2014 is
# small, that IS the finding.
q1 <- q[fuel == TOTAL_LAB]
if (!nrow(q1)) stop("[49] the pooled 'Total' chain is empty -- with_total() did not run?")

ttl1 <- sprintf("%s responsibility for bio-based transport fuels by %s: %s vs %s",
                META$short_label, UNIT_ONE[[UNIT]], names(PERIODS)[1], LAST)
sub1 <- sprintf(paste0("PBA production-based | CBA consumption-based | VA value-added-based (%s): ",
                       "three attributions of one and the same total, all three biofuel chains pooled.\n",
                       "%s; %s. %s\n",
                       "Y AXIS IS FIXED ACROSS BOTH ROWS -- the movement between the periods is the ",
                       "figure. Items below %.0f%% of an account x period cell %s are pooled as 'Other'. %s"),
                VA_DEFINITION_NOTE, PICK_NOTE, ORDER_NOTE, PERIOD_NOTE,
                100 * MIN_ITEM_SHARE_POOLED, SCOPE_NOTE, LEGEND_NOTE)
stem1 <- sprintf("responsibility_%s_item_periods_%s_%s_%s", UNIT, STAG, VA_BASE, ATAG)
caps  <- c(caps, lab_block(stem1, ttl1, sub1, SHOW_TITLES))

ttl1w <- wrap_lines(ttl1, W, BASE_SIZE + 2)
sub1w <- wrap_lines(sub1, W, BASE_SIZE - 4)
h1    <- 2.6 + 2.7 * max(uniqueN(q1$period), 1)
if (SHOW_TITLES)
  h1 <- h1 + text_height_in(ttl1w, BASE_SIZE + 2) +
  text_height_in(sub1w, BASE_SIZE - 4) + 0.25

save_svg(stem1,
         base_plot(q1, legend_rows(n_keys(q1), W)) +
           facet_grid(period ~ panel, scales = "fixed", switch = "both",
                      labeller = labeller(panel = strip_labeller(UNIT),
                                          period = label_value)) +
           labs(title    = if (SHOW_TITLES) ttl1w else NULL,
                subtitle = if (SHOW_TITLES) sub1w else NULL,
                caption  = if (SHOW_CAPTION) CAPTION else NULL),
         width = W, height = h1)

# =============================================================================
# 10. [2] one file per period: rows = Total then each chain, y FREE PER ROW
# =============================================================================
# This is 44's grid for a period mean. The axis is free per row because renewable
# diesel is an order of magnitude below biodiesel and would otherwise collapse to
# a flat line, and the pooled row is by construction the sum of the three beneath
# it. Bar heights are therefore NOT comparable across rows -- not between chains,
# and not against Total.
#
# One file PER PERIOD, and NOT one grid with chain and period both on the rows:
# free_y frees a scale per ROW, so such a grid would give the two periods of one
# chain different axes. Figure [1] is where the periods are compared; these files
# answer "what is the composition WITHIN this period".
if (BY_PERIOD_CHAINS && nlevels(q$fuel) > 1) {
  for (per in levels(q$period)) {
    q2 <- q[period == per]
    if (!nrow(q2)) next
    
    ttl2 <- sprintf("%s responsibility for bio-based transport fuels by %s, %s - by biofuel chain",
                    META$short_label, UNIT_ONE[[UNIT]], per)
    sub2 <- sprintf(paste0("PBA production-based | CBA consumption-based | VA value-added-based (%s): ",
                           "three attributions of one and the same total, per chain and pooled.\n",
                           "%s; %s -- the same panels and order as the two-period figure. %s\n",
                           "Top row is all three chains pooled. Items below %.0f%% of an account x chain ",
                           "cell (%.0f%% in the pooled row) %s are pooled as 'Other'.\n",
                           "Y AXIS IS FREE PER ROW -- compare panels within a row, not across rows, and ",
                           "not against Total. %s"),
                    VA_DEFINITION_NOTE, PICK_NOTE, ORDER_NOTE, PERIOD_NOTE,
                    100 * MIN_ITEM_SHARE_BYFUEL, 100 * MIN_ITEM_SHARE_POOLED,
                    SCOPE_NOTE, LEGEND_NOTE)
    stem2 <- sprintf("responsibility_%s_item_%s_%s_%s_%s",
                     UNIT, slug(per, "period"), STAG, VA_BASE, ATAG)
    caps  <- c(caps, lab_block(stem2, ttl2, sub2, SHOW_TITLES))
    
    ttl2w <- wrap_lines(ttl2, W, BASE_SIZE + 2)
    sub2w <- wrap_lines(sub2, W, BASE_SIZE - 4)
    h2    <- 2.6 + 2.7 * max(nlevels(q2$fuel), 1)          # 4 rows -> 13.4in
    if (SHOW_TITLES)
      h2 <- h2 + text_height_in(ttl2w, BASE_SIZE + 2) +
      text_height_in(sub2w, BASE_SIZE - 4) + 0.25
    
    save_svg(stem2,
             base_plot(q2, legend_rows(n_keys(q2), W)) +
               facet_grid(fuel ~ panel, scales = "free_y", switch = "both",
                          labeller = labeller(panel = strip_labeller(UNIT),
                                              fuel = label_value)) +
               labs(title    = if (SHOW_TITLES) ttl2w else NULL,
                    subtitle = if (SHOW_TITLES) sub2w else NULL,
                    caption  = if (SHOW_CAPTION) CAPTION else NULL),
             width = W, height = h2)
  }
}

write_captions(caps, paste0(UNIT, "_item_periods"), stamp = "periods")

# =============================================================================
# 11. console: the table behind the pooled figure
# =============================================================================
cat(sprintf("\n-- %s | %s: pooled accounts by %s, %s vs %s --\n",
            META$short_label, allocation, UNIT_ONE[[UNIT]], names(PERIODS)[1], LAST))
print(dcast(q1[, .(v = sum(value) / META$scale_factor), by = .(panel, period, account)],
            panel + period ~ account, value.var = "v", fill = 0)[order(panel, period)])
cat("\n - These are the SAME account totals 44 draws for a single year, averaged over each period.\n")
cat(" - Only the fill differs from 48: the stacks are ITEMS here, not domestic/export/import.\n")

message(">>> [49] plots written to ", PLOT_DIR)