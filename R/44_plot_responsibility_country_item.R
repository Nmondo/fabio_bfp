# =============================================================================
# 44_plot_responsibility_country_item.R
# ONE figure PER PANEL SET: the production- (PBA), consumption- (CBA) and
# value-added-based (VA) accounts as three ADJACENT stacked bars per panel, at
# PANEL x ITEM resolution, for PLOT_YEAR. A panel is a COUNTRY or a CONTINENT --
# see THE UNIT below.
#
#   responsibility_<unit>_item_<set>_*.svg
#       facet_grid(fuel ~ panel): x = panel (largest first), three bars each |
#       y = impact | fill = item. The TOP ROW is the three chains POOLED -- 40's
#       `total` chain, added by with_total() before the item pooling -- and the
#       three chain rows sit beneath it, with the same panels in the same order
#       in every row so the rows can be read against one another.
#
#       This used to be two files, a pooled one and a `_byfuel_` one. The pooled
#       figure is now the top row of this grid, which is why the stem is the
#       pooled figure's and the `_byfuel_` name is retired.
#
#       The y axis is FREE PER ROW: renewable diesel is an order of magnitude
#       below biodiesel and would otherwise collapse to a flat line, and the
#       pooled row is by construction the sum of all three. Each row therefore
#       shows its own COMPOSITION at its own scale, and bar heights are NOT
#       comparable across rows -- not between chains, and not against Total.
#
# THE UNIT: WHY THE CONTINENTAL FIGURE COSTS NOTHING UPSTREAM -----------------
# PBA and CBA are the two MARGINS of D (rowSums / colSums below) and VA is a
# plain sum off 42's CSV. Summing a margin over a group of countries IS the
# margin of the folded matrix, so the continental figure is an exact aggregation
# of the country one -- no new artefact, no recomputation, nothing lost.
# Contrast 48: a REGIONAL TRADE SPLIT cannot be rebuilt this way, because
# summing `justice_domestic` over the countries of a region keeps the COUNTRY
# definition of domestic and an intra-EU flow stays an export. That is why 41
# and 42 fold their flow matrices at source for 48. This figure has no diagonal
# term and is exempt, which is the whole reason `unit` is a field of a set here
# rather than a new pair of CSVs there.
# The country -> continent mapping is 40's continent_of(), never a local read of
# regions_full.csv: one answer to "which region is this country in" in the whole
# block (that is the bug the helper was written to kill).
#
# THE PANEL SETS (PANEL_SETS, below) ------------------------------------------
# One row of 18 countries made each panel ~0.7in wide, which is narrower than the
# three account labels under it. The set is now SPLIT BY ROLE rather than trimmed
# by rank, because the halves answer different questions:
#   `producers`   the 5 largest producers, ranked on the POOLED production-based
#                 account (the three biofuels added up)  -- who makes the stuff
#   `eu`          the 7 largest EU member states, ranked on the LARGEST of their
#                 three accounts, so a member that mainly CONSUMES and one that
#                 mainly CAPTURES VALUE both qualify   -- who in the EU is on the
#                 hook for it -- PLUS Belgium, shown regardless of rank
#                 (must_include), which makes eight panels
#   `continents`  every continent, unit = "continent". No selection: at six
#                 panels a top-N is a way to lose a region, not to declutter.
# Every figure below is written once per set; adding a fourth set to PANEL_SETS
# needs no other change here, and none at all in 45.
#
# WHY ONE SCRIPT AND NOT A SIBLING 44b ----------------------------------------
# The stack order and the palette are fixed ONCE over the UNION of every set
# drawn (make_levels / build_palette below). A separate script would build its
# own levels from its own keep-set, and palm oil could be the second segment of
# the country figure and the fifth of the continental one, in a different
# colour. The sets share a script precisely so the figures stay readable against
# each other.
#
# The whole grid comes out of one computation: D is built per fuel and the pooled
# top row sums over the fuel dimension. Item keep-sets, stack order and colours
# are fixed ONCE over the union of the sets, so an item keeps its colour and its
# height in the stack across every figure this script writes.
#
# WHY THIS SCRIPT COMPUTES INSTEAD OF ONLY PLOTTING ---------------------------
# The accounts CSV is keyed by iso3c only -- no item dimension -- so the PBA/CBA
# bars cannot be read off it. Both are margins of one bilateral matrix
#     D = diag(f) B[, bf] Y_bf     (node i x consuming country c)
#     f = e / x , B = L_<allocation> , Y_bf = biofuel final demand
# rebuilt here at node resolution through the shared builder in
# 00_responsibility_helpers.R, so the math cannot drift from 41's:
#     PBA(i) = rowSums(D)          impact at the producing node i
#     CBA(c) = colSums(D)          impact routed to the consumer of the biofuel
# The VA bar is NOT recomputed: it is read from 42's responsibility CSV for the
# variant 40's VA_VARIANT selects, already resolved to (va_iso3c, va_comm_code).
# All three are then checked against the country-level accounts CSV and against
# each other.
#
# ITEM SEMANTICS -- read before interpreting the stacks -----------------------
#   PBA / CBA   the PRODUCING node -- the item whose production exerts the
#               pressure (the source item). NOT necessarily where the damage
#               materialises: ibif_total sums CO2eq, NH3 and two land-use terms
#               (15_6), and only the land terms land where the farm is. PBA books
#               it to the producing country, CBA to the consuming one.
#   VA          the sector that CAPTURES the value -- the item that earns, not
#               the item that emits.
# Each account sums to the SAME grand total; only its distribution moves.
#
# WHY NOT THE FEEDSTOCK DIMENSION USED BY 18_01b / 19_01b ---------------------
# Those scripts stack by FIRST-DEGREE FEEDSTOCK, rolling a feedstock's whole
# upstream chain into the item that enters the biofuel, so the crop never appears
# next to its oil. That collapse is wrong here: the impact lands on the PRIMARY
# commodity (crop) and the value on the SECONDARY one (oil, fuel), and that split
# is precisely the contrast this figure draws.
#
# COLOURS -- 19's fixed feedstock palette, shaded by stage --------------------
#   item in feedstock_color_map      -> that colour, EXACTLY (Palm Oil, Sugar cane)
#   primary crop of such a feedstock -> the same colour, DARKENED
#   cake / by-product                -> the same colour, LIGHTENED
#   non-feedstock items (Grazing)    -> 19's reserve slots
# One hue = one chain; the shade gives the processing stage: dark = where the
# impact is, light = where the value is.
#
# THE FUELS ARE HATCHED, NOT RE-COLOURED --------------------------------------
# The fuels keep `fuel_colors` (19) and sit at the BASE of every stack -- they can
# only appear in the VA bar (refining causes no direct pressure but does capture
# value). fuel_colors and feedstock_color_map COLLIDE once the stage shading is
# applied: Biodiesel #5B2C6F vs darkened Soyabeans (dE 8.9), Biogasoline #D4A017
# vs Rape and Mustard Oil (dE 11.7), Renewable diesel #9B59B6 vs lightened
# Soyabean Cake (dE 14.3). Under ~15 dE those are the same colour at legend-key
# size, and they are the soy and rape chains -- exactly the feedstocks that
# dominate the fuels they collide with, so the segments are often ADJACENT in the
# same VA stack. Re-colouring does not fix it: `fill` already carries two
# variables (chain -> hue, stage -> lightness) and the shade step fills the
# lightness neighbourhood around every feedstock hue, so any third hue lands
# inside some feedstock's shade family. The free channel is TEXTURE, so the fuels
# are hatched and everything else is plain.
# CAVEAT: a hatch needs ~6pt of segment to read, and many fuel segments in the
# small panels are 2-3pt. Those are unidentifiable by ANY encoding at that size;
# the hatch earns its keep on the large ones, which is where misreading happens.
#
# READS  <MRIO>/losses/{X.rds, Y.rds, fd_labels.csv, <yr>_L_<alloc>.rds}   (13,14)
#        <base>/{E.rds, io_labels.csv}                                     (16,12_b)
#        inst/items_full_bcp.csv                                           (item, comm_group)
#        VA_RESP_CSV (42) | HDI_CSV (41, cross-check)
# WRITES output/plot/responsibility_<unit>_item_<set>_<ind>_<vabase>_<alloc>_<year>.svg
#          -- two figures per entry of PANEL_SETS; <unit> is "country" or
#          "continent" and <set> keeps two sets of the same unit apart. The
#          country stems are UNCHANGED from before the split, so anything already
#          citing responsibility_country_item_producers_* still resolves.
#        ORDER_CSV -- the country order, so 45 can follow it. Carries a
#          `country_set` column (one block of rows per set) and a `unit` column;
#          45 loops over whatever sets it finds there. ONLY unit == "country"
#          sets are written: 45 is a by-COUNTRY figure and joins this file's
#          `iso3c` against a country-keyed CSV, so a continent name in that
#          column would either break its merge or silently draw nothing. 45 also
#          filters on `unit` defensively, so neither side alone can leak a
#          region into it. NB the set column is not called `set`:
#          data.table::set() is a function and a column of that name shadows it
#          inside i/j expressions.
#        captions_<unit>_item_*.txt -- one caption file per UNIT, so a caption
#          cannot be pasted under a figure at the other resolution.
#
# RUN: Rscript R/44_plot_responsibility_country_item.R   (after 41 and 42)
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
library(svglite)
# ggpattern hatches the fuel segments. It is the only non-base dependency this
# script adds; fail with a useful message rather than an opaque
# "could not find function geom_col_pattern".
if (!requireNamespace("ggpattern", quietly = TRUE))
  stop("[44] package 'ggpattern' is required to hatch the fuel segments.\n",
       "     install.packages('ggpattern')   # pulls in 'gridpattern'")
library(ggpattern)

source("R/40_responsibility_shared.R")
source("R/00_responsibility_helpers.R")   # country_grid(), build_D()
SCRIPT <- "44"
run_banner()
message(sprintf(">>> [44] year = %d", PLOT_YEAR))

# --- what falls into the grey "Other" ----------------------------------------
# Two kinds of row, two crowding budgets, kept SEPARATE on purpose. The pooled
# Total row takes POOLED, on `account`; the chain rows take BYFUEL, on
# `account x fuel`, and so keep MORE items -- that is the point of the split: an
# item small overall but large within one fuel earns a colour there. A single
# shared constant would couple them, so decluttering the chain rows' legend would
# silently drop items from the pooled row. Raise BYFUEL to thin the chain rows;
# do not push it past ~0.03-0.04.
# Both feed ONE keep-set and ONE legend now (the rows share a figure), so the
# union is what gets drawn -- see the note at pool_items().
# These are the DEFAULTS; each country set overrides them below, and each also
# picks the DENOMINATOR the share is taken against (item_scope).
MIN_ITEM_SHARE_POOLED <- 0.02    # Total row: below this share of an account        -> "Other"
MIN_ITEM_SHARE_BYFUEL <- 0.03    # chain rows: below this share of an account x fuel -> "Other"

# --- figure switches ---------------------------------------------------------
# THE PANEL SETS. One entry = one pair of figures. Fields:
#   unit    "country" (default) | "continent"   what ONE PANEL is. Everything
#           below reads on whichever it is: "the 5 largest" and "below x% of an
#           account" mean the same thing at either resolution, because the
#           aggregation is exact (see THE UNIT in the header).
#   n       how many panels. Inf = all of them, which is what a continent set
#           wants: with six panels a top-N drops a region rather than declutters.
#   pool    "world" | "EU"   the universe ranked within (EU from EU_SOURCE below)
#           "EU" is only meaningful for unit = "country": Europe under
#           continent_of() is NOT EU27, and a set that means "the EU as a bloc"
#           is a different grouping needing its own aggregator, not this one.
#   select  which score decides WHO gets in
#             "PBA" / "CBA" / "VA"  the pooled account, largest first
#             "max" / "mean"        across the three pooled accounts
#             "share"               max over (fuel x account) of the panel's
#                                   SHARE of that fuel's world total
#   order   left-to-right order of the panels selected: same vocabulary, plus
#           "rank"  = keep the selection order
#           "alpha" = alphabetical (48's REGION_ORDER vocabulary; see below)
#   drop    panels to leave out entirely, e.g. "Unknown" -- 48's DROP_REGIONS
#   tag     goes into the file name, so two sets cannot overwrite each other
#   title   completes the plot title, after the count ("5 largest producers")
#   label   the criterion, spelled out for the subtitle
#   item_scope  what the "below x% -> Other" share is a share OF:
#             "world" the item's share of the account across ALL countries. The
#                     grey "Other" then means the same thing in every figure, and
#                     an item's colour does not depend on which set it is drawn
#                     in -- but a feedstock that is minor globally and major in
#                     the set disappears into the grey.
#             "set"   the item's share of the account across THE COUNTRIES SHOWN.
#                     The threshold then describes the figure the reader is
#                     actually looking at. This is what the EU set wants: rape
#                     oil, sunflower and UCO are a few per cent of the world and a
#                     large slice of the EU, and under "world" they were pooled.
#             The cost of "set" is that "Other" is no longer one category across
#             the figures, so the SUBTITLE names the denominator -- do not drop
#             that clause when writing captions.
#             NB for a set that shows EVERY panel of its unit -- the continent
#             set -- "set" and "world" are the SAME denominator, because the
#             shares group by account (x fuel) and sum over panels either way.
#             Use "world" there so the caption says the simpler true thing, and
#             so the continental figure keeps exactly the same items, and
#             therefore the same colours, as the world-scoped country figure.
#   min_pooled / min_byfuel   the "-> Other" thresholds, per set: min_pooled on
#                             the pooled Total row, min_byfuel on the chain rows
#
# WHY select AND order ARE SEPARATE. The fuels differ in size by more than an
# order of magnitude (2019: biogasoline 232,749 | biodiesel 124,658 | renewable
# diesel 14,008), so a ranking on ABSOLUTE pooled impact is really a ranking on
# the big two and renewable diesel's own leaders (the HVO refiners: NLD, FIN,
# NOR, and PNG on the feedstock side) never make the cut. "share" fixes that by
# normalising on each fuel's own total -- but it is a bad thing to SORT the axis
# by, because it is not the quantity the reader is looking at, so the bars come
# out in what looks like random order. Select on one score, order on another.
#
# WHY THESE TWO SETS. `producers` ranks on PBA alone: the question is who
# physically makes the fuel, and PBA is exactly that account -- pooled over the
# three fuels, i.e. the biofuels simply added up. `eu` ranks on "max" so that a
# member state that mostly consumes (DEU, FRA) and one that mostly captures value
# (NLD -- ~17% of the renewable-diesel VA total, negligible in production) both
# qualify; ranking the EU on PBA would return its refiners only.
# NOTE the side effect of ORDER on a producer-heavy set: a country big in a
# DIFFERENT account sinks to the right. Under "CBA", PNG (PBA 1,160, CBA 2) lands
# last -- honest, but "max" places producer-only countries by their strongest
# account instead, which is why `producers` orders on PBA.
#
# WHY THE CONTINENT SET ORDERS ON PBA. 48 puts its regions in PBA order by
# default (REGION_ORDER), and two continent figures in the same document with
# different left-to-right axes is a reading error waiting to happen. Change both
# or neither.
PANEL_SETS <- list(
  producers = list(
    tag    = "producers",
    unit   = "country",
    n      = 5L,
    pool   = "world",
    select = "PBA",
    order  = "PBA",
    title  = "largest producers",
    label  = "the largest pooled production-based account, i.e. the three biofuels added up",
    # These five carry most of the world total, so a world-scoped share and a
    # set-scoped one keep nearly the same items. "world" is kept because it makes
    # this figure's "Other" comparable with any other world-scoped figure.
    item_scope  = "world",
    min_pooled  = MIN_ITEM_SHARE_POOLED,
    min_byfuel  = MIN_ITEM_SHARE_BYFUEL
  ),
  eu = list(
    tag    = "eu",
    unit   = "country",
    n      = 7L,
    pool   = "EU",
    select = "max",
    order  = "CBA",
    # BELGIUM IS SHOWN WHATEVER THE RANKING SAYS. It does not make the top 7 on
    # any of its three accounts, but it is a refining and re-export hub in these
    # chains and the figure is read as "the EU", so its absence was read as
    # absence of involvement. must_include ADDS it to the ranked seven rather
    # than displacing the seventh -- eight panels, the seven unchanged -- and it
    # is ordered on CBA with everybody else, so it lands AMONG the other member
    # states instead of being tacked onto an end. Drop the entry to go back to
    # the ranked seven; do not "fix" it by raising n, which would pull in the
    # next country by rank and still not guarantee Belgium.
    must_include = "BEL",
    # "largest" is deliberately NOT in the title any more: with a forced panel
    # the set is no longer "the 8 largest", and the caption spells the criterion
    # (`label`) and the exception (must_include) out underneath.
    title  = "EU member states",
    label  = paste("the largest of their three pooled accounts, so consumers and",
                   "value-capture hubs qualify alongside refiners"),
    # Set-scoped: the EU mix is not the world mix. Rape and Mustard Oil,
    # Sunflowerseed Oil and Used Cooking Oil are a few per cent of the world
    # total and a large share of what these eight countries carry, so under
    # "world" they were swallowed by the grey "Other" -- in the one figure whose
    # subject is the EU feedstock mix. Belgium enters this denominator like any
    # other panel, so adding it can move an item across the threshold: that is
    # the intended behaviour of a set-scoped share, not drift.
    # The thresholds are held at the defaults; only the denominator changes.
    # If the legend now runs long, raise these
    # (0.03 / 0.04) rather than reverting the scope.
    item_scope  = "set",
    min_pooled  = MIN_ITEM_SHARE_POOLED,
    min_byfuel  = MIN_ITEM_SHARE_BYFUEL
  ),
  continents = list(
    tag    = "all",
    unit   = "continent",
    # Inf, not a number: the point of the continental view is that it is
    # COMPLETE -- the world total is on the canvas and the bars can be added up
    # by eye. A top-N would put part of the total in no panel at all.
    n      = Inf,
    pool   = "world",
    select = "PBA",
    order  = "PBA",
    # "Unknown" is continent_of()'s fallback bucket. It should be empty; if it
    # is not, the console says which countries landed in it and the panel is
    # dropped rather than drawn as a mystery region. Take it out of `drop` if
    # you would rather see the bucket than trust that it is empty.
    drop   = "Unknown",
    title  = "continents",
    # Under n = Inf the caption does not say "selected by", because nothing was
    # selected -- so this reads as a statement about the figure rather than as a
    # criterion. See PICK_NOTE below.
    label  = "the panels therefore sum to the world total",
    # World-scoped, and identical to "set" here since every panel is shown --
    # see the item_scope note above. Keeping it "world" is what makes this
    # figure's grey "Other" the same category as the producer figure's.
    item_scope  = "world",
    min_pooled  = MIN_ITEM_SHARE_POOLED,
    min_byfuel  = MIN_ITEM_SHARE_BYFUEL
  )
)

# --- defaults, so a set need only name what it changes ------------------------
# Applied ONCE, here, rather than with `%||%` at twenty call sites: a field that
# defaults in some places and not others is how `unit` would end up "country" in
# the file name and "continent" in the facet.
SET_DEFAULTS <- list(unit = "country", n = Inf, pool = "world", order = "rank",
                     drop = character(0), must_include = character(0),
                     item_scope = "world",
                     min_pooled = MIN_ITEM_SHARE_POOLED,
                     min_byfuel = MIN_ITEM_SHARE_BYFUEL)
PANEL_SETS <- lapply(PANEL_SETS, function(s) modifyList(SET_DEFAULTS, s))

# The panel column of `d` for a unit, and the noun the captions use for it.
UNIT_COL  <- c(country = "iso3c",     continent = "continent")
UNIT_ONE  <- c(country = "country",   continent = "continent")
UNIT_MANY <- c(country = "countries", continent = "continents")
cap1 <- function(s) paste0(toupper(substring(s, 1, 1)), substring(s, 2))

for (nm in names(PANEL_SETS)) {
  u <- PANEL_SETS[[nm]]$unit
  if (!u %in% names(UNIT_COL))
    stop("[44] set '", nm, "': unit must be 'country' or 'continent', not '", u, "'.")
  if (PANEL_SETS[[nm]]$pool == "EU" && u != "country")
    stop("[44] set '", nm, "': pool 'EU' needs unit 'country' -- the continent ",
         "'Europe' is not EU27, and an EU-as-a-bloc panel needs its own aggregator.")
  # must_include holds panel NAMES of the set's own unit -- iso3c for a country
  # set, a continent name for a continent one. Caught here rather than at
  # selection time, where a code that matches nothing is indistinguishable from a
  # panel that carries no impact and would only produce a warning.
  mi <- PANEL_SETS[[nm]]$must_include
  if (length(mi) && anyDuplicated(mi))
    stop("[44] set '", nm, "': must_include repeats ",
         paste(unique(mi[duplicated(mi)]), collapse = ", "), ".")
  if (length(intersect(mi, PANEL_SETS[[nm]]$drop)))
    stop("[44] set '", nm, "': ",
         paste(intersect(mi, PANEL_SETS[[nm]]$drop), collapse = ", "),
         " is in BOTH must_include and drop -- decide which.")
}
if (anyDuplicated(vapply(PANEL_SETS, function(s) paste(s$unit, s$tag), character(1))))
  stop("[44] two sets share a unit and a tag -- their SVGs would overwrite each other.")

# EU membership is read from the file the rest of the pipeline already uses
# (07_02 resolves EU27 the same way) rather than from a list pasted in here,
# which would go stale. EU_WITH_GBR = TRUE gives EU28; PLOT_YEAR is post-Brexit,
# so it is FALSE -- flip it only if the figure is meant to be historical.
EU_SOURCE   <- "inst/regions_full.csv"   # needs columns iso3c, EU27
EU_WITH_GBR <- FALSE

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

# --- separating the countries visually ---------------------------------------
# facet_wrap puts no mark at a panel boundary, so with three bars per country the
# only cue for where one country ends is a 3pt gap. The failure mode is reading
# the VA bar of one country as the PBA bar of the next -- which inverts the
# figure's argument. Two devices, independently switchable:
#   "border"  a thin rule around every country panel (the vertical lines)
#   "band"    every other country gets a tinted background. It spans the FULL
#             panel height, so it still separates where the bars are short and
#             the rule has nothing next to it; it is also the cue that survives
#             greyscale printing and a bad projector.
#   "both"    default -- the rule carries the boundary, the band carries the group
#   "none"    the old look
# The band is drawn BENEATH the bars with a FIXED fill, not a mapped one, so it
# never enters the fill legend and cannot collide with the feedstock palette.
SEPARATOR  <- "border"           # the rule alone; "band"/"both" restore the tint
SEP_COLOUR <- "grey89"           # L* 90: lighter than any fill, incl. the lightened
# by-product shades. Matches 45.
BAND_FILL  <- "grey96"           # only used when SEPARATOR includes "band"
PANEL_GAP  <- 5                  # pt between panels (was 3)

# --- how wide ONE panel has to be, per unit ----------------------------------
# The old rule (0.70in per panel) was measured on a 3-character ISO code under
# three rotated account labels. A continent name is not a 3-character code:
# "South America" needs either a wider panel or a wrapped strip, and at 0.70in it
# collides with its neighbour. So the per-panel allowance follows the UNIT, and
# the strip label is wrapped as well -- 48 solves the same problem by holding bar
# density constant, and these numbers are that rule expressed per panel.
# The country values are the previous constants exactly, so the country figures
# come out at the sizes they already had.
# One figure now, so one allowance. The wider (former figure-2) numbers are the
# ones that survive: the grid carries a rotated fuel strip down its left edge and
# the panels have to hold their labels alongside it.
PANEL_W_BYFUEL <- c(country = 0.78, continent = 1.25)
# Characters per line of a strip label before it wraps. Countries are never
# wrapped (a 3-letter code has nothing to wrap), continents are.
STRIP_WRAP <- c(country = 40L, continent = 11L)

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

# --- type size and furniture -------------------------------------------------
# ONE number for the type. 11pt was sized for a 17in standalone SVG; these figures
# now go into a document at roughly their natural size, where 7pt axis text lands
# at about 5pt on the page. Everything in the theme is expressed relative to this,
# so raise BASE_SIZE and the whole figure scales together.
BASE_SIZE <- 14

# The title and subtitle are dropped from the CANVAS, not from the record: with
# SHOW_TITLES = FALSE the text is written to captions_*.txt beside the SVGs, one
# block per figure, keyed by file name. Do not simply discard it. It
# carries three things the figure cannot be read honestly without:
#   * which countries were selected and on what score,
#   * what the grey "Other" is a share OF (the denominator differs between sets),
#   * for 45, whether the indicator's damage is local (it is not).
# A figure in a paper whose caption omits those is making claims it has not stated.
SHOW_TITLES  <- TRUE
SHOW_CAPTION <- TRUE     # the one-line allocation / VA-base note; cheap to keep

# One legend for both sets, or one legend per figure? See scale_fill_manual below.
# FALSE now that the sets pool their items differently: with per-set "Other"
# groupings, TRUE prints a key in the EU figure for every item that only the
# producer figure draws (sugar cane, cassava) and vice versa -- keys with no
# segment anywhere in the panel, in the figure whose whole point is that its
# item set is its own. Set TRUE if you would rather have one identical legend
# across every set's figure and can live with the dead keys.
LEGEND_KEEP_ALL <- FALSE

ACCOUNTS <- c("PBA", "CBA", "VA")       # bar order within each country

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

# --- inputs ------------------------------------------------------------------
YRC   <- as.character(PLOT_YEAR)
X     <- readRDS(need(file.path(MRIO_PATH, "losses", "X.rds"), "13/14"))
Y     <- readRDS(need(file.path(MRIO_PATH, "losses", "Y.rds"), "13"))
E     <- readRDS(need(file.path(base_path, "E.rds"), "16"))
B     <- readRDS(need(file.path(MRIO_PATH, "losses", paste0(YRC, "_L_", allocation, ".rds")), "14"))
io    <- fread(need(file.path(base_path, "io_labels.csv"), "12_b"))
fd    <- fread(need(file.path(MRIO_PATH, "losses", "fd_labels.csv"), "13"))
items <- fread(need("inst/items_full_bcp.csv", "00_update_items_list.R"))[
  , .(comm_code, item, comm_group)]
stopifnot(all(c("iso3c", "comm_code", "item") %in% names(io)), "iso3c" %in% names(fd))
if (!YRC %in% colnames(X) || is.null(Y[[YRC]]) || is.null(E[[YRC]]))
  stop("[44] year ", PLOT_YEAR, " absent from X / Y / E.")

# item table: readable UCO name, chain, stage -- the whole plot keys off these
items[grepl(UCO_PATTERN, item), item := "Used Cooking Oil"]
items[, chain := "Other"]
for (ch in names(CHAIN_CODES)) items[comm_code %in% CHAIN_CODES[[ch]], chain := ch]
items[, stage := fifelse(comm_group %in% STAGE_1, 1L,
                         fifelse(comm_group %in% STAGE_2, 2L, 3L))]

# ALIGNMENT GUARD. The math below is positional: as.numeric() strips names, so R
# lines up E, X, B and `io` by INDEX. If any artefact was rebuilt against a
# differently-ordered io_labels.csv, one item's extension is divided by another
# item's output and the impact lands on the WRONG item -- silently, and this is a
# by-item figure. Check it loudly before computing anything. (Mirrors 41.)
io_key <- paste0(io$iso3c, "_", io$comm_code)
for (obj in list(list(rownames(X), "X.rds rows"), list(colnames(E[[YRC]]), "E cols"))) {
  if (is.null(obj[[1]])) { warning("[44] ", obj[[2]], ": no names -- trusting position."); next }
  if (!identical(obj[[1]], io_key))
    stop("[44] ", obj[[2]], " != io_labels grid -- extensions would be misattributed. Re-run 16.")
}

# --- the bilateral matrices D_g: impact at node i, driven by consumer c ------
# ONE D PER FUEL. D is linear in Y_bf, so the pooled matrix is sum_g D_g and
# nothing is computed twice. build_D masks Y to the group's own rows
# (diag(sel) %*% Yc) rather than subsetting B; because B is sparse, the product
# only ever touches the columns of B at those few biofuel nodes, so the mask buys
# the same saving as an explicit subset without an index to keep in sync.
Xi <- as.vector(X[, YRC])
f  <- as.numeric(E[[YRC]][STRESSOR, ]) / Xi; f[!is.finite(f)] <- 0

grid <- country_grid(io, fd)
Yc   <- Y[[YRC]] %*% grid$S_fd                                         # N x consumer

acc_g <- list()
for (g in names(biofuel_groups)) {
  sel <- as.numeric(io$comm_code %in% biofuel_groups[[g]])
  if (!sum(sel)) { warning("[44] no node for fuel '", g, "' -- skipped."); next }
  
  # producing node x consuming country, at the node resolution the item stacks need
  D <- build_D(f, B, Yc, sel, countries = grid$countries)
  if (sum(D) == 0) { warning("[44] '", g, "' carries no ", STRESSOR, " in ", PLOT_YEAR, " -- skipped."); next }
  
  # PBA: impact at the producing node i -- rowSums(D_g)
  pba <- data.table(iso3c = io$iso3c, comm_code = io$comm_code, value = rowSums(D)
  )[value != 0, .(value = sum(value)), by = .(iso3c, comm_code)][, account := "PBA"]
  
  # CBA: same impact, routed to the consumer of the fuel -- colSums per source item
  Dc  <- rowsum(D, group = io$comm_code)                               # source item x consumer
  cba <- data.table(comm_code = rep(rownames(Dc), times = ncol(Dc)),
                    iso3c     = rep(colnames(Dc), each  = nrow(Dc)),
                    value     = as.vector(Dc))[value != 0][, account := "CBA"]
  
  acc_g[[g]] <- rbindlist(list(pba, cba), use.names = TRUE)[, fuel := g]
}
if (!length(acc_g)) stop("[44] the biofuel chains carry no ", STRESSOR, " in ", PLOT_YEAR, ".")
pc <- rbindlist(acc_g, use.names = TRUE)

# --- VA: read per fuel from 42 (its CSV already carries biofuel_group) -------
va <- fread(need(VA_RESP_CSV, "42"))[year == PLOT_YEAR & biofuel_group %in% names(biofuel_groups),
                                     .(value = sum(va_resp)),
                                     by = .(iso3c = va_iso3c, comm_code = va_comm_code,
                                            fuel = biofuel_group)][value != 0][, account := "VA"]
if (!nrow(va)) stop("[44] no VA responsibility rows for ", PLOT_YEAR, " in ", VA_RESP_CSV)

d <- rbindlist(list(pc, va), use.names = TRUE)
d[, account := factor(account, levels = ACCOUNTS)]
# `fuel` stays a RAW key -- biogasoline / biodiesel / renewable_diesel -- all the
# way through the checks, the panel scoring and the coverage report. It is only
# relabelled for display AFTER with_total() has added 40's pooled `total` chain,
# because with_total() orders its factor against CHAIN_LEVELS, which are keys.
d[, fuel    := factor(fuel, levels = names(biofuel_groups))]
d <- merge(d, items, by = "comm_code", all.x = TRUE)
if (anyNA(d$item)) stop("[44] comm_code(s) absent from items_full_bcp.csv: ",
                        paste(unique(d[is.na(item), comm_code]), collapse = ", "))

# --- the second panel unit ---------------------------------------------------
# 40's continent_of(), NOT a local read of regions_full.csv: eu_members() below
# does read the file itself, but only for a MEMBERSHIP test that has no bearing
# on which panel a country is drawn in. The panel assignment has exactly one
# source in this block, which is the point of the helper (41 filtered the file to
# current == TRUE and 42 did not, and the same country landed in two panels).
# Attached to `d` once, up here, so every downstream expression -- scores,
# pooling, coverage -- can key on either column without knowing which it got.
d[, continent := continent_of(iso3c)]
{
  cn <- sort(unique(d$continent))
  message(sprintf("[44] continents present (%d): %s", length(cn), paste(cn, collapse = ", ")))
  # continent_of() already names the countries it could not place; this says what
  # that costs the figure, which is the number a reader would ask for.
  if ("Unknown" %in% cn)
    message(sprintf("[44]   'Unknown' carries %.2f%% of the world total; it is dropped from the continent set.",
                    100 * d[continent == "Unknown", sum(value)] / d[account == "PBA", sum(value)]))
}

# --- consistency checks ------------------------------------------------------
# (1) the three accounts re-attribute ONE total -- per fuel AND pooled.
tot <- d[, .(total = sum(value)), by = .(fuel, account)]
bad <- tot[, .(dev = (max(total) - min(total)) / max(abs(total))), by = fuel][dev > TOL]
if (nrow(bad))
  warning("[44] the accounts do not share one total within: ",
          paste(sprintf("%s (rel. %.3g)", bad$fuel, bad$dev), collapse = ", "))

# (2) PBA/CBA rebuilt here must reproduce the accounts CSV, at
#     (account x iso3c x fuel) resolution.
#     This stays at COUNTRY resolution even though a set may draw continents, and
#     it is the stronger test for it: the continental bars are sums of exactly
#     these rows, so a country-level agreement implies a continental one, while
#     the reverse does not -- two countries wrong in opposite directions inside
#     one continent cancel in a continent-level check. Do not "upgrade" this to
#     the drawn unit; that would weaken it.
if (file.exists(HDI_CSV)) {
  acc <- fread(HDI_CSV)[year == PLOT_YEAR & biofuel_group %in% names(biofuel_groups)]
  ref <- rbindlist(list(
    acc[, .(ref = sum(production_based)),  by = .(iso3c, fuel = biofuel_group)][, account := "PBA"],
    acc[, .(ref = sum(consumption_based)), by = .(iso3c, fuel = biofuel_group)][, account := "CBA"]
  ), use.names = TRUE)
  cmp <- merge(d[account != "VA", .(new = sum(value)),
                 by = .(account = as.character(account), iso3c, fuel = as.character(fuel))],
               ref, by = c("account", "iso3c", "fuel"), all = TRUE)
  cmp[is.na(new), new := 0][is.na(ref), ref := 0]
  dev <- max(abs(cmp$new - cmp$ref)) / max(abs(cmp$ref), 1)
  if (dev > TOL) warning(sprintf("[44] PBA/CBA differ from the accounts CSV per fuel (max rel. %.3g) -- same switches?", dev))
  else message("[44] PBA/CBA match the accounts CSV, per fuel.")
} else message("[44] no accounts CSV to cross-check against: ", HDI_CSV)

# --- the panel sets ----------------------------------------------------------
# ONE ranking per set, shared by every row of that set's figure: the same panels,
# in the same order, appear in every fuel row -- that is what lets the rows be
# read against each other and against the pooled row on top. Ranking each row
# separately would optimise each panel but make the grid uncomparable, which is
# the whole point of the grid.
#
# Everything below takes the panel column as an ARGUMENT and renames it to
# `panel`, so one implementation serves both units. The alternative -- a country
# branch and a continent branch -- is two rankings to keep in step, and the whole
# claim of the continental figure is that it is the country figure summed.
.tot_env <- new.env(parent = emptyenv())
panel_totals <- function(col) {                       # pooled accounts per panel
  if (is.null(.tot_env[[col]])) {
    t <- dcast(d[, .(value = sum(value)), by = c(col, "account")],
               as.formula(paste(col, "~ account")), value.var = "value", fill = 0)
    setnames(t, col, "panel")
    t[, panel := as.character(panel)]
    .tot_env[[col]] <- t
  }
  .tot_env[[col]]
}

# Scores are ALWAYS normalised on the WORLD total, never on the pool. Otherwise
# "share" would mean one thing for the producer set and another for the EU set,
# and the two subtitles would quietly stop being comparable. Restricting to the
# pool happens after the score, in pick_set().
# The normalisation is also unit-invariant: the denominators group by
# (account, fuel) and sum over every panel, so a continent's "share" is the sum
# of its countries' shares and reads on the same axis as theirs.
score_panels <- function(how, col) {
  tot <- panel_totals(col)
  switch(
    how,
    # max over (fuel x account) of the panel's share of THAT fuel's world
    # total. Normalising by the fuel's own total is what stops the two big fuels
    # from deciding a whole panel set on their own.
    share = {
      s <- d[, .(v = sum(value)), by = c(col, "account", "fuel")]
      s[, wtot := sum(v), by = .(account, fuel)]
      out <- s[wtot > 0, .(score = max(v / wtot)), by = c(col)]
      setnames(out, col, "panel")
      out[, panel := as.character(panel)][]
    },
    max   = tot[, .(panel, score = pmax(PBA, CBA, VA))],
    mean  = tot[, .(panel, score = (PBA + CBA + VA) / 3)],
    {
      if (!how %in% names(tot))
        stop("[44] unknown score '", how, "': use PBA / CBA / VA / max / mean / share.")
      tot[, .(panel, score = get(how))]
    }
  )
}

eu_members <- function() {
  reg <- fread(need(EU_SOURCE, "the repo's inst/ directory"))
  if (!all(c("iso3c", "EU27") %in% names(reg)))
    stop("[44] ", EU_SOURCE, " lacks iso3c / EU27 -- cannot resolve the EU set.")
  if ("current" %in% names(reg)) reg <- reg[current == TRUE]   # as 07_02 does
  m <- unique(reg[EU27 == TRUE, iso3c])
  if (EU_WITH_GBR) m <- union(m, "GBR")
  if (!length(m)) stop("[44] no EU27 members resolved from ", EU_SOURCE, ".")
  m
}

pick_set <- function(nm, spec) {
  col  <- UNIT_COL[[spec$unit]]
  tot  <- panel_totals(col)
  pool <- switch(spec$pool,
                 world = unique(tot$panel),
                 EU    = eu_members(),          # unit == "country", enforced above
                 stop("[44] set '", nm, "': unknown pool '", spec$pool,
                      "' -- use 'world' or 'EU'."))
  pool <- setdiff(pool, spec$drop)              # 48's DROP_REGIONS, per set
  cand <- merge(tot[panel %in% pool], score_panels(spec$select, col),
                by = "panel", all.x = TRUE)
  if (!nrow(cand))
    stop("[44] set '", nm, "': no ", UNIT_ONE[[spec$unit]],
         " of the pool carries any ", STRESSOR, ".")
  cand[is.na(score), score := 0]
  setorder(cand, -score)
  sel <- cand[seq_len(min(spec$n, .N))]
  # is.finite(): n = Inf means "all of them", so falling short of it is not a
  # shortfall to warn about -- .N IS the answer in that case.
  if (is.finite(spec$n) && nrow(sel) < spec$n)
    warning("[44] set '", nm, "': only ", nrow(sel), " of the ", spec$n, " ",
            UNIT_MANY[[spec$unit]], " asked for exist in the pool.")
  # --- must_include: panels the set shows whatever the ranking says -----------
  # Applied AFTER the top-n and after the shortfall warning, so `n` keeps meaning
  # "how many the RANKING picks" and a forced panel is an addition, never a
  # substitution: the ranked selection is bit-for-bit what it was before the
  # entry was added. The rows come out of `cand`, i.e. out of the same pool with
  # the same score, so the ordering below treats them exactly like the rest and
  # they land AMONG the ranked panels rather than at one end -- which is the
  # whole point of forcing them in instead of drawing them in a figure of their
  # own. `forced` rides along so the caption can name them (a subtitle saying
  # all N were "selected by <criterion>" would otherwise be false) and so the
  # order file records which panel came from where.
  sel[, forced := FALSE]
  want <- setdiff(spec$must_include, sel$panel)
  if (length(want)) {
    have <- intersect(want, cand$panel)
    if (length(setdiff(want, have)))
      warning("[44] set '", nm, "': must_include ",
              paste(setdiff(want, have), collapse = ", "),
              " is not in pool '", spec$pool, "' or carries no ", STRESSOR,
              " -- NOT drawn.")
    if (length(have)) {
      add <- cand[panel %in% have]          # a fresh table; := on it is safe
      add[, forced := TRUE]
      sel <- rbind(sel, add)
      message("[44] set '", nm, "': ", paste(have, collapse = ", "),
              " added by must_include (outside the top ", spec$n, " on '",
              spec$select, "') -- ", nrow(sel), " panels in total.")
    }
  }
  # ...then re-sort THOSE panels left-to-right. sel$panel becomes the factor
  # levels at plot time, so this line alone sets the x axis of every row.
  # "alpha" is 48's REGION_ORDER vocabulary and is the one ordering that sorts
  # ASCENDING and on a name rather than on a score -- hence its own branch
  # instead of another entry in the switch. order_by is NA there because there is
  # no score behind the order, and writing the rank in would imply one.
  if (identical(spec$order, "alpha")) {
    setorder(sel, panel)
    sel[, order_by := NA_real_]
  } else {
    sel[, order_by := switch(spec$order,
                             rank = score,
                             max  = pmax(PBA, CBA, VA),
                             mean = (PBA + CBA + VA) / 3,
                             get(spec$order))]
    setorder(sel, -order_by)
  }
  sel[]
}

# Coverage is reported PER FUEL, not just pooled: a pooled "90% covered" can sit
# on top of a renewable-diesel row covering barely half its own total. It is also
# reported PER SET, and the numbers are necessarily lower than the 18-country
# figure's -- 5 and 8 countries do not cover the world, and the figures must not
# be described as if they did. For the EU set read the coverage as "share of the
# WORLD total that these eight members carry", not as EU completeness.
# The continent set should print 100% on every line (or 100% less whatever `drop`
# removed). If it does not, a panel went missing between the score and the
# selection -- that is what this report is FOR at that unit, and the reason it
# runs for a complete set at all instead of being skipped as trivially true.
report_coverage <- function(nm, spec, sel) {
  col <- UNIT_COL[[spec$unit]]
  cov <- merge(d[get(col) %in% sel$panel, .(shown = sum(value)), by = .(fuel, account)],
               d[, .(all = sum(value)), by = .(fuel, account)],
               by = c("fuel", "account"))
  cov[, pct := 100 * shown / all]
  message(sprintf("[44] set '%s': %d %s from pool '%s', selected by '%s', ordered by '%s': %s",
                  nm, nrow(sel), UNIT_MANY[[spec$unit]], spec$pool, spec$select,
                  spec$order, paste(sel$panel, collapse = " ")))
  for (f in levels(d$fuel)) {
    x <- cov[fuel == f]
    if (!nrow(x)) next
    message(sprintf("         %-17s %s", chain_labeller(f),
                    paste(sprintf("%s %.0f%%", x$account, x$pct), collapse = " | ")))
  }
}

sets <- lapply(names(PANEL_SETS), function(nm) pick_set(nm, PANEL_SETS[[nm]]))
names(sets) <- names(PANEL_SETS)
message("[44] coverage of each fuel's world total:")
for (nm in names(sets)) report_coverage(nm, PANEL_SETS[[nm]], sets[[nm]])

# --- persist the ordered country vectors so 45 can follow them ---------------
# 45 (the HDI trade-split figure) reads this file and shows the SAME countries in
# the SAME left-to-right order instead of deriving its own sets. ONE file with a
# `set` column, not one file per set: 45 loops over whatever sets it finds, so a
# further country entry in PANEL_SETS needs no change on its side and no new file
# name in 40. It matters that the `eu` set keeps VA in its ranking -- VA is the one
# account that genuinely diverges from PBA/CBA, so ranking on it pulls in
# value-capturing hubs (NLD) that a PBA/CBA-only ranking would drop. HDI is
# bounded between PBA and CBA and never selects a country those two miss, so
# letting the HDI figure inherit these sets loses nothing on its side.
#
# ONLY unit == "country" sets go in. 45 joins this file's `iso3c` against a
# country-keyed CSV and draws one figure per block it finds; a continent name in
# a column called `iso3c` would drop out of that join and 45 would abort on "none
# of 44's countries appear in ...". The `unit` column is written anyway, and 45
# filters on it as well, so neither the writer nor the reader alone is load-
# bearing. Column name kept as `country_set` rather than `panel_set`: order files
# already on disk carry it, and 45 reads it by name.
country_sets <- Filter(function(nm) PANEL_SETS[[nm]]$unit == "country", names(sets))
if (!length(country_sets)) {
  # Deliberately NOT writing an empty file: 45 tests file.exists() and would read
  # a zero-row order as "44 selected nothing" rather than as "44 drew no country
  # figure". A stale file is the lesser evil and the message says so.
  message("[44] no country set in PANEL_SETS -- ORDER_CSV left untouched; 45 will use ",
          "whatever is already at ", ORDER_CSV, " or fall back to its own ranking.")
} else {
  fwrite(rbindlist(lapply(country_sets, function(nm)
    sets[[nm]][, .(country_set = nm, set_tag = PANEL_SETS[[nm]]$tag,
                   unit = "country", position = .I,
                   iso3c = panel, rank_by = score, order_by,
                   # TRUE = in the figure by must_include, not by rank. 45 and
                   # 47b follow `position` and ignore this, as they ignore any
                   # column they do not name -- it is here so the order file
                   # still explains itself when a set holds a panel its own
                   # ranking would not have chosen.
                   forced)])), ORDER_CSV)
  message(sprintf(">>> [44] wrote the country order for %d of %d set(s) (%s) -> %s",
                  length(country_sets), length(sets),
                  paste(country_sets, collapse = ", "), ORDER_CSV))
  skipped <- setdiff(names(sets), country_sets)
  if (length(skipped))
    message("[44]   not written (not country-keyed, 45 cannot draw them): ",
            paste(skipped, collapse = ", "))
}

# --- the pooled chain, as a fourth row ---------------------------------------
# This script used to write TWO figures per set: the fuels pooled, and one row per
# fuel. They are now one grid whose top row is the pool. The pooled row is 40's
# `total` chain -- a duplicate of every row under a fourth key, aggregated by the
# same group-by as the chains, so it is arithmetically the pooled figure it
# replaces rather than a second computation of it.
#
# WHERE THIS SITS IS THE WHOLE POINT. Everything above -- the conservation checks,
# score_panels(), pick_set(), report_coverage(), the order file 45 reads -- runs
# on the three chains alone and is untouched, byte for byte, by this change. Move
# the call any earlier and every one of those would see the world twice.
#
# (The panel SCORE would in fact survive it: `share` takes a max over the fuel x
# account cells, and a pooled share is a wtot-weighted mean of the three chain
# shares and so can never exceed their max. Relying on that is still the wrong
# habit -- the coverage denominators just below it would double.)
d <- with_total(d, "fuel")
d[, fuel := factor(chain_labeller(fuel), levels = chain_labeller(levels(fuel)))]

# --- pool the small items into "Other" ---------------------------------------
# An item is kept if it reaches the threshold in any CELL of the figure, a cell
# being an account x fuel -- and `fuel` now includes the pooled `total` row, so
# an item large in the pool but small in every chain still earns its colour.
#
# BOTH THRESHOLDS SURVIVE, ONE PER ROW ----------------------------------------
# min_pooled (2%) and min_byfuel (3%) were the thresholds of the two figures this
# grid replaces, and they are NOT interchangeable: 3% of one chain is a different
# bar from 2% of the pool, which is why they were set apart in the first place.
# So min_share is a VECTOR over the fuel rows -- min_pooled on Total, min_byfuel
# on the three chains -- and the keep-set comes out as exactly the UNION of the
# two old keep-sets. That union is what the palette was already built over, so
# the legend is the legend the two figures already shared; nothing enters it that
# was not already drawn, and nothing that was drawn falls out.
#
# Collapsing the two to a single number was the obvious move and is wrong both
# ways round: 3% everywhere silently drops items the pooled figure showed at
# 2.5%, and 2% everywhere pads every chain row with items too small to see.
#
# `scope` is the data the SHARES are computed on; `dat` is the data relabelled.
# Keeping them separate is what lets one set take its shares against the world and
# another against the countries it shows, without either one deciding what the
# other draws.
# `min_share` is either ONE number, applied to every cell, or a vector NAMED by
# the levels of `fuel` -- the second form is what gives the Total row its own
# threshold. It is carried as a column rather than recycled, so a cell can never
# be matched against the wrong entry by position.
pool_items <- function(dat, cell, min_share, scope = dat, tag = "") {
  share <- scope[, .(v = sum(abs(value))), by = c(cell, "item")][, share := v / sum(v), by = cell]
  if (length(min_share) == 1L) {
    share[, thr := as.numeric(min_share)]
  } else {
    if (!"fuel" %in% cell)
      stop("[44] pool_items(): a per-row min_share needs `fuel` among the cell keys.")
    share[, thr := unname(min_share[as.character(fuel)])]
    if (anyNA(share$thr))
      stop("[44] pool_items(): no threshold for fuel level(s) ",
           paste(sort(unique(as.character(share$fuel[is.na(share$thr)]))), collapse = ", "))
  }
  keep <- share[share >= thr, unique(item)]
  out  <- copy(dat)
  out[!item %in% keep, `:=`(item = "Other", chain = "Other", stage = 3L, comm_code = "Other")]
  message(sprintf("[44] %-11s %-18s %2d items kept (>= %s of a cell), %d pooled as 'Other'.",
                  tag, paste0("[", paste(cell, collapse = " x "), "]"), length(keep),
                  paste(sprintf("%.0f%%", 100 * sort(unique(share$thr))), collapse = "/"),
                  uniqueN(share$item) - length(keep)))
  out
}

# --- the plotting table, one per panel set -----------------------------------
# Under item_scope = "world" the shares are computed on ALL countries and only
# then subset to the set, so a country outside the set cannot be silently
# excluded from the denominator -- the original behaviour. Under "set" the
# denominator IS the set, which is the point: the threshold then describes the
# figure the reader is looking at rather than a world the figure does not show.
# `panel` is assigned BEFORE the pooling, so pool_items() and everything after it
# never learn which unit they are working at -- and the aggregation to continents
# happens in the same group-by that already summed the item rows, not in a
# separate pass that could disagree with it.
message("[44] item pooling (per set):")
figs <- list()
for (nm in names(sets)) {
  spec  <- PANEL_SETS[[nm]]
  col   <- UNIT_COL[[spec$unit]]
  sel   <- sets[[nm]]
  rows  <- d[get(col) %in% sel$panel][, panel := get(col)]
  scope <- switch(spec$item_scope,
                  world = d,
                  set   = rows,
                  stop("[44] set '", nm, "': item_scope must be 'world' or 'set', not '",
                       spec$item_scope, "'."))
  
  # the chains take min_byfuel, the pooled row keeps min_pooled -- see above
  min_by <- setNames(rep(spec$min_byfuel, nlevels(d$fuel)), levels(d$fuel))
  min_by[[chain_labeller(TOTAL_KEY)]] <- spec$min_pooled
  
  q <- pool_items(rows, c("account", "fuel"), min_by, scope, nm)[
    , .(value = sum(value)), by = .(panel, account, fuel, item, comm_code, chain, stage)]
  q[, panel := factor(panel, levels = sel$panel)]
  figs[[nm]] <- q
}

# The keep-sets DIFFER between sets, so the stack order and the palette are fixed
# over the UNION of everything drawn. Without that, palm oil could be the second
# segment of the producer figure and the fifth of the EU one, in a different
# colour, and the two would stop being readable against each other. An item
# pooled into grey in one set's figure and coloured in another's is expected --
# that is what a per-set "Other" means.
p <- rbindlist(figs, use.names = TRUE)

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
lvl <- make_levels(p)
# `p` exists only to derive the shared levels; the per-set tables in `figs` are
# what gets drawn, so the factor goes onto those (by reference).
for (nm in names(figs))
  figs[[nm]][, item := factor(item, levels = as.character(lvl$item))]

# --- colours: 19's feedstock palette, shaded by processing stage -------------
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
    warning("[44] no colour for '", it, "' (", cc, ") -- grey. Map it in ANCHOR_FEEDSTOCK ",
            "or EXTRA_ITEM_COLORS.")
    unname(feedstock_color_map[["Other"]])
  }, character(1))
  setNames(cols, as.character(lvl$item))
}
# ONE palette over every set's keep-set: same item -> same colour in every figure.
pal <- build_palette(unique(lvl[, .(item, comm_code, chain, stage)], by = "item"))

# --- the shared plot skeleton ------------------------------------------------
# Every set's figure is the same bar grammar (x = account, stack = item, one
# panel per country, one facet row per fuel including the pooled Total row).
#
# WHY WE DRAW THE LEGEND KEY OURSELVES (key_glyph) ----------------------------
# ggpattern does not render its hatch into the legend keys under
# geom_col_pattern + svglite: routing the pattern to the fill guide via
# guide_legend(override.aes = list(pattern = ..)) still emits SOLID keys, no
# stripe geometry at all, which leaves the three fuels indistinguishable from the
# feedstocks they sit dE 4-7 from. So the swatch is painted with plain grid grobs,
# which svglite serialises cleanly: every key is a filled rect edged like the bars
# (SEG_BORDER, so NA leaves it unedged), and the three FUEL keys (detected by their exact fill -- the collisions are close
# but never identical hexes) get the same 45-deg white hatch as the bars.
`%||%` <- function(a, b) if (is.null(a)) b else a
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

# The alternating background band used to live here as country_bands(), keyed on
# `iso3c`. 40 already carries the identical layer generalised over the faceting
# column (panel_bands(p, panel_col, separator, band_fill)) because 48 needed it
# for continents -- so the local copy is gone and base_plot() calls the shared
# one on `panel`. One band implementation for 44, 45 and 48; the comment about
# why the +-Inf corners live in the DATA rather than inside aes() is over there.

# Strip labels: a 3-letter ISO code never needs wrapping, "South America" does.
# label_wrap_gen() rather than an abbreviation table -- an abbreviation is one
# more thing that can disagree with 48's panel names.
strip_labeller <- function(unit) label_wrap_gen(width = STRIP_WRAP[[unit]])

# legend_rows: the 18-country figure was ~17in wide and held its keys in 3 rows.
# At 5-7 countries the canvas is ~10in and the same 3 rows silently clip the last
# keys, so the row count follows the width instead of being fixed. ~2.4in per key
# is measured off the longest labels ("Rape and Mustard Oil", "Fats, Animals, Raw")
# AT 11pt, so it scales with BASE_SIZE -- otherwise raising the type quietly
# overflows the legend row instead of adding a row.
KEY_WIDTH_IN <- 2.4 * BASE_SIZE / 11
legend_rows <- function(n_keys, width_in)
  max(3L, as.integer(ceiling(n_keys / max(3, floor(width_in / KEY_WIDTH_IN)))))

# How many keys a figure actually shows: every level under LEGEND_KEEP_ALL, only
# the items it draws otherwise. Counting levels in the second case would reserve
# rows for keys that are never printed.
n_keys <- function(q, lvl) if (LEGEND_KEEP_ALL) nrow(lvl) else uniqueN(as.character(q$item))

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
    # LEGEND_KEEP_ALL = TRUE (drop = FALSE) gives BOTH sets the SAME legend, so a
    # colour can be looked up once and carried between the figures -- at the cost
    # of keys for items that appear only in the other set (sugar cane has no
    # segment in the EU figure). FALSE trims each legend to what its own figure
    # draws: shorter, but then the two legends are different objects and the
    # reader has to re-learn the palette per figure.
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
          # the panel separator: a rule around each panel, drawn OVER the band
          # but under nothing else, so it reads as the boundary rather than as a
          # frame around the plotting area.
          panel.border       = if (SEPARATOR %in% c("border", "both"))
            element_rect(fill = NA, colour = SEP_COLOUR, linewidth = 0.3)
          else element_blank(),
          panel.spacing.x    = unit(PANEL_GAP, "pt"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank(),
          axis.text.x        = element_text(size = BASE_SIZE - 3, angle = 90,
                                            hjust = 1, vjust = 0.5),
          plot.title         = element_text(size = BASE_SIZE + 2, face = "bold"),
          plot.subtitle      = element_text(size = BASE_SIZE - 4, colour = "grey30"),
          plot.caption       = element_text(size = BASE_SIZE - 5, colour = "grey40"))
}

# What the canvas no longer says has to go somewhere, but NOT down the console:
# four figures x a six-line subtitle buries every diagnostic this script prints,
# and console text is the worst possible place to copy a caption from anyway.
# Collected here, written once as a text file next to the SVGs, keyed by the
# figure's own file name so a caption cannot be pasted under the wrong figure.
# --- fitting the title block to the canvas -----------------------------------
# ggplot2 does NOT wrap plot.subtitle: a line longer than the canvas is drawn
# past the edge and lost. The longest line of these subtitles is ~360 characters,
# which needs 22.6in at 9pt -- so it was already being cut off on the 16.6in
# 18-country figure, and at BASE_SIZE it would be worse. Hard-wrap it instead.
#
# chars per line = width_in * 72 / (0.5 * pt): a proportional glyph averages
# about half an em. MARGIN keeps it clear of the plot margins.
WRAP_MARGIN <- 0.92
wrap_lines <- function(txt, width_in, pt) {
  if (!nzchar(txt)) return(txt)
  n <- max(40L, as.integer(WRAP_MARGIN * width_in * 72 / (0.5 * pt)))
  paste(vapply(strsplit(txt, "\n", fixed = TRUE)[[1]],
               function(par) if (nzchar(par)) paste(strwrap(par, width = n), collapse = "\n") else "",
               character(1), USE.NAMES = FALSE),
        collapse = "\n")
}

# Height the wrapped block will occupy, so the CANVAS grows with the text rather
# than the panels shrinking under it. Without this, turning the titles on steals
# ~2in from the drawing area and the bars come out squat.
text_height_in <- function(txt, pt, lead = 1.35)
  if (nzchar(txt)) length(strsplit(txt, "\n", fixed = TRUE)[[1]]) * pt * lead / 72 else 0

lab_block <- function(stem, ttl, sub)
  if (SHOW_TITLES) character(0) else c(paste0("## ", stem, ".svg"), ttl, sub, "")

# One caption file PER UNIT, keyed the same way the SVGs are, so a continental
# caption cannot be pasted under a country figure: the two figures answer the
# same question at different resolutions and their subtitles differ only in a
# noun, which is exactly the pair a reader would mix up.
write_captions <- function(caps, tag) {
  if (!length(caps)) return(invisible(NULL))
  path <- file.path(PLOT_DIR, sprintf("captions_%s_%s_%s_%s_%d.txt",
                                      tag, STAG, VA_BASE, ATAG, PLOT_YEAR))
  writeLines(caps, path)
  message(sprintf(">>> [44] title/subtitle not drawn; caption text -> %s", path))
  invisible(path)
}

CAPTION <- sprintf("%s allocation, %s value-added base; biogasoline + biodiesel + renewable diesel.",
                   allocation, VA_BASE)
# "where the pressure is exerted", not "where the impact occurs": the items name
# the PRODUCTION an impact is attributed to. Where the damage lands is a property
# of the indicator, and for ibif_total most of it (CO2, NH3) is not local. 45
# carries the full caveat in NONLOCAL_PATHWAYS; this figure has no room for it,
# so it states the weaker claim that is true under every indicator.
LEGEND_NOTE <- paste0("One hue per feedstock chain (as in the footprint figures), shaded dark for the ",
                      "primary commodity and light for the processed one:\nPBA/CBA items are where the ",
                      "pressure is exerted, VA items are where the value is captured. Fuel segments are hatched.")

# --- the figures: two per panel set ------------------------------------------
# The subtitle must say HOW the panels were picked -- "the 5 largest" alone is
# not reproducible, and under 'share' it is not even a ranking on the axis the
# reader is looking at. spec$label carries the selection criterion; the ordering
# criterion is spelled out from spec$order.
ORDER_NOTE <- function(spec)
  switch(spec$order,
         rank  = "the same score",
         alpha = "name",
         max   = "their largest pooled account",
         mean  = "their mean pooled account",
         sprintf("pooled %s", spec$order))

# The captions are collected PER UNIT and written to one file each, so `caps` is
# a list keyed by unit rather than a single vector.
caps <- setNames(vector("list", length(UNIT_COL)), names(UNIT_COL))
for (nm in names(sets)) {
  spec <- PANEL_SETS[[nm]]
  sel  <- sets[[nm]]
  unit <- spec$unit
  UNIT_NS   <- UNIT_MANY[[unit]]   # "countries" / "continents"
  # A forced panel was NOT selected by the criterion, so the clause that names
  # the criterion has to say so -- "Countries (8) selected by the largest of
  # their three accounts" is simply false about Belgium. Same discipline as the
  # DROPPED clause below: the subtitle describes the figure that was drawn.
  # `%in% TRUE`, not `which(sel$forced)`: it is NA- and NULL-safe, so an order
  # of an older shape (no `forced` column) yields character(0) rather than error.
  FORCED    <- as.character(sel$panel[sel$forced %in% TRUE])
  FORCED_NOTE <- if (length(FORCED))
    sprintf(", plus %s, shown regardless of rank", paste(FORCED, collapse = ", ")) else ""
  SET_NOTE  <- sprintf("%s%s; ordered by %s", spec$label, FORCED_NOTE, ORDER_NOTE(spec))
  SET_TITLE <- sprintf("%d %s", nrow(sel), spec$title)
  # HOW THE PANELS WERE PICKED, in one clause, because "the 5 largest" alone is
  # not reproducible. Two frames, because a set with n = Inf did not SELECT
  # anything: writing "Continents (6) selected by ..." under a figure that shows
  # every continent claims a filter that is not there, and a reader who believes
  # it will not add the bars up.
  # The completeness claim is CHECKED, not asserted. `drop` is a list of panels
  # to leave out, and if one of them actually carries impact then the bars do NOT
  # sum to the world total -- so the caption may only make that claim when the
  # dropped panels are empty, and must otherwise say what is missing and how big
  # it is. This is the same discipline as the SCOPE_NOTE clause below: a figure
  # that describes itself as complete when it is not is the failure mode these
  # subtitles exist to prevent.
  pcol    <- UNIT_COL[[unit]]
  DROPPED <- intersect(spec$drop, unique(as.character(d[[pcol]])))
  # The largest share over the THREE accounts, not PBA's: a panel can be empty in
  # production and carry consumption (or value), and quoting the account that
  # happens to be zero would understate exactly the case worth reporting.
  DROP_SHARE <- if (length(DROPPED)) {
    s <- merge(d[get(pcol) %in% DROPPED, .(x = sum(value)), by = account],
               d[, .(a = sum(value)), by = account], by = "account")
    max(s$x / s$a)
  } else 0
  PICK_NOTE <- if (is.finite(spec$n)) {
    sprintf("%s (%d) selected by %s", cap1(UNIT_NS), nrow(sel), SET_NOTE)
  } else if (!length(DROPPED)) {
    sprintf("All %d %s -- %s; ordered by %s",
            nrow(sel), UNIT_NS, spec$label, ORDER_NOTE(spec))
  } else {
    sprintf(paste0("All %d %s except %s, which carries up to %.2f%% of an account ",
                   "and is NOT drawn -- the panels therefore fall short of the ",
                   "world total by that much; ordered by %s"),
            nrow(sel), UNIT_NS, paste(DROPPED, collapse = ", "),
            100 * DROP_SHARE, ORDER_NOTE(spec))
  }
  PICK_NOTE2 <- if (is.finite(spec$n))
    sprintf("selected by %s", SET_NOTE)
  else if (!length(DROPPED))
    sprintf("all of them, ordered by %s", ORDER_NOTE(spec))
  else
    sprintf("all except %s, ordered by %s",
            paste(DROPPED, collapse = ", "), ORDER_NOTE(spec))
  # The threshold means nothing without its denominator, and the denominator now
  # differs between the sets. Never drop this clause from a caption: two figures
  # whose "Other" is built on different totals but described identically is
  # exactly the kind of quiet incomparability the rest of this block avoids.
  # Under item_scope = "set" the noun follows the UNIT: "across the continents
  # shown" is a different denominator from "across the countries shown", and a
  # caption that names the wrong one is the failure this clause exists to stop.
  SCOPE_NOTE <- switch(spec$item_scope,
                       world = "of the world total",
                       set   = sprintf("across the %s shown", UNIT_NS))
  
  # --- the figure: Total on top, then one row per biofuel chain --------------
  # facet_grid(fuel ~ panel) with switch = "both": fuel labels on the LEFT
  # (rotated), panel names on the BOTTOM -- same panel placement as 45. The top
  # row is 40's pooled `total` chain, so the figure that used to be written
  # separately as `responsibility_<unit>_item_...` is now this grid's first row
  # and the `_byfuel_` file is gone.
  #
  # scales = "free_y" frees the axis PER ROW, not per panel, so within a row the
  # panels stay directly comparable while renewable diesel still fills its row
  # instead of collapsing to a flat line. With Total in the grid that is no
  # longer a nicety: the pooled row is the sum of the three beneath it and a
  # shared axis would flatten all of them. Bar HEIGHTS MAY NOT BE COMPARED ACROSS
  # ROWS, including against Total; the axis labels carry the scale and the
  # subtitle says so.
  #
  # Width still has to fit the subtitle and the legend, and neither shrinks with
  # the panel count -- hence the floor. Below ~11in the subtitle wraps mid-clause
  # and the legend spills off the canvas. NB pattern_spacing is relative to the
  # plot, so a width change shifts the stripe pitch -- check the hatch on the fuel
  # segments after the first run. PANEL_W_BYFUEL, not a literal: a continent name
  # needs a wider panel than a 3-letter code even after wrapping.
  # h is the DRAWING area; the title block is measured and added below, so the
  # panels are the same size whether or not the text is drawn -- which is what
  # makes cropping the text off give a figure identical to SHOW_TITLES = FALSE.
  w <- max(if (SHOW_TITLES) 11 else 10, 5 + PANEL_W_BYFUEL[[unit]] * nrow(sel))
  h <- 2.6 + 2.7 * max(nlevels(figs[[nm]]$fuel), 1)      # 4 rows -> 13.4in
  
  ttl <- sprintf("%s responsibility for bio-based transport fuels: %s, %d",
                 META$short_label, SET_TITLE, PLOT_YEAR)
  sub <- sprintf(paste0("PBA production-based | CBA consumption-based | VA value-added-based ",
                        "(", VA_DEFINITION_NOTE, "): three attributions of one and the same total, ",
                        "per fuel and pooled.\n",
                        "Top row is all three chains pooled; %s in every row. Items below ",
                        "%.0f%% of an account x fuel cell (%.0f%% in the pooled row) %s pooled ",
                        "as 'Other'.\n",
                        "Y AXIS IS FREE PER ROW -- compare bars within a row, not across rows, ",
                        "and not against Total. %s"),
                 PICK_NOTE, 100 * spec$min_byfuel, 100 * spec$min_pooled,
                 SCOPE_NOTE, LEGEND_NOTE)
  # The free-y warning lives in this string. Dropping it from the canvas without
  # carrying it into the caption invites exactly the cross-row comparison the
  # figure cannot support.
  #
  # <unit> in the stem, so a continent set with the same tag cannot overwrite a
  # country one. The stem is the POOLED figure's, not the old `_byfuel_` one:
  # this grid is the successor to the headline figure, and anything referring to
  # that name -- a manuscript, a Makefile -- should keep resolving to it.
  stem <- sprintf("responsibility_%s_item_%s_%s_%s_%s_%d",
                  unit, spec$tag, STAG, VA_BASE, ATAG, PLOT_YEAR)
  caps[[unit]] <- c(caps[[unit]], lab_block(stem, ttl, sub))   # caption file gets it UNWRAPPED
  
  # wrap to the canvas, then add what the wrapped block needs to the height
  ttlw <- wrap_lines(ttl, w, BASE_SIZE + 2)
  subw <- wrap_lines(sub, w, BASE_SIZE - 4)
  if (SHOW_TITLES)
    h <- h + text_height_in(ttlw, BASE_SIZE + 2) +
    text_height_in(subw, BASE_SIZE - 4) + 0.25
  
  gg <- base_plot(figs[[nm]], legend_rows(n_keys(figs[[nm]], lvl), w)) +
    facet_grid(fuel ~ panel, scales = "free_y", switch = "both",
               labeller = labeller(panel = strip_labeller(unit), fuel = label_value)) +
    theme(strip.text.y.left = element_text(face = "bold", size = BASE_SIZE, angle = 90),
          panel.spacing.y   = unit(8, "pt")) +
    labs(title    = if (SHOW_TITLES) ttlw else NULL,
         subtitle = if (SHOW_TITLES) subw else NULL,
         caption  = if (SHOW_CAPTION) CAPTION else NULL)
  
  save_svg(stem, gg, width = w, height = h)
}

for (u in names(caps)) write_captions(caps[[u]], paste0(u, "_item"))