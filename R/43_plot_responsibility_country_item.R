# =============================================================================
# 43_plot_responsibility_country_item.R
# TWO figures: the production- (PBA), consumption- (CBA) and value-added-based
# (VA) accounts as three ADJACENT stacked bars per country, at COUNTRY x ITEM
# resolution, for a single year.
#
#   [1] responsibility_country_item_*.svg          -- the three fuels POOLED
#       x = country (top N, largest first), three bars each | y = impact | fill = item
#       items below MIN_ITEM_SHARE_POOLED of an account are pooled as "Other"
#
#   [2] responsibility_country_item_byfuel_*.svg   -- ONE PANEL ROW PER BIOFUEL
#       facet_grid(fuel ~ country): the same top-N countries, in the same order,
#       in every row, so the rows can be read against one another. The y axis is
#       FREE PER ROW (scales = "free_y"): renewable diesel is an order of
#       magnitude below biodiesel and would otherwise collapse to a flat line, so
#       each row shows its own COMPOSITION at its own scale. Bar heights are
#       therefore NOT comparable across rows -- figure [1] is the one to read for
#       cross-fuel magnitude. Items are pooled per (account x fuel), so an item
#       that is small overall but large within one fuel gets its own colour here
#       (and only here); the palette is shared, so an item that appears in both
#       figures has the same colour in both.
#
# Both figures come out of ONE computation: D is built per fuel and figure [1]
# sums over the fuel dimension, which is exactly what this script did before the
# split existed -- figure [1] is unchanged.
#
# WHY THIS SCRIPT COMPUTES INSTEAD OF ONLY PLOTTING ---------------------------
# The accounts CSV is keyed by iso3c only -- it has no item dimension -- so the
# PBA/CBA bars cannot be read off it. Both are margins of one bilateral matrix
#     D = diag(f) B[, bf] Y_bf     (node i x consuming country c)
#     f = e / x , B = L_<allocation> , Y_bf = biofuel final demand
# which is rebuilt here at node resolution through the shared builder in
# 00_responsibility_helpers.R, so the math and switches cannot drift from 40's:
#     PBA(i) = rowSums(D)          impact at the producing node i
#     CBA(c) = colSums(D)          impact routed to the consumer of the biofuel
# The VA bar is NOT recomputed: it is read from 41's ex_tls responsibility CSV,
# which is already resolved to (va_iso3c, va_comm_code). All three are then
# checked against the country-level accounts CSV and against each other.
#
# ITEM SEMANTICS -- read before interpreting the stacks -----------------------
#   PBA / CBA   the node where the impact PHYSICALLY OCCURS (the source item).
#               PBA books it to the producing country, CBA to the consuming one.
#   VA          the sector that CAPTURES the value -- the item that earns, not
#               the item that emits.
# Each account sums to the SAME grand total (one footprint, three attributions);
# only its distribution over (country, item) moves.
#
# WHY NOT THE FEEDSTOCK DIMENSION USED BY 18_01b / 19_01b ---------------------
# Those scripts stack by FIRST-DEGREE FEEDSTOCK (fp_feedstock rolls a feedstock's
# whole upstream chain into the item that enters the biofuel), which is why they
# never face this colouring problem: the crop never appears next to its oil. That
# collapse is wrong HERE, though -- the primary/secondary split is precisely the
# contrast this figure draws. The impact lands on the PRIMARY commodity (crop),
# the value on the SECONDARY one (oil, fuel), so folding them together would
# erase the very thing PBA/CBA vs VA is meant to show.
#
# COLOURS -- 19's fixed feedstock palette, shaded by stage --------------------
# Item colours are ANCHORED on feedstock_color_map (19) so a feedstock keeps the
# same colour here as in every other figure in the repo:
#   item in feedstock_color_map    -> that colour, EXACTLY (Palm Oil, Sugar cane, ...)
#   primary crop of such a feedstock -> the same colour, DARKENED (Oil, palm fruit
#                                     is a dark palm-oil green; Soyabeans a dark
#                                     soyabean-oil purple)
#   cake / by-product              -> the same colour, LIGHTENED
#   non-feedstock items (Grazing)  -> 19's reserve slots
# So one hue = one chain, and the shade tells you the processing stage: dark =
# where the impact is, light = where the value is.
#
# THE FUELS ARE HATCHED, NOT RE-COLOURED -- and why -----------------------------
# The three fuels keep `fuel_colors` (19) and sit at the BASE of every stack --
# they can only appear in the VA bar (refining causes no direct pressure but does
# capture value). fuel_colors and feedstock_color_map were never designed to share
# a stack and they COLLIDE HARD once the stage shading above is applied:
#     Biodiesel        #5B2C6F  vs  Soyabeans   (dark Soyabean Oil)     dE 8.9
#     Biogasoline      #D4A017  vs  Rape and Mustard Oil                dE 11.7
#     Renewable diesel #9B59B6  vs  Soyabean Cake (light Soyabean Oil)  dE 14.3
# Under ~15 dE those are the same colour at legend-key size -- and they are not
# random pairings: they are the soy and rape chains, i.e. exactly the feedstocks
# that dominate the fuels they collide with, so the two segments are frequently
# ADJACENT in the same VA stack.
# Re-colouring the fuels does not fix this. `fill` is already carrying two
# variables (chain -> hue, stage -> lightness), and the +/- SHADE step fills in
# the lightness neighbourhood around EVERY feedstock hue, so any third hue picked
# for a fuel lands inside some feedstock's shade family. There is no free hue.
# The free channel is TEXTURE. The fuels are therefore drawn with a horizontal
# hatch and everything else is drawn plain, which encodes fuel-vs-
# feedstock on an axis orthogonal to hue and stays correct no matter which
# feedstocks show up in a given year / stressor / allocation. The old fix -- a
# near-black outline on fuel segments only -- is GONE; every segment now gets the
# same white outline. The hatch is drawn by hand, not by ggpattern; see the
# `--- the fuel hatch` switch block for why.
# CAVEAT: a hatch needs ~6pt of segment to read as a hatch, and many fuel segments
# in the small panels are 2-3pt tall. Those segments are unidentifiable by ANY
# encoding at that size, so nothing is lost; the hatch earns its keep on the large
# ones (30-160pt), which is where the misreading actually happens.
# NOTE: none of this is a defect of inst/items_full_bcp.csv -- comm_group is a
# commodity-type classification the pipeline depends on and is left alone.
#
# READS  <MRIO>/losses/{X.rds, Y.rds, fd_labels.csv, <yr>_L_<alloc>.rds}   (13,14)
#        <base>/{E.rds, io_labels.csv}                                     (16,12_b)
#        inst/items_full_bcp.csv                                           (item, comm_group)
#        <IN_DIR>/FABIO_bcp_<ind>_value_added_responsibility_<vabase>_<alloc>_ex_tls.csv  (41)
#        <IN_DIR>/FABIO_bcp_<ind>_hdi_responsibility_<alloc>.csv           (40, cross-check)
# WRITES output/plot/responsibility_country_item_<ind>_<vabase>_<alloc>_<year>.svg
#        output/plot/responsibility_country_item_byfuel_<ind>_<vabase>_<alloc>_<year>.svg
#
# RUN: Rscript R/43_plot_responsibility_country_item.R   (after 40 and 41)
#      switches below must match the 40/41 run whose CSVs are being read.
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
# ggpattern hatches the fuel segments (see the fuel hatch block below). It is
# the only non-base dependency this script adds; fail with a useful message
# rather than an opaque "could not find function geom_col_pattern".
if (!requireNamespace("ggpattern", quietly = TRUE))
  stop("[43] package 'ggpattern' is required to hatch the fuel segments.\n",
       "     install.packages('ggpattern')   # pulls in 'gridpattern'")
library(ggpattern)

source("R/00_system_variables.R")   # output_dir_bcp, years
source("R/19_plot_definitions.R")   # indicator_meta, fuel_colors, feedstock_color_map, *_reserve
source("R/00_responsibility_helpers.R")   # country_grid(), build_D()

# --- run switches (must match the 40 run that wrote the CSVs) ----------------
YEAR           <- 2022L
allocation     <- "value"        # co-product rule of B: "mass" | "value"
STRESSOR       <- "ibif_total"   # "ibif_total" | "LCIM_EQ_terrestrial"
VA_BASE        <- "exiobase"     # "gloria" | "exiobase"

TOP_N          <- 18             # countries shown
# How the ONE shared country set is chosen. The fuels differ in size by more than
# an order of magnitude (2019: biogasoline 232,749 | biodiesel 124,658 |
# renewable diesel 14,008), so any ranking on ABSOLUTE pooled impact is really a
# ranking on the big two, and renewable diesel's own leaders never make the cut:
# under "max" at TOP_N = 15 the figure drops NLD, FIN, NOR and PNG, i.e. ranks 4,
# 5, 7 and 10 of renewable diesel -- the HVO refiners -- and the RD row then
# covers just 57% of the VA account.
#   "share"  score = max over (fuel x account) of the country's SHARE of that
#            fuel's world total. A country that is 17% of renewable diesel now
#            outranks one that is 2% of biogasoline. Keeps ONE country set, so the
#            fuel rows stay comparable; costs ~3pp of biogasoline coverage and
#            buys ~30pp of renewable diesel. THE DEFAULT.
#   "max"    the old behaviour: max of the pooled PBA/CBA/VA accounts.
#   "mean"   pooled, and favours countries that are consistently big.
#   "PBA" / "CBA" / "VA"   pooled, one account only.
RANK_BY        <- "share"        # "share" | "max" | "mean" | "PBA" | "CBA" | "VA"

# SELECTION and ORDERING are two different jobs and want two different scores.
# RANK_BY (above) decides WHICH countries appear -- it has to be fuel-normalised,
# or the small fuels lose their leaders. But that score ("max share of any ONE
# fuel") is a terrible thing to sort the x axis by: it is not the quantity the
# reader is looking at, so the bars come out in what looks like random order.
# ORDER_BY decides the LEFT-TO-RIGHT order of whichever countries got selected,
# on a pooled account the eye can actually follow down the page.
#   "CBA" / "PBA" / "VA"   pooled account, largest first. THE DEFAULT IS CBA.
#   "max" / "mean"         pooled across the three accounts
#   "rank"                 keep the selection order (the old behaviour)
# NOTE the side effect of ordering on one account: a country that is big in a
# DIFFERENT account sinks to the right. Under "CBA", PNG (PBA 1,160, CBA 2) lands
# last and UKR (PBA 6,790, CBA 737) second-last -- they are producers, not
# consumers. That is honest, but if you want the producer-only countries placed by
# their strongest account instead, use ORDER_BY = "max".
ORDER_BY       <- "CBA"          # "CBA" | "PBA" | "VA" | "max" | "mean" | "rank"
# Two figures, two crowding budgets -- kept SEPARATE on purpose. Figure 1 pools
# items on `account`; figure 2 pools on `account x fuel` and so keeps MORE items
# (that is the whole point of the split: an item small overall but large within
# one fuel earns a colour there). Figure 2 is therefore the crowded legend. A
# single shared constant would couple them -- raising it to declutter figure 2's
# legend would silently drop items from figure 1, whose keep-set must stay put.
# Raise MIN_ITEM_SHARE_BYFUEL to thin figure 2 (0.03 trims the tail without
# erasing the per-fuel detail the figure exists for); leave POOLED at 0.02 so
# figure 1 is byte-for-byte unchanged. Do not push BYFUEL past ~0.03-0.04.
MIN_ITEM_SHARE_POOLED <- 0.02    # figure 1: below this share of an account       -> "Other"
MIN_ITEM_SHARE_BYFUEL <- 0.03    # figure 2: below this share of an account x fuel -> "Other"
SHADE          <- 0.35           # how far a primary crop is darkened / a by-product lightened
TOL            <- 1e-6           # relative tolerance of the consistency checks

# --- the fuel hatch (ggpattern) ---------------------------------------------
# The fuels are drawn with a diagonal stripe; every feedstock is drawn plain.
# See THE FUELS ARE HATCHED in the header for why texture and not another hue.
#
# pattern_spacing is the parameter to touch. 0.03 puts ~10 stripes across a fuel
# segment and renders in seconds. Do NOT reach for a much finer value: at 0.0025
# ggpattern generates an enormous stripe field per patterned grob, clips each to
# its segment, and ggsave() grinds for minutes before svglite has to serialise it
# all. If the stripes look too coarse, step DOWN gently (0.03 -> 0.02 -> 0.015)
# and watch the render time.
PATTERN_SPACING <- 0.03          # ~10 stripes per fuel segment; see the warning above
PATTERN_DENSITY <- 0.40          # fraction of each stripe period that is ink
PATTERN_ANGLE   <- 45            # diagonal; no feedstock has any texture, so any angle works
PATTERN_INK     <- "white"       # reads on all three fuel fills (gold, dark purple, violet)

# The fuels, keyed EXACTLY as 40's `biofuel_groups` -- these names are the join
# key against 40's biofuel_group column in both the VA and the accounts CSV, so
# they must not be prettified here. Display names live in FUEL_LABELS.
BF_GROUPS <- list(biogasoline      = "c146",
                  biodiesel        = "c147",
                  renewable_diesel = "c149")   # c149 only; HVO co-products excluded (40)

# Panel/legend labels. Chosen to match `fuel_colors` (19) so the row a fuel sits
# in and the colour of its segment in the VA bar carry the same name.
FUEL_LABELS <- c(biogasoline      = "Biogasoline",
                 biodiesel        = "Biodiesel",
                 renewable_diesel = "Renewable diesel")

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
# COLLISION NOTE (checked in CIEDE2000, the metric 19's palette was built on):
#   * Grazing was oilcrop_reserve[2] (tan #B87340), which sits dE 11.7 from the
#     DARKENED Rape and Mustardseed (#9B6910) -- below the ~15 dE that keeps two
#     keys apart at 10pt -- and the two are frequently adjacent (rape is a lead
#     biodiesel feedstock). Moved to oilcrop_reserve[1] (maroon #9B3649): dE to
#     Rape and Mustardseed 34.8, nearest neighbour anywhere 18.4 (Cassava).
#   * Cattle had NO entry here or in ANCHOR_FEEDSTOCK, so build_palette() fell it
#     back to the grey "Other" catch-all -- an EXACT match (dE 0), i.e. Cattle read
#     as "pooled". Given its own livestock hide-brown, kept LOCAL to this script
#     (not added to 19's shared pool, which get_feedstock_palette indexes into for
#     other figures): dE 13.6 from the grey Other, 27.0 from Grazing (both are
#     livestock, so a related-but-distinct warm pair is intentional here).
EXTRA_ITEM_COLORS <- c(Grazing        = unname(oilcrop_reserve[1]),   # maroon: pasture, clear of the rape/mustard ambers
                       Cattle         = "#8C7B6B",                     # hide-brown: livestock, distinct from grey "Other"
                       `Fodder crops` = unname(starchy_reserve[1]))

# 19_01b's relabel: c145's FAOSTAT name is unreadable, and it IS the UCO node.
UCO_PATTERN <- "^Animal or vegetable fats and oils"

# processing stage, from comm_group: 1 = primary (where the impact lands),
# 2 = processed (where the value lands), 3 = residual. Sets the shade.
STAGE_1 <- c("Oil crops", "Cereals", "Sugar crops", "Roots and tubers",
             "Fibre crops", "Fodder crops", "Grazing", "Live animals")
STAGE_2 <- c("Vegetable oils", "Sugar, sweeteners", "Alcohol", "Meat", "Milk", "Biofuels")

# --- paths -------------------------------------------------------------------
model_version <- if (tolower(trimws(Sys.getenv("FABIO_RUN_MODE", "rescaled"))) == "bypass")
  "bypass" else "rescaled"
base_path <- sub("/+$", "", output_dir_bcp)                     # E, io_labels: version-invariant
MRIO_PATH <- if (model_version == "bypass") file.path(base_path, "bypass") else base_path
IN_DIR    <- if (model_version == "bypass") "output/bypass" else "output"
PLOT_DIR  <- file.path("output", "plot")
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

ATAG <- tolower(allocation)                       # "mass" / "value"
STAG <- tolower(sub("_total$", "", STRESSOR))     # "ibif" / "lcim_eq_terrestrial"
VA_FILE  <- file.path(IN_DIR, sprintf("FABIO_bcp_%s_value_added_responsibility_%s_%s_ex_tls.csv",
                                      STAG, VA_BASE, ATAG))
ACC_FILE <- file.path(IN_DIR, sprintf("FABIO_bcp_%s_hdi_responsibility_%s.csv", STAG, ATAG))

message(sprintf(">>> [43] %d | model='%s' | stressor='%s' | alloc='%s' | VA base='%s'",
                YEAR, model_version, STRESSOR, allocation, VA_BASE))

# --- indicator metadata ------------------------------------------------------
meta <- {
  i <- match(STAG, tolower(sub("_total$", "", indicator_meta$indicator)))
  if (is.na(i)) {
    warning("[43] no indicator_meta entry for '", STAG, "'; scale_factor = 1.")
    list(scale_factor = 1, y_label = STAG, short_label = STAG)
  } else as.list(indicator_meta[i, .(scale_factor, y_label, short_label)])
}

# --- inputs ------------------------------------------------------------------
need <- function(path, who) {
  if (!file.exists(path)) stop("Missing input: ", path, "  (produced by ", who, ")")
  path
}
YRC   <- as.character(YEAR)
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
  stop("[43] year ", YEAR, " absent from X / Y / E.")

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
# by-item figure. Check it loudly before computing anything. (Mirrors 40.)
io_key <- paste0(io$iso3c, "_", io$comm_code)
for (obj in list(list(rownames(X), "X.rds rows"), list(colnames(E[[YRC]]), "E cols"))) {
  if (is.null(obj[[1]])) { warning("[43] ", obj[[2]], ": no names -- trusting position."); next }
  if (!identical(obj[[1]], io_key))
    stop("[43] ", obj[[2]], " != io_labels grid -- extensions would be misattributed. Re-run 16.")
}

# --- the bilateral matrices D_g: impact at node i, driven by consumer c ------
# ONE D PER FUEL. D is linear in Y_bf, so the pooled matrix of the original
# figure is just sum_g D_g -- nothing is recomputed twice and the pooled figure
# is bit-for-bit what this script produced before the per-fuel split was added.
# Only the biofuel rows of Y carry demand, so B is subset to those columns
# (N x |bf_g|) rather than multiplied out in full.
Xi <- as.vector(X[, YRC])
f  <- as.numeric(E[[YRC]][STRESSOR, ]) / Xi; f[!is.finite(f)] <- 0

grid <- country_grid(io, fd)
Yc   <- Y[[YRC]] %*% grid$S_fd                                         # N x consumer

acc_g <- list()
for (g in names(BF_GROUPS)) {
  sel <- as.numeric(io$comm_code %in% BF_GROUPS[[g]])
  if (!sum(sel)) { warning("[43] no node for fuel '", g, "' -- skipped."); next }
  
  # producing node x consuming country, at the node resolution the item stacks need
  D <- build_D(f, B, Yc, sel, countries = grid$countries)
  if (sum(D) == 0) { warning("[43] '", g, "' carries no ", STRESSOR, " in ", YEAR, " -- skipped."); next }
  
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
if (!length(acc_g)) stop("[43] the biofuel chains carry no ", STRESSOR, " in ", YEAR, ".")
pc <- rbindlist(acc_g, use.names = TRUE)

# --- VA: read per fuel from 41 (its CSV already carries biofuel_group) -------
# The VA bar is NOT recomputed. 41 writes va_resp keyed by (biofuel_group,
# va_iso3c, va_comm_code), so the fuel dimension is simply *not summed away*
# here -- the pooled figure aggregates it back, the per-fuel figure keeps it.
va <- fread(need(VA_FILE, "41"))[year == YEAR & biofuel_group %in% names(BF_GROUPS),
                                 .(value = sum(va_resp)),
                                 by = .(iso3c = va_iso3c, comm_code = va_comm_code,
                                        fuel = biofuel_group)][value != 0][, account := "VA"]
if (!nrow(va)) stop("[43] no VA responsibility rows for ", YEAR, " in ", VA_FILE)

d <- rbindlist(list(pc, va), use.names = TRUE)
d[, account := factor(account, levels = ACCOUNTS)]
d[, fuel    := factor(unname(FUEL_LABELS[fuel]), levels = unname(FUEL_LABELS))]
d <- merge(d, items, by = "comm_code", all.x = TRUE)
if (anyNA(d$item)) stop("[43] comm_code(s) absent from items_full_bcp.csv: ",
                        paste(unique(d[is.na(item), comm_code]), collapse = ", "))

# --- consistency checks ------------------------------------------------------
# (1) the three accounts re-attribute ONE total -- per fuel AND pooled.
tot <- d[, .(total = sum(value)), by = .(fuel, account)]
bad <- tot[, .(dev = (max(total) - min(total)) / max(abs(total))), by = fuel][dev > TOL]
if (nrow(bad))
  warning("[43] the accounts do not share one total within: ",
          paste(sprintf("%s (rel. %.3g)", bad$fuel, bad$dev), collapse = ", "))

# (2) PBA/CBA rebuilt here must reproduce the accounts CSV -- checked at
#     (account x iso3c x fuel) resolution.
if (file.exists(ACC_FILE)) {
  acc <- fread(ACC_FILE)[year == YEAR & biofuel_group %in% names(BF_GROUPS)]
  ref <- rbindlist(list(
    acc[, .(ref = sum(production_based)),  by = .(iso3c, fuel = biofuel_group)][, account := "PBA"],
    acc[, .(ref = sum(consumption_based)), by = .(iso3c, fuel = biofuel_group)][, account := "CBA"]
  ), use.names = TRUE)
  ref[, fuel := unname(FUEL_LABELS[fuel])]
  cmp <- merge(d[account != "VA", .(new = sum(value)),
                 by = .(account = as.character(account), iso3c, fuel = as.character(fuel))],
               ref, by = c("account", "iso3c", "fuel"), all = TRUE)
  cmp[is.na(new), new := 0][is.na(ref), ref := 0]
  dev <- max(abs(cmp$new - cmp$ref)) / max(abs(cmp$ref), 1)
  if (dev > TOL) warning(sprintf("[43] PBA/CBA differ from the accounts CSV per fuel (max rel. %.3g) -- same switches?", dev))
  else message("[43] PBA/CBA match the accounts CSV, per fuel.")
} else message("[43] no accounts CSV to cross-check against: ", ACC_FILE)

# --- top-N countries: ONE ranking, shared by BOTH figures --------------------
# The same countries, in the same order, appear in every fuel row of figure 2 --
# that is what lets the rows be read against each other and against figure 1.
# (Ranking each row separately would optimise each panel but make the grid
# uncomparable, which is the whole point of the grid.) So the ranking has to be
# a single score per country -- see the RANK_BY switch for why "share" and not
# the pooled absolute total.
ctot <- dcast(d[, .(value = sum(value)), by = .(iso3c, account)],
              iso3c ~ account, value.var = "value", fill = 0)   # pooled, for the coverage report

rank_dt <- switch(
  RANK_BY,
  # max over (fuel x account) of the country's share of THAT fuel's world total.
  # Normalising by the fuel's own total is what stops the two big fuels from
  # deciding the whole country set on their own.
  share = {
    s <- d[, .(v = sum(value)), by = .(iso3c, account, fuel)]
    s[, tot := sum(v), by = .(account, fuel)]
    s[tot > 0, .(rank_by = max(v / tot)), by = iso3c]
  },
  max   = ctot[, .(iso3c, rank_by = pmax(PBA, CBA, VA))],
  mean  = ctot[, .(iso3c, rank_by = (PBA + CBA + VA) / 3)],
  ctot[, .(iso3c, rank_by = get(RANK_BY))]
)
ctot <- merge(ctot, rank_dt, by = "iso3c", all.x = TRUE)
ctot[is.na(rank_by), rank_by := 0]
setorder(ctot, -rank_by)
shown <- ctot[seq_len(min(TOP_N, .N))]

# ...then re-sort THOSE countries left-to-right on ORDER_BY. shown$iso3c becomes
# the factor levels below, so this line alone sets the x axis of both figures.
shown[, order_by := switch(ORDER_BY,
                           rank = rank_by,
                           max  = pmax(PBA, CBA, VA),
                           mean = (PBA + CBA + VA) / 3,
                           get(ORDER_BY))]
setorder(shown, -order_by)

# Coverage is reported PER FUEL, not just pooled: a pooled figure of "90% covered"
# can sit on top of a renewable-diesel row that covers barely half its own total,
# which is exactly the failure the "share" ranking exists to prevent. Read the
# smallest fuel's row -- that is the one the ranking is at risk of abandoning.
cov <- d[iso3c %in% shown$iso3c, .(shown = sum(value)), by = .(fuel, account)]
cov <- merge(cov, d[, .(all = sum(value)), by = .(fuel, account)], by = c("fuel", "account"))
cov[, pct := 100 * shown / all]
message(sprintf("[43] selected top %d by '%s', ordered left-to-right by '%s': %s",
                nrow(shown), RANK_BY, ORDER_BY, paste(shown$iso3c, collapse = " ")))
message("[43] coverage of each fuel's world total:")
for (f in levels(d$fuel)) {
  x <- cov[fuel == f]
  if (!nrow(x)) next
  message(sprintf("       %-17s %s", f,
                  paste(sprintf("%s %.0f%%", x$account, x$pct), collapse = " | ")))
}

# --- persist the ordered country vector so 44 can follow it ------------------
# 44 (the HDI trade-split figure) reads this file and shows the SAME countries in
# the SAME left-to-right order, instead of deriving its own set. This is what keeps
# the two responsibility figures aligned. It matters that the selection ABOVE keeps
# VA in the ranking: VA is the one account that genuinely diverges from PBA/CBA, so
# ranking on VA pulls in value-capturing hubs (in this run NLD -- ~17% of the
# renewable-diesel value-added total, but negligible in production/consumption) that
# a PBA/CBA-only ranking would drop. HDI, by contrast, is bounded between PBA and
# CBA and never selects a country those two miss, so letting the HDI figure inherit
# this set loses nothing on its side while preserving the VA finding on this one.
# Keyed by (indicator, VA base, allocation, year) so a run never reads another
# run's order; 44 selects which VA base to follow via its own ORDER_VA_BASE switch.
order_file <- file.path(IN_DIR, sprintf("FABIO_bcp_%s_responsibility_country_order_%s_%s_%d.csv",
                                        STAG, VA_BASE, ATAG, YEAR))
fwrite(shown[, .(position = .I, iso3c, rank_by, order_by)], order_file)
message(sprintf(">>> [43] wrote country order (%d countries, RANK_BY='%s' ORDER_BY='%s') -> %s",
                nrow(shown), RANK_BY, ORDER_BY, order_file))

# --- pool the small items into "Other" ---------------------------------------
# An item is kept if it reaches that figure's threshold (MIN_ITEM_SHARE_POOLED for
# figure 1, MIN_ITEM_SHARE_BYFUEL for figure 2) of any CELL of the figure it is
# drawn in -- a cell being an account (figure 1) or an account x fuel (figure 2).
# Two consequences, both deliberate:
#   * figure 1's keep-set is computed exactly as before, so that figure does not
#     change now that the fuel split exists;
#   * figure 2 keeps a few extra items -- one that is 4% of renewable diesel but
#     0.4% of the pooled total is invisible in figure 1 and visible here, which
#     is the entire reason for splitting the fuels. An item may therefore sit
#     inside figure 1's grey "Other" and have its own colour in figure 2.
# The palette is built ONCE over the union of both keep-sets, so an item that
# appears in both figures has the SAME colour in both.
pool_items <- function(dat, cell, min_share) {
  share <- dat[, .(v = sum(abs(value))), by = c(cell, "item")][, share := v / sum(v), by = cell]
  keep  <- share[share >= min_share, unique(item)]
  out   <- copy(dat)
  out[!item %in% keep, `:=`(item = "Other", chain = "Other", stage = 3L, comm_code = "Other")]
  message(sprintf("[43] %-28s %2d items kept (>= %.0f%% of a cell), %d pooled as 'Other'.",
                  paste0("[", paste(cell, collapse = " x "), "]"),
                  length(keep), 100 * min_share, uniqueN(share$item) - length(keep)))
  out
}

# NB shares are WORLD shares, computed on all countries and only then subset to
# the top N -- as before. Pooling on the top-N subset instead would let a country
# outside the top N change which items get a colour.
d1 <- pool_items(d, "account",           MIN_ITEM_SHARE_POOLED)   # figure 1: fuels pooled
d2 <- pool_items(d, c("account", "fuel"), MIN_ITEM_SHARE_BYFUEL)  # figure 2: one panel per fuel

p1 <- d1[iso3c %in% shown$iso3c,
         .(value = sum(value)), by = .(iso3c, account, item, comm_code, chain, stage)]
p2 <- d2[iso3c %in% shown$iso3c,
         .(value = sum(value)), by = .(iso3c, account, fuel, item, comm_code, chain, stage)]
p1[, iso3c := factor(iso3c, levels = shown$iso3c)]
p2[, iso3c := factor(iso3c, levels = shown$iso3c)]

# --- stack + legend order: fuels at the base, then chain, then stage ---------
# Same rule for both figures, applied to each figure's own item set.
make_levels <- function(p) {
  lvl <- unique(p[, .(item, comm_code, chain, stage)])
  lvl[p[, .(size = sum(abs(value))), by = item], size := i.size, on = "item"]
  lvl[lvl[, .(cs = sum(size)), by = chain], cs := i.cs, on = "chain"]  # big chains first
  lvl[, chain_rank := fifelse(chain == "Biofuels", Inf,                # fuels always at the base
                              fifelse(chain == "Other", -Inf, cs))]    # grey catch-all always last
  setorder(lvl, -chain_rank, stage, -size)                             # within a chain: crop -> processed
  lvl[]
}
lvl1 <- make_levels(p1); p1[, item := factor(item, levels = as.character(lvl1$item))]
lvl2 <- make_levels(p2); p2[, item := factor(item, levels = as.character(lvl2$item))]

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
    if (it %in% names(feedstock_color_map))  return(unname(feedstock_color_map[it]))   # exact, as in 19_01b
    if (it %in% names(EXTRA_ITEM_COLORS))    return(unname(EXTRA_ITEM_COLORS[it]))
    if (cc %in% names(ANCHOR_FEEDSTOCK)) {   # a stage of a mapped feedstock: same hue, shaded
      base <- unname(feedstock_color_map[ANCHOR_FEEDSTOCK[[cc]]])
      return(shade(base, if (lvl$stage[k] == 1L) -SHADE else SHADE))
    }
    warning("[43] no colour for '", it, "' (", cc, ") -- grey. Map it in ANCHOR_FEEDSTOCK ",
            "or EXTRA_ITEM_COLORS.")
    unname(feedstock_color_map[["Other"]])
  }, character(1))
  setNames(cols, as.character(lvl$item))
}
# ONE palette over the union of both keep-sets: same item -> same colour in both figures.
pal <- build_palette(unique(rbindlist(list(lvl1, lvl2), use.names = TRUE,
                                      fill = TRUE)[, .(item, comm_code, chain, stage)],
                            by = "item"))

# --- the shared plot skeleton ------------------------------------------------
# Both figures are the same bar grammar (x = account, stack = item, one panel per
# country); they differ only in whether the fuel dimension is a facet row.
#
# WHY WE DRAW THE LEGEND KEY OURSELVES (key_glyph) ----------------------------
# The bars are hatched by ggpattern, but ggpattern does NOT render that hatch
# into the legend keys under geom_col_pattern + svglite: the earlier fix routed
# the pattern to the fill guide via guide_legend(override.aes = list(pattern=..)),
# and the geom kept pattern_key_scale_factor = 0.4 to stop the key going solid --
# yet the emitted SVG keys came out SOLID anyway (no stripe geometry at all), so
# the three fuels were indistinguishable in the legend from the feedstocks whose
# colour they sit dE 4-7 from (Biodiesel vs Soyabeans, Biogasoline vs Rape and
# Mustard Oil). The reliable fix is to stop depending on ggpattern's key drawing
# and paint the swatch with plain grid grobs, which svglite serialises cleanly:
# every key is a filled rect + white border, and the three FUEL keys (detected by
# their exact fill -- the collisions are close but never identical hexes) get the
# same 45-deg white hatch as the bars overlaid on top. This is orthogonal to hue,
# exactly as in the bars, and needs no override.aes.
`%||%` <- function(a, b) if (is.null(a)) b else a
FUEL_HEX <- toupper(unname(fuel_colors))          # the three fuel fills

draw_key_fuelhatch <- function(data, params, size) {
  fill   <- data$fill %||% "grey50"
  swatch <- grid::rectGrob(gp = grid::gpar(fill = fill, col = "white",
                                           lwd = 0.25 * .pt))         # matches linewidth = 0.25
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

base_plot <- function(p) {
  ggplot(p, aes(x = account, y = value / meta$scale_factor,
                fill = item, pattern = item %in% names(fuel_colors))) +
    geom_col_pattern(
      position                 = position_stack(reverse = TRUE),  # first level at the BASE
      width                    = 0.85,
      colour                   = "white",            # ONE outline rule now, for every segment
      linewidth                = 0.25,
      pattern_colour           = NA,                 # no outline on the stripes themselves
      pattern_fill             = PATTERN_INK,
      pattern_angle            = PATTERN_ANGLE,
      pattern_density          = PATTERN_DENSITY,
      pattern_spacing          = PATTERN_SPACING,
      pattern_key_scale_factor = 0.4,                # (unused now the key is drawn by key_glyph)
      key_glyph                = draw_key_fuelhatch) +  # hatched fuel keys; see block above
    scale_pattern_manual(values = c(`FALSE` = "none", `TRUE` = "stripe"), guide = "none") +
    scale_fill_manual(values = pal, drop = FALSE) +
    scale_y_continuous(labels = scales::label_number(big.mark = ","),
                       expand = expansion(mult = c(0, 0.04))) +
    guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
    labs(x = NULL, y = meta$y_label, fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position    = "bottom",
          legend.key.size    = unit(10, "pt"),
          strip.placement    = "outside",
          strip.background   = element_blank(),
          strip.text         = element_text(face = "bold", size = 9),
          panel.spacing.x    = unit(3, "pt"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank(),
          axis.text.x        = element_text(size = 7, angle = 90, hjust = 1, vjust = 0.5),
          plot.subtitle      = element_text(size = 9, colour = "grey30"),
          plot.caption       = element_text(size = 8, colour = "grey40"))
}

CAPTION  <- sprintf("%s allocation, %s value-added base; biogasoline + biodiesel + renewable diesel.",
                    allocation, VA_BASE)
LEGEND_NOTE <- paste0("One hue per feedstock chain (as in the footprint figures), shaded dark for the ",
                      "primary commodity and light for the processed one:\nPBA/CBA items are where the ",
                      "impact occurs, VA items are where the value is captured. Fuel segments are hatched.")

# The subtitle must say HOW the countries were picked -- "top 18 countries" alone
# is not reproducible, and under 'share' it is not even a ranking on the axis the
# reader is looking at.
RANK_NOTE <- sprintf(
  "%s; ordered by %s",
  switch(RANK_BY,
         share = "largest share of any single fuel's world total, so the small fuels keep their own leaders",
         max   = "largest pooled PBA/CBA/VA account",
         mean  = "largest mean of the three pooled accounts",
         sprintf("largest pooled %s account", RANK_BY)),
  switch(ORDER_BY,
         rank = "the same score",
         max  = "their largest pooled account",
         mean = "their mean pooled account",
         sprintf("pooled %s", ORDER_BY)))

# --- figure 1: fuels pooled --------------------------------------------------
gg1 <- base_plot(p1) +
  facet_wrap(~ iso3c, nrow = 1, strip.position = "bottom") +
  labs(title = sprintf("%s responsibility for bio-based transport fuels, %d",
                       meta$short_label, YEAR),
       subtitle = sprintf(paste0("PBA production-based | CBA consumption-based | VA value-added-based ",
                                 "(ex TLS): three attributions of one and the same total.\n",
                                 "Countries (%d) selected by %s; items below %.0f%% of an account ",
                                 "pooled as 'Other'. %s"),
                          nrow(shown), RANK_NOTE, 100 * MIN_ITEM_SHARE_POOLED, LEGEND_NOTE),
       caption = CAPTION)

out1 <- file.path(PLOT_DIR, sprintf("responsibility_country_item_%s_%s_%s_%d.svg",
                                    STAG, VA_BASE, ATAG, YEAR))
# Width scales with the number of country panels: 15 countries fitted 14in, and at
# TOP_N = 18 the same panels would be squeezed ~20% narrower and the account labels
# would collide. NB pattern_spacing is relative to the plot, so a big change here
# shifts the stripe pitch a little -- check the hatch if you move TOP_N far.
W1 <- 4 + 0.70 * nrow(shown)          # 15 -> 14.5in, 18 -> 16.6in
ggsave(out1, gg1, device = svglite::svglite, width = W1, height = 7.5)
message(">>> [43] wrote ", out1)

# --- figure 2: one panel row per biofuel -------------------------------------
# facet_grid(fuel ~ country): the country columns are the SAME set, in the same
# order, in every row, so a row can be read against the one above it.
# scales = "free_y" frees the y axis PER ROW (not per panel): within a fuel the
# countries stay directly comparable, while renewable diesel -- an order of
# magnitude smaller than biodiesel -- still fills its row and shows its item
# composition instead of collapsing to a flat line. The consequence is that bar
# HEIGHTS MAY NOT BE COMPARED ACROSS ROWS; the axis labels carry the scale, and
# the subtitle says so. Use figure 1 (or scales = "fixed") for cross-fuel
# magnitude.
gg2 <- base_plot(p2) +
  # switch = "both": fuel labels on the LEFT (rotated), country codes on the BOTTOM
  # -- same country placement as figure 1 and as 44.
  facet_grid(fuel ~ iso3c, scales = "free_y", switch = "both") +
  theme(strip.text.y.left = element_text(face = "bold", size = 10, angle = 90),
        panel.spacing.y   = unit(8, "pt")) +
  labs(title = sprintf("%s responsibility for bio-based transport fuels by fuel, %d",
                       meta$short_label, YEAR),
       subtitle = sprintf(paste0("PBA production-based | CBA consumption-based | VA value-added-based ",
                                 "(ex TLS): three attributions of one and the same total, per fuel.\n",
                                 "Same %d countries as the pooled figure (selected by %s) in ",
                                 "every row; items below %.0f%% of an account x fuel pooled as 'Other'.\n",
                                 "Y AXIS IS FREE PER ROW -- compare bars within a fuel, not across ",
                                 "fuels. %s"),
                          nrow(shown), RANK_NOTE, 100 * MIN_ITEM_SHARE_BYFUEL, LEGEND_NOTE),
       caption = CAPTION)

out2 <- file.path(PLOT_DIR, sprintf("responsibility_country_item_byfuel_%s_%s_%s_%d.svg",
                                    STAG, VA_BASE, ATAG, YEAR))
W2 <- 5 + 0.78 * nrow(shown)          # 15 -> 16.7in, 18 -> 19.0in
ggsave(out2, gg2, device = svglite::svglite, width = W2, height = 12)
message(">>> [43] wrote ", out2)