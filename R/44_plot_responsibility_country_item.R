# =============================================================================
# 44_plot_responsibility_country_item.R
# TWO figures: the production- (PBA), consumption- (CBA) and value-added-based
# (VA) accounts as three ADJACENT stacked bars per country, at COUNTRY x ITEM
# resolution, for PLOT_YEAR.
#
#   [1] responsibility_country_item_*.svg          -- the three fuels POOLED
#       x = country (top N, largest first), three bars each | y = impact | fill = item
#
#   [2] responsibility_country_item_byfuel_*.svg   -- ONE PANEL ROW PER BIOFUEL
#       facet_grid(fuel ~ country): the same top-N countries, in the same order,
#       in every row, so the rows can be read against one another. The y axis is
#       FREE PER ROW: renewable diesel is an order of magnitude below biodiesel
#       and would otherwise collapse to a flat line, so each row shows its own
#       COMPOSITION at its own scale. Bar heights are NOT comparable across rows
#       -- figure [1] is the one to read for cross-fuel magnitude.
#
# Both come out of one computation: D is built per fuel and figure [1] sums over
# the fuel dimension.
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
# The VA bar is NOT recomputed: it is read from 42's ex_tls responsibility CSV,
# already resolved to (va_iso3c, va_comm_code). All three are then checked
# against the country-level accounts CSV and against each other.
#
# ITEM SEMANTICS -- read before interpreting the stacks -----------------------
#   PBA / CBA   the node where the impact PHYSICALLY OCCURS (the source item).
#               PBA books it to the producing country, CBA to the consuming one.
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
# WRITES output/plot/responsibility_country_item[_byfuel]_<ind>_<vabase>_<alloc>_<year>.svg
#        ORDER_CSV -- the country order, so 45 can follow it
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

# --- figure switches ---------------------------------------------------------
TOP_N <- 18             # countries shown
# How the ONE shared country set is chosen. The fuels differ in size by more than
# an order of magnitude (2019: biogasoline 232,749 | biodiesel 124,658 |
# renewable diesel 14,008), so any ranking on ABSOLUTE pooled impact is really a
# ranking on the big two, and renewable diesel's own leaders never make the cut:
# under "max" at TOP_N = 15 the figure drops NLD, FIN, NOR and PNG -- ranks 4, 5,
# 7 and 10 of renewable diesel, the HVO refiners -- and the RD row then covers
# just 57% of the VA account.
#   "share"  score = max over (fuel x account) of the country's SHARE of that
#            fuel's world total. A country that is 17% of renewable diesel
#            outranks one that is 2% of biogasoline. Keeps ONE country set, so
#            the fuel rows stay comparable; costs ~3pp of biogasoline coverage
#            and buys ~30pp of renewable diesel.
#   "max" / "mean"         pooled across the three accounts
#   "PBA" / "CBA" / "VA"   pooled, one account only
RANK_BY <- "share"

# SELECTION and ORDERING are two different jobs and want two different scores.
# RANK_BY has to be fuel-normalised or the small fuels lose their leaders, but
# that score ("max share of any ONE fuel") is a bad thing to sort the x axis by:
# it is not the quantity the reader is looking at, so the bars come out in what
# looks like random order. ORDER_BY sets the left-to-right order of whichever
# countries were selected, on a pooled account the eye can follow.
#   "CBA" / "PBA" / "VA"   pooled account, largest first
#   "max" / "mean"         pooled across the three accounts
#   "rank"                 keep the selection order
# NOTE the side effect: a country big in a DIFFERENT account sinks to the right.
# Under "CBA", PNG (PBA 1,160, CBA 2) lands last and UKR (PBA 6,790, CBA 737)
# second-last -- they are producers, not consumers. Honest, but use "max" to
# place producer-only countries by their strongest account instead.
ORDER_BY <- "CBA"

# Two figures, two crowding budgets, kept SEPARATE on purpose. Figure 1 pools
# items on `account`; figure 2 pools on `account x fuel` and so keeps MORE items
# -- that is the point of the split: an item small overall but large within one
# fuel earns a colour there. A single shared constant would couple them, so
# decluttering figure 2's legend would silently drop items from figure 1. Raise
# BYFUEL to thin figure 2; do not push it past ~0.03-0.04.
MIN_ITEM_SHARE_POOLED <- 0.02    # figure 1: below this share of an account        -> "Other"
MIN_ITEM_SHARE_BYFUEL <- 0.03    # figure 2: below this share of an account x fuel -> "Other"
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
d[, fuel    := factor(unname(biofuel_label[fuel]), levels = unname(biofuel_label))]
d <- merge(d, items, by = "comm_code", all.x = TRUE)
if (anyNA(d$item)) stop("[44] comm_code(s) absent from items_full_bcp.csv: ",
                        paste(unique(d[is.na(item), comm_code]), collapse = ", "))

# --- consistency checks ------------------------------------------------------
# (1) the three accounts re-attribute ONE total -- per fuel AND pooled.
tot <- d[, .(total = sum(value)), by = .(fuel, account)]
bad <- tot[, .(dev = (max(total) - min(total)) / max(abs(total))), by = fuel][dev > TOL]
if (nrow(bad))
  warning("[44] the accounts do not share one total within: ",
          paste(sprintf("%s (rel. %.3g)", bad$fuel, bad$dev), collapse = ", "))

# (2) PBA/CBA rebuilt here must reproduce the accounts CSV, at
#     (account x iso3c x fuel) resolution.
if (file.exists(HDI_CSV)) {
  acc <- fread(HDI_CSV)[year == PLOT_YEAR & biofuel_group %in% names(biofuel_groups)]
  ref <- rbindlist(list(
    acc[, .(ref = sum(production_based)),  by = .(iso3c, fuel = biofuel_group)][, account := "PBA"],
    acc[, .(ref = sum(consumption_based)), by = .(iso3c, fuel = biofuel_group)][, account := "CBA"]
  ), use.names = TRUE)
  ref[, fuel := unname(biofuel_label[fuel])]
  cmp <- merge(d[account != "VA", .(new = sum(value)),
                 by = .(account = as.character(account), iso3c, fuel = as.character(fuel))],
               ref, by = c("account", "iso3c", "fuel"), all = TRUE)
  cmp[is.na(new), new := 0][is.na(ref), ref := 0]
  dev <- max(abs(cmp$new - cmp$ref)) / max(abs(cmp$ref), 1)
  if (dev > TOL) warning(sprintf("[44] PBA/CBA differ from the accounts CSV per fuel (max rel. %.3g) -- same switches?", dev))
  else message("[44] PBA/CBA match the accounts CSV, per fuel.")
} else message("[44] no accounts CSV to cross-check against: ", HDI_CSV)

# --- top-N countries: ONE ranking, shared by BOTH figures --------------------
# The same countries, in the same order, appear in every fuel row of figure 2 --
# that is what lets the rows be read against each other and against figure 1.
# Ranking each row separately would optimise each panel but make the grid
# uncomparable, which is the whole point of the grid.
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

# Coverage is reported PER FUEL, not just pooled: a pooled "90% covered" can sit
# on top of a renewable-diesel row covering barely half its own total, which is
# the failure the "share" ranking exists to prevent. Read the smallest fuel's row.
cov <- d[iso3c %in% shown$iso3c, .(shown = sum(value)), by = .(fuel, account)]
cov <- merge(cov, d[, .(all = sum(value)), by = .(fuel, account)], by = c("fuel", "account"))
cov[, pct := 100 * shown / all]
message(sprintf("[44] selected top %d by '%s', ordered left-to-right by '%s': %s",
                nrow(shown), RANK_BY, ORDER_BY, paste(shown$iso3c, collapse = " ")))
message("[44] coverage of each fuel's world total:")
for (f in levels(d$fuel)) {
  x <- cov[fuel == f]
  if (!nrow(x)) next
  message(sprintf("       %-17s %s", f,
                  paste(sprintf("%s %.0f%%", x$account, x$pct), collapse = " | ")))
}

# --- persist the ordered country vector so 45 can follow it ------------------
# 45 (the HDI trade-split figure) reads this file and shows the SAME countries in
# the SAME left-to-right order instead of deriving its own set. It matters that
# the selection above keeps VA in the ranking: VA is the one account that
# genuinely diverges from PBA/CBA, so ranking on it pulls in value-capturing hubs
# (in this run NLD -- ~17% of the renewable-diesel value-added total, negligible
# in production/consumption) that a PBA/CBA-only ranking would drop. HDI is
# bounded between PBA and CBA and never selects a country those two miss, so
# letting the HDI figure inherit this set loses nothing on its side.
fwrite(shown[, .(position = .I, iso3c, rank_by, order_by)], ORDER_CSV)
message(sprintf(">>> [44] wrote country order (%d countries, RANK_BY='%s' ORDER_BY='%s') -> %s",
                nrow(shown), RANK_BY, ORDER_BY, ORDER_CSV))

# --- pool the small items into "Other" ---------------------------------------
# An item is kept if it reaches that figure's threshold of any CELL of the figure
# it is drawn in -- a cell being an account (figure 1) or an account x fuel
# (figure 2). Figure 2 therefore keeps a few extra items: one that is 4% of
# renewable diesel but 0.4% of the pooled total is invisible in figure 1 and
# visible here, which is the entire reason for splitting the fuels. An item may
# sit inside figure 1's grey "Other" and have its own colour in figure 2. The
# palette is built ONCE over the union of both keep-sets, so an item appearing in
# both figures has the SAME colour in both.
pool_items <- function(dat, cell, min_share) {
  share <- dat[, .(v = sum(abs(value))), by = c(cell, "item")][, share := v / sum(v), by = cell]
  keep  <- share[share >= min_share, unique(item)]
  out   <- copy(dat)
  out[!item %in% keep, `:=`(item = "Other", chain = "Other", stage = 3L, comm_code = "Other")]
  message(sprintf("[44] %-28s %2d items kept (>= %.0f%% of a cell), %d pooled as 'Other'.",
                  paste0("[", paste(cell, collapse = " x "), "]"),
                  length(keep), 100 * min_share, uniqueN(share$item) - length(keep)))
  out
}

# NB shares are WORLD shares, computed on all countries and only then subset to
# the top N. Pooling on the top-N subset would let a country outside the top N
# change which items get a colour.
d1 <- pool_items(d, "account",            MIN_ITEM_SHARE_POOLED)   # figure 1: fuels pooled
d2 <- pool_items(d, c("account", "fuel"), MIN_ITEM_SHARE_BYFUEL)   # figure 2: one panel per fuel

p1 <- d1[iso3c %in% shown$iso3c,
         .(value = sum(value)), by = .(iso3c, account, item, comm_code, chain, stage)]
p2 <- d2[iso3c %in% shown$iso3c,
         .(value = sum(value)), by = .(iso3c, account, fuel, item, comm_code, chain, stage)]
p1[, iso3c := factor(iso3c, levels = shown$iso3c)]
p2[, iso3c := factor(iso3c, levels = shown$iso3c)]

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
# ONE palette over the union of both keep-sets: same item -> same colour in both figures.
pal <- build_palette(unique(rbindlist(list(lvl1, lvl2), use.names = TRUE,
                                      fill = TRUE)[, .(item, comm_code, chain, stage)],
                            by = "item"))

# --- the shared plot skeleton ------------------------------------------------
# Both figures are the same bar grammar (x = account, stack = item, one panel per
# country); they differ only in whether the fuel dimension is a facet row.
#
# WHY WE DRAW THE LEGEND KEY OURSELVES (key_glyph) ----------------------------
# ggpattern does not render its hatch into the legend keys under
# geom_col_pattern + svglite: routing the pattern to the fill guide via
# guide_legend(override.aes = list(pattern = ..)) still emits SOLID keys, no
# stripe geometry at all, which leaves the three fuels indistinguishable from the
# feedstocks they sit dE 4-7 from. So the swatch is painted with plain grid grobs,
# which svglite serialises cleanly: every key is a filled rect + white border, and
# the three FUEL keys (detected by their exact fill -- the collisions are close
# but never identical hexes) get the same 45-deg white hatch as the bars.
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
  ggplot(p, aes(x = account, y = value / META$scale_factor,
                fill = item, pattern = item %in% names(fuel_colors))) +
    geom_col_pattern(
      position                 = position_stack(reverse = TRUE),  # first level at the BASE
      width                    = 0.85,
      colour                   = "white",
      linewidth                = 0.25,
      pattern_colour           = NA,                 # no outline on the stripes themselves
      pattern_fill             = PATTERN_INK,
      pattern_angle            = PATTERN_ANGLE,
      pattern_density          = PATTERN_DENSITY,
      pattern_spacing          = PATTERN_SPACING,
      key_glyph                = draw_key_fuelhatch) +
    scale_pattern_manual(values = c(`FALSE` = "none", `TRUE` = "stripe"), guide = "none") +
    scale_fill_manual(values = pal, drop = FALSE) +
    scale_y_continuous(labels = scales::label_number(big.mark = ","),
                       expand = expansion(mult = c(0, 0.04))) +
    guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
    labs(x = NULL, y = META$y_label, fill = NULL) +
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

CAPTION <- sprintf("%s allocation, %s value-added base; biogasoline + biodiesel + renewable diesel.",
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
                       META$short_label, PLOT_YEAR),
       subtitle = sprintf(paste0("PBA production-based | CBA consumption-based | VA value-added-based ",
                                 "(ex TLS): three attributions of one and the same total.\n",
                                 "Countries (%d) selected by %s; items below %.0f%% of an account ",
                                 "pooled as 'Other'. %s"),
                          nrow(shown), RANK_NOTE, 100 * MIN_ITEM_SHARE_POOLED, LEGEND_NOTE),
       caption = CAPTION)

# Width scales with the number of country panels: 15 countries fit 14in, and at
# TOP_N = 18 the same panels would be ~20% narrower and the account labels would
# collide. NB pattern_spacing is relative to the plot, so a big change here shifts
# the stripe pitch a little -- check the hatch if you move TOP_N far.
save_svg(sprintf("responsibility_country_item_%s_%s_%s_%d", STAG, VA_BASE, ATAG, PLOT_YEAR),
         gg1, width = 4 + 0.70 * nrow(shown), height = 7.5)   # 15 -> 14.5in, 18 -> 16.6in

# --- figure 2: one panel row per biofuel -------------------------------------
# facet_grid(fuel ~ country) with switch = "both": fuel labels on the LEFT
# (rotated), country codes on the BOTTOM -- same country placement as figure 1
# and as 45. scales = "free_y" frees the axis PER ROW, not per panel, so within a
# fuel the countries stay directly comparable while renewable diesel still fills
# its row instead of collapsing to a flat line. Bar HEIGHTS MAY NOT BE COMPARED
# ACROSS ROWS; the axis labels carry the scale and the subtitle says so.
gg2 <- base_plot(p2) +
  facet_grid(fuel ~ iso3c, scales = "free_y", switch = "both") +
  theme(strip.text.y.left = element_text(face = "bold", size = 10, angle = 90),
        panel.spacing.y   = unit(8, "pt")) +
  labs(title = sprintf("%s responsibility for bio-based transport fuels by fuel, %d",
                       META$short_label, PLOT_YEAR),
       subtitle = sprintf(paste0("PBA production-based | CBA consumption-based | VA value-added-based ",
                                 "(ex TLS): three attributions of one and the same total, per fuel.\n",
                                 "Same %d countries as the pooled figure (selected by %s) in ",
                                 "every row; items below %.0f%% of an account x fuel pooled as 'Other'.\n",
                                 "Y AXIS IS FREE PER ROW -- compare bars within a fuel, not across ",
                                 "fuels. %s"),
                          nrow(shown), RANK_NOTE, 100 * MIN_ITEM_SHARE_BYFUEL, LEGEND_NOTE),
       caption = CAPTION)

save_svg(sprintf("responsibility_country_item_byfuel_%s_%s_%s_%d", STAG, VA_BASE, ATAG, PLOT_YEAR),
         gg2, width = 5 + 0.78 * nrow(shown), height = 12)    # 15 -> 16.7in, 18 -> 19.0in