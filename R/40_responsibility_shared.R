# =============================================================================
# 40_responsibility_shared.R
# The run definition of the responsibility block, and the helpers, file names and
# palettes that 41-47 share.
#
# ONE combination -- indicator x allocation x VA base x year -- is computed and
# plotted at a time, and it is set HERE, once. 41 and 42 write the CSVs these
# switches name; 43-47 read the same names back through the same builders, so a
# reader and a writer cannot drift apart.
#
# SOURCED BY 41, 42 and 43-47, after their setwd() to the repo root. Each caller
# sets SCRIPT to its own number immediately afterwards, so the shared helpers
# report under the script the reader is actually looking at.
# =============================================================================

if (!requireNamespace("data.table", quietly = TRUE))
  stop("40_responsibility_shared.R requires the 'data.table' package.")
library(data.table)

source("R/00_system_variables.R")   # years, output_dir_bcp
source("R/19_plot_definitions.R")   # indicator_meta, continent_palette, fuel_colors

# --- the run -----------------------------------------------------------------
STRESSOR   <- "ibif_total"   # "ibif_total" | "LCIM_EQ_terrestrial"
allocation <- "value"        # co-product rule of the Leontief inverse: "mass" | "value"
VA_BASE    <- "exiobase"     # "gloria" | "exiobase"
VA_VARIANT <- "full"         # VA definition carried by the account 43-45/47 draw:
#   "full"   wages + capital + taxes-less-subsidies
#   "ex_tls" wages + capital
# 42 writes BOTH; this only picks which one is read
# back. 46 contrasts the two and ignores this switch.
PLOT_YEAR  <- 2021L          # the single year every by-country figure shows
resp_years <- as.character(years)   # 2012:2022, from 00_system_variables

SCRIPT <- "40"               # overwritten by each caller right after sourcing

# --- run mode and paths ------------------------------------------------------
# ONE derivation of the mode, and it defers to 00_run_config.R when that file has
# been sourced (41 and 42 do; 43-47 do not). run_config honours a pre-existing
# RUN_MODE *variable* as well as the env var, so re-reading only the env var here
# would let `RUN_MODE <- "bypass"` in an interactive session print a bypass banner
# while this file quietly pointed MRIO_PATH at the rescaled artefacts.
model_version <- if (exists("BYPASS_RESCALE") && isTRUE(BYPASS_RESCALE)) {
  "bypass"
} else if (tolower(trimws(Sys.getenv("FABIO_RUN_MODE", "rescaled"))) == "bypass") {
  "bypass"
} else {
  "rescaled"
}

base_path <- sub("/+$", "", output_dir_bcp)   # version-invariant (E, V, io_labels)
MRIO_PATH <- if (model_version == "bypass") file.path(base_path, "bypass") else base_path
OUT_DIR   <- if (model_version == "bypass") "output/bypass" else "output"
# PLOT_DIR hangs off OUT_DIR, not off a hard-coded "output": the figure names
# carry the indicator, VA base, allocation and year but NOT the model version, so
# a bypass run writing into output/plot/ would silently overwrite the rescaled
# run's SVGs -- the one place the "no two settings ever overwrite" rule leaked.
PLOT_DIR  <- file.path(OUT_DIR, "plot")
dir.create(OUT_DIR,  showWarnings = FALSE, recursive = TRUE)
dir.create(PLOT_DIR, showWarnings = FALSE, recursive = TRUE)

# --- file names --------------------------------------------------------------
# slug(): filename-safe form of a switch, so the switches and the file names can
# never drift apart.
slug <- function(x, fallback) {
  s <- gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", tolower(x)))
  if (nzchar(s)) s else fallback
}
ATAG <- slug(allocation, "alloc")                     # "mass" / "value"
STAG <- slug(sub("_total$", "", STRESSOR), "metric")  # "ibif" / "lcim_eq_terrestrial"

# the accounts take no VA base: they are keyed by indicator x allocation only
acc_csv <- function(what)
  file.path(OUT_DIR, sprintf("FABIO_bcp_%s_%s_%s.csv", STAG, what, ATAG))

# everything derived from the value-added extension carries the VA base as well
va_csv <- function(what, variant_tag = "")
  file.path(OUT_DIR, sprintf("FABIO_bcp_%s_%s_%s_%s%s.csv",
                             STAG, what, VA_BASE, ATAG, variant_tag))

# VA_TAG mirrors 42's `tagv` exactly -- 'full' keeps the base filename, every
# other variant is suffixed -- so the switch above and 42's output names cannot
# drift apart. VA_FULL_CSV / VA_EXTLS_CSV name the two variants OUTRIGHT, for 46,
# whose whole subject is the contrast: it must not follow VA_VARIANT, or it would
# plot one variant against itself.
if (!VA_VARIANT %in% c("full", "ex_tls"))
  stop("[40] VA_VARIANT must be 'full' or 'ex_tls', not '", VA_VARIANT, "'.")
VA_TAG <- if (VA_VARIANT == "full") "" else paste0("_", VA_VARIANT)

HDI_CSV      <- acc_csv("hdi_responsibility")                                 # 41
VA_RESP_CSV  <- va_csv("value_added_responsibility", VA_TAG)                  # 42
VA_FULL_CSV  <- va_csv("value_added_responsibility")                          # 42
VA_EXTLS_CSV <- va_csv("value_added_responsibility", "_ex_tls")               # 42
VA_SPLIT_CSV <- va_csv("value_added_trade_split", VA_TAG)                     # 42 (Pinero eq. 8)
ORDER_CSV    <- va_csv("responsibility_country_order", sprintf("_%d", PLOT_YEAR))  # 44 -> 45

# The continent x continent flow matrices, written by 41 and 42 and read by 48.
# They exist because a REGIONAL trade split cannot be rebuilt from country
# margins: summing `justice_domestic` over the countries of a region keeps the
# COUNTRY definition of domestic, so an intra-EU flow stays an export inside an
# EU panel. The matrices are folded onto continents at source, where the whole
# D[p, c] / H*[r, p] is still in memory, and the diagonal then means what it says.
CONT_FLOW_CSV    <- acc_csv("hdi_continent_flows")                            # 41 -> 48
VA_CONT_FLOW_CSV <- va_csv("value_added_continent_flows", VA_TAG)             # 42 -> 48

# --- the chains --------------------------------------------------------------
# Keys, not labels: `biofuel_groups`' names are the biofuel_group column 41 and 42
# write and 43-47 join on, so they must not be prettified. Display names below.
biofuel_groups <- list(biogasoline      = "c146",
                       biodiesel        = "c147",
                       renewable_diesel = "c149")   # c149 only; HVO co-products
# and c150/c151 are excluded

# THE POOLED PANEL IS A FOURTH CHAIN ------------------------------------------
# Every figure in 43-48 used to be written twice: once with the chains pooled and
# once faceted by chain. They are now ONE figure whose top row is the pool, which
# is only legitimate because the three groups above are DISJOINT sets of comm
# codes -- the sum over chains is then the total, with nothing counted twice. If
# a future group ever shares a code with another (c150/c151 landing in two HVO
# groups is the obvious way this happens), with_total() would silently double the
# overlap, so the guard is here rather than in a comment.
if (anyDuplicated(unlist(biofuel_groups, use.names = FALSE)))
  stop("[40] biofuel_groups share comm code(s): ",
       paste(unique(unlist(biofuel_groups)[duplicated(unlist(biofuel_groups))]),
             collapse = ", "),
       " -- the pooled 'Total' panel would double count them.")

TOTAL_KEY <- "total"                              # the biofuel_group value it carries

# Total FIRST, so every ordering built from names(biofuel_label) puts the pooled
# panel at the top of the grid without a special case at the call site.
biofuel_label  <- c(total            = "Total",
                    biogasoline      = "Biogasoline",
                    biodiesel        = "Biodiesel",
                    renewable_diesel = "Renewable diesel")
CHAIN_LEVELS   <- names(biofuel_label)

# For facet strips that key on the RAW biofuel_group (43, 47b). 45 and 48 relabel
# the column itself instead, so they do not need this. An unrecognised key is
# de-underscored rather than dropped -- a missing strip is worse than an ugly one.
chain_labeller <- function(x) {
  x <- as.character(x)
  ifelse(x %in% names(biofuel_label), unname(biofuel_label[x]), gsub("_", " ", x))
}

# Duplicate every row into a `total` chain, so ONE facet grid carries the pooled
# panel and the three chains. The duplicate is not aggregated here: the caller's
# existing `by = keys` sum does that, which is what keeps the Total panel exactly
# equal to the pooled figure this replaces.
#
# ONLY VALID ON ADDITIVE COLUMNS. A share, a percentage or a normalised series
# must be derived AFTER this call, from summed numerators and summed
# denominators -- divide first and the Total panel becomes an unweighted mean of
# three chains that are orders of magnitude apart. Equally, run any per-chain
# conservation guard and write any CSV BEFORE calling it, or the check re-checks
# duplicated rows and anything downstream that sums the file over chains doubles.
with_total <- function(dt, col = "biofuel_group") {
  if (!col %in% names(dt))
    stop(sprintf("[%s] with_total(): no `%s` column to pool over.", SCRIPT, col))
  if (TOTAL_KEY %in% as.character(dt[[col]]))
    stop(sprintf("[%s] with_total(): `%s` already carries '%s' -- called twice?",
                 SCRIPT, col, TOTAL_KEY))
  # as.character() first: `dt` may arrive with the column already a factor, and
  # assigning a new level to a factor by reference gives NA and a warning.
  a <- copy(dt); set(a, j = col, value = as.character(a[[col]]))
  b <- copy(a);  set(b, j = col, value = rep(TOTAL_KEY, nrow(b)))
  out <- rbind(a, b)
  v   <- out[[col]]
  # intersect() keeps CHAIN_LEVELS' order, so Total leads; anything unrecognised
  # is kept rather than dropped, and sorts after the known chains.
  lv  <- c(intersect(CHAIN_LEVELS, v), sort(setdiff(unique(v), CHAIN_LEVELS)))
  set(out, j = col, value = factor(v, levels = lv))
  out[]
}

# --- the accounts ------------------------------------------------------------
# The VA account is NAMED for the variant it carries, so a figure can never claim
# one definition while plotting the other. 43 and 47 label their VA column with
# this constant rather than a literal.
VA_ACCOUNT_LABEL <- if (VA_VARIANT == "full") "Value added" else "Value added (ex TLS)"

# The same thing spelled out, for figure subtitles that have room for it (44).
# Kept free of '%': it is pasted into sprintf() format strings.
VA_DEFINITION_NOTE <- if (VA_VARIANT == "full") "wages + capital + TLS" else
  "wages + capital, ex TLS"

ACCOUNT_LEVELS <- c("Production", "Consumption",
                    "HDI justice-based", VA_ACCOUNT_LABEL)

# Okabe-Ito, as elsewhere in the pipeline. An account keeps its colour across
# every figure in the block. setNames(), not c(name = value): the fourth name is
# a variable now, and c() would take `VA_ACCOUNT_LABEL` as the literal name.
# Keyed off ACCOUNT_LEVELS, so the names cannot drift from the levels either.
account_palette <- setNames(c("#0072B2",   # blue
                              "#D55E00",   # vermillion
                              "#009E73",   # green
                              "#E69F00"),  # orange
                            ACCOUNT_LEVELS)

# solid = descriptive (where it happens / who consumes),
# dashed = normative (how it ought to be attributed)
account_linetype <- setNames(c("solid", "solid", "22", "22"), ACCOUNT_LEVELS)

# 43's divergence figures fill by ALLOCATION rather than by account; the two hues
# are the entries above for the same two accounts, so the mapping survives.
alloc_palette <- c("HDI (justice-based)" = "#009E73",
                   "Value added"         = "#E69F00")

# 46 contrasts the two VA definitions
variant_palette <- c("Full VA (wages + capital + TLS)" = "#D55E00",
                     "ex-TLS VA (wages + capital)"     = "#0072B2")

# --- the two periods 43 contrasts --------------------------------------------
PERIODS <- list("2012-2014" = 2012:2014,
                "2020-2022" = 2020:2022)

# --- tolerances --------------------------------------------------------------
# TOL       the accounts re-attribute one total; a drift means the files do not
#           belong together, so the bound is tight.
# NOISE_TOL below this, relative to a row's own size, a defect is float noise.
# VA_GAP_TOL the VA account is NOT conserved: 41 logs the nodes with no VA cell
#           or a negative one as `conservation_gap_pct`, so a small shortfall is
#           a known property, not a fault. Only a gap big enough to bend a share
#           axis is escalated.
# DIV_TOL   43's sum-to-zero and one-total checks run on differences of large,
#           opposite-signed sums, where the cancellation costs a couple of digits.
TOL        <- 1e-6
NOISE_TOL  <- 1e-9
VA_GAP_TOL <- 1e-3
DIV_TOL    <- 1e-4

# --- indicator metadata ------------------------------------------------------
# indicator_meta keys are "LCIM_EQ_terrestrial" / "ibif_total"; STAG is the
# lowercase file tag, minus "_total" -- match case-insensitively.
META <- {
  i <- match(STAG, tolower(sub("_total$", "", indicator_meta$indicator)))
  if (is.na(i)) {
    warning("[40] no indicator_meta entry for '", STAG,
            "'; falling back to the raw tag and scale_factor = 1.")
    list(scale_factor = 1, y_label = STAG, short_label = STAG)
  } else as.list(indicator_meta[i, .(scale_factor, y_label, short_label)])
}

# --- helpers -----------------------------------------------------------------
run_banner <- function()
  message(sprintf(">>> [%s] model='%s' | stressor='%s' | alloc='%s' | VA base='%s'",
                  SCRIPT, model_version, STRESSOR, allocation, VA_BASE))

need <- function(path, who) {
  if (!file.exists(path))
    stop(sprintf("[%s] missing input: %s  (produced by %s)", SCRIPT, path, who))
  path
}

need_cols <- function(dt, cols, path) {
  miss <- setdiff(cols, names(dt))
  if (length(miss))
    stop(sprintf("[%s] %s lacks column(s): %s. Re-run 41/42 -- the metric-neutral column names are expected.",
                 SCRIPT, basename(path), paste(miss, collapse = ", ")))
  invisible(dt)
}

# The MEAN over the years of a period, not the sum, so the two periods stay
# comparable even where a run is short a year. `cols` are averaged, `keys` group.
period_mean <- function(dt, cols, keys) {
  out <- rbindlist(lapply(names(PERIODS), function(p) {
    yrs <- PERIODS[[p]]
    d   <- dt[year %in% yrs]
    if (!nrow(d)) return(NULL)
    n_yr <- uniqueN(d$year)                       # years actually present in the run
    if (n_yr < length(yrs))
      message(sprintf("[%s]   period %s: only %d of %d years present",
                      SCRIPT, p, n_yr, length(yrs)))
    d[, lapply(.SD, function(v) sum(v) / n_yr), by = keys, .SDcols = cols
    ][, period := p][]
  }))
  if (nrow(out)) out[, period := factor(period, levels = names(PERIODS))]
  out
}

save_svg <- function(name, plot, width, height) {
  path <- file.path(PLOT_DIR, paste0(name, ".svg"))
  ggplot2::ggsave(path, plot, device = svglite::svglite, width = width, height = height)
  message(sprintf(">>> [%s] wrote %s", SCRIPT, path))
  invisible(path)
}

# =============================================================================
# CONTINENTS -- one mapping, and one way to fold a matrix onto it
# =============================================================================
# 41 tagged its countries from regions_full.csv FILTERED to `current == TRUE`;
# 42 read the same file UNFILTERED. A country that is current = FALSE therefore
# got a continent in one file and NA in the other, and 43/48 would place it in
# two different panels. Both now go through continent_of(), so there is exactly
# one answer to "which region is this country in" in the whole block.
.cont_env <- new.env(parent = emptyenv())

continent_of <- function(iso, fallback = "Unknown") {
  if (is.null(.cont_env$map)) {
    r <- data.table::fread("inst/regions_full.csv")
    if ("current" %in% names(r)) r <- r[current == TRUE]
    if (!all(c("iso3c", "continent") %in% names(r)))
      stop(sprintf("[%s] inst/regions_full.csv lacks iso3c/continent.", SCRIPT))
    .cont_env$map <- stats::setNames(as.character(r$continent), as.character(r$iso3c))
  }
  out <- unname(.cont_env$map[as.character(iso)])
  bad <- which(is.na(out) | !nzchar(out))
  if (length(bad)) {
    message(sprintf("[%s]   no continent for %s -- bucketed as '%s'", SCRIPT,
                    paste(sort(unique(as.character(iso)[bad])), collapse = ", "), fallback))
    out[bad] <- fallback
  }
  out
}

# The N x R_cont 0/1 aggregator. `levels` fixes the axis order, so every matrix
# 41 and 42 write carries the SAME grid and 48 can align them by position as
# well as by label.
continent_aggregator <- function(iso, levels = NULL) {
  if (!requireNamespace("Matrix", quietly = TRUE))
    stop(sprintf("[%s] continent_aggregator() needs the 'Matrix' package.", SCRIPT))
  cont <- continent_of(iso)
  if (is.null(levels)) levels <- sort(unique(cont))
  Matrix::sparseMatrix(i = seq_along(iso), j = match(cont, levels), x = 1,
                       dims = c(length(iso), length(levels)),
                       dimnames = list(as.character(iso), levels))
}

# S' M S: a country x country matrix folded onto continents on BOTH axes. Row
# and column margins -- and therefore every account total -- are invariant under
# this; only the diagonal/off-diagonal SPLIT moves. That invariance is what 48
# checks itself against.
fold_continents <- function(M, S) {
  out <- as.matrix(Matrix::crossprod(S, M %*% S))
  dimnames(out) <- list(colnames(S), colnames(S))
  out
}

# continent x continent matrix -> long, column-major, so two matrices on the
# same grid can be melted independently and bound by position.
melt_continent_matrix <- function(M, value_name = "value") {
  dt <- data.table(from = rep(rownames(M), times = ncol(M)),
                   to   = rep(colnames(M), each  = nrow(M)),
                   .v   = as.vector(M))
  setnames(dt, ".v", value_name)
  dt[]
}

# =============================================================================
# THE TRADE-SPLIT FIGURE GRAMMAR -- shared by 45 (countries) and 48 (regions)
# =============================================================================
# These lived in 45, with a note that they belong here once a third script needs
# them. 48 is that script.

COMPONENTS <- c("domestic", "export_kept", "import_kept")   # stack order, all >= 0

# THE GREYS, ON A PERCEPTUAL SCALE --------------------------------------------
# R's greyNN is a percentage of white in sRGB and is NOT perceptually uniform,
# so these are picked on CIE L* instead: three fills on equal L* steps, with the
# panel rule pushed OUT of the fill band.
#   export_kept  grey23  #3B3B3B  L* 24.9   darkest: the block that moves
#   domestic     grey47  #787878  L* 50.4
#   import_kept  grey72  #B8B8B8  L* 74.8
#   SEP_COLOUR   grey89  #E3E3E3  L* 90.2   the panel rule
#   background   white            L* 100
# Adjacent pairs in a bar are 25.6 and 24.4 apart; keep any future edit above
# ~12 dL* on those, and keep every fill below the rule.
flow_colors <- c(domestic    = "grey47",
                 export_kept = "grey23",
                 import_kept = "grey72")

# The left side of each arrow names WHERE THE PRODUCTION IS, not where the damage
# materialises -- see NONLOCAL_PATHWAYS. "charged here" means the party the
# account charges: the consumer for PBA/CBA/HDI, the value generator for VA.
# `unit` switches the spatial words between 45's countries and 48's regions; the
# claim is identical, only the resolution of "here" changes.
flow_labels_for <- function(unit = c("country", "region")) {
  unit <- match.arg(unit)
  if (unit == "country")
    c(domestic    = "Production here \u2192 charged here",
      export_kept = "Production here, exported \u2192 charged here",
      import_kept = "Production abroad \u2192 charged here")
  else
    c(domestic    = "Production in-region \u2192 charged in-region",
      export_kept = "Produced in-region, exported OUT of it \u2192 charged in-region",
      import_kept = "Production outside the region \u2192 charged in-region")
}

# Which pathways of an indicator do NOT do their damage where the production is.
# "" = fully local, so no caveat is printed; a MISSING entry gets a warning
# rather than silence, because an unlisted indicator is an unchecked one.
NONLOCAL_PATHWAYS <- list(
  ibif_total                        = "CO2 (global) and NH3 (deposited downwind); only the two land-use terms are local",
  land_harv                         = "",
  LCIM_EQ_terrestrial_land_use      = "",
  LCIM_EQ_marine                    = "climate alone -- this indicator has NO local pathway",
  LCIM_EQ_terrestrial_climate       = "climate alone -- this indicator has NO local pathway",
  LCIM_EQ_terrestrial               = "climate (global) and acidification (deposited downwind)",
  LCIM_EQ_terrestrial_excl_land_use = "climate (global) and acidification (deposited downwind)",
  LCIM_EQ_terrestrial_acidification = "acidification, deposited downwind of the source",
  LCIM_EQ_freshwater                = "climate (global) and eutrophication (carried downstream)"
)

provenance_note <- function(stressor = STRESSOR) {
  np <- NONLOCAL_PATHWAYS[[stressor]]
  if (is.null(np)) {
    warning(sprintf(paste0("[%s] '%s' is not in NONLOCAL_PATHWAYS -- the figure cannot say whether ",
                           "its damage is local. Add an entry before publishing the caption."),
                    SCRIPT, stressor))
    "\nNB the segments locate PRODUCTION, not damage; this indicator's pathways have not been checked."
  } else if (nzchar(np)) {
    sprintf(paste0("\nNB the segments locate the PRODUCTION an impact is attributed to, NOT the place ",
                   "the damage materialises: this indicator includes %s."), np)
  } else ""
}

# The alternating background band. One row per shaded panel and NO facet-row
# column, so ggplot recycles it down every row of a facet_grid and the band runs
# the full height of the column. inherit.aes = FALSE and a CONSTANT fill keep it
# out of the flow legend and out of scale_fill_manual.
#   `panel_col` is the faceting variable: "iso3c" in 45, "continent" in 48.
panel_bands <- function(p, panel_col, separator, band_fill) {
  if (!separator %in% c("band", "both")) return(NULL)
  lv <- levels(p[[panel_col]])
  if (is.null(lv) || length(lv) < 2) return(NULL)
  # The +-Inf corners live in the DATA, not as constants inside aes(): written as
  # aes(xmin = -Inf, ...) every aesthetic is length 1 while the data has one row
  # per shaded panel, and ggplot2 (>= 3.5) suggests annotate() -- which cannot be
  # used here, because the one non-constant column is exactly the point.
  dd <- data.table(.panel = factor(lv[seq(2, length(lv), by = 2)], levels = lv),
                   xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
  setnames(dd, ".panel", panel_col)
  ggplot2::geom_rect(data = dd,
                     ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                     fill = band_fill, inherit.aes = FALSE)
}

# --- fitting a title block to the canvas -------------------------------------
# ggplot2 does NOT wrap plot.subtitle: a line longer than the canvas is drawn
# past the edge and lost. Hard-wrap it, then grow the CANVAS by the height of the
# wrapped block instead of letting the panels shrink under it.
WRAP_MARGIN <- 0.92

wrap_lines <- function(txt, width_in, pt) {
  if (!nzchar(txt)) return(txt)
  n <- max(40L, as.integer(WRAP_MARGIN * width_in * 72 / (0.5 * pt)))
  paste(vapply(strsplit(txt, "\n", fixed = TRUE)[[1]],
               function(par) if (nzchar(par)) paste(strwrap(par, width = n), collapse = "\n") else "",
               character(1), USE.NAMES = FALSE),
        collapse = "\n")
}

text_height_in <- function(txt, pt, lead = 1.35)
  if (nzchar(txt)) length(strsplit(txt, "\n", fixed = TRUE)[[1]]) * pt * lead / 72 else 0

# Title and subtitle off the CANVAS, not out of the record: with show_titles =
# FALSE the text goes to captions_*.txt beside the SVGs, one block per figure.
# `show_titles` is an ARGUMENT rather than a global read at call time -- these
# functions live in 40 now, and a silently inherited switch is exactly how a
# caption file would end up describing the wrong figure.
lab_block <- function(stem, ttl, sub, show_titles)
  if (isTRUE(show_titles)) character(0) else c(paste0("## ", stem, ".svg"), ttl, sub, "")

write_captions <- function(caps, tag, stamp = as.character(PLOT_YEAR)) {
  if (!length(caps)) return(invisible(NULL))
  path <- file.path(PLOT_DIR, sprintf("captions_%s_%s_%s_%s.txt", tag, STAG, ATAG, stamp))
  writeLines(caps, path)
  message(sprintf(">>> [%s] title/subtitle not drawn; caption text -> %s", SCRIPT, path))
  invisible(path)
}