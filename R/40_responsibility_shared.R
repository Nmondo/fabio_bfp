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
VA_BASE    <- "gloria"     # "gloria" | "exiobase"
VA_VARIANT <- "full"         # VA definition carried by the account 43-45/47 draw:
#   "full"   wages + capital + taxes-less-subsidies
#   "ex_tls" wages + capital
# 42 writes BOTH; this only picks which one is read
# back. 46 contrasts the two and ignores this switch.
PLOT_YEAR  <- 2022L          # the single year every by-country figure shows
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

# --- the chains --------------------------------------------------------------
# Keys, not labels: `biofuel_groups`' names are the biofuel_group column 41 and 42
# write and 43-47 join on, so they must not be prettified. Display names below.
biofuel_groups <- list(biogasoline      = "c146",
                       biodiesel        = "c147",
                       renewable_diesel = "c149")   # c149 only; HVO co-products
# and c150/c151 are excluded
biofuel_label  <- c(biogasoline      = "Biogasoline",
                    biodiesel        = "Biodiesel",
                    renewable_diesel = "Renewable diesel")

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