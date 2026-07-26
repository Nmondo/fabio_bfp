# =============================================================================
# 46_plot_responsibility_timeseries.R
# TEMPORAL development of the four responsibility accounts (production,
# consumption, HDI justice-based, value added ex TLS) at COUNTRY x BIOFUEL
# resolution: one panel per country, four lines per panel, one figure per chain.
#
# Nothing is recomputed here. 40 and 41 already resolve every account to
# (year, biofuel_group, iso3c); this script selects countries, normalises, and
# plots. It is the time-series counterpart of 42 (which collapses the years into
# two periods and the countries into continents).
#
# READS  <IN_DIR>/                                           (same pair as 42)
#   FABIO_bcp_<ind>_hdi_responsibility_<alloc>.csv                         (40)
#   FABIO_bcp_<ind>_value_added_responsibility_<vabase>_<alloc>_ex_tls.csv (41)
#
# WRITES output/plot/ -- two views per biofuel chain
#   responsibility_ts_share_<bf>_<ind>_<vabase>_<alloc>.svg
#   responsibility_ts_abs_<bf>_<ind>_<vabase>_<alloc>.svg
#
# THE THREE DESIGN DECISIONS, AND WHY --------------------------------------
#
# [1] SHARE IS THE PRIMARY VIEW, ABSOLUTE THE COMPANION.
#     The four accounts are four attributions of ONE identical total (42 checks
#     this; so does check_identity() below), so within a country-year the only
#     information is HOW THE TOTAL IS SPLIT, never the level. On an absolute
#     axis that signal competes with the growth of the chain itself, and for
#     renewable diesel the chain grows ~8x over 2012-2022: all four lines sweep
#     upward together and the re-attribution disappears into the trend. Dividing
#     by the chain's global total for that YEAR removes the growth and leaves the
#     redistribution. The absolute view is still written, because a share alone
#     cannot say whether a rising share is a country doing more or the rest of
#     the world doing less -- read the two together.
#
# [2] COUNTRIES ARE THE UNION OF THE TOP N UNDER *EACH* ACCOUNT, NOT THE TOP N
#     OF ONE. Relevance is account-dependent, and the disagreement is the point.
#     In this run's renewable-diesel chain: IDN is ~17% of production and ~0% of
#     consumption; SWE is ~22% of consumption and ~5% of production; NLD is ~12%
#     of value added and ~1% of production. Ranking on any single account (or on
#     the mean of the four, which averages the divergence away) drops exactly the
#     countries the figure exists to show. The union of the top N per account
#     costs a few panels -- N = 6 gives 7-9 countries here -- and keeps them.
#     Selection runs on the mean of SEL_YEARS (the recent end), per chain, so a
#     chain's panel set reflects what it looks like now, not in 2012.
#
# [3] THE PRODUCTION-CONSUMPTION RIBBON. The shaded band between the two
#     descriptive accounts is the responsibility gap; the two normative accounts
#     (justice, VA) are then read by WHERE THEY FALL INSIDE OR OUTSIDE IT. Solid
#     lines = descriptive (where it happens / who consumes), dashed = normative
#     (how it ought to be attributed). Colours are 42's account_palette, so the
#     accounts keep their identity across the two figures.
#
# Y IS FREE PER PANEL (FREE_Y, default TRUE). The leaders dwarf the tail (USA is
# ~28% of renewable diesel, NOR ~2%); a common axis flattens every panel but the
# first. The x axis is shared, so trajectories remain comparable in SHAPE across
# panels -- heights are not. Set FREE_Y = FALSE for one common scale.
#
# RUN: Rscript R/46_plot_responsibility_timeseries.R   (after 40 and 41)
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
library(ggplot2)
library(svglite)

source("R/19_plot_definitions.R")   # indicator_meta

model_version <- Sys.getenv("FABIO_RUN_MODE", unset = "rescaled")
model_version <- if (tolower(trimws(model_version)) == "bypass") "bypass" else "rescaled"
IN_DIR   <- if (model_version == "bypass") "output/bypass" else "output"
PLOT_DIR <- file.path("output", "plot")
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)
message(sprintf(">>> [46] model_version = '%s'  (reading responsibility CSVs from: %s)",
                model_version, IN_DIR))

# --- config ------------------------------------------------------------------
TOP_N      <- 6                 # per ACCOUNT; the panel set is the union of the four
SEL_YEARS  <- 2020:2022         # countries are picked on the mean of these years
SHOW_ROW   <- TRUE              # add a "Rest of world" remainder panel
FREE_Y     <- TRUE              # see the header
NCOL       <- 3                 # panels per row
MANUAL_COUNTRIES <- NULL        # e.g. c("USA","BRA","IDN") to override the selection

ACCOUNT_LEVELS <- c("Production", "Consumption",
                    "HDI justice-based", "Value added (ex TLS)")

# 42's palette, unchanged -- an account keeps its colour across the two figures
account_palette <- c("Production"           = "#0072B2",   # blue
                     "Consumption"          = "#D55E00",   # vermillion
                     "HDI justice-based"    = "#009E73",   # green
                     "Value added (ex TLS)" = "#E69F00")   # orange

# solid = descriptive (observed), dashed = normative (attributed)
account_linetype <- c("Production"           = "solid",
                      "Consumption"          = "solid",
                      "HDI justice-based"    = "22",
                      "Value added (ex TLS)" = "22")

ROW_LABEL <- "Rest of world"

# Two different tolerances, because two different things can go wrong ---------
# PBA / CBA / justice are re-attributions of a footprint that is conserved BY
# CONSTRUCTION, so any drift between them means the three files do not belong to
# one another (wrong allocation, wrong VA base, a country dropped) -- a real
# error, hence a tight bound.
# VALUE ADDED IS NOT CONSERVED, and 40 says so itself: nodes where the VA base
# has no cell (`no_VA_cell`) or a negative one (`negative_VA`) cannot take an
# attribution, so their footprint is never allocated. 40 quantifies the residual
# per (year, chain) as `conservation_gap_pct` in its coverage CSV -- e.g. -0.0012%
# for biogasoline 2020 in the ibif/exiobase/value run, from Finnish fodder crops
# and NLD/HKG/KOR palm oil (see the gaps CSV). That is a KNOWN, LOGGED property of
# the account, not a fault of this figure, so VA gets a loose bound and a message
# rather than a warning; only a gap large enough to bend the share view (> 0.1%)
# is escalated.
TOL        <- 1e-6    # PBA / CBA / justice against each other
VA_GAP_TOL <- 1e-3    # VA against them; cf. 40's conservation_gap_pct

biofuel_label <- c(biogasoline      = "Biogasoline",
                   biodiesel        = "Biodiesel",
                   renewable_diesel = "Renewable diesel")

# --- indicator metadata (identical to 42) ------------------------------------
meta_for <- function(ind_tag) {
  key <- tolower(sub("_total$", "", indicator_meta$indicator))
  i   <- match(tolower(ind_tag), key)
  if (is.na(i)) {
    warning("[46] no indicator_meta entry for '", ind_tag,
            "'; falling back to the raw tag and scale_factor = 1.")
    return(list(scale_factor = 1, y_label = ind_tag, short_label = ind_tag))
  }
  as.list(indicator_meta[i, .(scale_factor, y_label, short_label)])
}

# --- discovery (identical to 42) ---------------------------------------------
discover_runs <- function(dir = IN_DIR) {
  pat   <- "^FABIO_bcp_(.+)_value_added_responsibility_([^_]+)_([^_]+)_ex_tls\\.csv$"
  files <- list.files(dir, pattern = pat)
  if (!length(files)) return(data.table())
  m <- regmatches(files, regexec(pat, files))
  runs <- rbindlist(lapply(m, function(x)
    data.table(indicator = x[2], va_base = x[3], alloc = x[4])))
  runs[, `:=`(
    va_file  = file.path(dir, sprintf("FABIO_bcp_%s_value_added_responsibility_%s_%s_ex_tls.csv",
                                      indicator, va_base, alloc)),
    hdi_file = file.path(dir, sprintf("FABIO_bcp_%s_hdi_responsibility_%s.csv",
                                      indicator, alloc)))]
  runs[]
}

need_cols <- function(dt, cols, path) {
  miss <- setdiff(cols, names(dt))
  if (length(miss))
    stop("[46] ", basename(path), " lacks column(s): ", paste(miss, collapse = ", "),
         ". Re-run 40/41.")
  invisible(dt)
}

# --- long table: year x biofuel x country x account --------------------------
# The VA bar comes from the _ex_tls file, as in 42.
load_run <- function(run) {
  va  <- fread(run$va_file)
  hdi <- fread(run$hdi_file)
  
  need_cols(va,  c("year", "va_variant", "biofuel_group", "va_iso3c", "va_resp"), run$va_file)
  need_cols(hdi, c("year", "biofuel_group", "iso3c", "production_based",
                   "consumption_based", "justice_based"), run$hdi_file)
  
  long <- rbindlist(list(
    hdi[, .(year, biofuel_group, iso3c, account = "Production",  value = production_based)],
    hdi[, .(year, biofuel_group, iso3c, account = "Consumption", value = consumption_based)],
    hdi[, .(year, biofuel_group, iso3c, account = "HDI justice-based", value = justice_based)],
    va[va_variant == "ex_tls",
       .(year, biofuel_group, iso3c = va_iso3c,
         account = "Value added (ex TLS)", value = va_resp)]
  ), use.names = TRUE)
  
  long <- long[, .(value = sum(value)),
               by = .(year, biofuel_group, iso3c, account)]
  long[, account := factor(account, levels = ACCOUNT_LEVELS)]
  long[]
}

# --- one total, four attributions: the invariant the share view relies on ----
# Returns the CONSERVED total (the PBA one) as the share denominator, so a VA gap
# shows up honestly as a chain whose VA shares sum to slightly under 100% rather
# than being normalised away.
check_identity <- function(d, what) {
  tot <- dcast(d[, .(total = sum(value)), by = .(year, biofuel_group, account)],
               year + biofuel_group ~ account, value.var = "total")
  setnames(tot, ACCOUNT_LEVELS, c("pba", "cba", "jus", "va"))
  
  # [1] the three conserved accounts must agree -- if they do not, the files do
  #     not belong together and nothing below is trustworthy.
  tot[, rel_core := {
    hi <- pmax(pba, cba, jus); lo <- pmin(pba, cba, jus)
    fifelse(abs(hi) > 0, (hi - lo) / abs(hi), 0)
  }]
  if (any(tot$rel_core > TOL)) {
    bad <- tot[rel_core > TOL][order(-rel_core)]
    warning(sprintf("[46] %s: production/consumption/justice do not share one total (worst %s %d, rel %.3g) -- are the accounts and VA files from the SAME indicator, VA base and allocation?",
                    what, bad$biofuel_group[1], bad$year[1], bad$rel_core[1]))
  }
  
  # [2] VA is allowed to fall short by 40's conservation gap; report it, and only
  #     escalate if it grows big enough to matter for the share axis.
  tot[, va_gap := fifelse(abs(pba) > 0, (va - pba) / abs(pba), 0)]
  gaps <- tot[abs(va_gap) > TOL][order(va_gap)]
  if (nrow(gaps)) {
    message(sprintf("[46]   VA conservation gap (40's `no_VA_cell` / `negative_VA` nodes) in %d of %d chain-years; worst %s %d: %+.4f%% of the total. VA shares will sum to that little under 100%%.",
                    nrow(gaps), nrow(tot), gaps$biofuel_group[1], gaps$year[1],
                    100 * gaps$va_gap[1]))
    if (max(abs(gaps$va_gap)) > VA_GAP_TOL)
      warning(sprintf("[46] %s: the VA account is short by up to %.3f%% of the chain total -- large enough to distort the share view. Check 40's coverage CSV (`conservation_gap_pct`) before using these panels.",
                      what, 100 * max(abs(gaps$va_gap))))
  }
  
  tot[, .(year, biofuel_group, chain_total = pba)]
}

# --- union of the top N under EACH account, on the SEL_YEARS mean ------------
select_countries <- function(d_bf) {
  if (!is.null(MANUAL_COUNTRIES)) return(MANUAL_COUNTRIES)
  s <- d_bf[year %in% SEL_YEARS,
            .(m = sum(value) / uniqueN(year)), by = .(account, iso3c)]
  picks <- s[order(account, -m), head(.SD, TOP_N), by = account]$iso3c
  keep  <- unique(picks)
  # order the panels by the largest share the country reaches under ANY account
  ord <- s[iso3c %in% keep, .(m = max(m)), by = iso3c][order(-m), iso3c]
  ord
}

# --- plot one (biofuel, view) ------------------------------------------------
plot_ts <- function(d_bf, countries, chain_tot, meta, bf, run, share) {
  d <- copy(d_bf)
  d[, panel := fifelse(iso3c %in% countries, iso3c, ROW_LABEL)]
  if (!SHOW_ROW) d <- d[panel != ROW_LABEL]
  d <- d[, .(value = sum(value)), by = .(year, panel, account)]
  
  # every panel x account x year present, so a zero reads as a zero, not a gap
  d <- d[CJ(year = sort(unique(d$year)), panel = unique(d$panel),
            account = factor(ACCOUNT_LEVELS, levels = ACCOUNT_LEVELS), unique = TRUE),
         on = .(year, panel, account)]
  d[is.na(value), value := 0]
  
  if (share) {
    d <- merge(d, chain_tot[biofuel_group == bf, .(year, chain_total)], by = "year", all.x = TRUE)
    d[, value := 100 * value / chain_total]
    y_lab <- sprintf("Share of the %s chain's global %s (%%)",
                     tolower(biofuel_label[[bf]]), meta$short_label)
    sub <- "Each year is normalised on that year's chain total, so the growth of the chain is divided out: what moves here is the ATTRIBUTION, not the volume."
  } else {
    d[, value := value / meta$scale_factor]
    y_lab <- meta$y_label
    sub <- "Absolute. All four lines rise when the chain grows -- read the share figure to separate growth from re-attribution."
  }
  
  lev <- c(intersect(countries, unique(d$panel)),
           if (ROW_LABEL %in% d$panel) ROW_LABEL)
  d[, panel := factor(panel, levels = lev)]
  
  # the production-consumption gap, as a band
  rib <- dcast(d[account %in% c("Production", "Consumption")],
               year + panel ~ account, value.var = "value")
  setnames(rib, c("Production", "Consumption"), c("pba", "cba"))
  rib[, `:=`(lo = pmin(pba, cba), hi = pmax(pba, cba))]
  
  ggplot(d, aes(x = year, y = value)) +
    geom_ribbon(data = rib, aes(x = year, ymin = lo, ymax = hi),
                inherit.aes = FALSE, fill = "grey50", alpha = 0.16) +
    geom_line(aes(colour = account, linetype = account), linewidth = 0.7) +
    geom_point(aes(colour = account), size = 0.9) +
    scale_colour_manual(values = account_palette, drop = FALSE) +
    scale_linetype_manual(values = account_linetype, drop = FALSE) +
    scale_x_continuous(breaks = scales::breaks_width(4)) +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
    facet_wrap(~ panel, ncol = NCOL, scales = if (FREE_Y) "free_y" else "fixed") +
    labs(x = NULL, y = y_lab, colour = "Account", linetype = "Account",
         title = sprintf("%s responsibility over time - %s (%s VA base, %s allocation)",
                         meta$short_label, biofuel_label[[bf]], run$va_base, run$alloc),
         subtitle = paste0(sub, if (FREE_Y) "\nY axis is FREE PER PANEL: compare trajectories across countries, not heights.")) +
    guides(colour = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1)) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          legend.title    = element_text(face = "bold"),
          strip.text      = element_text(face = "bold", hjust = 0),
          panel.grid.minor = element_blank(),
          plot.subtitle   = element_text(size = 8.5, colour = "grey30"))
}

# --- run ---------------------------------------------------------------------
runs <- discover_runs()

if (!nrow(runs)) {
  message(">>> [46] no `*_value_added_responsibility_*_ex_tls.csv` in ", IN_DIR,
          " -- nothing to plot.")
} else {
  message(sprintf(">>> [46] %d run(s) discovered in %s", nrow(runs), IN_DIR))
  
  for (i in seq_len(nrow(runs))) {
    run <- runs[i]
    key <- sprintf("%s / %s / %s", run$indicator, run$va_base, run$alloc)
    if (!file.exists(run$va_file))  { message("[46] skip ", key, ": no ex_tls value-added file"); next }
    if (!file.exists(run$hdi_file)) { message("[46] skip ", key, ": no HDI responsibility file"); next }
    
    message(">>> [46] ", key)
    meta      <- meta_for(run$indicator)
    d         <- load_run(run)
    chain_tot <- check_identity(d, key)
    
    for (bf in intersect(names(biofuel_label), unique(d$biofuel_group))) {
      d_bf <- d[biofuel_group == bf]
      cty  <- select_countries(d_bf)
      if (!length(cty)) { message("[46]   ", bf, ": no countries selected"); next }
      
      cov <- d_bf[year %in% SEL_YEARS & account == "Production",
                  .(v = sum(value)), by = .(sel = iso3c %in% cty)]
      message(sprintf("[46]   %-17s %d panels: %s  (%.0f%% of production)",
                      bf, length(cty), paste(cty, collapse = " "),
                      100 * cov[sel == TRUE, sum(v)] / cov[, sum(v)]))
      
      n_panel <- length(cty) + as.integer(SHOW_ROW)
      n_row   <- ceiling(n_panel / NCOL)
      for (share in c(TRUE, FALSE)) {
        f <- sprintf("responsibility_ts_%s_%s_%s_%s_%s.svg",
                     if (share) "share" else "abs", bf,
                     run$indicator, run$va_base, run$alloc)
        ggsave(file.path(PLOT_DIR, f),
               plot_ts(d_bf, cty, chain_tot, meta, bf, run, share),
               device = svglite::svglite,
               width  = 1.2 + 3.2 * NCOL,
               height = 2.2 + 2.4 * n_row)
      }
    }
  }
  message(">>> [46] plots written to ", PLOT_DIR)
}