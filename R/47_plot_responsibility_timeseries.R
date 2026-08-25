# =============================================================================
# 47_plot_responsibility_timeseries.R
# TEMPORAL development of the four responsibility accounts (production,
# consumption, HDI justice-based, value added in 40's VA_VARIANT) at COUNTRY x
# BIOFUEL resolution: one panel per country, four lines per panel, one figure per
# chain and panel set.
#
# Nothing is recomputed. 41 and 42 already resolve every account to
# (year, biofuel_group, iso3c); this script selects countries, normalises, and
# plots. It is the time-series counterpart of 43, which collapses the years into
# two periods and the countries into continents.
#
# READS  HDI_CSV (41), VA_RESP_CSV (42), ORDER_CSV (44, for the `producers` set)
# WRITES output/plot/ -- two views per chain per panel set
#   responsibility_ts_share_<set>_<bf>_<ind>_<vabase>_<alloc>.svg
#   responsibility_ts_abs_<set>_<bf>_<ind>_<vabase>_<alloc>.svg
#
# THE THREE DESIGN DECISIONS, AND WHY -----------------------------------------
#
# [1] SHARE IS THE PRIMARY VIEW, ABSOLUTE THE COMPANION.
#     The four accounts are four attributions of ONE identical total, so within a
#     country-year the only information is HOW THE TOTAL IS SPLIT, never the
#     level. On an absolute axis that signal competes with the growth of the
#     chain itself, and for renewable diesel the chain grows ~8x over 2012-2022:
#     all four lines sweep upward together and the re-attribution disappears into
#     the trend. Dividing by the chain's global total for that YEAR removes the
#     growth and leaves the redistribution. The absolute view is still written,
#     because a share alone cannot say whether a rising share is a country doing
#     more or the rest of the world doing less -- read the two together.
#
# [2] TWO PANEL SETS, BECAUSE THERE ARE TWO QUESTIONS (PANEL_SETS).
#     `union` = the union of the top N under *EACH* account, per chain. Relevance
#     is account-dependent and the disagreement is the point: in this run's
#     renewable-diesel chain IDN is ~17% of production and ~0% of consumption;
#     SWE is ~22% of consumption and ~5% of production; NLD is ~12% of value
#     added and ~1% of production. Ranking on any single account (or on the mean
#     of the four, which averages the divergence away) drops exactly the
#     countries the figure exists to show. The union costs a few panels -- N = 6
#     gives 7-9 countries here -- and keeps them. Selection runs on the mean of
#     SEL_YEARS (the recent end), per chain, so a chain's panel set reflects what
#     it looks like now, not in 2012.
#     `producers` = 44's set, read from ORDER_CSV: the five largest holders of the
#     POOLED production-based account in PLOT_YEAR, in 44's left-to-right order,
#     so 44, 45, 47b and this script cannot name different countries. Pooled and
#     fixed, it puts the SAME five panels in all three chain figures, which is
#     what makes the chains readable against each other -- and is why a country
#     at ~0 in one chain keeps its (flat) panel there rather than dropping out.
#
# [3] THE PRODUCTION-CONSUMPTION RIBBON. The shaded band between the two
#     descriptive accounts is the responsibility gap; the two normative accounts
#     (justice, VA) are then read by WHERE THEY FALL INSIDE OR OUTSIDE IT. Solid
#     lines = descriptive (where it happens / who consumes), dashed = normative
#     (how it ought to be attributed).
#
# Y IS FREE PER PANEL (FREE_Y, default TRUE). The leaders dwarf the tail (USA is
# ~28% of renewable diesel, NOR ~2%); a common axis flattens every panel but the
# first. The x axis is shared, so trajectories remain comparable in SHAPE across
# panels -- heights are not. Set FREE_Y = FALSE for one common scale.
#
# RUN: Rscript R/47_plot_responsibility_timeseries.R   (after 41, 42 and 44)
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

source("R/40_responsibility_shared.R")
SCRIPT <- "47"
run_banner()

# --- figure switches ---------------------------------------------------------
PANEL_SETS <- c("union", "producers")     # one share and one absolute figure per chain EACH
TOP_N      <- 6                           # `union`: per ACCOUNT; the panel set is the union of the four
SEL_YEARS  <- (PLOT_YEAR - 2):PLOT_YEAR   # `union`: countries are picked on the mean of these years
SHOW_ROW   <- TRUE                        # add a "Rest of world" remainder panel
FREE_Y     <- TRUE                        # see the header
NCOL       <- 3                           # panels per row
MANUAL_COUNTRIES <- NULL                  # e.g. c("USA","BRA","IDN"); replaces PANEL_SETS with one hand-set figure

PRODUCERS_SET <- "producers"   # which of 44's `country_set` blocks the set is read from ("eu" also works)
PRODUCERS_N   <- 5L            # only for the fallback below; NA makes a missing order file an error

ROW_LABEL <- "Rest of world"

# --- long table: year x biofuel x country x account --------------------------
load_run <- function() {
  va  <- fread(need(VA_RESP_CSV, "42"))
  hdi <- fread(need(HDI_CSV,     "41"))
  
  need_cols(va,  c("year", "va_variant", "biofuel_group", "va_iso3c", "va_resp"), VA_RESP_CSV)
  need_cols(hdi, c("year", "biofuel_group", "iso3c", "production_based",
                   "consumption_based", "justice_based"), HDI_CSV)
  
  long <- rbindlist(list(
    hdi[, .(year, biofuel_group, iso3c, account = "Production",  value = production_based)],
    hdi[, .(year, biofuel_group, iso3c, account = "Consumption", value = consumption_based)],
    hdi[, .(year, biofuel_group, iso3c, account = "HDI justice-based", value = justice_based)],
    va[va_variant == VA_VARIANT,
       .(year, biofuel_group, iso3c = va_iso3c,
         account = VA_ACCOUNT_LABEL, value = va_resp)]
  ), use.names = TRUE)
  
  long <- long[, .(value = sum(value)), by = .(year, biofuel_group, iso3c, account)]
  long[, account := factor(account, levels = ACCOUNT_LEVELS)]
  long[]
}

# --- one total, four attributions: the invariant the share view relies on ----
# Returns the CONSERVED total (the PBA one) as the share denominator, so a VA gap
# shows up honestly as a chain whose VA shares sum to slightly under 100% rather
# than being normalised away.
check_identity <- function(d) {
  tot <- dcast(d[, .(total = sum(value)), by = .(year, biofuel_group, account)],
               year + biofuel_group ~ account, value.var = "total")
  # dcast drops a level with no rows at all, and setnames would then fail with an
  # opaque "items of 'old' not found". Say what is actually missing instead: an
  # absent account means one of the two CSVs is from a different run (or 42 was
  # never run for the selected VA variant), which is the same diagnosis the
  # identity check below gives for an account that is merely inconsistent.
  miss <- setdiff(ACCOUNT_LEVELS, names(tot))
  if (length(miss))
    stop(sprintf(paste0("[47] no rows at all for account(s): %s. The accounts (41) and value-added ",
                        "(42) CSVs must come from the SAME indicator, VA base and allocation, and 42 ",
                        "must have written the '%s' variant."),
                 paste(miss, collapse = ", "), VA_VARIANT))
  setnames(tot, ACCOUNT_LEVELS, c("pba", "cba", "jus", "va"))
  
  # [1] the three conserved accounts must agree -- if they do not, the files do
  #     not belong together and nothing below is trustworthy.
  tot[, rel_core := {
    hi <- pmax(pba, cba, jus); lo <- pmin(pba, cba, jus)
    fifelse(abs(hi) > 0, (hi - lo) / abs(hi), 0)
  }]
  if (any(tot$rel_core > TOL)) {
    bad <- tot[rel_core > TOL][order(-rel_core)]
    warning(sprintf("[47] production/consumption/justice do not share one total (worst %s %d, rel %.3g) -- are the accounts and VA files from the SAME indicator, VA base and allocation?",
                    bad$biofuel_group[1], bad$year[1], bad$rel_core[1]))
  }
  
  # [2] VA is allowed to fall short by 42's conservation gap; report it, and only
  #     escalate if it grows big enough to matter for the share axis.
  tot[, va_gap := fifelse(abs(pba) > 0, (va - pba) / abs(pba), 0)]
  gaps <- tot[abs(va_gap) > TOL][order(va_gap)]
  if (nrow(gaps)) {
    message(sprintf("[47]   VA conservation gap (42's `no_VA_cell` / `negative_VA` nodes) in %d of %d chain-years; worst %s %d: %+.4f%% of the total. VA shares will sum to that little under 100%%.",
                    nrow(gaps), nrow(tot), gaps$biofuel_group[1], gaps$year[1],
                    100 * gaps$va_gap[1]))
    if (max(abs(gaps$va_gap)) > VA_GAP_TOL)
      warning(sprintf("[47] the VA account is short by up to %.3f%% of the chain total -- large enough to distort the share view. Check 42's coverage CSV (`conservation_gap_pct`) before using these panels.",
                      100 * max(abs(gaps$va_gap))))
  }
  
  tot[, .(year, biofuel_group, chain_total = pba)]
}

# --- `union`: top N under EACH account, on the SEL_YEARS mean, per chain -----
select_union <- function(d_bf) {
  s <- d_bf[year %in% SEL_YEARS,
            .(m = sum(value) / uniqueN(year)), by = .(account, iso3c)]
  keep <- unique(s[order(account, -m), head(.SD, TOP_N), by = account]$iso3c)
  # order the panels by the largest share the country reaches under ANY account
  as.character(s[iso3c %in% keep, .(m = max(m)), by = iso3c][order(-m), iso3c])
}

# --- `producers`: 44's set, taken as given -----------------------------------
# Resolved once from the whole table, not per chain: 44 ranks on the POOLED
# account, so the set belongs to the run rather than to a chain. `position` is
# 44's order, kept so a country sits in the same slot here, in 44 and in 45.
select_producers <- function(d) {
  fallback <- function(why) {
    if (is.na(PRODUCERS_N))
      stop("[47] ", why, " -- run 44 for PLOT_YEAR = ", PLOT_YEAR, " or set PRODUCERS_N.")
    message("[47] ", why, " -- falling back to this script's own pooled-PBA top ", PRODUCERS_N, ".")
    s <- d[year == PLOT_YEAR & account == "Production" & biofuel_group %in% CHAIN_LEVELS,
           .(v = sum(value)), by = iso3c][order(-v)]
    as.character(head(s$iso3c, PRODUCERS_N))
  }
  
  if (!file.exists(ORDER_CSV))
    return(fallback(paste0("no 44 order file (", basename(ORDER_CSV), ")")))
  ord <- fread(ORDER_CSV)
  if (!all(c("position", "iso3c") %in% names(ord)))
    stop("[47] ", basename(ORDER_CSV), " lacks 'position'/'iso3c' -- re-run 44.")
  if (!"country_set" %in% names(ord)) ord[, country_set := "all"]
  if (!"unit"        %in% names(ord)) ord[, unit        := "country"]
  ord <- ord[unit == "country"]            # a continent name in `iso3c` has no panel here
  if (!PRODUCERS_SET %in% ord$country_set)
    return(fallback(sprintf("set '%s' is not in %s (has: %s)", PRODUCERS_SET,
                            basename(ORDER_CSV),
                            paste(unique(ord$country_set), collapse = ", "))))
  
  o    <- ord[country_set == PRODUCERS_SET][order(position)]
  miss <- setdiff(o$iso3c, unique(d$iso3c))
  if (length(miss))
    message("[47] ", length(miss), " of 44's '", PRODUCERS_SET,
            "' countries have no rows in the accounts CSVs, dropped: ",
            paste(miss, collapse = ", "))
  keep <- as.character(o$iso3c[o$iso3c %in% unique(d$iso3c)])
  if (!length(keep))
    stop("[47] none of 44's '", PRODUCERS_SET, "' countries appear in ", basename(HDI_CSV), ".")
  message(sprintf("[47] set '%s' FOLLOWS 44 (%s): %s",
                  PRODUCERS_SET, basename(ORDER_CSV), paste(keep, collapse = " ")))
  keep
}

# --- the sets: `pick` takes one chain's rows and returns its countries -------
# A fixed set ignores the argument -- that is what makes it fixed. `note` says in
# the subtitle how the panels were picked, without which two figures of the same
# chain differ only in panels and cannot be told apart outside output/plot/.
panel_sets <- function(d) {
  if (!is.null(MANUAL_COUNTRIES))
    return(list(manual = list(
      tag  = "manual",
      pick = function(d_bf) as.character(MANUAL_COUNTRIES),
      note = "Panels: set by hand (MANUAL_COUNTRIES), not by any ranking.")))
  
  unknown <- setdiff(PANEL_SETS, c("union", "producers"))
  if (length(unknown))
    stop("[47] unknown panel set(s): ", paste(unknown, collapse = ", "),
         " -- PANEL_SETS takes 'union' and/or 'producers'.")
  if (!length(PANEL_SETS)) stop("[47] PANEL_SETS is empty -- nothing to draw.")
  
  out <- list()
  if ("union" %in% PANEL_SETS)
    out$union <- list(
      tag  = "union",
      pick = select_union,
      note = sprintf(paste("Panels: the union of the %d largest countries under EACH account, on the %d-%d",
                           "mean -- relevance is account-dependent, so the set differs by chain."),
                     TOP_N, min(SEL_YEARS), max(SEL_YEARS)))
  if ("producers" %in% PANEL_SETS) {
    cty <- select_producers(d)
    out$producers <- list(
      tag  = "producers",
      pick = function(d_bf) cty,
      note = sprintf(paste("Panels: the %d largest producers by the pooled production-based account in %d",
                           "(44's set) -- the same countries, in the same order, in every chain."),
                     length(cty), PLOT_YEAR))
  }
  out
}

# --- plot one (biofuel, set, view) -------------------------------------------
plot_ts <- function(d_bf, countries, chain_tot, bf, share, set_note) {
  d <- copy(d_bf)
  d[, panel := fifelse(iso3c %in% countries, iso3c, ROW_LABEL)]
  if (!SHOW_ROW) d <- d[panel != ROW_LABEL]
  d <- d[, .(value = sum(value)), by = .(year, panel, account)]
  
  # every panel x account x year present, so a zero reads as a zero, not a gap.
  # The levels come from `countries` rather than from the data: a country in a
  # fixed set can have no rows at all in one chain, and its panel has to be drawn
  # as a zero there -- otherwise the chain figures lose the shared panel set that
  # is the reason for choosing a fixed one.
  panels <- c(countries, if (SHOW_ROW && ROW_LABEL %in% d$panel) ROW_LABEL)
  d <- d[CJ(year = sort(unique(d$year)), panel = panels,
            account = factor(ACCOUNT_LEVELS, levels = ACCOUNT_LEVELS), unique = TRUE),
         on = .(year, panel, account)]
  d[is.na(value), value := 0]
  
  if (share) {
    d <- merge(d, chain_tot[biofuel_group == bf, .(year, chain_total)], by = "year", all.x = TRUE)
    # A chain-year with no footprint at all has no share to show: dividing by it
    # gives Inf/NaN panels rather than the empty ones the guards elsewhere produce.
    bad_yr <- d[!is.finite(chain_total) | chain_total == 0, sort(unique(year))]
    if (length(bad_yr)) {
      message(sprintf("[47]   %s: chain total is zero or absent in %s -- dropped from the share view.",
                      bf, paste(bad_yr, collapse = ", ")))
      d <- d[is.finite(chain_total) & chain_total != 0]
    }
    if (!nrow(d)) return(NULL)
    d[, value := 100 * value / chain_total]
    # the unit cancels in a share, so the indicator is named in the title, not here
    y_lab <- sprintf("Share of the %s chain's global total (%%)", tolower(biofuel_label[[bf]]))
    sub <- "Each year is normalised on that year's chain total, so the growth of the chain is divided out: what moves here is the ATTRIBUTION, not the volume."
  } else {
    d[, value := value / META$scale_factor]
    y_lab <- META$y_label
    sub <- "Absolute. All four lines rise when the chain grows -- read the share figure to separate growth from re-attribution."
  }
  
  d[, panel := factor(panel, levels = panels)]
  
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
    # 40's ACCOUNT_LEGEND_LABELS on BOTH scales, and no name on either: ggplot
    # merges the colour and linetype guides into one only when their titles AND
    # their labels agree, so a label set given to just one of them would split
    # the legend in two.
    scale_colour_manual(values = account_palette,
                        labels = ACCOUNT_LEGEND_LABELS, drop = FALSE) +
    scale_linetype_manual(values = account_linetype,
                          labels = ACCOUNT_LEGEND_LABELS, drop = FALSE) +
    scale_x_continuous(breaks = scales::breaks_width(4)) +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
    facet_wrap(~ panel, ncol = NCOL, scales = if (FREE_Y) "free_y" else "fixed") +
    labs(x = NULL, y = y_lab, colour = NULL, linetype = NULL,
         title = sprintf("%s responsibility over time - %s (%s VA base, %s allocation)",
                         META$short_label, biofuel_label[[bf]], VA_BASE, allocation),
         subtitle = paste0(sub, "\n", set_note,
                           if (FREE_Y) "\nY axis is FREE PER PANEL: compare trajectories across countries, not heights.")) +
    guides(colour = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1)) +
    theme_minimal(base_size = 11) +
    theme(legend.position  = "bottom",
          legend.title     = element_blank(),
          strip.text       = element_text(face = "bold", hjust = 0),
          panel.grid.minor = element_blank(),
          plot.subtitle    = element_text(size = 8.5, colour = "grey30"))
}

# --- run ---------------------------------------------------------------------
d         <- load_run()
chain_tot <- check_identity(d)
sets      <- panel_sets(d)

for (bf in intersect(names(biofuel_label), unique(d$biofuel_group))) {
  d_bf <- d[biofuel_group == bf]
  
  for (sn in names(sets)) {
    spec <- sets[[sn]]
    cty  <- spec$pick(d_bf)
    if (!length(cty)) { message("[47]   ", bf, " / ", sn, ": no countries selected"); next }
    
    # coverage is a property of the set IN THIS CHAIN: a pooled ranking can hold
    # most of biodiesel production and little of renewable diesel's
    cov <- d_bf[year %in% SEL_YEARS & account == "Production",
                .(v = sum(value)), by = .(sel = iso3c %in% cty)]
    message(sprintf("[47]   %-17s %-9s %d panels: %s  (%.0f%% of production)",
                    bf, sn, length(cty), paste(cty, collapse = " "),
                    100 * cov[sel == TRUE, sum(v)] / cov[, sum(v)]))
    
    n_row <- ceiling((length(cty) + as.integer(SHOW_ROW)) / NCOL)
    for (share in c(TRUE, FALSE)) {
      p <- plot_ts(d_bf, cty, chain_tot, bf, share, spec$note)
      if (is.null(p)) {
        message(sprintf("[47]   %s / %s: nothing to draw in the %s view -- skipped.",
                        bf, sn, if (share) "share" else "absolute"))
        next
      }
      save_svg(sprintf("responsibility_ts_%s_%s_%s_%s_%s_%s",
                       if (share) "share" else "abs", spec$tag, bf, STAG, VA_BASE, ATAG),
               p, width  = 1.2 + 3.2 * NCOL,
               height = 2.2 + 2.4 * n_row)
    }
  }
}

message(">>> [47] plots written to ", PLOT_DIR)