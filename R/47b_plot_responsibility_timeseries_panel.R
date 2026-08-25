# =============================================================================
# 47b_plot_responsibility_timeseries_panel.R
# ONE figure instead of one per chain: the four responsibility accounts over
# time on a FUEL x COUNTRY grid -- FOUR rows (the three chains pooled, then
# biogasoline, biodiesel, renewable diesel) by one column per country (44's `eu`
# set -- the 7 largest member states plus Belgium, eight columns -- in 44's
# left-to-right order).
#
# THE TOP ROW IS THE POOL, as in 43, 44 and 45: 40's with_total() adds a `total`
# chain to the long table before it is drawn, so the pooled series is a ROW of
# this grid rather than a figure of its own. Note the ORDER of operations in the
# run section -- check_identity() sees the chains only, and the pooled share
# denominator is DERIVED from theirs rather than recomputed. Both are load-
# bearing; the comments there say why.
#
# This is 47 with three things swapped out:
#   [1] the country set is INHERITED from 44's ORDER_CSV (as 45 does) instead of
#       being re-derived per chain, so the columns are the same countries in
#       every row -- which is the whole point of a grid.
#   [2] facet_grid(biofuel_group ~ iso3c, switch = "both") replaces
#       facet_wrap(~ panel), and the per-chain loop collapses into the row
#       dimension. `switch = "both"` is what puts the fuels on the LEFT and the
#       country codes on the BOTTOM, i.e. where 44's two figures and 45 put them.
#   [3] every per-chain quantity (the share denominator, the ribbon, the CJ
#       completion) gains a biofuel_group key, because the data frame handed to
#       ggplot now holds all three chains -- and their pool -- at once.
# Everything else -- load_run(), check_identity(), the palette, the ribbon
# semantics, share-vs-absolute -- is 47's, unchanged.
#
# Y IS FREE PER ROW, not per panel: facet_grid with scales = "free_y" frees the
# axis by row, so within a row the countries stay directly comparable while
# renewable diesel (an order of magnitude below biodiesel) still fills its row
# instead of collapsing to a flat line. With the pooled row in the grid this
# stops being a nicety -- Total is by construction the sum of the three rows
# beneath it. It carries 44's caveat, now with one more row to apply it to:
# LINE HEIGHTS MAY NOT BE COMPARED ACROSS ROWS, INCLUDING AGAINST TOTAL.
#
# NO "Rest of world" PANEL. It was a legitimate extra panel in a wrapped layout;
# in a grid it would be one more COLUMN sitting inside a row of EU member states,
# implying it belongs to the same set. The coverage each row's countries reach is
# reported to the console and named in the subtitle instead.
#
# READS  HDI_CSV (41), VA_RESP_CSV (42), ORDER_CSV (44)
# WRITES output/plot/
#   responsibility_ts_panel_share_<set>_<ind>_<vabase>_<alloc>.svg
#   responsibility_ts_panel_abs_<set>_<ind>_<vabase>_<alloc>.svg
#
# RUN: Rscript R/47b_plot_responsibility_timeseries_panel.R   (after 41, 42, 44)
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
SCRIPT <- "47b"
run_banner()

# --- figure switches ---------------------------------------------------------
# Which of 44's sets becomes the columns. "eu" is the seven ranked member states
# plus Belgium (44's must_include); "producers" is the five-country set and works
# unchanged (the grid just gets narrower). The width follows length(cty), so a
# set that grows or shrinks needs no change here.
COUNTRY_SET <- "eu"
# Fallback only, for a run where 44 has not been executed for this PLOT_YEAR --
# ORDER_CSV's name carries the year, so a 2019 order file is not found by a 2022
# run. Leave NULL to require 44.
MANUAL_COUNTRIES <- NULL
FREE_Y   <- TRUE     # free per ROW under facet_grid; FALSE = one scale for the whole grid

# --- type size and furniture -------------------------------------------------
# One number for the type, and the same value as 44/45/48/49. This grid, 44's
# figure 2 and 45 are meant to be read column-for-column, so type that disagreed
# between them would break that reading before anything else did. The panel
# geometry below is sized WITH it: eight country columns at 11pt fitted in 1.55in
# each, and holding that width while the type grew would just collide the year
# labels, so both dimensions carry the same 16/11 factor.
BASE_SIZE <- 16
PANEL_W  <- 2.25     # inches per country column
PANEL_H  <- 3.1      # inches per fuel row
X_BREAKS <- 5        # year label pitch; 4 collides at this column width

# --- long table: year x biofuel x country x account --------------------------
# Verbatim from 47.
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

# --- one total, four attributions --------------------------------------------
# Verbatim from 47: returns (year, biofuel_group, chain_total). It was ALREADY
# keyed by chain -- 47 simply filtered it down to one chain at plot time. Here
# the key is used as a key, which is the only change on this side.
check_identity <- function(d) {
  tot <- dcast(d[, .(total = sum(value)), by = .(year, biofuel_group, account)],
               year + biofuel_group ~ account, value.var = "total")
  miss <- setdiff(ACCOUNT_LEVELS, names(tot))
  if (length(miss))
    stop(sprintf(paste0("[47b] no rows at all for account(s): %s. The accounts (41) and value-added ",
                        "(42) CSVs must come from the SAME indicator, VA base and allocation, and 42 ",
                        "must have written the '%s' variant."),
                 paste(miss, collapse = ", "), VA_VARIANT))
  setnames(tot, ACCOUNT_LEVELS, c("pba", "cba", "jus", "va"))
  
  tot[, rel_core := {
    hi <- pmax(pba, cba, jus); lo <- pmin(pba, cba, jus)
    fifelse(abs(hi) > 0, (hi - lo) / abs(hi), 0)
  }]
  if (any(tot$rel_core > TOL)) {
    bad <- tot[rel_core > TOL][order(-rel_core)]
    warning(sprintf("[47b] production/consumption/justice do not share one total (worst %s %d, rel %.3g) -- are the accounts and VA files from the SAME indicator, VA base and allocation?",
                    bad$biofuel_group[1], bad$year[1], bad$rel_core[1]))
  }
  
  tot[, va_gap := fifelse(abs(pba) > 0, (va - pba) / abs(pba), 0)]
  gaps <- tot[abs(va_gap) > TOL][order(va_gap)]
  if (nrow(gaps)) {
    message(sprintf("[47b]   VA conservation gap in %d of %d chain-years; worst %s %d: %+.4f%% of the total.",
                    nrow(gaps), nrow(tot), gaps$biofuel_group[1], gaps$year[1],
                    100 * gaps$va_gap[1]))
    if (max(abs(gaps$va_gap)) > VA_GAP_TOL)
      warning(sprintf("[47b] the VA account is short by up to %.3f%% of the chain total -- large enough to distort the share view. Check 42's coverage CSV before using these panels.",
                      100 * max(abs(gaps$va_gap))))
  }
  
  tot[, .(year, biofuel_group, chain_total = pba)]
}

# --- the columns: 44's set, in 44's order ------------------------------------
# Replaces 47's select_countries(). The set must NOT be re-derived per chain:
# 47 picked a union of top-N per account per chain, which gives a different --
# and differently ordered -- panel set in every figure. A grid needs one set of
# columns shared by all three rows, and 44 already owns that statement (and 45
# already follows it), so it is read, not recomputed.
select_columns <- function(available) {
  if (!is.null(MANUAL_COUNTRIES)) {
    keep <- intersect(MANUAL_COUNTRIES, available)
    if (!length(keep)) stop("[47b] none of MANUAL_COUNTRIES appear in the accounts CSVs.")
    return(keep)
  }
  if (!file.exists(ORDER_CSV))
    stop("[47b] ", basename(ORDER_CSV), " not found -- run 44 for PLOT_YEAR = ", PLOT_YEAR,
         " first, or set MANUAL_COUNTRIES.")
  ord <- fread(ORDER_CSV)
  if (!all(c("position", "iso3c") %in% names(ord)))
    stop("[47b] ", basename(ORDER_CSV), " lacks 'position'/'iso3c' -- re-run 44.")
  if (!"country_set" %in% names(ord)) ord[, country_set := "all"]
  if (!COUNTRY_SET %in% ord$country_set)
    stop("[47b] set '", COUNTRY_SET, "' is not in ", basename(ORDER_CSV),
         " (has: ", paste(unique(ord$country_set), collapse = ", "), ").")
  
  o    <- ord[country_set == COUNTRY_SET][order(position)]
  miss <- setdiff(o$iso3c, available)
  if (length(miss))
    message("[47b] ", length(miss), " of 44's countries have no rows in the accounts CSVs, dropped: ",
            paste(miss, collapse = ", "))
  keep <- o$iso3c[o$iso3c %in% available]
  if (!length(keep))
    stop("[47b] none of 44's '", COUNTRY_SET, "' countries appear in ", basename(HDI_CSV), ".")
  message(sprintf("[47b] columns FOLLOW 44 (%s), set '%s': %s",
                  basename(ORDER_CSV), COUNTRY_SET, paste(keep, collapse = " ")))
  as.character(keep)
}

# --- the grid ----------------------------------------------------------------
# 47's plot_ts, with the chain promoted from a loop variable to a facet
# dimension. The three edits are marked [A] [B] [C].
plot_panel <- function(d_all, countries, chain_tot, share) {
  d <- d_all[iso3c %in% countries]
  
  # [A] the completion grid gains biofuel_group, or a missing chain-country-year
  #     is filled from the wrong cell and a structural zero reads as a gap.
  d <- d[, .(value = sum(value)), by = .(year, biofuel_group, iso3c, account)]
  d <- d[CJ(year          = sort(unique(d$year)),
            biofuel_group = unique(d$biofuel_group),
            iso3c         = countries,
            account       = factor(ACCOUNT_LEVELS, levels = ACCOUNT_LEVELS),
            unique = TRUE),
         on = .(year, biofuel_group, iso3c, account)]
  d[is.na(value), value := 0]
  
  if (share) {
    # [B] merge on year AND biofuel_group. In 47 the table was pre-filtered to one
    #     chain and merged on year alone; doing that here silently multiplies the
    #     rows (three chain_total rows per year) and normalises each chain by all
    #     three denominators.
    d <- merge(d, chain_tot, by = c("year", "biofuel_group"), all.x = TRUE)
    bad <- d[!is.finite(chain_total) | chain_total == 0,
             unique(.SD), .SDcols = c("biofuel_group", "year")]
    if (nrow(bad)) {
      message(sprintf("[47b]   chain total zero or absent in %d chain-year(s) -- dropped from the share view.",
                      nrow(bad)))
      d <- d[is.finite(chain_total) & chain_total != 0]
    }
    if (!nrow(d)) return(NULL)
    d[, value := 100 * value / chain_total]
    y_lab <- "Share of that row's global total (%)"
    sub   <- "Each year is normalised on that YEAR's and that ROW's global total -- the chain's for a chain row, all three pooled for the Total row -- so growth is divided out: what moves here is the ATTRIBUTION, not the volume."
  } else {
    d[, value := value / META$scale_factor]
    y_lab <- META$y_label
    sub   <- "Absolute. All four lines rise when a chain grows -- read the share figure to separate growth from re-attribution."
  }
  
  # rows in the pipeline's chain order, columns in 44's order
  d[, biofuel_group := factor(as.character(biofuel_group),
                              levels = intersect(names(biofuel_label),
                                                 as.character(unique(biofuel_group))))]
  d[, iso3c := factor(iso3c, levels = countries)]
  
  # [C] the ribbon is now per (chain, country), so both enter the dcast LHS.
  rib <- dcast(d[account %in% c("Production", "Consumption")],
               year + biofuel_group + iso3c ~ account, value.var = "value")
  setnames(rib, c("Production", "Consumption"), c("pba", "cba"))
  rib[, `:=`(lo = pmin(pba, cba), hi = pmax(pba, cba))]
  
  ggplot(d, aes(x = year, y = value)) +
    geom_ribbon(data = rib, aes(x = year, ymin = lo, ymax = hi),
                inherit.aes = FALSE, fill = "grey50", alpha = 0.16) +
    geom_line(aes(colour = account, linetype = account), linewidth = 0.6) +
    geom_point(aes(colour = account), size = 0.7) +
    # 40's ACCOUNT_LEGEND_LABELS on BOTH scales, and no name on either: ggplot
    # merges the colour and linetype guides into one only when their titles AND
    # their labels agree, so a label set given to just one of them would split
    # the legend in two.
    scale_colour_manual(values = account_palette,
                        labels = ACCOUNT_LEGEND_LABELS, drop = FALSE) +
    scale_linetype_manual(values = account_linetype,
                          labels = ACCOUNT_LEGEND_LABELS, drop = FALSE) +
    scale_x_continuous(breaks = scales::breaks_width(X_BREAKS),
                       guide  = guide_axis(check.overlap = TRUE)) +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
    # switch = "both" is 44's figure 2 exactly: fuel labels on the LEFT (rotated),
    # country codes on the BOTTOM. Without it facet_grid puts the columns on top
    # and the rows on the right, and the country codes would sit in a different
    # place here than in 44's two figures and in 45 -- three figures the reader is
    # meant to line up column-for-column. scales = "free_y" frees by ROW here.
    facet_grid(biofuel_group ~ iso3c,
               scales   = if (FREE_Y) "free_y" else "fixed",
               switch   = "both",
               labeller = labeller(biofuel_group = biofuel_label)) +
    labs(x = NULL, y = y_lab, colour = NULL, linetype = NULL,
         title = sprintf("%s responsibility over time: %s, by fuel (%s VA base, %s allocation)",
                         META$short_label, SET_TITLE, VA_BASE, allocation),
         subtitle = paste0(sub,
                           if (FREE_Y) "\nTop row is all three chains pooled. Y AXIS IS FREE PER ROW: compare countries within a row, never heights across rows -- not even against Total." else "")) +
    guides(colour = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1)) +
    theme_minimal(base_size = BASE_SIZE) +
    theme(legend.position   = "bottom",
          legend.title      = element_blank(),
          # The switched strips are their OWN theme elements: setting strip.text.x
          # / strip.text.y leaves the bottom and left strips at the inherited
          # defaults, so the settings appear to do nothing.
          strip.text.x.bottom = element_text(face = "bold"),
          strip.text.y.left   = element_text(face = "bold", angle = 90),
          # OUTSIDE the axes, not between them and the panels. With free_y every
          # row carries its own y labels, and the default "inside" wedges the fuel
          # name between those numbers and the panel -- the label ends up reading
          # as part of the axis. "outside" applies to both switched strips, so the
          # country codes also drop below the year labels, which is where a column
          # heading belongs.
          strip.placement     = "outside",
          panel.grid.minor  = element_blank(),
          panel.spacing.x   = unit(6, "pt"),
          panel.spacing.y   = unit(9, "pt"),
          # The year labels are the densest text on the canvas -- one per
          # X_BREAKS across a 2.25in column -- so they sit further below
          # BASE_SIZE than anything else in the theme.
          axis.text.x       = element_text(size = BASE_SIZE - 5),
          plot.subtitle     = element_text(size = BASE_SIZE - 4, colour = "grey30"))
}

# --- run ---------------------------------------------------------------------
d         <- load_run()
chain_tot <- check_identity(d)          # the three chains, unpooled -- see below

# --- the pooled row ----------------------------------------------------------
# check_identity() runs FIRST, on the chains alone. Handing it the pooled rows as
# well would re-report the same VA conservation gap a second time under a fourth
# key that is just the three chains added up, and a duplicated warning is how a
# real one stops being read.
#
# The pooled denominator is derived instead of recomputed: chain_total is the
# production-based total, which is additive over the disjoint chains, so summing
# it over the three IS what a second check_identity() pass would have returned.
# This matters more here than anywhere else in the block -- get it wrong and the
# share view normalises the Total row on one chain's total and the row silently
# reads as several hundred percent.
chain_tot <- rbind(chain_tot,
                   chain_tot[, .(biofuel_group = TOTAL_KEY, chain_total = sum(chain_total)),
                             by = year])
d <- with_total(d)

cty       <- select_columns(unique(d$iso3c))
n_bf      <- uniqueN(d[biofuel_group %in% names(biofuel_label)]$biofuel_group)
# NOT "largest EU member states": 44 forces Belgium into the `eu` set regardless
# of rank, so "the 8 largest" would be false. Same wording as 44's set title.
SET_TITLE <- sprintf("%d %s", length(cty),
                     if (COUNTRY_SET == "eu") "EU member states" else COUNTRY_SET)

d <- d[biofuel_group %in% names(biofuel_label)]

# Coverage PER ROW: a grid invites reading the columns as if they were the whole
# story, and for renewable diesel these countries are a much smaller slice of
# the world than they are for biodiesel. Report it per chain, as 44 does -- and
# for the pooled row too, which is the number a reader takes away if they read
# only the top of the figure.
for (bf in intersect(names(biofuel_label), as.character(unique(d$biofuel_group)))) {
  cov <- d[biofuel_group == bf & account == "Production",
           .(v = sum(value)), by = .(sel = iso3c %in% cty)]
  message(sprintf("[47b]   %-17s the %d columns carry %.0f%% of the world production-based total",
                  chain_labeller(bf), length(cty),
                  100 * cov[sel == TRUE, sum(v)] / cov[, sum(v)]))
}

for (share in c(TRUE, FALSE)) {
  p <- plot_panel(d, cty, chain_tot, share)
  if (is.null(p)) {
    message(sprintf("[47b]   nothing to draw in the %s view -- skipped.",
                    if (share) "share" else "absolute"))
    next
  }
  save_svg(sprintf("responsibility_ts_panel_%s_%s_%s_%s_%s",
                   if (share) "share" else "abs", COUNTRY_SET, STAG, VA_BASE, ATAG),
           p,
           width  = 1.5 + PANEL_W * length(cty),
           height = 2.4 + PANEL_H * n_bf)
}

message(">>> [47b] plots written to ", PLOT_DIR)