# =============================================================================
# 43_plot_responsibility.R
# The CONTINENT-LEVEL view of the responsibility accounts, contrasted between an
# early (2012-2014) and a late (2020-2022) period. Nothing is recomputed -- 41 and
# 42 already conserve the same footprint, so this script aggregates, differences,
# averages over the years of a period, and plots.
#
# TWO FAMILIES OF FIGURE ------------------------------------------------------
# [1] THE ACCOUNTS THEMSELVES. Four alternative attributions of ONE identical
#     total -- production, consumption, HDI justice-based, and the value-added
#     account for whichever VA variant 40's VA_VARIANT names
#     -- side by side per continent. Within a panel the four must sum to the same
#     grand total; a spread between them is a broken input, not a finding.
#
# [2] HOW FAR EACH NORMATIVE ALLOCATION PULLS A CONTINENT off the naive 50/50
#     producer/consumer split. Per country i, chain g and year y:
#         baseline_i = 0.5 * (production_based_i + consumption_based_i)  ("50/50")
#         div_hdi_i  = justice_based_i - baseline_i     (= 41's `imbalance`)
#         div_va_i   = va_resp_i       - baseline_i     (42, summed over commodities)
#     Both sum to ~0 over all countries: a divergence is a REDISTRIBUTION between
#     countries, never a change of the global total, so a continent's bar is read
#     against the other bars in the same panel, not on its own.
#       positive: the allocation loads MORE onto the region than an even split
#       negative: the allocation lets the region off, relative to an even split
#     The two criteria answer different questions and are NOT expected to agree:
#       HDI (41)  splits every bilateral flow by the two agents' relative HDI --
#                 it moves burden toward the DEVELOPED end of each trade pair.
#       VA  (42)  re-allocates each chain's footprint to whoever CAPTURES THE
#                 VALUE along it -- toward processors, traders and brand owners,
#                 wherever they sit.
#     Where they point the same way the two criteria reinforce; where they oppose,
#     the choice of criterion decides who carries the impact.
#
# Family [1] is the level, family [2] is the movement, and both are built from ONE
# country-level table assembled once from the two CSVs.
#
# READS  HDI_CSV     (41)  production / consumption / justice, and `imbalance`
#        VA_RESP_CSV (42)  the value-added account, in 40's VA_VARIANT
#
# WRITES output/plot/
#   responsibility_accounts_<ind>_<vabase>_<alloc>.svg     continent x account
#   responsibility_divergence_<ind>_<vabase>_<alloc>.svg   absolute units
#   responsibility_divergence_<ind>_<vabase>_<alloc>_pct.svg  % of the 50/50 baseline
# and the country-level table behind the divergence figures, via
#   va_csv("responsibility_divergence")
#
# ONE GRID PER FIGURE, TOTAL ON TOP -------------------------------------------
# There used to be two files per family: the chains pooled, and a `_by_biofuel`
# companion. They are now ONE figure of four rows -- Total, then the three chains
# -- built by handing the country table to 40's with_total() before it is
# aggregated. The Total row is the same number the pooled file used to carry:
# with_total() only duplicates rows under a fourth key, and the sum that follows
# does the pooling, so nothing is recomputed and the two cannot drift.
#
# EVERY FIGURE IS THEREFORE FREE-SCALED PER ROW -------------------------------
# facet_grid(biofuel_group ~ period): periods are the COLUMNS, chains the ROWS,
# and the value axis is freed PER ROW. This was already necessary between chains
# -- renewable diesel is an order of magnitude below biodiesel -- and the Total
# row makes it unavoidable, since it is by construction the sum of the three and
# on one common scale would flatten all of them. Price: bar heights/lengths MAY
# NOT be compared ACROSS rows, INCLUDING against Total. Read the axis. Within a
# row the two periods share a scale and are directly comparable, which is the
# comparison every one of these figures is actually for.
#
# THE % FIGURE IS THE DANGEROUS ONE -------------------------------------------
# divergence / baseline explodes wherever the baseline is small: ROW's VA
# divergence reaches ~1000% in the late period on a baseline three orders of
# magnitude below NAM's. Continents below MIN_SHARE_PCT of the global baseline are
# therefore dropped from the % figure (never from the absolute one, which shows
# them at their true, negligible size). Read the absolute figure first.
#
# RUN: Rscript R/43_plot_responsibility.R   (after 41 and 42)
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
SCRIPT <- "43"
run_banner()

# --- figure switches ---------------------------------------------------------
# Free the value axis per biofuel row (see the header). FALSE gives one common
# scale across the whole grid, which now includes the Total row -- keep it TRUE
# unless you specifically want to SEE how small the chains are against the pool.
BY_BIOFUEL_FREE_Y <- TRUE
if (!BY_BIOFUEL_FREE_Y)
  message("[43] BY_BIOFUEL_FREE_Y = FALSE: the Total row shares its scale with the ",
          "chains and will flatten them. This is a deliberate setting, not a default.")

# % figure only: drop continents whose 50/50 baseline is below this share of the
# global baseline in a period -- see the header. Set to 0 to keep everything.
MIN_SHARE_PCT <- 0.5

ALLOC_LEVELS <- names(alloc_palette)

# --- column -> display label, BY NAME ----------------------------------------
# ACCOUNT_LEVELS and alloc_palette live in 40 next to the colours, where they read
# as display constants; reordering either to change a legend used to silently
# relabel the accounts, because the melts below mapped melt()'s factor CODES onto
# them by position. Map by column name instead, and fail loudly if the labels and
# the palettes ever drift apart.
ACCOUNT_OF <- c(production_based     = "Production",
                consumption_based    = "Consumption",
                justice_based        = "HDI justice-based",
                va_resp              = VA_ACCOUNT_LABEL)
DIVERGENCE_OF <- c(divergence_hdi = "HDI (justice-based)",
                   divergence_va  = "Value added")

if (!setequal(ACCOUNT_OF, ACCOUNT_LEVELS))
  stop("[43] ACCOUNT_OF and 40's ACCOUNT_LEVELS disagree: ",
       paste(sort(union(setdiff(ACCOUNT_OF, ACCOUNT_LEVELS),
                        setdiff(ACCOUNT_LEVELS, ACCOUNT_OF))), collapse = ", "))
if (!setequal(DIVERGENCE_OF, ALLOC_LEVELS))
  stop("[43] DIVERGENCE_OF and 40's alloc_palette disagree: ",
       paste(sort(union(setdiff(DIVERGENCE_OF, ALLOC_LEVELS),
                        setdiff(ALLOC_LEVELS, DIVERGENCE_OF))), collapse = ", "))

# --- the one country-level table both families are built from ----------------
# The value-added column is 40's VA_VARIANT throughout -- the same variant the
# account label and palette are built for -- and 46 is the figure that contrasts
# the two definitions and justifies the choice.
load_countries <- function() {
  hdi <- fread(need(HDI_CSV,     "41"))
  va  <- fread(need(VA_RESP_CSV, "42"))
  
  need_cols(hdi, c("year", "biofuel_group", "iso3c", "continent", "production_based",
                   "consumption_based", "avg_prod_cons", "justice_based", "imbalance"),
            HDI_CSV)
  need_cols(va,  c("year", "va_variant", "biofuel_group", "va_iso3c", "va_resp",
                   "va_continent"), VA_RESP_CSV)
  
  # 42 resolves VA per (country, commodity); we want it per country
  vac <- va[va_variant == VA_VARIANT,
            .(va_resp = sum(va_resp)), by = .(year, biofuel_group, iso3c = va_iso3c,
                                              va_continent)]
  if (!nrow(vac)) stop("[43] ", basename(VA_RESP_CSV), " holds no '", VA_VARIANT,
                       "' rows.")
  
  # FULL OUTER join. A country can produce or consume and still capture no value
  # added in these chains (ANT, BRN, DMA, ERI, PRI, SSD in the shipped run): it
  # must enter with va_resp = 0 and a divergence of -baseline, not vanish. The
  # reverse -- VA without any production or consumption -- cannot happen by
  # construction, but is zero-filled symmetrically rather than assumed away.
  d <- merge(hdi[, .(year, biofuel_group, iso3c, continent, production_based,
                     consumption_based, baseline_5050 = avg_prod_cons,
                     justice_based, imbalance)],
             vac, by = c("year", "biofuel_group", "iso3c"), all = TRUE)
  num <- c("production_based", "consumption_based", "baseline_5050",
           "justice_based", "imbalance", "va_resp")
  for (j in num) set(d, which(is.na(d[[j]])), j, 0)
  
  # continent: the accounts lookup, then the VA table's, then a visible bucket --
  # resolved ONCE here, so both figure families place a country the same way
  d[continent == "" | is.na(continent), continent := va_continent]
  d[, va_continent := NULL]
  if (anyNA(d$continent) || any(d$continent == "")) {
    message("[43]   no continent for: ",
            paste(sort(unique(d[is.na(continent) | continent == "", iso3c])), collapse = ", "),
            " -- shown as 'Unknown'")
    d[is.na(continent) | continent == "", continent := "Unknown"]
  }
  
  d[, `:=`(divergence_hdi = justice_based - baseline_5050,
           divergence_va  = va_resp       - baseline_5050)]
  
  # --- guards -----------------------------------------------------------------
  # (a) 41's imbalance must be exactly the HDI divergence we just recomputed
  sref <- max(abs(d$imbalance), 0)
  if (sref > 0 && max(abs(d$divergence_hdi - d$imbalance)) / sref > DIV_TOL)
    warning("[43] recomputed HDI divergence != 41's `imbalance` column -- check the inputs.")
  
  # (b) the four accounts must re-attribute ONE total per (year, chain). This is
  #     the identity BOTH families rest on: if it fails, the accounts figure is
  #     comparing unlike things and the divergences are not on a common footing.
  #     Checked at source, before any aggregation -- a continent sum or a period
  #     mean cannot break an identity that already holds per year and chain.
  tot <- d[, .(prod = sum(production_based), cons = sum(consumption_based),
               just = sum(justice_based),    va = sum(va_resp)),
           by = .(year, biofuel_group)]
  tot[, rel := (pmax(prod, cons, just, va) - pmin(prod, cons, just, va)) / pmax(abs(prod), 1e-12)]
  if (any(tot$rel > DIV_TOL))
    warning(sprintf(paste0("[43] production / consumption / HDI / VA do not share one total ",
                           "(max relative spread %.3g) -- the four accounts are not four views ",
                           "of one number, and the divergences are not comparable."),
                    max(tot$rel)))
  
  # (c) a redistribution sums to zero across countries
  z <- d[, .(h = sum(divergence_hdi), v = sum(divergence_va), base = sum(baseline_5050)),
         by = .(year, biofuel_group)]
  if (any(pmax(abs(z$h), abs(z$v)) / pmax(z$base, 1e-12) > DIV_TOL))
    warning("[43] divergences do not sum to zero across countries -- 41/42 are not conserving.")
  
  # `imbalance` has served its purpose in guard (a) and is by definition
  # divergence_hdi, so it does not travel into the exported table.
  d[, imbalance := NULL]
  setcolorder(d, c("year", "biofuel_group", "iso3c", "continent",
                   "production_based", "consumption_based", "baseline_5050",
                   "justice_based", "va_resp", "divergence_hdi", "divergence_va"))
  d[]
}

# the four accounts as one long column, ready to facet. Relabelled by COLUMN NAME
# through ACCOUNT_OF, so neither the measure.vars order nor ACCOUNT_LEVELS' order
# can silently rename an account.
as_accounts <- function(d, keys) {
  long <- melt(d, id.vars = c("year", keys),
               measure.vars = names(ACCOUNT_OF),
               variable.name = "account", value.name = "value")
  long[, account := factor(ACCOUNT_OF[as.character(account)], levels = ACCOUNT_LEVELS)]
  long[, .(value = sum(value)), by = c("year", keys, "account")]
}

# the two divergences as one long column, ready to dodge
as_divergences <- function(d, keys) {
  m <- melt(d, id.vars = c(keys, "period", "baseline_5050"),
            measure.vars = names(DIVERGENCE_OF),
            variable.name = "allocation", value.name = "divergence")
  m[, allocation := factor(DIVERGENCE_OF[as.character(allocation)], levels = ALLOC_LEVELS)]
  m[]
}

# continents ordered by the size of what they are actually involved in, so a big
# bar on a small base cannot be mistaken for a big shift (that is the % figure's
# job, and it drops those continents outright).
order_by_baseline <- function(m) {
  ord <- m[, .(b = sum(baseline_5050)), by = continent][order(b), continent]
  m[, continent := factor(continent, levels = ord)][]
}

# --- plots -------------------------------------------------------------------
# ONE layout, always faceted by chain: the `by_biofuel` argument is gone because
# there is no longer a pooled figure to switch to -- the pool is the top ROW.
plot_accounts <- function(d, title, subtitle = NULL) {
  d <- copy(d)[, value := value / META$scale_factor]
  ggplot(d, aes(x = continent, y = value, fill = account)) +
    geom_col(position = position_dodge2(preserve = "single"), width = 0.75) +
    scale_fill_manual(values = account_palette, drop = FALSE) +
    labs(x = NULL, y = META$y_label, fill = "Account",
         title = title, subtitle = subtitle) +
    # ggplot frees a scale per row/column, never per panel, so "free_y" on this
    # layout gives each chain -- and the Total row -- its own y axis while both
    # periods inside a row share one. The columns are the same two periods in
    # every row, so rows can still be read against each other in SHAPE, just not
    # in height.
    facet_grid(biofuel_group ~ period,
               scales   = if (BY_BIOFUEL_FREE_Y) "free_y" else "fixed",
               switch   = "y",
               labeller = labeller(biofuel_group = chain_labeller)) +
    theme_minimal() +
    theme(legend.position   = "bottom",
          legend.title      = element_text(face = "bold"),
          strip.text        = element_text(face = "bold"),
          axis.text.x       = element_text(angle = 45, hjust = 1),
          strip.placement   = "outside",
          strip.text.y.left = element_text(face = "bold", angle = 90),
          panel.spacing.y   = unit(8, "pt"))
}

# THE VALUE IS MAPPED TO y AND THE PANEL IS FLIPPED, NOT MAPPED TO x ----------
# This looks like a detour and is not. facet_grid frees a scale PER ROW for y and
# PER COLUMN for x -- always, regardless of which way the bars point. The earlier
# version mapped the divergence to x and asked for `free_x` under
# biofuel_group ~ period, which freed it per PERIOD COLUMN: every chain in a
# column shared one range, the exact opposite of what its own comment and
# subtitle claimed. It was survivable while the rows were three chains of broadly
# similar size. It is not survivable now that the top row is their sum, which
# would set the column range and squash all three chains into slivers.
# So: value on y, categories on x, coord_flip() for the horizontal bars, and
# `free_y` genuinely frees the value axis per row.
plot_divergence <- function(m, title, subtitle, x_lab) {
  ggplot(m, aes(x = continent, y = divergence, fill = allocation)) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
    geom_col(position = position_dodge2(preserve = "single"), width = 0.75) +
    coord_flip() +
    scale_fill_manual(values = alloc_palette, drop = FALSE) +
    labs(x = NULL, y = x_lab, fill = "Allocation",
         title = title, subtitle = subtitle) +
    facet_grid(biofuel_group ~ period,
               scales   = if (BY_BIOFUEL_FREE_Y) "free_y" else "fixed",
               switch   = "y",
               labeller = labeller(biofuel_group = chain_labeller)) +
    theme_minimal() +
    theme(legend.position    = "bottom",
          legend.title       = element_text(face = "bold"),
          strip.text         = element_text(face = "bold"),
          # under coord_flip the theme grid elements follow the RENDERED axes, so
          # .y is still the horizontal ruling behind the continent categories --
          # the value gridlines survive, which is the point.
          panel.grid.major.y = element_blank(),
          strip.placement    = "outside",
          strip.text.y.left  = element_text(face = "bold", angle = 90),
          panel.spacing.x    = unit(10, "pt"))
}

# --- run ---------------------------------------------------------------------
d <- load_countries()

fwrite(d[order(year, biofuel_group, -divergence_hdi)], va_csv("responsibility_divergence"))
message(">>> [43] wrote ", va_csv("responsibility_divergence"))

FTAG    <- sprintf("%s_%s_%s", STAG, VA_BASE, ATAG)
TTL_ACC <- sprintf("%s responsibility by continent - %s VA base, %s allocation",
                   META$short_label, VA_BASE, allocation)
TTL_DIV <- sprintf("%s: divergence from the 50/50 producer/consumer split - %s VA base, %s allocation",
                   META$short_label, VA_BASE, allocation)
SUB_DIV <- paste0("positive: the allocation loads more onto the region than an even ",
                  "producer/consumer split; both allocations sum to zero across all regions")
X_LAB   <- paste0(META$y_label, "  (allocation - 50/50 average)")
DIV_COLS <- c("baseline_5050", "divergence_hdi", "divergence_va")
SUB_ROWS <- if (BY_BIOFUEL_FREE_Y)
  paste0("Top row is all three chains pooled. The value axis is FREE PER ROW: ",
         "compare continents and periods WITHIN a row, never bar sizes across ",
         "rows -- not even against Total.") else NULL

# THE PLOTTING TABLE ----------------------------------------------------------
# `dp` is `d` plus a copy of every row under the `total` chain. Every aggregation
# below is a sum by keys that INCLUDE biofuel_group, so the duplicate rows pool
# themselves and the Total panel is arithmetically identical to the pooled figure
# this used to write as a separate file. `d` itself stays unpooled, for the CSV
# above and the console table below.
dp <- with_total(d)

# --- [1] the four accounts: Total, then one row per chain --------------------
acc <- period_mean(as_accounts(dp, c("continent", "biofuel_group")), "value",
                   c("continent", "account", "biofuel_group"))
if (!nrow(acc)) stop("[43] no years of ", paste(names(PERIODS), collapse = " / "), " in the CSVs.")

n_row <- uniqueN(acc$biofuel_group)   # 4: Total + the three chains
n_col <- uniqueN(acc$period)
# the canvas follows the grid rather than staying at a fixed size
save_svg(paste0("responsibility_accounts_", FTAG),
         plot_accounts(acc, TTL_ACC, subtitle = SUB_ROWS),
         width  = 3.0 + 4.2 * max(n_col, 1),    # 2 periods -> 11.4in
         height = 2.6 + 2.9 * max(n_row, 1))    # 4 rows    -> 14.2in

# --- [2] divergence from the 50/50 split, absolute ---------------------------
# order_by_baseline() sees the total rows as well as the chains, which exactly
# doubles every continent's baseline sum and therefore leaves the ORDER -- the
# only thing it uses -- untouched.
cont <- period_mean(dp, DIV_COLS, c("continent", "biofuel_group"))
m    <- order_by_baseline(as_divergences(cont, c("continent", "biofuel_group")))
m[, divergence := divergence / META$scale_factor]

save_svg(paste0("responsibility_divergence_", FTAG),
         plot_divergence(m, TTL_DIV, paste(SUB_DIV, SUB_ROWS), x_lab = X_LAB),
         width  = 3.0 + 4.2 * max(uniqueN(m$period), 1),
         height = 2.6 + 2.9 * max(uniqueN(m$biofuel_group), 1))

# --- [2b] relative: % of each continent's own 50/50 baseline -----------------
# Scale-free, and therefore treacherous on a small baseline -- hence the cut.
#
# THE DENOMINATOR IS PER ROW. `by = .(period, biofuel_group)` and not `by =
# period`: with the total rows in the table, a global-per-period denominator
# would be the pool PLUS the three chains, i.e. twice the world, and every share
# would read half its true value -- silently, and just far enough below
# MIN_SHARE_PCT to drop continents that belong on the figure.
pc <- copy(cont)
pc[, share := 100 * baseline_5050 / sum(baseline_5050), by = .(period, biofuel_group)]

# The cut is now made PANEL BY PANEL, so a continent can be material in the pool
# and negligible in renewable diesel. It keeps its slot on the shared axis and
# simply has no bar in the panels where it was cut -- `continent` is a factor
# whose levels are fixed by order_by_baseline(), so dropping rows leaves a gap
# rather than re-indexing the axis. A continent cut EVERYWHERE disappears.
drop <- unique(pc[share < MIN_SHARE_PCT,
                  .(biofuel_group, continent = as.character(continent))])
if (nrow(drop))
  for (g in unique(drop$biofuel_group))
    message("[43]   % figure, ", chain_labeller(g), ": no bar for (baseline < ",
            MIN_SHARE_PCT, "% of that panel's total) ",
            paste(sort(drop[biofuel_group == g, continent]), collapse = ", "))
pc <- pc[share >= MIN_SHARE_PCT & baseline_5050 > 0]
if (nrow(pc)) {
  mp <- order_by_baseline(as_divergences(pc, c("continent", "biofuel_group")))
  mp[, divergence := 100 * divergence / baseline_5050]   # NOT scaled by META: it is a %
  save_svg(paste0("responsibility_divergence_", FTAG, "_pct"),
           plot_divergence(mp, paste0(TTL_DIV, " - relative"),
                           paste0(SUB_DIV, ". Within each panel, regions below ",
                                  MIN_SHARE_PCT, "% of that panel's baseline are ",
                                  "omitted: a % on a negligible base is noise."),
                           x_lab = "Divergence from the 50/50 average (% of that region's baseline)"),
           width  = 3.0 + 4.2 * max(uniqueN(mp$period), 1),
           height = 2.6 + 2.9 * max(uniqueN(mp$biofuel_group), 1))
}

# --- console: who moves, and do the two criteria agree? ----------------------
# The pooled row only: the per-chain numbers are on the figure, and four times
# the rows in the terminal is not four times the information.
late <- copy(cont[period == names(PERIODS)[length(PERIODS)] & biofuel_group == TOTAL_KEY])
if (nrow(late)) {
  late[, agree := fifelse(sign(divergence_hdi) == sign(divergence_va), "same", "OPPOSED")]
  cat(sprintf("\n-- %s | %s: divergence from the 50/50 split, %s --\n",
              META$short_label, allocation, names(PERIODS)[length(PERIODS)]))
  print(late[order(-abs(divergence_hdi)),
             .(continent, baseline_5050 = round(baseline_5050),
               hdi = round(divergence_hdi), va = round(divergence_va), agree)])
}

message(">>> [43] plots written to ", PLOT_DIR)