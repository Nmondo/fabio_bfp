# =============================================================================
# 43_plot_responsibility.R
# The CONTINENT-LEVEL view of the responsibility accounts, contrasted between an
# early (2012-2014) and a late (2020-2022) period. Nothing is recomputed -- 41 and
# 42 already conserve the same footprint, so this script aggregates, differences,
# averages over the years of a period, and plots.
#
# TWO FAMILIES OF FIGURE ------------------------------------------------------
# [1] THE ACCOUNTS THEMSELVES. Four alternative attributions of ONE identical
#     total -- production, consumption, HDI justice-based, value added (ex TLS)
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
#        VA_RESP_CSV (42)  the ex-TLS value-added account
#
# WRITES output/plot/
#   responsibility_accounts_<ind>_<vabase>_<alloc>.svg              continent x account
#   responsibility_accounts_<ind>_<vabase>_<alloc>_by_biofuel.svg   same, per chain
#   responsibility_divergence_<ind>_<vabase>_<alloc>.svg            absolute units
#   responsibility_divergence_<ind>_<vabase>_<alloc>_pct.svg        % of the 50/50 baseline
#   responsibility_divergence_<ind>_<vabase>_<alloc>_by_biofuel.svg per chain
# and the country-level table behind the divergence figures, via
#   va_csv("responsibility_divergence")
#
# THE BY-BIOFUEL FIGURES ARE FREE-SCALED --------------------------------------
# Both use facet_grid(biofuel_group ~ period): periods are the COLUMNS, chains the
# ROWS, and the value axis is freed PER ROW. Renewable diesel is an order of
# magnitude below biodiesel and would otherwise flatten out, so each chain gets
# its own scale while the two periods inside a row stay directly comparable.
# Price: bar heights/lengths MAY NOT be compared ACROSS rows -- read the axis, and
# use the pooled figures for cross-chain magnitude.
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
# scale across all chains, in both by-biofuel figures.
BY_BIOFUEL_FREE_Y <- TRUE

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
                va_resp              = "Value added (ex TLS)")
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
# The value-added column is the ex_tls variant throughout: it is the variant the
# account palette is labelled for, and 46 is the figure that justifies it.
load_countries <- function() {
  hdi <- fread(need(HDI_CSV,     "41"))
  va  <- fread(need(VA_RESP_CSV, "42"))
  
  need_cols(hdi, c("year", "biofuel_group", "iso3c", "continent", "production_based",
                   "consumption_based", "avg_prod_cons", "justice_based", "imbalance"),
            HDI_CSV)
  need_cols(va,  c("year", "va_variant", "biofuel_group", "va_iso3c", "va_resp",
                   "va_continent"), VA_RESP_CSV)
  
  # 42 resolves VA per (country, commodity); we want it per country
  vac <- va[va_variant == "ex_tls",
            .(va_resp = sum(va_resp)), by = .(year, biofuel_group, iso3c = va_iso3c,
                                              va_continent)]
  if (!nrow(vac)) stop("[43] ", basename(VA_RESP_CSV), " holds no 'ex_tls' rows.")
  
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
plot_accounts <- function(d, title, subtitle = NULL, by_biofuel = FALSE) {
  d <- copy(d)[, value := value / META$scale_factor]
  p <- ggplot(d, aes(x = continent, y = value, fill = account)) +
    geom_col(position = position_dodge2(preserve = "single"), width = 0.75) +
    scale_fill_manual(values = account_palette, drop = FALSE) +
    labs(x = NULL, y = META$y_label, fill = "Account",
         title = title, subtitle = subtitle) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title    = element_text(face = "bold"),
          strip.text      = element_text(face = "bold"),
          axis.text.x     = element_text(angle = 45, hjust = 1))
  
  if (!by_biofuel) return(p + facet_wrap(~ period, nrow = 1))
  
  # ggplot frees a scale per row/column, never per panel, so "free_y" on this
  # layout gives each chain its own y axis while both periods of a chain share
  # one. The columns are the same two periods in every row, so rows can still be
  # read against each other in SHAPE, just not in height.
  p + facet_grid(biofuel_group ~ period,
                 scales = if (BY_BIOFUEL_FREE_Y) "free_y" else "fixed",
                 switch = "y") +
    theme(strip.placement   = "outside",
          strip.text.y.left = element_text(face = "bold", angle = 90),
          panel.spacing.y   = unit(8, "pt"))
}

plot_divergence <- function(m, title, subtitle, x_lab, by_biofuel = FALSE) {
  p <- ggplot(m, aes(x = divergence, y = continent, fill = allocation)) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
    geom_col(position = position_dodge2(preserve = "single"), width = 0.75) +
    scale_fill_manual(values = alloc_palette, drop = FALSE) +
    labs(x = x_lab, y = NULL, fill = "Allocation",
         title = title, subtitle = subtitle) +
    theme_minimal() +
    theme(legend.position    = "bottom",
          legend.title       = element_text(face = "bold"),
          strip.text         = element_text(face = "bold"),
          panel.grid.major.y = element_blank())
  
  if (!by_biofuel) return(p + facet_wrap(~ period, nrow = 1))
  
  # the bars run horizontally here, so it is the x axis that is freed per row
  p + facet_grid(biofuel_group ~ period,
                 scales = if (BY_BIOFUEL_FREE_Y) "free_x" else "fixed",
                 switch = "y") +
    theme(strip.placement   = "outside",
          strip.text.y.left = element_text(face = "bold", angle = 90),
          panel.spacing.x   = unit(10, "pt"))
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

# --- [1] the four accounts, chains pooled ------------------------------------
acc_p <- period_mean(as_accounts(d, "continent"), "value", c("continent", "account"))
if (!nrow(acc_p)) stop("[43] no years of ", paste(names(PERIODS), collapse = " / "), " in the CSVs.")

save_svg(paste0("responsibility_accounts_", FTAG),
         plot_accounts(acc_p, TTL_ACC), width = 12, height = 6)

# --- [1b] the same bars, one row per biofuel chain ---------------------------
bf_acc <- period_mean(as_accounts(d, c("continent", "biofuel_group")), "value",
                      c("continent", "account", "biofuel_group"))
n_bf   <- uniqueN(bf_acc$biofuel_group)
n_per  <- uniqueN(bf_acc$period)
# the canvas follows the grid rather than staying at a fixed size
save_svg(paste0("responsibility_accounts_", FTAG, "_by_biofuel"),
         plot_accounts(bf_acc, paste0(TTL_ACC, " - by biofuel"),
                       subtitle = if (BY_BIOFUEL_FREE_Y)
                         "Y axis is FREE PER BIOFUEL ROW: compare continents and periods within a row, not bar heights across rows.",
                       by_biofuel = TRUE),
         width  = 3.0 + 4.2 * max(n_per, 1),    # 2 periods -> 11.4in
         height = 2.6 + 2.9 * max(n_bf, 1))     # 3 chains  -> 11.3in

# --- [2] divergence from the 50/50 split, absolute ---------------------------
cont <- period_mean(d, DIV_COLS, "continent")
m    <- order_by_baseline(as_divergences(cont, "continent"))
m[, divergence := divergence / META$scale_factor]

save_svg(paste0("responsibility_divergence_", FTAG),
         plot_divergence(m, TTL_DIV, SUB_DIV, x_lab = X_LAB), width = 11, height = 6)

# --- [2b] relative: % of each continent's own 50/50 baseline -----------------
# Scale-free, and therefore treacherous on a small baseline -- hence the cut.
pc <- copy(cont)
pc[, share := 100 * baseline_5050 / sum(baseline_5050), by = period]
drop <- sort(unique(pc[share < MIN_SHARE_PCT, as.character(continent)]))
if (length(drop))
  message("[43]   % figure drops (baseline < ", MIN_SHARE_PCT, "% of global): ",
          paste(drop, collapse = ", "))
pc <- pc[share >= MIN_SHARE_PCT & baseline_5050 > 0]
if (nrow(pc)) {
  mp <- order_by_baseline(as_divergences(pc, "continent"))
  mp[, divergence := 100 * divergence / baseline_5050]   # NOT scaled by META: it is a %
  save_svg(paste0("responsibility_divergence_", FTAG, "_pct"),
           plot_divergence(mp, paste0(TTL_DIV, " - relative"),
                           paste0(SUB_DIV, ". Regions below ", MIN_SHARE_PCT,
                                  "% of the global baseline are omitted: a % on a ",
                                  "negligible base is noise."),
                           x_lab = "Divergence from the 50/50 average (% of that region's baseline)"),
           width = 11, height = 6)
}

# --- [2c] divergence, one row per biofuel chain ------------------------------
bf_div <- period_mean(d, DIV_COLS, c("continent", "biofuel_group"))
if (nrow(bf_div)) {
  mb <- order_by_baseline(as_divergences(bf_div, c("continent", "biofuel_group")))
  mb[, divergence := divergence / META$scale_factor]
  save_svg(paste0("responsibility_divergence_", FTAG, "_by_biofuel"),
           plot_divergence(mb, paste0(TTL_DIV, " - by biofuel"),
                           if (BY_BIOFUEL_FREE_Y)
                             "X axis is FREE PER BIOFUEL ROW: compare regions within a row, not bar lengths across rows.",
                           x_lab = X_LAB, by_biofuel = TRUE),
           width  = 3.0 + 4.2 * max(uniqueN(mb$period), 1),
           height = 2.6 + 2.9 * max(uniqueN(mb$biofuel_group), 1))
}

# --- console: who moves, and do the two criteria agree? ----------------------
late <- copy(cont[period == names(PERIODS)[length(PERIODS)]])
if (nrow(late)) {
  late[, agree := fifelse(sign(divergence_hdi) == sign(divergence_va), "same", "OPPOSED")]
  cat(sprintf("\n-- %s | %s: divergence from the 50/50 split, %s --\n",
              META$short_label, allocation, names(PERIODS)[length(PERIODS)]))
  print(late[order(-abs(divergence_hdi)),
             .(continent, baseline_5050 = round(baseline_5050),
               hdi = round(divergence_hdi), va = round(divergence_va), agree)])
}

message(">>> [43] plots written to ", PLOT_DIR)