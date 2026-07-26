# =============================================================================
# 42_plot_responsibility.R
# Continent-level responsibility figures: four accounts side by side, contrasted
# between an early (2012-2014) and a late (2020-2022) period. Nothing is
# recomputed here -- the script aggregates, averages over the years of a period,
# and plots.
#
# READS  <IN_DIR>/ (one set per indicator x VA base x allocation, found by pattern)
#   FABIO_bcp_<ind>_hdi_responsibility_<alloc>.csv                         (40)
#   FABIO_bcp_<ind>_value_added_responsibility_<vabase>_<alloc>_ex_tls.csv (41)
#
# WRITES output/plot/
#   responsibility_accounts_<ind>_<vabase>_<alloc>.svg            continent x account
#   responsibility_accounts_<ind>_<vabase>_<alloc>_by_biofuel.svg same, per chain:
#       facet_grid(biofuel_group ~ period) -- PERIODS ARE THE COLUMNS, biofuel
#       chains the ROWS, so with scales = "free_y" (BY_BIOFUEL_FREE_Y, on by
#       default) each chain gets its own y axis while the two periods inside a
#       row stay on one scale and remain directly comparable. Renewable diesel is
#       an order of magnitude below biodiesel and would otherwise flatten out.
#       Price: bar heights MAY NOT be compared ACROSS rows -- read the axis, and
#       use the pooled figure above for cross-chain magnitude.
#   hdi_imbalance_<ind>_<alloc>.svg                               justice - 50/50 split
#
# CAVEAT: the four accounts (production, consumption, HDI justice-based, value
# added) are alternative attributions of ONE identical total, so within a panel
# each account's bars must sum to the same grand total. The script checks this and
# warns if they drift apart.
#
# RUN: Rscript R/42_plot_responsibility.R   (after 40 and 41)
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

source("R/19_plot_definitions.R")   # continent_palette, indicator_meta

model_version <- Sys.getenv("FABIO_RUN_MODE", unset = "rescaled")
model_version <- if (tolower(trimws(model_version)) == "bypass") "bypass" else "rescaled"
IN_DIR   <- if (model_version == "bypass") "output/bypass" else "output"
PLOT_DIR <- file.path("output", "plot")
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)
message(sprintf(">>> [42] model_version = '%s'  (reading responsibility CSVs from: %s)",
                model_version, IN_DIR))

# --- config ------------------------------------------------------------------
PERIODS <- list("2012-2014" = 2012:2014,
                "2020-2022" = 2020:2022)

ACCOUNT_LEVELS <- c("Production", "Consumption",
                    "HDI justice-based", "Value added (ex TLS)")

# by-biofuel figure: free the y axis per biofuel row (see the header). Set to
# FALSE for one common scale across all chains, i.e. the old behaviour.
BY_BIOFUEL_FREE_Y <- TRUE

# Okabe-Ito hues, as elsewhere in the pipeline. Fill is by ACCOUNT here, not by
# continent, so continent_palette does not apply to it.
account_palette <- c("Production"           = "#0072B2",   # blue
                     "Consumption"          = "#D55E00",   # vermillion
                     "HDI justice-based"    = "#009E73",   # green
                     "Value added (ex TLS)" = "#E69F00")   # orange

continent_pal <- c(continent_palette, Unknown = "#BBBBBB")

TOL <- 1e-6   # relative tolerance of the identical-total check

# --- indicator metadata ------------------------------------------------------
# file tags are lowercase ("lcim_eq_terrestrial", "ibif"); indicator_meta keys are
# "LCIM_EQ_terrestrial" / "ibif_total" -- match case-insensitively, minus "_total".
meta_for <- function(ind_tag) {
  key <- tolower(sub("_total$", "", indicator_meta$indicator))
  i   <- match(tolower(ind_tag), key)
  if (is.na(i)) {
    warning("[42] no indicator_meta entry for '", ind_tag,
            "'; falling back to the raw tag and scale_factor = 1.")
    return(list(scale_factor = 1, y_label = ind_tag, short_label = ind_tag))
  }
  as.list(indicator_meta[i, .(scale_factor, y_label, short_label)])
}

# --- discovery ---------------------------------------------------------------
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
    # the accounts take no VA base: keyed by indicator x allocation only
    hdi_file = file.path(dir, sprintf("FABIO_bcp_%s_hdi_responsibility_%s.csv",
                                      indicator, alloc)))]
  runs[]
}

need_cols <- function(dt, cols, path) {
  miss <- setdiff(cols, names(dt))
  if (length(miss))
    stop("[42] ", basename(path), " lacks column(s): ", paste(miss, collapse = ", "),
         ". Re-run 40/41 -- the metric-neutral column names are expected.")
  invisible(dt)
}

# --- load one (indicator, VA base, allocation) combination -------------------
# Returns the four accounts in one long table plus the HDI imbalance, both still
# annual and keyed by continent. Files of DIFFERENT indicator/allocation are never
# mixed: everything below comes from the one `run`.
load_run <- function(run) {
  va  <- fread(run$va_file)
  hdi <- fread(run$hdi_file)
  
  need_cols(va,  c("year", "va_variant", "biofuel_group", "va_iso3c", "va_resp", "va_continent"), run$va_file)
  need_cols(hdi, c("year", "biofuel_group", "iso3c", "continent", "production_based",
                   "consumption_based", "justice_based", "imbalance"), run$hdi_file)
  
  # the value-added bar is the ex_tls variant; the other three accounts come from
  # the accounts file, which resolves them per (year, biofuel_group, iso3c).
  long <- rbindlist(list(
    hdi[, .(year, biofuel_group, iso3c, continent,
            account = "Production",  value = production_based)],
    hdi[, .(year, biofuel_group, iso3c, continent,
            account = "Consumption", value = consumption_based)],
    hdi[, .(year, biofuel_group, iso3c, continent,
            account = "HDI justice-based", value = justice_based)],
    va[va_variant == "ex_tls",
       .(year, biofuel_group, iso3c = va_iso3c, continent = va_continent,
         account = "Value added (ex TLS)", value = va_resp)]
  ), use.names = TRUE)
  
  # a handful of iso3c carry no continent in the VA table's lookup; fill from the
  # accounts, and bucket whatever is still unresolved so the totals stay complete.
  long[continent == "", continent := NA_character_]
  cmap <- unique(hdi[continent != "", .(iso3c, continent)])
  long[is.na(continent), continent := cmap$continent[match(iso3c, cmap$iso3c)]]
  if (anyNA(long$continent)) {
    message("[42]   no continent for: ",
            paste(sort(unique(long[is.na(continent), iso3c])), collapse = ", "),
            " -- shown as 'Unknown'")
    long[is.na(continent), continent := "Unknown"]
  }
  
  long[, account := factor(account, levels = ACCOUNT_LEVELS)]
  imb <- hdi[, .(imbalance = sum(imbalance)), by = .(year, continent)]
  
  list(accounts  = long[, .(value = sum(value)), by = .(year, biofuel_group, continent, account)],
       imbalance = imb)
}

# --- mean over the years of each period (not the sum) ------------------------
period_mean <- function(dt, value_col, keys) {
  out <- rbindlist(lapply(names(PERIODS), function(p) {
    yrs <- PERIODS[[p]]
    d   <- dt[year %in% yrs]
    if (!nrow(d)) return(NULL)
    n_yr <- uniqueN(d$year)                       # years actually present in the run
    if (n_yr < length(yrs))
      message(sprintf("[42]   period %s: only %d of %d years present", p, n_yr, length(yrs)))
    d[, .(value = sum(get(value_col)) / n_yr), by = keys][, period := p][]
  }))
  if (nrow(out)) out[, period := factor(period, levels = names(PERIODS))]
  out
}

# --- the four accounts are one total, re-attributed --------------------------
check_totals <- function(d, what) {
  tot <- d[, .(total = sum(value)), by = .(period, account)]
  spread <- tot[, .(rel = if (max(abs(total)) > 0)
    (max(total) - min(total)) / max(abs(total)) else 0), by = period]
  if (any(spread$rel > TOL))
    warning(sprintf("[42] %s: the four accounts do not share one total (max relative spread %.3g)",
                    what, max(spread$rel)))
  invisible(tot)
}

# --- plots -------------------------------------------------------------------
plot_accounts <- function(d, meta, title, subtitle = NULL, by_biofuel = FALSE) {
  d <- copy(d)[, value := value / meta$scale_factor]
  p <- ggplot(d, aes(x = continent, y = value, fill = account)) +
    geom_col(position = position_dodge2(preserve = "single"), width = 0.75) +
    scale_fill_manual(values = account_palette, drop = FALSE) +
    labs(x = NULL, y = meta$y_label, fill = "Account",
         title = title, subtitle = subtitle) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title    = element_text(face = "bold"),
          strip.text      = element_text(face = "bold"),
          axis.text.x     = element_text(angle = 45, hjust = 1))
  
  if (!by_biofuel) return(p + facet_wrap(~ period, nrow = 1))
  
  # facet_grid(biofuel_group ~ period): the periods are the COLUMNS and the
  # biofuel chains the ROWS. ggplot frees a scale per row/column, never per
  # panel, so "free_y" on this layout gives each chain its own y axis while both
  # periods of a chain share one -- exactly the comparison we want (early vs late
  # within a chain), at the cost of cross-row heights (see the header caveat).
  # The columns are the same two periods in every row, so rows can still be read
  # against each other in SHAPE, just not in height.
  p + facet_grid(biofuel_group ~ period,
                 scales = if (BY_BIOFUEL_FREE_Y) "free_y" else "fixed",
                 switch = "y") +
    theme(strip.placement   = "outside",
          strip.text.y.left = element_text(face = "bold", angle = 90),
          panel.spacing.y   = unit(8, "pt"))
}

plot_imbalance <- function(d, meta, title) {
  d   <- copy(d)[, value := value / meta$scale_factor]
  ord <- d[, .(m = mean(value)), by = continent][order(m), continent]
  d[, continent := factor(continent, levels = ord)]
  ggplot(d, aes(x = value, y = continent, fill = continent)) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
    geom_col(width = 0.7) +
    # fill only restates the y axis, so the legend is dropped rather than doubled
    scale_fill_manual(values = continent_pal, guide = "none") +
    facet_wrap(~ period, nrow = 1) +
    labs(x = paste0(meta$y_label, "  (justice-based - 50/50 average)"),
         y = NULL, title = title,
         subtitle = "positive: the region shoulders more than an even producer/consumer split") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title    = element_text(face = "bold"),
          strip.text      = element_text(face = "bold"))
}

# --- run ---------------------------------------------------------------------
runs <- discover_runs()

if (!nrow(runs)) {
  message(">>> [42] no `*_value_added_responsibility_*_ex_tls.csv` in ", IN_DIR,
          " -- nothing to plot.")
} else {
  message(sprintf(">>> [42] %d run(s) discovered in %s", nrow(runs), IN_DIR))
  done_imbalance <- character()   # the HDI imbalance does not depend on the VA base
  
  for (i in seq_len(nrow(runs))) {
    run <- runs[i]
    key <- sprintf("%s / %s / %s", run$indicator, run$va_base, run$alloc)
    
    if (!file.exists(run$va_file))  { message("[42] skip ", key, ": no ex_tls value-added file"); next }
    if (!file.exists(run$hdi_file)) { message("[42] skip ", key, ": no HDI responsibility file"); next }
    
    message(">>> [42] ", key)
    meta <- meta_for(run$indicator)
    d    <- load_run(run)
    
    # main figure: biofuel groups pooled
    acc_p <- period_mean(d$accounts, "value", c("continent", "account"))
    if (!nrow(acc_p)) { message("[42] skip ", key, ": no years in the plotted periods"); next }
    check_totals(acc_p, key)
    
    ttl <- sprintf("%s responsibility by continent - %s VA base, %s allocation",
                   meta$short_label, run$va_base, run$alloc)
    ggsave(file.path(PLOT_DIR, sprintf("responsibility_accounts_%s_%s_%s.svg",
                                       run$indicator, run$va_base, run$alloc)),
           plot_accounts(acc_p, meta, ttl),
           device = svglite::svglite, width = 12, height = 6)
    
    # variant: same bars, split by biofuel chain (rows) x period (columns)
    bf_p <- period_mean(d$accounts, "value", c("continent", "account", "biofuel_group"))
    n_bf  <- uniqueN(bf_p$biofuel_group)
    n_per <- uniqueN(bf_p$period)
    bf_sub <- if (BY_BIOFUEL_FREE_Y)
      "Y axis is FREE PER BIOFUEL ROW: compare continents and periods within a row, not bar heights across rows."
    else NULL
    # the grid is now n_per columns wide and n_bf rows tall, so the canvas has to
    # follow the data rather than stay at the old fixed 13 x 9.
    ggsave(file.path(PLOT_DIR, sprintf("responsibility_accounts_%s_%s_%s_by_biofuel.svg",
                                       run$indicator, run$va_base, run$alloc)),
           plot_accounts(bf_p, meta, paste0(ttl, " - by biofuel"),
                         subtitle = bf_sub, by_biofuel = TRUE),
           device = svglite::svglite,
           width  = 3.0 + 4.2 * max(n_per, 1),    # 2 periods -> 11.4in
           height = 2.6 + 2.9 * max(n_bf, 1))     # 3 chains   -> 11.3in
    
    # HDI imbalance (indicator x allocation only -- write it once)
    imb_key <- paste(run$indicator, run$alloc)
    if (!imb_key %in% done_imbalance) {
      imb_p <- period_mean(d$imbalance, "imbalance", "continent")
      if (nrow(imb_p)) {
        ggsave(file.path(PLOT_DIR, sprintf("hdi_imbalance_%s_%s.svg", run$indicator, run$alloc)),
               plot_imbalance(imb_p, meta,
                              sprintf("%s: HDI justice imbalance, %s allocation",
                                      meta$short_label, run$alloc)),
               device = svglite::svglite, width = 10, height = 6)
        done_imbalance <- c(done_imbalance, imb_key)
      }
    }
  }
  message(">>> [42] plots written to ", PLOT_DIR)
}