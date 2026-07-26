# =============================================================================
# 47_plot_responsibility_divergence.R
# How far do the TWO responsibility allocations pull a continent away from the
# naive 50/50 producer/consumer split? Nothing is recomputed here -- 40 and 41
# already conserve the same footprint, so this script only differences, sums by
# continent, averages over the years of a period, and plots.
#
# WHAT IS PLOTTED -------------------------------------------------------------
# Per country i, chain g and year y, both allocations are re-attributions of ONE
# identical total (the chain's consumer footprint). Their divergence from the
# even split of the two conventional accounts is therefore directly comparable:
#     baseline_i = 0.5 * (production_based_i + consumption_based_i)   ("50/50")
#     div_hdi_i  = justice_based_i - baseline_i      (= 40's `imbalance`)
#     div_va_i   = va_resp_i       - baseline_i      (41, summed over commodities)
# Both sum to ~0 over all countries -- a divergence is a REDISTRIBUTION between
# countries, never a change of the global total. So a continent's bar is read
# against the other bars in the same panel, not on its own.
#
#   positive: the allocation loads MORE onto the region than an even split
#   negative: the allocation lets the region off, relative to an even split
#
# The two answer different questions and are NOT expected to agree:
#   HDI (40)  splits every bilateral flow by the two agents' relative HDI -- it
#             moves burden toward the DEVELOPED end of each trade pair.
#   VA  (41)  re-allocates each chain's footprint to whoever CAPTURES THE VALUE
#             along it -- it moves burden toward processors, traders and brand
#             owners, wherever they sit.
# Where they point the same way, the two justice criteria reinforce; where they
# oppose, the choice of criterion decides who carries the impact. That contrast
# is the point of the figure.
#
# READS  <IN_DIR>/ (one set per indicator x VA base x allocation, found by pattern)
#   FABIO_bcp_<ind>_hdi_responsibility_<alloc>.csv                         (40)
#   FABIO_bcp_<ind>_value_added_responsibility_<vabase>_<alloc>_ex_tls.csv (41)
#       ... falling back to the `full` VA variant if the ex_tls file is absent
#
# WRITES output/plot/
#   responsibility_divergence_<ind>_<vabase>_<alloc>.svg            absolute units
#   responsibility_divergence_<ind>_<vabase>_<alloc>_pct.svg        % of the 50/50 baseline
#   responsibility_divergence_<ind>_<vabase>_<alloc>_by_biofuel.svg per chain
# and <IN_DIR>/
#   FABIO_bcp_<ind>_responsibility_divergence_<vabase>_<alloc>.csv  the country-level
#       table behind the figures (year, biofuel_group, iso3c, continent,
#       production_based, consumption_based, baseline_5050, justice_based,
#       va_resp, divergence_hdi, divergence_va)
#
# THE % FIGURE IS THE DANGEROUS ONE -------------------------------------------
# divergence / baseline explodes wherever the baseline is small: ROW's VA
# divergence reaches ~1000% in the late period on a baseline three orders of
# magnitude below NAM's. Continents below MIN_SHARE_PCT of the global baseline
# are therefore dropped from the % figure (never from the absolute one, which
# shows them at their true, negligible size). Read the absolute figure first.
#
# RUN: Rscript R/47_plot_responsibility_divergence.R   (after 40 and 41)
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
message(sprintf(">>> [47] model_version = '%s'  (reading responsibility CSVs from: %s)",
                model_version, IN_DIR))

# --- config ------------------------------------------------------------------
PERIODS <- list("2012-2014" = 2012:2014,
                "2020-2022" = 2020:2022)

# VA variant to difference. 'ex_tls' (value added = wages + capital, taxes-less-
# subsidies removed) is what 42 plots, so the two figures stay comparable; the
# script falls back to 'full' per run if the ex_tls file is missing.
VA_VARIANT <- "ex_tls"          # "ex_tls" | "full"

# % figure only: drop continents whose 50/50 baseline is below this share of the
# global baseline in a period -- see the header. Set to 0 to keep everything.
MIN_SHARE_PCT <- 0.5

TOL <- 1e-4   # relative tolerance of the one-total / sum-to-zero checks

# Fill is by ALLOCATION. The two hues are 42's account_palette entries for the
# same two accounts, so a reader moving between the figures keeps the mapping.
alloc_palette <- c("HDI (justice-based)" = "#009E73",   # green
                   "Value added"         = "#E69F00")   # orange
ALLOC_LEVELS  <- names(alloc_palette)

# --- indicator metadata (as in 42) -------------------------------------------
meta_for <- function(ind_tag) {
  key <- tolower(sub("_total$", "", indicator_meta$indicator))
  i   <- match(tolower(ind_tag), key)
  if (is.na(i)) {
    warning("[47] no indicator_meta entry for '", ind_tag,
            "'; falling back to the raw tag and scale_factor = 1.")
    return(list(scale_factor = 1, y_label = ind_tag, short_label = ind_tag))
  }
  as.list(indicator_meta[i, .(scale_factor, y_label, short_label)])
}

# --- discovery ---------------------------------------------------------------
# Keyed off the value-added file, exactly as 42: it is the one artefact that
# carries all three tags (indicator, VA base, allocation) in its name.
discover_runs <- function(dir = IN_DIR) {
  pat   <- "^FABIO_bcp_(.+)_value_added_responsibility_([^_]+)_([^_]+)_ex_tls\\.csv$"
  files <- list.files(dir, pattern = pat)
  if (!length(files)) return(data.table())
  m <- regmatches(files, regexec(pat, files))
  runs <- rbindlist(lapply(m, function(x)
    data.table(indicator = x[2], va_base = x[3], alloc = x[4])))
  runs[, `:=`(
    va_ex_file  = file.path(dir, sprintf("FABIO_bcp_%s_value_added_responsibility_%s_%s_ex_tls.csv",
                                         indicator, va_base, alloc)),
    va_full_file = file.path(dir, sprintf("FABIO_bcp_%s_value_added_responsibility_%s_%s.csv",
                                          indicator, va_base, alloc)),
    # the accounts take no VA base: keyed by indicator x allocation only
    hdi_file    = file.path(dir, sprintf("FABIO_bcp_%s_hdi_responsibility_%s.csv",
                                         indicator, alloc)))]
  runs[]
}

need_cols <- function(dt, cols, path) {
  miss <- setdiff(cols, names(dt))
  if (length(miss))
    stop("[47] ", basename(path), " lacks column(s): ", paste(miss, collapse = ", "),
         ". Re-run 40/41 -- the metric-neutral column names are expected.")
  invisible(dt)
}

# --- load one run and build the country-level divergence table ----------------
load_divergence <- function(run) {
  hdi <- fread(run$hdi_file)
  need_cols(hdi, c("year", "biofuel_group", "iso3c", "continent", "production_based",
                   "consumption_based", "avg_prod_cons", "justice_based", "imbalance"),
            run$hdi_file)
  
  # VA variant: ex_tls if we have it, else the full file
  va_file <- if (VA_VARIANT == "ex_tls" && file.exists(run$va_ex_file)) run$va_ex_file
  else run$va_full_file
  if (!file.exists(va_file)) return(NULL)
  variant <- if (identical(va_file, run$va_ex_file)) "ex_tls" else "full"
  if (variant != VA_VARIANT)
    message("[47]   no ", VA_VARIANT, " VA file -- differencing the '", variant, "' variant instead")
  va <- fread(va_file)
  need_cols(va, c("year", "va_variant", "biofuel_group", "va_iso3c", "va_resp", "va_continent"),
            va_file)
  va <- va[va_variant == variant]
  if (!nrow(va)) { message("[47]   VA file holds no '", variant, "' rows -- skipping"); return(NULL) }
  
  # 41 resolves VA per (country, commodity); we want it per country
  vac <- va[, .(va_resp = sum(va_resp)), by = .(year, biofuel_group, iso3c = va_iso3c,
                                                va_continent)]
  
  # FULL OUTER join. A country can produce or consume and still capture no value
  # added in these chains (ANT, BRN, DMA, ERI, PRI, SSD in the shipped run): it
  # must enter with va_resp = 0 and a divergence of -baseline, not vanish. The
  # reverse -- VA without any production or consumption -- cannot happen by
  # construction, but is zero-filled symmetrically rather than assumed away.
  d <- merge(hdi[, .(year, biofuel_group, iso3c, continent, production_based,
                     consumption_based, baseline_5050 = avg_prod_cons, justice_based)],
             vac, by = c("year", "biofuel_group", "iso3c"), all = TRUE)
  num <- c("production_based", "consumption_based", "baseline_5050", "justice_based", "va_resp")
  for (j in num) set(d, which(is.na(d[[j]])), j, 0)
  
  # continent: the accounts lookup, then the VA table's, then a visible bucket
  d[continent == "" | is.na(continent), continent := va_continent]
  d[, va_continent := NULL]
  if (anyNA(d$continent) || any(d$continent == "")) {
    message("[47]   no continent for: ",
            paste(sort(unique(d[is.na(continent) | continent == "", iso3c])), collapse = ", "),
            " -- shown as 'Unknown'")
    d[is.na(continent) | continent == "", continent := "Unknown"]
  }
  
  d[, `:=`(divergence_hdi = justice_based - baseline_5050,
           divergence_va  = va_resp       - baseline_5050)]
  
  # --- guards --------------------------------------------------------------
  # (a) 40's imbalance must be exactly the HDI divergence we just recomputed
  imb <- hdi[, .(year, biofuel_group, iso3c, imbalance)]
  chk <- merge(d[, .(year, biofuel_group, iso3c, divergence_hdi)], imb,
               by = c("year", "biofuel_group", "iso3c"))
  sref <- max(abs(chk$imbalance), 0)
  if (sref > 0 && max(abs(chk$divergence_hdi - chk$imbalance)) / sref > TOL)
    warning("[47] recomputed HDI divergence != 40's `imbalance` column -- check the inputs.")
  
  # (b) the two allocations must re-attribute ONE total: sum(justice) = sum(va)
  #     = sum(production) per (year, chain). If they do not, the divergences are
  #     not on a common footing and the figure is meaningless.
  tot <- d[, .(prod = sum(production_based), cons = sum(consumption_based),
               just = sum(justice_based),    va = sum(va_resp)),
           by = .(year, biofuel_group)]
  tot[, rel := (pmax(prod, cons, just, va) - pmin(prod, cons, just, va)) / pmax(abs(prod), 1e-12)]
  if (any(tot$rel > TOL))
    warning(sprintf(paste0("[47] %s/%s/%s: production / consumption / HDI / VA do not share one ",
                           "total (max relative spread %.3g) -- the divergences are not comparable."),
                    run$indicator, run$va_base, run$alloc, max(tot$rel)))
  
  # (c) a redistribution sums to zero across countries
  z <- d[, .(h = sum(divergence_hdi), v = sum(divergence_va), base = sum(baseline_5050)),
         by = .(year, biofuel_group)]
  if (any(pmax(abs(z$h), abs(z$v)) / pmax(z$base, 1e-12) > TOL))
    warning("[47] divergences do not sum to zero across countries -- 40/41 are not conserving.")
  
  # carried as a COLUMN, not an attribute: it survives every data.table op below
  # and lands in the exported CSV, where the reader needs it to know which VA
  # definition the divergence was taken against.
  d[, va_variant := variant]
  setcolorder(d, c("year", "biofuel_group", "iso3c", "continent", "va_variant",
                   "production_based", "consumption_based", "baseline_5050",
                   "justice_based", "va_resp", "divergence_hdi", "divergence_va"))
  d[]
}

# --- mean over the years of each period, summed over countries ---------------
# The MEAN over years (not the sum), so the two periods stay comparable even if a
# run is short a year; keys are whatever the figure facets on.
period_mean <- function(dt, keys) {
  out <- rbindlist(lapply(names(PERIODS), function(p) {
    yrs <- PERIODS[[p]]
    d   <- dt[year %in% yrs]
    if (!nrow(d)) return(NULL)
    n_yr <- uniqueN(d$year)
    if (n_yr < length(yrs))
      message(sprintf("[47]   period %s: only %d of %d years present", p, n_yr, length(yrs)))
    d[, .(baseline_5050  = sum(baseline_5050)  / n_yr,
          divergence_hdi = sum(divergence_hdi) / n_yr,
          divergence_va  = sum(divergence_va)  / n_yr),
      by = keys][, period := p][]
  }))
  if (nrow(out)) out[, period := factor(period, levels = names(PERIODS))]
  out
}

# long format: one row per (facet keys, allocation)
melt_alloc <- function(d, keys) {
  m <- melt(d, id.vars = c(keys, "period", "baseline_5050"),
            measure.vars = c("divergence_hdi", "divergence_va"),
            variable.name = "allocation", value.name = "divergence")
  m[, allocation := factor(fifelse(allocation == "divergence_hdi",
                                   ALLOC_LEVELS[1], ALLOC_LEVELS[2]),
                           levels = ALLOC_LEVELS)]
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
plot_divergence <- function(m, meta, title, subtitle, x_lab, by_biofuel = FALSE) {
  p <- ggplot(m, aes(x = divergence, y = continent, fill = allocation)) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
    geom_col(position = position_dodge2(preserve = "single"), width = 0.75) +
    scale_fill_manual(values = alloc_palette, drop = FALSE) +
    labs(x = x_lab, y = NULL, fill = "Allocation",
         title = title, subtitle = subtitle) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title    = element_text(face = "bold"),
          strip.text      = element_text(face = "bold"),
          panel.grid.major.y = element_blank())
  
  if (!by_biofuel) return(p + facet_wrap(~ period, nrow = 1))
  
  # facet_grid(chain ~ period), free x per ROW: renewable diesel is an order of
  # magnitude below biodiesel and would otherwise be a flat line. Price: bar
  # LENGTHS may not be compared across rows -- read the axis (same trade-off as
  # 42's by-biofuel figure).
  p + facet_grid(biofuel_group ~ period, scales = "free_x", switch = "y") +
    theme(strip.placement   = "outside",
          strip.text.y.left = element_text(face = "bold", angle = 90),
          panel.spacing.x   = unit(10, "pt"))
}

# --- run ---------------------------------------------------------------------
runs <- discover_runs()

if (!nrow(runs)) {
  message(">>> [47] no `*_value_added_responsibility_*_ex_tls.csv` in ", IN_DIR,
          " -- nothing to plot.")
} else {
  message(sprintf(">>> [47] %d run(s) discovered in %s", nrow(runs), IN_DIR))
  
  for (i in seq_len(nrow(runs))) {
    run <- runs[i]
    key <- sprintf("%s / %s / %s", run$indicator, run$va_base, run$alloc)
    
    if (!file.exists(run$hdi_file)) { message("[47] skip ", key, ": no HDI responsibility file"); next }
    message(">>> [47] ", key)
    
    d <- load_divergence(run)
    if (is.null(d) || !nrow(d)) { message("[47] skip ", key, ": no value-added file"); next }
    variant <- d$va_variant[1]
    meta    <- meta_for(run$indicator)
    
    va_lab <- if (variant == "ex_tls") "Value added (ex TLS)" else "Value added (full)"
    sub    <- paste0("positive: the allocation loads more onto the region than an even ",
                     "producer/consumer split; both allocations sum to zero across all regions")
    
    # the table behind the figures
    fwrite(d[order(year, biofuel_group, -divergence_hdi)],
           file.path(IN_DIR, sprintf("FABIO_bcp_%s_responsibility_divergence_%s_%s.csv",
                                     run$indicator, run$va_base, run$alloc)))
    
    # --- 1. absolute, chains pooled ------------------------------------------
    cont <- period_mean(d, "continent")
    if (!nrow(cont)) { message("[47] skip ", key, ": no years in the plotted periods"); next }
    m <- order_by_baseline(melt_alloc(cont, "continent"))
    m[, divergence := divergence / meta$scale_factor]
    
    ttl <- sprintf("%s: divergence from the 50/50 producer/consumer split - %s VA base, %s allocation",
                   meta$short_label, run$va_base, run$alloc)
    ggsave(file.path(PLOT_DIR, sprintf("responsibility_divergence_%s_%s_%s.svg",
                                       run$indicator, run$va_base, run$alloc)),
           plot_divergence(m, meta, ttl, sub,
                           x_lab = paste0(meta$y_label, "  (allocation - 50/50 average)")),
           device = svglite::svglite, width = 11, height = 6)
    
    # --- 2. relative: % of each continent's own 50/50 baseline ----------------
    # Scale-free, and therefore treacherous on a small baseline -- hence the cut.
    pc <- copy(cont)
    pc[, share := 100 * baseline_5050 / sum(baseline_5050), by = period]
    drop <- sort(unique(pc[share < MIN_SHARE_PCT, as.character(continent)]))
    if (length(drop))
      message("[47]   % figure drops (baseline < ", MIN_SHARE_PCT, "% of global): ",
              paste(drop, collapse = ", "))
    pc <- pc[share >= MIN_SHARE_PCT & baseline_5050 > 0]
    if (nrow(pc)) {
      mp <- order_by_baseline(melt_alloc(pc, "continent"))
      mp[, divergence := 100 * divergence / baseline_5050]   # NOT scaled by meta: it is a %
      ggsave(file.path(PLOT_DIR, sprintf("responsibility_divergence_%s_%s_%s_pct.svg",
                                         run$indicator, run$va_base, run$alloc)),
             plot_divergence(mp, meta, paste0(ttl, " - relative"),
                             paste0(sub, ". Regions below ", MIN_SHARE_PCT,
                                    "% of the global baseline are omitted: a % on a ",
                                    "negligible base is noise."),
                             x_lab = "Divergence from the 50/50 average (% of that region's baseline)"),
             device = svglite::svglite, width = 11, height = 6)
    }
    
    # --- 3. absolute, per biofuel chain --------------------------------------
    bf <- period_mean(d, c("continent", "biofuel_group"))
    if (nrow(bf)) {
      mb <- order_by_baseline(melt_alloc(bf, c("continent", "biofuel_group")))
      mb[, divergence := divergence / meta$scale_factor]
      n_bf  <- uniqueN(mb$biofuel_group); n_per <- uniqueN(mb$period)
      ggsave(file.path(PLOT_DIR, sprintf("responsibility_divergence_%s_%s_%s_by_biofuel.svg",
                                         run$indicator, run$va_base, run$alloc)),
             plot_divergence(mb, meta, paste0(ttl, " - by biofuel"),
                             "X axis is FREE PER BIOFUEL ROW: compare regions within a row, not bar lengths across rows.",
                             x_lab = paste0(meta$y_label, "  (allocation - 50/50 average)"),
                             by_biofuel = TRUE),
             device = svglite::svglite,
             width  = 3.0 + 4.2 * max(n_per, 1),
             height = 2.6 + 2.9 * max(n_bf, 1))
    }
    
    # --- console: who moves, and do the two criteria agree? ------------------
    late <- copy(cont[period == names(PERIODS)[length(PERIODS)]])
    if (nrow(late)) {
      late[, agree := fifelse(sign(divergence_hdi) == sign(divergence_va), "same", "OPPOSED")]
      cat(sprintf("\n-- %s | %s: divergence from the 50/50 split, %s (%s VA) --\n",
                  meta$short_label, run$alloc, names(PERIODS)[length(PERIODS)], va_lab))
      print(late[order(-abs(divergence_hdi)),
                 .(continent, baseline_5050 = round(baseline_5050),
                   hdi = round(divergence_hdi), va = round(divergence_va), agree)])
    }
  }
  message(">>> [47] plots written to ", PLOT_DIR)
}