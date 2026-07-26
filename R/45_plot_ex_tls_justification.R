# =============================================================================
# 45_plot_ex_tls_justification.R
# Why the VA-based responsibility uses the ex-TLS variant and not the full
# account. Nothing is recomputed here -- the script reads 41's two responsibility
# CSVs (full and ex_tls) for the same indicator x VA base x allocation, and
# contrasts them.
#
# THE ARGUMENT (two figures) --------------------------------------------------
# A  SIGN FLIP. Full VA = wages + capital + TLS, and TLS (net taxes less
#    subsidies) is NEGATIVE for net-subsidised sectors. Where the subsidy exceeds
#    wages + capital the value intensity v_i < 0, and the normalised allocation
#    hands that node a NEGATIVE responsibility -- a sector that un-does the
#    pressure it feeds. The N most negative chain nodes, with an arrow to the
#    value the same node takes under ex-TLS. Under ex_tls the negatives vanish.
# B  NO COST. Country totals, full vs ex-TLS, on the 1:1 line. Dropping TLS buys
#    sign-integrity without moving the headline attribution -- which pre-empts
#    the "you picked the variant that suited you" objection.
#
# READS  <IN_DIR>/ (one set per indicator x VA base x allocation, found by pattern)
#   FABIO_bcp_<ind>_value_added_responsibility_<vabase>_<alloc>.csv         (41, full)
#   FABIO_bcp_<ind>_value_added_responsibility_<vabase>_<alloc>_ex_tls.csv  (41, ex_tls)
#
# WRITES output/plot/
#   ex_tls_sign_flip_<ind>_<vabase>_<alloc>_<year>.svg
#   ex_tls_country_totals_<ind>_<vabase>_<alloc>_<year>.svg
#
# RUN: Rscript R/45_plot_ex_tls_justification.R   (after 41)
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
library(scales)      # pseudo_log_trans() for the symlog x axis of panel A

source("R/19_plot_definitions.R")   # indicator_meta

model_version <- Sys.getenv("FABIO_RUN_MODE", unset = "rescaled")
model_version <- if (tolower(trimws(model_version)) == "bypass") "bypass" else "rescaled"
IN_DIR   <- if (model_version == "bypass") "output/bypass" else "output"
PLOT_DIR <- file.path("output", "plot")
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)
message(sprintf(">>> [45] model_version = '%s'  (reading responsibility CSVs from: %s)",
                model_version, IN_DIR))

# --- config ------------------------------------------------------------------
YEAR         <- 2019   # the year both panels show
N_TOP        <- 10     # panel A: how many of the most negative nodes to draw
LABEL_TOP_N  <- 8      # panel B: label the N largest countries (0 = no labels)
SIGMA        <- 1      # panel A: symlog linear-threshold, in plotted units

variant_palette <- c("Full VA (wages + capital + TLS)" = "#D55E00",   # vermillion
                     "ex-TLS VA (wages + capital)"     = "#0072B2")   # blue
VARIANT_LEVELS <- names(variant_palette)

# --- indicator metadata (as in 42) -------------------------------------------
meta_for <- function(ind_tag) {
  key <- tolower(sub("_total$", "", indicator_meta$indicator))
  i   <- match(tolower(ind_tag), key)
  if (is.na(i)) {
    warning("[45] no indicator_meta entry for '", ind_tag,
            "'; falling back to the raw tag and scale_factor = 1.")
    return(list(scale_factor = 1, y_label = ind_tag, short_label = ind_tag))
  }
  as.list(indicator_meta[i, .(scale_factor, y_label, short_label)])
}

# --- discovery ---------------------------------------------------------------
# The pattern matches the FULL file only: the ex_tls file has three underscored
# tokens after "responsibility_" and so cannot satisfy ([^_]+)_([^_]+)\.csv$.
discover_runs <- function(dir = IN_DIR) {
  files <- list.files(dir,
                      pattern = "^FABIO_bcp_.+_value_added_responsibility_[^_]+_[^_]+\\.csv$")
  if (!length(files)) return(data.table())
  m <- regmatches(files, regexec(
    "^FABIO_bcp_(.+)_value_added_responsibility_([^_]+)_([^_]+)\\.csv$", files))
  runs <- rbindlist(lapply(m, function(x)
    data.table(indicator = x[2], va_base = x[3], alloc = x[4])))
  runs[, `:=`(
    full_file = file.path(dir, sprintf("FABIO_bcp_%s_value_added_responsibility_%s_%s.csv",
                                       indicator, va_base, alloc)),
    ex_file   = file.path(dir, sprintf("FABIO_bcp_%s_value_added_responsibility_%s_%s_ex_tls.csv",
                                       indicator, va_base, alloc)))]
  runs[]
}

need_cols <- function(dt, cols, path) {
  miss <- setdiff(cols, names(dt))
  if (length(miss))
    stop("[45] ", basename(path), " lacks column(s): ", paste(miss, collapse = ", "),
         ". Re-run 41 -- the metric-neutral column names are expected.")
  invisible(dt)
}

# --- load one run: the two variants, outer-joined on the chain node ----------
# A node absent from one variant is a true zero there (41 drops exact zeros), so
# the fill is 0, not NA. Both variants normalise to the same footprint, so the
# grand totals must agree; the check below says so out loud.
NODE_KEYS <- c("year", "biofuel_group", "va_iso3c", "va_comm_code", "va_item")

load_run <- function(run) {
  cols <- c(NODE_KEYS, "va_resp")
  full <- fread(run$full_file)
  ex   <- fread(run$ex_file)
  need_cols(full, c(cols, "va_variant"), run$full_file)
  need_cols(ex,   c(cols, "va_variant"), run$ex_file)
  
  d <- merge(full[, ..cols], ex[, ..cols], by = NODE_KEYS,
             all = TRUE, suffixes = c("_full", "_ex"))
  d[is.na(va_resp_full), va_resp_full := 0][is.na(va_resp_ex), va_resp_ex := 0]
  
  tot <- d[year == YEAR, .(full = sum(va_resp_full), ex = sum(va_resp_ex))]
  if (tot$full > 0 && abs(tot$full - tot$ex) / abs(tot$full) > 1e-6)
    warning(sprintf("[45] %s/%s/%s: the two variants do not re-attribute one total (%.3g vs %.3g)",
                    run$indicator, run$va_base, run$alloc, tot$full, tot$ex))
  d[]
}

# --- panel A: the sign flip --------------------------------------------------
plot_sign_flip <- function(d, meta) {
  top <- d[year == YEAR & va_resp_full < 0]
  if (!nrow(top)) {
    message("[45]   no negative nodes under full VA in ", YEAR, " -- panel A skipped.")
    return(NULL)
  }
  top <- head(top[order(va_resp_full)], N_TOP)
  top[, `:=`(full = va_resp_full / meta$scale_factor,
             ex   = va_resp_ex   / meta$scale_factor,
             lab  = sprintf("%s %s (%s)", va_iso3c, va_item,
                            gsub("_", " ", biofuel_group)))]
  # least negative first => it becomes the bottom of the discrete axis, and the
  # most negative node sits at the top
  top[, lab := factor(lab, levels = top[order(-va_resp_full), lab])]
  
  ggplot(top, aes(y = lab)) +
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
             fill = "#D55E00", alpha = 0.06) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
    geom_segment(aes(x = full, xend = ex, yend = lab), colour = "grey55",
                 linewidth = 0.4,
                 arrow = arrow(length = unit(0.16, "cm"), type = "closed")) +
    geom_point(aes(x = full, colour = VARIANT_LEVELS[1]), size = 2.6) +
    geom_point(aes(x = ex,   colour = VARIANT_LEVELS[2]), size = 2.6) +
    scale_colour_manual(values = variant_palette,
                        limits = VARIANT_LEVELS, breaks = VARIANT_LEVELS) +
    scale_x_continuous(trans = scales::pseudo_log_trans(sigma = SIGMA),
                       breaks = c(-100, -10, -1, 0, 1, 10, 100, 1000)) +
    labs(x = meta$y_label, y = NULL, colour = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid.major.y = element_blank())
}

# --- panel B: the country totals -------------------------------------------
plot_country_totals <- function(d, meta) {
  c_tot <- d[year == YEAR, .(full = sum(va_resp_full) / meta$scale_factor,
                             ex   = sum(va_resp_ex)   / meta$scale_factor),
             by = .(iso3c = va_iso3c)]
  drop <- c_tot[full <= 0 | ex <= 0]
  if (nrow(drop))
    message("[45]   log axes drop ", nrow(drop), " country/countries with a non-positive total: ",
            paste(drop$iso3c, collapse = ", "))
  c_tot <- c_tot[full > 0 & ex > 0]
  if (!nrow(c_tot)) return(NULL)
  
  lim <- range(c(c_tot$full, c_tot$ex))
  labs_dt <- head(c_tot[order(-ex)], LABEL_TOP_N)
  
  p <- ggplot(c_tot, aes(x = full, y = ex)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                colour = "grey55", linewidth = 0.4) +
    geom_point(colour = "#0072B2", alpha = 0.75, size = 1.9) +
    scale_x_log10(limits = lim) + scale_y_log10(limits = lim) +
    coord_fixed() +
    labs(x = paste0(meta$y_label, " - full VA"),
         y = paste0(meta$y_label, " - ex-TLS VA")) +
    theme_minimal()
  
  if (LABEL_TOP_N > 0 && nrow(labs_dt))
    p <- p + geom_text(data = labs_dt, aes(label = iso3c),
                       hjust = -0.25, vjust = 0.4, size = 2.8, colour = "grey25")
  p
}

# --- run ---------------------------------------------------------------------
runs <- discover_runs()

if (!nrow(runs)) {
  message(">>> [45] no `*_value_added_responsibility_*.csv` in ", IN_DIR, " -- nothing to plot.")
} else {
  message(sprintf(">>> [45] %d run(s) discovered in %s", nrow(runs), IN_DIR))
  
  for (i in seq_len(nrow(runs))) {
    run <- runs[i]
    key <- sprintf("%s / %s / %s", run$indicator, run$va_base, run$alloc)
    
    if (!file.exists(run$ex_file)) { message("[45] skip ", key, ": no ex_tls file"); next }
    
    message(">>> [45] ", key)
    meta <- meta_for(run$indicator)
    d    <- load_run(run)
    if (!nrow(d[year == YEAR])) { message("[45] skip ", key, ": no year ", YEAR); next }
    
    tag <- sprintf("%s_%s_%s_%d", run$indicator, run$va_base, run$alloc, YEAR)
    
    p_a <- plot_sign_flip(d, meta)
    if (!is.null(p_a))
      ggsave(file.path(PLOT_DIR, sprintf("ex_tls_sign_flip_%s.svg", tag)),
             p_a, device = svglite::svglite, width = 9, height = 5)
    
    p_b <- plot_country_totals(d, meta)
    if (!is.null(p_b))
      ggsave(file.path(PLOT_DIR, sprintf("ex_tls_country_totals_%s.svg", tag)),
             p_b, device = svglite::svglite, width = 6, height = 6)
    
    # the numbers behind the argument, for the caption
    n_neg <- d[year == YEAR & va_resp_full < 0, .N]
    message(sprintf("[45]   %d node(s) negative under full VA in %d; %d under ex_tls",
                    n_neg, YEAR, d[year == YEAR & va_resp_ex < 0, .N]))
  }
  message(">>> [45] plots written to ", PLOT_DIR)
}