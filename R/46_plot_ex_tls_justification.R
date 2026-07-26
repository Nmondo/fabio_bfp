# =============================================================================
# 46_plot_ex_tls_justification.R
# Why the VA-based responsibility uses the ex-TLS variant and not the full
# account. Nothing is recomputed -- the script reads 42's two responsibility CSVs
# (full and ex_tls) and contrasts them.
#
# THE ARGUMENT (two figures) --------------------------------------------------
# A  SIGN FLIP. Full VA = wages + capital + TLS, and TLS (net taxes less
#    subsidies) is NEGATIVE for net-subsidised sectors. Where the subsidy exceeds
#    wages + capital the value intensity v_i < 0, and the normalised allocation
#    hands that node a NEGATIVE responsibility -- a sector that un-does the
#    pressure it feeds. The N most negative chain nodes, with an arrow to the
#    value the same node takes under ex-TLS, where the negatives vanish.
# B  NO COST. Country totals, full vs ex-TLS, on the 1:1 line. Dropping TLS buys
#    sign-integrity without moving the headline attribution -- which pre-empts
#    the "you picked the variant that suited you" objection.
#
# READS  VA_FULL_CSV, VA_RESP_CSV  (42)
# WRITES output/plot/
#   ex_tls_sign_flip_<ind>_<vabase>_<alloc>_<year>.svg
#   ex_tls_country_totals_<ind>_<vabase>_<alloc>_<year>.svg
#
# RUN: Rscript R/46_plot_ex_tls_justification.R   (after 42)
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

source("R/40_responsibility_shared.R")
SCRIPT <- "46"
run_banner()
message(sprintf(">>> [46] year = %d", PLOT_YEAR))

# --- figure switches ---------------------------------------------------------
N_TOP       <- 10     # panel A: how many of the most negative nodes to draw
LABEL_TOP_N <- 8      # panel B: label the N largest countries (0 = no labels)
SIGMA       <- 1      # panel A: symlog linear-threshold, in plotted units

VARIANT_LEVELS <- names(variant_palette)

# --- load the two variants, outer-joined on the chain node -------------------
# A node absent from one variant is a true zero there (42 drops exact zeros), so
# the fill is 0, not NA. Both variants normalise to the same footprint, so the
# grand totals must agree; the check below says so out loud.
NODE_KEYS <- c("year", "biofuel_group", "va_iso3c", "va_comm_code", "va_item")

load_run <- function() {
  cols <- c(NODE_KEYS, "va_resp")
  full <- fread(need(VA_FULL_CSV, "42"))
  ex   <- fread(need(VA_RESP_CSV, "42"))
  need_cols(full, c(cols, "va_variant"), VA_FULL_CSV)
  need_cols(ex,   c(cols, "va_variant"), VA_RESP_CSV)
  
  # Subset to PLOT_YEAR BEFORE the join: 42 writes every year x node, and only one
  # year is ever drawn, so joining the whole files on five keys costs an order of
  # magnitude in both time and memory for nothing. need_cols runs first so a
  # malformed file still gets the informative error rather than "year not found".
  full <- full[year == PLOT_YEAR, ..cols]
  ex   <- ex[  year == PLOT_YEAR, ..cols]
  
  d <- merge(full, ex, by = NODE_KEYS, all = TRUE, suffixes = c("_full", "_ex"))
  d[is.na(va_resp_full), va_resp_full := 0][is.na(va_resp_ex), va_resp_ex := 0]
  
  tot <- d[, .(full = sum(va_resp_full), ex = sum(va_resp_ex))]
  if (tot$full > 0 && abs(tot$full - tot$ex) / abs(tot$full) > TOL)
    warning(sprintf("[46] the two variants do not re-attribute one total (%.3g vs %.3g)",
                    tot$full, tot$ex))
  d[]
}

# --- panel A: the sign flip --------------------------------------------------
plot_sign_flip <- function(d) {
  top <- d[year == PLOT_YEAR & va_resp_full < 0]
  if (!nrow(top)) {
    message("[46]   no negative nodes under full VA in ", PLOT_YEAR, " -- panel A skipped.")
    return(NULL)
  }
  top <- head(top[order(va_resp_full)], N_TOP)
  top[, `:=`(full = va_resp_full / META$scale_factor,
             ex   = va_resp_ex   / META$scale_factor,
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
    scale_x_continuous(transform = scales::pseudo_log_trans(sigma = SIGMA),
                       breaks = c(-100, -10, -1, 0, 1, 10, 100, 1000)) +
    labs(x = META$y_label, y = NULL, colour = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid.major.y = element_blank())
}

# --- panel B: the country totals ---------------------------------------------
plot_country_totals <- function(d) {
  c_tot <- d[year == PLOT_YEAR, .(full = sum(va_resp_full) / META$scale_factor,
                                  ex   = sum(va_resp_ex)   / META$scale_factor),
             by = .(iso3c = va_iso3c)]
  drop <- c_tot[full <= 0 | ex <= 0]
  if (nrow(drop))
    message("[46]   log axes drop ", nrow(drop), " country/countries with a non-positive total: ",
            paste(drop$iso3c, collapse = ", "))
  c_tot <- c_tot[full > 0 & ex > 0]
  if (!nrow(c_tot)) return(NULL)
  
  lim     <- range(c(c_tot$full, c_tot$ex))
  labs_dt <- head(c_tot[order(-ex)], LABEL_TOP_N)
  
  p <- ggplot(c_tot, aes(x = full, y = ex)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                colour = "grey55", linewidth = 0.4) +
    geom_point(colour = "#0072B2", alpha = 0.75, size = 1.9) +
    scale_x_log10(limits = lim) + scale_y_log10(limits = lim) +
    coord_fixed() +
    labs(x = paste0(META$y_label, " - full VA"),
         y = paste0(META$y_label, " - ex-TLS VA")) +
    theme_minimal()
  
  if (LABEL_TOP_N > 0 && nrow(labs_dt))
    p <- p + geom_text(data = labs_dt, aes(label = iso3c),
                       hjust = -0.25, vjust = 0.4, size = 2.8, colour = "grey25")
  p
}

# --- run ---------------------------------------------------------------------
d <- load_run()
if (!nrow(d[year == PLOT_YEAR])) stop("[46] no rows for ", PLOT_YEAR, " in ", VA_RESP_CSV)

FTAG <- sprintf("%s_%s_%s_%d", STAG, VA_BASE, ATAG, PLOT_YEAR)

p_a <- plot_sign_flip(d)
if (!is.null(p_a))
  save_svg(paste0("ex_tls_sign_flip_", FTAG), p_a, width = 9, height = 5)

p_b <- plot_country_totals(d)
if (!is.null(p_b))
  save_svg(paste0("ex_tls_country_totals_", FTAG), p_b, width = 6, height = 6)

# the numbers behind the argument, for the caption
message(sprintf("[46] %d node(s) negative under full VA in %d; %d under ex_tls",
                d[year == PLOT_YEAR & va_resp_full < 0, .N], PLOT_YEAR,
                d[year == PLOT_YEAR & va_resp_ex   < 0, .N]))