# =============================================================================
# 44_plot_responsibility_trade_split.R
# ONE figure: the production- (PBA), consumption- (CBA) and HDI justice-based
# accounts as ADJACENT stacked bars per country, for a single year, each split by
# the ORIGIN x DESTINATION of the CHARGED impact flow -- 19_01b's balance
# convention, carried over to the responsibility accounts.
#   x = country (order taken from 43, see below), one bar per account | y = impact
#   Each bar is a single account, ALL >= 0: the bar HEIGHT is the account total,
#   and its segments show where that charged impact flows (domestic / export /
#   import). What an account does NOT charge is simply not drawn.
#
# THE PBA BAR IS THE REFERENCE (SHOW_PBA, on by default) -----------------------
# PBA charges a country everything it produces and nothing it imports, so its bar
# is domestic (mid-grey) + the WHOLE export (dark) -- the country's physical
# production, drawn to scale. That same dark export block is what the other two
# accounts hand around: CBA charges none of it (its dark block vanishes) and HDI
# keeps the beta share. Reading left to right across a country's three bars -- the
# dark block shrinking from PBA to HDI to CBA -- IS the argument.
#
# THE DISAGGREGATION -----------------------------------------------------------
# All accounts are margins of the SAME bilateral matrix D[p, c] (impact in
# producing country p, driven by final demand in consuming country c), which 40
# already computed. Per country i, the three physical flows are
#     domestic(i) = D[i, i]              domestic production, domestic consumption
#     export(i)   = sum_c!=i D[i, c]     domestic production, FOREIGN consumption
#     import(i)   = sum_p!=i D[p, i]     FOREIGN production, domestic consumption
#     PBA(i) = domestic + export ,  CBA(i) = domestic + import
# An account is a rule for how much of each flow a country is CHARGED (and only the
# charged part is drawn -- the rest is booked to the trade partner and left out):
#
#   CBA   domestic charged in full; export booked to the consumer (not drawn);
#         import charged in full.
#   HDI   the two trade flows are SPLIT by relative HDI (Sun et al. 2022, eqs 1-3),
#         beta = HDI_p / (HDI_p + HDI_c): the producer keeps beta of its export,
#         the consumer keeps (1-beta) of its import. CBA is the beta = 0 corner.
#   PBA   domestic + export, both charged in full; nothing imported.
#
# So each bar's height is exactly its account (PBA / CBA / HDI), and the PBA bar's
# height is the yardstick the other two are read against.
#
# COUNTRY ORDER ---------------------------------------------------------------
# The country set and their left-to-right order are TAKEN FROM 43's value-added
# figure (see the top-N block), so the two figures line up country by country.
#
# CONSERVATION ----------------------------------------------------------------
# Summed over ALL countries the CHARGED segments of each account reach the SAME
# global footprint -- the accounts re-attribute, never create. Checked below.
#
# NEGATIVE FLOWS -- why a country can have a MALFORMED bar ---------------------
# D[p, c] is not guaranteed non-negative: FABIO's Y carries negative components
# (stock change), so a country that produces none of a chain and draws it down
# from stock ends up with PBA = domestic = 0 and a NEGATIVE CBA -- the import
# residual then IS the whole account, which is what a "violation of 1.0 of its own
# scale" means. It is not noise and not a 41 bug; it is what the underlying MRIO
# says. Pooling the chains usually cancels it, so it surfaces only per chain.
# NEG_FLOWS decides what such a row does to the figure:
#   clamp  (default) floors the flows at zero and REBUILDS the account totals from
#          the clamped components. A negative account is drawn as zero -- a lie,
#          but a bounded, reported one.
#   keep   touches nothing. Honest, but a negative flow would dip a segment below
#          zero and distort the bar.
#   drop   removes the flagged (country, chain) rows from the figure entirely.
# The console prints each flagged row's weight as a share of the world total: judge
# the relative violation against THAT before caring about it.
#
# READS  <IN_DIR>/FABIO_bcp_<ind>_hdi_responsibility_<alloc>.csv               (40)
#          needs: year, biofuel_group, iso3c, continent, production_based,
#                 consumption_based, justice_based, justice_domestic,
#                 justice_export, justice_import
#        <IN_DIR>/FABIO_bcp_<ind>_responsibility_country_order_<vabase>_<alloc>_<year>.csv (43)
#          the country order to follow; if absent, this script ranks its own.
#        Nothing is recomputed -- 40's CSV already carries every term.
# WRITES output/plot/
#   responsibility_trade_split_<ind>_<alloc>_<year>.svg
#   responsibility_trade_split_<ind>_<alloc>_<year>_by_biofuel.svg   (if BY_BIOFUEL)
#
# RUN: Rscript R/44_plot_responsibility_trade_split.R   (after 40, and 43 for order)
#      the switches below must match the 40 run whose CSV is being read.
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
library(scales)
library(svglite)

source("R/19_plot_definitions.R")   # indicator_meta

# --- run switches (must match the 41 run that wrote the CSV) -----------------
YEAR         <- 2019L
allocation   <- "value"          # co-product rule of B: "mass" | "value"
STRESSOR     <- "ibif_total"     # "ibif_total" | "LCIM_EQ_terrestrial"

GROUPS       <- NULL             # biofuel chains to pool; NULL = all in the CSV
# The country set and their order are TAKEN FROM 43's value-added figure so the two
# responsibility figures show the same countries in the same order (see the top-N
# block below). ORDER_VA_BASE picks WHICH of 43's per-VA-base order files to follow;
# PBA/CBA/HDI do not depend on the VA base, so this only names the file. It must
# match a VA base 43 was actually run for.
ORDER_VA_BASE <- "exiobase"      # "gloria" | "exiobase" -- which 43 order file to follow
# TOP_N and RANK_BY are now a FALLBACK only: they are used just when 43's order file
# is absent (43 not yet run, or run for a different VA base / year / stressor).
TOP_N        <- 15               # FALLBACK countries shown (only if no 43 order file)
RANK_BY      <- "max"            # FALLBACK ranking: "max" | "mean" | "CBA" | "HDI" | "PBA"
SHOW_PBA     <- TRUE             # draw production-based as its own BAR: domestic + the whole export.
#   Its height is the country's physical production -- the reference
#   the CBA and HDI bars move against.
BY_BIOFUEL   <- TRUE             # also write the per-chain variant
NEG_FLOWS    <- "clamp"          # malformed rows (see below): "clamp" | "keep" | "drop"
TOL          <- 1e-6             # relative tolerance of the consistency checks
NOISE_TOL    <- 1e-9             # below this (relative to a row's own size) a defect is float noise

# --- paths -------------------------------------------------------------------
model_version <- if (tolower(trimws(Sys.getenv("FABIO_RUN_MODE", "rescaled"))) == "bypass")
  "bypass" else "rescaled"
IN_DIR   <- if (model_version == "bypass") "output/bypass" else "output"
PLOT_DIR <- file.path("output", "plot")
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

ATAG <- tolower(allocation)                       # "mass" / "value"
STAG <- tolower(sub("_total$", "", STRESSOR))     # "ibif" / "lcim_eq_terrestrial"
HDI_FILE <- file.path(IN_DIR, sprintf("FABIO_bcp_%s_hdi_responsibility_%s.csv", STAG, ATAG))

message(sprintf(">>> [44] %d | model='%s' | stressor='%s' | alloc='%s'",
                YEAR, model_version, STRESSOR, allocation))

if (!file.exists(HDI_FILE)) {
  have <- list.files(IN_DIR, pattern = "_hdi_responsibility_.*\\.csv$")
  stop("Missing input: ", HDI_FILE, "  (produced by 40)\n",
       if (length(have)) paste0("  present in ", IN_DIR, ": ",
                                paste(have, collapse = ", ")) else "")
}

# --- indicator metadata (as in 43) -------------------------------------------
meta <- {
  i <- match(STAG, tolower(sub("_total$", "", indicator_meta$indicator)))
  if (is.na(i)) {
    warning("[44] no indicator_meta entry for '", STAG, "'; scale_factor = 1.")
    list(scale_factor = 1, y_label = STAG, short_label = STAG)
  } else as.list(indicator_meta[i, .(scale_factor, y_label, short_label)])
}

# --- load 41's responsibility CSV --------------------------------------------
NEED <- c("year", "biofuel_group", "iso3c", "continent", "production_based",
          "consumption_based", "justice_based", "justice_domestic",
          "justice_export", "justice_import")
h <- fread(HDI_FILE)
miss <- setdiff(NEED, names(h))
if (length(miss))
  stop("[44] ", basename(HDI_FILE), " lacks column(s): ", paste(miss, collapse = ", "),
       ". Re-run 41 -- the justice_domestic/export/import split is expected.")

if (!YEAR %in% h$year)
  stop("[44] year ", YEAR, " absent from ", basename(HDI_FILE),
       "; available: ", paste(sort(unique(h$year)), collapse = ", "))

h <- h[year == YEAR]
if (!is.null(GROUPS)) {
  unknown <- setdiff(GROUPS, unique(h$biofuel_group))
  if (length(unknown)) stop("[44] biofuel_group(s) not in the CSV: ",
                            paste(unknown, collapse = ", "))
  h <- h[biofuel_group %in% GROUPS]
}
message(sprintf("[44] %d rows | chains pooled: %s", nrow(h),
                paste(sort(unique(h$biofuel_group)), collapse = " + ")))

# --- the charged components --------------------------------------------------
# 41 writes `justice_export` / `justice_import` ALREADY HDI-WEIGHTED (the beta the
# producer keeps of its exports; the 1-beta the consumer keeps of its imports).
# The PHYSICAL flows are the residuals of the two totals -- the kept shares are
# taken from them:
#     export_full(i) = production_based(i)  - domestic(i)
#     import_full(i) = consumption_based(i) - domestic(i)
ACCOUNTS   <- c(if (SHOW_PBA) "PBA", "CBA", "HDI")
COMPONENTS <- c("domestic", "export_kept", "import_kept")   # each bar IS the charged account, all >= 0
CHARGED    <- COMPONENTS                                     # (kept for the identity check below)

split_components <- function(d, keys) {
  a <- d[, .(domestic    = sum(justice_domestic),
             export_full = sum(production_based)  - sum(justice_domestic),
             import_full = sum(consumption_based) - sum(justice_domestic),
             hdi_export  = sum(justice_export),
             hdi_import  = sum(justice_import),
             PBA         = sum(production_based),
             CBA         = sum(consumption_based),
             HDI         = sum(justice_based)),
         by = keys]
  
  # HDI's own columns must rebuild its total (41 writes them that way). Test this on
  # the RAW numbers, BEFORE anything below touches them: it is a check on 41, and a
  # clamp would hide it.
  dev_h <- max(abs(a$domestic + a$hdi_export + a$hdi_import - a$HDI)) / max(abs(a$HDI), 1)
  if (dev_h > NOISE_TOL)
    warning(sprintf("[44] justice_domestic+export+import != justice_based (max rel. %.3g)", dev_h))
  
  # --- are the PHYSICAL flows well-formed? ------------------------------------
  # D[p, c] is NOT guaranteed non-negative: FABIO's Y carries negative components
  # (stock change), and the value-allocated L can carry small negatives too. So a
  # country-chain's export or import RESIDUAL can come out negative, and an HDI
  # share can then exceed the flow it splits. That is a property of 41's input, not
  # a bug here -- but it would push a kept share negative and distort the bar, so
  # surface it with
  # NUMBERS instead of a bare country name, and judge it against each row's OWN
  # scale: an absolute tolerance is meaningless when one country-chain is 1e4 and
  # the next is 1e-9. (This is why the pooled figure can be clean while a single
  # chain is not: summing the chains cancels the negative entries.)
  s   <- pmax(abs(a$PBA), abs(a$CBA), .Machine$double.eps)     # each row's own scale
  chk <- data.table(a[, ..keys], scale = s,
                    neg_domestic = pmax(-a$domestic,    0) / s,
                    neg_export   = pmax(-a$export_full, 0) / s,
                    neg_import   = pmax(-a$import_full, 0) / s,
                    over_export  = pmax(a$hdi_export - a$export_full, 0) / s,
                    over_import  = pmax(a$hdi_import - a$import_full, 0) / s)
  chk[, worst := pmax(neg_domestic, neg_export, neg_import, over_export, over_import)]
  chk[, who := if ("biofuel_group" %in% keys) paste0(iso3c, "/", biofuel_group) else iso3c]
  
  # A RELATIVE violation alone cannot be judged: "1.0 of its own scale" is what a
  # country whose whole account is one small NEGATIVE number looks like (produces
  # nothing, consumes a stock drawdown -- so PBA = domestic = 0 and CBA < 0, and the
  # import residual IS the violation). Catastrophic as a ratio, invisible on the
  # figure. So report the row's absolute weight in the same breath.
  world <- sum(abs(a$CBA))
  chk[, share_of_world := scale / max(world, .Machine$double.eps)]
  
  noise <- chk[worst > 0 & worst <= NOISE_TOL]
  if (nrow(noise))
    message(sprintf("[44] %d country-chain(s) off by <= %.0e of their own scale (float noise).",
                    nrow(noise), NOISE_TOL))
  
  bad <- chk[worst > NOISE_TOL][order(-share_of_world)]
  if (nrow(bad)) {
    warning(sprintf(paste0("[44] %d country-chain(s) with a MALFORMED flow: a negative export/import ",
                           "residual, or an HDI share above the flow it splits. D has negative bilateral ",
                           "entries there -- FABIO's Y carries negative components (stock change), so a ",
                           "country that produces none of a chain and draws it down from stock gets a ",
                           "NEGATIVE account. NEG_FLOWS = '%s'. Ranked by weight: %s"),
                    nrow(bad), NEG_FLOWS,
                    paste(sprintf("%s (%.0e of the world total; violates by %.1e of its own scale)",
                                  head(bad$who, 5), head(bad$share_of_world, 5), head(bad$worst, 5)),
                          collapse = "; ")))
    message(sprintf("[44]   flagged rows are %.2g%% of the world total -- %s",
                    100 * sum(bad$share_of_world),
                    if (sum(bad$share_of_world) < 1e-4)
                      "negligible; whatever NEG_FLOWS does to them cannot move the figure."
                    else "big enough to matter: check them before quoting any bar."))
  }
  
  # NEG_FLOWS decides what a malformed row does to the figure:
  #   clamp  flows floored at zero and the account totals REBUILT from the clamped
  #          components, so each bar equals its clamped account total. A negative
  #          account is shown as zero -- a lie, but a bounded one, reported above.
  #   keep   nothing is touched. Honest, but a negative flow dips a segment below
  #          zero and distorts the bar.
  #   drop   the flagged (country, chain) rows leave the figure entirely.
  if (NEG_FLOWS == "drop" && nrow(bad)) {
    a <- a[!bad[, ..keys], on = keys]
  } else if (NEG_FLOWS == "clamp") {
    a[, `:=`(domestic    = pmax(domestic,    0),
             export_full = pmax(export_full, 0),
             import_full = pmax(import_full, 0))]
    a[, `:=`(hdi_export  = pmin(pmax(hdi_export, 0), export_full),
             hdi_import  = pmin(pmax(hdi_import, 0), import_full))]
    a[, `:=`(PBA = domestic + export_full,
             CBA = domestic + import_full,
             HDI = domestic + hdi_export + hdi_import)]
  } else if (NEG_FLOWS != "keep") {
    stop("[44] NEG_FLOWS must be 'clamp', 'keep' or 'drop'.")
  }
  
  # Each bar is the account's charged impact, split by where the flow goes:
  # PBA keeps its whole export; CBA cedes it entirely (export_kept = 0); HDI keeps
  # the beta share. What is ceded is simply not drawn -- the bar height is the account.
  mk <- function(acc, kept_exp, kept_imp)
    data.table(a[, ..keys], account = acc,
               domestic    = a$domestic,
               export_kept = kept_exp,
               import_kept = kept_imp)
  
  parts <- list(
    mk("CBA", kept_exp = 0,            kept_imp = a$import_full),
    mk("HDI", kept_exp = a$hdi_export, kept_imp = a$hdi_import)
  )
  if (SHOW_PBA)
    parts <- c(list(mk("PBA", kept_exp = a$export_full, kept_imp = 0)), parts)
  
  long <- melt(rbindlist(parts, use.names = TRUE),
               id.vars       = c(keys, "account"),
               measure.vars  = COMPONENTS,
               variable.name = "component", value.name = "value")
  
  # --- the identity the whole figure rests on ---------------------------------
  # charged = domestic + export kept + import kept. Derive it from THOSE components,
  # not from "every segment above zero": a country with no positive segment in an
  # account would drop out of the comparison entirely, and the clamp above is the
  # only thing standing between a negative flow and a segment on the wrong side.
  # MERGE on the keys -- never compare by position. (The two are not in the same
  # order the moment a country-chain has nothing charged under CBA, which is exactly
  # what a pure exporter looks like once the chains are split.)
  chg <- long[component %in% CHARGED,
              .(charged = sum(value)), by = c(keys, "account")]
  ref <- melt(a[, c(keys, ACCOUNTS), with = FALSE], id.vars = keys,
              variable.name = "account", value.name = "acct")
  ref[, account := as.character(account)]
  cmp <- merge(ref, chg, by = c(keys, "account"), all = TRUE)
  cmp[is.na(charged), charged := 0][is.na(acct), acct := 0]
  for (acc in ACCOUNTS) {
    z   <- cmp[account == acc]
    dev <- max(abs(z$charged - z$acct)) / max(max(abs(z$acct)), 1)
    if (dev > NOISE_TOL)
      warning(sprintf("[44] %s: the charged segments do not sum to the account (max rel. %.3g)",
                      acc, dev))
  }
  
  long[, component := factor(component, levels = COMPONENTS)]
  long[, account   := factor(account,   levels = ACCOUNTS)]
  
  list(long = long, wide = a)
}

d     <- split_components(h, c("iso3c", "continent"))
long  <- d$long
wide  <- d$wide

# --- conservation & context --------------------------------------------------
tot <- long[component %in% CHARGED, .(charged = sum(value)), by = account]
if ((max(tot$charged) - min(tot$charged)) / max(abs(tot$charged)) > TOL)
  warning("[44] the accounts do not share one total:\n",
          paste(sprintf("  %-4s %.6g", tot$account, tot$charged), collapse = "\n"))
message("[44] charged totals (", meta$short_label, ", ", YEAR, "): ",
        paste(sprintf("%s %.6g", tot$account, tot$charged), collapse = " | "))

# world exports must equal world imports: every traded flow is somebody's both
we <- sum(wide$export_full); wi <- sum(wide$import_full)
if (abs(we - wi) / max(abs(we), 1) > TOL)
  warning(sprintf("[44] world exports (%.6g) != world imports (%.6g)", we, wi))
message(sprintf("[44] traded share of the footprint: %.1f%% (the rest never crosses a border)",
                100 * we / max(sum(wide$CBA), .Machine$double.eps)))

# --- countries: follow 43's value-added figure -------------------------------
# The set and its left-to-right order are read from 43's order file so this figure
# shows the SAME countries in the SAME order. 43 keeps VA in its selection, which
# pulls in value-capturing hubs (NLD) that a PBA/CBA-only ranking drops; HDI stays
# inside the PBA/CBA envelope, so inheriting 43's set loses nothing here. Only when
# that file is absent do we fall back to this script's own charged-total ranking.
ctot <- dcast(long[component %in% CHARGED, .(value = sum(value)), by = .(iso3c, account)],
              iso3c ~ account, value.var = "value", fill = 0)

order_file <- file.path(IN_DIR, sprintf("FABIO_bcp_%s_responsibility_country_order_%s_%s_%d.csv",
                                        STAG, ORDER_VA_BASE, ATAG, YEAR))
from_43 <- file.exists(order_file)

if (from_43) {
  ord <- fread(order_file)
  if (!all(c("position", "iso3c") %in% names(ord)))
    stop("[44] ", basename(order_file), " lacks 'position'/'iso3c' -- re-run 43.")
  setorder(ord, position)
  missing <- setdiff(ord$iso3c, ctot$iso3c)   # 43's set but no charged rows in this CSV
  if (length(missing))
    message("[44] ", length(missing), " of 43's countries absent from the HDI CSV, dropped: ",
            paste(missing, collapse = ", "))
  keep  <- ord$iso3c[ord$iso3c %in% ctot$iso3c]     # 43's order, restricted to what we have
  shown <- ctot[match(keep, ctot$iso3c)]            # rows in 43's order; carries PBA/CBA/HDI
  message(sprintf("[44] country set + order FOLLOW 43 (%s): %s",
                  basename(order_file), paste(shown$iso3c, collapse = " ")))
} else {
  # FALLBACK: 43 not available for this run -- rank on the CHARGED (positive) total,
  # so a pure exporter still enters on its HDI bar even though CBA charges it little.
  message("[44] no 43 order file (", basename(order_file),
          ") -- falling back to this script's own top-", TOP_N, " '", RANK_BY, "' ranking.")
  ctot[, rank_by := switch(RANK_BY,
                           max  = do.call(pmax, as.list(ctot[, ..ACCOUNTS])),
                           mean = rowMeans(as.matrix(ctot[, ..ACCOUNTS])),
                           ctot[[RANK_BY]])]
  setorder(ctot, -rank_by)
  shown <- ctot[seq_len(min(TOP_N, .N))]
}
message(sprintf("[44] %d countries shown; they cover %s of the world charged total.", nrow(shown),
                paste(sprintf("%s %.0f%%", ACCOUNTS,
                              100 * colSums(shown[, ..ACCOUNTS]) / colSums(ctot[, ..ACCOUNTS])),
                      collapse = " | ")))

# how the caption/subtitle should describe the selection
SEL_NOTE <- if (from_43) {
  sprintf("Countries and their order follow the value-added figure (43, %s base).", ORDER_VA_BASE)
} else {
  sprintf("Top %d countries by %s charged account (43 order file absent).", nrow(shown), RANK_BY)
}

# --- plot --------------------------------------------------------------------
# One panel per country, strips BELOW the axis: a nested "account within country"
# axis, so the bars sit side by side without any manual x-offset arithmetic (43).
# Each bar is a single account, all >= 0, stacked by where its charged impact flows.
# Black-and-white: three well-separated greys, since three categories read cleanly
# by lightness alone (patterns would be overkill). The EXPORT block is the darkest
# on purpose -- its shrinking from PBA to HDI to CBA is the figure's whole point,
# so it stays the loudest mark. Domestic is mid, import the lightest.
flow_colors <- c(domestic     = "grey52",
                 export_kept  = "grey25",
                 import_kept  = "grey78")
flow_labels <- c(domestic     = "Domestic \u2192 domestic",
                 export_kept  = "Domestic \u2192 foreign",
                 import_kept  = "Foreign \u2192 domestic")

SUB <- paste0(
  if (SHOW_PBA) "PBA production-based | " else "",
  "CBA consumption-based | HDI justice-based (Sun et al. 2022): re-attributions of one ",
  "and the same total.\nEvery bar is the SAME country; only the charging rule moves, so the ",
  "bar HEIGHT is that account. Segments show where the charged impact flows (see legend):\ndark = produced-here-for-export, ",
  "mid-grey = domestic, light = imported-and-consumed-here. ",
  if (SHOW_PBA)
    paste0("PBA charges the whole dark export block; CBA charges none of it (it vanishes); ",
           "HDI keeps HDI_p/(HDI_p+HDI_c) of it.")
  else
    paste0("CBA charges none of the dark export block; HDI keeps HDI_p/(HDI_p+HDI_c) of it."))

plot_split <- function(p, title, subtitle, by_biofuel = FALSE) {
  gg <- ggplot(p, aes(x = account, y = value / meta$scale_factor, fill = component)) +
    geom_col(position = position_stack(reverse = TRUE),   # domestic against the zero line
             width = 0.8, colour = "white", linewidth = 0.2) +
    scale_fill_manual(values = flow_colors, labels = flow_labels, drop = FALSE,
                      name = "Impact flow (origin \u2192 destination)") +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    labs(x = NULL, y = meta$y_label, title = title, subtitle = subtitle,
         caption = sprintf("%s allocation; %s.", allocation,
                           paste(sort(unique(h$biofuel_group)), collapse = " + "))) +
    theme_minimal(base_size = 11) +
    theme(legend.position    = "bottom",
          legend.key.size    = unit(10, "pt"),
          legend.title       = element_text(face = "bold"),
          strip.placement    = "outside",
          strip.background   = element_blank(),
          strip.text         = element_text(face = "bold", size = 9),
          panel.spacing.x    = unit(3, "pt"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank(),
          axis.text.x        = element_text(size = 7, angle = 90, hjust = 1, vjust = 0.5),
          plot.subtitle      = element_text(size = 9, colour = "grey30"),
          plot.caption       = element_text(size = 8, colour = "grey40"))
  if (by_biofuel)
    # rows are free: the chains differ by an order of magnitude, and a shared y
    # axis would flatten the small ones. switch = "both" puts the fuel labels on the
    # LEFT (rotated) and the country codes on the BOTTOM, matching 43's by-fuel figure.
    gg + facet_grid(biofuel_group ~ iso3c, scales = "free_y", switch = "both") +
    theme(strip.text.y.left = element_text(face = "bold", size = 10, angle = 90),
          panel.spacing.y   = unit(8, "pt"))
  else
    gg + facet_wrap(~ iso3c, nrow = 1, strip.position = "bottom")
}

# a panel has to fit ACCOUNTS bars: 15 countries x 3 bars is 45 bars on one row.
# Hold the bar DENSITY constant instead of the canvas width (the old 14in figure was
# 30 bars, i.e. ~0.45in each), and cap it -- past ~24in the labels are the problem,
# not the space. If the bars still come out too thin, lower TOP_N.
PLOT_W <- min(24, max(12, 2 + 0.45 * nrow(shown) * length(ACCOUNTS)))

p <- long[iso3c %in% shown$iso3c]
p[, iso3c := factor(iso3c, levels = shown$iso3c)]

out <- file.path(PLOT_DIR, sprintf("responsibility_trade_split_%s_%s_%d.svg",
                                   STAG, ATAG, YEAR))
ggsave(out,
       plot_split(p,
                  sprintf("%s responsibility for bio-based transport fuels, %d - where each account charges the impact",
                          meta$short_label, YEAR),
                  sprintf("%s\n%s", SUB, SEL_NOTE)),
       device = svglite::svglite, width = PLOT_W, height = 8)
message(">>> [44] wrote ", out)

# --- variant: the same bars, one row per biofuel chain -----------------------
# Same countries and same order as above, so a row can be read against the main
# figure country by country. The fuel row labels and their order match 43's
# by-fuel figure (pretty names; biogasoline -> biodiesel -> renewable diesel).
FUEL_LABELS <- c(biogasoline = "Biogasoline", biodiesel = "Biodiesel",
                 renewable_diesel = "Renewable diesel")   # keys in 43's row order
if (BY_BIOFUEL && uniqueN(h$biofuel_group) > 1) {
  db <- split_components(h, c("iso3c", "continent", "biofuel_group"))
  pb <- db$long[iso3c %in% shown$iso3c]
  pb[, iso3c := factor(iso3c, levels = shown$iso3c)]
  # pretty labels + 43's order; any unexpected chain is title-cased and appended
  raw  <- as.character(pb$biofuel_group)
  lab  <- ifelse(raw %in% names(FUEL_LABELS), unname(FUEL_LABELS[raw]),
                 tools::toTitleCase(gsub("_", " ", raw)))
  lvls <- c(unname(FUEL_LABELS[names(FUEL_LABELS) %in% raw]),
            sort(setdiff(unique(lab), unname(FUEL_LABELS))))
  pb[, biofuel_group := factor(lab, levels = lvls)]
  
  out_bf <- file.path(PLOT_DIR, sprintf("responsibility_trade_split_%s_%s_%d_by_biofuel.svg",
                                        STAG, ATAG, YEAR))
  ggsave(out_bf,
         plot_split(pb,
                    sprintf("%s responsibility, %d - by biofuel chain", meta$short_label, YEAR),
                    paste0(SUB, "\nSame countries and order as the main figure; ",
                           "the y axis is free PER CHAIN."),
                    by_biofuel = TRUE),
         device = svglite::svglite, width = PLOT_W + 1, height = 10.5)
  message(">>> [44] wrote ", out_bf)
}