# =============================================================================
# 45_plot_responsibility_trade_split.R
# ONE figure: the production- (PBA), consumption- (CBA), HDI justice-based and
# value added-based (VA) accounts as ADJACENT stacked bars per country, for
# PLOT_YEAR, each split by the ORIGIN x DESTINATION of the CHARGED impact flow --
# 19_01b's balance convention, carried over to the responsibility accounts.
#   x = country (order taken from 44) | y = impact | one bar per account
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
# producing country p, driven by final demand in consuming country c), which 41
# already computed. Per country i, the three physical flows are
#     domestic(i) = D[i, i]              domestic production, domestic consumption
#     export(i)   = sum_c!=i D[i, c]     domestic production, FOREIGN consumption
#     import(i)   = sum_p!=i D[p, i]     FOREIGN production, domestic consumption
#     PBA(i) = domestic + export ,  CBA(i) = domestic + import
# An account is a rule for how much of each flow a country is CHARGED (and only
# the charged part is drawn -- the rest is booked to the trade partner):
#
#   CBA   domestic charged in full; export booked to the consumer (not drawn);
#         import charged in full.
#   HDI   the two trade flows are SPLIT by relative HDI (Sun et al. 2022, eqs 1-3),
#         beta = HDI_p / (HDI_p + HDI_c): the producer keeps beta of its export,
#         the consumer keeps (1-beta) of its import. CBA is the beta = 0 corner.
#   PBA   domestic + export, both charged in full; nothing imported.
#   VA    a DIFFERENT matrix. 42 computes Pinero et al. (2019) eq. 8,
#         H* = v_hat B T' with T = f_hat B y_hat, rolled up to countries on both
#         axes: H*[r, p] is pressure extracted in p and charged to value added
#         generated in r. Its margins are
#             va_domestic(i) = H*[i, i]           extracted here, charged here
#             va_import(i)   = sum_p!=i H*[i, p]  extracted abroad, charged here
#             va_export(i)   = sum_r!=i H*[r, i]  extracted here, charged abroad
#             VA(i) = va_domestic + va_import = the row margin
#         Like CBA it charges none of its export, so its dark block is empty --
#         but its mid-grey block is H*[i, i], NOT the D[i, i] the other three bars
#         share, because the two matrices resolve "domestic" against different
#         second axes (consumer vs. value generator). Both trade blocks are also
#         about VALUE CAPTURE, not consumption. Do not read the VA bar as a fourth
#         slicing of D.
#
# Summed over ALL countries the CHARGED segments of each account reach the SAME
# global footprint -- the accounts re-attribute, never create. Checked below.
#
# NEGATIVE FLOWS -- why a country can have a MALFORMED bar ---------------------
# D[p, c] is not guaranteed non-negative: FABIO's Y carries negative components
# (stock change), so a country that produces none of a chain and draws it down
# from stock ends up with PBA = domestic = 0 and a NEGATIVE CBA -- the import
# residual then IS the whole account. It is not noise and not a 42 bug; it is what
# the underlying MRIO says. Pooling the chains usually cancels it, so it surfaces
# only per chain. NEG_FLOWS decides what such a row does to the figure:
#   clamp  (default) floors the flows at zero and REBUILDS the account totals from
#          the clamped components. A negative account is drawn as zero -- a lie,
#          but a bounded, reported one.
#   keep   touches nothing. Honest, but a negative flow dips a segment below zero.
#   drop   removes the flagged (country, chain) rows from the figure entirely.
# The console prints each flagged row's weight as a share of the world total:
# judge the relative violation against THAT before caring about it.
#
# READS  HDI_CSV (41) -- needs year, biofuel_group, iso3c, continent,
#          production_based, consumption_based, justice_based, justice_domestic,
#          justice_export, justice_import. Nothing is recomputed.
#        VA_SPLIT_CSV (42) -- needs year, va_variant, biofuel_group, iso3c,
#          va_based, va_domestic, va_import, va_export, va_production. Merged onto
#          41's rows as a FULL join (a pure value-capture hub can appear in one
#          file and not the other), filtered to VA_VARIANT.
#        ORDER_CSV (44) -- the country order to follow; if absent, this script
#          ranks its own.
# WRITES output/plot/
#   responsibility_trade_split_<ind>_<alloc>_<year>.svg
#   responsibility_trade_split_<ind>_<alloc>_<year>_by_biofuel.svg   (if BY_BIOFUEL)
#
# RUN: Rscript R/45_plot_responsibility_trade_split.R   (after 41 and 42, and 44 for order)
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

source("R/40_responsibility_shared.R")
SCRIPT <- "45"
run_banner()
message(sprintf(">>> [45] year = %d", PLOT_YEAR))

# --- figure switches ---------------------------------------------------------
GROUPS     <- NULL     # biofuel chains to pool; NULL = all in the CSV
SHOW_PBA   <- TRUE     # draw production-based as its own BAR: domestic + the whole
# export. Its height is the country's physical production,
# the reference the CBA and HDI bars move against.
BY_BIOFUEL <- TRUE     # also write the per-chain variant
NEG_FLOWS  <- "clamp"  # malformed rows (see the header): "clamp" | "keep" | "drop"

# Used only when 44's order file is absent (44 not yet run for this run).
TOP_N   <- 15
RANK_BY <- "max"       # "max" | "mean" | "CBA" | "HDI" | "PBA"

# --- load 41's responsibility CSV --------------------------------------------
h <- fread(need(HDI_CSV, "41"))
need_cols(h, c("year", "biofuel_group", "iso3c", "continent", "production_based",
               "consumption_based", "justice_based", "justice_domestic",
               "justice_export", "justice_import"), HDI_CSV)

if (!PLOT_YEAR %in% h$year)
  stop("[45] year ", PLOT_YEAR, " absent from ", basename(HDI_CSV),
       "; available: ", paste(sort(unique(h$year)), collapse = ", "))

h <- h[year == PLOT_YEAR]
if (!is.null(GROUPS)) {
  unknown <- setdiff(GROUPS, unique(h$biofuel_group))
  if (length(unknown)) stop("[45] biofuel_group(s) not in the CSV: ",
                            paste(unknown, collapse = ", "))
  h <- h[biofuel_group %in% GROUPS]
}
message(sprintf("[45] %d rows | chains pooled: %s", nrow(h),
                paste(sort(unique(h$biofuel_group)), collapse = " + ")))

# --- load 42's eq. 8 trade split and merge it onto 41's rows ------------------
# 41 keeps a country only when it produces or consumes the chain
# (prod_v != 0 | cons_v != 0). A pure value-capture hub can do neither and still
# carry VA responsibility, so this is a FULL join, not a left one -- an inner or
# left join would silently drop exactly the countries the VA account exists to
# surface. Countries new to the figure get zeros in the 41 columns, and vice
# versa; both are true statements about those accounts.
vs <- fread(need(VA_SPLIT_CSV, "42"))
need_cols(vs, c("year", "va_variant", "biofuel_group", "iso3c", "va_based",
                "va_domestic", "va_import", "va_export", "va_production"),
          VA_SPLIT_CSV)
vs <- vs[year == PLOT_YEAR & va_variant == VA_VARIANT]
if (!nrow(vs))
  stop("[45] ", basename(VA_SPLIT_CSV), " has no rows for year ", PLOT_YEAR,
       " / variant '", VA_VARIANT, "' -- re-run 42.")
if (!is.null(GROUPS)) vs <- vs[biofuel_group %in% GROUPS]

VA_COLS  <- c("va_based", "va_domestic", "va_import", "va_export")
KEY_COLS <- c("year", "biofuel_group", "iso3c")

only_va  <- setdiff(unique(vs$iso3c), unique(h$iso3c))
only_hdi <- setdiff(unique(h$iso3c),  unique(vs$iso3c))
if (length(only_va))
  message(sprintf("[45] %d country(ies) carry VA responsibility but no PBA/CBA row in 41 (added, zeros elsewhere): %s",
                  length(only_va), paste(only_va, collapse = ", ")))
if (length(only_hdi))
  message(sprintf("[45] %d country(ies) in 41 with no VA row (VA columns zeroed): %s",
                  length(only_hdi), paste(only_hdi, collapse = ", ")))

h <- merge(h, vs[, c(KEY_COLS, VA_COLS), with = FALSE], by = KEY_COLS, all = TRUE)
NUM <- c("production_based", "consumption_based", "justice_based", "justice_domestic",
         "justice_export", "justice_import", VA_COLS)
for (cc in NUM) set(h, which(is.na(h[[cc]])), cc, 0)
# `continent` rides along for the keys; fill it for countries only 42 knows about
if (anyNA(h$continent)) {
  reg <- tryCatch(fread("inst/regions_full.csv"), error = function(e) NULL)
  if (!is.null(reg) && all(c("iso3c", "continent") %in% names(reg)))
    h[is.na(continent), continent := reg$continent[match(iso3c, reg$iso3c)]]
  h[is.na(continent), continent := "Unknown"]
}

# --- the charged components --------------------------------------------------
# 41 writes `justice_export` / `justice_import` ALREADY HDI-WEIGHTED (the beta the
# producer keeps of its exports; the 1-beta the consumer keeps of its imports).
# The PHYSICAL flows are the residuals of the two totals:
#     export_full(i) = production_based(i)  - domestic(i)
#     import_full(i) = consumption_based(i) - domestic(i)
ACCOUNTS   <- c(if (SHOW_PBA) "PBA", "CBA", "HDI", "VA")
COMPONENTS <- c("domestic", "export_kept", "import_kept")   # each bar IS the charged account, all >= 0

# THE VA BAR IS NOT A FOURTH VIEW OF THE SAME THREE FLOWS ----------------------
# PBA, CBA and HDI are all margins of ONE matrix D[p, c] (producer x consumer),
# so their `domestic` segment is literally the same number, D[i, i], in all three
# bars and only the trade blocks move. The VA bar comes from a DIFFERENT matrix,
# 42's H*[r, p] (value generator x extraction origin, Pinero eq. 8), whose
# diagonal H[i, i] -- extracted in i, charged to value added in i -- is NOT
# D[i, i]. Its `domestic` segment therefore moves too, and mk() below takes the
# domestic block as an argument rather than reading one shared column.
# Its trade blocks also mean something else: `import_kept` is foreign extraction
# charged to DOMESTIC VALUE ADDED, not to domestic consumption, and va_export is
# extraction here booked to FOREIGN value added. Like CBA, VA charges none of its
# export, so its dark block is empty -- the VA argument lives in the other two
# segments, not in the shrinking export block that drives PBA -> HDI -> CBA.

split_components <- function(d, keys) {
  a <- d[, .(domestic    = sum(justice_domestic),
             export_full = sum(production_based)  - sum(justice_domestic),
             import_full = sum(consumption_based) - sum(justice_domestic),
             hdi_export  = sum(justice_export),
             hdi_import  = sum(justice_import),
             va_domestic = sum(va_domestic),
             va_import   = sum(va_import),
             va_export   = sum(va_export),
             PBA         = sum(production_based),
             CBA         = sum(consumption_based),
             HDI         = sum(justice_based),
             VA          = sum(va_based)),
         by = keys]
  
  # HDI's own columns must rebuild its total (41 writes them that way). Test this
  # on the RAW numbers, BEFORE anything below touches them: it is a check on 41,
  # and a clamp would hide it.
  dev_h <- max(abs(a$domestic + a$hdi_export + a$hdi_import - a$HDI)) / max(abs(a$HDI), 1)
  if (dev_h > NOISE_TOL)
    warning(sprintf("[45] justice_domestic+export+import != justice_based (max rel. %.3g)", dev_h))
  
  # Same test on 42's columns: the row margin of H* must be its own diagonal plus
  # its off-diagonal. Also raw, also before any clamp.
  dev_v <- max(abs(a$va_domestic + a$va_import - a$VA)) / max(abs(a$VA), 1)
  if (dev_v > NOISE_TOL)
    warning(sprintf("[45] va_domestic+va_import != va_based (max rel. %.3g) -- re-run 42", dev_v))
  
  # --- are the PHYSICAL flows well-formed? ------------------------------------
  # D[p, c] is NOT guaranteed non-negative (see the header), so an export or
  # import RESIDUAL can come out negative and an HDI share can then exceed the
  # flow it splits. Surface it with NUMBERS rather than a bare country name, and
  # judge each row against its OWN scale: an absolute tolerance is meaningless
  # when one country-chain is 1e4 and the next is 1e-9.
  s   <- pmax(abs(a$PBA), abs(a$CBA), .Machine$double.eps)     # each row's own scale
  chk <- data.table(a[, ..keys], scale = s,
                    neg_domestic = pmax(-a$domestic,    0) / s,
                    neg_export   = pmax(-a$export_full, 0) / s,
                    neg_import   = pmax(-a$import_full, 0) / s,
                    over_export  = pmax(a$hdi_export - a$export_full, 0) / s,
                    over_import  = pmax(a$hdi_import - a$import_full, 0) / s,
                    # VA has its own way of going negative, independent of D's:
                    # v = p/x is negative wherever value added is (net subsidies,
                    # negative operating surplus), so H* can carry negative cells
                    # even where every D entry is clean.
                    neg_va_dom   = pmax(-a$va_domestic, 0) / s,
                    neg_va_imp   = pmax(-a$va_import,   0) / s)
  chk[, worst := pmax(neg_domestic, neg_export, neg_import, over_export, over_import,
                      neg_va_dom, neg_va_imp)]
  chk[, who := if ("biofuel_group" %in% keys) paste0(iso3c, "/", biofuel_group) else iso3c]
  
  # A RELATIVE violation alone cannot be judged: "1.0 of its own scale" is what a
  # country whose whole account is one small NEGATIVE number looks like --
  # catastrophic as a ratio, invisible on the figure. So report the row's
  # absolute weight in the same breath.
  world <- sum(abs(a$CBA))
  chk[, share_of_world := scale / max(world, .Machine$double.eps)]
  # What actually reaches the figure is the PRODUCT of the two: a row that is 3e-4
  # of the world and violates by 2e-2 of itself distorts the world total by 6e-6,
  # which is orders of magnitude below a pixel. Ranking and thresholding on the
  # weight ALONE overstates the problem, because a row's weight is what it is
  # whether or not the clamp touches it. `distortion` is the honest number.
  chk[, distortion := worst * share_of_world]
  # Name the check that fired, so a VA negative (v = p/x < 0) is not mistaken for a
  # negative bilateral entry in D -- they have different causes and different fixes.
  CULPRITS <- c(neg_domestic = "domestic < 0",  neg_export = "export residual < 0",
                neg_import   = "import residual < 0", over_export = "HDI export > flow",
                over_import  = "HDI import > flow",   neg_va_dom  = "VA domestic < 0",
                neg_va_imp   = "VA import < 0")
  chk[, culprit := CULPRITS[max.col(as.matrix(.SD), ties.method = "first")],
      .SDcols = names(CULPRITS)]
  
  noise <- chk[worst > 0 & worst <= NOISE_TOL]
  if (nrow(noise))
    message(sprintf("[45] %d country-chain(s) off by <= %.0e of their own scale (float noise).",
                    nrow(noise), NOISE_TOL))
  
  bad <- chk[worst > NOISE_TOL][order(-distortion)]
  if (nrow(bad)) {
    drawn <- if (exists("shown", inherits = TRUE)) intersect(bad$iso3c, shown$iso3c) else NULL
    warning(sprintf(paste0("[45] %d country-chain(s) with a MALFORMED flow. D has negative bilateral ",
                           "entries there -- FABIO's Y carries negative components (stock change), so a ",
                           "country that produces none of a chain and draws it down from stock gets a ",
                           "NEGATIVE account -- or the VA intensity v = p/x is negative (net subsidies, ",
                           "negative operating surplus). NEG_FLOWS = '%s'. Ranked by the distortion each ",
                           "one puts on the WORLD total: %s"),
                    nrow(bad), NEG_FLOWS,
                    paste(sprintf("%s [%s] %.1e of the world (weight %.0e x violation %.1e)",
                                  head(bad$who, 5), head(bad$culprit, 5), head(bad$distortion, 5),
                                  head(bad$share_of_world, 5), head(bad$worst, 5)),
                          collapse = "; ")))
    tot_d <- sum(bad$distortion)
    message(sprintf("[45]   total distortion from the clamp: %.2g%% of the world total -- %s",
                    100 * tot_d,
                    if (tot_d < 1e-4)
                      "far below a pixel; whatever NEG_FLOWS does to them cannot move the figure."
                    else "big enough to see: check them before quoting any bar."))
    if (length(drawn))
      message("[45]   of these, DRAWN in the figure: ", paste(drawn, collapse = ", "),
              " -- the others are flagged but never plotted.")
    else if (!is.null(drawn))
      message("[45]   none of the flagged countries are among those drawn; the figure is untouched.")
  }
  
  if (NEG_FLOWS == "drop" && nrow(bad)) {
    a <- a[!bad[, ..keys], on = keys]
  } else if (NEG_FLOWS == "clamp") {
    a[, `:=`(domestic    = pmax(domestic,    0),
             export_full = pmax(export_full, 0),
             import_full = pmax(import_full, 0))]
    a[, `:=`(hdi_export  = pmin(pmax(hdi_export, 0), export_full),
             hdi_import  = pmin(pmax(hdi_import, 0), import_full))]
    a[, `:=`(va_domestic = pmax(va_domestic, 0),
             va_import   = pmax(va_import,   0),
             va_export   = pmax(va_export,   0))]
    a[, `:=`(PBA = domestic + export_full,
             CBA = domestic + import_full,
             HDI = domestic + hdi_export + hdi_import,
             VA  = va_domestic + va_import)]
  } else if (NEG_FLOWS != "keep") {
    stop("[45] NEG_FLOWS must be 'clamp', 'keep' or 'drop'.")
  }
  
  # Each bar is the account's charged impact, split by where the flow goes: PBA
  # keeps its whole export; CBA cedes it entirely (export_kept = 0); HDI keeps the
  # beta share. What is ceded is not drawn -- the bar height is the account.
  # `dom` is explicit: the VA bar's domestic block is H*[i, i], not D[i, i]
  # (see the note at ACCOUNTS above).
  mk <- function(acc, dom, kept_exp, kept_imp)
    data.table(a[, ..keys], account = acc,
               domestic    = dom,
               export_kept = kept_exp,
               import_kept = kept_imp)
  
  parts <- list(
    mk("CBA", dom = a$domestic,    kept_exp = 0,            kept_imp = a$import_full),
    mk("HDI", dom = a$domestic,    kept_exp = a$hdi_export, kept_imp = a$hdi_import),
    mk("VA",  dom = a$va_domestic, kept_exp = 0,            kept_imp = a$va_import)
  )
  if (SHOW_PBA)
    parts <- c(list(mk("PBA", dom = a$domestic, kept_exp = a$export_full, kept_imp = 0)), parts)
  
  long <- melt(rbindlist(parts, use.names = TRUE),
               id.vars       = c(keys, "account"),
               measure.vars  = COMPONENTS,
               variable.name = "component", value.name = "value")
  
  # --- the identity the whole figure rests on ---------------------------------
  # charged = domestic + export kept + import kept. Derive it from THOSE
  # components, not from "every segment above zero": a country with no positive
  # segment in an account would drop out of the comparison entirely. MERGE on the
  # keys -- never compare by position, since the two are not in the same order the
  # moment a country-chain has nothing charged under CBA, which is exactly what a
  # pure exporter looks like once the chains are split.
  chg <- long[component %in% COMPONENTS,
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
      warning(sprintf("[45] %s: the charged segments do not sum to the account (max rel. %.3g)",
                      acc, dev))
  }
  
  long[, component := factor(component, levels = COMPONENTS)]
  long[, account   := factor(account,   levels = ACCOUNTS)]
  
  list(long = long, wide = a)
}

d    <- split_components(h, c("iso3c", "continent"))
long <- d$long
wide <- d$wide

# --- conservation & context --------------------------------------------------
tot <- long[, .(charged = sum(value)), by = account]
# PBA/CBA/HDI are re-attributions of ONE total, so they get the tight bound. VA is
# not: 42 drops the chains with s_j <= 0 (no valued sector to carry them) and
# reports the shortfall as conservation_gap_pct, so a small VA deficit is a known
# property of the account, not a mismatched pair of files. Hence VA_GAP_TOL, and
# hence the two checks are separate -- folding VA into the first would either
# raise the bound for everyone or cry wolf every run.
d3  <- tot[account != "VA"]
if (nrow(d3) > 1 && (max(d3$charged) - min(d3$charged)) / max(abs(d3$charged)) > TOL)
  warning("[45] the D-based accounts do not share one total:\n",
          paste(sprintf("  %-4s %.6g", d3$account, d3$charged), collapse = "\n"))
va_gap <- (tot[account == "VA", charged] - d3[account == "CBA", charged]) /
  max(abs(d3[account == "CBA", charged]), .Machine$double.eps)
if (abs(va_gap) > VA_GAP_TOL)
  warning(sprintf(paste0("[45] the VA account is %.3g%% off the consumer footprint it re-allocates. ",
                         "Small gaps are 42's s_j <= 0 chains; a gap this size will visibly bend the ",
                         "VA bars -- check conservation_gap_pct in 42's coverage CSV before quoting them."),
                  100 * va_gap))
message("[45] charged totals (", META$short_label, ", ", PLOT_YEAR, "): ",
        paste(sprintf("%s %.6g", tot$account, tot$charged), collapse = " | "),
        sprintf("  [VA gap vs CBA: %+.3g%%]", 100 * va_gap))

# world exports must equal world imports: every traded flow is somebody's both
we <- sum(wide$export_full); wi <- sum(wide$import_full)
if (abs(we - wi) / max(abs(we), 1) > TOL)
  warning(sprintf("[45] world exports (%.6g) != world imports (%.6g)", we, wi))
message(sprintf("[45] traded share of the footprint: %.1f%% (the rest never crosses a border)",
                100 * we / max(sum(wide$CBA), .Machine$double.eps)))

# The same identity on the VA axis: every unit of pressure booked to foreign value
# added is somebody's imported-into-VA. This is H*'s off-diagonal counted twice,
# by row and by column, so it is a real test of 42's eq. 8 rather than a tautology.
ve <- sum(wide$va_export); vi <- sum(wide$va_import)
if (abs(ve - vi) / max(abs(ve), 1) > TOL)
  warning(sprintf("[45] VA: world exports (%.6g) != world imports (%.6g) -- H* margins disagree", ve, vi))
message(sprintf("[45] share of the VA account resting on FOREIGN extraction: %.1f%%",
                100 * vi / max(sum(wide$VA), .Machine$double.eps)))

# --- countries: follow 44's value-added figure -------------------------------
# The set and its left-to-right order are read from 44's order file so this figure
# shows the SAME countries in the SAME order. 44 keeps VA in its selection, which
# pulls in value-capturing hubs (NLD) that a PBA/CBA-only ranking drops; HDI stays
# inside the PBA/CBA envelope, so inheriting 44's set loses nothing here. Only
# when that file is absent do we fall back to our own charged-total ranking.
ctot <- dcast(long[, .(value = sum(value)), by = .(iso3c, account)],
              iso3c ~ account, value.var = "value", fill = 0)

from_44 <- file.exists(ORDER_CSV)
if (from_44) {
  ord <- fread(ORDER_CSV)
  if (!all(c("position", "iso3c") %in% names(ord)))
    stop("[45] ", basename(ORDER_CSV), " lacks 'position'/'iso3c' -- re-run 44.")
  setorder(ord, position)
  missing <- setdiff(ord$iso3c, ctot$iso3c)   # 44's set but no charged rows in this CSV
  if (length(missing))
    message("[45] ", length(missing), " of 44's countries absent from the HDI CSV, dropped: ",
            paste(missing, collapse = ", "))
  keep  <- ord$iso3c[ord$iso3c %in% ctot$iso3c]
  shown <- ctot[match(keep, ctot$iso3c)]      # rows in 44's order; carries PBA/CBA/HDI
  message(sprintf("[45] country set + order FOLLOW 44 (%s): %s",
                  basename(ORDER_CSV), paste(shown$iso3c, collapse = " ")))
} else {
  # Rank on the CHARGED (positive) total, so a pure exporter still enters on its
  # HDI bar even though CBA charges it little.
  message("[45] no 44 order file (", basename(ORDER_CSV),
          ") -- falling back to this script's own top-", TOP_N, " '", RANK_BY, "' ranking.")
  ctot[, rank_by := switch(RANK_BY,
                           max  = do.call(pmax, as.list(ctot[, ..ACCOUNTS])),
                           mean = rowMeans(as.matrix(ctot[, ..ACCOUNTS])),
                           ctot[[RANK_BY]])]
  setorder(ctot, -rank_by)
  shown <- ctot[seq_len(min(TOP_N, .N))]
}
message(sprintf("[45] %d countries shown; they cover %s of the world charged total.", nrow(shown),
                paste(sprintf("%s %.0f%%", ACCOUNTS,
                              100 * colSums(shown[, ..ACCOUNTS]) / colSums(ctot[, ..ACCOUNTS])),
                      collapse = " | ")))

# Braces are load-bearing: at TOP LEVEL R closes an `if` as soon as its branch
# ends, so a bare `else` on the next line is a parse error when the file is
# echoed line by line (console, Ctrl+Enter). Inside {} the parser knows more is
# coming. Same reason the `else`s further down sit inside a call or a function.
SEL_NOTE <- if (from_44) {
  sprintf("Countries and their order follow the value-added figure (44, %s base).", VA_BASE)
} else {
  sprintf("Top %d countries by %s charged account (44 order file absent).", nrow(shown), RANK_BY)
}

# --- plot --------------------------------------------------------------------
# One panel per country, strips BELOW the axis: a nested "account within country"
# axis, so the bars sit side by side without any manual x-offset arithmetic.
# Black-and-white: three well-separated greys, since three categories read cleanly
# by lightness alone. The EXPORT block is the darkest on purpose -- its shrinking
# from PBA to HDI to CBA is the figure's whole point, so it stays the loudest mark.
flow_colors <- c(domestic    = "grey52",
                 export_kept = "grey25",
                 import_kept = "grey78")
# "destination" means the party the account CHARGES: the consumer for PBA/CBA/HDI,
# the value generator for VA. The labels are deliberately neutral about which, so
# they stay true across all four bars; the subtitle spells the difference out.
flow_labels <- c(domestic    = "Extracted here \u2192 charged here",
                 export_kept = "Extracted here \u2192 charged here (exported)",
                 import_kept = "Extracted abroad \u2192 charged here")

SUB <- paste0(
  if (SHOW_PBA) "PBA production-based | " else "",
  "CBA consumption-based | HDI justice-based (Sun et al. 2022) | VA value added-based ",
  "(Pi\u00f1ero et al. 2019, eq. 8): re-attributions of one and the same total.\n",
  "Every bar is the SAME country; only the charging rule moves, so the bar HEIGHT is that ",
  "account. Segments show where the charged impact was EXTRACTED (see legend):\ndark = extracted here for export, ",
  "mid-grey = extracted and charged here, light = extracted abroad. ",
  if (SHOW_PBA)
    paste0("PBA charges the whole dark export block; CBA charges none of it (it vanishes); ",
           "HDI keeps HDI_p/(HDI_p+HDI_c) of it.")
  else
    paste0("CBA charges none of the dark export block; HDI keeps HDI_p/(HDI_p+HDI_c) of it."),
  "\nThe first three bars split ONE matrix D[producer, consumer], so their mid-grey block is ",
  "the same number in each. VA splits a DIFFERENT matrix, H*[value generator, extraction ",
  "origin],\nso its mid-grey block moves too, and its light block is extraction abroad charged ",
  "to value added here -- not to consumption here. Like CBA, VA charges none of its export.")

plot_split <- function(p, title, subtitle, by_biofuel = FALSE) {
  gg <- ggplot(p, aes(x = account, y = value / META$scale_factor, fill = component)) +
    geom_col(position = position_stack(reverse = TRUE),   # domestic against the zero line
             width = 0.8, colour = "white", linewidth = 0.2) +
    scale_fill_manual(values = flow_colors, labels = flow_labels, drop = FALSE,
                      name = "Impact flow (origin \u2192 destination)") +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    labs(x = NULL, y = META$y_label, title = title, subtitle = subtitle,
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
    # axis would flatten the small ones. switch = "both" puts the fuel labels on
    # the LEFT (rotated) and the country codes on the BOTTOM, matching 44's
    # by-fuel figure.
    gg + facet_grid(biofuel_group ~ iso3c, scales = "free_y", switch = "both") +
    theme(strip.text.y.left = element_text(face = "bold", size = 10, angle = 90),
          panel.spacing.y   = unit(8, "pt"))
  else
    gg + facet_wrap(~ iso3c, nrow = 1, strip.position = "bottom")
}

# a panel has to fit ACCOUNTS bars: 15 countries x 3 bars is 45 bars on one row.
# Hold the bar DENSITY constant instead of the canvas width (~0.45in per bar) and
# cap it -- past ~24in the labels are the problem, not the space. If the bars come
# out too thin, lower TOP_N.
PLOT_W <- min(24, max(12, 2 + 0.45 * nrow(shown) * length(ACCOUNTS)))

p <- long[iso3c %in% shown$iso3c]
p[, iso3c := factor(iso3c, levels = shown$iso3c)]

save_svg(sprintf("responsibility_trade_split_%s_%s_%d", STAG, ATAG, PLOT_YEAR),
         plot_split(p,
                    sprintf("%s responsibility for bio-based transport fuels, %d - where each account charges the impact",
                            META$short_label, PLOT_YEAR),
                    sprintf("%s\n%s", SUB, SEL_NOTE)),
         width = PLOT_W, height = 8)

# --- variant: the same bars, one row per biofuel chain -----------------------
# Same countries and same order as above, so a row can be read against the main
# figure country by country. The fuel row labels and their order match 44's.
if (BY_BIOFUEL && uniqueN(h$biofuel_group) > 1) {
  db <- split_components(h, c("iso3c", "continent", "biofuel_group"))
  pb <- db$long[iso3c %in% shown$iso3c]
  pb[, iso3c := factor(iso3c, levels = shown$iso3c)]
  # pretty labels; any unexpected chain is title-cased and appended
  raw  <- as.character(pb$biofuel_group)
  lab  <- ifelse(raw %in% names(biofuel_label), unname(biofuel_label[raw]),
                 tools::toTitleCase(gsub("_", " ", raw)))
  lvls <- c(unname(biofuel_label[names(biofuel_label) %in% raw]),
            sort(setdiff(unique(lab), unname(biofuel_label))))
  pb[, biofuel_group := factor(lab, levels = lvls)]
  
  save_svg(sprintf("responsibility_trade_split_%s_%s_%d_by_biofuel", STAG, ATAG, PLOT_YEAR),
           plot_split(pb,
                      sprintf("%s responsibility, %d - by biofuel chain", META$short_label, PLOT_YEAR),
                      paste0(SUB, "\nSame countries and order as the main figure; ",
                             "the y axis is free PER CHAIN."),
                      by_biofuel = TRUE),
           width = PLOT_W + 1, height = 10.5)
}