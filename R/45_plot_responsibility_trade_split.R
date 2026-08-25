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
#        ORDER_CSV (44) -- the country SETS and their order. 44 writes one
#          block of rows per set (`country_set` / `set_tag` columns: the 5
#          largest producers, and the 7 largest EU member states plus Belgium,
#          which 44 forces in by must_include), and this script
#          writes one figure per set it finds there. An order file from before
#          the split -- no `country_set` column -- is read as a single set named
#          "all", so the old artefact still works. If the file is absent entirely, this script
#          ranks its own single set, as before.
#          44 can also draw CONTINENT panels; it does not write those sets here,
#          and this script additionally filters on the `unit` column, so a region
#          cannot reach a by-country figure from either direction. A file without
#          that column is read as all-country, as before.
# WRITES output/plot/
#   responsibility_trade_split_<set>_<ind>_<alloc>_<year>.svg
#     ONE figure per country set: facet_grid(biofuel_group ~ iso3c), with 40's
#     pooled `total` chain as the top row and the three chains beneath it. There
#     used to be a second `_by_biofuel` file; the pool is now a row in this one,
#     built by handing the country table to with_total() before it is aggregated,
#     so the top row is arithmetically the figure that file used to be.
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
NEG_FLOWS  <- "clamp"  # malformed rows (see the header): "clamp" | "keep" | "drop"

# Used only when 44's order file is absent (44 not yet run for this run).
TOP_N   <- 15
RANK_BY <- "max"       # "max" | "mean" | "CBA" | "HDI" | "PBA"

# --- type size and furniture -------------------------------------------------
# One number for the type; everything in the theme is relative to it. 11pt was
# sized for a wide standalone SVG and lands at ~5pt on a document page. The
# eight-panel EU set is read at PANEL width, not at canvas width, so the value
# matches 44's: the two figures are meant to be lined up column-for-column, and
# type that disagreed between them would break that reading first.
BASE_SIZE <- 16

# Title and subtitle off the CANVAS, not out of the record: with SHOW_TITLES =
# FALSE the text is written to captions_*.txt beside the SVGs, one block per
# figure. This figure's subtitle is the one that must not be lost -- it carries
# the PROVENANCE_NOTE (this indicator's damage is NOT where the production is)
# and the statement that the bar height is the account, not a stack of flows.
SHOW_TITLES  <- TRUE
SHOW_CAPTION <- TRUE

# --- separating the countries visually ---------------------------------------
# Four bars per country and nothing but a 3pt gap at the panel boundary: the
# failure mode is reading a country's VA bar as the next country's PBA bar, which
# inverts the figure's argument (the export block SHRINKS left to right within a
# country; across a boundary it jumps back up). Same devices and same defaults as
# 44, so the two figures separate their countries identically.
#   "border" a rule around each country panel | "band" alternating tint |
#   "both"   (default) | "none" the old look
SEPARATOR  <- "border"   # the rule alone; "band"/"both" restore the tint
SEP_COLOUR <- "grey89"   # ABOVE every fill in lightness -- see flow_colors below
BAND_FILL  <- "grey96"   # only used when SEPARATOR includes "band"
PANEL_GAP  <- 5          # pt between country panels (was 3)

# NA = no line between stacked segments. Safe here: the three flow greys are far
# apart in lightness (grey25 / grey52 / grey78) and the stack never puts the two
# closest of them next to each other.
SEG_BORDER     <- NA     # e.g. "white" to restore it
SEG_BORDER_LWD <- 0.2

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
# COMPONENTS (domestic / export_kept / import_kept -- each bar IS the charged
# account, all >= 0) is 40's, shared with 48.

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
    # `all_shown` is the union of the country sets; it does not exist yet on the
    # first call (the sets are picked from these very numbers), so the first
    # report lists no "drawn" countries and the by-chain call below does.
    drawn <- if (exists("all_shown", inherits = TRUE)) intersect(bad$iso3c, all_shown) else NULL
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

# ONE decomposition, at chain resolution, with 40's `total` chain carried along.
# This used to be two calls -- one pooled here, one per chain at the figure loop
# -- which meant split_components() ran twice and printed its malformed-flow
# report twice. Pooling is now just the `total` key: with_total() duplicates each
# country row under it and the `by = keys` sum inside split_components() does the
# rest, so the Total panel is the same arithmetic the pooled call used to do.
#
# The NEG_FLOWS clamp is why the key has to go in BEFORE the sum rather than the
# chains being added up afterwards: the clamp is applied to `a` AFTER the
# aggregation, so clamping the pool is not the same as summing three clamped
# chains. Keeping `total` as a key reproduces the first, which is what the pooled
# figure always showed.
d    <- split_components(with_total(h), c("iso3c", "continent", "biofuel_group"))
long <- d$long                                  # Total + the three chains
wide <- d$wide

# Every conservation check and every country ranking below is a statement about
# the WHOLE footprint, so each reads the pooled rows only -- summing `long` as it
# stands would count the world twice, once as Total and once as three chains.
long_tot <- long[biofuel_group == TOTAL_KEY]
wide_tot <- wide[biofuel_group == TOTAL_KEY]
if (!nrow(long_tot))
  stop("[45] with_total() produced no '", TOTAL_KEY, "' rows -- 40 and 45 disagree.")

# --- conservation & context --------------------------------------------------
tot <- long_tot[, .(charged = sum(value)), by = account]
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
we <- sum(wide_tot$export_full); wi <- sum(wide_tot$import_full)
if (abs(we - wi) / max(abs(we), 1) > TOL)
  warning(sprintf("[45] world exports (%.6g) != world imports (%.6g)", we, wi))
message(sprintf("[45] traded share of the footprint: %.1f%% (the rest never crosses a border)",
                100 * we / max(sum(wide_tot$CBA), .Machine$double.eps)))

# The same identity on the VA axis: every unit of pressure booked to foreign value
# added is somebody's imported-into-VA. This is H*'s off-diagonal counted twice,
# by row and by column, so it is a real test of 42's eq. 8 rather than a tautology.
ve <- sum(wide_tot$va_export); vi <- sum(wide_tot$va_import)
if (abs(ve - vi) / max(abs(ve), 1) > TOL)
  warning(sprintf("[45] VA: world exports (%.6g) != world imports (%.6g) -- H* margins disagree", ve, vi))
message(sprintf("[45] share of the VA account resting on FOREIGN extraction: %.1f%%",
                100 * vi / max(sum(wide_tot$VA), .Machine$double.eps)))

# --- countries: follow 44's value-added figure -------------------------------
# The set and its left-to-right order are read from 44's order file so this figure
# shows the SAME countries in the SAME order. 44 keeps VA in its selection, which
# pulls in value-capturing hubs (NLD) that a PBA/CBA-only ranking drops; HDI stays
# inside the PBA/CBA envelope, so inheriting 44's set loses nothing here. Only
# when that file is absent do we fall back to our own charged-total ranking.
ctot <- dcast(long_tot[, .(value = sum(value)), by = .(iso3c, account)],
              iso3c ~ account, value.var = "value", fill = 0)

# 44 writes one BLOCK OF ROWS PER SET, keyed by `country_set` / `set_tag`, and this script
# writes one figure per set. Looping over whatever is in the file rather than over
# a list defined here is deliberate: the sets are a statement about which
# countries are worth showing, 44 owns that statement, and duplicating it here is
# how the two figures would drift apart.
from_44 <- file.exists(ORDER_CSV)
if (from_44) {
  ord <- fread(ORDER_CSV)
  if (!all(c("position", "iso3c") %in% names(ord)))
    stop("[45] ", basename(ORDER_CSV), " lacks 'position'/'iso3c' -- re-run 44.")
  # Backwards compatible with an order file written before 44 was split.
  if (!"country_set" %in% names(ord)) ord[, country_set := "all"]
  if (!"set_tag"     %in% names(ord)) ord[, set_tag     := country_set]
  # 44 draws its panels at COUNTRY or CONTINENT resolution and writes only the
  # country sets here, so this filter should never remove anything. It is kept
  # because the failure it guards against is silent-ish and expensive: a
  # continent name in the `iso3c` column drops out of the join against the HDI
  # CSV below and the set aborts with "none of 44's countries appear in ...",
  # which reads like a broken pipeline rather than a units mismatch. Defaulting a
  # missing column to "country" keeps pre-unit order files working.
  if (!"unit" %in% names(ord)) ord[, unit := "country"]
  n_other <- ord[unit != "country", .N]
  if (n_other) {
    message("[45] ", basename(ORDER_CSV), ": ignoring ", n_other,
            " row(s) from non-country set(s) (",
            paste(unique(ord[unit != "country", country_set]), collapse = ", "),
            ") -- this figure is by country. 44 draws those.")
    ord <- ord[unit == "country"]
  }
  if (!nrow(ord))
    stop("[45] ", basename(ORDER_CSV), " holds no country-keyed set -- ",
         "re-run 44 with at least one set whose unit is 'country'.")
  set_names <- unique(ord$country_set)          # file order, not alphabetical
  sets <- lapply(set_names, function(s) {
    o <- ord[country_set == s][order(position)]
    miss <- setdiff(o$iso3c, ctot$iso3c)   # in 44's set, no charged rows in this CSV
    if (length(miss))
      message("[45] set '", s, "': ", length(miss),
              " of 44's countries absent from the HDI CSV, dropped: ",
              paste(miss, collapse = ", "))
    keep <- o$iso3c[o$iso3c %in% ctot$iso3c]
    if (!length(keep))
      stop("[45] set '", s, "': none of 44's countries appear in ", basename(HDI_CSV), ".")
    list(tag   = as.character(o$set_tag[1]),
         name  = s,
         shown = ctot[match(keep, ctot$iso3c)])   # 44's order; carries PBA/CBA/HDI/VA
  })
  names(sets) <- set_names
  for (s in set_names)
    message(sprintf("[45] set '%s' FOLLOWS 44 (%s): %s",
                    s, basename(ORDER_CSV), paste(sets[[s]]$shown$iso3c, collapse = " ")))
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
  sets <- list(top = list(tag   = sprintf("top%d", TOP_N),
                          name  = "top",
                          shown = ctot[seq_len(min(TOP_N, .N))]))
}

for (s in names(sets))
  message(sprintf("[45] set '%s': %d countries; they cover %s of the world charged total.",
                  s, nrow(sets[[s]]$shown),
                  paste(sprintf("%s %.0f%%", ACCOUNTS,
                                100 * colSums(sets[[s]]$shown[, ..ACCOUNTS]) /
                                  colSums(ctot[, ..ACCOUNTS])),
                        collapse = " | ")))

# The union of the sets, for the malformed-flow report below: it names the
# flagged countries that are actually DRAWN, and "drawn" now means "drawn in any
# of the figures".
all_shown <- unique(unlist(lapply(sets, function(z) as.character(z$shown$iso3c)),
                           use.names = FALSE))

# Braces are load-bearing: at TOP LEVEL R closes an `if` as soon as its branch
# ends, so a bare `else` on the next line is a parse error when the file is
# echoed line by line (console, Ctrl+Enter). Inside {} the parser knows more is
# coming. Same reason the `else`s further down sit inside a call or a function.
SEL_NOTE <- function(z) {
  if (from_44) {
    sprintf("The %d countries and their order follow the value-added figure (44, %s base), set '%s'.",
            nrow(z$shown), VA_BASE, z$name)
  } else {
    sprintf("Top %d countries by %s charged account (44 order file absent).",
            nrow(z$shown), RANK_BY)
  }
}

# The chains, resolved ONCE while `h` is unambiguously the data.table. plot_split
# used to reach for the global `h` at draw time, which made it hostage to
# whatever else had been assigned that name by then.
CHAIN_NOTE <- paste(sort(unique(h$biofuel_group)), collapse = " + ")

# --- plot --------------------------------------------------------------------
# One panel per country, strips BELOW the axis: a nested "account within country"
# axis, so the bars sit side by side without any manual x-offset arithmetic.
# Black-and-white: three greys on equal perceptual steps, since three categories
# read cleanly by lightness alone. The EXPORT block is the darkest on purpose --
# its shrinking from PBA to HDI to CBA is the figure's whole point, so it stays
# the loudest mark. Exact values and their separations are tabulated below.
# THE GREYS AND THE FLOW LABELS LIVE IN 40 -----------------------------------
# flow_colors (three fills on equal CIE L* steps, with the panel rule pushed out
# of the fill band), flow_labels_for(), NONLOCAL_PATHWAYS and provenance_note()
# moved there when 48 became the third script to need them. The rationale for
# each grey, and the caveat about where an indicator's damage actually lands,
# travelled with them.
#   45 draws COUNTRIES, so its "here" is a country; 48 passes "region".
FLOW_LABS       <- flow_labels_for("country")
PROVENANCE_NOTE <- provenance_note()

SUB <- paste0(
  if (SHOW_PBA) "PBA production-based | " else "",
  "CBA consumption-based | HDI justice-based (Sun et al. 2022) | VA value added-based ",
  "(Pi\u00f1ero et al. 2019, eq. 8): re-attributions of one and the same total.\n",
  "Every bar is the SAME country; only the charging rule moves, so the bar HEIGHT is that ",
  "account. Segments locate the PRODUCTION the accounted-for impact is attributed to (see legend):\ndark = produced here for export, ",
  "mid-grey = produced and accounted for here, light = produced abroad. ",
  if (SHOW_PBA)
    paste0("PBA charges the whole dark export block; CBA charges none of it (it vanishes); ",
           "HDI keeps HDI_p/(HDI_p+HDI_c) of it.")
  else
    paste0("CBA charges none of the dark export block; HDI keeps HDI_p/(HDI_p+HDI_c) of it."),
  "\nThe first three bars split ONE matrix D[producer, consumer], so their mid-grey block is ",
  "the same number in each. VA splits a DIFFERENT matrix, H*[value generator, extraction ",
  "origin],\nso its mid-grey block moves too, and its light block is production abroad charged ",
  "to value added here -- not to consumption here. Like CBA, VA charges none of its export.",
  PROVENANCE_NOTE)

# The alternating background band is 40's panel_bands(), parameterised by the
# faceting column -- "iso3c" here, "continent" in 48. Same behaviour as the
# country_bands() that used to sit here.

# The title-block helpers (wrap_lines / text_height_in / lab_block /
# write_captions) live in 40 as well. The note that used to sit here said they
# belonged beside save_svg once a THIRD script needed them; 48 is that script.
# lab_block() now takes SHOW_TITLES as an ARGUMENT rather than reading it off
# the global environment at call time -- it is defined in another file now, and
# a silently inherited switch is exactly how a caption file would end up
# describing the wrong figure.

# legend_nrow: three keys in ONE row was safe at 11pt on a 24in canvas. At
# BASE_SIZE 16 on a ~13in canvas the labels ("Production here, exported ->
# accounted for here") no longer fit side by side, so the row count follows the
# width.
plot_split <- function(p, title, subtitle, legend_nrow = 1L) {
  gg <- ggplot(p, aes(x = account, y = value / META$scale_factor, fill = component)) +
    panel_bands(p, "iso3c", SEPARATOR, BAND_FILL) +        # BENEATH the bars
    geom_col(position = position_stack(reverse = TRUE),   # domestic against the zero line
             width = 0.8, colour = SEG_BORDER, linewidth = SEG_BORDER_LWD) +
    scale_fill_manual(values = flow_colors, labels = FLOW_LABS, drop = FALSE,
                      name = NULL) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    guides(fill = guide_legend(nrow = legend_nrow, byrow = TRUE)) +
    labs(x = NULL, y = META$y_label, title = title, subtitle = subtitle,
         caption = sprintf("%s allocation; %s.", allocation, CHAIN_NOTE)) +
    theme_minimal(base_size = BASE_SIZE) +
    theme(legend.position    = "bottom",
          legend.key.size    = unit(BASE_SIZE - 1, "pt"),
          legend.text        = element_text(size = BASE_SIZE - 3),
          legend.title       = element_blank(),
          axis.title         = element_text(size = BASE_SIZE - 1),
          axis.text.y        = element_text(size = BASE_SIZE - 3),
          strip.placement    = "outside",
          strip.background   = element_blank(),
          strip.text         = element_text(face = "bold", size = BASE_SIZE - 1),
          panel.border       = if (SEPARATOR %in% c("border", "both"))
            element_rect(fill = NA, colour = SEP_COLOUR, linewidth = 0.3)
          else element_blank(),
          panel.spacing.x    = unit(PANEL_GAP, "pt"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank(),
          axis.text.x        = element_text(size = BASE_SIZE - 3, angle = 90,
                                            hjust = 1, vjust = 0.5),
          plot.title         = element_text(size = BASE_SIZE + 2, face = "bold"),
          plot.subtitle      = element_text(size = BASE_SIZE - 4, colour = "grey30"),
          plot.caption       = element_text(size = BASE_SIZE - 5, colour = "grey40"))
  # Rows are free: the Total row is the sum of the three chains and they already
  # differ by an order of magnitude between themselves, so a shared y axis would
  # flatten everything below it. switch = "both" puts the chain labels on the
  # LEFT (rotated) and the country codes on the BOTTOM, matching 44's figure.
  gg + facet_grid(biofuel_group ~ iso3c, scales = "free_y", switch = "both") +
    theme(strip.text.y.left = element_text(face = "bold", size = BASE_SIZE, angle = 90),
          panel.spacing.y   = unit(8, "pt"))
}

# --- the figures: one per country set ----------------------------------------
# pretty chain labels; `total` is in biofuel_label, so it leads the levels and
# lands in the top row. Any unexpected chain is title-cased and appended.
raw  <- as.character(long$biofuel_group)
lab  <- ifelse(raw %in% names(biofuel_label), unname(biofuel_label[raw]),
               tools::toTitleCase(gsub("_", " ", raw)))
lvls <- c(unname(biofuel_label[names(biofuel_label) %in% raw]),
          sort(setdiff(unique(lab), unname(biofuel_label))))
long[, biofuel_group := factor(lab, levels = lvls)]

caps <- character(0)
for (s in names(sets)) {
  z     <- sets[[s]]
  iso   <- as.character(z$shown$iso3c)
  
  # a panel has to fit length(ACCOUNTS) bars: 15 countries x 4 bars is 60 bars on
  # one row. Hold the bar DENSITY constant instead of the canvas width (~0.45in
  # per bar) and cap it -- past ~24in the labels are the problem, not the space.
  # The FLOOR is what matters now that the sets are 5-7 countries: the subtitle is
  # six lines of fixed-length text and at BASE_SIZE - 4 needs ~14in whatever the
  # country count is, so below that it wraps mid-clause. Wide canvas + few panels
  # = fat bars; that is the trade for panels wide enough to label.
  # With the titles off only the legend needs room -- its three flow labels come
  # to ~13in side by side at BASE_SIZE - 3 -- so the floor drops to 13 and the
  # legend wraps to two rows below that.
  PLOT_W <- min(24, max(if (SHOW_TITLES) 14 else 13,
                        2 + 0.45 * length(iso) * length(ACCOUNTS)))
  LEG_N  <- if (PLOT_W >= 13) 1L else 2L
  
  p <- long[iso3c %in% iso]
  p[, iso3c := factor(iso3c, levels = iso)]
  
  # DRAWING area; the title block is measured and added below. One row per chain
  # plus the pooled row, so the canvas follows the grid rather than the old fixed
  # 6.8in (pooled) / 9.3in (three chains).
  PLOT_H <- 2.4 + 2.3 * max(uniqueN(p$biofuel_group), 1)   # 4 rows -> 11.6in
  
  ttl <- sprintf("%s responsibility for bio-based transport fuels, %d - where each account charges the impact",
                 META$short_label, PLOT_YEAR)
  sub <- sprintf("%s\nTop row is all three chains pooled; the y axis is FREE PER ROW, so bar heights compare within a row only.\n%s",
                 SUB, SEL_NOTE(z))
  stem <- sprintf("responsibility_trade_split_%s_%s_%s_%d", z$tag, STAG, ATAG, PLOT_YEAR)
  caps <- c(caps, lab_block(stem, ttl, sub, SHOW_TITLES))   # the caption file gets it UNWRAPPED
  
  ttlw <- wrap_lines(ttl, PLOT_W, BASE_SIZE + 2)
  subw <- wrap_lines(sub, PLOT_W, BASE_SIZE - 4)
  # NOT `h`: this loop runs at TOP LEVEL, so a bare `h` overwrites the data.table
  # `h` in the global environment, and plot_split() -- defined at top level, so
  # closing over globalenv -- then reads a number where it expects the table.
  fig_h <- PLOT_H
  if (SHOW_TITLES)
    fig_h <- fig_h + text_height_in(ttlw, BASE_SIZE + 2) +
    text_height_in(subw, BASE_SIZE - 4) + 0.25
  
  save_svg(stem,
           plot_split(p,
                      if (SHOW_TITLES) ttlw else NULL,
                      if (SHOW_TITLES) subw else NULL,
                      legend_nrow = LEG_N),
           width = PLOT_W, height = fig_h)
}

write_captions(caps, "trade_split")