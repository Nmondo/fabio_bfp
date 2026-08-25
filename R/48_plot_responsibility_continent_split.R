# =============================================================================
# 48_plot_responsibility_continent_split.R
# 43's REGIONAL accounts figure with every bar opened up: the production- (PBA),
# consumption- (CBA), HDI justice-based and value added-based (VA) accounts as
# adjacent STACKED bars per continent, each split by the ORIGIN x DESTINATION of
# the charged impact flow. It is 45's figure with countries replaced by regions
# and 45's single year replaced by 43's two periods.
#   x = account (4 bars) | facet columns = continent | facet rows = period
#   Each bar is a single account, ALL >= 0: the bar HEIGHT is the account total
#   -- the SAME height 43 already draws -- and its segments show where that
#   charged impact flows (domestic / export / import). What an account does not
#   charge is simply not drawn.
#
# THE BAR HEIGHTS ARE 43'S. ONLY THE INTERIOR IS NEW --------------------------
# This figure adds no number 43 does not already have. The row and column
# margins of a folded matrix are the country margins summed by region, so PBA,
# CBA, HDI and VA per continent are exactly 43's four bars; the split is a
# strict enrichment of them. That is not an assumption -- 41 and 42 each verify
# the fold against their own country tables, and check_against_43() below
# repeats the test across the file boundary, which is what stops the two figures
# drifting apart.
#
# WHY THIS CANNOT BE BUILT FROM 45'S INPUTS -----------------------------------
# 45 reads country margins (justice_domestic / justice_export / ...). Summing
# those over the countries of a region keeps the COUNTRY meaning of "domestic",
# so a German biodiesel export to France stays in the dark export block inside
# an EU panel. The figure's whole argument is that dark block shrinking from PBA
# to HDI to CBA, read as burden handed to TRADE PARTNERS; for the EU, most of
# that block would be the bloc trading with itself, and the reader would take
# away the opposite of what the numbers say. So 41 and 42 fold D[p, c] and
# H*[r, p] onto continents at source, where the whole matrix still exists, and
# this script reads the folded matrices. 41 prints the size of the difference:
# the share of the world's traded impact that never leaves its own region.
#
# THE FOUR BARS ---------------------------------------------------------------
# Per region C, from 41's D_c (impact in producing region x consuming region)
# and its HDI-weighted twin W_c (the part of each block charged to the PRODUCER):
#     domestic(C)  = D_c[C, C]
#     export(C)    = rowSums(D_c)[C] - domestic       to OTHER regions
#     import(C)    = colSums(D_c)[C] - domestic       from OTHER regions
#   PBA  domestic + the whole export, both charged in full; nothing imported.
#   CBA  domestic in full; export booked to the consuming region (not drawn);
#        import charged in full.
#   HDI  the two INTER-regional flows split by relative HDI (Sun et al. 2022):
#        the region keeps sum beta*D over its outbound pairs and sum (1-beta)*D
#        over its inbound ones. Note the betas stay BILATERAL -- no regional
#        average HDI is ever formed. Intra-regional flows need no beta at all:
#        whichever side of an intra-EU pair the split charges, the charge lands
#        in the EU, so the block returns to the region in full, exactly like
#        domestic production. This is why HDI(C) is invariant under the fold.
#   VA   a DIFFERENT matrix: 42's H*[value generator, extraction origin], folded
#        the same way. Its diagonal is NOT D_c[C, C], and its trade blocks are
#        about VALUE CAPTURE, not consumption. Like CBA it charges none of its
#        export, so its dark block is empty. Do not read the VA bar as a fourth
#        slicing of D_c.
#
# PERIODS, NOT A YEAR ---------------------------------------------------------
# 43's PERIODS, and its period MEAN (not sum), so a short run stays comparable.
# Every component is additive, so averaging them and averaging the accounts are
# the same operation and the identity survives.
#
# NEGATIVE FLOWS --------------------------------------------------------------
# D has negative bilateral entries (FABIO's Y carries stock change), and v = p/x
# is negative where value added is. Folding onto 8 regions cancels most of it,
# but not by construction, so the check 45 runs per country-chain is run here per
# region-chain, with the same NEG_FLOWS switch ("clamp" | "keep" | "drop"). The
# identity check against 43 runs on the RAW numbers, before any clamp.
#
# READS  CONT_FLOW_CSV    (41)  year, biofuel_group, producer_continent,
#                               consumer_continent, impact, producer_kept
#        VA_CONT_FLOW_CSV (42)  year, va_variant, biofuel_group, va_continent,
#                               origin_continent, impact
#        HDI_CSV (41) and VA_RESP_CSV (42), for the cross-file identity check only
# WRITES output/plot/
#   responsibility_continent_split_<ind>_<vabase>_<alloc>.svg
#   responsibility_continent_split_<ind>_<vabase>_<alloc>_<period>_by_biofuel.svg
#     rows = Total (the three chains pooled) then one row per chain; one file per
#     period. The Total row is the same bars the pooled figure above shows for
#     that period -- see the note at [2] for why that figure is not retired the
#     way 43's, 44's and 45's pooled figures were.
#
# RUN: Rscript R/48_plot_responsibility_continent_split.R   (after 41 and 42)
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
SCRIPT <- "48"
run_banner()

# --- figure switches ---------------------------------------------------------
GROUPS      <- NULL     # biofuel chains to pool; NULL = all in the CSV
SHOW_PBA    <- TRUE     # draw production-based as its own BAR: domestic + the whole
# export. Its height is the region's physical production,
# the reference the CBA, HDI and VA bars move against.
BY_BIOFUEL  <- TRUE     # also write the per-chain variant, one file per period
NEG_FLOWS   <- "clamp"  # malformed rows (see the header): "clamp" | "keep" | "drop"
DROP_REGIONS <- character(0)   # e.g. "Unknown" -- regions to leave out entirely

# Left-to-right order of the panels. "pba" ranks by production in the LAST
# period, pooled over chains, so the two periods and the three chain rows all
# use ONE order and a region sits in the same column in every figure of the set.
# "alpha" restores 43's alphabetical axis.
REGION_ORDER <- "pba"   # "pba" | "cba" | "alpha"

# --- type size and furniture -------------------------------------------------
# Same value as 44/45, so a region panel here and a country panel there are read
# at the same size on the page; with eight regions side by side the panel, not
# the canvas, sets what is legible.
BASE_SIZE    <- 16
SHOW_TITLES  <- TRUE    # FALSE -> the text goes to captions_*.txt instead
SHOW_CAPTION <- TRUE

# Same separation devices and defaults as 44/45, so the three figures separate
# their panels identically. With four bars per panel the failure mode is reading
# a region's VA bar as the next region's PBA bar, which inverts the argument.
SEPARATOR  <- "border"   # "border" | "band" | "both" | "none"
SEP_COLOUR <- "grey89"   # ABOVE every fill in lightness -- see 40's flow_colors
BAND_FILL  <- "grey96"
PANEL_GAP  <- 5          # pt between region panels

SEG_BORDER     <- NA     # NA = no line between stacked segments; the three greys
SEG_BORDER_LWD <- 0.2    # are far enough apart in L* to carry the boundary

ACCOUNTS   <- c(if (SHOW_PBA) "PBA", "CBA", "HDI", "VA")
FLOW_LABS  <- flow_labels_for("region")
CHAIN_NOTE <- NULL     # resolved once, below, while `fl` is unambiguously the table
# (45 learned this the hard way: a plot function that reaches
# for a global at DRAW time is hostage to whatever else has
# been assigned that name by then)

# =============================================================================
# 1. the folded matrices
# =============================================================================
fl <- fread(need(CONT_FLOW_CSV, "41"))
need_cols(fl, c("year", "biofuel_group", "producer_continent", "consumer_continent",
                "impact", "producer_kept"), CONT_FLOW_CSV)

vf <- fread(need(VA_CONT_FLOW_CSV, "42"))
need_cols(vf, c("year", "va_variant", "biofuel_group", "va_continent",
                "origin_continent", "impact"), VA_CONT_FLOW_CSV)
vf <- vf[va_variant == VA_VARIANT]
if (!nrow(vf))
  stop("[48] ", basename(VA_CONT_FLOW_CSV), " holds no '", VA_VARIANT,
       "' rows -- re-run 42.")

if (!is.null(GROUPS)) {
  unknown <- setdiff(GROUPS, unique(fl$biofuel_group))
  if (length(unknown)) stop("[48] biofuel_group(s) not in the CSV: ",
                            paste(unknown, collapse = ", "))
  fl <- fl[biofuel_group %in% GROUPS]; vf <- vf[biofuel_group %in% GROUPS]
}

# The two files must be folded onto the SAME partition -- 41 and 42 both go
# through 40's continent_of(), so a difference here means one of them was run
# against a different regions_full.csv and the panels would not line up.
r_fl <- sort(unique(c(fl$producer_continent, fl$consumer_continent)))
r_vf <- sort(unique(c(vf$va_continent, vf$origin_continent)))
if (!setequal(r_fl, r_vf))
  warning("[48] 41 and 42 folded onto DIFFERENT region sets: ",
          paste(sort(union(setdiff(r_fl, r_vf), setdiff(r_vf, r_fl))), collapse = ", "),
          " -- re-run whichever is older.")

CHAIN_NOTE <- paste(sort(unique(fl$biofuel_group)), collapse = " + ")
message(sprintf("[48] %d flow rows | %d regions | chains pooled: %s",
                nrow(fl), length(r_fl), CHAIN_NOTE))

# =============================================================================
# 2. margins: the components each account charges
# =============================================================================
# `keys` is the grouping BESIDES the region: c("year") for the pooled figure,
# c("year", "biofuel_group") for the per-chain one. Everything is a sum over the
# folded matrix, so both resolutions come from the same code.
d_margins <- function(fl, keys) {
  # producer side: the row margin, and the beta share kept on OUTBOUND flows only
  # (the diagonal returns in full through `domestic` and must not be counted twice)
  by_p <- fl[, .(PBA     = sum(impact),
                 hdi_exp = sum(producer_kept * (producer_continent != consumer_continent))),
             by = c(keys, "producer_continent")]
  setnames(by_p, "producer_continent", "continent")
  
  # consumer side: the column margin, the (1-beta) share kept on INBOUND flows,
  # and the diagonal
  by_c <- fl[, .(CBA      = sum(impact),
                 hdi_imp  = sum((impact - producer_kept) *
                                  (producer_continent != consumer_continent)),
                 domestic = sum(impact * (producer_continent == consumer_continent))),
             by = c(keys, "consumer_continent")]
  setnames(by_c, "consumer_continent", "continent")
  
  m <- merge(by_p, by_c, by = c(keys, "continent"), all = TRUE)
  for (j in c("PBA", "CBA", "hdi_exp", "hdi_imp", "domestic"))
    set(m, which(is.na(m[[j]])), j, 0)
  m[, `:=`(export_full = PBA - domestic,
           import_full = CBA - domestic,
           HDI         = domestic + hdi_exp + hdi_imp)]
  m[]
}

# H* folded: rows are value generators, columns extraction origins.
va_margins <- function(vf, keys) {
  by_r <- vf[, .(VA = sum(impact)), by = c(keys, "va_continent")]
  setnames(by_r, "va_continent", "continent")
  by_o <- vf[, .(va_production = sum(impact)), by = c(keys, "origin_continent")]
  setnames(by_o, "origin_continent", "continent")
  dg <- vf[va_continent == origin_continent, .(va_domestic = sum(impact)),
           by = c(keys, "va_continent")]
  setnames(dg, "va_continent", "continent")
  
  m <- merge(merge(by_r, by_o, by = c(keys, "continent"), all = TRUE),
             dg, by = c(keys, "continent"), all = TRUE)
  for (j in c("VA", "va_production", "va_domestic"))
    set(m, which(is.na(m[[j]])), j, 0)
  m[, `:=`(va_import = VA            - va_domestic,    # extracted abroad, charged to this VA
           va_export = va_production - va_domestic)]   # extracted here, charged to FOREIGN VA
  m[]
}

# `flows` / `va_flows` default to the globals; they are arguments so the per-chain
# build can be handed with_total()ed copies without the pooled build, the chain
# note or check_against_43() ever seeing them.
build_wide <- function(keys, flows = fl, va_flows = vf) {
  a <- merge(d_margins(flows, keys), va_margins(va_flows, keys),
             by = c(keys, "continent"), all = TRUE)
  NUMS <- c("PBA", "CBA", "HDI", "VA", "domestic", "export_full", "import_full",
            "hdi_exp", "hdi_imp", "va_domestic", "va_import", "va_export",
            "va_production")
  for (j in NUMS) set(a, which(is.na(a[[j]])), j, 0)
  if (length(DROP_REGIONS)) a <- a[!continent %in% DROP_REGIONS]
  a[]
}

# =============================================================================
# 3. the checks -- run on the RAW numbers, before any clamp
# =============================================================================
# (a) the fold reproduces 43's accounts, ACROSS the file boundary. 41 and 42
#     each check their own fold internally; this one catches the case where the
#     four CSVs come from different runs, which is the failure that would put a
#     correct-looking split on the wrong bars.
#     The country tables are aggregated with continent_of(), NOT with their own
#     `continent` column: that makes this a test of the NUMBERS rather than of
#     the tagging, and it is the same partition the fold used.
check_against_43 <- function(a) {
  hdi <- tryCatch(fread(HDI_CSV),     error = function(e) NULL)
  var <- tryCatch(fread(VA_RESP_CSV), error = function(e) NULL)
  if (is.null(hdi) || is.null(var)) {
    message("[48] 43's country CSVs not both present -- skipping the cross-file identity check.")
    return(invisible(NULL))
  }
  # the same chain filter the flow files went through, or the check compares a
  # subset against the whole and fails for a reason that has nothing to do with
  # the fold
  if (!is.null(GROUPS)) {
    hdi <- hdi[biofuel_group %in% GROUPS]
    var <- var[biofuel_group %in% GROUPS]
  }
  ref <- hdi[, .(PBA = sum(production_based), CBA = sum(consumption_based),
                 HDI = sum(justice_based)),
             by = .(year, continent = continent_of(iso3c))]
  vr  <- var[va_variant == VA_VARIANT,
             .(VA = sum(va_resp)), by = .(year, continent = continent_of(va_iso3c))]
  ref <- merge(ref, vr, by = c("year", "continent"), all = TRUE)
  cmp <- merge(a[, .(year, continent, PBA, CBA, HDI, VA)], ref,
               by = c("year", "continent"), all = TRUE, suffixes = c("", "_43"))
  for (j in names(cmp)) set(cmp, which(is.na(cmp[[j]])), j, 0)
  
  worst <- 0; who <- ""
  for (acc in c("PBA", "CBA", "HDI", "VA")) {
    dev <- max(abs(cmp[[acc]] - cmp[[paste0(acc, "_43")]])) /
      max(max(abs(cmp[[paste0(acc, "_43")]])), 1e-12)
    if (dev > worst) { worst <- dev; who <- acc }
  }
  if (worst > DIV_TOL)
    warning(sprintf(paste0("[48] the folded matrices do not reproduce 43's regional accounts ",
                           "(worst: %s, rel. %.3g). The bar HEIGHTS here must be 43's bars ",
                           "exactly; a gap means 41/42's matrices and their country tables are ",
                           "from different runs. Re-run 41 and 42 before reading this figure."),
                    who, worst))
  else
    message(sprintf("[48] bar heights verified against 43's accounts (worst rel. dev %.1e, %s)",
                    worst, who))
  invisible(worst)
}

# (b) are the folded flows well-formed? Same test as 45, one resolution up.
clean_flows <- function(a, keys) {
  s <- pmax(abs(a$PBA), abs(a$CBA), .Machine$double.eps)   # each row's own scale
  chk <- data.table(a[, c(keys, "continent"), with = FALSE], scale = s,
                    neg_domestic = pmax(-a$domestic,    0) / s,
                    neg_export   = pmax(-a$export_full, 0) / s,
                    neg_import   = pmax(-a$import_full, 0) / s,
                    over_export  = pmax(a$hdi_exp - a$export_full, 0) / s,
                    over_import  = pmax(a$hdi_imp - a$import_full, 0) / s,
                    neg_va_dom   = pmax(-a$va_domestic, 0) / s,
                    neg_va_imp   = pmax(-a$va_import,   0) / s)
  CULPRITS <- c(neg_domestic = "domestic < 0",         neg_export  = "export residual < 0",
                neg_import   = "import residual < 0",  over_export = "HDI export > flow",
                over_import  = "HDI import > flow",    neg_va_dom  = "VA domestic < 0",
                neg_va_imp   = "VA import < 0")
  chk[, worst := pmax(neg_domestic, neg_export, neg_import, over_export, over_import,
                      neg_va_dom, neg_va_imp)]
  chk[, culprit := CULPRITS[max.col(as.matrix(.SD), ties.method = "first")],
      .SDcols = names(CULPRITS)]
  chk[, share_of_world := scale / max(sum(abs(a$CBA)), .Machine$double.eps)]
  chk[, distortion := worst * share_of_world]
  
  bad <- chk[worst > NOISE_TOL][order(-distortion)]
  if (nrow(bad)) {
    warning(sprintf(paste0("[48] %d region-row(s) with a MALFORMED flow -- D has negative ",
                           "bilateral entries (FABIO's Y carries stock change) or the VA ",
                           "intensity v = p/x is negative. NEG_FLOWS = '%s'. Ranked by the ",
                           "distortion each puts on the world total: %s"),
                    nrow(bad), NEG_FLOWS,
                    paste(sprintf("%s [%s] %.1e of the world", head(bad$continent, 5),
                                  head(bad$culprit, 5), head(bad$distortion, 5)),
                          collapse = "; ")))
    tot_d <- sum(bad$distortion)
    message(sprintf("[48]   total distortion from the clamp: %.2g%% of the world total -- %s",
                    100 * tot_d,
                    if (tot_d < 1e-4)
                      "far below a pixel; whatever NEG_FLOWS does cannot move the figure."
                    else "big enough to see: check it before quoting any bar."))
  } else {
    message("[48] no malformed regional flows: the fold cancelled them (45 flags them per country).")
  }
  
  if (NEG_FLOWS == "drop" && nrow(bad)) {
    a <- a[!bad[, c(keys, "continent"), with = FALSE], on = c(keys, "continent")]
  } else if (NEG_FLOWS == "clamp") {
    a[, `:=`(domestic    = pmax(domestic,    0),
             export_full = pmax(export_full, 0),
             import_full = pmax(import_full, 0))]
    a[, `:=`(hdi_exp = pmin(pmax(hdi_exp, 0), export_full),
             hdi_imp = pmin(pmax(hdi_imp, 0), import_full))]
    a[, `:=`(va_domestic = pmax(va_domestic, 0),
             va_import   = pmax(va_import,   0),
             va_export   = pmax(va_export,   0))]
    a[, `:=`(PBA = domestic + export_full,
             CBA = domestic + import_full,
             HDI = domestic + hdi_exp + hdi_imp,
             VA  = va_domestic + va_import)]
  } else if (NEG_FLOWS != "keep") {
    stop("[48] NEG_FLOWS must be 'clamp', 'keep' or 'drop'.")
  }
  a[]
}

# =============================================================================
# 4. wide -> the long table the bars are drawn from
# =============================================================================
# `dom` is an explicit argument rather than one shared column: the VA bar's
# domestic block is H*_c[C, C], NOT D_c[C, C]. The first three bars are margins
# of one matrix and share their mid-grey block; the fourth is not and does not.
as_bars <- function(a, keys) {
  kk <- c(keys, "continent")
  mk <- function(acc, dom, kept_exp, kept_imp)
    data.table(a[, kk, with = FALSE], account = acc,
               domestic = dom, export_kept = kept_exp, import_kept = kept_imp)
  
  parts <- list(
    mk("CBA", dom = a$domestic,    kept_exp = 0,          kept_imp = a$import_full),
    mk("HDI", dom = a$domestic,    kept_exp = a$hdi_exp,  kept_imp = a$hdi_imp),
    mk("VA",  dom = a$va_domestic, kept_exp = 0,          kept_imp = a$va_import)
  )
  if (SHOW_PBA)
    parts <- c(list(mk("PBA", dom = a$domestic, kept_exp = a$export_full, kept_imp = 0)),
               parts)
  
  long <- melt(rbindlist(parts, use.names = TRUE), id.vars = c(kk, "account"),
               measure.vars = COMPONENTS, variable.name = "component",
               value.name = "value")
  
  # the identity every bar rests on: charged = domestic + export kept + import
  # kept. Merged on the keys, never compared by position -- the two tables fall
  # out of order the moment a region has nothing charged under one account.
  chg <- long[, .(charged = sum(value)), by = c(kk, "account")]
  ref <- melt(a[, c(kk, ACCOUNTS), with = FALSE], id.vars = kk,
              variable.name = "account", value.name = "acct")
  ref[, account := as.character(account)]
  cmp <- merge(ref, chg, by = c(kk, "account"), all = TRUE)
  cmp[is.na(charged), charged := 0][is.na(acct), acct := 0]
  for (acc in ACCOUNTS) {
    z   <- cmp[account == acc]
    dev <- max(abs(z$charged - z$acct)) / max(max(abs(z$acct)), 1)
    if (dev > NOISE_TOL)
      warning(sprintf("[48] %s: the charged segments do not sum to the account (max rel. %.3g)",
                      acc, dev))
  }
  
  long[, component := factor(component, levels = COMPONENTS)]
  long[, account   := factor(account,   levels = ACCOUNTS)]
  long[]
}

# =============================================================================
# 5. build both resolutions
# =============================================================================
w_pool <- build_wide("year")
check_against_43(w_pool)                       # RAW, before the clamp
w_pool <- clean_flows(w_pool, "year")

COMP_COLS <- c("PBA", "CBA", "HDI", "VA", "domestic", "export_full", "import_full",
               "hdi_exp", "hdi_imp", "va_domestic", "va_import", "va_export")

p_pool <- period_mean(w_pool, COMP_COLS, "continent")
if (!nrow(p_pool))
  stop("[48] no years of ", paste(names(PERIODS), collapse = " / "), " in the flow files.")

# --- panel order -------------------------------------------------------------
LAST <- names(PERIODS)[length(PERIODS)]
ord <- switch(REGION_ORDER,
              alpha = sort(unique(as.character(p_pool$continent))),
              pba   = p_pool[period == LAST][order(-PBA), as.character(continent)],
              cba   = p_pool[period == LAST][order(-CBA), as.character(continent)],
              stop("[48] REGION_ORDER must be 'pba', 'cba' or 'alpha'."))
ord <- unique(c(ord, sort(setdiff(unique(as.character(p_pool$continent)), ord))))
p_pool[, continent := factor(continent, levels = ord)]
message("[48] panel order (", REGION_ORDER, "): ", paste(ord, collapse = " "))

bars_pool <- as_bars(p_pool, "period")

# --- the per-chain resolution -----------------------------------------------
bars_bf <- NULL
if (BY_BIOFUEL && uniqueN(fl$biofuel_group) > 1) {
  # with_total() on BOTH flow tables: every margin below is a sum over the folded
  # matrices, so the `total` key reproduces build_wide("year") exactly -- which is
  # the pooled figure's own numbers, and is what makes the Total row and figure
  # [1] two views of one thing rather than two computations of it.
  w_bf <- clean_flows(build_wide(c("year", "biofuel_group"),
                                 with_total(fl), with_total(vf)),
                      c("year", "biofuel_group"))
  p_bf <- period_mean(w_bf, COMP_COLS, c("continent", "biofuel_group"))
  p_bf[, continent := factor(continent, levels = ord)]     # SAME order as above
  # pretty chain labels; anything unexpected is title-cased and appended
  raw  <- as.character(p_bf$biofuel_group)
  lab  <- ifelse(raw %in% names(biofuel_label), unname(biofuel_label[raw]),
                 tools::toTitleCase(gsub("_", " ", raw)))
  lvls <- c(unname(biofuel_label[names(biofuel_label) %in% raw]),
            sort(setdiff(unique(lab), unname(biofuel_label))))
  p_bf[, biofuel_group := factor(lab, levels = lvls)]
  bars_bf <- as_bars(p_bf, c("period", "biofuel_group"))
}

# --- context for the console -------------------------------------------------
# The single number that says how much the fold changed: at country resolution
# these flows are trade, at regional resolution most of them are not.
late <- p_pool[period == LAST]
message(sprintf("[48] %s: traded share of the footprint AT REGIONAL RESOLUTION: %.1f%% (45 shows the country figure)",
                LAST, 100 * sum(late$export_full) / max(sum(late$CBA), .Machine$double.eps)))
message(sprintf("[48] %s: share of the VA account resting on extraction OUTSIDE the region: %.1f%%",
                LAST, 100 * sum(late$va_import) / max(sum(late$VA), .Machine$double.eps)))

# =============================================================================
# 6. plot
# =============================================================================
SUB <- paste0(
  if (SHOW_PBA) "PBA production-based | " else "",
  "CBA consumption-based | HDI justice-based (Sun et al. 2022) | VA value added-based ",
  "(Pi\u00f1ero et al. 2019, eq. 8): re-attributions of one and the same total.\n",
  "Every bar is the SAME region; only the charging rule moves, so the bar HEIGHT is that ",
  "account -- the same heights 43 draws. Segments locate the PRODUCTION the accounted-for ",
  "impact is attributed to (see legend):\ndark = produced in-region for export, ",
  "mid-grey = produced and accounted for in-region, light = produced outside it. ",
  if (SHOW_PBA)
    paste0("PBA charges the whole dark export block; CBA charges none of it (it vanishes); ",
           "HDI keeps HDI_p/(HDI_p+HDI_c) of it, pair by pair.")
  else
    paste0("CBA charges none of the dark export block; HDI keeps HDI_p/(HDI_p+HDI_c) of it."),
  "\nTRADE IS BETWEEN REGIONS HERE, NOT BETWEEN COUNTRIES: everything two countries of one ",
  "region trade with each other is drawn as mid-grey, because the impact and the charge both ",
  "stay in the region.\n45 is the same figure at country resolution, where those flows are ",
  "exports. The first three bars split ONE matrix D[producer, consumer], so their mid-grey ",
  "block is the same number in each;\nVA splits a DIFFERENT matrix, H*[value generator, ",
  "extraction origin], so its mid-grey block moves too, and its light block is production ",
  "outside the region charged to value added inside it -- not to consumption inside it.",
  provenance_note())

plot_split <- function(p, title, subtitle, rows = "period", free_y = FALSE,
                       legend_nrow = 1L) {
  ggplot(p, aes(x = account, y = value / META$scale_factor, fill = component)) +
    panel_bands(p, "continent", SEPARATOR, BAND_FILL) +      # BENEATH the bars
    geom_col(position = position_stack(reverse = TRUE),      # domestic against the zero line
             width = 0.8, colour = SEG_BORDER, linewidth = SEG_BORDER_LWD) +
    scale_fill_manual(values = flow_colors, labels = FLOW_LABS, drop = FALSE,
                      name = NULL) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    guides(fill = guide_legend(nrow = legend_nrow, byrow = TRUE)) +
    # switch = "both": the region names go UNDER the panels (as in 45) and the row
    # variable to the LEFT, where 43 puts its chain labels.
    facet_grid(stats::as.formula(paste(rows, "~ continent")),
               scales = if (free_y) "free_y" else "fixed", switch = "both") +
    labs(x = NULL, y = META$y_label, title = title, subtitle = subtitle,
         caption = if (SHOW_CAPTION)
           sprintf("%s allocation; %s VA base; %s.", allocation, VA_BASE, CHAIN_NOTE)
         else NULL) +
    theme_minimal(base_size = BASE_SIZE) +
    theme(legend.position    = "bottom",
          legend.key.size    = unit(BASE_SIZE - 1, "pt"),
          legend.text        = element_text(size = BASE_SIZE - 3),
          legend.title       = element_blank(),
          axis.title         = element_text(size = BASE_SIZE - 1),
          axis.text.y        = element_text(size = BASE_SIZE - 3),
          axis.text.x        = element_text(size = BASE_SIZE - 4, angle = 90,
                                            hjust = 1, vjust = 0.5),
          strip.placement    = "outside",
          strip.background   = element_blank(),
          strip.text.x       = element_text(face = "bold", size = BASE_SIZE - 1),
          strip.text.y.left  = element_text(face = "bold", size = BASE_SIZE - 1,
                                            angle = 90),
          panel.border       = if (SEPARATOR %in% c("border", "both"))
            element_rect(fill = NA, colour = SEP_COLOUR, linewidth = 0.3)
          else element_blank(),
          panel.spacing.x    = unit(PANEL_GAP, "pt"),
          panel.spacing.y    = unit(8, "pt"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank(),
          plot.title         = element_text(size = BASE_SIZE + 2, face = "bold"),
          plot.subtitle      = element_text(size = BASE_SIZE - 4, colour = "grey30"),
          plot.caption       = element_text(size = BASE_SIZE - 5, colour = "grey40"))
}

# --- canvas ------------------------------------------------------------------
# Hold the bar DENSITY constant (~0.42in per bar) rather than the canvas width,
# with a floor set by the LEGEND: at BASE_SIZE 16 the three regional flow labels
# are longer than 45's and need ~16in side by side, so below that the legend
# wraps to two rows instead of overflowing. Eight regions come to ~15in on the
# density rule alone, so for the usual grid it is this floor that sets the width.
N_REG  <- length(ord)
PLOT_W <- min(26, max(if (SHOW_TITLES) 16 else 15,
                      2 + 0.42 * N_REG * length(ACCOUNTS)))
LEG_N  <- if (PLOT_W >= 16) 1L else 2L
caps   <- character(0)

# --- [1] the pooled figure: rows = period, columns = region ------------------
ttl  <- sprintf("%s responsibility by region, %s vs %s - where each account charges the impact",
                META$short_label, names(PERIODS)[1], LAST)
sub  <- SUB
stem <- sprintf("responsibility_continent_split_%s_%s_%s", STAG, VA_BASE, ATAG)
caps <- c(caps, lab_block(stem, ttl, sub, SHOW_TITLES))

ttlw <- wrap_lines(ttl, PLOT_W, BASE_SIZE + 2)
subw <- wrap_lines(sub, PLOT_W, BASE_SIZE - 4)
fig_h <- 3.4 * max(uniqueN(p_pool$period), 1) + 1.4
if (SHOW_TITLES)
  fig_h <- fig_h + text_height_in(ttlw, BASE_SIZE + 2) +
  text_height_in(subw, BASE_SIZE - 4) + 0.25

save_svg(stem,
         plot_split(bars_pool,
                    if (SHOW_TITLES) ttlw else NULL,
                    if (SHOW_TITLES) subw else NULL,
                    rows = "period", free_y = FALSE, legend_nrow = LEG_N),
         width = PLOT_W, height = fig_h)

# --- [2] one file per period, rows = Total then each biofuel chain -----------
# NOT one figure with chain and period both on the rows: `scales = "free_y"`
# frees a scale per ROW, so a row mixing the two would silently give the two
# periods of one chain different axes -- the exact trap 43's header warns about.
# Chains on the rows, one period per file: the axis is then free per chain (they
# differ by an order of magnitude) and the regions inside a row stay comparable.
#
# WHY FIGURE [1] SURVIVES HERE AND NOWHERE ELSE -------------------------------
# In 43, 44 and 45 the pooled figure became the top row of the grid and the file
# was retired. Here it cannot be: figure [1] puts the two PERIODS on the rows at
# a FIXED scale, which is the only place in this script the periods are directly
# comparable. These per-period files each carry their own free scales, so the
# Total row added to them answers "what does the pool look like in this period",
# not "how did the pool move between them". Both questions are worth a figure,
# so both figures stay.
if (!is.null(bars_bf)) {
  for (per in levels(bars_bf$period)) {
    pb <- bars_bf[period == per]
    if (!nrow(pb)) next
    
    ttl_b <- sprintf("%s responsibility by region, %s - by biofuel chain",
                     META$short_label, per)
    sub_b <- paste0(SUB, "\nSame regions and order as the pooled figure. Top row is all ",
                    "three chains pooled -- the same bars figure [1] shows for this period. ",
                    "The y axis is FREE PER ROW: compare regions and accounts within a row, ",
                    "not bar heights across rows, and not against Total.")
    stem_b <- sprintf("responsibility_continent_split_%s_%s_%s_%s_by_biofuel",
                      STAG, VA_BASE, ATAG, slug(per, "period"))
    caps   <- c(caps, lab_block(stem_b, ttl_b, sub_b, SHOW_TITLES))
    
    ttl_bw <- wrap_lines(ttl_b, PLOT_W + 1, BASE_SIZE + 2)
    sub_bw <- wrap_lines(sub_b, PLOT_W + 1, BASE_SIZE - 4)
    h_b    <- 2.9 * uniqueN(pb$biofuel_group) + 1.6
    if (SHOW_TITLES)
      h_b <- h_b + text_height_in(ttl_bw, BASE_SIZE + 2) +
      text_height_in(sub_bw, BASE_SIZE - 4) + 0.25
    
    save_svg(stem_b,
             plot_split(pb,
                        if (SHOW_TITLES) ttl_bw else NULL,
                        if (SHOW_TITLES) sub_bw else NULL,
                        rows = "biofuel_group", free_y = TRUE, legend_nrow = LEG_N),
             width = PLOT_W + 1, height = h_b)
  }
}

write_captions(caps, "continent_split", stamp = "periods")

# --- console: the table behind the pooled figure -----------------------------
cat(sprintf("\n-- %s | %s: regional accounts and their split, %s --\n",
            META$short_label, allocation, LAST))
print(late[order(-PBA),
           .(continent,
             PBA      = round(PBA / META$scale_factor),
             CBA      = round(CBA / META$scale_factor),
             HDI      = round(HDI / META$scale_factor),
             VA       = round(VA  / META$scale_factor),
             domestic = round(domestic    / META$scale_factor),
             exports  = round(export_full / META$scale_factor),
             imports  = round(import_full / META$scale_factor))])
cat("\n - PBA/CBA/HDI/VA must be identical to 43's four bars; only the split is new.\n")
cat(" - domestic/exports/imports are BETWEEN REGIONS: intra-regional trade is domestic here.\n")

message(">>> [48] plots written to ", PLOT_DIR)