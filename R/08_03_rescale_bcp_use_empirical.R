# =============================================================================
# rescale_feedstock_mix.R
# FABIO-BCP  -  Retrofit biofuel feedstock-mix (supply shares) to RED targets
# -----------------------------------------------------------------------------
# PROBLEM (per year x biofuel)
#   Build a country x feedstock matrix in SUPPLY space (= use x TCF). Move the
#   *aggregate*, supply-weighted feedstock shares (summed over a selected set of
#   producer countries) from the FABIO mix to the RED mix, while:
#     (i)  each country's TOTAL biofuel output stays fixed   -> row margins fixed
#     (ii) reallocation happens only where a feedstock was    -> RAS preserves the
#          initially used                                        zero pattern
#   => fixed row margins + RED-pinned column margins = biproportional fit (RAS).
#
#   Selected countries:
#     c146 : EU
#     c147 : EU + IDN + CHN
#     c149 : EU + SGP
#
#   Special cases:
#     - c901 : its column increase is spread PROPORTIONAL TO COUNTRY SUPPLY
#              SHARE (not to its own initial use) -> seed the c901 column prior
#              with that pattern; RAS scales it to the RED level.
#     - IDN/CHN : only the EU-export fraction of their output is rescalable.
#                 Split their supply into rescalable (eu_share) + fixed; only the
#                 rescalable part enters RAS, the rest keeps its FABIO mix. This
#                 cap also flows into the c901 proportional allocation via row_t.
#
#   NOTE: every item tagged `TODO/CONFIRM` is a spec I could not resolve from the
#   brief - see the companion flag list.
# =============================================================================

suppressPackageStartupMessages({ library(data.table); library(dplyr) })

input_path <- "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/"


# -----------------------------------------------------------------------------
# 0. CONFIG  (CONFIRM the tagged values)
# -----------------------------------------------------------------------------
TCF_VALUE_COL              <- "output_qty"  # kl biofuel output per tonne feedstock
# (= 1 / multiplier_output_kl_to_input_t)
TCF_AS_OUTPUT_PER_FEEDSTOCK <- TRUE         # supply [kl] = use [t] * output_qty

C901                 <- "c901"
C901_SEED_LAMBDA     <- 1.0   # 1 => c901 column seeded purely proportional to country supply;
# <1 => blend with c901's actual (normalised) pattern
C901_IN_FEEDSTOCK_GAP <- TRUE # c901 = "Other, Waste" pseudo-commodity: tiny FABIO prior, carries a
#        feedstock-level RED share to hit. Column seeded proportional to
#        country supply, then RAS scales it to the RED level.

ZERO_NONGAP_FEEDSTOCKS <- TRUE # TODO feedstocks used in FABIO but absent from RED list:
#      TRUE => driven to 0 (RED authoritative); FALSE => held.

# Blocks where RAS is structurally infeasible (orthogonal structure / tightness>1):
# fall back to imposing the RED global-average mix on EVERY country directly.
# Keyed "year biofuel_code". Add others (e.g. divergent c146 blocks) as needed.
DIRECT_RED_BLOCKS <- c("2012 c149", "2013 c149", "2014 c149")

CAP_COUNTRIES <- c("IDN", "CHN")
RAS_MAX_ITER  <- 5000L
RAS_TOL       <- 1e-9

# -----------------------------------------------------------------------------
# 1. LOAD INPUTS
# -----------------------------------------------------------------------------
regions <- fread("inst/regions_full.csv")[current == TRUE]
items   <- fread("inst/items_full_bcp.csv")

# TODO the brief lists BOTH gaps under "inst/gap_empirical_ shares_rescale.rds"
#      (note the stray space). Point these at the two real files:
gap_feed <- as.data.table(readRDS("inst/gap_empirical_shares_rescale.rds")) # 186 obs
gap_ctry <- as.data.table(readRDS("inst/gap_empirical_origin_shares_rescale.rds"))   # 2939 obs

X       <- readRDS(paste0(input_path, "losses/X.rds"))
bfTrade <- fread("output/FABIO_bfTrade_2012-2022_BF_final_demand_country.csv")
use_raw <- as.data.table(readRDS("intermediate_data/use_rebal_bcp.rds"))

# TCF (the table already carries comm_code, incl. c901 "Other, Waste"; only fill
# any gaps from items rather than letting the join clobber/duplicate it) ---------
tcf <- readRDS("intermediate_data/tcf_table_final.rds")
tcf$item <- ifelse(tcf$item == " Triticale", "Triticale", tcf$item)
setDT(tcf)
tcf[items, comm_code := fcoalesce(comm_code, i.comm_code), on = "item"]  # keep tcf's, fill NAs
tcf <- tcf %>% mutate(biofuel_code = case_when(
  grepl("Biogasoline", proc)      ~ "c146",
  grepl("Biodiesel", proc)        ~ "c147",
  grepl("Renewable diesel", proc) ~ "c149",
  TRUE ~ NA_character_)) %>%
  filter(!item %in% c("Oilcrops Oil, Other", "Total"))
setDT(tcf)

# -----------------------------------------------------------------------------
# 2. SELECTION OF THE SET OF COUNTRIES FOR RESCALING
# -----------------------------------------------------------------------------
# Confirmed: filter regions$continent == "EU" (member states). "EUR" is the
# separate bucket for non-EU European countries (Albania, Norway, ...), excluded.
EU_ISO3 <- regions[continent %in% c("EU", "EUR"), iso3c]

selected_iso3 <- function(bf) switch(bf,
                                     "c146" = EU_ISO3,
                                     "c147" = union(EU_ISO3, c("IDN", "CHN")),
                                     "c149" = union(EU_ISO3, c("SGP", "CHN")),
                                     stop("unknown biofuel ", bf))


# -----------------------------------------------------------------------------
# 3. SUPPLY TABLE  (supply = use x tcf)
# -----------------------------------------------------------------------------
proc_to_bf <- function(p) fcase(
  grepl("Biogasoline", p),      "c146",
  grepl("Biodiesel", p),        "c147",
  grepl("Renewable diesel", p), "c149",
  default = NA_character_)

sup <- copy(use_raw)
setnames(sup, "use", "use_qty")
sup[, year := as.integer(year)]
sup[, biofuel_code := proc_to_bf(proc)]
sup <- sup[!is.na(biofuel_code)]

sup <- merge(sup, regions[, .(code, iso3c, continent)],
             by.x = "area_code", by.y = "code", all.x = TRUE)
if (anyNA(sup$iso3c))
  warning("Unmapped area_code -> iso3c: ",
          paste(unique(sup[is.na(iso3c), area_code]), collapse = ", "))

# whitespace-robust comm_code backfill, BEFORE the tcf_join merge
i2c <- unique(items[!is.na(comm_code), .(itrim = trimws(item), comm_code)])
i2c <- i2c[!duplicated(itrim)]                       # ensure 1 comm_code per item
sup[is.na(comm_code), comm_code := i2c[.(trimws(item)), comm_code, on = "itrim"]]

# join TCF on (comm_code, biofuel_code) - robust to item-string fragility
tcf_join <- unique(tcf[!is.na(comm_code),
                       .(comm_code, biofuel_code, tcf = get(TCF_VALUE_COL))])
if (anyDuplicated(tcf_join[, .(comm_code, biofuel_code)]))
  warning("TCF not unique per (comm_code, biofuel_code) - check grain (year/country?).")
sup <- merge(sup, tcf_join, by = c("comm_code", "biofuel_code"), all.x = TRUE)

# FLAG missing conversion factors (use>0, no TCF). Strategy: pending (yours).
missing_tcf <- sup[is.na(tcf) & use_qty > 0,
                   .(item, comm_code, biofuel_code, iso3c, year, use_qty)]
if (nrow(missing_tcf))
  message(">>> ", nrow(missing_tcf),
          " rows have use>0 but NO TCF -> inspect `missing_tcf` (excluded from supply).")

if (TCF_AS_OUTPUT_PER_FEEDSTOCK) sup[, supply := use_qty * tcf] else sup[, supply := use_qty / tcf]

# sanity: how many supply rows per biofuel (catches an empty c149 if use lacks
# "Renewable diesel production" rows -> that block would otherwise return NULL)
message("Supply rows (supply>0) by biofuel: ",
        paste(sprintf("%s=%d", c("c146", "c147", "c149"),
                      vapply(c("c146", "c147", "c149"),
                             function(b) sup[biofuel_code == b & supply > 0, .N], integer(1))),
              collapse = "  "))

# -----------------------------------------------------------------------------
# 4. IDN/CHN EU-EXPORT SHARE  (cap factor = exports_to_EU / total_output)
# -----------------------------------------------------------------------------
X_dt   <- as.data.table(X, keep.rownames = "key")
X_long <- melt(X_dt, id.vars = "key", variable.name = "year", value.name = "X_out")
X_long[, year := as.integer(as.character(year))]
X_long[, c("iso3c", "biofuel_code") := tstrsplit(key, "_", fixed = TRUE)]
X_long[, biofuel_code := tolower(biofuel_code)]   # guard the "C149" capitalisation

# exports to EU for CHN/IDN, per (year, biofuel)
# TODO confirm whether to filter flow_class (e.g. exclude any non inter/intra-regional)
eu_exp <- bfTrade[source_iso %in% CAP_COUNTRIES & target_continent == "EU",
                  .(eu_out = sum(value)), by = .(year, biofuel, source_iso)]
setnames(eu_exp, c("biofuel", "source_iso"), c("biofuel_code", "iso3c"))

cap <- merge(eu_exp,
             X_long[iso3c %in% CAP_COUNTRIES, .(iso3c, biofuel_code, year, X_out)],
             by = c("iso3c", "biofuel_code", "year"), all.x = TRUE)
cap[, eu_share := pmin(1, eu_out / X_out)]

# -----------------------------------------------------------------------------
# 5. RAS balancer (swap in fit_ras_imputation() if you prefer your framework)
# -----------------------------------------------------------------------------
ras_balance <- function(P, row_t, col_t, max_iter = RAS_MAX_ITER, tol = RAS_TOL) {
  P <- as.matrix(P); storage.mode(P) <- "double"
  row_t <- row_t[rownames(P)]; col_t <- col_t[colnames(P)]
  inf_rows <- names(row_t)[rowSums(P) == 0 & row_t > 0]   # zero prior, positive target
  inf_cols <- names(col_t)[colSums(P) == 0 & col_t > 0]   # structurally infeasible
  M <- P
  for (it in seq_len(max_iter)) {
    rs <- rowSums(M); rs[rs == 0] <- 1; M <- M * (row_t / rs)
    cs <- colSums(M); cs[cs == 0] <- 1; M <- t(t(M) * (col_t / cs))
    if (max(abs(rowSums(M) - row_t)) < tol &&
        max(abs(colSums(M) - col_t)) < tol) break
  }
  attr(M, "iter") <- it; attr(M, "inf_rows") <- inf_rows; attr(M, "inf_cols") <- inf_cols
  M
}

# Non-converged (structurally infeasible) blocks: enforce country totals exactly
# and stay NON-NEGATIVE, accepting an APPROXIMATE RED feedstock mix. Row-normalise
# the RAS result -> rows = row_t exactly, zeros preserved, no negatives. The column
# deviation from RED is the unallocated gap (logged; expected minor).
enforce_row_totals <- function(M, row_t, col_t, yr = NA, bf = NA) {
  row_t <- row_t[rownames(M)]; col_t <- col_t[colnames(M)]
  rs <- rowSums(M); rs[rs == 0] <- 1
  M1 <- M * (row_t / rs)                         # totals exact, non-negative, zeros kept
  gap <- col_t - colSums(M1)                      # RED mix left unallocated (sum ~ 0)
  if (exists("residual_log"))
    residual_log[[paste(yr, bf)]] <<- data.table(
      year = yr, biofuel_code = bf, comm_code = names(gap),
      col_target = as.numeric(col_t), col_got = as.numeric(colSums(M1)),
      gap = as.numeric(gap))
  max_dev <- max(abs(gap)) / max(sum(row_t), 1)
  message("  [", yr, " ", bf, "] totals enforced; max feedstock-share gap = ",
          signif(max_dev, 3), if (max_dev > 0.05) "  <-- NOT minor" else "")
  M1
}

# -----------------------------------------------------------------------------
# 6. PER-BLOCK RESCALE
# -----------------------------------------------------------------------------
rescale_block <- function(yr, bf) {
  iso_set <- selected_iso3(bf)
  s <- sup[year == yr & biofuel_code == bf & iso3c %in% iso_set &
             !is.na(supply) & supply > 0, .(iso3c, comm_code, supply)]
  if (!nrow(s)) return(NULL)
  
  # participation weight: EU & SGP = 1 ; IDN/CHN = eu_share
  capf <- cap[year == yr & biofuel_code == bf, .(iso3c, eu_share)]
  s <- merge(s, capf, by = "iso3c", all.x = TRUE)
  s[is.na(eu_share), eu_share := 1]
  s[, supply_resc  := supply * eu_share]          # enters RAS
  s[, supply_fixed := supply * (1 - eu_share)]     # keeps FABIO mix, added back
  
  # prior matrix on rescalable mass
  P <- dcast(s, iso3c ~ comm_code, value.var = "supply_resc",
             fun.aggregate = sum, fill = 0)
  iso_rows <- P$iso3c; P <- as.matrix(P[, -1, with = FALSE]); rownames(P) <- iso_rows
  row_t <- rowSums(P)                              # fixed country output (rescalable part)
  
  # RED column shares for this block
  # line 238, extend the gap_feed pull to carry the residual + full RED value
  red <- gap_feed[year == yr & biofuel_code == bf,
                  .(comm_code, share_red, red_resid, red_value)]   # <- add red_resid, red_value
  red[, share_red := share_red / sum(share_red)]                  # (existing safety renorm)
  
  # resid fraction for THIS block: how much of producer output is up for re-mixing
  resid_frac <- sum(red$red_resid, na.rm = TRUE) / sum(red$red_value, na.rm = TRUE)
  if (!is.finite(resid_frac)) resid_frac <- 1          # guard: no RED -> treat as full
  resid_frac <- min(max(resid_frac, 0), 1)             # clamp
  
  row_t_full <- row_t                                  # keep full margins for the add-back
  row_t      <- row_t * resid_frac                     # shrink to residual scale
  total_mass <- sum(row_t)
  
  direct <- paste(yr, bf) %in% DIRECT_RED_BLOCKS
  
  # ---- DIRECT RED-MIX override (structurally infeasible blocks) ----------------
  # supply[c,f] = row_t[c] * share_red[f]: impose the RED global-average mix on
  # every country. Country totals preserved and RED columns hit exactly, but each
  # country's initial feedstock structure is DELIBERATELY discarded (homogenised).
  if (direct) {
    M <- outer(row_t, setNames(red$share_red, red$comm_code))
    dimnames(M) <- list(names(row_t), red$comm_code)
    message("  [", yr, " ", bf, "] DIRECT RED-mix imposed (initial structure discarded).")
  }
  
  if (!direct) {
    # c901 prior: pattern proportional to country supply share (blended)
    if (C901_IN_FEEDSTOCK_GAP && C901 %in% red$comm_code) {
      if (!C901 %in% colnames(P)) { P <- cbind(P, 0); colnames(P)[ncol(P)] <- C901 }
      pat <- row_t / sum(row_t)
      cur <- P[, C901]; cur <- if (sum(cur) > 0) cur / sum(cur) else pat
      P[, C901] <- C901_SEED_LAMBDA * pat + (1 - C901_SEED_LAMBDA) * cur
    }
    
    # column targets aligned to P  (UNCHANGED — already correct)
    col_t <- setNames(numeric(ncol(P)), colnames(P))
    hit   <- intersect(colnames(P), red$comm_code)
    col_t[hit] <- red[match(hit, comm_code), share_red] * total_mass
    if (!ZERO_NONGAP_FEEDSTOCKS) {
      keep <- setdiff(colnames(P), red$comm_code)
      col_t[keep] <- colSums(P)[keep]
      rem <- total_mass - sum(col_t[keep])
      col_t[hit] <- red[match(hit, comm_code), share_red] /
        sum(red[match(hit, comm_code), share_red]) * rem
    }
    
    # ---- NO net-of-fixed subtraction --------------------------------------------
    # `supply_fixed` is the capped countries' NON-EU-export mass. compute_AY_origin
    # never routes it to EU final demand, so it must NOT offset the EU-facing column
    # target. The capped countries' EU-EXPORT feedstock is already a set of rows in
    # P (supply_resc = supply*eu_share), so the residual target share_red — built on
    # red_resid = red_value - fabio_nonS — is reproduced by the pool directly.
    # supply_fixed is still carried through reassembly below for IDN/CHN output
    # conservation; it just never touches col_t. (Replaces the old block that pinned
    # colSums(M)+fixed == share_red*grand_total, which balanced combined SUPPLY space
    # instead of the consumption-trace space the comparison actually measures.)
    
    # (optional) guardrail: when a capped country's EU-export of a feedstock exceeds
    # that feedstock's residual target, the RAS can't fit it (post-2018 IDN/CHN palm
    # rebound vs collapsing RED palm). enforce_row_totals absorbs it as a logged gap.
    cap_exp <- s[iso3c %in% CAP_COUNTRIES,
                 .(cx = sum(supply_resc) * resid_frac), by = comm_code]
    cx <- setNames(numeric(ncol(P)), colnames(P)); cx[cap_exp$comm_code] <- cap_exp$cx
    tight <- names(which(cx > col_t + 1e-6 * total_mass))
    if (length(tight))
      message("  [", yr, " ", bf, "] capped EU-export exceeds residual target {",
              paste(tight, collapse = ", "), "} -> RED/FABIO export mismatch; gap logged.")
    
    M <- ras_balance(P, row_t, col_t)
    
    if (length(attr(M, "inf_cols")))
      message("  [", yr, " ", bf, "] INFEASIBLE feedstocks (zero FABIO prior, RED>0): ",
              paste(attr(M, "inf_cols"), collapse = ", "), " -> need seeding strategy.")
    # non-converged (structurally infeasible): enforce totals, keep non-negative,
    # accept an approximate RED mix (gap logged in residual_log)
    if (max(abs(rowSums(M) - row_t)) > 1e-6 * max(total_mass, 1)) {
      M <- enforce_row_totals(M, row_t, col_t, yr, bf)
    }
  } 
  
  # reassemble + add back fixed (non-EU IDN/CHN) supply
  # reassemble + add back fixed (non-EU IDN/CHN) supply + held-back (1-resid_frac) mass
  out <- melt(as.data.table(M, keep.rownames = "iso3c"),
              id.vars = "iso3c", variable.name = "comm_code",
              value.name = "supply_resc_new", variable.factor = FALSE)
  
  fixed <- s[, .(supply_fixed = sum(supply_fixed)), by = .(iso3c, comm_code)]
  held  <- s[, .(supply_held  = sum(supply_resc) * (1 - resid_frac)), by = .(iso3c, comm_code)]
  
  out <- merge(out, fixed, by = c("iso3c", "comm_code"), all = TRUE)
  out <- merge(out, held,  by = c("iso3c", "comm_code"), all = TRUE)
  
  out[is.na(supply_resc_new), supply_resc_new := 0]
  out[is.na(supply_fixed),    supply_fixed    := 0]
  out[is.na(supply_held),     supply_held     := 0]
  
  out[, supply_new := supply_resc_new + supply_fixed + supply_held]

    # initial (full, pre-cap) supply per feedstock, for side-by-side comparison
  init <- s[, .(supply_init = sum(supply)), by = .(iso3c, comm_code)]
  out <- merge(out, init, by = c("iso3c", "comm_code"), all = TRUE)
  out[is.na(supply_init), supply_init := 0]
  out[, `:=`(year = yr, biofuel_code = bf)]
  out[]
}

# -----------------------------------------------------------------------------
# 7. RUN + CHECKS
# -----------------------------------------------------------------------------
jobs <- unique(gap_feed[, .(year, biofuel_code)])
residual_log <- list()   # collected gap-not-allocated per non-converged block
res  <- rbindlist(Map(rescale_block, jobs$year, jobs$biofuel_code),
                  use.names = TRUE, fill = TRUE)
residual_log <- if (length(residual_log)) rbindlist(residual_log) else NULL  # the spread residuals

# (a) country totals preserved?
chk_ctry <- merge(
  res[, .(new = sum(supply_new)), by = .(year, biofuel_code, iso3c)],
  sup[supply > 0, .(old = sum(supply)), by = .(year, biofuel_code, iso3c)],
  by = c("year", "biofuel_code", "iso3c"))
chk_ctry[, rel_gap := (new - old) / pmax(old, 1e-30)]
message("Max |country-total rel. gap| = ", signif(max(abs(chk_ctry$rel_gap)), 3))

# (b) aggregate feedstock mix vs RED  (measured on the RESCALABLE mass)
chk_share <- res[, .(s = sum(supply_resc_new)), by = .(year, biofuel_code, comm_code)]
chk_share[, share_new := s / sum(s), by = .(year, biofuel_code)]
chk_share <- merge(chk_share,
                   gap_feed[, .(year, biofuel_code, comm_code, share_red)],
                   by = c("year", "biofuel_code", "comm_code"), all.x = TRUE)
chk_share[, dev := share_new - share_red]

# -----------------------------------------------------------------------------
# 7b. INITIAL vs NEW feedstock contributions  (structure-preservation check)
# -----------------------------------------------------------------------------
# res carries supply_init beside supply_new per (country, feedstock).
res[, delta := supply_new - supply_init]

# within-country feedstock MIX: a palm-heavy country should stay palm-heavy.
res[, share_init_ctry := supply_init / sum(supply_init), by = .(year, biofuel_code, iso3c)]
res[, share_new_ctry  := supply_new  / sum(supply_new),  by = .(year, biofuel_code, iso3c)]

# AGGREGATE contribution per feedstock across the selected countries = the margin
# RED pins. contrib_new should equal share_red on converged blocks.
agg_contrib <- res[, .(supply_init = sum(supply_init), supply_new = sum(supply_new)),
                   by = .(year, biofuel_code, comm_code)]
agg_contrib[, contrib_init := supply_init / sum(supply_init), by = .(year, biofuel_code)]
agg_contrib[, contrib_new  := supply_new  / sum(supply_new),  by = .(year, biofuel_code)]
agg_contrib <- merge(agg_contrib,
                     gap_feed[, .(year, biofuel_code, comm_code, share_red)],
                     by = c("year", "biofuel_code", "comm_code"), all.x = TRUE)
setcolorder(agg_contrib, c("year", "biofuel_code", "comm_code",
                           "supply_init", "supply_new",
                           "contrib_init", "contrib_new", "share_red"))

# Inspect disproportionate reallocation directly, e.g. who absorbs palm (c074):
#   res[year == 2018 & biofuel_code == "c147" & comm_code == "c074"][order(-delta)]
# or one country's mix shift:
#   res[year == 2018 & biofuel_code == "c147" & iso3c == "ESP"][order(-supply_new)]


# -----------------------------------------------------------------------------
# 8. c901 ORIGIN ALLOCATION  (self-supply first, then proportional remainder)
# -----------------------------------------------------------------------------
# Per (year, biofuel) block, attribute each producing country's NEW c901 supply
# to ORIGIN countries with a self-supply PRIORITY (no trade data exists for waste):
#   T          = total block c901 supply (= sum of producers' supply_new)
#   need_o     = c901 used by country o (supply space)
#   budget_o   = share_red_o * T            (RED origin budget, the self cap)
#   (1) self_o = min(budget_o, need_o)      -> each country covers its own need first,
#                                              capped at its RED origin budget
#   (2) surplus origins (budget>need) spread their leftover budget across the
#       residual need of deficit consumers, proportional to that residual need.
# Margins are preserved EXACTLY: origin totals = budget_o (RED), dest totals = need_o.
# Since sum(budget)=sum(need)=T, sum(rem_budget)=sum(rem_need), so step (2) closes.
allocate_c901_origin <- function(res, gap_ctry) {
  # consumer need: c901 used by each producing country (supply space)
  need <- res[comm_code == C901, .(need = sum(supply_new)),
              by = .(year, biofuel_code, country = iso3c)]
  
  # origin budget: RED origin share * total block c901 supply
  org <- gap_ctry[comm_code == C901,
                  .(year, biofuel_code, country = origin_country_iso3, share_red)]
  org[, share_red := share_red / sum(share_red), by = .(year, biofuel_code)]
  tot <- need[, .(T = sum(need)), by = .(year, biofuel_code)]
  org <- merge(org, tot, by = c("year", "biofuel_code"), all.x = TRUE)
  org[, budget := share_red * T]
  
  # one node row per (block, country) carrying both need and budget
  nodes <- merge(need[, .(year, biofuel_code, country, need)],
                 org [, .(year, biofuel_code, country, budget)],
                 by = c("year", "biofuel_code", "country"), all = TRUE)
  nodes[is.na(need), need := 0][is.na(budget), budget := 0]
  
  # (1) self-supply (diagonal), capped at min(budget, need)
  nodes[, self       := pmin(budget, need)]
  nodes[, rem_budget := budget - self]   # > 0 only for surplus origins
  nodes[, rem_need   := need   - self]   # > 0 only for deficit consumers
  
  bad <- nodes[, .(need = sum(need), budget = sum(budget)),
               by = .(year, biofuel_code)][need > 1e-9 & budget <= 1e-9]
  if (nrow(bad))
    message(">>> c901: ", nrow(bad), " block(s) use c901 but have no RED origin ",
            "budget -> residual need left unallocated (inspect gap_ctry).")
  
  diag <- nodes[self > 0, .(year, biofuel_code, iso3c = country,
                            origin_country_iso3 = country, c901_by_origin = self)]
  
  # (2) proportional remainder: surplus budget -> deficit need
  R     <- nodes[, .(R = sum(rem_need)), by = .(year, biofuel_code)]
  sup_n <- nodes[rem_budget > 0, .(year, biofuel_code,
                                   origin_country_iso3 = country, rem_budget)]
  con_n <- nodes[rem_need   > 0, .(year, biofuel_code, iso3c = country, rem_need)]
  off   <- merge(sup_n, con_n, by = c("year", "biofuel_code"), allow.cartesian = TRUE)
  off   <- merge(off, R, by = c("year", "biofuel_code"), all.x = TRUE)
  off[, c901_by_origin := rem_budget * rem_need / R]
  off <- off[c901_by_origin > 0,
             .(year, biofuel_code, iso3c, origin_country_iso3, c901_by_origin)]
  
  rbind(diag, off, use.names = TRUE)[
    , .(c901_by_origin = sum(c901_by_origin)),
    by = .(year, biofuel_code, iso3c, origin_country_iso3)][]
}
c901_origin <- allocate_c901_origin(res, gap_ctry)





# -----------------------------------------------------------------------------
# 9. REVERSE: supply -> use, rebuild use_final_bcp (and c901_origin in use space)
# -----------------------------------------------------------------------------
# Forward was supply [kl] = use [t] * output_qty, so use = supply / output_qty.
# c901 "Other, Waste" now carries its own output_qty per biofuel in tcf_join
# (0.240/1.000/1.053 for c146/c147/c149), so no manual override is needed.

orig_cols <- names(use_raw)   # exact column set/order of use_final_bcp

# reverse-conversion factor table (real feedstocks + c901, all from tcf_join)
tcf_rev <- tcf_join

# metadata lookups to rebuild use_final_bcp columns (each unique on its join key)
proc_map <- unique(sup[, .(biofuel_code, proc, proc_code)], by = "biofuel_code")  # bf -> proc/code
item_map <- unique(use_raw[, .(comm_code, item, item_code)], by = "comm_code")    # comm -> item/code
area_map <- unique(regions[, .(iso3c, area_code = code, area = name)], by = "iso3c")


# rescaled supply -> use --------------------------------------------------------
use_resc <- merge(res[, .(year, biofuel_code, iso3c, comm_code, supply_new)],
                  tcf_rev, by = c("comm_code", "biofuel_code"), all.x = TRUE)

miss_rev <- use_resc[is.na(tcf) & supply_new > 0,
                     .(supply = sum(supply_new)), by = .(comm_code, biofuel_code)]
if (nrow(miss_rev))
  message(">>> reverse TCF missing for: ",
          paste(sprintf("%s/%s", miss_rev$comm_code, miss_rev$biofuel_code), collapse = ", "),
          " (incl. c901 if C901_TCF unset) -> use left NA, inspect `miss_rev`.")

use_resc[, use := supply_new / tcf]
use_resc <- use_resc[!is.na(use) & use > 0]              # drop zeros to match original sparsity

use_resc <- merge(use_resc, item_map, by = "comm_code",     all.x = TRUE)
use_resc <- merge(use_resc, proc_map, by = "biofuel_code",  all.x = TRUE)
use_resc <- merge(use_resc, area_map, by = "iso3c",         all.x = TRUE)
use_resc[, `:=`(unit = "tonnes", type = NA_character_)]     # TODO carry original `type` if needed
use_resc_final <- use_resc[, ..orig_cols]

# splice: keep original rows for non-rescaled (year, biofuel, country); replace rest
use_base <- copy(use_raw)
use_base[, biofuel_code := proc_to_bf(proc)]
use_base <- merge(use_base, regions[, .(code, iso3c)],
                  by.x = "area_code", by.y = "code", all.x = TRUE)
rescaled_keys <- unique(res[, .(year, biofuel_code, iso3c)])[, key := paste(year, biofuel_code, iso3c)]
use_base[, key := paste(year, biofuel_code, iso3c)]
use_kept <- use_base[!key %in% rescaled_keys$key]

use_final_bcp <- rbind(use_kept[, ..orig_cols], use_resc_final, use.names = TRUE)

# reconciliation: rebuilt total use vs supply-derived total (should match within rounding)
chk_use <- merge(
  use_final_bcp[, .(use_new = sum(use)), by = .(year, comm_code, area_code)],
  sup[, .(use_old = sum(use_qty)), by = .(year, comm_code, area_code)],
  by = c("year", "comm_code", "area_code"), all = TRUE)

# c901_origin in use space (join the per-biofuel c901 factor from tcf_rev) -------
c901_tcf_rev <- tcf_rev[comm_code == C901, .(biofuel_code, c901_tcf = tcf)]
c901_origin <- merge(c901_origin, c901_tcf_rev, by = "biofuel_code", all.x = TRUE)
c901_origin[, use_by_origin := c901_by_origin / c901_tcf]



# -----------------------------------------------------------------------------
# 10. c145 (UCO) ORIGIN ALLOCATION  (btd-based, rescaled to the rescaled use)
# -----------------------------------------------------------------------------
# Unlike c901 (no trade data -> synthetic priority rule), UCO has REAL bilateral
# flows in btd_final (self-flows = domestic supply, included). Use btd as the
# origin x destination PRIOR and rescale each destination's inflow to match the
# rescaled biofuel UCO use (use_final_bcp). btd is total-UCO trade (all uses), so
# the per-destination scale a/b (<= 1) carves out the biofuel-relevant slice while
# keeping each destination's actual sourcing mix.
#   (a) dest_use_c = rescaled c145 use by consuming country c   (from use_final_bcp)
#   (b) b_in_c     = sum_o btd[o,c]   (total UCO into c, incl. domestic self-flow)
#   flow[o->c]     = btd[o,c] * a_c / b_c ; then split across biofuels by c's bf-mix
# Destinations with NO btd inflow (b_c = 0) cannot be sourced from btd -> FALLBACK:
# distribute their use over origins by the gap_ctry RED origin shares (share_red,
# biofuel-specific). This is the ONLY use of gap_ctry here (no double-margin RAS).
# Both paths meet destination (a) exactly; origin totals follow btd where available
# and RED where not. `c145_origin_target` (RED origin totals in use space) is kept
# below for reference / reconciliation only.
C145     <- "c145"
bf_procs <- c("Biodiesel production", "Renewable diesel production", "Biogasoline production")

# (i) RED origin TARGET in use space, from gap_ctry  (reference; parallels c901 tcf)
#     origin_supply = share_red * total block c145 supply ;  origin_use = / tcf
c145_origin_target <- {
  org <- gap_ctry[comm_code == C145, .(year, biofuel_code, origin_country_iso3, share_red)]
  org[, share_red := share_red / sum(share_red), by = .(year, biofuel_code)]
  tot_sup <- res[comm_code == C145, .(tot_supply = sum(supply_new)), by = .(year, biofuel_code)]
  org <- merge(org, tot_sup, by = c("year", "biofuel_code"), all.x = TRUE)
  org <- merge(org, tcf_rev[comm_code == C145, .(biofuel_code, tcf)],
               by = "biofuel_code", all.x = TRUE)
  org[, origin_use_target := share_red * tot_supply / tcf]
  org[, .(year, biofuel_code, origin_country_iso3, origin_use_target)]
}

# (ii) destination use (a): rescaled c145 use per (year, biofuel, consuming country)
c145_dest <- use_final_bcp[comm_code == C145 & proc %in% bf_procs, .(year, area_code, proc, use)]
c145_dest[, biofuel_code := proc_to_bf(proc)]
c145_dest <- merge(c145_dest, regions[, .(code, iso3c)],
                   by.x = "area_code", by.y = "code", all.x = TRUE)
c145_dest <- c145_dest[, .(dest_use = sum(use)), by = .(year, biofuel_code, iso3c)]

# (iii) bilateral prior (b): btd c145 flows, self-flows included
btd_final <- as.data.table(readRDS("data/btd_final.rds"))
btd145 <- btd_final[comm_code == C145, .(year, from_code, to_code, value)]
rm(btd_final)
btd145 <- merge(btd145, regions[, .(code, from_iso = iso3c)],
                by.x = "from_code", by.y = "code", all.x = TRUE)
btd145 <- merge(btd145, regions[, .(code, to_iso = iso3c)],
                by.x = "to_code", by.y = "code", all.x = TRUE)

# (iv) allocate per destination, two paths (both meet destination use (a) exactly):
#      (A) destinations WITH btd inflow -> rescale btd's origin mix to (a);
#      (B) destinations with NO btd inflow -> distribute (a) over origins by the
#          gap_ctry RED origin shares (the only place gap_ctry is used; no
#          double-margin RAS). RED shares are biofuel-specific, so (B) needs no
#          separate bf split. Split (A) across biofuels by each destination's mix.
allocate_c145_origin <- function(btd145, c145_dest, gap_ctry) {
  b_in <- btd145[, .(b_in = sum(value)), by = .(year, to_iso)]              # (b) total inflow
  a_in <- c145_dest[, .(a_in = sum(dest_use)), by = .(year, iso3c)]         # (a) biofuel use
  
  sc <- merge(a_in, b_in, by.x = c("year", "iso3c"),
              by.y = c("year", "to_iso"), all.x = TRUE)
  sc[is.na(b_in), b_in := 0]
  no_btd <- sc[b_in == 0, .(year, iso3c)]                  # -> RED-share fallback
  if (nrow(no_btd))
    message(">>> c145: ", nrow(no_btd), " (year,dest) use UCO but have NO btd inflow ",
            "-> RED-share fallback (gap_ctry). e.g. ",
            paste(utils::head(unique(no_btd$iso3c), 5), collapse = ", "))
  
  # ---- (A) btd path: rescale each btd-sourced destination's inflow to meet (a) --
  scl <- merge(b_in, a_in, by.x = c("year", "to_iso"),
               by.y = c("year", "iso3c"))                  # inner: b_in>0 AND a_in>0
  scl[, scale := a_in / b_in]
  over <- scl[scale > 1 + 1e-9]
  if (nrow(over))
    message(">>> c145: ", nrow(over), " (year,dest) need MORE UCO than btd records ",
            "(scale>1) -> btd flows scaled up. e.g. ",
            paste(utils::head(over$to_iso, 5), collapse = ", "))
  flow <- merge(btd145, scl[, .(year, to_iso, scale)], by = c("year", "to_iso"))  # inner
  flow[, use_scaled := value * scale]                      # meets a_in per destination
  flow <- flow[use_scaled > 0,
               .(year, iso3c = to_iso, origin_country_iso3 = from_iso, use_scaled)]
  w <- copy(c145_dest)
  w[, w := dest_use / sum(dest_use), by = .(year, iso3c)]  # destination's bf-use mix
  out_btd <- merge(flow, w[, .(year, biofuel_code, iso3c, w)],
                   by = c("year", "iso3c"), allow.cartesian = TRUE)
  out_btd[, c145_use_by_origin := use_scaled * w]
  out_btd <- out_btd[, .(year, biofuel_code, iso3c, origin_country_iso3, c145_use_by_origin)]
  
  # ---- (B) RED-share fallback: no btd inflow -> distribute (a) by gap_ctry ------
  red <- gap_ctry[comm_code == C145, .(year, biofuel_code, origin_country_iso3, share_red)]
  red[, share_red := share_red / sum(share_red), by = .(year, biofuel_code)]
  dest_nb <- merge(c145_dest, no_btd, by = c("year", "iso3c"))   # use of fallback dests
  out_red <- merge(dest_nb, red, by = c("year", "biofuel_code"), allow.cartesian = TRUE)
  out_red[, c145_use_by_origin := dest_use * share_red]
  out_red <- out_red[, .(year, biofuel_code, iso3c, origin_country_iso3, c145_use_by_origin)]
  
  red_blocks  <- unique(red[, .(year, biofuel_code)])          # blocks with origin shares
  n_unmatched <- nrow(dest_nb[!red_blocks, on = c("year", "biofuel_code")])
  if (n_unmatched > 0)
    message(">>> c145: ", n_unmatched,
            " fallback (year,biofuel,dest) have no gap_ctry c145 origin share ",
            "-> left unattributed.")
  
  rbind(out_btd, out_red, use.names = TRUE)[
    c145_use_by_origin > 0,
    .(c145_use_by_origin = sum(c145_use_by_origin)),
    by = .(year, biofuel_code, iso3c, origin_country_iso3)][]
}
c145_origin <- allocate_c145_origin(btd145, c145_dest, gap_ctry)


###########################################################
########### MAKING UPDATED TABLES #########
###########################################################

# Collapse the per-biofuel origin allocations to bilateral (origin -> dest) flows
# in USE/mass space (tonnes), self-flows (origin == dest) included.
#   c145 : top up btd ONLY where the biofuel estimate exceeds the existing flow
#          (new = max(old, est)). Flows the estimate would LOWER keep their original
#          total (the biofuel share of them is implicit). -> data/btd_final_resc.rds
#   c901 : NOT merged into btd_final. Saved as a standalone `waste_flows` object
#          (btd_final schema, self-flows incl.) in intermediate_data/.
# Outputs use NEW names so btd_final.rds / cbs_sua_adjusted.rds are never mutated in
# place across scripts (keeps this step idempotent on re-run).
reg_code <- regions[, .(iso3c, code)]

bilateralise <- function(dt, val_col) {
  b <- dt[, .(value = sum(get(val_col))), by = .(year, origin_country_iso3, iso3c)]
  b <- merge(b, reg_code, by.x = "origin_country_iso3", by.y = "iso3c", all.x = TRUE)
  setnames(b, "code", "from_code")
  b <- merge(b, reg_code, by = "iso3c", all.x = TRUE)
  setnames(b, "code", "to_code")
  drop <- b[is.na(from_code) | is.na(to_code), sum(value)]
  if (drop > 0)
    message(sprintf(">>> %s: %.0f t dropped (origin/dest iso3c not mapped to area code).",
                    val_col, drop))
  b[!is.na(from_code) & !is.na(to_code),
    .(year = as.integer(year), from_code = as.integer(from_code),
      to_code = as.integer(to_code), value)]
}

c145_bil <- bilateralise(c145_origin, "c145_use_by_origin")   # estimated biofuel UCO flows
c901_bil <- bilateralise(c901_origin, "use_by_origin")        # estimated waste flows

ITEM_C145 <- 1274L
ITEM_C901 <- items[comm_code == "c901", item_code][1]
if (is.na(ITEM_C901))
  message(">>> c901 has no item_code in items -> waste_flows stored with item_code = NA ",
          "(c901 is keyed on comm_code downstream).")

# --- 2a. c145: top up btd_final where estimate > existing flow (never lower) ----
btd_final <- as.data.table(readRDS("data/btd_final.rds"))

old145 <- btd_final[comm_code == "c145", .(year, from_code, to_code, old = value)]
m145   <- merge(old145, c145_bil[, .(year, from_code, to_code, est = value)],
                by = c("year", "from_code", "to_code"), all.x = TRUE)
m145[is.na(est), est := 0]
m145[, `:=`(new = pmax(old, est), comm_code = "c145")]

message(sprintf("c145 btd: %d flows raised (est>old), +%.0f t topped up; %.0f t of estimates below existing flow left unchanged.",
                m145[new > old, .N], m145[, sum(new - old)], m145[est < old, sum(old - est)]))

# write only the higher value back onto the existing dense c145 block
btd_final[m145, on = c("year", "from_code", "to_code", "comm_code"), value := i.new]



# --- 2b. c901: standalone waste_flows (btd schema, self-flows included) ---------
waste_flows <- copy(c901_bil)
waste_flows[, `:=`(item_code = as.integer(ITEM_C901), comm_code = "c901")]
setcolorder(waste_flows, c("from_code", "to_code", "value", "year", "item_code", "comm_code"))


# --- 2c. cbs_sua_adjusted: add the c145 top-up to item 1274 imports/exports -----
# Only the RAISED part (max(0, est-old), cross-border) is added; original 1274
# imports/exports are preserved. SUA identity left to the downstream rebalancing.
topup <- m145[from_code != to_code & new > old, .(year, from_code, to_code, d = new - old)]
dimp  <- topup[, .(d_imports = sum(d)), by = .(year, area_code = to_code)]
dexp  <- topup[, .(d_exports = sum(d)), by = .(year, area_code = from_code)]
dimp[, item_code := ITEM_C145]; dexp[, item_code := ITEM_C145]

cbs_sua_adjusted <- as.data.table(readRDS("intermediate_data/cbs_sua_adjusted.rds"))
# align key types to cbs_sua_adjusted (year/area_code numeric, item_code integer)
for (D in list(dimp, dexp))
  D[, `:=`(year = as.numeric(year), area_code = as.numeric(area_code),
           item_code = as.integer(item_code))]
cbs_sua_adjusted[dimp, on = .(year, area_code, item_code), imports := fcoalesce(imports, 0) + i.d_imports]
cbs_sua_adjusted[dexp, on = .(year, area_code, item_code), exports := fcoalesce(exports, 0) + i.d_exports]


###########################################################
########### SAVING TABLES #########
###########################################################

setwd("/home/mmondolfo/fabio_bfp/")

# (1) the RED-rescaled use table -----------------------------------------------
saveRDS(use_final_bcp,  "data/use_final_bcp.rds")
saveRDS(btd_final, "data/btd_final_resc.rds")     # NB new name; btd_final.rds untouched
saveRDS(cbs_sua_adjusted, "intermediate_data/cbs_sua_adjusted_resc.rds")   # NB new name; original untouched
saveRDS(waste_flows, "data/waste_flows.rds")

rm(list = ls())