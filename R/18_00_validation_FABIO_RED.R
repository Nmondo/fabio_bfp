## --- portable repo root: FABIO_BFP_ROOT override, else walk up to the repo marker ---
fabio_root <- Sys.getenv("FABIO_BFP_ROOT", unset = "")
if (!nzchar(fabio_root)) {
  fabio_root <- getwd()
  while (!file.exists(file.path(fabio_root, "R", "00_system_variables.R")) &&
         dirname(fabio_root) != fabio_root) fabio_root <- dirname(fabio_root)
  if (!file.exists(file.path(fabio_root, "R", "00_system_variables.R")))
    stop("Repo root not found above ", getwd(), " - set FABIO_BFP_ROOT or run from inside the repo.")
}
setwd(fabio_root)
setwd(fabio_root)


#############################################################################
########### LOADING PACKAGES #################
#############################################################################

library(patchwork)
library(Matrix); library(data.table); library(dplyr)

## Shared feedstock colour palette (fixed name -> colour map + get_feedstock_palette()).
## Provides feedstock_color_map, feedstock_meta, get_feedstock_palette(), continent_palette.
source("R/19_plot_definitions.R")


################################################################################
##  1.  LOAD MODELS & INPUT DATA
################################################################################

# Read labels ------------------------------------------------------------------
input_path <- "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/"
regions <- fread(file="inst/regions_full.csv")[current==TRUE]

allocation <- "value"

## ---------------------------------------------------------------------------
## Load BOTH model versions in parallel:
##   "resc" = rescaled (RED-aligned, default paths)          -> fabio_resc
##   "init" = non-rescaled / bypass (pre-08_03, `bypass/`)   -> fabio_init
## The bypass artefacts are produced by the run-mode toggle (FABIO_RUN_MODE),
## living under <v2_bcp>/bypass/... with btd tagged `_noresc`. The MRIO label
## STRUCTURE (io/fd) is identical across versions — only the values (Z/Y/btd)
## differ — but we still load each version's own labels for internal consistency.
## X and ex are not used downstream, so they are not loaded.
## ---------------------------------------------------------------------------
load_model <- function(scenario = c("resc", "init")) {
  scenario <- match.arg(scenario)
  base <- sub("/+$", "", input_path)
  if (scenario == "init") base <- file.path(base, "bypass")
  io_s <- fread(file.path(base, "io_labels.csv"));            setDT(io_s)
  fd_s <- fread(file.path(base, "losses/fd_labels.csv"));     setDT(fd_s)
  Y_s  <- readRDS(file.path(base, "losses/Y.rds"))
  Z_s  <- readRDS(file.path(base, "losses", paste0("Z_", allocation, ".rds")))
  btd_s <- if (scenario == "init") readRDS("data/btd_final_merged_noresc.rds")
  else                    readRDS("data/btd_final_merged.rds")
  list(io = io_s, fd = fd_s, Y = Y_s, Z = Z_s, btd = btd_s)
}

m_resc <- load_model("resc")
m_init <- load_model("init")

## primary handles used by the shared downstream code (label lookups, red_empirical);
## structure is version-invariant, so the rescaled labels stand in for both.
io <- m_resc$io; fd <- m_resc$fd
items <- fread(file="inst/items_full_bcp.csv")[comm_code %in% unique(io$comm_code)]

# Read empirical data from RED
red_data <- readRDS("own_data/feedstock_origin_ts.rds")
red_data_imputed <- readRDS("own_data/feedstock_origin_ts_imputed.rds")
red_data <- c(red_data, red_data_imputed)


years         <- as.character(2012:2022)
biofuel_codes <- c("c146","c147","c149")
target_iso3   <- c("SWE","NOR","GBR","IRL", "FRA", "DEU", "NLD", "ESP")

setDT(io); setDT(fd)


################################################################################
##  2.  TECHNICAL CONVERSION FACTORS (TCF)
################################################################################

# Loading TCFs
tcf <- readRDS("intermediate_data/tcf_table_final.rds")

tcf$item <- ifelse(tcf$item == " Triticale", "Triticale", tcf$item)

# Formatting for this specific case
tcf <- tcf %>% left_join(items[,c("item", "comm_code")], 
                         by = "item")
tcf <- tcf %>% mutate(biofuel_code = case_when(grepl("Biogasoline", proc) ~ "c146",
                                               grepl("Biodiesel", proc) ~ "c147", 
                                               grepl("Renewable diesel", proc) ~ "c149",
                                               TRUE ~ NA)) %>%
  filter(! item %in% c("Oilcrops Oil, Other", "Total"))

setDT(tcf)


################################################################################
##  3.  RECONSTRUCT A*Y  (origin resolution, both model versions)
################################################################################

## share[i (true origin), j (consumer)] for one (year, biofuel)
## regions: data.table(code, iso3c) matching btd's from_code/to_code.
##          codes outside the model set map to NA -> folded to "ROW".
EU_ISO3 <- regions[continent %in% c("EU", "EUR"), iso3c]

selected_iso3 <- function(bf) switch(bf,
                                     "c146" = EU_ISO3,
                                     "c147" = union(EU_ISO3, c("IDN", "CHN")),
                                     "c149" = union(EU_ISO3, c("SGP", "CHN")),
                                     stop("unknown biofuel ", bf))


## ---------------------------------------------------------------------------
## 2012-2013: restricted-country rescaling set
## ---------------------------------------------------------------------------
## From 2014 on we rescale the WHOLE (EU + capped partners) feedstock mix, so the
## producer set is `selected_iso3(bf)`. For 2012-2013 we instead rescale ONLY the
## countries that actually carry empirical RED feedstock data that year.
##
## This block is defined HERE — before compute_AY_origin is *called* (AY_resc /
## AY_init below, and the fabio_fs_* extension in §6) — because the nonS split
## inside compute_AY_origin now references rescale_producer_set(). It is built
## from the RED sources ONLY (feedstock_origin ts + no-origin detail), which are
## FABIO-independent, so the country set is available this early.
##
## `country` here is the CONSUMER; it is reinterpreted as the PRODUCER restriction
## in 08_03 (intersect with selected_iso3). For EU members these are the same ISO
## codes, so the two uses are coherent.
early_years <- c(2012L, 2013L)
bf3         <- c("c146", "c147", "c149")

.red_ctry_main <- rbindlist(lapply(red_data, `[[`, "feedstock_origin"),
                            use.names = TRUE, fill = TRUE)[
                              , .(q = sum(quantity, na.rm = TRUE)),
                              by = .(year = as.integer(year), biofuel_code, country)]

.red_ctry_extra <- rbindlist(
  readRDS("own_data/feedstock_detail_no_origin.rds"), use.names = TRUE, fill = TRUE)[
    , .(q = sum(quantity, na.rm = TRUE)),
    by = .(year = as.integer(year), biofuel_code, country)]

countries_1213 <- unique(rbindlist(list(.red_ctry_main, .red_ctry_extra),
                                   use.names = TRUE, fill = TRUE)[
                                     year %in% early_years & biofuel_code %in% bf3 & q > 0,
                                     .(year, biofuel_code, country)])
setorder(countries_1213, year, biofuel_code, country)
saveRDS(countries_1213, "inst/gap_empirical_countries_1213_rescale.rds")

## year-aware rescaling / nonS set: full set from 2014, restricted for 2012-2013.
## A (2012/2013, bf) with no restricted country -> character(0): then nonS_prod
## = all producers, fabio_nonS = full trace, red_resid clamps to 0, and 08_03
## skips the block. Everything degrades coherently.
rescale_producer_set <- function(bc, yr) {
  base <- selected_iso3(bc)
  if (yr %in% early_years) {
    keep <- countries_1213[year == yr & biofuel_code == bc, unique(country)]
    return(if (length(keep)) intersect(base, keep) else character(0))
  }
  base
}


resolve_origin <- function(Tmat, P, leak = 1e-6) {
  diag(Tmat) <- 0                                  # (1) self-trade is domestic, not export
  exports <- rowSums(Tmat); imports <- colSums(Tmat)
  
  ## (2) feasibility: a net exporter must produce at least its net exports,
  ##     else rowSums(B) > 1 and (I - B) is singular
  P <- pmax(P, exports - imports)
  g <- P + imports
  
  ## (3) strictly positive domestic leak so ||B||_inf < 1 (keeps g = P + imports)
  tight <- exports > g * (1 - leak)
  if (any(tight)) {
    add <- pmax(exports[tight] * leak, .Machine$double.eps)
    P[tight] <- P[tight] + add; g[tight] <- g[tight] + add
  }
  g[g <= 0] <- .Machine$double.eps
  
  B <- Tmat / g                                    # now max rowSum < 1 -> well-conditioned
  N <- solve(diag(nrow(B)) - B)
  S <- sweep(P * N, 2, g, "/")
  dimnames(S) <- dimnames(Tmat); S
}

compute_AY_origin <- function(years, biofuel_codes, target_iso3,
                              Z, Y, io, fd, tcf, btd, regions,
                              renormalise_missing = TRUE,
                              split_nonS = TRUE) {        # NEW
  
  code2iso <- setNames(regions$iso3c, as.character(regions$code))
  if (!identical(key(btd), c("comm_code","year"))) setkey(btd, comm_code, year)
  
  target_cols <- fd[, which(iso3c %in% target_iso3 & fd == "fuel")]
  
  rbindlist(lapply(years, function(t) {
    ty <- as.character(t)
    if (is.null(Y[[ty]]) || is.null(Z[[ty]])) {
      warning("no Y/Z for year ", ty, " — skipped"); return(NULL)
    }
    Yt <- Y[[ty]]; Zt <- Z[[ty]]
    
    rbindlist(lapply(biofuel_codes, function(bc) {
      
      cidx     <- io[, which(comm_code == bc)]
      prod_iso <- io$iso3c[cidx]
      
      oqmap <- tcf[biofuel_code == bc]
      oq    <- oqmap$output_qty[match(io$comm_code, oqmap$comm_code)]; oq[is.na(oq)] <- 0
      Zc    <- Zt[, cidx, drop = FALSE]
      P_Z   <- as.numeric(crossprod(oq, Zc)); keep <- P_Z > 0
      Zc <- Zc[, keep, drop = FALSE]; P_Z <- P_Z[keep]; prod_iso <- prod_iso[keep]
      if (!length(P_Z)) return(NULL)
      A_dom <- Zc %*% Diagonal(x = 1 / P_Z); colnames(A_dom) <- prod_iso
      
      tr <- btd[.(bc, as.numeric(t))]
      tr[, `:=`(from_iso = code2iso[as.character(from_code)],
                to_iso   = code2iso[as.character(to_code)])]
      tr[is.na(from_iso), from_iso := "ROW"]; tr[is.na(to_iso), to_iso := "ROW"]
      tr <- tr[from_iso != to_iso & value > 0,
               .(qty = sum(value)), by = .(from_iso, to_iso)]
      
      ctry <- sort(unique(c(tr$from_iso, tr$to_iso, prod_iso, fd$iso3c[target_cols])))
      Tm <- matrix(0, length(ctry), length(ctry), dimnames = list(ctry, ctry))
      Tm[cbind(match(tr$from_iso, ctry), match(tr$to_iso, ctry))] <- tr$qty
      P  <- setNames(numeric(length(ctry)), ctry); P[prod_iso] <- P_Z
      S  <- resolve_origin(Tm, P)
      
      Cmod <- tapply(colSums(Yt[cidx, target_cols, drop = FALSE]),
                     fd$iso3c[target_cols], sum)
      
      j_iso <- intersect(colnames(S), names(Cmod))
      if (!length(j_iso)) return(NULL)
      Sij   <- S[prod_iso, j_iso, drop = FALSE]
      if (renormalise_missing) { cs <- colSums(Sij); cs[cs == 0] <- 1
      Sij <- sweep(Sij, 2, cs, "/") }
      D <- Sij * rep(Cmod[j_iso], each = length(prod_iso))
      if (is.null(dim(D)))                                   # guard: single consumer
        D <- matrix(D, nrow = length(prod_iso), dimnames = list(prod_iso, j_iso))
      
      ## matmul -> long data.table, tagged by `source`
      emit <- function(Msub, src) {
        s <- summary(as(drop0(Msub), "TsparseMatrix"))
        if (!nrow(s)) return(NULL)
        data.table(year = t, fd_cat = "fuel", biofuel_code = bc, source = src,
                   io_row = s$i, country = j_iso[s$j], value = s$x)
      }
      
      ## (all producers)  -- the existing trace
      out_all <- emit(A_dom[, prod_iso, drop = FALSE] %*% D, "all")
      
      ## (non-rescaled producers only) -- feedstock embodied in biodiesel produced
      ## OUTSIDE the rescaling set: the "pre-committed" mass the rescaled countries
      ## must NOT be forced to reproduce. The boundary is YEAR-AWARE: for 2014+ it
      ## is everything outside selected_iso3(bc); for 2012-2013 it is everything
      ## outside the RESTRICTED set — i.e. outside-EU imports PLUS EU members not in
      ## the restricted RED-data set — so fabio_nonS travels with the rescaling set.
      out_nonS <- NULL
      if (split_nonS) {
        nonS_prod <- setdiff(prod_iso, rescale_producer_set(bc, as.integer(t)))
        if (length(nonS_prod))
          out_nonS <- emit(A_dom[, nonS_prod, drop = FALSE] %*%
                             D[nonS_prod, , drop = FALSE], "nonS")
      }
      
      res <- rbindlist(list(out_all, out_nonS), use.names = TRUE)
      if (!nrow(res)) return(NULL)
      res
    }))
  }))
}                     

## Turn a raw compute_AY_origin() output into the origin-level long table with
## fabio_value (all producers) + fabio_nonS (non-rescaling-set producers). io_s is
## the version's own label table (io_row indexes into it).
finalize_AY <- function(AY, io_s) {
  AY[, `:=`(origin_country_iso3 = io_s$iso3c[io_row],
            comm_code           = io_s$comm_code[io_row],
            item                = io_s$item[io_row])][, io_row := NULL]
  AY <- merge(AY, tcf[, .(comm_code, biofuel_code, output_qty)],
              by = c("comm_code","biofuel_code"), all.x = TRUE)
  AY[, value := value * output_qty][, output_qty := NULL]
  AY <- dcast(AY,
              year + country + fd_cat + biofuel_code + comm_code + item + origin_country_iso3 ~ source,
              value.var = "value", fun.aggregate = sum, fill = 0)
  if (!"nonS" %in% names(AY)) AY[, nonS := 0]
  if (!"all"  %in% names(AY)) AY[, all  := 0]
  setnames(AY, c("all", "nonS"), c("fabio_value", "fabio_nonS"))
  AY[]
}

## Compute A*Y origin reconstruction for BOTH model versions.
AY_resc <- finalize_AY(
  compute_AY_origin(years, biofuel_codes, target_iso3,
                    m_resc$Z, m_resc$Y, m_resc$io, m_resc$fd, tcf, m_resc$btd, regions),
  m_resc$io)
AY_init <- finalize_AY(
  compute_AY_origin(years, biofuel_codes, target_iso3,
                    m_init$Z, m_init$Y, m_init$io, m_init$fd, tcf, m_init$btd, regions),
  m_init$io)


################################################################################
##  4.  RED EMPIRICAL  (harmonised to the FABIO schema)
################################################################################

# red_empirical: fallback to feedstock_std when comm_code is NA
red_empirical <- rbindlist(lapply(red_data, `[[`, "feedstock_origin"), use.names = TRUE)
red_empirical[, `:=`(
  fd_cat              = "fuel",
  origin_country_iso3 = unname(origin_country_iso3),
  value               = quantity
)]
# 1) try the io lookup first
red_empirical[, item := io$item[match(comm_code, io$comm_code)]]
# 2) fall back to feedstock_std wherever the lookup failed
red_empirical[is.na(item), item := as.character(unname(feedstock_std))]
red_empirical <- red_empirical[, .(year, country, fd_cat, biofuel_code, comm_code,
                                   item, origin_country_iso3, value)]

red_empirical <- red_empirical[
  , .(value = sum(value, na.rm = TRUE)),
  by = .(year, country, fd_cat, biofuel_code, comm_code, item, origin_country_iso3)
]


# remap "Oil, palm fruit" -> c074 / "Palm Oil", then re-aggregate
red_empirical[item %in% c("Oil, palm fruit", "PFAD"), `:=`(
  comm_code = "c074",
  item      = "Palm Oil"
)]

# remap mislabelled maize: "Maize and products"/c004 under BIODIESEL & RENEWABLE DIESEL is Maize Germ Oil -> c079. 
red_empirical[comm_code == "c004" & biofuel_code %in% c("c147", "c149"), `:=`(
  comm_code = "c079",
  item      = "Maize Germ Oil"
)]

red_empirical <- red_empirical[
  , .(value = sum(value, na.rm = TRUE)),
  by = .(year, country, fd_cat, biofuel_code, comm_code, item, origin_country_iso3)
]


################################################################################
##  5.  COMPARISON DATASET  (cmp)
################################################################################

key_cols <- c("year", "country", "biofuel_code", "comm_code", "origin_country_iso3")

harmonise <- function(dt) {
  dt[, year := as.integer(year)]                       # year -> integer
  for (col in setdiff(key_cols, "year"))               # the rest -> character
    if (!is.character(dt[[col]])) dt[, (col) := as.character(get(col))]
  invisible(dt)
}

harmonise(AY_init)
harmonise(AY_resc)
harmonise(red_empirical)

## FABIO both versions, merged on the 5 origin keys (item coalesced afterwards).
## fabio_nonS is taken from the NON-rescaled (init) version — it is the quantity
## produced outside the rescaling set that fed the initial comparison / rescaling.
AY_init2 <- AY_init[, .(year, country, biofuel_code, comm_code, origin_country_iso3,
                        item, fabio_init = fabio_value, fabio_nonS)]
AY_resc2 <- AY_resc[, .(year, country, biofuel_code, comm_code, origin_country_iso3,
                        item, fabio_resc = fabio_value)]

fab_both <- merge(AY_init2, AY_resc2, by = key_cols, all = TRUE)
fab_both[, item := fcoalesce(item.x, item.y)][, c("item.x","item.y") := NULL]

keys_red <- unique(red_empirical[, .(year, country, biofuel_code)])
fab_f <- fab_both[keys_red, on = .(year, country, biofuel_code), nomatch = NULL]

cmp <- merge(
  fab_f[,         .(year, country, biofuel_code, comm_code, item, origin_country_iso3,
                    fabio_init, fabio_resc, fabio_nonS)],
  red_empirical[, .(year, country, biofuel_code, comm_code, item, origin_country_iso3,
                    red_value = value)],
  by  = key_cols,
  all = TRUE
)

cmp[, item := fcoalesce(item.x, item.y)][, c("item.x","item.y") := NULL]
cmp[is.na(fabio_init), fabio_init := 0]
cmp[is.na(fabio_resc), fabio_resc := 0]
cmp[is.na(fabio_nonS), fabio_nonS := 0]
cmp[, fabio_init := fabio_init / 1e3]
cmp[, fabio_resc := fabio_resc / 1e3]
cmp[, fabio_nonS := fabio_nonS / 1e3]        # SAME scaling -> M liters
cmp[, red_value  := red_value / 1e6]
cmp[is.na(red_value), red_value := 0]
cmp[comm_code == "NA", comm_code := NA]

cmp <- cmp[year <= 2022 & biofuel_code %in% c("c146", "c147", "c149")]

cmp[item == "Animal or vegetable fats and oils and their fractions, chemically modified, except those hydrogenated, inter-esterified, re-esterified or elaidinized; inedible mixtures or preparations of animal or vegetable fats or oils",
    item := "UCO"]

cmp[regions, continent := i.continent, on = c(origin_country_iso3 = "iso3c")]


################################################################################
##  6.  EXTENSION: ITA / DEU / AUT  (cmp_extra)  [prior for the extra plots]
################################################################################
#
# Adds ITALY and (2012-2021, low-res) GERMANY + AUSTRIA to the
# FEEDSTOCK-LEVEL comparison. Both lack RED origin detail, so they are built
# as a parallel `cmp_extra` (same schema as `cmp` minus origin/continent) and
# fed to plot_cmp_country_biofuel_item_ts() (which never uses origin) and,
# for DEU, folded into the global item-bar plot via `extra=`.
# Built here, ABOVE the plot functions, so the extra runs have their prior.
# PREREQUISITE: source robustness_check_FABIO_RED.R first, same session.

extra_iso3 <- c("ITA", "DEU", "AUT")

## 1. FABIO side: compute_AY_origin for BOTH versions, then aggregate origin away.
finalize_AY_extra <- function(AY, io_s) {
  AY[, `:=`(comm_code = io_s$comm_code[io_row],
            item      = io_s$item[io_row])][, io_row := NULL]
  AY <- merge(AY, tcf[, .(comm_code, biofuel_code, output_qty)],
              by = c("comm_code","biofuel_code"), all.x = TRUE)
  AY[, value := value * output_qty][, output_qty := NULL]
  fs <- dcast(AY, year + country + biofuel_code + comm_code + item ~ source,
              value.var = "value", fun.aggregate = sum, fill = 0)
  if (!"nonS" %in% names(fs)) fs[, nonS := 0]
  if (!"all"  %in% names(fs)) fs[, all  := 0]
  setnames(fs, c("all", "nonS"), c("fabio_value", "fabio_nonS"))
  fs[, year := as.integer(year)]
  fs[]
}

fabio_fs_init <- finalize_AY_extra(
  compute_AY_origin(years, biofuel_codes, extra_iso3,
                    m_init$Z, m_init$Y, m_init$io, m_init$fd, tcf, m_init$btd, regions),
  m_init$io)
fabio_fs_resc <- finalize_AY_extra(
  compute_AY_origin(years, biofuel_codes, extra_iso3,
                    m_resc$Z, m_resc$Y, m_resc$io, m_resc$fd, tcf, m_resc$btd, regions),
  m_resc$io)

fabio_fs <- merge(
  fabio_fs_init[, .(year, country, biofuel_code, comm_code, item,
                    fabio_init = fabio_value, fabio_nonS)],
  fabio_fs_resc[, .(year, country, biofuel_code, comm_code, item,
                    fabio_resc = fabio_value)],
  by = c("year", "country", "biofuel_code", "comm_code", "item"), all = TRUE
)
fabio_fs[is.na(fabio_init), fabio_init := 0]
fabio_fs[is.na(fabio_resc), fabio_resc := 0]
fabio_fs[is.na(fabio_nonS), fabio_nonS := 0]

## 2. RED side (no origin). ITA from export; DEU = region (2012-2021) stitched
##    with origin-resolved DEU (2022-2024) from red_empirical.
detail <- readRDS("own_data/feedstock_detail_no_origin.rds")    # list(ITA, DEU)
red_fs  <- rbindlist(detail, use.names = TRUE)
red_fs[, item := io$item[match(comm_code, io$comm_code)]]
red_fs[is.na(item), item := as.character(feedstock_std)]
red_fs[, value := quantity]
red_fs[item %in% c("Oil, palm fruit", "PFAD"),
       `:=`(comm_code = "c074", item = "Palm Oil")]
red_fs <- red_fs[, .(red_value = sum(value, na.rm = TRUE)),
                 by = .(year, country, biofuel_code, comm_code, item)]
red_fs[, year := as.integer(year)]

deu_detailed <- red_empirical[country == "DEU",
                              .(red_value = sum(value, na.rm = TRUE)),
                              by = .(year, country, biofuel_code, comm_code, item)]
deu_detailed[, year := as.integer(year)]
det_years <- unique(deu_detailed$year)
red_fs <- red_fs[!(country == "DEU" & year %in% det_years)]
red_fs <- rbindlist(list(red_fs, deu_detailed), use.names = TRUE)

## 3. cmp_extra: same schema as cmp minus origin/continent. Merge on
##    (year, country, biofuel_code, comm_code), coalesce item; scale FABIO /1e3,
##    RED /1e6 (as in the main script).
cmp_extra <- merge(
  fabio_fs[, .(year, country, biofuel_code, comm_code, item_f = item,
               fabio_init, fabio_resc, fabio_nonS)],
  red_fs  [, .(year, country, biofuel_code, comm_code, item_r = item, red_value)],
  by = c("year", "country", "biofuel_code", "comm_code"), all = TRUE
)
cmp_extra[, item := fcoalesce(item_f, item_r)][, c("item_f", "item_r") := NULL]
cmp_extra[is.na(fabio_init), fabio_init := 0]
cmp_extra[is.na(fabio_resc), fabio_resc := 0]
cmp_extra[is.na(fabio_nonS), fabio_nonS := 0]
cmp_extra[is.na(red_value),  red_value  := 0]
cmp_extra[, fabio_init := fabio_init / 1e3]
cmp_extra[, fabio_resc := fabio_resc / 1e3]
cmp_extra[, fabio_nonS := fabio_nonS / 1e3]
cmp_extra[, red_value  := red_value  / 1e6]
cmp_extra <- cmp_extra[year <= 2022 & biofuel_code %in% c("c146", "c147", "c149")]
cmp_extra[item == paste0(
  "Animal or vegetable fats and oils and their fractions, chemically modified, ",
  "except those hydrogenated, inter-esterified, re-esterified or elaidinized; ",
  "inedible mixtures or preparations of animal or vegetable fats or oils"),
  item := "UCO"]
setcolorder(cmp_extra, c("year", "country", "biofuel_code", "comm_code",
                         "item", "fabio_init", "fabio_resc", "fabio_nonS", "red_value"))


################################################################################
##  7.  EMPIRICAL SHARES  (rescaling inputs)  [commented generator; prior]
################################################################################
#
# Generates inst/gap_empirical_shares_rescale.rds and
# inst/gap_empirical_origin_shares_rescale.rds, read downstream by 08_03.
# Kept COMMENTED (run manually to regenerate). Placed here, above the plot
# functions/executions, because it depends only on cmp + cmp_extra (built
# above) — so if uncommented it never runs ahead of its priors.
#
# 2012-2013 REGIME: for these two years only the RED-data countries are
# rescaled, so BOTH the feedstock shares (gap_feed) and the origin shares
# (gap_ctry) are aggregated over the restricted consumer set `countries_1213`
# (defined in §3). From 2014 the full set is used. The restricted feedstock
# aggregation keeps red_value and fabio_nonS on the SAME country set, so
# red_resid = red_value - fabio_nonS is the quantity the restricted producers
# alone must explain.

################################################################################################
# Time series of feedstock shares in cmp by (year, biofuel_code, comm_code)
# NOW including extended DEU (2012-2024 stitched) + ITA + AUT via cmp_extra.
# share_fabio = sum(fabio_init for comm_code) / sum(fabio_init for biofuel_code)
# share_red   = sum(red_value   for comm_code) / sum(red_value   for biofuel_code)
# NOTE: shares derive from fabio_init (the non-rescaled / init version); fabio_nonS is the init nonS.
################################################################################################

## Fold in the no-origin extension (ITA + extended DEU). cmp's DEU (2022 only,
## origin-resolved) is dropped so the stitched 2012-2024 DEU in cmp_extra
## replaces it (same de-dup as the global item-bar plot). The shares aggregate
## over country/origin, so the missing origin detail in cmp_extra is irrelevant.

share_cols <- c("year", "country", "biofuel_code", "comm_code",
                "item", "fabio_init", "fabio_nonS", "red_value")
cmp_shares_input <- rbindlist(list(
  cmp[!country %in% "DEU", ..share_cols],
  cmp_extra[,              ..share_cols]
), use.names = TRUE)

## Regime split: full country set for 2014+, restricted RED-data set (countries_1213,
## defined in §3) for 2012-2013. `country` = consumer.
shares_ts <- rbindlist(list(
  cmp_shares_input[year >= 2014],
  cmp_shares_input[countries_1213, on = .(year, biofuel_code, country), nomatch = NULL]
), use.names = TRUE)[
  ! comm_code %in% c("c999", "c015", "NA") & !is.na(comm_code),
  .(fabio_init = sum(fabio_init, na.rm = TRUE),
    fabio_nonS = sum(fabio_nonS, na.rm = TRUE),
    red_value  = sum(red_value,  na.rm = TRUE)),
  by = .(year, biofuel_code, comm_code, item)]

## ---- conditional (residual) RED target -----------------------------------
## What the rescaled producers must still explain = full RED minus the feedstock
## FABIO already delivers as biodiesel produced OUTSIDE the rescaling set
## (MYS palm, ARG/BRA soy, ... and — for 2012-2013 — non-restricted EU members).
shares_ts[, red_resid_raw := red_value - fabio_nonS]   # may be < 0 (diagnostic)
shares_ts[, red_resid     := pmax(red_resid_raw, 0)]
shares_ts[, import_capped := red_resid_raw < 0]        # imports already exceed total RED

## keep a feedstock if EITHER its residual RED target OR its FABIO presence is
## non-negligible -> high-FABIO/low-residual feedstocks (palm) stay in with a
## SMALL target so RAS shrinks them instead of 08_03 zeroing them as non-RED.
shares_ts[, `:=`(
  share_red_prov   = red_resid   / sum(red_resid,   na.rm = TRUE),
  share_fabio_prov = fabio_init / sum(fabio_init, na.rm = TRUE)
), by = .(year, biofuel_code)]
shares_ts <- shares_ts[share_red_prov >= 0.001 | share_fabio_prov >= 0.001]
shares_ts[, c("share_red_prov", "share_fabio_prov") := NULL]

## final shares; share_red is the RESIDUAL share 08_03 reads as gap_feed$share_red
shares_ts[, `:=`(
  share_fabio = fabio_init / sum(fabio_init, na.rm = TRUE),
  share_red   = red_resid   / sum(red_resid,   na.rm = TRUE)
), by = .(year, biofuel_code)]

setcolorder(shares_ts,
            c("year", "biofuel_code", "comm_code", "item",
              "fabio_init", "fabio_nonS", "red_value", "red_resid",
              "red_resid_raw", "import_capped", "share_fabio", "share_red"))
setorder(shares_ts, year, biofuel_code, -share_fabio)
shares_ts <- shares_ts[year >= 2012]        # keep 2012-2013 restricted-set rows

saveRDS(shares_ts, file = "inst/gap_empirical_shares_rescale.rds")

################################################################################################
# Time series of origin-country shares in cmp by (year, biofuel_code, origin_country_iso3)
# share_fabio = sum(fabio_init for origin) / sum(fabio_init for biofuel_code)
# share_red   = sum(red_value   for origin) / sum(red_value   for biofuel_code)
#
# EXTENDED to 2012-2013 with the SAME restricted-country regime as the feedstock
# shares: for those two years the origin composition is aggregated over the
# restricted RED-data consumer set (countries_1213), so 08_03's c901 / c145
# origin allocation has a matching gap_ctry budget for the same producers it
# rescales. Origin shares are raw RED composition (NOT residual-adjusted), exactly
# as in the 2014+ path.
################################################################################################

## Regime split (consumer restriction) — mirrors the feedstock-share input above.
## Uses `cmp` directly (origin-resolved); cmp_extra has no origin detail, so a
## restricted country present only in cmp_extra simply contributes no origin rows.
shares_origin_input <- rbindlist(list(
  cmp[year >= 2014],
  cmp[countries_1213, on = .(year, biofuel_code, country), nomatch = NULL]
), use.names = TRUE)

shares_origin_ts <- shares_origin_input[comm_code %in% c("c145", "c901") ,
                                        .(fabio_init = sum(fabio_init, na.rm = TRUE),
                                          red_value   = sum(red_value,   na.rm = TRUE)),
                                        by = .(year, biofuel_code, comm_code, origin_country_iso3)
]
# Shares
shares_origin_ts[,
                 `:=`(
                   share_fabio = fabio_init / sum(fabio_init, na.rm = TRUE),
                   share_red   = red_value   / sum(red_value,   na.rm = TRUE)
                 ),
                 by = .(year, biofuel_code, comm_code)
]

setcolorder(shares_origin_ts,
            c("year", "biofuel_code", "comm_code", "origin_country_iso3",
              "fabio_init", "red_value", "share_fabio", "share_red"))

setorder(shares_origin_ts, year, biofuel_code, -share_fabio)

shares_origin_ts <- shares_origin_ts[year >= 2012]   # keep 2012-2013 restricted-set rows

saveRDS(shares_origin_ts, file = "inst/gap_empirical_origin_shares_rescale.rds")




################################################################################
##  8.  DEFINITIONS: PALETTES
################################################################################

## continent palette — mirrors 19_plot_definitions.R; kept local so the plot
## functions have a stable fixed map even if the sourced file changes.
continent_palette <- c(
  AFR = "#D55E00",  # vermillion
  ASI = "#E69F00",  # orange
  EU  = "#0072B2",  # blue
  EUR = "#56B4E9",  # sky blue
  LAM = "#009E73",  # green
  NAM = "#CC79A7",  # pink
  OCE = "#F0E442",  # yellow
  ROW = "#000000"   # black
)

## fixed global feedstock palette (one feedstock = one colour).
## Items in feedstock_color_map (from 19_plot_definitions) keep their colour;
## items outside it draw deterministically from a fallback pool over the GLOBAL
## sorted set of unknown items (union of cmp + cmp_extra), so colours never shift.

feedstock_fallback_pool <- c(
  "#1F78B4", "#33A02C", "#E31A1C", "#FF7F00", "#6A3D9A", "#B15928",
  "#A6CEE3", "#B2DF8A", "#FB9A99", "#FDBF6F", "#CAB2D6", "#8DD3C7",
  "#BC80BD", "#FFED6F", "#80B1D3", "#FCCDE5"
)

.all_items     <- sort(unique(c(as.character(cmp$item), as.character(cmp_extra$item))))
.unknown_items <- setdiff(.all_items, names(feedstock_color_map))
.unknown_items <- .unknown_items[!is.na(.unknown_items)]

feedstock_palette_master <- c(
  feedstock_color_map,
  if (length(.unknown_items))
    setNames(feedstock_fallback_pool[((seq_along(.unknown_items) - 1) %%
                                        length(feedstock_fallback_pool)) + 1],
             .unknown_items)
  else character(0)
)

if (length(.unknown_items) > length(feedstock_fallback_pool))
  warning("More unmatched feedstocks (", length(.unknown_items), ") than fallback ",
          "colours (", length(feedstock_fallback_pool), "); some will repeat.")

## helper: fixed colours for a given set of item levels (unknown -> grey70 guard)
feedstock_pal_for <- function(levels) {
  pal <- feedstock_palette_master[as.character(levels)]
  pal[is.na(pal)] <- "grey70"
  stats::setNames(pal, levels)
}


################################################################################
##  9.  PLOT FUNCTIONS
################################################################################



## ---- 1. Total feedstocks embodied in consumption, by origin continent ----
##
## compare_rescale = FALSE  -> original behaviour (single `version`: FABIO vs RED)
## compare_rescale = TRUE   -> three bars per facet: FABIO init | FABIO resc | RED
##                            (RED is identical in both, so it is shown once)

plot_cmp_country_year_biofuel_continent <- function(cmp, country_sel, year_sel, biofuel_sel,
                                                    n_fabio_items = 5, n_red_items = 5,
                                                    show_totals = TRUE,
                                                    version = c("resc", "init"),
                                                    compare_rescale = FALSE,
                                                    source_labels = c(
                                                      init = "FABIO\ninit",
                                                      resc = "FABIO\nresc",
                                                      red  = "RED")) {
  version  <- match.arg(version)
  ver_lab  <- if (version == "resc") "post-rescale" else "pre-rescale"
  mode_lab <- if (compare_rescale) "init \u2192 resc \u2192 RED" else ver_lab
  
  d <- cmp[country == country_sel & year == year_sel & biofuel_code == biofuel_sel]
  if (!nrow(d)) stop("No rows for ", country_sel, " / ", year_sel, " / ", biofuel_sel)
  d <- copy(d)
  
  ## single-version convenience column (only when not comparing)
  if (!compare_rescale)
    d[, fabio_value := if (version == "resc") fabio_resc else fabio_init]
  ## column used for ranking / ordering (post-rescale is the headline when comparing)
  d[, fabio_ord := if (compare_rescale) fabio_resc else fabio_value]
  
  ## --- Item selection ---
  item_tot <- d[, .(fi  = sum(fabio_init, na.rm = TRUE),
                    fr  = sum(fabio_resc, na.rm = TRUE),
                    red = sum(red_value,  na.rm = TRUE)), by = item]
  item_tot[, fabio := if (compare_rescale) pmax(fi, fr)
           else if (version == "resc") fr else fi]
  top_f_it <- item_tot[order(-fabio)][seq_len(min(n_fabio_items, .N)), item]
  top_r_it <- item_tot[!item %in% top_f_it][order(-red)][seq_len(min(n_red_items, .N)), item]
  item_order <- c(item_tot[item %in% top_f_it][order(-fabio), item],
                  item_tot[item %in% top_r_it][order(-red),   item])
  
  d <- d[item %in% item_order]
  
  ## --- Continent handling (no top-N needed; few categories) ---
  d[, continent := fifelse(is.na(continent) | continent == "", "Other", continent)]
  cont_tot <- d[, .(tot = sum(fabio_ord, na.rm = TRUE) + sum(red_value, na.rm = TRUE)),
                by = continent]
  cont_order <- cont_tot[order(-tot), continent]
  cont_order <- c(setdiff(cont_order, "Other"), if ("Other" %in% cont_order) "Other")
  
  ## --- Aggregate & reshape (2 or 3 sources depending on mode) ---
  measure_cols <- if (compare_rescale) c("fabio_init", "fabio_resc", "red_value")
  else                 c("fabio_value", "red_value")
  src_labs     <- if (compare_rescale)
    c(source_labels[["init"]], source_labels[["resc"]], source_labels[["red"]])
  else
    c("FABIO", source_labels[["red"]])
  
  agg <- d[, lapply(.SD, sum, na.rm = TRUE),
           by = .(item, continent), .SDcols = measure_cols]
  long <- melt(agg, id.vars = c("item", "continent"),
               measure.vars = measure_cols,
               variable.name = "source", value.name = "value")
  
  long[, item      := factor(item, levels = item_order)]
  long[, continent := factor(continent, levels = cont_order)]
  long[, source    := factor(source, levels = measure_cols, labels = src_labs)]
  
  tot_lab <- long[, .(value = sum(value, na.rm = TRUE)), by = .(item, source)]  # bar totals for labels
  
  ## --- Palette ---
  n_named <- length(cont_order)
  pal <- if (requireNamespace("pals", quietly = TRUE)) {
    unname(pals::polychrome(n_named + 2))[3:(n_named + 2)]
  } else {
    rep(c(RColorBrewer::brewer.pal(12, "Set3"),
          RColorBrewer::brewer.pal(8,  "Dark2")), length.out = n_named)
  }
  pal <- setNames(pal, cont_order)
  if ("Other" %in% cont_order) pal["Other"] <- "grey70"
  
  p <- ggplot2::ggplot(long, ggplot2::aes(x = source, y = value, fill = continent)) +
    ggplot2::geom_col(position = "stack", width = 0.85) +
    ggplot2::facet_grid(~ item, switch = "x",
                        labeller = ggplot2::label_wrap_gen(width = 14)) +
    ggplot2::scale_fill_manual(values = pal, name = "Origin continent") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.10))) +
    ggplot2::labs(title    = sprintf("FABIO vs RED \u2014 %s, %s, %s [%s]",
                                     country_sel, year_sel, biofuel_sel, mode_lab),
                  subtitle = sprintf("Top %d items \u00b7 by origin continent",
                                     length(item_order)),
                  x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position    = "right",
                   strip.placement    = "outside",
                   strip.background   = ggplot2::element_blank(),
                   strip.text         = ggplot2::element_text(size = 8, lineheight = 0.9),
                   panel.spacing.x    = grid::unit(0.15, "lines"),
                   panel.grid.major.x = ggplot2::element_blank(),
                   axis.text.x        = ggplot2::element_text(
                     size = if (compare_rescale) 7 else 8,
                     lineheight = 0.85))
  
  if (show_totals)
    p <- p + ggplot2::geom_text(
      data = tot_lab,
      ggplot2::aes(x = source, y = value,
                   label = scales::label_number(accuracy = 1, big.mark = "")(value)),
      inherit.aes = FALSE, vjust = -0.4, size = 2.4, colour = "grey20")
  
  p
}

## ---- 2. Time series of total consumption + embodied-feedstock decomposition ----
##
## compare_rescale = FALSE  -> original behaviour (single `version`: FABIO vs RED)
## compare_rescale = TRUE   -> three bars per year: FABIO init | FABIO resc | RED

plot_cmp_country_biofuel_item_ts <- function(cmp, country_sel, biofuel_sel,
                                             n_items = 8,
                                             overlap_only = TRUE,
                                             show_totals  = TRUE,
                                             version = c("resc", "init"),
                                             compare_rescale = FALSE,
                                             feedstock_palette = feedstock_palette_master,
                                             source_labels = c(
                                               init = "FABIO\ninit",
                                               resc = "FABIO\nresc",
                                               red  = "RED")) {
  version  <- match.arg(version)
  ver_lab  <- if (version == "resc") "post-rescale" else "pre-rescale"
  mode_lab <- if (compare_rescale) "init \u2192 resc \u2192 RED" else ver_lab
  
  d <- cmp[country == country_sel & biofuel_code == biofuel_sel]
  if (!nrow(d)) stop("No rows for ", country_sel, " / ", biofuel_sel)
  d <- copy(d)
  if (!compare_rescale)
    d[, fabio_value := if (version == "resc") fabio_resc else fabio_init]
  d[, item := as.character(item)]
  
  ## --- Overlapping years (both sources present) ---
  yr_tot <- d[, .(fi  = sum(fabio_init, na.rm = TRUE),
                  fr  = sum(fabio_resc, na.rm = TRUE),
                  red = sum(red_value,  na.rm = TRUE)), by = year]
  yr_tot[, fabio := if (compare_rescale) pmax(fi, fr)
         else if (version == "resc") fr else fi]
  yrs <- if (overlap_only) yr_tot[fabio > 0 & red > 0, year] else yr_tot$year
  if (!length(yrs)) stop("No overlapping years for ", country_sel, " / ", biofuel_sel)
  d <- d[year %in% yrs]
  
  ## --- Item selection: top-N by combined total, rest -> "Other" ---
  item_tot <- d[, .(fi  = sum(fabio_init, na.rm = TRUE),
                    fr  = sum(fabio_resc, na.rm = TRUE),
                    red = sum(red_value,  na.rm = TRUE)), by = item]
  item_tot[, tot := (if (compare_rescale) pmax(fi, fr)
                     else if (version == "resc") fr else fi) + red]
  item_tot <- item_tot[order(-tot)]
  top_it <- item_tot[seq_len(min(n_items, .N)), item]
  d[, item := fifelse(item %in% top_it, item, "Other")]
  item_order <- c(top_it, if (nrow(item_tot) > length(top_it)) "Other")
  
  ## --- Aggregate & reshape (2 or 3 sources depending on mode) ---
  measure_cols <- if (compare_rescale) c("fabio_init", "fabio_resc", "red_value")
  else                 c("fabio_value", "red_value")
  src_labs     <- if (compare_rescale)
    c(source_labels[["init"]], source_labels[["resc"]], source_labels[["red"]])
  else
    c("FABIO", source_labels[["red"]])
  
  agg <- d[, lapply(.SD, sum, na.rm = TRUE),
           by = .(item, year), .SDcols = measure_cols]
  long <- melt(agg, id.vars = c("item", "year"),
               measure.vars = measure_cols,
               variable.name = "source", value.name = "value")
  
  long[, item   := factor(item, levels = item_order)]
  long[, source := factor(source, levels = measure_cols, labels = src_labs)]
  long[, year   := factor(year, levels = sort(unique(year)))]   # discrete, time-ordered facet
  
  tot_lab <- long[, .(value = sum(value)), by = .(year, source)] # bar totals for labels
  
  ## --- Palette (fixed feedstock colours; folded items -> "Other") ---
  pal <- feedstock_palette[as.character(item_order)]
  pal[is.na(pal)] <- "grey70"
  names(pal) <- item_order
  if ("Other" %in% item_order && "Other" %in% names(feedstock_palette))
    pal["Other"] <- unname(feedstock_palette[["Other"]])
  
  p <- ggplot2::ggplot(long, ggplot2::aes(x = source, y = value, fill = item)) +
    ggplot2::geom_col(position = "stack", width = 0.8) +
    ggplot2::facet_grid(~ year, switch = "x") +
    ggplot2::scale_fill_manual(values = pal, name = "Feedstock") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.10))) +
    ggplot2::labs(title    = sprintf("FABIO vs RED \u2014 %s, %s [%s]",
                                     country_sel, biofuel_sel, mode_lab),
                  subtitle = sprintf("Total quantity & item composition \u00b7 %d overlapping years%s",
                                     length(yrs),
                                     if ("Other" %in% item_order)
                                       sprintf(" \u00b7 top %d + Other", length(top_it)) else ""),
                  x = NULL, y = "Quantity") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position    = "right",
                   strip.placement    = "outside",
                   strip.background   = ggplot2::element_blank(),
                   strip.text         = ggplot2::element_text(size = 9, face = "bold"),
                   panel.spacing.x    = grid::unit(0.2, "lines"),
                   panel.grid.major.x = ggplot2::element_blank(),
                   axis.text.x        = ggplot2::element_text(
                     size = if (compare_rescale) 7 else 8,
                     lineheight = 0.85))
  
  if (show_totals)
    p <- p + ggplot2::geom_text(
      data = tot_lab,
      ggplot2::aes(x = source, y = value,
                   label = scales::label_number(accuracy = 1, big.mark = "")(value)),
      inherit.aes = FALSE, vjust = -0.4, size = 2.4, colour = "grey20")
  
  p 
}


## ---- 3. Feedstock totals: FABIO initial vs rescaled vs empirical RED ----
plot_global_gap_item_bars <- function(cmp, ncol = 2,
                                      free_y = TRUE, label_accuracy = 1,
                                      extra = NULL, extra_overrides = character(0),
                                      n_items = 12,
                                      rank_years = 2014:2022,
                                      item_order = NULL,
                                      feedstock_palette = feedstock_palette_master,
                                      red_col    = "#d95f02",
                                      alpha_init = 0.4,
                                      alpha_resc = 0.8) {
  
  ## --- fold in the no-origin extension (ITA + extended DEU), as before ---
  if (!is.null(extra)) {
    common <- c("year", "country", "biofuel_code", "comm_code",
                "item", "fabio_init", "fabio_resc", "red_value")
    cmp <- rbindlist(list(cmp[!country %in% extra_overrides, ..common],
                          extra[, ..common]), use.names = TRUE)
  }
  
  ## --- item selection & ordering: top n_items by summed fabio_init over rank_years ---
  ## (data-driven, replaces the previously hard-coded item_order vector; pass
  ##  item_order explicitly if you want to override this ranking with a fixed vector)
  if (is.null(item_order)) {
    item_rank <- cmp[year %in% rank_years, .(fabio_init_sum = sum(fabio_init, na.rm = TRUE)),
                     by = item][order(-fabio_init_sum)]
    item_order <- item_rank[seq_len(min(n_items, .N)), item]
  }
  
  ## --- restrict to 2012-2022 and map each year to a period bin ---
  d <- cmp[year >= 2012 & year <= 2022]
  period_levels <- c("2012-2014", "2015-2017", "2018-2020", "2021-2022")
  d[, period := fcase(
    year %in% 2012:2014, "2012-2014",
    year %in% 2015:2017, "2015-2017",
    year %in% 2018:2020, "2018-2020",
    year %in% 2021:2022, "2021-2022")]
  d[, period := factor(period, levels = period_levels)]
  
  ## --- aggregate to period x item (both FABIO versions + RED target) ---
  ay <- d[, .(fabio_init = sum(fabio_init, na.rm = TRUE),
              fabio_resc = sum(fabio_resc, na.rm = TRUE),
              red_value  = sum(red_value,  na.rm = TRUE)),
          by = .(period, item)]
  
  ## --- FIXED selection & x-ordering (item_order now data-driven, see above) ---
  ay <- ay[item %in% item_order]
  ay[, item := factor(item, levels = item_order)]   # left -> right = item_order
  ay[, x    := as.integer(item)]                    # numeric x = level index (stable slots)
  
  ## --- residual (post-rescaling) gap + signed label ---
  ay[, resid_gap := fabio_resc - red_value]
  ay[, gap_lab := paste0(ifelse(resid_gap >= 0, "+\u2009", "-\u2009"),
                         scales::label_number(accuracy = label_accuracy,
                                              big.mark = ",")(abs(resid_gap)))]
  
  ## --- per-facet vertical reference, so label offset scales sensibly under free_y ---
  ay[, period_max := max(pmax(fabio_init, fabio_resc, red_value), na.rm = TRUE), by = period]
  ay[, lab_y := red_value + 0.05 * period_max]   # sits just above the RED tick, with a gap
  
  ## --- geometry constants ---
  bar_w  <- 0.6
  cap_w  <- 0.06
  seg_i  <- bar_w / 2 + 0.05     # initial-gap segment offset
  seg_r  <- bar_w / 2 + 0.18     # rescaled-gap segment offset
  arr_x  <- bar_w / 2 + 0.115    # trajectory-arrow offset
  lab_x  <- bar_w / 2 + 0.02     # gap-label offset (sits left of seg_i / arrow / seg_r)
  
  ggplot2::ggplot(ay) +
    ## faint initial bar
    ggplot2::geom_col(ggplot2::aes(x = x, y = fabio_init, fill = item),
                      width = bar_w, alpha = alpha_init) +
    ## solid, outlined rescaled bar on top
    ggplot2::geom_col(ggplot2::aes(x = x, y = fabio_resc, fill = item),
                      width = bar_w, alpha = alpha_resc,
                      colour = "grey25", linewidth = 0.2) +
    ## RED target tick (full bar width)
    ggplot2::geom_segment(ggplot2::aes(x = x - bar_w / 2, xend = x + bar_w / 2,
                                       y = red_value, yend = red_value),
                          colour = red_col, linewidth = 0.5) +
    ## initial gap (faint): RED -> fabio_init, with cap tick at RED
    ggplot2::geom_segment(ggplot2::aes(x = x + seg_i, xend = x + seg_i,
                                       y = red_value, yend = fabio_init),
                          colour = red_col, alpha = 0.45, linewidth = 0.4) +
    ggplot2::geom_segment(ggplot2::aes(x = x + seg_i - cap_w, xend = x + seg_i + cap_w,
                                       y = red_value, yend = red_value),
                          colour = red_col, alpha = 0.45, linewidth = 0.4) +
    ## residual gap (solid): RED -> fabio_resc, with cap tick at RED
    ggplot2::geom_segment(ggplot2::aes(x = x + seg_r, xend = x + seg_r,
                                       y = red_value, yend = fabio_resc),
                          colour = red_col, linewidth = 0.6) +
    ggplot2::geom_segment(ggplot2::aes(x = x + seg_r - cap_w, xend = x + seg_r + cap_w,
                                       y = red_value, yend = red_value),
                          colour = red_col, linewidth = 0.6) +
    ## trajectory arrow: fabio_init -> fabio_resc (fine line)
    ggplot2::geom_segment(ggplot2::aes(x = x + arr_x, xend = x + arr_x,
                                       y = fabio_init, yend = fabio_resc),
                          colour = "grey20", linewidth = 0.3,
                          arrow = grid::arrow(length = grid::unit(0.035, "in"),
                                              type = "closed")) +
    ## residual gap label, left of the initial-gap / arrow / rescaled-gap cluster,
    ## anchored just above the RED-value line (vjust = 0 -> text sits above lab_y)
    ggplot2::geom_text(ggplot2::aes(x = x + lab_x,
                                    y = lab_y,
                                    label = gap_lab),
                       colour = red_col, hjust = 1, vjust = 0, size = 2.2) +
    ggplot2::scale_fill_manual(values = feedstock_pal_for(item_order),
                               name = "Feedstock", drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = seq_along(item_order), labels = item_order,
                                limits = c(0.5, length(item_order) + 0.9),
                                expand = ggplot2::expansion(mult = c(0.02, 0.14))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.10))) +
    ggplot2::facet_wrap(~ period, ncol = ncol,
                        scales = if (free_y) "free_y" else "fixed") +
    ggplot2::labs(
      x = NULL, y = "Volume (M liters)",
      caption = paste(
        "Faint bar = initial (non-rescaled); solid outlined bar = rescaled.",
        "Red tick = RED target; faint / solid red segment = initial / residual gap to RED;",
        "grey arrow = initial \u2192 rescaled trajectory.")) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(legend.position    = "right",
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor   = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
                   strip.text  = ggplot2::element_text(face = "bold"),
                   plot.caption = ggplot2::element_text(colour = "grey40", hjust = 0))
}

## ---- 4. Origin countries: FABIO initial vs rescaled vs empirical RED ----

plot_global_gap_country_bars <- function(cmp, ncol = 2,
                                         free_y = TRUE, label_accuracy = 1,
                                         country_order = c(
                                           "FRA", "DEU", "IDN", "MYS", "ARG",
                                           "GBR", "CHN", "USA", "ESP", "SGP",
                                           "BEL", "BRA", "UKR", "ROU", "AUS"
                                         ),
                                         continent_palette = c(
                                           AFR = "#D55E00",
                                           ASI = "#E69F00",
                                           EU  = "#0072B2",
                                           EUR = "#56B4E9",
                                           LAM = "#009E73",
                                           NAM = "#CC79A7",
                                           OCE = "#F0E442",
                                           ROW = "#000000"
                                         ),
                                         red_col    = "#d95f02",
                                         alpha_init = 0.4,
                                         alpha_resc = 0.8) {
  
  ## --- restrict to 2012-2022 and map each year to a period bin ---
  d <- cmp[year >= 2012 & year <= 2022]
  period_levels <- c("2012-2014", "2015-2017", "2018-2020", "2021-2022")
  d[, period := fcase(
    year %in% 2012:2014, "2012-2014",
    year %in% 2015:2017, "2015-2017",
    year %in% 2018:2020, "2018-2020",
    year %in% 2021:2022, "2021-2022")]
  d[, period := factor(period, levels = period_levels)]
  
  ## --- one continent per origin country (first non-missing match; unmapped -> ROW) ---
  country_cont <- unique(d[, .(origin_country_iso3, continent)], by = "origin_country_iso3")
  country_cont[, continent := fifelse(is.na(continent) | continent == "", "ROW", continent)]
  
  ## --- aggregate to period x origin country (both FABIO versions + RED target) ---
  ay <- d[, .(fabio_init = sum(fabio_init, na.rm = TRUE),
              fabio_resc = sum(fabio_resc, na.rm = TRUE),
              red_value  = sum(red_value,  na.rm = TRUE)),
          by = .(period, origin_country_iso3)]
  ay[country_cont, on = "origin_country_iso3", continent := i.continent]
  
  ## --- FIXED selection & x-ordering (no top-N) ---
  ay <- ay[origin_country_iso3 %in% country_order]
  ay[, origin_country_iso3 := factor(origin_country_iso3, levels = country_order)]
  ay[, x := as.integer(origin_country_iso3)]        # numeric x = level index (stable slots)
  ay[, continent := factor(continent, levels = names(continent_palette))]
  
  ## --- residual (post-rescaling) gap + signed label ---
  ay[, resid_gap := fabio_resc - red_value]
  ay[, gap_lab := paste0(ifelse(resid_gap >= 0, "+\u2009", "-\u2009"),
                         scales::label_number(accuracy = label_accuracy,
                                              big.mark = ",")(abs(resid_gap)))]
  
  ## --- per-facet vertical reference, so label offset scales sensibly under free_y ---
  ay[, period_max := max(pmax(fabio_init, fabio_resc, red_value), na.rm = TRUE), by = period]
  ay[, lab_y := red_value + 0.05 * period_max]   # sits just above the RED tick, with a gap
  
  ## --- geometry constants ---
  bar_w  <- 0.6
  cap_w  <- 0.06
  seg_i  <- bar_w / 2 + 0.05     # initial-gap segment offset
  seg_r  <- bar_w / 2 + 0.18     # rescaled-gap segment offset
  arr_x  <- bar_w / 2 + 0.115    # trajectory-arrow offset
  lab_x  <- bar_w / 2 + 0.02     # gap-label offset (sits left of seg_i / arrow / seg_r)
  
  ggplot2::ggplot(ay) +
    ## faint initial bar
    ggplot2::geom_col(ggplot2::aes(x = x, y = fabio_init, fill = continent),
                      width = bar_w, alpha = alpha_init) +
    ## solid, outlined rescaled bar on top
    ggplot2::geom_col(ggplot2::aes(x = x, y = fabio_resc, fill = continent),
                      width = bar_w, alpha = alpha_resc,
                      colour = "grey25", linewidth = 0.2) +
    ## RED target tick (full bar width)
    ggplot2::geom_segment(ggplot2::aes(x = x - bar_w / 2, xend = x + bar_w / 2,
                                       y = red_value, yend = red_value),
                          colour = red_col, linewidth = 0.5) +
    ## initial gap (faint): RED -> fabio_init, with cap tick at RED
    ggplot2::geom_segment(ggplot2::aes(x = x + seg_i, xend = x + seg_i,
                                       y = red_value, yend = fabio_init),
                          colour = red_col, alpha = 0.45, linewidth = 0.4) +
    ggplot2::geom_segment(ggplot2::aes(x = x + seg_i - cap_w, xend = x + seg_i + cap_w,
                                       y = red_value, yend = red_value),
                          colour = red_col, alpha = 0.45, linewidth = 0.4) +
    ## residual gap (solid): RED -> fabio_resc, with cap tick at RED
    ggplot2::geom_segment(ggplot2::aes(x = x + seg_r, xend = x + seg_r,
                                       y = red_value, yend = fabio_resc),
                          colour = red_col, linewidth = 0.6) +
    ggplot2::geom_segment(ggplot2::aes(x = x + seg_r - cap_w, xend = x + seg_r + cap_w,
                                       y = red_value, yend = red_value),
                          colour = red_col, linewidth = 0.6) +
    ## trajectory arrow: fabio_init -> fabio_resc (fine line)
    ggplot2::geom_segment(ggplot2::aes(x = x + arr_x, xend = x + arr_x,
                                       y = fabio_init, yend = fabio_resc),
                          colour = "grey20", linewidth = 0.3,
                          arrow = grid::arrow(length = grid::unit(0.035, "in"),
                                              type = "closed")) +
    ## residual gap label, left of the initial-gap / arrow / rescaled-gap cluster,
    ## anchored just above the RED-value line (vjust = 0 -> text sits above lab_y)
    ggplot2::geom_text(ggplot2::aes(x = x + lab_x,
                                    y = lab_y,
                                    label = gap_lab),
                       colour = red_col, hjust = 1, vjust = 0, size = 2.2) +
    ggplot2::scale_fill_manual(values = continent_palette,
                               name = "Origin continent", drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = seq_along(country_order), labels = country_order,
                                limits = c(0.5, length(country_order) + 0.9),
                                expand = ggplot2::expansion(mult = c(0.02, 0.14))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.10))) +
    ggplot2::facet_wrap(~ period, ncol = ncol,
                        scales = if (free_y) "free_y" else "fixed") +
    ggplot2::labs(
      x = NULL, y = "Volume (M liters)",
      caption = paste(
        "Faint bar = initial (non-rescaled); solid outlined bar = rescaled.",
        "Red tick = RED target; faint / solid red segment = initial / residual gap to RED;",
        "grey arrow = initial \u2192 rescaled trajectory.")) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(legend.position    = "right",
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor   = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
                   strip.text  = ggplot2::element_text(face = "bold"),
                   plot.caption = ggplot2::element_text(colour = "grey40", hjust = 0))
}




################################################################################
##  10.  PLOT EXECUTIONS  (combined init-vs-resc-vs-RED, one file per country)
################################################################################
#
# Single pass now (no _pre / _post loop): compare_rescale = TRUE puts all three
# bars in one figure. Files carry a "_cmp" suffix. Width bumped slightly to give
# the extra (third) bar room to breathe.

combos <- CJ(country = c("GBR", "SWE", "NOR", "IRL", "FRA", "DEU", "NLD", "ESP"),
             biofuel = c("c146", "c147", "c149"),
             sorted  = FALSE)                            # keep your row/col order
countries <- c("GBR", "SWE", "NOR", "IRL", "FRA", "DEU", "NLD", "ESP")

## ---- (1) origin-continent breakdown, 2022, combined init/resc/RED ----
{
  plots <- Map(function(co, bf) {
    plot_cmp_country_year_biofuel_continent(cmp, country_sel = co,
                                            year_sel = "2022", biofuel_sel = bf,
                                            compare_rescale = TRUE)
  }, combos$country, combos$biofuel)
  
  invisible(lapply(countries, function(co) {
    idx <- which(combos$country == co)
    p <- wrap_plots(plots[idx], nrow = 3, ncol = 1, byrow = TRUE) &
      ggplot2::theme(
        axis.title.y    = ggplot2::element_blank(),
        legend.position = "right",
        plot.title      = ggplot2::element_text(size = 9),
        plot.subtitle   = ggplot2::element_text(size = 8),
        legend.text     = ggplot2::element_text(size = 7),
        legend.title    = ggplot2::element_text(size = 8),
        legend.key.size = grid::unit(0.3, "cm")
      )
    p <- p + patchwork::plot_annotation(
      theme = ggplot2::theme(plot.margin = ggplot2::margin(l = 10))
    ) & ggplot2::labs(y = NULL)
    y_lab <- cowplot::ggdraw() +
      cowplot::draw_label("Biofuel consumption (million liters)", angle = 90, size = 9)
    p_final <- cowplot::plot_grid(y_lab, p, ncol = 2, rel_widths = c(0.03, 0.97))
    ggplot2::ggsave(
      filename = paste0("output/plot/FABIO_v_RED_", co, "_2022_cmp.pdf"),
      plot = p_final, width = 9, height = 11, units = "in", device = cairo_pdf
    )
  }))
}

## ---- (2) item time-series, all years, combined init/resc/RED ----
{
  plots <- Map(function(co, bf) {
    plot_cmp_country_biofuel_item_ts(cmp, country_sel = co, biofuel_sel = bf,
                                     compare_rescale = TRUE)   # no year_sel — spans all years
  }, combos$country, combos$biofuel)
  
  invisible(lapply(countries, function(co) {
    idx <- which(combos$country == co)
    p <- wrap_plots(plots[idx], nrow = 3, ncol = 1, byrow = TRUE) &
      ggplot2::theme(
        axis.title.y    = ggplot2::element_blank(),
        legend.position = "right",
        plot.title      = ggplot2::element_text(size = 9),
        plot.subtitle   = ggplot2::element_text(size = 8),
        legend.text     = ggplot2::element_text(size = 7),
        legend.title    = ggplot2::element_text(size = 8),
        legend.key.size = grid::unit(0.3, "cm")
      )
    p <- p + patchwork::plot_annotation(
      theme = ggplot2::theme(plot.margin = ggplot2::margin(l = 10))
    ) & ggplot2::labs(y = NULL)
    y_lab <- cowplot::ggdraw() +
      cowplot::draw_label("Biofuel consumption (million liters)", angle = 90, size = 9)
    p_final <- cowplot::plot_grid(y_lab, p, ncol = 2, rel_widths = c(0.03, 0.97))
    ggplot2::ggsave(
      filename = paste0("output/plot/FABIO_v_RED_TS_", co, "_cmp.pdf"),
      plot = p_final, width = 13, height = 11, units = "in", device = cairo_pdf
    )
  }))
}


########## FEEDSTOCK-LEVEL TIME SERIES FOR ITALY AND EXTENDED GERMANY  ##########
## 4. Feedstock-level TS plots for ITA & DEU (DEU file = stitched 2012-2024),
##    combined init/resc/RED.
combos_extra <- CJ(country = extra_iso3, biofuel = c("c146", "c147", "c149"),
                   sorted = FALSE)
{
  plots_extra <- Map(function(co, bf) {
    tryCatch(plot_cmp_country_biofuel_item_ts(cmp_extra, country_sel = co,
                                              biofuel_sel = bf, compare_rescale = TRUE),
             error = function(e) ggplot2::ggplot() + ggplot2::theme_void() +
               ggplot2::labs(title = sprintf("%s / %s \u2014 %s", co, bf, conditionMessage(e))))
  }, combos_extra$country, combos_extra$biofuel)
  
  invisible(lapply(extra_iso3, function(co) {
    idx <- which(combos_extra$country == co)
    p <- patchwork::wrap_plots(plots_extra[idx], nrow = 3, ncol = 1, byrow = TRUE) &
      ggplot2::theme(axis.title.y = ggplot2::element_blank(), legend.position = "right",
                     plot.title = ggplot2::element_text(size = 9), plot.subtitle = ggplot2::element_text(size = 8),
                     legend.text = ggplot2::element_text(size = 7), legend.title = ggplot2::element_text(size = 8),
                     legend.key.size = grid::unit(0.3, "cm"))
    p <- p + patchwork::plot_annotation(
      theme = ggplot2::theme(plot.margin = ggplot2::margin(l = 10))) & ggplot2::labs(y = NULL)
    y_lab <- cowplot::ggdraw() +
      cowplot::draw_label("Biofuel consumption (million liters)", angle = 90, size = 9)
    p_final <- cowplot::plot_grid(y_lab, p, ncol = 2, rel_widths = c(0.03, 0.97))
    ggplot2::ggsave(paste0("output/plot/FABIO_v_RED_TS_", co, "_cmp.pdf"),
                    p_final, width = 13, height = 11, units = "in", device = cairo_pdf)
  }))
}


## ---- (3) global gap item bars — FABIO init vs rescaled vs RED (DEU folded via extra=) ----

p_item_bars <- plot_global_gap_item_bars(cmp, ncol = 2, n_items = 10, 
                                         free_y = TRUE,
                                         extra = cmp_extra, 
                                         extra_overrides = "DEU")

n_periods <- 4; ncol <- 2; nrow <- ceiling(n_periods / ncol)
ggplot2::ggsave(
  filename  = "output/plot/FABIO_v_RED_global_item_bars_periods_init_vs_resc.pdf",
  plot      = p_item_bars,
  width     = 5.5 * ncol, height = 3.2 * nrow,
  units     = "in", limitsize = FALSE, device = cairo_pdf
)


## ---- (4) global gap country bars — FABIO init vs rescaled vs RED ----

p_country_bars <- plot_global_gap_country_bars(cmp, ncol = 2, free_y = TRUE)

ggplot2::ggsave(
  filename  = "output/plot/FABIO_v_RED_global_country_bars_periods_init_vs_resc.pdf",
  plot      = p_country_bars,
  width     = 5.5 * ncol, height = 3.2 * nrow,
  units     = "in", limitsize = FALSE, device = cairo_pdf
)





