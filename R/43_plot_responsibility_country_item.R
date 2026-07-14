# =============================================================================
# 43_plot_responsibility_country_item.R
# ONE figure: the production- (PBA), consumption- (CBA) and value-added-based
# (VA) accounts as three ADJACENT stacked bars per country, at COUNTRY x ITEM
# resolution, for a single year.
#   x = country (top N, largest first), three bars each | y = impact | fill = item
#   items below MIN_ITEM_SHARE of an account are pooled as "Other"
#
# WHY THIS SCRIPT COMPUTES INSTEAD OF ONLY PLOTTING ---------------------------
# 40's accounts CSV is keyed by iso3c only -- it has no item dimension -- so the
# PBA/CBA bars cannot be read off it. Both are margins of one bilateral matrix
#     D = diag(f) B[, bf] Y_bf     (node i x consuming country c)
#     f = e / x , B = L_<allocation> , Y_bf = biofuel final demand
# which this script rebuilds with 40's exact math and switches:
#     PBA(i) = rowSums(D)          impact at the producing node i
#     CBA(c) = colSums(D)          impact routed to the consumer of the biofuel
# The VA bar is NOT recomputed: it is read from 40's ex_tls responsibility CSV,
# which is already resolved to (va_iso3c, va_comm_code). All three are then
# checked against 40's country-level accounts CSV and against each other.
#
# ITEM SEMANTICS -- read before interpreting the stacks -----------------------
# The item means the same thing in PBA and CBA, and something else in VA:
#   PBA / CBA   the node where the impact PHYSICALLY OCCURS (the source item).
#               PBA books it to the producing country, CBA to the consuming one,
#               so in a CBA bar country and item refer to different agents:
#               "the impact country c's biofuel demand causes in <item>".
#   VA          the sector that CAPTURES the value along the chain -- the item
#               that earns, not the item that emits. This is why the biofuels
#               themselves (Biogasoline, Biodiesel, Renewable diesel) appear in
#               the VA bar and nowhere else: refining causes no direct pressure
#               but takes a cut of the value. That contrast is the figure's point,
#               so the three fuels keep the pipeline's fixed `fuel_colors` (19).
# Each account still sums to the SAME grand total (one footprint, three
# attributions); only its distribution over (country, item) moves.
#
# COLOURS. Items are coloured by their `comm_group` (inst/items_full_bcp.csv):
# one hue family per group, shaded dark (largest) -> light (smallest) within the
# group, so a stack reads as families -- oil crops, vegetable oils, cereals -- and
# not as 15 unrelated colours. The three biofuels override this with fuel_colors
# and sit at the BASE of every stack. If this palette proves useful elsewhere,
# GROUP_PALETTE below belongs in 19_plot_definitions.R.
#
# READS  <MRIO>/losses/{X.rds, Y.rds, fd_labels.csv, <yr>_L_<alloc>.rds}   (13,14)
#        <base>/{E.rds, io_labels.csv}                                     (16,12_b)
#        inst/items_full_bcp.csv                                           (comm_group)
#        <IN_DIR>/FABIO_bcp_<ind>_value_added_responsibility_<vabase>_<alloc>_ex_tls.csv  (40)
#        <IN_DIR>/FABIO_bcp_<ind>_accounts_<vabase>_<alloc>.csv            (40, cross-check)
# WRITES output/plot/responsibility_country_item_<ind>_<vabase>_<alloc>_<year>.svg
#
# RUN: Rscript R/43_plot_responsibility_country_item.R   (after 40)
#      switches below must match the 40 run whose CSVs are being read.
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
library(Matrix)
library(ggplot2)
library(svglite)

source("R/00_system_variables.R")   # output_dir_bcp, years
source("R/19_plot_definitions.R")   # indicator_meta, fuel_colors

# --- run switches (must match the 40 run that wrote the CSVs) ----------------
YEAR           <- 2019L
allocation     <- "value"        # co-product rule of B: "mass" | "value"
STRESSOR       <- "ibif_total"   # "ibif_total" | "LCIM_EQ_terrestrial"
VA_BASE        <- "exiobase"     # "gloria" | "exiobase"

TOP_N          <- 15             # countries shown
RANK_BY        <- "max"          # rank countries by their "max" | "mean" account, or "PBA"/"CBA"/"VA"
MIN_ITEM_SHARE <- 0.02           # items below this share of ANY account -> "Other"
TOL            <- 1e-6           # relative tolerance of the consistency checks

BF_CODES <- c("c146", "c147", "c149")   # biogasoline, biodiesel, renewable diesel (40)
ACCOUNTS <- c("PBA", "CBA", "VA")       # bar order within each country

# --- paths -------------------------------------------------------------------
model_version <- if (tolower(trimws(Sys.getenv("FABIO_RUN_MODE", "rescaled"))) == "bypass")
  "bypass" else "rescaled"
base_path <- sub("/+$", "", output_dir_bcp)                     # E, io_labels: version-invariant
MRIO_PATH <- if (model_version == "bypass") file.path(base_path, "bypass") else base_path
IN_DIR    <- if (model_version == "bypass") "output/bypass" else "output"
PLOT_DIR  <- file.path("output", "plot")
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

ATAG <- tolower(allocation)                       # "mass" / "value"
STAG <- tolower(sub("_total$", "", STRESSOR))     # "ibif" / "lcim_eq_terrestrial"
VA_FILE  <- file.path(IN_DIR, sprintf("FABIO_bcp_%s_value_added_responsibility_%s_%s_ex_tls.csv",
                                      STAG, VA_BASE, ATAG))
ACC_FILE <- file.path(IN_DIR, sprintf("FABIO_bcp_%s_accounts_%s_%s.csv", STAG, VA_BASE, ATAG))

message(sprintf(">>> [43] %d | model='%s' | stressor='%s' | alloc='%s' | VA base='%s'",
                YEAR, model_version, STRESSOR, allocation, VA_BASE))

# --- indicator metadata ------------------------------------------------------
meta <- {
  i <- match(STAG, tolower(sub("_total$", "", indicator_meta$indicator)))
  if (is.na(i)) {
    warning("[43] no indicator_meta entry for '", STAG, "'; scale_factor = 1.")
    list(scale_factor = 1, y_label = STAG, short_label = STAG)
  } else as.list(indicator_meta[i, .(scale_factor, y_label, short_label)])
}

# --- colours: one hue family per comm_group ----------------------------------
# light -> dark endpoints per group; items within a group are ranked by size and
# take the ramp from dark (largest) to light (smallest). Unlisted groups fall back
# to grey with a warning. The three fuels are handled separately (fuel_colors).
GROUP_PALETTE <- data.table(
  comm_group = c("Oil crops", "Vegetable oils", "Oil cakes", "Cereals", "Sugar crops",
                 "Roots and tubers", "Grazing", "Fodder crops", "Live animals", "Meat",
                 "Milk", "Fibre crops", "Vegetables, fruit, nuts, pulses, spices",
                 "Sugar, sweeteners", "Alcohol", "Building blocks", "Biopolymers",
                 "Waste", "Other"),
  light      = c("#C7E9C0", "#FDD0A2", "#DFC27D", "#E6E2B0", "#FCBBA1",
                 "#A8DDD5", "#D9F0A3", "#E2EFC7", "#FDE0DD", "#FCC5C0",
                 "#F1B6DA", "#CCECE6", "#E5F5E0", "#FDE0C8", "#CBD5E8",
                 "#C6DBEF", "#BCBDDC", "#D9D9D9", "#E0E0E0"),
  dark       = c("#00441B", "#A63603", "#8C510A", "#6E6B1E", "#A50F15",
                 "#01665E", "#4D7C0F", "#7F9A3F", "#AE017E", "#7A0177",
                 "#C51B7D", "#238B8D", "#41AB5D", "#B35806", "#4A5D7E",
                 "#08519C", "#3F007D", "#525252", "#969696"))

FUEL_ITEMS <- names(fuel_colors)    # Biogasoline, Biodiesel, Renewable diesel

# lvl: unique (item, comm_group, size), item already a factor in stack order
build_palette <- function(lvl) {
  fuels <- lvl[item %in% FUEL_ITEMS, setNames(fuel_colors[as.character(item)], item)]
  rest  <- lvl[!item %in% FUEL_ITEMS]
  cols  <- unlist(lapply(split(rest, rest$comm_group), function(g) {
    ends <- GROUP_PALETTE[comm_group == g$comm_group[1]]
    if (!nrow(ends)) {
      warning("[43] no hue family for comm_group '", g$comm_group[1], "' -- using grey.")
      ends <- GROUP_PALETTE[comm_group == "Other"]
    }
    g <- g[order(-size)]                                   # largest item = darkest shade
    setNames(rev(colorRampPalette(c(ends$light, ends$dark))(nrow(g) + 1)[-1]),
             as.character(g$item))
  }), use.names = TRUE)
  names(cols) <- sub("^.*\\.", "", names(cols))            # drop the split() prefix
  c(fuels, cols)[levels(lvl$item)]
}

# --- inputs ------------------------------------------------------------------
need <- function(path, who) {
  if (!file.exists(path)) stop("Missing input: ", path, "  (produced by ", who, ")")
  path
}
YRC   <- as.character(YEAR)
X     <- readRDS(need(file.path(MRIO_PATH, "losses", "X.rds"), "13/14"))
Y     <- readRDS(need(file.path(MRIO_PATH, "losses", "Y.rds"), "13"))
E     <- readRDS(need(file.path(base_path, "E.rds"), "16"))
B     <- readRDS(need(file.path(MRIO_PATH, "losses", paste0(YRC, "_L_", allocation, ".rds")), "14"))
io    <- fread(need(file.path(base_path, "io_labels.csv"), "12_b"))
fd    <- fread(need(file.path(MRIO_PATH, "losses", "fd_labels.csv"), "13"))
items <- fread(need("inst/items_full_bcp.csv", "00_update_items_list.R"))[
  , .(comm_code, item, comm_group)]
stopifnot(all(c("iso3c", "comm_code", "item") %in% names(io)), "iso3c" %in% names(fd))
if (!YRC %in% colnames(X) || is.null(Y[[YRC]]) || is.null(E[[YRC]]))
  stop("[43] year ", YEAR, " absent from X / Y / E.")

# ALIGNMENT GUARD. The math below is positional: as.numeric() strips names, so R
# lines up E, X, B and `io` by INDEX. If any artefact was rebuilt against a
# differently-ordered io_labels.csv, one item's extension is divided by another
# item's output and the impact lands on the WRONG item -- silently, and this is a
# by-item figure. Check it loudly before computing anything. (Mirrors 40.)
io_key <- paste0(io$iso3c, "_", io$comm_code)
for (obj in list(list(rownames(X), "X.rds rows"), list(colnames(E[[YRC]]), "E cols"))) {
  if (is.null(obj[[1]])) { warning("[43] ", obj[[2]], ": no names -- trusting position."); next }
  if (!identical(obj[[1]], io_key))
    stop("[43] ", obj[[2]], " != io_labels grid -- extensions would be misattributed. Re-run 16.")
}

# --- the bilateral matrix D: impact at node i, driven by consumer c ----------
# Only biofuel rows of Y carry demand, so B is subset to those columns (N x |bf|)
# rather than multiplied out in full.
Xi <- as.vector(X[, YRC])
f  <- as.numeric(E[[YRC]][STRESSOR, ]) / Xi; f[!is.finite(f)] <- 0

bf_idx <- which(io$comm_code %in% BF_CODES)
cons   <- factor(fd$iso3c)
Mcons  <- sparseMatrix(i = seq_along(cons), j = as.integer(cons), x = 1,
                       dims = c(length(cons), nlevels(cons)))          # Y cols -> consumer
Y_bf   <- as.matrix(Y[[YRC]][bf_idx, , drop = FALSE] %*% Mcons)        # bf nodes x consumer
colnames(Y_bf) <- levels(cons)
D <- f * as.matrix(B[, bf_idx, drop = FALSE] %*% Y_bf)                 # N x consumer
if (sum(D) == 0) stop("[43] the biofuel chains carry no ", STRESSOR, " in ", YEAR, ".")

# --- the three accounts, all keyed (iso3c, comm_code) ------------------------
pba <- data.table(iso3c = io$iso3c, comm_code = io$comm_code, value = rowSums(D)
)[value != 0, .(value = sum(value)), by = .(iso3c, comm_code)][, account := "PBA"]

Dc  <- rowsum(D, group = io$comm_code)                                # source item x consumer
cba <- data.table(comm_code = rep(rownames(Dc), times = ncol(Dc)),
                  iso3c     = rep(colnames(Dc), each  = nrow(Dc)),
                  value     = as.vector(Dc))[value != 0][, account := "CBA"]

va <- fread(need(VA_FILE, "40"))[year == YEAR, .(value = sum(va_resp)),
                                 by = .(iso3c = va_iso3c, comm_code = va_comm_code)
][value != 0][, account := "VA"]
if (!nrow(va)) stop("[43] no VA responsibility rows for ", YEAR, " in ", VA_FILE)

d <- rbindlist(list(pba, cba, va), use.names = TRUE)
d[, account := factor(account, levels = ACCOUNTS)]
d <- merge(d, items, by = "comm_code", all.x = TRUE)
if (anyNA(d$item)) stop("[43] comm_code(s) absent from items_full_bcp.csv: ",
                        paste(unique(d[is.na(item), comm_code]), collapse = ", "))

# --- consistency checks ------------------------------------------------------
# (1) the three accounts re-attribute ONE total -- they must still share it.
tot <- d[, .(total = sum(value)), by = account]
if ((max(tot$total) - min(tot$total)) / max(abs(tot$total)) > TOL)
  warning("[43] the accounts do not share one total:\n",
          paste(sprintf("  %-4s %.6g", tot$account, tot$total), collapse = "\n"))

# (2) PBA/CBA rebuilt here must reproduce 40's country-level accounts CSV.
if (file.exists(ACC_FILE)) {
  ref <- fread(ACC_FILE)[year == YEAR & account %in% c("production", "consumption"),
                         .(ref = sum(value)), by = .(account, iso3c)]
  ref[, account := c(production = "PBA", consumption = "CBA")[account]]
  cmp <- merge(d[account != "VA", .(new = sum(value)), by = .(account = as.character(account), iso3c)],
               ref, by = c("account", "iso3c"), all = TRUE)
  cmp[is.na(new), new := 0][is.na(ref), ref := 0]
  dev <- max(abs(cmp$new - cmp$ref)) / max(abs(cmp$ref), 1)
  if (dev > TOL) warning(sprintf("[43] PBA/CBA differ from 40's accounts CSV (max rel. %.3g) -- same switches?", dev))
  else message("[43] PBA/CBA match 40's accounts CSV.")
} else message("[43] no accounts CSV to cross-check against: ", ACC_FILE)

# --- pool the small items into "Other" ---------------------------------------
# An item is kept if it reaches MIN_ITEM_SHARE in ANY account, so one item = one
# colour in every bar and the stacks stay comparable across the three accounts.
share <- d[, .(v = sum(abs(value))), by = .(account, item)][, share := v / sum(v), by = account]
keep  <- share[share >= MIN_ITEM_SHARE, unique(item)]
d[!item %in% keep, `:=`(item = "Other", comm_group = "Other")]
message(sprintf("[43] %d items kept (>= %.0f%% of an account), %d pooled as 'Other'.",
                length(keep), 100 * MIN_ITEM_SHARE, uniqueN(share$item) - length(keep)))

# --- top-N countries ---------------------------------------------------------
# ONE ranking for all three accounts: the bars only line up per country if the
# countries -- and their order -- are shared. RANK_BY = "max" keeps a country that
# is big in ANY account; "mean" favours those that are consistently big.
ctot <- dcast(d[, .(value = sum(value)), by = .(iso3c, account)],
              iso3c ~ account, value.var = "value", fill = 0)
ctot[, rank_by := switch(RANK_BY,
                         max  = pmax(PBA, CBA, VA),
                         mean = (PBA + CBA + VA) / 3,
                         get(RANK_BY))]
setorder(ctot, -rank_by)
shown <- ctot[seq_len(min(TOP_N, .N))]
message(sprintf("[43] ranked by %s; top %d cover %s of the world total.", RANK_BY, nrow(shown),
                paste(sprintf("%s %.0f%%", ACCOUNTS,
                              100 * colSums(shown[, ..ACCOUNTS]) / colSums(ctot[, ..ACCOUNTS])),
                      collapse = " | ")))

p <- d[iso3c %in% shown$iso3c, .(value = sum(value)), by = .(iso3c, account, item, comm_group)]
p[, iso3c := factor(iso3c, levels = shown$iso3c)]

# --- stack order: fuels at the base, then hue family, then size --------------
lvl <- p[, .(size = sum(abs(value))), by = .(item, comm_group)]
lvl[, grp_rank := match(comm_group, c("Biofuels", GROUP_PALETTE$comm_group))]
lvl[item == "Other", grp_rank := Inf]                     # the grey catch-all always last
setorder(lvl, grp_rank, -size)
lvl[, item := factor(item, levels = item)]
p[, item := factor(item, levels = levels(lvl$item))]
pal <- build_palette(lvl)

# --- plot --------------------------------------------------------------------
# One panel per country, strips BELOW the axis: a nested "account within country"
# axis, so the three bars sit side by side without any manual x-offset arithmetic.
gg <- ggplot(p, aes(x = account, y = value / meta$scale_factor, fill = item)) +
  geom_col(position = position_stack(reverse = TRUE),      # first level at the BASE
           width = 0.85, colour = "white", linewidth = 0.15) +
  facet_wrap(~ iso3c, nrow = 1, strip.position = "bottom") +
  scale_fill_manual(values = pal, drop = FALSE) +
  scale_y_continuous(labels = scales::label_number(big.mark = ","),
                     expand = expansion(mult = c(0, 0.04))) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  labs(x = NULL, y = meta$y_label, fill = NULL,
       title = sprintf("%s responsibility for bio-based transport fuels, %d",
                       meta$short_label, YEAR),
       subtitle = sprintf(paste0("PBA production-based | CBA consumption-based | VA value-added-based ",
                                 "(ex TLS): three attributions of one and the same total.\n",
                                 "Top %d countries by %s account; items below %.0f%% of an account ",
                                 "pooled as 'Other'. PBA/CBA items are where the impact occurs, ",
                                 "VA items are where the value is captured."),
                          nrow(shown), RANK_BY, 100 * MIN_ITEM_SHARE),
       caption = sprintf("%s allocation, %s value-added base; biogasoline + biodiesel + renewable diesel.",
                         allocation, VA_BASE)) +
  theme_minimal(base_size = 11) +
  theme(legend.position    = "bottom",
        legend.key.size    = unit(10, "pt"),
        strip.placement    = "outside",
        strip.background   = element_blank(),
        strip.text         = element_text(face = "bold", size = 9),
        panel.spacing.x    = unit(3, "pt"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.x        = element_text(size = 7, angle = 90, hjust = 1, vjust = 0.5),
        plot.subtitle      = element_text(size = 9, colour = "grey30"),
        plot.caption       = element_text(size = 8, colour = "grey40"))

out <- file.path(PLOT_DIR, sprintf("responsibility_country_item_%s_%s_%s_%d.svg",
                                   STAG, VA_BASE, ATAG, YEAR))
ggsave(out, gg, device = svglite::svglite, width = 14, height = 7.5)
message(">>> [43] wrote ", out)