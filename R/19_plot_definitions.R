# ============================================================================
# 19_plot_definitions.R
#
# Central, shared definitions for the FABIO-BCP plotting scripts:
#   - general colour vectors (biofuels, continents)
#   - commodity sets (biofuels, biopolymers)
#   - metadata tables (indicators, commodities, feedstocks)
#   - the FIXED feedstock colour palette (one feedstock = one colour)
#
# `source()` this once near the top of any plotting script, AFTER loading
# data.table (the meta tables below are data.tables).
#
#   library(data.table)
#   source("R/19_plot_definitions.R")
# ============================================================================

if (!requireNamespace("data.table", quietly = TRUE))
  stop("19_plot_definitions.R requires the 'data.table' package.")
library(data.table)

# ============================================================================
# 1. GENERAL COLOUR VECTORS
# ============================================================================

# Colours for biofuels
fuel_colors <- c(
  "Biogasoline"      = "#D4A017",
  "Biodiesel"        = "#5B2C6F",
  "Renewable diesel" = "#9B59B6"
)

# Okabe-Ito-based continent palette (colour-blind safe)
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

# ============================================================================
# 2. COMMODITY SETS
# ============================================================================

bf_set <- c("c146", "c147", "c149", "c150", "c151")          # biofuels
bp_set <- paste0("c", 152:170)                               # biopolymers / building blocks

# ============================================================================
# 3. METADATA TABLES
# ============================================================================

# ---- indicator labels ------------------------------------------------------
# NAMING. The `LCIM_EQ_*` rows of E.rds are LC-IMPACT ECOSYSTEM QUALITY damage
# factors, expressed as a potentially disappeared fraction of species over time
# (PDF*yr). They are BIODIVERSITY-LOSS indicators, not ecotoxicity: 15_8 builds
# each realm by summing that realm's impact CATEGORIES, and ecotoxicity is not
# one of them --
#     LCIM_EQ_freshwater  = climate + eutrophication + water use
#     LCIM_EQ_marine      = climate
#     LCIM_EQ_terrestrial = climate + LAND USE + acidification
# The previous "... ecotoxicity" labels were wrong for all three realms.
#
# SCOPE WARNING -- two different indicators currently share the name
# "LCIM_EQ_terrestrial":
#   * 40/41/42 read the E.rds row directly       -> climate + land use + acidification
#   * 19_01b (lines ~115-145) REBUILDS it as     -> climate + acidification only
#     ("ruling out land use", to avoid double-counting land with IBIF)
# Land use is the dominant terrestrial pathway for agriculture, so the two are
# NOT comparable and must not carry the same axis label. Until that is resolved,
# label whichever one a script actually computed:
#   * keep `LCIM_EQ_terrestrial` for the FULL (land-use-inclusive) E.rds row;
#   * have 19_01b/19_02 name their recomputed series
#     `LCIM_EQ_terrestrial_excl_land_use` and pick up the row below.
indicator_meta <- data.table(
  indicator    = c("land_harv", "ibif_total",
                   "LCIM_EQ_freshwater", "LCIM_EQ_marine", "LCIM_EQ_terrestrial",
                   "LCIM_EQ_terrestrial_excl_land_use",
                   "LCIM_EQ_terrestrial_land_use",
                   "LCIM_EQ_terrestrial_climate",
                   "LCIM_EQ_terrestrial_acidification"),
  scale_factor = c(1e3, rep(1, 8)),
  y_label      = c(
    "Land use (1000 ha)",
    "Species abundance loss (MSA\u00b7km\u00b2\u00b7yr)",
    "Freshwater biodiversity loss (PDF\u00b7yr)",
    "Marine biodiversity loss (PDF\u00b7yr)",
    "Terrestrial biodiversity loss (PDF\u00b7yr)",
    "Terrestrial biodiversity loss, excl. land use (PDF\u00b7yr)",
    "Terrestrial biodiversity loss from land use (PDF\u00b7yr)",
    "Terrestrial biodiversity loss from climate change (PDF\u00b7yr)",
    "Terrestrial biodiversity loss from acidification (PDF\u00b7yr)"),
  short_label  = c(
    "Land use", "IBIF",
    "Freshwater biodiversity loss",
    "Marine biodiversity loss",
    "Terrestrial biodiversity loss",
    "Terrestrial biodiversity loss (excl. land use)",
    "Terrestrial biodiversity loss (land use)",
    "Terrestrial biodiversity loss (climate)",
    "Terrestrial biodiversity loss (acidification)")
)

commodity_meta <- data.table(
  item      = c("Biogasoline", "Biodiesel", "Renewable diesel"),
  comm_code = c("c146", "c147", "c149")
)

feedstock_meta <- data.table(
  feedstock = c(
    # Oils & fats (biodiesel / HVO feedstocks)
    "Palm Oil", "Palmkernel Oil", "Coconut Oil",
    "Soyabean Oil", "Rape and Mustard Oil", "Sunflowerseed Oil",
    "Cottonseed Oil", "Maize Germ Oil",
    "Used Cooking Oil", "Fats, Animals, Raw",
    # Sugar / starch (bioethanol feedstocks)
    "Sugar cane", "Sugar beet", "Molasses",
    "Maize and products", "Wheat and products", "Rice and products",
    "Barley and products", "Rye and products", "Triticale",
    "Sorghum and products", "Cassava and products",
    # Residual
    "Other, Waste"
  ),
  category = c(
    rep("Oilcrops",            10),
    rep("Starchy / Sugar crops", 11),
    "Other"
  )
)

# ============================================================================
# 4. FIXED FEEDSTOCK COLOUR PALETTE
#
# One feedstock = one colour, permanently. Adding new data in the future
# never reshuffles colours for feedstocks already in the map.
#
# Design rationale (revised):
#   The earlier "warm family for sugar/starch, cool family for oilcrops"
#   scheme packed each category into a narrow slice of the colour wheel,
#   which made feedstocks WITHIN a category hard to tell apart in a stacked
#   bar. That family logic is dropped. Instead, each category's pool now
#   SPANS the full hue + lightness range and was selected to MAXIMISE
#   perceptual contrast: the colours were chosen greedily by max-min
#   CIEDE2000 distance over a perceptually uniform candidate grid, so the
#   minimum pairwise distance within each pool is ~18-20 ΔE (well above the
#   ~10-12 ΔE needed to separate adjacent slices in a stack).
#
#   The two pools deliberately reuse the whole colour wheel and may share a
#   hue (e.g. both contain a green): oilcrops and starchy/sugar crops are
#   very rarely stacked in the same bar, so cross-category similarity is
#   acceptable. WITHIN a category, every pair is strongly distinct.
#
#   "Other" / "Other, Waste" stay neutral greys (dark vs light).
#
#   Each pool has 12 slots: enough to fix every currently-known feedstock
#   plus headroom for new ones, drawn from the named reserve slots below.
# ============================================================================

# ---- Max-contrast pool: Starchy / Sugar crops (12 slots) -------------------
# min pairwise ΔE2000 ≈ 20.4 (slot 1 was #61C748 leaf green; see note there)
starchy_pool <- c(
  "#24757E",  # deep teal  (slot 1 = Sugar cane; see note below)
  "#9E1597",  # magenta
  "#00441B",  # dark forest green
  "#F53666",  # raspberry / pink-red
  "#1DC6AF",  # teal
  "#874A18",  # brown
  "#3EBBFF",  # sky blue
  "#5A5D05",  # dark olive
  "#CF9EEB",  # light lavender
  "#CAAE5D",  # khaki / gold
  "#3257A0",  # royal blue
  "#F69978"   # salmon (reserve slot)
)
# NOTE on slot 1 (Sugar cane). It was #61C748 (leaf green), which sits only
# ~8.3 dE from the oilcrops' slot-1 green #8EC029 (Palm Oil). The two pools are
# meant to be "rarely stacked together", but the biofuel figures (43_*, 18_01b,
# 19_01b) DO put a bioethanol feedstock (sugar cane) next to a biodiesel one
# (palm oil), so those two greens were indistinguishable there. Slot 1 is now a
# deep teal #24757E: 43 dE from Palm Oil, >=23 dE within its own pool (so the
# max-contrast guarantee is preserved), and 28 dE from the pool's other teal
# (#1DC6AF, Sugar beet). The two sugar crops now read as a distinct cool pair
# rather than colliding with the oilcrop greens.

# ---- Max-contrast pool: Oilcrops (12 slots) --------------------------------
# min pairwise ΔE2000 ≈ 18.1
oilcrop_pool <- c(
  "#8EC029",  # yellow-green
  "#7E34B1",  # purple
  "#EEA119",  # amber / orange
  "#E63DA7",  # hot pink
  "#016833",  # deep green
  "#FD938A",  # light coral
  "#543005",  # espresso
  "#B4A6F8",  # periwinkle
  "#878834",  # olive
  "#3588D1",  # azure blue
  "#9B3649",  # maroon (reserve slot)
  "#B87340"   # tan / ochre (reserve slot)
)

# ---- Neutral pool: Other / Other, Waste ------------------------------------
other_pool <- c(
  "Other, Waste" = "#4D4D4D",  # dark grey
  "Other"        = "#999999"   # light grey
)

# ---- Fixed name -> colour assignment ---------------------------------------
# Ordering follows the legend: starchy/sugar crops first, then oilcrops,
# then the two neutrals. Names match feedstock_meta$feedstock exactly.

starchy_names <- c(
  "Sugar cane",
  "Maize and products",
  "Molasses",
  "Cassava and products",
  "Sugar beet",
  "Wheat and products",
  "Rice and products",
  "Sorghum and products",
  "Rye and products",
  "Triticale",
  "Barley and products"
)

oilcrop_names <- c(
  "Palm Oil",
  "Soyabean Oil",
  "Rape and Mustard Oil",
  "Used Cooking Oil",
  "Fats, Animals, Raw",
  "Maize Germ Oil",
  "Sunflowerseed Oil",
  "Coconut Oil",
  "Palmkernel Oil",
  "Cottonseed Oil"
)

feedstock_color_map <- c(
  setNames(starchy_pool[seq_along(starchy_names)], starchy_names),
  setNames(oilcrop_pool[seq_along(oilcrop_names)], oilcrop_names),
  other_pool
)

# Unused slots in each pool are kept as a named "reserve" so future additions
# draw from a known, stable sequence rather than a random hue_pal.
starchy_reserve <- starchy_pool[(length(starchy_names) + 1):length(starchy_pool)]
oilcrop_reserve <- oilcrop_pool[(length(oilcrop_names) + 1):length(oilcrop_pool)]

# ============================================================================
# 5. ACCESSOR: extend the fixed map to whatever feedstocks are present in a
#    given dataset, pulling unmapped/new feedstocks from the correct
#    category's reserve pool (deterministically, alphabetically) so repeated
#    calls are stable across runs/sessions.
# ============================================================================
get_feedstock_palette <- function(feedstocks,
                                  feedstock_meta = get0("feedstock_meta",
                                                        ifnotfound = NULL),
                                  other_color    = unname(feedstock_color_map[["Other"]])) {
  
  feedstocks <- unique(as.character(feedstocks))
  known      <- intersect(feedstocks, names(feedstock_color_map))
  unknown    <- setdiff(feedstocks, names(feedstock_color_map))
  
  out <- feedstock_color_map[known]
  
  if (length(unknown) > 0) {
    if (is.null(feedstock_meta)) {
      warning("Unmapped feedstocks found and no feedstock_meta available; ",
              "assigning '", other_color, "' to: ",
              paste(unknown, collapse = ", "))
      out_unknown <- setNames(rep(other_color, length(unknown)), unknown)
    } else {
      cat_lookup <- setNames(as.character(feedstock_meta$category),
                             as.character(feedstock_meta$feedstock))
      cats       <- ifelse(unknown %in% names(cat_lookup),
                           cat_lookup[unknown], "Other")
      
      assign_one <- function(name, cat) {
        if (cat == "Oilcrops" && length(oilcrop_reserve) > 0) {
          idx <- (which(sort(unknown[cats == cat]) == name) - 1) %% length(oilcrop_reserve) + 1
          oilcrop_reserve[idx]
        } else if (cat == "Starchy / Sugar crops" && length(starchy_reserve) > 0) {
          idx <- (which(sort(unknown[cats == cat]) == name) - 1) %% length(starchy_reserve) + 1
          starchy_reserve[idx]
        } else {
          other_color
        }
      }
      
      out_unknown <- setNames(
        mapply(assign_one, unknown, cats, SIMPLIFY = TRUE),
        unknown
      )
      
      if (sum(cats == "Oilcrops") > length(oilcrop_reserve) ||
          sum(cats == "Starchy / Sugar crops") > length(starchy_reserve)) {
        warning("More new feedstocks in a category than reserve colours ",
                "available; some reserve colours will repeat. Consider ",
                "adding hex codes to starchy_pool / oilcrop_pool.")
      }
    }
    out <- c(out, out_unknown)
  }
  
  out[feedstocks]   # return in the same order as input
}

# ============================================================================
# Example usage:
#
#   library(data.table)
#   source("R/19_plot_definitions.R")
#
#   pal <- get_feedstock_palette(unique(d_plot$feedstock))   # feedstock_meta auto-used
#   ggplot(d_plot, aes(year, value, fill = feedstock)) +
#     geom_col() +
#     scale_fill_manual(values = pal)
# ============================================================================