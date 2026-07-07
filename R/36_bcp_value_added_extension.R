# =============================================================================
# 36_bcp_value_added_extension.R
# Standalone value-added extension for fabio-bcp: writes V.rds + v_labels.csv to
# output_dir_bcp, parallel to E.rds/ex_labels.csv (NOT mixed into them).
#
# V.rds = year-keyed list of 2 x N matrices, rows value_added_gloria /
# value_added_exiobase, columns = target_order (same grid/order as E, X, L),
# zero-filled where uncovered.  Each row = per-(iso3c,comm_code,year) total VA:
#   (A) FABIO commodities  -> E_bamboo VA strands, ISIC-A or ISIC-C per commodity
#   (B) bio-based commodities -> bcp_value_added_combined_<base>.rds
#
# Self-contained: run AFTER R/16 (reads the E.rds + io_labels.csv it produces).
# =============================================================================

## --- portable repo root: FABIO_BFP_ROOT override, else walk up to the marker ---
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

source("R/00_system_variables.R")   # output_dir, output_dir_bcp
source("R/00_run_config.R")          # tag()

# --- v123 -> bcp column remap (mirrors R/16_extensions_main.R) ----------------
build_remap <- function(items_src, items_bcp) {
  src <- unique(as.data.table(items_src)[, .(item_code, comm_code_src = comm_code)])
  bcp <- unique(as.data.table(items_bcp)[, .(item_code, comm_code_bcp = comm_code)])
  merge(src, bcp, by = "item_code", all = FALSE)
}
remap_columns <- function(yr_mat, remap_dt) {
  cn <- colnames(yr_mat)
  if (is.null(cn)) return(yr_mat)
  area_part <- sub("_[^_]+$", "", cn)
  comm_part <- sub("^[^_]+_", "", cn)
  new_comm  <- remap_dt$comm_code_bcp[match(comm_part, remap_dt$comm_code_src)]
  keep <- !is.na(new_comm)
  out  <- yr_mat[, keep, drop = FALSE]
  colnames(out) <- paste0(area_part[keep], "_", new_comm[keep])
  out
}
remap_extension <- function(ext_list, remap_dt) {
  lapply(ext_list, remap_columns, remap_dt = remap_dt) |> setNames(names(ext_list))
}

# --- helpers -----------------------------------------------------------------
# Read a per-base combined bio-based VA table from intermediate_data.
read_combined_va <- function(file_key) {
  as.data.table(readRDS(tag(sprintf(
    "intermediate_data/bcp_value_added_combined_%s.rds", file_key))))
}

# Sum a named numeric vector over duplicate names.
.agg_by_name <- function(v) if (length(v)) tapply(v, names(v), sum) else v

# Build the single-row VA extension for one base.
#   base         "gloria" / "exiobase"
#   E_bamboo     year-keyed stressor x N matrices (v123 comm_code columns),
#                carrying VA_{wages,capital,tls}_isic_{a,c}_<base> rows
#   combined_dt  bio-based combined table (iso3c, comm_code, year, value_added_usd)
#   items_isic   named vector comm_code -> ISIC ("A"/"C"/NA), v123 convention
#   target_order bcp column order (== E/X/L)
#   year_names   years to emit (aligned, zero-filled)
build_va_extension <- function(base, E_bamboo, combined_dt, items_isic,
                               target_order, year_names) {
  
  row_nm <- paste0("value_added_", base)
  a_rows <- sprintf("VA_%s_isic_a_%s", c("wages", "capital", "tls"), base)
  c_rows <- sprintf("VA_%s_isic_c_%s", c("wages", "capital", "tls"), base)
  
  # (A) FABIO commodities: strand-sum + ISIC-column A/C/none selection, on v123 columns.
  yrs_a <- intersect(year_names, names(E_bamboo))
  va_v123 <- lapply(yrs_a, function(yr) {
    m <- E_bamboo[[yr]]
    stopifnot(all(c(a_rows, c_rows) %in% rownames(m)))
    a_sum <- colSums(m[a_rows, , drop = FALSE])
    c_sum <- colSums(m[c_rows, , drop = FALSE])
    comm  <- sub("^[^_]+_", "", colnames(m))
    isic  <- items_isic[match(comm, names(items_isic))]
    sel   <- numeric(ncol(m))
    sel[isic %in% "A"] <- a_sum[isic %in% "A"]
    sel[isic %in% "C"] <- c_sum[isic %in% "C"]
    matrix(sel, nrow = 1, dimnames = list(row_nm, colnames(m)))
  })
  names(va_v123) <- yrs_a
  va_bcp <- if (length(va_v123)) remap_extension(va_v123, remap_v123) else list()
  
  # (B) bio-based commodities: keyed "<iso3c>_<comm_code>", drop NA value_added_usd.
  diag <- vector("list", length(year_names))
  overlaps_all <- character(0)
  
  out <- lapply(seq_along(year_names), function(i) {
    yr <- year_names[i]
    v  <- setNames(numeric(length(target_order)), target_order)
    
    a_vec <- if (!is.null(va_bcp[[yr]]))
      .agg_by_name(setNames(as.numeric(va_bcp[[yr]]), colnames(va_bcp[[yr]]))) else numeric(0)
    a_keys <- names(a_vec)[names(a_vec) %in% target_order]
    
    b  <- combined_dt[year == as.integer(yr)]
    na_dropped <- sum(!is.finite(b$value_added_usd))
    b  <- b[is.finite(value_added_usd)]
    # Group by the real columns (always length nrow) rather than by a pasted
    # key: on an empty-year slice paste0(character(0), "_", character(0)) is
    # "_" (length 1, recycle0 = FALSE default), which data.table rejects as a
    # by-list length mismatch.  Build the key column after aggregating.
    b_agg <- b[, .(v = sum(value_added_usd)), by = .(iso3c, comm_code)]
    b_agg[, key := paste0(iso3c, "_", comm_code)]
    b_keys <- b_agg$key[b_agg$key %in% target_order]
    
    v[a_keys] <- a_vec[a_keys]
    v[b_keys] <- b_agg$v[match(b_keys, b_agg$key)]   # (B) wins overlaps
    
    overlap <- intersect(a_keys, b_keys)
    overlaps_all <<- union(overlaps_all, overlap)
    diag[[i]] <<- data.table(year = yr, from_v2 = length(a_keys),
                             from_bcp = length(b_keys), overlaps = length(overlap),
                             na_dropped = na_dropped)
    
    matrix(v, nrow = 1, dimnames = list(row_nm, target_order))
  })
  names(out) <- year_names
  
  message(sprintf("[V:%s] coverage (columns filled):", base))
  print(rbindlist(diag))
  if (length(overlaps_all))
    warning(sprintf("[V:%s] %d overlapping key(s) between v2 and bcp sources: %s",
                    base, length(overlaps_all), paste(overlaps_all, collapse = ", ")))
  
  out
}

# --- inputs (same artefacts R/16 uses; E.rds gives the year set) --------------
items_v123    <- fread("inst/items_full_123.csv")
items_bcp     <- fread("inst/items_full_bcp.csv")
io_labels_bcp <- fread(file.path(output_dir_bcp, "io_labels.csv"))

remap_v123   <- build_remap(items_v123, items_bcp)
target_order <- paste0(io_labels_bcp$iso3c, "_", io_labels_bcp$comm_code)
items_isic   <- setNames(items_v123$ISIC, items_v123$comm_code)   # comm_code -> ISIC

E_bamboo    <- readRDS(paste0(output_dir, "E_bamboo.rds"))
E_bcp_years <- names(readRDS(paste0(output_dir_bcp, "E.rds")))     # year set (== names(E_bcp) in R/16)
va_years    <- intersect(E_bcp_years, names(E_bamboo))             # drop years absent from E_bamboo
va_years    <- intersect(va_years, as.character(years))            # keep only the bcp model window (2012:2022);
# the bcp E.rds is named 2011:2023 by R/16, but the
# bio-based VA sources (R/34) only cover `years`.

# --- build both bases, rbind (fixed order gloria, exiobase), guard, save ------
bases <- c("gloria", "exiobase")
V_by_base <- lapply(bases, function(b)
  build_va_extension(b, E_bamboo, read_combined_va(b), items_isic, target_order, va_years))
names(V_by_base) <- bases

V_bcp <- lapply(va_years, function(yr)
  do.call(rbind, lapply(bases, function(b) V_by_base[[b]][[yr]])))
names(V_bcp) <- va_years

v_labels <- fread("inst/V_labels_initial.csv")
v_labels <- v_labels[order(match(Stressor, paste0("value_added_", bases)))]

for (yr in names(V_bcp)) {
  stopifnot(ncol(V_bcp[[yr]]) == length(target_order),
            all(colnames(V_bcp[[yr]]) == target_order),
            all(rownames(V_bcp[[yr]]) == v_labels$Stressor))
}

saveRDS(V_bcp, paste0(output_dir_bcp, "V.rds"))
fwrite(v_labels, paste0(output_dir_bcp, "v_labels.csv"))