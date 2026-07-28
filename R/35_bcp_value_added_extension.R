# =============================================================================
# 35_bcp_value_added_extension.R
# Standalone value-added extension for fabio-bcp: writes V.rds + v_labels.csv,
# parallel to E.rds/ex_labels.csv (not mixed into them).  Both are written to
# intermediate_data/ (repo-local, always writable) and to output_dir_bcp (the
# shared mnt tree, when writable).
#
# V.rds = year-keyed list of 8 x N matrices, rows = the two aggregate totals
# (value_added_gloria / value_added_exiobase) plus the six components
# value_added_{wages,capital,tls}_{gloria,exiobase}, columns = target_order (same
# grid/order as E, X, L), zero-filled where uncovered.  Each row = per-
# (iso3c,comm_code,year) VA:
#   (A) FABIO commodities  -> v2 V.rds VA strands (VA_{wages,capital,tls}_isic_{a,c}_<base>),
#                             ISIC-A or ISIC-C per commodity
#   (B) bio-based commodities -> bcp_value_added_combined_<base>.rds
# The total rows equal the sum of their three components per cell.
#
# Inputs: the v2 value-added block V.rds (from output_dir) supplies the FABIO-
# commodity strands; R/34 supplies the bio-based combined tables; the bcp E.rds +
# io_labels.csv (from output_dir_bcp) fix the column grid and year set.  Run AFTER
# R/16 and after the v2 pipeline has produced output_dir/V.rds.
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
  dt <- as.data.table(readRDS(tag(sprintf(
    "intermediate_data/bcp_value_added_combined_%s.rds", file_key))))
  need <- c("iso3c", "comm_code", "year", "value_added_usd",
            sprintf("value_added_%s_usd", c("wages", "capital", "tls")))
  miss <- setdiff(need, names(dt))
  if (length(miss))
    stop("combined ", file_key, " table is missing column(s): ",
         paste(miss, collapse = ", "), " -- re-run 34.")
  dt
}

VA_COMPONENTS <- c("wages", "capital", "tls")

# Build the total + three component VA rows for one base.
#   base         "gloria" / "exiobase"
#   va_v2        v2 value-added block (V.rds): year-keyed VA-strand x N matrices
#                (v123 comm_code columns), rows VA_{wages,capital,tls}_isic_{a,c}_<base>
#   combined_dt  bio-based combined table (iso3c, comm_code, year,
#                value_added_usd + value_added_{wages,capital,tls}_usd)
#   items_isic   named vector comm_code -> ISIC ("A"/"C"/NA), v123 convention
#   target_order bcp column order (== E/X/L)
#   year_names   years to emit (aligned, zero-filled)
# Returns a year-keyed list of 4 x N matrices (rows: total, wages, capital, tls).
build_va_extension <- function(base, va_v2, combined_dt, items_isic,
                               target_order, year_names) {
  
  rowtypes <- c("total", VA_COMPONENTS)
  row_nm   <- c(total = paste0("value_added_", base),
                setNames(sprintf("value_added_%s_%s", VA_COMPONENTS, base), VA_COMPONENTS))
  a_rows   <- sprintf("VA_%s_isic_a_%s", VA_COMPONENTS, base)
  c_rows   <- sprintf("VA_%s_isic_c_%s", VA_COMPONENTS, base)
  
  # (A) FABIO commodities: per-component A/C-column selection (no strand-sum),
  # total = sum of the three, on v123 columns.
  yrs_a <- intersect(year_names, names(va_v2))
  va_v123 <- lapply(yrs_a, function(yr) {
    m <- va_v2[[yr]]
    stopifnot(all(c(a_rows, c_rows) %in% rownames(m)))
    comm <- sub("^[^_]+_", "", colnames(m))
    isic <- items_isic[match(comm, names(items_isic))]
    is_a <- isic %in% "A"; is_c <- isic %in% "C"
    sel_comp <- function(comp) {
      s <- numeric(ncol(m))
      s[is_a] <- m[sprintf("VA_%s_isic_a_%s", comp, base), ][is_a]
      s[is_c] <- m[sprintf("VA_%s_isic_c_%s", comp, base), ][is_c]
      s
    }
    w <- sel_comp("wages"); cap <- sel_comp("capital"); tl <- sel_comp("tls")
    mat <- rbind(w + cap + tl, w, cap, tl)
    dimnames(mat) <- list(row_nm[rowtypes], colnames(m))
    mat
  })
  names(va_v123) <- yrs_a
  va_bcp <- if (length(va_v123)) remap_extension(va_v123, remap_v123) else list()
  
  # (B) bio-based commodities: keyed "<iso3c>_<comm_code>", drop NA value_added_usd.
  diag <- vector("list", length(year_names))
  overlaps_all <- character(0)
  
  out <- lapply(seq_along(year_names), function(i) {
    yr <- year_names[i]
    M  <- matrix(0, nrow = length(rowtypes), ncol = length(target_order),
                 dimnames = list(row_nm[rowtypes], target_order))
    
    a_keys <- character(0)
    if (!is.null(va_bcp[[yr]])) {
      am     <- va_bcp[[yr]]
      am_agg <- t(rowsum(t(am), group = colnames(am)))   # sum duplicate columns per row
      a_keys <- colnames(am_agg)[colnames(am_agg) %in% target_order]
      if (length(a_keys)) M[, a_keys] <- am_agg[, a_keys]
    }
    
    b <- combined_dt[year == as.integer(yr)]
    na_dropped <- sum(!is.finite(b$value_added_usd))
    b <- b[is.finite(value_added_usd)]
    b_agg <- b[, .(total   = sum(value_added_usd),
                   wages   = sum(value_added_wages_usd),
                   capital = sum(value_added_capital_usd),
                   tls     = sum(value_added_tls_usd)),
               by = .(iso3c, comm_code)]
    b_agg[, key := paste0(iso3c, "_", comm_code)]
    b_keys <- b_agg$key[b_agg$key %in% target_order]
    if (length(b_keys)) {                                 # (B) wins overlaps
      idx <- match(b_keys, b_agg$key)
      M[row_nm["total"],   b_keys] <- b_agg$total[idx]
      M[row_nm["wages"],   b_keys] <- b_agg$wages[idx]
      M[row_nm["capital"], b_keys] <- b_agg$capital[idx]
      M[row_nm["tls"],     b_keys] <- b_agg$tls[idx]
    }
    
    overlap <- intersect(a_keys, b_keys)
    overlaps_all <<- union(overlaps_all, overlap)
    diag[[i]] <<- data.table(year = yr, from_v2 = length(a_keys),
                             from_bcp = length(b_keys), overlaps = length(overlap),
                             na_dropped = na_dropped)
    M
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

va_v2       <- readRDS(paste0(output_dir, "V_updated.rds"))               # v2 value-added strands
need_rows   <- sprintf("VA_%s_isic_%s_%s",
                       rep(VA_COMPONENTS, times = 4),
                       rep(rep(c("a", "c"), each = 3), times = 2),
                       rep(c("gloria", "exiobase"), each = 6))
miss_rows   <- setdiff(need_rows, rownames(va_v2[[1]]))
if (length(miss_rows))
  stop("v2 V.rds is missing strand row(s): ", paste(miss_rows, collapse = ", "),
       " -- re-run the v2 R/14_5.")
E_bcp_years <- names(readRDS(paste0(output_dir_bcp, "E.rds")))     # year set (== names(E_bcp) in R/16)
va_years    <- intersect(E_bcp_years, names(va_v2))               # drop years absent from v2 V.rds
va_years    <- intersect(va_years, as.character(years))            # keep only the bcp model window (2012:2022);
# the bcp E.rds is named 2011:2023 by R/16, but the
# bio-based VA sources (R/34) only cover `years`.

# --- build both bases, rbind to 8 rows in a fixed order, guard, save ----------
bases     <- c("gloria", "exiobase")
row_order <- c(paste0("value_added_", bases),
               unlist(lapply(bases, function(b)
                 sprintf("value_added_%s_%s", VA_COMPONENTS, b))))

V_by_base <- lapply(bases, function(b)
  build_va_extension(b, va_v2, read_combined_va(b), items_isic, target_order, va_years))
names(V_by_base) <- bases

V_bcp <- lapply(va_years, function(yr) {
  m <- do.call(rbind, lapply(bases, function(b) V_by_base[[b]][[yr]]))
  m[row_order, , drop = FALSE]
})
names(V_bcp) <- va_years

v_labels <- fread("inst/V_labels_initial.csv")
v_labels <- v_labels[match(row_order, Stressor)]
if (anyNA(v_labels$Stressor))
  stop("inst/V_labels_initial.csv is missing row(s): ",
       paste(setdiff(row_order, v_labels$Stressor), collapse = ", "))

for (yr in names(V_bcp)) {
  stopifnot(ncol(V_bcp[[yr]]) == length(target_order),
            all(colnames(V_bcp[[yr]]) == target_order),
            all(rownames(V_bcp[[yr]]) == v_labels$Stressor))
}

# Repo-local copy (always writable) — this is what the 40s scripts consume
dir.create("intermediate_data", showWarnings = FALSE, recursive = TRUE)
saveRDS(V_bcp,   "intermediate_data/V.rds")
fwrite(v_labels, "intermediate_data/v_labels.csv")

# Shared mnt copy (requires fabio_bcp write access).
saveRDS(V_bcp,   paste0(output_dir_bcp, "V.rds"))
fwrite(v_labels, paste0(output_dir_bcp, "v_labels.csv"))