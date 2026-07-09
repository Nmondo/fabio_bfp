# define system variables ----------------------------------------------------------------

# Determine the output directory based on the branch
branch_name <- system("git symbolic-ref --short HEAD", intern = TRUE)
if (branch_name == "data-1986-2013") {
  output_dir <- "/mnt/nfs_fineprint/tmp/fabio/v1.2/8613"
} else if (branch_name == "data-2010-current") {
  output_dir <- "/mnt/nfs_fineprint/tmp/fabio/v2/"
} else {
  stop("Unknown branch!")
}

input_path <- output_dir
output_dir_v525 <-"/mnt/nfs_fineprint/tmp/fabio/v2_525/"
output_dir_bcp  <- "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/"
# Ensure the directories exist
dir.create(output_dir,     recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir_bcp, recursive = TRUE, showWarnings = FALSE)

# # Use the output_dir in your code
# write.csv(data, file.path(output_dir, "output.csv"))

# Determine years
years <- 2012:2022


# Universal producer prices for the HVO co-product bundle ---------------------
# Basis: European wholesale (fob/fca ARA), premium-INCLUSIVE, i.e. the basic
# ex-terminal price the producer actually receives. One flat price each, held
# CONSTANT across countries and years. Values are indicative central estimates
# for the 2023-2026 window (see per-item ranges + sources). For a consistent
# basis, all three are at comparable per-tonne wholesale value.
universal_bcp_prices <- data.table::data.table(
  comm_code   = c("c149",             "c150",       "c151"),
  item        = c("Renewable diesel", "Biopropane", "Bionaphtha"),
  price_usd_t = c(2300,               2000,         2500),   # USD/t, see sources
  t_per_1000L = c(1 / 1.282,          0.51,         0.71))   # HVO 0.780 / propane / naphtha

# Sources & ranges (central value chosen mid-to-conservative in each range):
#  c149 Renewable diesel (HVO): ~1,900-2,600 USD/t.
#     European UCO-HVO spot ~2,530 USD/mt, late 2025 (Vesper Tool, Feb 2026).
#     Benchmark series: Argus / S&P Platts / Fastmarkets HVO fob ARA (paywalled).
#     Public reference: Neste "Renewable Products" market data page.
#  c150 Biopropane: ~1,800-2,500 USD/t.
#     European bio-propane ~2,496 USD/mt w/ ~1,950 USD/mt premium over fossil
#     propane (S&P Global Commodity Insights, 2023); premiums have since
#     compressed on weak petrochemical demand -> central set below the peak.
#     Benchmark series: Argus biopropane fca ARA (paywalled).
#  c151 Bionaphtha: ~2,200-3,200 USD/t (Inkwood Research; renewable naphtha
#     avg vs ~550-680 USD/t fossil naphtha). Scarce, petrochem-demand-driven.
#     Benchmark series: Argus / Quantum bionaphtha fob ARA (paywalled).


# Recursive sum over vectors with NA, returns NA if all values are NA
na_sum <- function(..., rowwise = TRUE) {
  dots <- list(...)
  if(length(dots) == 1) { # Base
    ifelse(all(is.na(dots[[1]])), NA_real_, sum(dots[[1]], na.rm = TRUE))
  } else { # Recurse
    if(rowwise) {
      x <- do.call(cbind, dots)
      return(apply(x, 1, na_sum))
    }
    return(na_sum(vapply(dots, na_sum, double(1L))))
  }
}

# Aggregate a matrix based on its column names
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }