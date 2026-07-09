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


# Universal assumed producer prices for the HVO co-product bundle -------------
# Renewable diesel (c149) and its HVO co-products biopropane (c150) and
# bionaphtha (c151) have no reliable trade-based producer price, so we assume ONE
# flat price each, held CONSTANT across countries and years (data-poor
# assumption).
universal_bcp_prices <- data.table::data.table(
  comm_code   = c("c149",             "c150",       "c151"),
  item        = c("Renewable diesel", "Biopropane", "Bionaphtha"),
  price_usd_t = c(1500,               800,          950),   # USD/t
  t_per_1000L = c(1 / 1.282,          0.51,         0.71))  # HVO 0.780 / propane / naphtha


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