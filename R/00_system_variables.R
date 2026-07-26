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
# c149/c150/c151 have no unique trading codes, so the trade-based price ladder
# returns nothing. Each gets one flat producer price, constant across countries
# and years: the bid/ask midpoint of the RED-grade, premium-inclusive fob/fca
# ARA spot assessment in Argus Biofuels Issue 26-23, 3 Feb 2026
# Densities are Argus' own for those assessments.
#   c149  RED HVO fob ARA, Class II        2,373.05 / 2,385.87 -> 2,379.46
#   c150  Biopropane fca ARA               1,360.00 / 1,370.00 -> 1,365.00
#   c151  RED bionaphtha fob ARA           2,210.00 / 2,220.00 -> 2,215.00
# Caveats: 2026 prices applied to 2012:2022 without deflation; a single trading
# day stands in for a constant; ARA is a European benchmark used globally (fob
# China HVO II is 27% lower), which cancels in value-allocation shares but not
# in absolute producer values.
universal_bcp_prices <- data.table::data.table(
  comm_code   = c("c149",             "c150",       "c151"),
  item        = c("Renewable diesel", "Biopropane", "Bionaphtha"),
  price_usd_t = c(2379.46,            1365.00,      2215.00),  # USD/t
  t_per_1000L = c(0.780,              0.522,        0.690))    # kg/l


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

source("R/00_helpers.R")