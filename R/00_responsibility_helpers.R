# =============================================================================
# 00_responsibility_helpers.R
# Building blocks shared by the responsibility accounts: the country grid and
# the bilateral impact matrix D. Sourced by 40 and 43 so the matrix is built in
# one place and cannot drift between them.
# =============================================================================

# One country universe for both axes of D: producers (io rows) and consumers
# (Y columns). T_origin rolls the io grid up to producing countries, S_fd rolls
# the final-demand columns up to consuming countries.
country_grid <- function(io, fd) {
  countries <- sort(unique(c(io$iso3c, fd$iso3c)))
  N <- nrow(io); R <- length(countries)
  list(
    countries = countries,
    R         = R,
    T_origin  = Matrix::sparseMatrix(i = seq_len(N), j = match(io$iso3c, countries),
                                     x = 1, dims = c(N, R)),
    S_fd      = Matrix::sparseMatrix(i = seq_along(fd$iso3c), j = match(fd$iso3c, countries),
                                     x = 1, dims = c(length(fd$iso3c), R))
  )
}

# Impact driven by final demand for ONE biofuel group, resolved by consumer.
#   f          impact intensity e / x, over the io grid
#   B          Leontief inverse
#   Yc         final demand rolled up to consuming countries (N x R), Y %*% S_fd
#   sel        0/1 mask over the io grid selecting the group's commodities
#   T_origin   supplied -> country x country ; NULL -> node x country
build_D <- function(f, B, Yc, sel, T_origin = NULL, countries = NULL) {
  Yg <- Matrix::Diagonal(x = sel) %*% Yc
  D  <- Matrix::Diagonal(x = f) %*% (B %*% Yg)
  if (!is.null(T_origin)) D <- Matrix::crossprod(T_origin, D)
  D <- as.matrix(D)
  if (!is.null(countries)) {
    colnames(D) <- countries
    if (!is.null(T_origin)) rownames(D) <- countries
  }
  D
}
