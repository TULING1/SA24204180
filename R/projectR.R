#' @title Kernel Density Estimation using R
#' @description Kernel Density Estimation using R
#' @importFrom stats dnorm
#' @useDynLib SA24204180
#' @name kde
#' @param x A numeric vector of data points.
#' @param h Numeric value representing the bandwidth.
#' @param x_grid A numeric vector of points at which to evaluate the density.
#' @return A numeric vector representing the estimated density at each point in x_grid.
#' @export
kde <- function(x, h, x_grid) {
  n <- length(x)
  k <- function(u) { exp(-0.5 * u^2) / sqrt(2 * pi) }  # Gaussian kernel

  # Initialize the density vector
  density <- numeric(length(x_grid))

  # Calculate the density estimate for each point in x_grid
  for (i in seq_along(x_grid)) {
    for (j in seq_along(x)) {
      density[i] <- density[i] + k((x_grid[i] - x[j]) / h)
    }
    density[i] <- density[i] / (n * h)
  }

  return(density)
}
