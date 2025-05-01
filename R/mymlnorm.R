#' Maximum Likelihood Estimation for Normal Distribution
#'
#' Computes the log-likelihood surface for a normal distribution with unknown
#' mean (\eqn{\mu}) and standard deviation (\eqn{\sigma}), then finds and visualizes
#' the maximum likelihood estimates (MLEs).
#'
#' @param x A numeric vector of sample data.
#' @param mu A numeric vector of candidate values for the population mean \eqn{\mu}.
#' @param sig A numeric vector of candidate values for the population standard deviation \eqn{\sigma}.
#' @param ... Additional graphical parameters passed to the contour plot.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{x}{The input sample data.}
#'   \item{coord}{The matrix coordinates (row, column) of the MLE in the likelihood surface.}
#'   \item{maxl}{The maximum likelihood value on the original (non-log) scale.}
#' }
#'
#' @details
#' This function evaluates the log-likelihood at each combination of \code{mu} and \code{sig}
#' values using the normal distribution. It visualizes the likelihood surface with a contour
#' plot, overlays the true mean and standard deviation, and highlights the maximum likelihood
#' estimates. Useful for teaching and diagnostics.
#'
#' @examples
#' y <- c(10, 12, 13, 15, 12, 11, 10)
#' mymlnorm(x = y,
#'          mu = seq(8, 16, length = 100),
#'          sig = seq(0.1, 5, length = 100),
#'          lwd = 2, labcex = 1)
#'
#' @export
mymlnorm <- function(x, mu, sig, ...) {
  nmu <- length(mu)
  nsig <- length(sig)
  n <- length(x)
  zz <- c()

  lfun <- function(x, m, p) log(dnorm(x, mean = m, sd = p))

  for (j in 1:nsig) {
    z <- outer(x, mu, lfun, p = sig[j])
    y <- apply(z, 2, sum)
    zz <- cbind(zz, y)
  }

  maxl <- max(exp(zz))
  coord <- which(exp(zz) == maxl, arr.ind = TRUE)

  contour(mu, sig, exp(zz),
          las = 3,
          xlab = expression(mu),
          ylab = expression(sigma),
          axes = TRUE,
          main = expression(paste("L(", mu, ",", sigma, ")", sep = "")),
          ...)

  mlx <- round(mean(x), 2)
  mly <- round(sqrt((n - 1) / n) * sd(x), 2)

  abline(v = mean(x), lwd = 2, col = "Green")
  abline(h = sqrt((n - 1) / n) * sd(x), lwd = 2, col = "Red")

  muest <- mu[coord[1]]
  sigest <- sig[coord[2]]

  abline(v = muest, h = sigest)

  return(list(x = x, coord = coord, maxl = maxl))
}
