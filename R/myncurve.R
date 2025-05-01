#' Plots a Normal Curve with Shaded Probability Region
#'
#' This function plots a normal distribution curve with mean `mu` and standard deviation `sigma`,
#' shading the area under the curve from `-Inf` to `a`. It also calculates and returns the probability `P(X ≤ a)`.
#'
#' @param mu Mean of the normal distribution.
#' @param sigma Standard deviation of the normal distribution.
#' @param a The value up to which the probability is calculated and shaded.
#'
#' @return A list containing:
#'   \item{mu}{The mean of the distribution.}
#'   \item{sigma}{The standard deviation of the distribution.}
#'   \item{a}{The cutoff value for shading and probability calculation.}
#'   \item{probability}{The computed probability `P(X <= a)`.}
#'
#' @examples
#' myncurve(mu = 10, sigma = 5, a = 6)
#' @importFrom graphics curve polygon text legend
#' @importFrom stats dnorm pnorm
#' @export
myncurve <- function(mu, sigma, a) {
  # Define the x range for plotting
  x_vals <- seq(mu - 3*sigma, mu + 3*sigma, length = 1000)

  # Compute the density values
  y_vals <- dnorm(x_vals, mean = mu, sd = sigma)

  # Plot the normal curve
  plot(x_vals, y_vals, type = "l", col = "blue", lwd = 2,
       main = paste("Normal Curve: mu =", mu, "sigma =", sigma, "P(X <=", a, ")"),
       ylab = "Density", xlab = "X")

  # Shade the area from -Inf to a
  x_shade <- seq(mu - 3*sigma, a, length=500)
  y_shade <- dnorm(x_shade, mean=mu, sd=sigma)
  polygon(c(mu - 3*sigma, x_shade, a), c(0, y_shade, 0), col="red", border=NA)

  # Compute the probability P(X ≤ a)
  prob <- round(pnorm(a, mean=mu, sd=sigma), 4)

  # Display probability on plot
  legend("topright", legend = paste("P(X <=", a, ") =", prob),
         col = "black", cex = 0.8, bty = "n")

  # Return the results as a list
  return(list(mu=mu, sigma=sigma, a=a, probability=prob))
}
