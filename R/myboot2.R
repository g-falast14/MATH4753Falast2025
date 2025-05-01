#' Bootstrap Confidence Interval Function
#'
#' Computes a bootstrap confidence interval for a statistic computed on the input vector.
#' The function resamples the data with replacement, applies the specified statistic,
#' and plots a histogram of the bootstrap sample statistics with the confidence interval marked.
#'
#' @param iter Integer. The number of bootstrap iterations (default is 10000).
#' @param x Numeric vector. The data sample.
#' @param fun Function or character. The function used to compute the statistic on \code{x} (default is "mean").
#' @param alpha Numeric. The significance level used to compute the confidence interval (default is 0.05).
#' @param cx Numeric. The character expansion factor for text in the plot (default is 1.5).
#' @param ... Additional arguments passed to the histogram plotting function.
#'
#' @return A list with the following components:
#' \item{ci}{A numeric vector of length 2 containing the lower and upper bounds of the confidence interval.}
#' \item{fun}{The statistic function used.}
#' \item{x}{The original data vector.}
#'
#' @details The function first generates \code{iter} bootstrap samples (each of size \code{n}, the length of \code{x})
#' by sampling with replacement from \code{x}. It then computes the statistic for each bootstrap sample using
#' \code{fun} and calculates the confidence interval as the \code{alpha/2} and \code{1 - alpha/2} quantiles of the
#' bootstrap distribution. A histogram of the bootstrap sample statistics is plotted with a vertical line for the point
#' estimate and a horizontal segment indicating the confidence interval.
#'
#' @examples
#' \dontrun{
#'   # Bootstrap confidence interval for the mean of a random sample:
#'   set.seed(123)
#'   sample_data <- rnorm(30, mean = 10, sd = 2)
#'   result <- myboot2(iter = 10000, x = sample_data, fun = "mean", alpha = 0.05)
#'   print(result$ci)
#' }
#'
#' @export
myboot2 <- function(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...) {
  n <- length(x)   # sample size

  # Resample with replacement
  y <- sample(x, n * iter, replace = TRUE)

  # Construct a matrix with each column representing one bootstrap sample
  rs.mat <- matrix(y, nr = n, nc = iter, byrow = TRUE)

  # Compute the statistic for each bootstrap sample
  xstat <- apply(rs.mat, 2, fun)  # xstat is a vector with iter values

  # Calculate the confidence interval based on quantiles
  ci <- quantile(xstat, c(alpha/2, 1 - alpha/2))

  # Plot histogram of bootstrap sample statistics
  para <- hist(xstat, freq = FALSE, las = 1,
               main = paste("Histogram of Bootstrap sample statistics", "\n",
                            "alpha=", alpha, " iter=", iter, sep = ""),
               ...)

  # Compute point estimate from the original sample
  mat <- matrix(x, nr = length(x), nc = 1, byrow = TRUE)
  pte <- apply(mat, 2, fun)

  # Add the point estimate line and the confidence interval on the histogram
  abline(v = pte, lwd = 3, col = "Black")  # Vertical line for point estimate
  segments(ci[1], 0, ci[2], 0, lwd = 4)      # Draw horizontal segment for CI
  text(ci[1], 0, paste("(", round(ci[1], 2), sep = ""), col = "Red", cex = cx)
  text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""), col = "Red", cex = cx)

  # Label the point estimate at half the maximum density of the histogram
  text(pte, max(para$density)/2, round(pte, 2), cex = cx)

  # Return the confidence interval and other details invisibly
  invisible(list(ci = ci, fun = fun, x = x))
}
