#' Central Limit Theorem Demonstration Function
#'
#' This function generates sample means from a uniform distribution
#' and visualizes the distribution of sample means using a histogram.
#' It also overlays a theoretical normal curve to demonstrate the Central Limit Theorem (CLT).
#'
#' @param n Integer. The sample size for each iteration (number of observations per sample).
#' @param iter Integer. The number of iterations (samples to draw).
#' @param a Numeric. The lower bound of the uniform distribution (default is 0).
#' @param b Numeric. The upper bound of the uniform distribution (default is 5).
#'
#' @return A numeric vector containing the sample means from each iteration.
#' The function also generates a histogram of the sample means with a normal curve overlay.
#'
#' @examples
#' # Generate 10,000 sample means with sample size 10 from U(0,5)
#' myclt(n = 10, iter = 10000)
#'
#' # Generate 5,000 sample means with sample size 30 from U(0,10)
#' myclt(n = 30, iter = 5000, a = 0, b = 10)
#'
#' @importFrom grDevices rainbow
#' @importFrom stats runif
#' @export
myclt <- function(n, iter, a = 0, b = 5) {
  x <- 0.5

  ## Generate random values from U(a, b)
  y <- runif(n * iter, a, b)

  ## Reshape into a matrix with n rows and iter columns
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)

  ## Compute the sample mean for each column (iteration)
  sm_means <- apply(data, 2, mean)

  ## Create histogram of sample means
  h <- hist(sm_means, plot = FALSE)  # Get histogram data without plotting first
  hist(sm_means, col = rainbow(length(h$mids)), freq = FALSE,
       main = "Distribution of Sample Means", xlab = "Sample Mean")

  ## Overlay normal curve with theoretical mean and standard deviation
  curve(dnorm(x, mean = (a + b) / 2, sd = sqrt((b - a)^2 / (12 * n))),
        add = TRUE, lwd = 2, col = "Blue", xname = "x")

  ## Return the vector of sample means
  return(sm_means)
}
