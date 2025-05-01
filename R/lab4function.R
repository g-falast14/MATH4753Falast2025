#' A Custom Function for Piecewise Quadratic Regression
#'
#' This function evaluates a piecewise quadratic regression equation.
#'
#' @param x A numeric value or vector. Represents the independent variable(s).
#' @param coef A numeric vector of coefficients. Must contain three elements:
#'   - coef[1]: The intercept.
#'   - coef[2]: The linear term coefficient.
#'   - coef[3]: The quadratic term coefficient for (x - 18), applied only when (x > 18).
#' @return A numeric value or vector representing the result of the piecewise equation.
#' @examples
#' myf(20, c(2, 3, 4))
#' myf(c(15, 18, 20), c(2, 3, 4))
#' @export
myf <- function(x, coef) {
  coef[1] + coef[2] * (x) + coef[3] * (x - 18) * (x - 18 > 0)
}
