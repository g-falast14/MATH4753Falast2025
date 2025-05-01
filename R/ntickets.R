#' Calculate Optimal Number of Tickets to Sell without Overbooking
#'
#' This function helps calculate the optimal number of tickets to sell for a flight without overbooking
#' seats, given the number of seats `N`, probability `p` of a customer showing up, and a maximum
#' allowed overbooking probability `gamma`. Two plost are also generated for the discrete case, and continuous case.
#'
#' @param N Integer. Number of seats on the plane.
#' @param gamma Numeric between 0 and 1. Maximum acceptable probability of overbooking.
#' @param p Numeric between 0 and 1. Probability that a ticketed passenger shows up.
#'
#' @return A named list containing:
#' \describe{
#'   \item{nd}{Optimal number of tickets}
#'   \item{nc}{Optimal number of tickets}
#'   \item{N}{Number of seats}
#'   \item{p}{Probability of people showing up}
#'   \item{gamma}{Input overbooking threshold}
#' }
#'
#' @examples
#' ntickets(N = 200, gamma = 0.02, p = 0.95)
#'
#' @importFrom stats pbinom pnorm optimise
#' @importFrom graphics abline plot points
#' @export
ntickets <- function(N, gamma, p) {

  # discrete calculation using binomial distribution
  objective_discrete <- function(n) {
    prob <- pbinom(N, n, p) # rprobability <= n people show up for n sold tickets
    abs(1 - gamma - prob)
  }

  # try values from N to N+30 tickets
  n_vals <- N:(N + 30)
  obj_vals_discrete <- sapply(n_vals, objective_discrete)
  nd <- n_vals[which.min(obj_vals_discrete)]  # optimal n (discrete)

  # continuous calculation using normal approximation
  objective_normal <- function(n) {
    mean <- n * p
    sd <- sqrt(n * p * (1 - p))
    prob <- pnorm(N + 0.5, mean, sd)  # continuity correction
    abs(1 - gamma - prob)
  }

  # find best n to minimize objective
  nc <- optimise(objective_normal, interval = c(N, N + 30))$minimum

  # output
  result <- list(
    nd = nd,
    nc = nc,
    N = N,
    p = p,
    gamma = gamma
  )

  # print output
  print(result)

  # discrete plot
  plot(n_vals, obj_vals_discrete, type = "b", col = "blue", pch = 16,
       ylab = "Objective", xlab = "n",
       main = paste("Objective Vs n to find optimal tickets sold\n(",
                    nd, ") gamma=", gamma, " N=", N, " discrete", sep=""))
  abline(v = nd, col = "red", lwd = 2)
  abline(h = 0, col = "red", lwd = 2)

  # continuous plot
  n_seq <- seq(N, N + 30, by = 0.1)
  obj_vals_cont <- sapply(n_seq, objective_normal)

  plot(n_seq, obj_vals_cont, type = "l", lwd = 2,
       ylab = "Objective", xlab = "n",
       main = paste("Objective Vs n to find optimal tickets sold\n(",
                    round(nc, 4), ") gamma=", gamma, " N=", N, " continuous", sep=""))
  abline(v = nc, col = "blue", lwd = 2)
  abline(h = 0, col = "blue", lwd = 2)
}
