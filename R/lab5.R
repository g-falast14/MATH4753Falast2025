#' Simulates a Binomial Experiment
#'
#' This function simulates a binomial experiment by generating random samples
#' from a binomial distribution and visualizing the results.
#'
#' @param iter Number of iterations (simulations).
#' @param n Number of trials in each simulation.
#' @param p Probability of success in each trial.
#'
#' @return A table with the proportion of each number of successes.
#' @examples
#' mybin(1000, 10, 0.7)
#'
#' @export
mybin <- function(iter=100, n=10, p=0.5){
  # Create a matrix to store the samples
  sam.mat <- matrix(NA, nrow=n, ncol=iter, byrow=TRUE)

  # Vector to store number of successes
  succ <- numeric(iter)

  for(i in 1:iter){
    # Generate a sample of successes (1) and failures (0)
    sam.mat[,i] <- sample(c(1,0), n, replace=TRUE, prob=c(p,1-p))

    # Count the number of successes
    succ[i] <- sum(sam.mat[,i])
  }

  # Plot the histogram of successes
  hist(succ, breaks=seq(-0.5, n+0.5, by=1), col="blue",
       main=paste("Binomial Simulation: n=", n, ", p=", p, ", iter=", iter),
       xlab="Number of Successes", ylab="Frequency")

  # Return the proportion of each number of successes
  return(table(succ) / iter)
}
