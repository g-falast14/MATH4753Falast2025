#' Fire Damage Dataset
#'
#' A dataset used to analyze the relationship between distance from a fire station and fire damage amount.
#'
#' @format A data frame with `n` rows and 2 variables:
#' \describe{
#'   \item{firedam.dist}{numeric: Distance from the fire station (in miles)}
#'   \item{firedam.damage}{numeric: Amount of fire damage (in thousands of dollars)}
#' }
#'
#' @details
#' This dataset was used in simple linear regression modeling to investigate whether proximity to a fire station is associated with lower property damage due to fires.
#'
#' @source Originally adapted for instructional use in MATH 4753 at the University of Oklahoma. Based on examples found in: Sincich, T., & Mendenhall, W. *Statistics for Engineering and the Sciences*. 6th Ed.
#'
#' @examples
#' data(firedam)
#' plot(firedam$firedam.dist, firedam$firedam.damage,
#'      main = "Fire Damage by Distance from Station",
#'      xlab = "Distance (miles)", ylab = "Damage ($000s)")
#' abline(lm(firedam.damage ~ firedam.dist, data = firedam), col = "red")
"firedam"
