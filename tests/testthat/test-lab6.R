library(testthat)
library(MATH4753Falast2025)

test_that("myncurve function returns correct values", {
  result <- myncurve(10, 5, 6)

  expect_type(result, "list")  # Ensure the output is a list
  expect_true(all(c("mu", "sigma", "a", "probability") %in% names(result)))  # Check correct components

  expect_equal(result$mu, 10)
  expect_equal(result$sigma, 5)
  expect_equal(result$a, 6)
  expect_equal(result$probability, round(pnorm(6, mean=10, sd=5), 4))  # Verify probability calculation
})
