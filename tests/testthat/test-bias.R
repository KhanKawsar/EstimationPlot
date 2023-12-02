# Test bias correction


# Exact method bias correction as described by Hedges (fails for large DF)
HedgesBiasCorrectExact <- function(df) {
  gamma(df / 2) / (sqrt(df / 2) * gamma((df - 1) / 2))
}

# Some debugging stuff that isn't part of the test suite
# microbenchmark::microbenchmark(esBiasCorrectExact(10), esBiasCorrectApprox(10), times = 10000)

test_that("bias correction", {

  dfs <- seq(2, 100, length = 30)

  # Compare my exact method with that in package effectsize
  expect_equal(esBiasCorrectExact(dfs), HedgesBiasCorrectExact(dfs))

  # Compare exact method with Hedges' approximation
  for (df in dfs) {
    # Check approximation is with tolerance described by Hedges. I have slightly increased the tolerance values from those specified by Hedges
    tol <- ifelse(df >= 10, ifelse(df > 50, 1.5 * 10e-5, .000335), 0.0075)
    if (abs(esBiasCorrectApprox(df) - esBiasCorrectExact(df)) > tol) {
      message(sprintf("Bias approximation failed: df %d, approx %g, exact %g, diff %g, tolerance %g",
                      df, esBiasCorrectApprox(df), esBiasCorrectExact(df), esBiasCorrectApprox(df) - esBiasCorrectExact(df), tol))
      expect(FALSE, "Bias correction")
    }
  }

  # plot(dfs, esBiasCorrectApprox(dfs) - esBiasCorrectExact(dfs), log = "y", pch = 16, cex = 0.5,
  #      main = "Error in approx. method", xlab = "Degrees of freedom", ylab = "Error",
  #      sub = "With Hedges' stated accuracy")
  # #points(dfs, esBiasCorrectApprox(dfs) - HedgesBiasCorrectExact(dfs), pch = 3, col = 6)
  # abline(h = c(0.007, .00033, 1.5 * 10e-6), col = 2:4)
  # abline(v = c(2, 10, 50), col = 2:4)
})

test_that("bias limits", {

  # Bias correction for large sample sizes should be almost the same as no bias correction
  df <- data.frame(val = c(rnorm(500), rnorm(500, 0.2, sd = 0.5)),
                   group = rep(c("Group 1", "Group 2"), each = 500))
  dg <- DurgaDiff(df, 1, 2, effect.type = "hedges g")
  dd <- DurgaDiff(df, 1, 2, effect.type = "cohens d")
  # I don't know what the "correct" tolerance should be here
  expect_equal(dg$group.differences[[1]]$t1, dd$group.differences[[1]]$t1)

  df <- data.frame(val = c(rnorm(100), rnorm(100, 0.2)),
                   group = rep(c("Group 1", "Group 2"), each = 100))
  expect_error(DurgaDiff(df, 1, 2, effect.type = "hedges g"), NA)
})



# I do not have any way to test the degrees of freedom calculation described in
# Equation 16 in Delacre et al., 2021. This is not a test, but visually compares
# it to the calculation used for the same purpose in the effectsize package.
# Unfortunately, the source of the effectsize calculation is not identified
if (FALSE) {
  d <- replicate(100, {
    x1 <- rnorm(round(runif(1, 3, 60)))
    x2 <- rnorm(round(runif(1, 3, 60)))
    delacreDF <- esDFAsterisk(x1, x2)

    # The effectsize calculation
    s1 <- stats::sd(x1)
    s2 <- stats::sd(x2)
    n1 <- length(x1)
    n2 <- length(x2)
    se1 <- sqrt(s1^2 / n1)
    se2 <- sqrt(s2^2 / n2)
    se <- sqrt(se1^2 + se2^2)
    effectsizeDF <- se^4 / (se1^4 / (n1 - 1) + se2^4 / (n2 - 1))

    c(delacreDF, effectsizeDF)
  })
  plot(d[1, ], d[2, ])
}

