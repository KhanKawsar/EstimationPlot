test_that("new effect types", {

  set.seed(1) # For consistent bootstrapping

  # Test using values from Lakens supplementary spreadsheet version 4.3, except
  # with SD values replaced with true values (not rounded to 2 decimal places)
  Group_1 <- c(9.00, 7.00, 8.00, 9.00, 8.00, 9.00, 9.00, 10.00, 9.00, 9.00)
  Group_2 <- c(9.00, 6.00, 7.00, 8.00, 7.00, 9.00, 8.00, 8.00, 8.00, 7.00)
  n1 <- length(Group_1)
  n2 <- length(Group_2)
  data <- data.frame(Value = c(Group_1, Group_2),
                     Group = c(rep("Group 1", n1), rep("Group 2", n2)),
                     Id = c(seq_len(n1), seq_len(n2)))

  DurgaEst <- function(effectType) DurgaDiff(data, "Value", "Group", groups = c("Group 2", "Group 1"), effect.type = effectType)$group.differences[[1]]$t0
  DurgaPairEst <- function(effectType) DurgaDiff(data, "Value", "Group", groups = c("Group 2", "Group 1"), id.col = "Id", effect.type = effectType)$group.differences[[1]]$t0

  # Reject previously accepted shorthand effect types "cohens" and "hedges", i.e. not backwards compatible
  expect_error(DurgaDiff(data, "Value", "Group", effect.type = "cohens"), "effect type")
  expect_error(DurgaDiff(data, "Value", "Group", effect.type = "hedges"), "effect type")

  # Independent samples, i.e. not paired
  expect_equal(DurgaEst("cohens d_s"), 1.125879938)  # Spreadsheet cell U19
  # Spreadsheet contains approximation correction, so check value is within Hedges stated tolerance
  expect_equal(DurgaEst("hedges g_s"), 1.078307546, tolerance = .00033)  # Spreadsheet cell U21

  # Correlated/dependent samples, i.e. paired
  expect_equal(DurgaPairEst("cohens d_z"), 1.5)          # Spreadsheet cell V10
  # NOTE Laken's spreadsheet v 4.3 uses the formula for Cohens d* but labels it d_av
  # expect_equal(DurgaPairEst("cohens d_av"), 1.125879938)  # Spreadsheet v1 cell V13
  expect_equal(DurgaPairEst("cohens d"), 1.125879938)  # Spreadsheet v1 cell V13
  # Spreadsheet says it's Hedges' g_av, but uses the formula for g*!!! Also uses approximate correction
  #expect_equal(DurgaPairEst("hedges g_av"), 1.029375943)  # Spreadsheet cell 14V
  expect_equal(DurgaPairEst("hedges g"), 1.029375943, tolerance = 0.0075)  # Spreadsheet cell 14V

  d_av <- mean(Group_1 - Group_2) / ((sd(Group_1) + sd(Group_2)) / 2)
  expect_equal(DurgaPairEst("cohens d_av"), d_av)
  expect_equal(DurgaPairEst("hedges g_av"), d_av * esBiasCorrectExact(9))

  # Note that DurgaEst swaps the groups
  expect_equal(DurgaEst("glass delta_pre"), (mean(Group_1) - mean(Group_2)) / sd(Group_1))
  expect_equal(DurgaEst("glass delta_post"), (mean(Group_1) - mean(Group_2)) / sd(Group_2))

  d_ast <- mean(Group_1 - Group_2) / sqrt((sd(Group_1)^2 + sd(Group_2)^2) / 2)
  expect_equal(DurgaPairEst("cohens d"), d_ast)
  expect_equal(DurgaPairEst("hedges g"), d_ast * esBiasCorrectExact(9)) # Not a very good test

  # Check case insensitivity
  expect_equal(DurgaEst("Cohens d"), DurgaEst("COHENS d"))
  expect_equal(DurgaPairEst("cOhEnS d"), DurgaPairEst("CoHeNs d"))
})

# Commented out so that MOTE is not a dependency of Durga
# test_that("calculations, MOTE::", {
### Also compare against MOTE
  # if (nzchar(system.file(package = "MOTE"))) {
  #   m1 <- mean(Group_1)
  #   m2 <- mean(Group_2)
  #   sd1 <- sd(Group_1)
  #   sd2 <- sd(Group_2)
  #   n1 <- length(Group_1)
  #   n2 <- length(Group_2)
  #   # Cohen's d_s, unpaired
  #   expect_equal(DurgaEst("cohens d_s"), MOTE::d.ind.t(m1, m2, sd1, sd2, n1, n2)$d)
  #   # Hedges' g_s, unpaired. MOTE uses the approximate method
  #   expect_equal(DurgaEst("hedges g_s"), MOTE::g.ind.t(m1, m2, sd1, sd2, n1, n2)$d, tolerance = 0.0075)
  #   # Cohen's d_av, paired
  #   expect_equal(DurgaPairEst("cohens d_av"), MOTE::d.dep.t.avg(m1, m2, sd1, sd2, n1)$d)
  # }
# })

# Commented out so that effectsize is not a dependency of Durga
# Compare to results from another package
# test_that("calculations, effectsize::", {
#   # Only run tests if the package is installed
#   if (!nzchar(system.file(package = "effectsize"))) {
#     expect_true(TRUE) # Just to avoid a warning about no tests
#     return()
#   }
#
#   # Unpaired
#   set.seed(1)
#   x1 <- rnorm(10)
#   x2 <- rnorm(15, mean = 0.7)
#   df <- data.frame(Val = c(x1, x2), Grp = c(rep("1", length(x1)), rep("2", length(x2))))
#   DurgaEst <- function(effectType) DurgaDiff(df, 1, 2, effect.type = effectType)$group.differences[[1]]$t0
#
#   expect_equal(DurgaEst("cohens d_s"), effectsize::cohens_d(x2, x1)$Cohens_d)
#   expect_equal(DurgaEst("cohens d"), effectsize::cohens_d(x2, x1, pooled_sd = FALSE)$Cohens_d)
#   expect_equal(DurgaEst("hedges g_s"), effectsize::hedges_g(x2, x1)$Hedges_g)
#   # effectsize uses a different degrees of freedom calculation for bias correction, with no attribution
#   expect_equal(DurgaEst("hedges g"), effectsize::hedges_g(x2, x1, pooled_sd = FALSE)$Hedges_g, tolerance = 0.0001)
#   expect_equal(DurgaEst("glass delta_post"), effectsize::glass_delta(x2, x1)$Glass_delta)
#   expect_equal(DurgaEst("glass delta_pre"), -effectsize::glass_delta(x1, x2)$Glass_delta)
#
#   # Paired
#   set.seed(1)
#   x1 <- rnorm(12)
#   x2 <- rnorm(12, mean = 0.7)
#   id <- seq_along(x1)
#   df <- data.frame(Val = c(x1, x2), Grp = c(rep("1", length(x1)), rep("2", length(x2))), Id = c(id, id))
#   DurgaPairEst <- function(effectType) DurgaDiff(df, 1, 2, id.col = 3, effect.type = effectType)$group.differences[[1]]$t0
#
#   expect_equal(DurgaPairEst("cohens d_z"), effectsize::cohens_d(x2, x1, paired = TRUE)$Cohens_d)
#   expect_equal(DurgaEst("cohens d"), effectsize::cohens_d(x2, x1, pooled_sd = FALSE)$Cohens_d)
#   expect_equal(DurgaEst("hedges g_s"), effectsize::hedges_g(x2, x1)$Hedges_g)
#   expect_equal(DurgaEst("hedges g"), effectsize::hedges_g(x2, x1, pooled_sd = FALSE)$Hedges_g)
# })

test_that("calculations, deffectsize::", {
  # Only run tests if the package is installed
  if (!nzchar(system.file(package = "deffectsize"))) {
    expect_true(TRUE) # Just to avoid a warning about no tests
    return()
  }

  # Unpaired/independent/between subjects
  set.seed(2)
  x1 <- rnorm(10)
  x2 <- rnorm(15, mean = 0.7)
  df <- data.frame(Val = c(x1, x2), Grp = c(rep("1", length(x1)), rep("2", length(x2))))
  d <- DurgaDiff(df, 1, 2, effect.type = "mean")
  expect_equal(d$group.differences[[1]]$t0, deffectsize::datameandiff_CI(x2, x1, var.equal = TRUE, conf.level = 0.95, alternative = "two.sided", na.rm = TRUE)$Meandiff)
  d <- DurgaDiff(df, 1, 2, effect.type = "cohens d_s")
  expect_equal(d$group.differences[[1]]$t0, deffectsize::datacohen_CI(x2, x1, var.equal = TRUE, unbiased = FALSE, conf.level = 0.95, alternative = "two.sided", na.rm = TRUE)$ES)
  d <- DurgaDiff(df, 1, 2, effect.type = "hedges g_s")
  expect_equal(d$group.differences[[1]]$t0, deffectsize::datacohen_CI(x2, x1, var.equal = TRUE, unbiased = TRUE, conf.level = 0.95, alternative = "two.sided", na.rm = TRUE)$ES)
  d <- DurgaDiff(df, 1, 2, effect.type = "cohens d")
  expect_equal(d$group.differences[[1]]$t0, deffectsize::datacohen_CI(x2, x1, var.equal = FALSE, unbiased = FALSE, conf.level = 0.95, alternative = "two.sided", na.rm = TRUE)$ES)
  d <- DurgaDiff(df, 1, 2, effect.type = "hedges g")
  expect_equal(d$group.differences[[1]]$t0, deffectsize::datacohen_CI(x2, x1, var.equal = FALSE, unbiased = TRUE, conf.level = 0.95, alternative = "two.sided", na.rm = TRUE)$ES)
})

