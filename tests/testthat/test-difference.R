#### TODO test different values of ci.type

# To report test coverage, run
# devtools::test_coverage()

makeData <- function(N = 40) {
  set.seed(1)
  data <- data.frame(Measurement = c(rnorm(N, mean = 100, sd = 25),
                                     rnorm(N, mean = 100, sd = 50),
                                     rnorm(N, mean = 120, sd = 25),
                                     rnorm(N, mean = 80, sd = 50),
                                     rnorm(N, mean = 100, sd = 12),
                                     rnorm(N, mean = 100, sd = 50)),
                     Group = c(rep("ZControl1", N),
                               rep("Control2", N),
                               rep("Group1", N),
                               rep("Group2", N),
                               rep("Group3", N),
                               rep("Group4", N)),
                     Gender = rep(c(rep('Male', N/2), rep('Female', N/2)), 6),
                     ID = rep(1:N, 6)
  )
  # Shuffle
  data[sample(nrow(data)), ]
}

makeES1 <- function() {
  set.seed(1)
  data <- makeData()
  SAKDifference(data[data$Group %in% c("ZControl1", "Group1"),], data.col = "Measurement", group.col = "Group", R = 1000)
}

makePairedData <- function(addSomeNAs = FALSE, reverseGroups = FALSE) {
  N <- 40
  set.seed(1)
  data <- data.frame(Measurement = c(rnorm(N, mean = 100, sd = 25),
                                     rnorm(N, mean = 100, sd = 50),
                                     rnorm(N, mean = 120, sd = 25),
                                     rnorm(N, mean = 80, sd = 50),
                                     rnorm(N, mean = 100, sd = 12),
                                     rnorm(N, mean = 100, sd = 50)),
                     Group = c(rep("ZControl1", N),
                               rep("Control2", N),
                               rep("Group1", N),
                               rep("Group2", N),
                               rep("Group3", N),
                               rep("Group4", N)),
                     Sex = rep(c(rep('Male', N/2), rep('Female', N/2)), 6),
                     ID = rep(1:N, 6)
  )
  if (addSomeNAs)
    data[c(1, 4, 10, 65), 1] <- NA

  # Shuffle
  data <- data[sample(nrow(data)), ]
  if (reverseGroups) {
    SAKDifference(data[data$Group %in% c("ZControl1", "Group1"),],
               groups = c("ZControl1", "Group1"),
               na.rm = addSomeNAs,
               data.col = "Measurement", group.col = "Group", R = 1000,
               id.col = "ID")
  } else {
    SAKDifference(data[data$Group %in% c("ZControl1", "Group1"),],
               na.rm = addSomeNAs,
               data.col = "Measurement", group.col = "Group", R = 1000,
               id.col = "ID")
  }
}

compareDiffs <- function(d1, d2) {
  expect_equal(names(d1), names(d2))
  expect_equal(d1$t0, d2$t0)
  expect_equal(d1$R, d2$R)
  expect_equal(d1$bca, d2$bca)
}

##########################################################################
# Tests start here ####

test_that("contrast plots", {
  data <- makeData()
  groups <- c("ZControl1", "Group1", "Group2", "Group3")
  #data <- data[data$Group %in% groups, ]

  # Default contrasts
  d <- SAKDifference(data, "Measurement", "Group", groups = groups)
  ng <- length(d$groups)
  expect_equal(length(d$group.differences), ng - 1)
  SAKPlot(d, main = "Default contrasts")

  # 2 contrasts
  contrasts <- "Group1 - ZControl1, Group2 - ZControl1, Group3 - ZControl1"
  d <- SAKDifference(data, "Measurement", "Group", groups = groups, contrasts = contrasts)
  expect_equal(length(d$group.differences), 3)
  expect_equal(d$group.differences[[1]]$groups[1], "Group1")
  expect_equal(d$group.differences[[1]]$groups[2], "ZControl1")
  expect_equal(d$group.differences[[2]]$groups[1], "Group2")
  expect_equal(d$group.differences[[2]]$groups[2], "ZControl1")
  expect_equal(d$group.differences[[3]]$groups[1], "Group3")
  expect_equal(d$group.differences[[3]]$groups[2], "ZControl1")
  SAKPlot(d, main = "Explicit contrasts")
})

test_that("plots", {
  d <- makeES1()
  expected <- paste0("Bootstrapped effect size\\n",
                     "  Measurement ~ Group\\n",
                     "Groups:\\n",
                     "              mean   median       sd       se  CI\\.lower CI\\.upper  N\\n",
                     "Group1    122\\.9210 118\\.8367 21\\.31850 3\\.370750 116\\.10300 129\\.7390 40\\n",
                     "ZControl1 102\\.3007 103\\.2276 22\\.16668 3\\.504859  95\\.21141 109\\.3899 40\\n",
                     "Pairwise unstandardised effect size:\\n",
                     "  ZControl1 - Group1: -20\\.[0-9]+, 95% CI \\(bca\\) \\[-30\\.[0-9]+, -10\\.[0-9]+\\]")
  expect_output(print(d), expected)
})

test_that("contrasts", {
  data <- makeData()

  # Default contrasts
  d <- SAKDifference(data, "Measurement", "Group")
  ng <- length(d$groups)
  expect_equal(length(d$group.differences), ng - 1)

  # All in 1 string
  contrasts = "Group1 - ZControl1, Group2 - ZControl1, Group3 - ZControl1, Group4 - ZControl1"
  d <- SAKDifference(data, "Measurement", "Group", contrasts = contrasts)
  expect_equal(length(d$group.differences), 4)
  expect_equal(d$group.differences[[1]]$groups[1], "Group1")
  expect_equal(d$group.differences[[1]]$groups[2], "ZControl1")
  expect_equal(d$group.differences[[2]]$groups[1], "Group2")
  expect_equal(d$group.differences[[2]]$groups[2], "ZControl1")
  expect_equal(d$group.differences[[3]]$groups[1], "Group3")
  expect_equal(d$group.differences[[3]]$groups[2], "ZControl1")
  expect_equal(d$group.differences[[4]]$groups[1], "Group4")
  expect_equal(d$group.differences[[4]]$groups[2], "ZControl1")

  contrasts = c("Group1 - ZControl1", "Group2 - ZControl1", "Group3 - ZControl1", "Group4 - ZControl1")
  d2 <- SAKDifference(data, "Measurement", "Group", contrasts = contrasts)
  expect_equal(length(d2$group.differences), 4)
  expect_equal(d2$group.differences[[1]]$groups[1], "Group1")
  expect_equal(d2$group.differences[[1]]$groups[2], "ZControl1")
  compareDiffs(d2, d)

  contrasts <- matrix(c("Group1", "ZControl1", "Group2", "ZControl1", "Group3", "ZControl1", "Group4", "ZControl1"), nrow = 2)
  d2 <- SAKDifference(data, "Measurement", "Group", contrasts = contrasts)
  expect_equal(length(d2$group.differences), 4)
  expect_equal(d2$group.differences[[1]]$groups[1], "Group1")
  expect_equal(d2$group.differences[[1]]$groups[2], "ZControl1")
  compareDiffs(d2, d)

  expect_error(SAKDifference(data, "Measurement", "Group", contrasts = "Group2:ZControl"))
  expect_error(SAKDifference(data, "Measurement", "Group", contrasts = "ZControl"))
  expect_error(SAKDifference(data, "Measurement", "Group", contrasts = ""))
  expect_error(SAKDifference(data, "Measurement", "Group", contrasts = "Group 2 - Group 1"))

  # Wildcard contrasts
  d <- SAKDifference(data, "Measurement", "Group", effect.type = "cohen", contrasts = "*")
  ng <- length(unique(data$Group))
  expect_equal(length(d$group.differences), ng * (ng - 1) / 2)
})

test_that("difference effect types", {
  n <- 100
  set.seed(1)
  realDiff <- 1
  df <- data.frame(val = c(rnorm(n, mean = 10), rnorm(n, mean = 10 + realDiff)),
                   group = c(rep("Control", n), rep("Group", n)),
                   id = c(1:n, 1:n))

  # Check all effect types
  expect_error(SAKDifference(df, effect.type = "wrong", data.col = 1, group.col = 2)) # Should throw an error
  expect_error(SAKDifference(df, effect.type = "unstandardised", data.col = 1, group.col = 2), NA)
  expect_error(SAKDifference(df, effect.type = "cohens", data.col = 1, group.col = 2), NA)
  expect_error(SAKDifference(df, effect.type = "hedges", data.col = 1, group.col = 2), NA)
  expect_error(SAKDifference(df, effect.type = "unstandardised", id.col = "id", data.col = 1, group.col = 2), NA)
  expect_error(SAKDifference(df, effect.type = "cohens", id.col = "id", data.col = 1, group.col = 2), NA)
  expect_error(SAKDifference(df, effect.type = "hedges", id.col = "id", data.col = 1, group.col = 2), NA)

  # Check unstandardised diff
  d <- SAKDifference(df, data.col = 1, group.col = 2, effect.type = "unstandardised")
  pwd <- d$group.difference[[1]]
  expect_equal(pwd$groups[1], "Group")
  expect_equal(pwd$groups[2], "Control")
  expect_lt(pwd$t0, realDiff)
  expect_lt(pwd$bca[4], realDiff)
  expect_gt(pwd$bca[5], realDiff)

  # Check Cohen's D
  d <- SAKDifference(df, data.col = 1, group.col = 2, effect.type = "cohens")
  pwd <- d$group.difference[[1]]
  expect_equal(pwd$groups[1], "Group")
  expect_equal(pwd$groups[2], "Control")
  expect_equal(pwd$t0, 0.918991, tolerance = 0.0001) # Cohen's d
  expect_lt(pwd$bca[4], 0.918991) # Should be positive but small
  expect_gt(pwd$bca[5], 0.918991)
  # Save Cohen's d for later
  cohensD <- pwd$t0
  # Swap groups
  d <- SAKDifference(df, groups = c("Group", "Control"), data.col = 1, group.col = 2, effect.type = "cohens")
  pwd <- d$group.difference[[1]]
  expect_equal(pwd$groups[1], "Control")
  expect_equal(pwd$groups[2], "Group")
  expect_equal(pwd$t0, -0.918991, tolerance = 0.0001) # Cohen's d
  expect_lt(pwd$bca[4], -0.918991) # Should be negative but small
  expect_gt(pwd$bca[5], -0.918991)

  # Check Hedge's g
  d <- SAKDifference(df, data.col = 1, group.col = 2, effect.type = "hedges")
  pwd <- d$group.difference[[1]]
  expect_equal(pwd$groups[1], "Group")
  expect_equal(pwd$groups[2], "Control")
  # Estimate Hedges' g by correcting Cohen's d (Lakens 2013, p. 3 eqn 4)
  n1 <- d$group.statistics[2,"N"]
  n2 <- d$group.statistics[1,"N"]
  hedgesG <- cohensD * (1 - 3 / (4 * (n1 + n2) - 9))
  expect_equal(pwd$t0, hedgesG, tolerance = 0.0001) # Hedge's g
  expect_lt(pwd$bca[4], 0.918991) # Should be positive but small
  expect_gt(pwd$bca[5], 0.918991)
  # Swap groups
  d <- SAKDifference(df, groups = c("Group", "Control"), data.col = 1, group.col = 2, effect.type = "hedges")
  pwd <- d$group.difference[[1]]
  expect_equal(pwd$groups[1], "Control")
  expect_equal(pwd$groups[2], "Group")
  expect_equal(pwd$t0, -hedgesG, tolerance = 0.0001) # Hedge's g
  expect_lt(pwd$bca[4], -hedgesG) # Should be negative but small
  expect_gt(pwd$bca[5], -hedgesG)

  ### Paired effect sizes ###
  # Check unstandardised diff
  d <- SAKDifference(df, data.col = 1, group.col = 2, id.col = 3, effect.type = "unstandardised")
  pwd <- d$group.difference[[1]]
  expect_equal(pwd$groups[1], "Group")
  expect_equal(pwd$groups[2], "Control")
  expect_lt(pwd$t0, realDiff)
  expect_lt(pwd$bca[4], realDiff)
  expect_gt(pwd$bca[5], realDiff)

  # Check Cohen's D
  d <- SAKDifference(df, data.col = 1, group.col = 2, id.col = 3, effect.type = "cohens")
  pwd <- d$group.difference[[1]]
  expect_equal(pwd$groups[1], "Group")
  expect_equal(pwd$groups[2], "Control")
  diffs <- df$val[df$group == "Group"] - df$val[df$group == "Control"]
  cohensD <- mean(diffs) / sd(diffs)
  expect_equal(pwd$t0, cohensD)
  expect_lt(pwd$bca[4], cohensD) # Should be positive but small
  expect_gt(pwd$bca[5], cohensD)

  d <- SAKDifference(df, data.col = 1, group.col = 2, id.col = 3, effect.type = "hedges")
  pwd <- d$group.difference[[1]]
  expect_equal(pwd$groups[1], "Group")
  expect_equal(pwd$groups[2], "Control")
  # Estimate Hedges' g by correcting Cohen's d (Lakens 2013, p. 3 eqn 4)
  hedgesG <- cohensD * (1 - 3 / (4 * n - 9))
  expect_equal(pwd$t0, hedgesG)
  expect_lt(pwd$bca[4], hedgesG) # Should be positive but small
  expect_gt(pwd$bca[5], hedgesG)
})

test_that("group factors", {
  n <- 100
  df <- data.frame(val = c(rnorm(n), rnorm(n, mean = 1)),
                   group = factor(c(rep("Control", n), rep("Group", n))),
                   id = c(1:n, 1:n))

  # Check all effect types
  expect_error(SAKDifference(df, effect.type = "unstandardised", data.col = 1, group.col = 2), NA)
  expect_error(SAKDifference(df, effect.type = "cohens", data.col = 1, group.col = 2), NA)
  expect_error(SAKDifference(df, effect.type = "hedges", data.col = 1, group.col = 2), NA)
  expect_error(SAKDifference(df, id.col = "id", data.col = 1, group.col = 2), NA)

})

test_that("difference handles NA", {
  n <- 100
  df <- data.frame(val = c(rnorm(n), rnorm(n, mean = 1)),
                   group = c(rep("Control", n), rep("Group", n)))
  df[c(1, 4, 10, 65), 1] <- NA

  # This should throw an error
  expect_error(SAKDifference(df, na.rm = FALSE, data.col = 1, group.col = 2))
  # This should NOT throw an error
  expect_error(SAKDifference(df, na.rm = TRUE, data.col = 1, group.col = 2), NA)
  # This should throw an error if one of a pair is missing
  expect_error(SAKDifference(df, effect.type = "paired", id.col = "id", na.rm = TRUE, group.col = 2))
})

test_that("two groups", {
  N <- 40
  df <- data.frame(Measurement = c(rnorm(N, mean = 100, sd = 25),
                                   rnorm(N, mean = 120, sd = 25)),
                   Group = c(rep("Control", N),
                             rep("Treatment", N)),
                   Gender = rep(c(rep('Male', N/2), rep('Female', N/2)), 2),
                   ID = rep(1:N, 2)
  )

  d2 <- SAKDifference(df, data.col = 1, group.col = 2)
  # This should NOT throw an error
  op <- par(mar = c(5, 4, 4, 4) + 0.1)
  expect_error(SAKPlot(d2, ef.size = TRUE, main = "Two groups, effect size default"), NA)
  expect_error(SAKPlot(d2, ef.size.pos = "right", main = "Two groups, effect size right"), NA)
  par(op)
  expect_error(SAKPlot(d2, ef.size = FALSE, main = "Two groups, no effect size"), NA)
  expect_error(SAKPlot(d2, ef.size.position = "below", main = "Two groups, effect size below"), NA)
})

test_that("three groups", {
  N <- 40
  df <- data.frame(Measurement = c(rnorm(N, mean = 100, sd = 25),
                                   rnorm(N, mean = 120, sd = 25),
                                   rnorm(N, mean = 80, sd = 50)),
                   Group = c(rep("ZControl1", N),
                             rep("Group1", N),
                             rep("Group2", N)),
                   Gender = rep(c(rep('Male', N/2), rep('Female', N/2)), 3),
                   ID = rep(1:N, 3)
  )

  d3 <- SAKDifference(df, data.col = 1, group.col = 2)
  # This should NOT throw an error
  expect_error(SAKPlot(d3, bar = FALSE, box = FALSE, main = "Three groups"), NA)
  expect_error(SAKPlot(d3, violin.trunc = FALSE, main = "No violin truncation"), NA)
  expect_error(SAKPlot(d3, violin.trunc = 0.05, main = "0.05 violin truncation"), NA)
  expect_error(SAKPlot(d3, ef.size.violin = FALSE, main = "No effect size violin"), NA)
})

test_that("three groups with factor", {
  N <- 40
  # Use factor to control group order
  df <- data.frame(Measurement = c(rnorm(N, mean = 100, sd = 25),
                                             rnorm(N, mean = 120, sd = 25),
                                             rnorm(N, mean = 80, sd = 50)),
                             Group = factor(c(rep("ZControl1", N),
                                       rep("Group1", N),
                                       rep("Group2", N)),
                                       levels = c("ZControl1", "Group1", "Group2")),
                             Gender = rep(c(rep('Male', N/2), rep('Female', N/2)), 3),
                             ID = rep(1:N, 3)
  )

  d3 <- SAKDifference(df, data.col = 1, group.col = 2)
  # This should NOT throw an error
  expect_error(SAKPlot(d3, bar = FALSE, box = FALSE, main = "Group factor"), NA)
})

test_that("many groups", {
  n <- 12
  groupMean <- round(rnorm(n, mean = 20, sd = 8))
  val <- c(sapply(groupMean, function(m) rnorm(n, m, 4)))
  trt <- c(sapply(seq_along(groupMean), function(i) rep(paste0("G", i, "-", groupMean[i]), n)))
  df <- data.frame(Height = val, Treatment = trt)
  d <- SAKDifference(df, "Height", "Treatment")
  expect_error(SAKPlot(d, main = "Many groups"), NA)
})

test_that("plots work", {
  #### This fails with "figure margins too large"
  if (FALSE) {
    N <- 40
    set.seed(1)
    data <- data.frame(Measurement = c(rnorm(N, mean = 100, sd = 25),
                                       rnorm(N, mean = 100, sd = 50),
                                       rnorm(N, mean = 120, sd = 25),
                                       rnorm(N, mean = 80, sd = 50),
                                       rnorm(N, mean = 100, sd = 12),
                                       rnorm(N, mean = 100, sd = 50)),
                       Group = c(rep("ZControl1", N),
                                 rep("Control2", N),
                                 rep("Group1", N),
                                 rep("Group2", N),
                                 rep("Group3", N),
                                 rep("Group4", N)),
                       Gender = rep(c(rep('Male', N/2), rep('Female', N/2)), 6),
                       ID = rep(1:N, 6)
    )
    # Shuffle
    data <- data[sample(nrow(data)), ]
    es <- SAKDifference(data[data$Group %in% c("ZControl1", "Group1"),], data.col = "Measurement", group.col = "Group", R = 1000)
    es2 <- SAKDifference(data[data$Group %in% c("ZControl1", "Group1"),],
                         data.col = "Measurement", group.col = "Group", R = 1000,
                         id.col = "ID")


    # Generate some plots
    l <- layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), nrow = 4, ncol = 5, byrow =  TRUE))
    #layout.show(l)

    #par(mfrow = c(2, 4))
    #a)
    SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = FALSE, box.fill = FALSE,
            central.tendency = "median", error.bars.type = "CI", ef.size = FALSE,
            points = SAKTransparent(c("red", "blue"), .5))

    #b)
    SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "white", box.fill = "white",
            central.tendency = "median", error.bars = "SD", ef.size = FALSE,
            points = SAKTransparent(c("red", "blue"), .5),)

    #c)
    SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "white", box.fill = "white",
            central.tendency = "median", error.bars = "SE", ef.size = FALSE,
            points = SAKTransparent(c("red", "blue"), .5),)

    #d)
    SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "white", box.fill = "white",
            central.tendency.type = "mean", error.bars.type = "CI", ef.size = FALSE,
            points = SAKTransparent(c("red", "blue"), .5),)

    #e)
    SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "white", box.fill = "white",
            central.tendency.type = "mean", error.bars = "SD", ef.size = FALSE,
            points = SAKTransparent(c("red", "blue"), .5),)

    #f)
    SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "white", box.fill = "white",
            central.tendency.type = "mean", error.bars = "SE", ef.size = FALSE,
            points = SAKTransparent(c("red", "blue"), .5),)

    #g)
    SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = SAKTransparent(c("red", "blue"), .5),
            box.fill = "white", error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = FALSE)

    #h)
    SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = SAKTransparent(c("red", "blue"), .5),
            box.fill = "white", error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = SAKTransparent(c("red", "blue"), .5))

    #i)
    SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = SAKTransparent(c("red", "blue"), .5),
            box.fill = SAKTransparent(c("red", "blue"), .7), error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = FALSE)

    #j)
    SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = SAKTransparent(c("red", "blue"), .5),
            box.fill = SAKTransparent(c("red", "blue"), .7), error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = SAKTransparent(c("red", "blue"), .5))


    #k)
    SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin.shape = "left-half",
            violin = SAKTransparent(c("red", "blue"), .6),
            violin.fill = SAKTransparent(c("red", "blue"), .6),
            box = "white",
            box.fill = "white",
            error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = SAKTransparent(c("red", "blue"), .5))

    #l)
    SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin.shape = "left-half",
            violin = SAKTransparent(c("red", "blue"), .6),
            violin.fill = SAKTransparent(c("red", "blue"), .6),
            box = SAKTransparent(c("red", "blue"), .5),
            box.fill = SAKTransparent(c("red", "blue"), .7), error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = FALSE)

    ##m)
    SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin.shape = "left-half",
            violin = SAKTransparent(c("red", "blue"), .6),
            violin.fill = SAKTransparent(c("red", "blue"), .6),
            box = SAKTransparent(c("red", "blue"), .5),
            box.fill = SAKTransparent(c("red", "blue"), .7), error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = SAKTransparent(c("red", "blue"), .5))

    #n)
    SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin.shape = "right-half",
            violin = SAKTransparent(c("red", "blue"), .6),
            violin.fill = SAKTransparent(c("red", "blue"), .6),
            box = "white",
            box.fill = "white",
            error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = SAKTransparent(c("red", "blue"), .5))
    #o)
    SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin.shape = "right-half",
            violin = SAKTransparent(c("red", "blue"), .6),
            violin.fill = SAKTransparent(c("red", "blue"), .6),
            box = SAKTransparent(c("red", "blue"), .5),
            box.fill = SAKTransparent(c("red", "blue"), .7), error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = FALSE)

    #p)
    SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin.shape = "right-half",
            violin = SAKTransparent(c("red", "blue"), .6),
            violin.fill = SAKTransparent(c("red", "blue"), .6),
            box = SAKTransparent(c("red", "blue"), .5),
            box.fill = SAKTransparent(c("red", "blue"), .7), error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = SAKTransparent(c("red", "blue"), .5))

    #q)
    SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin.shape = "full",
            violin = SAKTransparent(c("red", "blue"), .6),
            violin.fill = SAKTransparent(c("red", "blue"), .6),
            box = SAKTransparent(c("red", "blue"), .5),
            box.fill = SAKTransparent(c("red", "blue"), .7), error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = FALSE)

    #r)
    SAKPlot(es2, bar = FALSE, bar.fill = FALSE, violin.shape = "full",
            violin = SAKTransparent(c("red", "blue"), .4),
            violin.fill = SAKTransparent(c("red", "blue"), .8),
            box = SAKTransparent(c("grey10"), .1),
            box.fill = SAKTransparent(c("red", "blue"), .7), error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = TRUE,
            points = SAKTransparent(c("red", "blue"), .5), paired = TRUE)

    #s)
    SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin.shape = "full",
            violin = SAKTransparent(c("red", "blue"), .6),
            violin.fill = SAKTransparent(c("red", "blue"), .6),
            box = "white",
            box.fill = "white",
            error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = SAKTransparent(c("red", "blue"), .5))

    #t)
    SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin.shape = "full",
            violin = SAKTransparent(c("red", "blue"), .6),
            violin.fill = SAKTransparent(c("red", "blue"), .6),
            box = "white",
            box.fill = "white",
            error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = FALSE)

  }
  expect_equal(1, 1)
})

test_that("box FALSE works", {
  es <- makeES1()
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = FALSE, box.fill = FALSE,
          central.tendency.type = "median", error.bars.type = "CI", ef.size = FALSE,
          points = SAKTransparent(c("red", "blue"), .5), main = "Violin FALSE, median, no effect size")
  SAKPlot(es, violin = FALSE, central.tendency = FALSE, error.bars = FALSE, ef.size = FALSE,
          main = "No central tendency, error bar, effect size")
  expect_equal(1, 1)
})

test_that("central tendency FALSE works", {
  es <- makeES1()
  expect_false(es$paired.data)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "red", box.fill = "blue",
         central.tendency = FALSE, error.bars.type = "CI", ef.size = FALSE,
         points = SAKTransparent(c("red", "blue"), .5), main = "Central tendency FALSE")
  expect_equal(1, 1)
})

test_that("paired works", {
  es <- makePairedData()
  expect_true(es$paired.data)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "red", box.fill = "blue",
          central.tendency = FALSE, error.bars.type = "CI", ef.size = FALSE,
          points = SAKTransparent(c("red", "blue"), .5), main = "Paired")
  SAKPlot(es, violin = FALSE, ef.size = FALSE, main = "Paired, no violin, no effect size")
  op <- par(mar = c(5, 4, 4, 4) + 0.1)
  on.exit(par(op))
  SAKPlot(es, violin = FALSE, ef.size = TRUE, points = FALSE, main = "Paired, no violin, effect size, no points")
  SAKPlot(es, violin.shape = c("left", "right"), violin.width = 0.2, main = "Custom")

  expect_equal(1, 1)
})

test_that("paired (reversed groups) works", {
  es <- makePairedData(reverseGroups = TRUE)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "red", box.fill = "blue",
         central.tendency = FALSE, error.bars.type = "CI", ef.size = FALSE,
         points = SAKTransparent(c("red", "blue"), .5), main = "Paired with reversed groups")
  expect_equal(1, 1)
})

test_that("paired with NAs works", {
  es <- makePairedData(addSomeNAs = TRUE)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "red", box.fill = "blue",
         central.tendency = FALSE, error.bars.type = "CI", ef.size = FALSE,
         points = SAKTransparent(c("red", "blue"), .5), main = "Paired with some NAs")
  expect_equal(1, 1)
})

test_that("bar charts work", {
  es <- makeES1()
  SAKPlot(es, bar = TRUE, violin = FALSE, box = FALSE, box.fill = "blue",
         central.tendency = FALSE, error.bars.type = "CI", ef.size = FALSE,
         points = FALSE, main = "Bar chart")
  expect_equal(1, 1)
})

test_that("custom effect axis", {
  n <- 100
  df <- data.frame(val = c(rnorm(n, mean = 10), rnorm(n, mean = 10 + 1)),
                   group = rep(c("Control", "Group"), each = 2),
                   id = rep(1:n, each = 2))

  ef.size.ticks = c("Large negative effect" = -0.8,
                    "Medium negative effect" = -0.5, "Small negative effect" = -0.2,
                    "No effect" = 0, "Small positive effect" = 0.2,
                    "Medium positive effect" = 0.5, "Large positive effect" = 0.8)


  d <- SAKDifference(df, effect.type = "cohens", data.col = 1, group.col = 2)
  op <- par(mar = c(5, 4, 4, 10))
  on.exit(par(op))
  expect_error(SAKPlot(d, ef.size.ticks = ef.size.ticks, ef.size.las = 1, ef.size.label = "", main = "Cohen's with custom labels"), NA)
})

test_that("Axis las", {
  n <- 100
  df <- data.frame(val = c(rnorm(n, mean = 10), rnorm(n, mean = 10 + 1), rnorm(n, mean = 10 + 1.4)),
                   group = rep(c("Group1", "Group2", "Group3"), each = n))
  d2 <- SAKDifference(df, groups = c("Group1", "Group2"), data.col = 1, group.col = 2)
  op <- par(mar = c(5, 4, 4, 4))
  expect_error(SAKPlot(d2, las = 1, ef.size.las = 1, main = "las horizontal"), NA)
  par(op)
  d3 <- SAKDifference(df, data.col = 1, group.col = 2)
  expect_error(SAKPlot(d3, las = 1, ef.size.las = 1, main = "las horizontal"), NA)
})
