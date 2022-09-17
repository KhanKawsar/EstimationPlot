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

compareDiffs <- function(d1, d2, tolerance = 0.1) {
  expect_equal(length(d1$group.differences), length(d2$group.differences))
  for (i in seq_len(length(d1$group.differences))) {
    pd1 <- d1$group.differences[[i]]
    pd2 <- d2$group.differences[[i]]
    expect_equal(pd1$groups, pd2$groups)
    expect_equal(pd1$t0, pd2$t0)
    expect_equal(pd1$R, pd2$R)
    expect_equal(pd1$bca[, 1], pd2$bca[, 1])
    # Columns 3 and 4 can vary randomly
    expect_equal(pd1$bca[, c(4, 5)], pd2$bca[, c(4, 5)], tolerance = tolerance)
  }
}

##########################################################################
# Tests start here ####

test_that("contrast plots", {
  data <- makeData()
  groups <- c("ZControl1", "Group1", "Group2", "Group3")

  # Default contrasts
  d <- SAKDifference(data, "Measurement", "Group", groups = groups)
  ng <- length(d$groups)
  expect_equal(length(d$group.differences), ng * (ng - 1) / 2)
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

  # Shorthand for same as above
  d <- SAKDifference(data, "Measurement", "Group", groups = groups, contrasts = ". - ZControl1")
  expect_equal(length(d$group.differences), 3)
  expect_equal(d$group.differences[[1]]$groups[1], "Group1")
  expect_equal(d$group.differences[[1]]$groups[2], "ZControl1")
  expect_equal(d$group.differences[[2]]$groups[1], "Group2")
  expect_equal(d$group.differences[[2]]$groups[2], "ZControl1")
  expect_equal(d$group.differences[[3]]$groups[1], "Group3")
  expect_equal(d$group.differences[[3]]$groups[2], "ZControl1")
  SAKPlot(d, main = "Explicit contrast shorthand")
})

test_that("group stats", {

  checkGroup <- function(gs, vals, popMean) {
    gs <- unname(gs)
    expect_equal(gs[1], mean(vals))
    expect_equal(gs[2], median(vals))
    expect_equal(gs[3], sd(vals))
    expect_equal(gs[4], sd(vals) / sqrt(length(vals)))
    expect_lt(gs[5], popMean)
    expect_gt(gs[6], popMean)
  }

  # Test single group at a time
  set.seed(3)
  mean <- 50
  sd <- 17
  ns <- c(10, 20, 80, 200)
  for (n in ns) {
    v <- rnorm(n, mean, sd)
    df <- data.frame(v, rep("g", length(v)))
    d <- SAKDifference(df, 1, 2)
    checkGroup(d$group.statistics, v, mean)
  }
  # Now combine all groups into one data set
  set.seed(1)
  df <- do.call(rbind, lapply(ns, function(n) data.frame(val = rnorm(n, mean, sd), group = rep(sprintf("g%03d", n), n))))
  d <- SAKDifference(df, 1, 2)
  for (i in seq_along(ns)) {
    gn <- sprintf(sprintf("g%03d", ns[i]))
    checkGroup(d$group.statistics[i, ], df$val[df$group == gn], mean)
  }

  # For debugging
  if (FALSE) {
    SAKPlot(d, violin = F, points.method = "jitter", ef.size = F, central.tendency.dx = 0.15, error.bars.type = "CI")
    abline(h = mean, col = "lightgrey")
    SAKPlot(d, add = T, violin = F, points = F, ef.size = F, central.tendency.dx = 0.25, error.bars.type = "SD")
    SAKPlot(d, add = T, violin = F, points = F, ef.size = F, central.tendency.dx = 0.35, error.bars.type = "SE")
  }
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
  expect_equal(length(d$group.differences), ng * (ng - 1) / 2)

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
  compareDiffs(d2, d, tolerance = 1)

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

  # Check Hedges' g
  d <- SAKDifference(df, data.col = 1, group.col = 2, effect.type = "hedges")
  pwd <- d$group.difference[[1]]
  expect_equal(pwd$groups[1], "Group")
  expect_equal(pwd$groups[2], "Control")
  # Estimate Hedges' g by correcting Cohen's d (Lakens 2013, p. 3 eqn 4)
  n1 <- d$group.statistics[2,"N"]
  n2 <- d$group.statistics[1,"N"]
  hedgesG <- cohensD * (1 - 3 / (4 * (n1 + n2) - 9))
  expect_equal(pwd$t0, hedgesG, tolerance = 0.0001)
  expect_lt(pwd$bca[4], 0.918991) # Should be positive but small
  expect_gt(pwd$bca[5], 0.918991)
  # Swap groups
  d <- SAKDifference(df, groups = c("Group", "Control"), data.col = 1, group.col = 2, effect.type = "hedges")
  pwd <- d$group.difference[[1]]
  expect_equal(pwd$groups[1], "Control")
  expect_equal(pwd$groups[2], "Group")
  expect_equal(pwd$t0, -hedgesG, tolerance = 0.0001)
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
                   group = factor(c(rep("Control", n), rep("Treatment", n))),
                   id = c(1:n, 1:n))

  d <- SAKDifference(df, effect.type = "unstandardised", data.col = 1, group.col = 2)
  pwd <- d$group.difference[[1]]
  expect_equal(d$group.names, c("Control", "Treatment"))
  expect_equal(pwd$groups[1], "Treatment")
  expect_equal(pwd$groups[2], "Control")

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
  expect_error(SAKPlot(d, main = "1/3) Many groups"), NA)
  expect_error(SAKPlot(d, main = "2/3) Many groups, control-.", contrasts = paste(df$Treatment[1], "-.")), NA)
  expect_error(SAKPlot(d, main = "3/3) Many groups, .-control", contrasts = paste(" . - ", df$Treatment[1])), NA)
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
          central.tendency.type = "median", error.bars.type = "CI", error.bars.cross.width = 0.05,
          ef.size = FALSE, points = SAKTransparent(c("red", "blue"), .5),
          main = "Violin FALSE, median, no effect size")
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
  SAKPlot(es,
          paired = SAKTransparent("green", 0.5), paired.lty = 2,
          bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "red", box.fill = "blue",
          central.tendency = FALSE, error.bars.type = "CI", ef.size = FALSE,
          points = SAKTransparent(c("red", "blue"), .5), main = "Paired")
  SAKPlot(es, violin = FALSE, ef.size = FALSE, paired.lwd = 3, main = "Paired, no violin, no effect size")
  op <- par(mar = c(5, 4, 4, 4) + 0.1)
  on.exit(par(op))
  SAKPlot(es, violin = FALSE, ef.size = TRUE, points = FALSE, main = "Paired, no violin, effect size, no points")
  SAKPlot(es, violin.shape = c("left", "right"), violin.width = 0.2, main = "Custom")

  # Craft simple paired data to ensure that pair lines are correct
  n <- 10
  control <- sort(rnorm(n, 0))
  before <- sort(rnorm(n, 3))
  after <- sort(rnorm(n, 2.5))
  treatments <- c("Control", "Before", "After")
  df <- data.frame(Id = rep(seq_len(n), 3),
                   Treatment = rep(treatments, each = n),
                   Value = c(control, before, after))
  # Shuffle
  set.seed(1)
  sortedD <- SAKDifference(df, "Value", "Treatment", "Id", groups = treatments)
  df <- df[sample(nrow(df)), ]
  set.seed(1)
  d <- SAKDifference(df, "Value", "Treatment", "Id", groups = treatments)
  # Shuffling the rows should not affect the results
  compareDiffs(sortedD, d, tolerance = 0.1)
  SAKPlot(d, contrasts = "Before-Control, After - Before", ef.size = FALSE, violin = FALSE, points.dx = c(0.2, 0, -0.2), main = "No intersecting lines?")

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
          error.bars = FALSE, error.bars.type = "CI", ef.size = FALSE,
          points = FALSE, main = "Bar chart, no error bars")
  SAKPlot(es, bar = TRUE, violin = FALSE, box = FALSE, box.fill = "blue",
          error.bars = TRUE, error.bars.type = "CI", ef.size = FALSE,
          points = FALSE, main = "Bar chart, error bars")
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

test_that("Other data frame classes", {
  n <- 40
  df <- tibble::tibble(val = c(rnorm(n, mean = 10), rnorm(n, mean = 10 + 1), rnorm(n, mean = 10 + 1.4)),
                   group = rep(c("Group1", "Group2", "Group3"), each = n))
  d <- SAKDifference(df, data.col = 1, group.col = 2)
  expect_error(SAKPlot(d, main = "Tibble"), NA)

  df <- data.table::data.table(val = c(rnorm(n, mean = 10), rnorm(n, mean = 10 + 1), rnorm(n, mean = 10 + 1.4)),
                               group = rep(c("Group1", "Group2", "Group3"), each = n))
  d <- SAKDifference(df, data.col = 1, group.col = 2)
  expect_error(SAKPlot(d, main = "Data.table"), NA)
})

test_that("point colours", {
  n <- 40
  df <- data.frame(val = c(rnorm(n, mean = 10), rnorm(n, mean = 10 + 1)),
                   group = rep(c("Group1", "Group2"), each = n),
                   sex = sample(factor(c("Male", "Female")), n, replace = TRUE))
  d <- SAKDifference(df, data.col = 1, group.col = 2)
  expect_error(SAKPlot(d, points = 1:2, main = "Group colours"), NA)
  expect_error(SAKPlot(d, points = 1:5, main = "Group colours (truncated palette)"), NA)
  expect_error(SAKPlot(d, points = as.numeric(df$sex) + 1, main = "Sex colours"), NA)
})

test_that("detect missing paired data", {
  n <- 40
  # Test whether missing paired data is detected correctly
  set.seed(1)
  df <- data.frame(val = c(rnorm(n, mean = 10), rnorm(n, mean = 10 + 1)),
                   group = rep(c("Group1", "Group2"), each = n),
                   id = rep(1:n, 2))
  # If we take a random sample of rows, some will be missing their paired data
  df <- df[sample(seq_len(nrow(df)), round(n / 2)), ]
  expect_error(SAKDifference(df, data.col = 1, group.col = 2, id = 3), "paired data")
})

test_that("plot contrasts", {
  n <- 40
  set.seed(1)
  df <- data.frame(val = c(rnorm(n, mean = 10), rnorm(n, mean = 10 + 1)),
                   group = rep(c("Group1", "Group2"), each = n),
                   sex = sample(factor(c("Male", "Female")), n, replace = TRUE))
  d <- SAKDifference(df, data.col = 1, group.col = 2)
  expect_error(SAKPlot(d, points = 1:2, main = "1/3) Default contrast"), NA)
  d <- SAKDifference(df, data.col = 1, group.col = 2, contrasts = ". - Group1")
  expect_error(SAKPlot(d, points = 1:2, main = "2/3) SAKDifference contrast"), NA)
  d <- SAKDifference(df, data.col = 1, group.col = 2)
  expect_error(SAKPlot(d, points = 1:2, main = "3/3) SAKPlot contrast", contrasts = ". - Group1"), NA)
})

test_that("effect size position", {
  n <- 20
  set.seed(1)
  df <- data.frame(val = c(rnorm(n, mean = 10), rnorm(n, mean = 10 + 0.8), rnorm(n, mean = 10 + 0.4)),
                   group = rep(c("Group1", "Group2", "Group3"), each = n),
                   sex = sample(factor(c("Male", "Female", "Juvenile")), n, replace = TRUE))
  d <- SAKDifference(df, data.col = 1, group.col = 2)

  expect_error(SAKPlot(d, contrasts = Filter(function(d) d$bca[4] > 0 || d$bca[5] < 0, d$group.differences),
                       main = "1/3) ef.size position default, filtered contrasts"), NA)
  expect_error(SAKPlot(d, ef.size.position = "below", contrasts = "Group3 - Group1", main = "2/3) ef.size below, 1 contrast"), NA)
  expect_error(SAKPlot(d, main = "3/3) ef.size default position, shorthand contrasts", contrasts = ". - Group1"), NA)

  # Check missing contrast
  d <- SAKDifference(df, data.col = 1, group.col = 2, contrasts = "Group2 - Group1")
  expect_error(SAKPlot(d, contrasts = "Group3 - Group1"))
})

test_that("expand contrasts", {
  .makeX <- function(g) matrix(g, nrow = 2)

  expect_equal(expandContrasts("g1 - g2, g3 - g4", c("g1", "g2", "g3", "g4")), .makeX(c("g1", "g2", "g3", "g4")))
  expect_error(expandContrasts("g1 - g2, g3 - g5", c("g1", "g2", "g3", "g4")))
  expect_error(expandContrasts("g1 - g1", c("g1", "g2", "g3", "g4")))
  expect_equal(expandContrasts(" g1-g2 ", c("g1", "g2")), .makeX(c("g1", "g2")))
  expect_equal(expandContrasts(" g1    -g2 ", c("g2", "g1")), .makeX(c("g1", "g2")))
  expect_equal(expandContrasts(" g2  - g1 ", c("g2", "g1")), .makeX(c("g2", "g1")))
  expect_equal(expandContrasts("g1 - g2", c("g4", "g1", "g2")), .makeX(c("g1", "g2")))
  expect_equal(expandContrasts("g1 - g2", c("g4", "g2", "g1")), .makeX(c("g1", "g2")))
  expect_equal(expandContrasts(". - g1", c("g1", "g2", "g3")), .makeX(c("g2", "g1", "g3", "g1")))
  expect_equal(expandContrasts(". - g2", c("g1", "g2", "g3")), .makeX(c("g1", "g2", "g3", "g2")))
  expect_equal(expandContrasts("g3-.", c("g1", "g2", "g3")), .makeX(c("g3", "g1", "g3", "g2")))
  expect_equal(expandContrasts("*", c("g1", "g2")), .makeX(c("g2", "g1")))
  expect_equal(expandContrasts("*", c("g2", "g1")), .makeX(c("g1", "g2")))
  expect_equal(expandContrasts("*", c("g1")), NULL)
  expect_equal(expandContrasts(" g1 - g11 ", c("g1", "g11")), .makeX(c("g1", "g11")))
  expect_equal(expandContrasts(" g1 - g11 ", c("g11", "g1")), .makeX(c("g1", "g11")))
  expect_equal(expandContrasts(" g11 - g1 ", c("g1", "g11")), .makeX(c("g11", "g1")))
  expect_equal(expandContrasts(" . - g1", c("g1", "g11")), .makeX(c("g11", "g1")))
  expect_equal(expandContrasts(" . - g1", c("g1", "g11", "g111")), .makeX(c("g11", "g1", "g111", "g1")))
  expect_equal(expandContrasts(" g1 - .", c("g1", "g11")), .makeX(c("g1", "g11")))
  expect_equal(expandContrasts(" g1 - . ", c("g1", "g11", "g111")), .makeX(c("g1", "g11", "g1", "g111")))
  expect_error(expandContrasts(" . - . ", c("g1", "g11")))
})

test_that("Grouped plot", {
  # Attempt to reproduce Figure 1 in
  # Ostap-Chec, M., Opalek, M., Stec, D., & Miler, K. (2021). Discontinued alcohol consumption elicits withdrawal symptoms in honeybees. Biology Letters, 17(6), 20210182. doi:10.1098/rsbl.2021.0182

  n <- 6
  meansA <- c("water" = 5, "0.125%" = 5, "1.25%" = 5, "2%" = 12, "sugar" = 100)
  groups <- c("no exposure", "short exposure", "exposure withheld", "constant exposure")
  df <- data.frame(group = rep(groups, each = n),
                   prob = sapply(names(meansA), function(nm) pmin(100, rnorm(n * length(groups), meansA[nm], meansA[nm] / 2))),
                   check.names = FALSE)
  # Convert wide to long format
  rows <- lapply(names(meansA), function(nm) cbind.data.frame(group = df$group, treatment = rep(nm, nrow(df)), prob = df[[paste0("prob.", nm)]]))
  long <- as.data.frame(do.call(rbind, rows))
  long$combined <- paste(long$group, long$treatment)

  cg <- sapply(names(meansA), function(t) paste(groups, t))
  d <- SAKDifference(long, "prob", "combined", groups = cg, contrasts = NULL)
  expect_error(SAKPlot(d, ef.size = FALSE, violin = FALSE, points = FALSE, group.dx = c(0.6, 0.2, -0.2, -0.6), central.tendency = c("green", "orange", "purple", "pink")), NA)
})

test_that("group labels etc", {
  # Add in some fake sex data to the insulin data set
  data <- cbind(insulin, Sex = sample(c("Male", "Female"), nrow(insulin), replace = TRUE))
  # Thin it the data so individual symbols are visible
  data <- data[data$id %in% 1:15, ]

  d <- SAKDifference(data, "sugar", "treatment", "id", groups = c("Before insulin" = "before", "After insulin" = "after"), na.rm = TRUE)
  expect_error(SAKPlot(d,
          left.ylab = "Blood sugar level",
          violin.shape = c("left", "right"), violin.dx = c(-0.055, 0.055), violin.width = 0.3,
          points = "black",
          points.params = list(bg = ifelse(data$Sex == "Female", "red", "blue"), pch = ifelse(data$Sex == "Female", 21, 24)),
          ef.size.pch = 19,
          ef.size.violin = "blue",
          ef.size.violin.shape = "full",
          central.tendency = FALSE,
          main = "Customised plot"),
          NA)

  d <- SAKDifference(iris, data.col = "Sepal.Length", group.col = "Species")
  expect_error(SAKPlot(d, bar = TRUE, error.bars.type = "SD", points = FALSE, main = "Bar chart with std. deviation"), NA)
  expect_error(SAKPlot(d, box = TRUE, error.bars = TRUE, central.tendency.type = "median", error.bars.type = "CI", points = FALSE, main = "Box plot with 95% CI"), NA)
  expect_error(SAKPlot(d, bar = TRUE, central.tendency.symbol = "segment", error.bars.type = "SE", points = FALSE, main = "Box plot with SE"), NA)
})

test_that("plot miscellanea", {
  d <- SAKDifference(damselfly, "length", "group",
                     groups = c("Immature" = "juvenile", "Mature" = "adult"))

  # Axis text is smaller when there are multiple columns
  par(mfrow = c(1, 3))
  SAKPlot(d, ef.size.position = "below", main = "Text size consistent")
  par(mar = c(5, 4, 4, 6) + 0.1)
  SAKPlot(d, bar = T)
  SAKPlot(d, box = T, xlim = c(0, 5), ylim = c(28, 40))
  expect_equal(1, 1)

})

test_that("CI", {

  x <- rnorm(200)
  CI90 <- CI(x, .9)
  CI95 <- CI(x, .95)
  expect_lt(CI95[1], mean(x))
  expect_lt(CI95[1], CI90[1])
  expect_gt(CI95[2], mean(x))
  expect_gt(CI95[2], CI90[2])

  set.seed(1)
  d90 <- SAKDifference(petunia, "height", "group", ci.conf = .9)
  d95 <- SAKDifference(petunia, "height", "group", ci.conf = .95)
  d99 <- SAKDifference(petunia, "height", "group", ci.conf = .99)
  # CI should be smaller for lower levels
  for (g in seq_along(d90$groups)) {
    # Ensure we are comparing the same things
    expect_equal(d90$group.differences[[g]]$groupLabels, d95$group.differences[[g]]$groupLabels)
    expect_equal(d90$group.differences[[g]]$groupLabels, d99$group.differences[[g]]$groupLabels)

    # Check CI of mean differences
    # Lower limits
    expect_gt(d90$group.differences[[g]]$bca[4], d95$group.differences[[g]]$bca[4])
    expect_gt(d95$group.differences[[g]]$bca[4], d99$group.differences[[g]]$bca[4])
    # Upper limits
    expect_lt(d90$group.differences[[g]]$bca[5], d95$group.differences[[g]]$bca[5])
    expect_lt(d95$group.differences[[g]]$bca[5], d99$group.differences[[g]]$bca[5])

    # Check CI of group means
    expect_gt(d90$group.statistics[g, "CI.lower"], d95$group.statistics[g, "CI.lower"])
    expect_gt(d90$group.statistics[g, "CI.lower"], d95$group.statistics[g, "CI.lower"])
    expect_lt(d95$group.statistics[g, "CI.upper"], d99$group.statistics[g, "CI.upper"])
    expect_lt(d95$group.statistics[g, "CI.upper"], d99$group.statistics[g, "CI.upper"])
  }

})
