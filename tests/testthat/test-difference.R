
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
  DurgaDiff(data[data$Group %in% c("ZControl1", "Group1"),], data.col = "Measurement", group.col = "Group", R = 1000)
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
    DurgaDiff(data[data$Group %in% c("ZControl1", "Group1"),],
               groups = c("ZControl1", "Group1"),
               na.rm = addSomeNAs,
               data.col = "Measurement", group.col = "Group", R = 1000,
               id.col = "ID")
  } else {
    DurgaDiff(data[data$Group %in% c("ZControl1", "Group1"),],
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

test_that("expand contrasts", {
  .makeX <- function(g) matrix(g, nrow = 2)

  expect_equal(expandContrasts("g1 - g2, g3 - g4", c("g1", "g2", "g3", "g4")), .makeX(c("g1", "g2", "g3", "g4")), ignore_attr = TRUE)
  expect_error(expandContrasts("g1 - g2, g3 - g5", c("g1", "g2", "g3", "g4")))
  expect_error(expandContrasts("g1 - g1", c("g1", "g2", "g3", "g4")))
  expect_equal(expandContrasts(" g1-g2 ", c("g1", "g2")), .makeX(c("g1", "g2")), ignore_attr = TRUE)
  expect_equal(expandContrasts(" g1    -g2 ", c("g2", "g1")), .makeX(c("g1", "g2")), ignore_attr = TRUE)
  expect_equal(expandContrasts(" g2  - g1 ", c("g2", "g1")), .makeX(c("g2", "g1")), ignore_attr = TRUE)
  expect_equal(expandContrasts("g1 - g2", c("g4", "g1", "g2")), .makeX(c("g1", "g2")), ignore_attr = TRUE)
  expect_equal(expandContrasts("g1 - g2", c("g4", "g2", "g1")), .makeX(c("g1", "g2")), ignore_attr = TRUE)
  expect_equal(expandContrasts(". - g1", c("g1", "g2", "g3")), .makeX(c("g2", "g1", "g3", "g1")), ignore_attr = TRUE)
  expect_equal(expandContrasts(". - g2", c("g1", "g2", "g3")), .makeX(c("g1", "g2", "g3", "g2")), ignore_attr = TRUE)
  expect_equal(expandContrasts("g3-.", c("g1", "g2", "g3")), .makeX(c("g3", "g1", "g3", "g2")), ignore_attr = TRUE)
  expect_equal(expandContrasts("*", c("g1", "g2")), .makeX(c("g2", "g1")), ignore_attr = TRUE)
  expect_equal(expandContrasts("*", c("g2", "g1")), .makeX(c("g1", "g2")), ignore_attr = TRUE)
  expect_equal(expandContrasts("*", c("g1")), NULL, ignore_attr = TRUE)
  expect_equal(expandContrasts(" g1 - g11 ", c("g1", "g11")), .makeX(c("g1", "g11")), ignore_attr = TRUE)
  expect_equal(expandContrasts(" g1 - g11 ", c("g11", "g1")), .makeX(c("g1", "g11")), ignore_attr = TRUE)
  expect_equal(expandContrasts(" g11 - g1 ", c("g1", "g11")), .makeX(c("g11", "g1")), ignore_attr = TRUE)
  expect_equal(expandContrasts(" . - g1", c("g1", "g11")), .makeX(c("g11", "g1")), ignore_attr = TRUE)
  expect_equal(expandContrasts(" . - g1", c("g1", "g11", "g111")), .makeX(c("g11", "g1", "g111", "g1")), ignore_attr = TRUE)
  expect_equal(expandContrasts(" g1 - .", c("g1", "g11")), .makeX(c("g1", "g11")), ignore_attr = TRUE)
  expect_equal(expandContrasts(" g1 - . ", c("g1", "g11", "g111")), .makeX(c("g1", "g11", "g1", "g111")), ignore_attr = TRUE)
  expect_error(expandContrasts(" . - . ", c("g1", "g11")))

  expect_equal(expandContrasts("g1 - g2", c("g1", "g2", "g3", "g4"), c("G1", "G2", "G3", "G4")), .makeX(c("g1", "g2")))
  expect_equal(expandContrasts("G1 - G2", c("g1", "g2", "g3", "g4"), c("G1", "G2", "G3", "G4")), .makeX(c("g1", "g2")))
  expect_equal(expandContrasts("g2 - g1", c("g1", "g2", "g3", "g4"), c("G1", "G2", "G3", "G4")), .makeX(c("g2", "g1")))
  expect_equal(expandContrasts("G2 - G1", c("g1", "g2", "g3", "g4"), c("G1", "G2", "G3", "G4")), .makeX(c("g2", "g1")))
  expect_equal(expandContrasts("G2 - g1", c("g1", "g2", "g3", "g4"), c("G1", "G2", "G3", "G4")), .makeX(c("g2", "g1")))
  expect_equal(expandContrasts("g3 - g2", c("g1", "g2", "g3", "g4"), c("G1", "G2", "G3", "G4")), .makeX(c("g3", "g2")))
  expect_equal(expandContrasts("G3 - G2", c("g1", "g2", "g3", "g4"), c("G1", "G2", "G3", "G4")), .makeX(c("g3", "g2")))
  expect_equal(expandContrasts("g4 - g1", c("g1", "g2", "g3", "g4"), c("G1", "G2", "G3", "G4")), .makeX(c("g4", "g1")))
  expect_equal(expandContrasts("G4 - G1, g3-G2", c("g1", "g2", "g3", "g4"), c("G1", "G2", "G3", "G4")), .makeX(c("g4", "g1", "g3", "g2")))
  expect_equal(expandContrasts(c("G4 - G1", "g3-G2"), c("g1", "g2", "g3", "g4"), c("G1", "G2", "G3", "G4")), .makeX(c("g4", "g1", "g3", "g2")))
  expect_equal(expandContrasts("g2 - g1", c("g1", "g2", "g3", "g4"), c("G1", "G2", "G3", "G4")), .makeX(c("g2", "g1")))
  expect_equal(expandContrasts("g2 - g1", c(G1 = "g1", G2 = "g2", G3 = "g3", G4 = "g4"), c("G1", "G2", "G3", "G4")), .makeX(c("g2", "g1")))
  expect_equal(expandContrasts("G2 - G1", c(G1 = "g1", G2 = "g2", G3 = "g3", G4 = "g4"), c("G1", "G2", "G3", "G4")), .makeX(c("g2", "g1")))
  expect_equal(expandContrasts(c("G2 - G1", "G3 - G1"), c(G1 = "g1", G2 = "g2", G3 = "g3", G4 = "g4"), c("G1", "G2", "G3", "G4")), .makeX(c("g2", "g1", "g3", "g1")))
})

test_that("group names and contrasts", {
  data <- makeData()
  groups <- c(Ctrl = "ZControl1", `G 1` = "Group1", `G 2` = "Group2")
  d <- DurgaDiff(data, "Measurement", "Group", groups = groups)

  DurgaDiff(data, "Measurement", "Group", groups = groups, contrasts = "*")
  # Contrasts with data values
  DurgaDiff(data, "Measurement", "Group", groups = groups, contrasts = c("Group2 - Group1"))
  # Contrasts with group labels
  DurgaDiff(data, "Measurement", "Group", groups = groups, contrasts = c("G 2 - G 1"))
  # Contrasts with micture
  DurgaDiff(data, "Measurement", "Group", groups = groups, contrasts = c("G 2 - Group1"))

  # Each pair of plots in a row should be identical. One uses group data value, the other uses group label
  op <- par(mfrow = c(2, 2))
  expect_error(DurgaPlot(d, contrasts = "Group1 - ZControl1", main = "1 contrast"), NA)
  expect_error(DurgaPlot(d, contrasts = "G 1 - Ctrl", main = "1 contrast"), NA)
  expect_error(DurgaPlot(d, contrasts = "Group1 - ZControl1, Group2 - ZControl1", main = "2 contrasts"), NA)
  expect_error(DurgaPlot(d, contrasts = "G 1 - Ctrl, G 2 - Ctrl", main = "2 contrasts"), NA)
  par(op)
})


test_that("matrix data", {
  n <- 20
  m <- matrix(c(val = rnorm(n), sample(1:3, n, replace = TRUE)), nrow = n)
  d <- DurgaDiff(m, 1, 2)
  expect_error(DurgaPlot(d), NA)
})

test_that("contrast plots", {
  data <- makeData()
  groups <- c("ZControl1", "Group1", "Group2", "Group3")

  # Default contrasts
  d <- DurgaDiff(data, "Measurement", "Group", groups = groups)
  ng <- length(d$groups)
  expect_equal(length(d$group.differences), ng * (ng - 1) / 2)
  DurgaPlot(d, main = "Default contrasts")

  # 2 contrasts
  contrasts <- "Group1 - ZControl1, Group2 - ZControl1, Group3 - ZControl1"
  d <- DurgaDiff(data, "Measurement", "Group", groups = groups, contrasts = contrasts)
  expect_equal(length(d$group.differences), 3)
  expect_equal(d$group.differences[[1]]$groups[1], "Group1")
  expect_equal(d$group.differences[[1]]$groups[2], "ZControl1")
  expect_equal(d$group.differences[[2]]$groups[1], "Group2")
  expect_equal(d$group.differences[[2]]$groups[2], "ZControl1")
  expect_equal(d$group.differences[[3]]$groups[1], "Group3")
  expect_equal(d$group.differences[[3]]$groups[2], "ZControl1")
  DurgaPlot(d, violin = F, central.tendency.width = 0.1, central.tendency.symbol = "segment", central.tendency.params = , main = "Explicit contrasts")

  # Shorthand for same as above
  d <- DurgaDiff(data, "Measurement", "Group", groups = groups, contrasts = ". - ZControl1")
  expect_equal(length(d$group.differences), 3)
  expect_equal(d$group.differences[[1]]$groups[1], "Group1")
  expect_equal(d$group.differences[[1]]$groups[2], "ZControl1")
  expect_equal(d$group.differences[[2]]$groups[1], "Group2")
  expect_equal(d$group.differences[[2]]$groups[2], "ZControl1")
  expect_equal(d$group.differences[[3]]$groups[1], "Group3")
  expect_equal(d$group.differences[[3]]$groups[2], "ZControl1")
  DurgaPlot(d, points.method = "jitter", main = "Explicit contrast shorthand")

  # Invalid contrasts
  expect_error(DurgaDiff(data, "Measurement", "Group", groups = groups, contrasts = 1))
  expect_error(DurgaPlot(d, contrasts = 1))

  # Just 1 contrast
  data <- makeData()
  d <- DurgaDiff(data, "Measurement", "Group", groups = groups)
  DurgaPlot(d, contrasts = c(`Diff` = "Group3 - Group2"), main = "Plot 1 labelled diff")
  DurgaPlot(d, contrasts = c(`Diff` = "Group2 - Group3"), mai = "Plot negative diff")
  d <- DurgaDiff(data, "Measurement", "Group", groups = groups, effect.type = "cohens")
  DurgaPlot(d, contrasts = c(`Diff` = "Group3 - Group2"), main = "Plot 1 Cohen's")
  DurgaPlot(d, contrasts = c(`Diff` = "Group2 - Group3"), mai = "Plot negative Cohen's")
  # Single contrast in diff
  d <- DurgaDiff(data, "Measurement", "Group", groups = c("Group3", "Group2"))
  DurgaPlot(d, main = "Restricted groups in diff")
  d <- DurgaDiff(data, "Measurement", "Group", groups = c("Group3", "Group2"), effect.type = "cohens")
  DurgaPlot(d, main = "Restricted groups in diff Cohen's")
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
    d <- DurgaDiff(df, 1, 2)
    checkGroup(d$group.statistics, v, mean)
  }
  # Now combine all groups into one data set
  set.seed(1)
  df <- do.call(rbind, lapply(ns, function(n) data.frame(val = rnorm(n, mean, sd), group = rep(sprintf("g%03d", n), n))))
  d <- DurgaDiff(df, 1, 2)
  for (i in seq_along(ns)) {
    gn <- sprintf(sprintf("g%03d", ns[i]))
    checkGroup(d$group.statistics[i, ], df$val[df$group == gn], mean)
  }

  # For debugging
  if (FALSE) {
    DurgaPlot(d, violin = F, points.method = "jitter", ef.size = F, central.tendency.dx = 0.15, error.bars.type = "CI")
    abline(h = mean, col = "lightgrey")
    DurgaPlot(d, add = T, violin = F, points = F, ef.size = F, central.tendency.dx = 0.25, error.bars.type = "SD")
    DurgaPlot(d, add = T, violin = F, points = F, ef.size = F, central.tendency.dx = 0.35, error.bars.type = "SE")
  }
})

test_that("print", {
  d <- makeES1()
  # This is a regex
  expected <- paste0("Bootstrapped effect size\\n",
              "  Measurement ~ Group\\n",
              "Groups:\\n",
              "              mean   median       sd       se  CI\\.lower CI\\.upper  n\\n",
              "Group1    122\\.9210 118\\.8367 21\\.31850 3\\.370750 116\\.10300 129\\.7390 40\\n",
              "ZControl1 102\\.3007 103\\.2276 22\\.16668 3\\.504859  95\\.21141 109\\.3899 40\\n",
              "Unpaired Mean difference \\(R = 1000, bootstrap CI method = bca\\):\\n",
              "  ZControl1 - Group1: -20\\.6203, 95% CI \\[-30\\.[0-9]+, -10\\.[0-9]+\\]")
  expect_output(print(d), expected)
  # Print mismatched lines
  # got <- capture.output(print(d))
  # expLines <- gsub("\\\\", "", strsplit(expected, "\\\\n")[[1]])
  # matched <- got == expLines
  # print(got[!matched])
  # print(expLines[!matched])

  checkSummaryMatches <- function(d1, d2) {
    d1s <- capture.output(print(d1))
    d2s <- capture.output(print(d2))
    # Crude check - just remove all numbers
    d1s <- sub("[0-9].*", "", d1s)
    d2s <- sub("[0-9].*", "", d2s)
    expect_equal(d1s, d2s)
  }

  # Check that formula output matches non-formula
  d1 <- DurgaDiff(petunia, 1, 2)
  d2 <- DurgaDiff(height ~ group, petunia)
  checkSummaryMatches(d1, d2)

  # Paired
  d1 <- DurgaDiff(insulin, 1, 2, 3)
  d2 <- DurgaDiff(sugar ~ treatment, insulin, id.col = "id")
  checkSummaryMatches(d1, d2)

  d <- DurgaDiff(log(sugar) ~ treatment, insulin, id.col = "id")
  s <- capture.output(print(d))
  expect_equal(s[2], "  log(sugar) ~ treatment")

  # Spaces in names
  `Scapus length` <- rnorm(40, 10)
  `Group name` <- sample(c("Group one", "Group two", "Group three"), 40, replace = TRUE)
  # Just check it doesn't crash
  expect_error(capture.output(print(DurgaDiff(`Scapus length` ~ `Group name`))), NA)
  expect_error(capture.output(print(DurgaDiff(log(`Scapus length`) ~ `Group name`))), NA)
})

test_that("contrasts", {
  data <- makeData()

  # Default contrasts
  d <- DurgaDiff(data, "Measurement", "Group")
  ng <- length(d$groups)
  expect_equal(length(d$group.differences), ng * (ng - 1) / 2)

  # All in 1 string
  contrasts = "Group1 - ZControl1, Group2 - ZControl1, Group3 - ZControl1, Group4 - ZControl1"
  d <- DurgaDiff(data, "Measurement", "Group", contrasts = contrasts)
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
  d2 <- DurgaDiff(data, "Measurement", "Group", contrasts = contrasts)
  expect_equal(length(d2$group.differences), 4)
  expect_equal(d2$group.differences[[1]]$groups[1], "Group1")
  expect_equal(d2$group.differences[[1]]$groups[2], "ZControl1")
  compareDiffs(d2, d)

  contrasts <- matrix(c("Group1", "ZControl1", "Group2", "ZControl1", "Group3", "ZControl1", "Group4", "ZControl1"), nrow = 2)
  d2 <- DurgaDiff(data, "Measurement", "Group", contrasts = contrasts)
  expect_equal(length(d2$group.differences), 4)
  expect_equal(d2$group.differences[[1]]$groups[1], "Group1")
  expect_equal(d2$group.differences[[1]]$groups[2], "ZControl1")
  compareDiffs(d2, d, tolerance = 1)

  expect_error(DurgaDiff(data, "Measurement", "Group", contrasts = "Group2:ZControl"))
  expect_error(DurgaDiff(data, "Measurement", "Group", contrasts = "ZControl"))
  expect_error(DurgaDiff(data, "Measurement", "Group", contrasts = ""))
  expect_error(DurgaDiff(data, "Measurement", "Group", contrasts = "Group 2 - Group 1"))

  # Wildcard contrasts
  d <- DurgaDiff(data, "Measurement", "Group", effect.type = "cohen", contrasts = "*")
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
  expect_error(DurgaDiff(df, effect.type = "wrong", data.col = 1, group.col = 2)) # Should throw an error
  expect_error(DurgaDiff(df, effect.type = "mean", data.col = 1, group.col = 2), NA)
  expect_error(DurgaDiff(df, effect.type = "cohens", data.col = 1, group.col = 2), NA)
  expect_error(DurgaDiff(df, effect.type = "hedges", data.col = 1, group.col = 2), NA)
  expect_error(DurgaDiff(df, effect.type = "mean", id.col = "id", data.col = 1, group.col = 2), NA)
  expect_error(DurgaDiff(df, effect.type = "cohens", id.col = "id", data.col = 1, group.col = 2), NA)
  expect_error(DurgaDiff(df, effect.type = "hedges", id.col = "id", data.col = 1, group.col = 2), NA)

  # Check unstandardised diff (diff of means)
  d <- DurgaDiff(df, data.col = 1, group.col = 2, effect.type = "mean")
  pwd <- d$group.difference[[1]]
  expect_equal(pwd$groups[1], "Group")
  expect_equal(pwd$groups[2], "Control")
  expect_lt(pwd$t0, realDiff)
  expect_lt(pwd$bca[4], realDiff)
  expect_gt(pwd$bca[5], realDiff)

  # Check Cohen's D
  d <- DurgaDiff(df, data.col = 1, group.col = 2, effect.type = "cohens")
  pwd <- d$group.difference[[1]]
  expect_equal(pwd$groups[1], "Group")
  expect_equal(pwd$groups[2], "Control")
  expect_equal(pwd$t0, 0.918991, tolerance = 0.0001) # Cohen's d
  expect_lt(pwd$bca[4], 0.918991) # Should be positive but small
  expect_gt(pwd$bca[5], 0.918991)
  # Save Cohen's d for later
  cohensD <- pwd$t0
  # Swap groups
  d <- DurgaDiff(df, groups = c("Group", "Control"), data.col = 1, group.col = 2, effect.type = "cohens")
  pwd <- d$group.difference[[1]]
  expect_equal(pwd$groups[1], "Control")
  expect_equal(pwd$groups[2], "Group")
  expect_equal(pwd$t0, -0.918991, tolerance = 0.0001) # Cohen's d
  expect_lt(pwd$bca[4], -0.918991) # Should be negative but small
  expect_gt(pwd$bca[5], -0.918991)

  # Check Hedges' g
  d <- DurgaDiff(df, data.col = 1, group.col = 2, effect.type = "hedges")
  pwd <- d$group.difference[[1]]
  expect_equal(pwd$groups[1], "Group")
  expect_equal(pwd$groups[2], "Control")
  # Estimate Hedges' g by correcting Cohen's d (Lakens 2013, p. 3 eqn 4)
  n1 <- d$group.statistics[2,"n"]
  n2 <- d$group.statistics[1,"n"]
  hedgesG <- cohensD * (1 - 3 / (4 * (n1 + n2) - 9))
  expect_equal(pwd$t0, hedgesG, tolerance = 0.0001)
  expect_lt(pwd$bca[4], 0.918991) # Should be positive but small
  expect_gt(pwd$bca[5], 0.918991)
  # Swap groups
  d <- DurgaDiff(df, groups = c("Group", "Control"), data.col = 1, group.col = 2, effect.type = "hedges")
  pwd <- d$group.difference[[1]]
  expect_equal(pwd$groups[1], "Control")
  expect_equal(pwd$groups[2], "Group")
  expect_equal(pwd$t0, -hedgesG, tolerance = 0.0001)
  expect_lt(pwd$bca[4], -hedgesG) # Should be negative but small
  expect_gt(pwd$bca[5], -hedgesG)

  ### Paired effect sizes ###
  # Check unstandardised diff
  d <- DurgaDiff(df, data.col = 1, group.col = 2, id.col = 3, effect.type = "mean")
  pwd <- d$group.difference[[1]]
  expect_equal(pwd$groups[1], "Group")
  expect_equal(pwd$groups[2], "Control")
  expect_lt(pwd$t0, realDiff)
  expect_lt(pwd$bca[4], realDiff)
  expect_gt(pwd$bca[5], realDiff)

  # Check Cohen's D
  d <- DurgaDiff(df, data.col = 1, group.col = 2, id.col = 3, effect.type = "cohens")
  pwd <- d$group.difference[[1]]
  expect_equal(pwd$groups[1], "Group")
  expect_equal(pwd$groups[2], "Control")
  diffs <- df$val[df$group == "Group"] - df$val[df$group == "Control"]
  cohensD <- mean(diffs) / sd(diffs)
  expect_equal(pwd$t0, cohensD)
  expect_lt(pwd$bca[4], cohensD) # Should be positive but small
  expect_gt(pwd$bca[5], cohensD)

  d <- DurgaDiff(df, data.col = 1, group.col = 2, id.col = 3, effect.type = "hedges")
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

  d <- DurgaDiff(df, effect.type = "mean", data.col = 1, group.col = 2)
  pwd <- d$group.difference[[1]]
  expect_equal(d$group.names, c("Control", "Treatment"))
  expect_equal(pwd$groups[1], "Treatment")
  expect_equal(pwd$groups[2], "Control")

  # Check all effect types
  expect_error(DurgaDiff(df, effect.type = "mean", data.col = 1, group.col = 2), NA)
  expect_error(DurgaDiff(df, effect.type = "cohens", data.col = 1, group.col = 2), NA)
  expect_error(DurgaDiff(df, effect.type = "hedges", data.col = 1, group.col = 2), NA)
  expect_error(DurgaDiff(df, id.col = "id", data.col = 1, group.col = 2), NA)

})

test_that("difference handles NA", {
  n <- 100
  df <- data.frame(val = c(rnorm(n), rnorm(n, mean = 1)),
                   group = c(rep("Control", n), rep("Group", n)))
  df[c(1, 4, 10, 65), 1] <- NA

  # This should throw an error (something about na.rm)
  expect_error(DurgaDiff(df, na.rm = FALSE, data.col = 1, group.col = 2), "na.rm")
  # This should NOT throw an error
  expect_error(DurgaDiff(df, na.rm = TRUE, data.col = 1, group.col = 2), NA)
  # Check column name checks
  expect_error(DurgaDiff(df, id.col = "id", na.rm = TRUE, data.col = 1, group.col = 2), "column name")
  expect_error(DurgaDiff(df, data.col = "XXX", group.col = 2), "column name")
  expect_error(DurgaDiff(df, data.col = 1, group.col = "XXX"), "column name")
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

  d2 <- DurgaDiff(df, data.col = 1, group.col = 2)
  # This should NOT throw an error
  op <- par(mar = c(5, 4, 4, 4) + 0.1)
  expect_error(DurgaPlot(d2, ef.size = TRUE, main = "Two groups, effect size default"), NA)
  expect_error(DurgaPlot(d2, ef.size.pos = "right", main = "Two groups, effect size right"), NA)
  par(op)
  expect_error(DurgaPlot(d2, ef.size = FALSE, main = "Two groups, no effect size"), NA)
  expect_error(DurgaPlot(d2, ef.size.position = "below", main = "Two groups, effect size below"), NA)
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

  d3 <- DurgaDiff(df, data.col = 1, group.col = 2)
  # This should NOT throw an error
  expect_error(DurgaPlot(d3, bar = FALSE, box = FALSE, main = "Three groups"), NA)
  expect_error(DurgaPlot(d3, violin.trunc = FALSE, main = "No violin truncation"), NA)
  expect_error(DurgaPlot(d3, violin.trunc = 0.05, main = "0.05 violin truncation"), NA)
  expect_error(DurgaPlot(d3, ef.size.violin = FALSE, main = "No effect size violin"), NA)
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

  d3 <- DurgaDiff(df, data.col = 1, group.col = 2)
  # This should NOT throw an error
  expect_error(DurgaPlot(d3, bar = FALSE, box = FALSE, main = "Group factor"), NA)
})

test_that("many groups", {
  n <- 12
  groupMean <- round(rnorm(n, mean = 20, sd = 8))
  val <- c(sapply(groupMean, function(m) rnorm(n, m, 4)))
  groups <- sapply(seq_len(n), function(i) paste0("G", i, "-", groupMean[i]))
  trt <- c(sapply(seq_along(groupMean), function(i) rep(groups[i], n)))
  df <- data.frame(Height = val, Treatment = trt)
  d <- DurgaDiff(df, "Height", "Treatment", groups = groups)
  par(cex = 0.8)
  expect_error(DurgaPlot(d, main = "1/3) Many groups"), NA)
  expect_error(DurgaPlot(d, main = "2/3) Many groups, control-.", contrasts = paste(df$Treatment[1], "-.")), NA)
  expect_error(DurgaPlot(d, main = "3/3) Many groups, .-control", contrasts = paste(" . - ", df$Treatment[1])), NA)
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
    es <- DurgaDiff(data[data$Group %in% c("ZControl1", "Group1"),], data.col = "Measurement", group.col = "Group", R = 1000)
    es2 <- DurgaDiff(data[data$Group %in% c("ZControl1", "Group1"),],
                         data.col = "Measurement", group.col = "Group", R = 1000,
                         id.col = "ID")


    # Generate some plots
    l <- layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), nrow = 4, ncol = 5, byrow =  TRUE))
    #layout.show(l)

    #par(mfrow = c(2, 4))
    #a)
    DurgaPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = FALSE, box.fill = FALSE,
            central.tendency = "median", error.bars.type = "CI", ef.size = FALSE,
            points = DurgaTransparent(c("red", "blue"), .5))

    #b)
    DurgaPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "white", box.fill = "white",
            central.tendency = "median", error.bars = "SD", ef.size = FALSE,
            points = DurgaTransparent(c("red", "blue"), .5),)

    #c)
    DurgaPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "white", box.fill = "white",
            central.tendency = "median", error.bars = "SE", ef.size = FALSE,
            points = DurgaTransparent(c("red", "blue"), .5),)

    #d)
    DurgaPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "white", box.fill = "white",
            central.tendency.type = "mean", error.bars.type = "CI", ef.size = FALSE,
            points = DurgaTransparent(c("red", "blue"), .5),)

    #e)
    DurgaPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "white", box.fill = "white",
            central.tendency.type = "mean", error.bars = "SD", ef.size = FALSE,
            points = DurgaTransparent(c("red", "blue"), .5),)

    #f)
    DurgaPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "white", box.fill = "white",
            central.tendency.type = "mean", error.bars = "SE", ef.size = FALSE,
            points = DurgaTransparent(c("red", "blue"), .5),)

    #g)
    DurgaPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = DurgaTransparent(c("red", "blue"), .5),
            box.fill = "white", error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = FALSE)

    #h)
    DurgaPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = DurgaTransparent(c("red", "blue"), .5),
            box.fill = "white", error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = DurgaTransparent(c("red", "blue"), .5))

    #i)
    DurgaPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = DurgaTransparent(c("red", "blue"), .5),
            box.fill = DurgaTransparent(c("red", "blue"), .7), error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = FALSE)

    #j)
    DurgaPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = DurgaTransparent(c("red", "blue"), .5),
            box.fill = DurgaTransparent(c("red", "blue"), .7), error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = DurgaTransparent(c("red", "blue"), .5))


    #k)
    DurgaPlot(es, bar = FALSE, bar.fill = FALSE, violin.shape = "left-half",
            violin = DurgaTransparent(c("red", "blue"), .6),
            violin.fill = DurgaTransparent(c("red", "blue"), .6),
            box = "white",
            box.fill = "white",
            error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = DurgaTransparent(c("red", "blue"), .5))

    #l)
    DurgaPlot(es, bar = FALSE, bar.fill = FALSE, violin.shape = "left-half",
            violin = DurgaTransparent(c("red", "blue"), .6),
            violin.fill = DurgaTransparent(c("red", "blue"), .6),
            box = DurgaTransparent(c("red", "blue"), .5),
            box.fill = DurgaTransparent(c("red", "blue"), .7), error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = FALSE)

    ##m)
    DurgaPlot(es, bar = FALSE, bar.fill = FALSE, violin.shape = "left-half",
            violin = DurgaTransparent(c("red", "blue"), .6),
            violin.fill = DurgaTransparent(c("red", "blue"), .6),
            box = DurgaTransparent(c("red", "blue"), .5),
            box.fill = DurgaTransparent(c("red", "blue"), .7), error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = DurgaTransparent(c("red", "blue"), .5))

    #n)
    DurgaPlot(es, bar = FALSE, bar.fill = FALSE, violin.shape = "right-half",
            violin = DurgaTransparent(c("red", "blue"), .6),
            violin.fill = DurgaTransparent(c("red", "blue"), .6),
            box = "white",
            box.fill = "white",
            error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = DurgaTransparent(c("red", "blue"), .5))
    #o)
    DurgaPlot(es, bar = FALSE, bar.fill = FALSE, violin.shape = "right-half",
            violin = DurgaTransparent(c("red", "blue"), .6),
            violin.fill = DurgaTransparent(c("red", "blue"), .6),
            box = DurgaTransparent(c("red", "blue"), .5),
            box.fill = DurgaTransparent(c("red", "blue"), .7), error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = FALSE)

    #p)
    DurgaPlot(es, bar = FALSE, bar.fill = FALSE, violin.shape = "right-half",
            violin = DurgaTransparent(c("red", "blue"), .6),
            violin.fill = DurgaTransparent(c("red", "blue"), .6),
            box = DurgaTransparent(c("red", "blue"), .5),
            box.fill = DurgaTransparent(c("red", "blue"), .7), error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = DurgaTransparent(c("red", "blue"), .5))

    #q)
    DurgaPlot(es, bar = FALSE, bar.fill = FALSE, violin.shape = "full",
            violin = DurgaTransparent(c("red", "blue"), .6),
            violin.fill = DurgaTransparent(c("red", "blue"), .6),
            box = DurgaTransparent(c("red", "blue"), .5),
            box.fill = DurgaTransparent(c("red", "blue"), .7), error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = FALSE)

    #r)
    DurgaPlot(es2, bar = FALSE, bar.fill = FALSE, violin.shape = "full",
            violin = DurgaTransparent(c("red", "blue"), .4),
            violin.fill = DurgaTransparent(c("red", "blue"), .8),
            box = DurgaTransparent(c("grey10"), .1),
            box.fill = DurgaTransparent(c("red", "blue"), .7), error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = TRUE,
            points = DurgaTransparent(c("red", "blue"), .5), paired = TRUE)

    #s)
    DurgaPlot(es, bar = FALSE, bar.fill = FALSE, violin.shape = "full",
            violin = DurgaTransparent(c("red", "blue"), .6),
            violin.fill = DurgaTransparent(c("red", "blue"), .6),
            box = "white",
            box.fill = "white",
            error.bars.type = "CI",
            central.tendency.type = "mean", central.tendency = FALSE, ef.size = FALSE,
            points = DurgaTransparent(c("red", "blue"), .5))

    #t)
    DurgaPlot(es, bar = FALSE, bar.fill = FALSE, violin.shape = "full",
            violin = DurgaTransparent(c("red", "blue"), .6),
            violin.fill = DurgaTransparent(c("red", "blue"), .6),
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
  DurgaPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = FALSE, box.fill = FALSE,
          central.tendency.type = "median", error.bars.type = "CI", error.bars.cross.width = 0.05,
          ef.size = FALSE, points = DurgaTransparent(c("red", "blue"), .5),
          main = "Violin FALSE, median, no effect size")
  DurgaPlot(es, violin = FALSE, central.tendency = FALSE, error.bars = FALSE, ef.size = FALSE,
          main = "No central tendency, error bar, effect size")
  expect_equal(1, 1)
})

test_that("central tendency FALSE works", {
  es <- makeES1()
  expect_false(es$paired.data)
  DurgaPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "red", box.fill = "blue",
         central.tendency = FALSE, error.bars.type = "CI", ef.size = FALSE,
         points = DurgaTransparent(c("red", "blue"), .5), main = "Central tendency FALSE")
  expect_equal(1, 1)
})

test_that("paired works", {
  es <- makePairedData()
  expect_true(es$paired.data)
  DurgaPlot(es,
          paired = DurgaTransparent("green", 0.5), paired.lty = 2,
          bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "red", box.fill = "blue",
          central.tendency = FALSE, error.bars.type = "CI", ef.size = FALSE,
          points = DurgaTransparent(c("red", "blue"), .5), main = "Paired")
  DurgaPlot(es, violin = FALSE, ef.size = FALSE, paired.lwd = 3, main = "Paired, no violin, no effect size")
  op <- par(mar = c(5, 4, 4, 4) + 0.1)
  on.exit(par(op))
  DurgaPlot(es, violin = FALSE, ef.size = TRUE, points = FALSE, main = "Paired, no violin, effect size, no points")
  DurgaPlot(es, violin.shape = c("left", "right"), violin.width = 0.2, main = "Custom")

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
  sortedD <- DurgaDiff(df, "Value", "Treatment", "Id", groups = treatments)
  df <- df[sample(nrow(df)), ]
  set.seed(1)
  d <- DurgaDiff(df, "Value", "Treatment", "Id", groups = treatments)
  # Shuffling the rows should not affect the results
  compareDiffs(sortedD, d, tolerance = 0.1)
  DurgaPlot(d, contrasts = "Before-Control, After - Before", ef.size = FALSE, violin = FALSE, points.dx = c(0.2, 0, -0.2), main = "No intersecting lines?")

  expect_equal(1, 1)
})

test_that("paired (reversed groups) works", {
  es <- makePairedData(reverseGroups = TRUE)
  DurgaPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "red", box.fill = "blue",
         central.tendency = FALSE, error.bars.type = "CI", ef.size = FALSE,
         points = DurgaTransparent(c("red", "blue"), .5), main = "Paired with reversed groups")
  expect_equal(1, 1)
})

test_that("paired with NAs works", {
  es <- makePairedData(addSomeNAs = TRUE)
  DurgaPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "red", box.fill = "blue",
         central.tendency = FALSE, error.bars.type = "CI", ef.size = FALSE,
         points = DurgaTransparent(c("red", "blue"), .5), main = "Paired with some NAs")
  expect_equal(1, 1)
})

test_that("bar charts work", {
  es <- makeES1()
  DurgaPlot(es, bar = TRUE, violin = FALSE, box = FALSE, box.fill = "blue",
          error.bars = FALSE, error.bars.type = "CI", ef.size = FALSE,
          points = FALSE, main = "Bar chart, no error bars")
  DurgaPlot(es, bar = TRUE, violin = FALSE, box = FALSE, box.fill = "blue",
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


  d <- DurgaDiff(df, effect.type = "cohens", data.col = 1, group.col = 2)
  op <- par(mar = c(5, 4, 4, 10))
  on.exit(par(op))
  expect_error(DurgaPlot(d, ef.size.ticks = ef.size.ticks, ef.size.las = 1, ef.size.label = "", main = "Cohen's with custom labels"), NA)
})

test_that("Axis las", {
  n <- 100
  df <- data.frame(val = c(rnorm(n, mean = 10), rnorm(n, mean = 10 + 1), rnorm(n, mean = 10 + 1.4)),
                   group = rep(c("Group1", "Group2", "Group3"), each = n))
  d2 <- DurgaDiff(df, groups = c("Group1", "Group2"), data.col = 1, group.col = 2)
  op <- par(mar = c(5, 4, 4, 4))
  expect_error(DurgaPlot(d2, las = 1, ef.size.las = 1, main = "las horizontal"), NA)
  par(op)
  d3 <- DurgaDiff(df, data.col = 1, group.col = 2)
  expect_error(DurgaPlot(d3, las = 1, ef.size.las = 1, main = "las horizontal"), NA)
})

test_that("Other data frame classes", {
  n <- 40
  df <- tibble::tibble(val = c(rnorm(n, mean = 10), rnorm(n, mean = 10 + 1), rnorm(n, mean = 10 + 1.4)),
                   group = rep(c("Group1", "Group2", "Group3"), each = n))
  d <- DurgaDiff(df, data.col = 1, group.col = 2)
  expect_error(DurgaPlot(d, main = "Tibble"), NA)

  df <- data.table::data.table(val = c(rnorm(n, mean = 10), rnorm(n, mean = 10 + 1), rnorm(n, mean = 10 + 1.4)),
                               group = rep(c("Group1", "Group2", "Group3"), each = n))
  d <- DurgaDiff(df, data.col = 1, group.col = 2)
  expect_error(DurgaPlot(d, main = "Data.table"), NA)
})

test_that("point colours", {
  n <- 40
  df <- data.frame(val = c(rnorm(n, mean = 10), rnorm(n, mean = 10 + 1)),
                   group = rep(c("Group1", "Group2"), each = n),
                   sex = sample(factor(c("Male", "Female")), n, replace = TRUE))
  d <- DurgaDiff(df, data.col = 1, group.col = 2)
  expect_error(DurgaPlot(d, points = 1:2, main = "Group colours"), NA)
  expect_error(DurgaPlot(d, points = 1:5, main = "Group colours (truncated palette)"), NA)
  expect_error(DurgaPlot(d, points = as.numeric(df$sex) + 1, main = "Sex colours"), NA)
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
  expect_error(DurgaDiff(df, data.col = 1, group.col = 2, id.col = 3), "paired data")
})

test_that("plot contrasts", {
  n <- 40
  set.seed(1)
  df <- data.frame(val = c(rnorm(n, mean = 10), rnorm(n, mean = 10 + 1)),
                   group = rep(c("Group1", "Group2"), each = n),
                   sex = sample(factor(c("Male", "Female")), n, replace = TRUE))
  d <- DurgaDiff(df, data.col = 1, group.col = 2)
  expect_error(DurgaPlot(d, points = 1:2, main = "1/3) Default contrast"), NA)
  d <- DurgaDiff(df, data.col = 1, group.col = 2, contrasts = ". - Group1")
  expect_error(DurgaPlot(d, points = 1:2, main = "2/3) DurgaDiff contrast"), NA)
  d <- DurgaDiff(df, data.col = 1, group.col = 2)
  expect_error(DurgaPlot(d, points = 1:2, main = "3/3) DurgaPlot contrast", contrasts = ". - Group1"), NA)
})

test_that("effect size position", {
  n <- 20
  set.seed(1)
  df <- data.frame(val = c(rnorm(n, mean = 10), rnorm(n, mean = 10 + 0.8), rnorm(n, mean = 10 + 0.4)),
                   group = rep(c("Group1", "Group2", "Group3"), each = n),
                   sex = sample(factor(c("Male", "Female", "Juvenile")), n, replace = TRUE))
  d <- DurgaDiff(df, data.col = 1, group.col = 2)

  expect_error(DurgaPlot(d, contrasts = Filter(function(d) d$bca[4] > 0 || d$bca[5] < 0, d$group.differences),
                       main = "1/3) ef.size position default, filtered contrasts"), NA)
  expect_error(DurgaPlot(d, ef.size.position = "below", contrasts = "Group3 - Group1", main = "2/3) ef.size below, 1 contrast"), NA)
  expect_error(DurgaPlot(d, main = "3/3) ef.size default position, shorthand contrasts", contrasts = ". - Group1"), NA)

  # Check missing contrast
  d <- DurgaDiff(df, data.col = 1, group.col = 2, contrasts = "Group2 - Group1")
  expect_error(DurgaPlot(d, contrasts = "Group3 - Group1"))
})

test_that("Grouped plot", {
  # Attempt to reproduce Figure 1 in
  # Ostap-Chec, M., Opalek, M., Stec, D., & Miler, K. (2021). Discontinued alcohol consumption elicits withdrawal symptoms in honeybees. Biology Letters, 17(6), 20210182. doi:10.1098/rsbl.2021.0182

  op <- par(mar = c(4, 5, 1, 10.5) + 0.1, mfrow = c(2, 1))
  on.exit(par(op))
  cols <- RColorBrewer::brewer.pal(4, "Dark2")
  for (i in 1:2) {

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
    d <- DurgaDiff(long, "prob", "combined", groups = cg, contrasts = NULL)
    ps <- expect_error(DurgaPlot(d, x.axis = FALSE, left.las = 1, left.ylab = "probability of\nresponding (%)",
                                 frame.plot = FALSE, ef.size = FALSE, violin = FALSE, points = FALSE,
                                 group.dx = c(0.6, 0.2, -0.2, -0.6),
                                 central.tendency = cols, error.bars = cols), NA)
    # Force axis lines to cover the plot extents
    axis(1, at = par("usr")[1:2], labels = c("", ""), lwd.ticks = 0)
    axis(2, at = par("usr")[3:4], labels = c("", ""), lwd.ticks = 0)
    # Calculate centre of each group
    at <- colMeans(matrix(ps$extents[,1], nrow = 4))
    axis(1, at = at, labels = names(meansA))
    title(xlab = "solution")
    legend(par("usr")[2] + 0.2, mean(par("usr")[3:4]), yjust = 0.5, xpd = NA,
           c("1, no exposure", "2, short exposure", "3, exposure withheld", "4, constant exposure"),
           col = cols, lwd = 3, pch = 19, bty = "n")
  }
})

test_that("group labels etc", {
  # Add in some fake sex data to the insulin data set
  data <- cbind(insulin, Sex = sample(c("Male", "Female"), nrow(insulin), replace = TRUE))
  # Thin it the data so individual symbols are visible
  data <- data[data$id %in% 1:15, ]

  d <- DurgaDiff(data, "sugar", "treatment", "id", groups = c("Before insulin" = "before", "After insulin" = "after"), na.rm = TRUE)
  expect_error(DurgaPlot(d,
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

  d <- DurgaDiff(iris, data.col = "Sepal.Length", group.col = "Species")
  expect_error(DurgaPlot(d, bar = TRUE, error.bars.type = "SD", points = FALSE, main = "Bar chart with std. deviation"), NA)
  expect_error(DurgaPlot(d, box = TRUE, error.bars = TRUE, central.tendency.type = "median", error.bars.type = "CI", points = FALSE, main = "Box plot with 95% CI"), NA)
  expect_error(DurgaPlot(d, bar = TRUE, central.tendency.symbol = "segment", error.bars.type = "SE", points = FALSE, main = "Bar chart with SE"), NA)
})

test_that("plot miscellanea", {
  d <- DurgaDiff(damselfly, "length", "maturity",
                     groups = c("Immature" = "juvenile", "Mature" = "adult"))

  # Axis text is smaller when there are multiple columns
  op <- par(mfrow = c(1, 3))
  on.exit(par(op))

  DurgaPlot(d, ef.size.position = "below", main = "Text size consistent")
  par(mar = c(5, 4, 4, 6) + 0.1)
  DurgaPlot(d, bar = T)
  DurgaPlot(d, box = T, xlim = c(0, 5), ylim = c(28, 40), main = "Explicit limits")
  expect_equal(1, 1)

  expect_error(DurgaDiff(petunia, 1, 2,
                         groups = c("self-fertilised" = "self_fertilised",
                                    "intercrossed" = "inter_cross",
                                    "Westerham-crossed" = "westerham_cross"),
                         contrasts = c("Westerham-crossed - self-fertilised",
                                       "Westerham-crossed - intercrossed",
                                       "intercrossed - self-fertilised")), NA)
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
  d90 <- DurgaDiff(petunia, "height", "group", ci.conf = .9)
  d95 <- DurgaDiff(petunia, "height", "group", ci.conf = .95)
  d99 <- DurgaDiff(petunia, "height", "group", ci.conf = .99)
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

test_that("Diff error detection", {
  n <- 40
  set.seed(1)
  realDiff <- 1
  df <- data.frame(val = c(rnorm(n, mean = 10), rnorm(n, mean = 10 + realDiff)),
                   group = c(rep("Control", n), rep("Group", n)),
                   id = c(1:n, 1:n))
  # Shuffle
  df <- df[sample(nrow(df)), ]

  # Missing group
  expect_error(DurgaDiff(df, data.col = 1, group.col = 2, groups = c("Control", "Group", "Nonexistent")), "Nonexistent")

  # Non factor ID
  d1 <- DurgaDiff(df, data.col = 1, group.col = 2, id.col = 3)
  expect_true(d1$paired.data)
  # Factor ID
  df$id <- factor(df$id)
  d2 <- DurgaDiff(df, data.col = 1, group.col = 2, id.col = 3)
  expect_true(d2$paired.data)
  compareDiffs(d2, d1)

  # Missing IDs - remove some of the Group values
  removeIdxs <- sample(which(df$group == "Group"), 10)
  df <- df[-removeIdxs, ]
  expect_error(DurgaDiff(df, data.col = 1, group.col = 2, id.col = 3), "id")

  # Non-numeric value
  expect_error(DurgaDiff(df, data.col = 3, group.col = 2, id.col = 3), "numeric")
  # Empty data frame
  expect_error(DurgaDiff(df[numeric(0), ], data.col = 1, group.col = 2, id.col = 3), "No data")
})

test_that("single group", {
  op <- par(mfrow = c(1, 2))
  on.exit(par(op))

  n <- 40
  df <- data.frame(val = rnorm(n, 10),
                   group = rep("G1", each = n))
  d <- DurgaDiff(df, data.col = 1, group.col = 2)
  p <- expect_error(DurgaPlot(d, main = "1 group in data"), NA)

  df <- data.frame(val = c(rnorm(n, 10), rnorm(n, 11)),
                   group = rep(c("G1", "G2"), each = n))
  d <- DurgaDiff(df, data.col = 1, group.col = 2, groups = "G1")
  expect_error(DurgaPlot(d, main = "1 group in diff"), NA)
})

test_that("group colours", {
  op <- par(mfrow = c(2, 2))
  on.exit(par(op))

  d <- DurgaDiff(petunia, 1, 2)
  DurgaPlot(d, ef.size = FALSE, group.colour = "Set1", main = "Group colours Set1")
  DurgaPlot(d, ef.size = FALSE, group.colour = "blue", main = "group.colours blue")
  DurgaPlot(d, ef.size = FALSE, group.colour = c("red", "Green", "blue"), main = "group.colours RGB, points fill coloured",
            points = "black", points.params = list(pch = 21))
  DurgaPlot(d, ef.size = FALSE, group.colour = c("red", "Green", "#0000ff"), main = "group.colours RGB, points red fill",
            points.params = list(pch = 21, bg = "red"))

  # Invalid colour/palette
  expect_error(DurgaPlot(d, group.colour = "Fred"))
  expect_error(DurgaPlot(d, group.colour = c("Set1", "Dark1")))
})

test_that("minor formatting", {
  d <- DurgaDiff(damselfly, 1, 3)
  expect_error(DurgaPlot(d, main = "Offset lines, styled error bars, ef size lines",
                         error.bars = "red", error.bars.lwd = 5, error.bars.lty = 4,
                         ef.size.mean.line.dx = 0.2, ef.size.line.col = "blue",
                         ef.size.line.lwd = 3, ef.size.line.lty = 3,
                         ef.size.violin = "blue", ef.size.violin.fill = "pink"), NA)
  expect_error(DurgaPlot(d, main = "Offset lines, styled error bars, ef size line",
                         ef.size.position = "below",
                         error.bars = "red", error.bars.lwd = 5, error.bars.lty = 4,
                         ef.size.mean.line.dx = 0.2, ef.size.line.col = "blue",
                         ef.size.line.lwd = 3, ef.size.line.lty = 1,
                         ef.size.violin = "blue", ef.size.violin.fill = "pink"), NA)
})

test_that("Spaces in column names", {
  n <- 40
  df <- data.frame(`Genital length` = c(rnorm(n, mean = 10)),
                   `Cannibalism y/n` = sample(c("Yes", "No"), n, replace = TRUE),
                   check.names = FALSE)
  d <- DurgaDiff(df, "Genital length", "Cannibalism y/n")
  expect_error(DurgaPlot(d, xlab = "Cannibalism?", main = "Spaces in names, xlab"), NA)
})


test_that("Formula", {

  n <- 40
  set.seed(1)
  valVar <- c(rnorm(n, mean = 10), rnorm(n, mean = 11))
  df <- data.frame(`Scapus length` = valVar,
                   val = valVar,
                   group = c(rep("Control", n), rep("Group", n)),
                   id = c(1:n, 1:n),
                   check.names = FALSE)
  `Group cat` <- df$group

  op <- par(mfrow = c(1, 2))
  on.exit(par(op))
  DurgaPlot(DurgaDiff(valVar ~ group, df, id.col = "id", groups = c("Control", "Group")), main = "Formula interface")
  DurgaPlot(DurgaDiff(df, "val", "group", "id", groups = c("Control", "Group")), main = "Standard interface")

  # More complicated formulae
  DurgaPlot(DurgaDiff(log(valVar) ~ group, df), main = "Formula interface")
  DurgaPlot(DurgaDiff(`Scapus length` ~ group, data = df), main = "Formula interface")
  DurgaPlot(DurgaDiff(log(`Scapus length`) ~ group, data = df), main = "Formula interface")
  DurgaPlot(DurgaDiff(log(`Scapus length`) ~ `Group cat`, data = df), main = "Formula interface")

  expect_error(DurgaDiff(`Scapus lengh` ~ group, data = df))
})

test_that("custom stat", {
  # Example from http://www.estimationstats.com/#/analyze/shared-control

  ds <- "A1	A2	B1	B2	C1	C2	D1	D2
8.885	10.135	8	-35	3.375	6.625	0.54	-0.54
14.38	11.94	7	-30	-0.3	2.3	1.98	0.02
8.015	6.025	17	-25	10.025	11.975	1.1	0.9
5.835	3.045	15	-20	2.35	3.65	3.42	0.58
5.47	1.87	12	-15	7.675	8.325	2.54	1.46
12.06	12.64	5	-10	9	9	1.655	2.345
11.72	9.66	6	-5	7.325	6.675	4.865	1.135
10.315	9.265	19	0	6.65	5.35	3.98	2.02
5.065	6.155	16	5	4.975	3.025	3.1	2.9
8.235	10.785	11	10	3.3	0.7	2.215	3.785
15.08	12.36	18	15	11.625	8.375	6.305	1.695
13.485	10.175	9	20	17.765	8.235	5.42	2.58
11.3	12.38	14	25	17.09	6.91	4.54	3.46
9.82	9.66	13	30	19.41	8.59	3.655	4.345
9.565	6.955	10	35	20.735	9.265	2.775	5.225"

  dw <- read.delim(text = ds)
  # Convert to long format
  l <- lapply(colnames(dw), function(c) data.frame(value = as.numeric(dw[[c]]), group = c, id = seq_along(c)))
  df <- do.call(rbind, l)
  dd <- DurgaDiff(df, 1, 2, contrasts = ". - A1")
  dc <- DurgaDiff(df, 1, 2, contrasts = ". - A1", effect.type = function(x1, x2) { median(x2) - median(x1) })

  # Check that group details are the same, pairwise differences are different
  od <- capture.output(print(dd))
  oc <- capture.output(print(dc))
  expect_equal(which(od == oc), 1:12)

  op <- par(mfrow = c(2, 1))
  expect_error(DurgaPlot(dd, main = "Defaults"), NA)
  expect_error(DurgaPlot(dc, main = "Custom median differences"), NA)
  par(op)
})

# TODO if we introduce median diff
# test_that("median diff", {
#   n <- 100
#   set.seed(1)
#   g1v <- rnorm(n, mean = 10, sd = 10)
#   g2v <- rexp(n, 0.05)
#   df <- data.frame(val = c(g1v, g2v),
#                    group = c(rep("Control", n), rep("Group", n)),
#                    id = c(1:n, 1:n))
#   d <- DurgaDiff(df, 1, 2, effect.type = "median")
#   DurgaPlot(d, box = TRUE)
#   expect_equal(length(d$group.differences), 1)
#   expect_equal(d$group.differences[[1]]$t0, median(g2v) - median(g1v))
#
#   d <- DurgaDiff(df, 1, 2, effect.type = "mean")
#   DurgaPlot(d, box = TRUE)
#   expect_equal(length(d$group.differences), 1)
#   expect_equal(d$group.differences[[1]]$t0, mean(g2v) - mean(g1v))
# })
