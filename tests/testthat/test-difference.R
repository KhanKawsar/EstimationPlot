#### TODO test different values of ci.type

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
               effect.type = "paired", id.col = "ID")
  } else {
    SAKDifference(data[data$Group %in% c("ZControl1", "Group1"),],
               na.rm = addSomeNAs,
               data.col = "Measurement", group.col = "Group", R = 1000,
               effect.type = "paired", id.col = "ID")
  }
}

compareDiffs <- function(d1, d2) {
  expect_equal(names(d1), names(d2))
  expect_equal(d1$t0, d2$t0, tolerance = )
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
  expect_error(SAKDifference(df, effect.type = "paired", id.col = "id", data.col = 1, group.col = 2), NA)

  # Check unstandardised diff
  d <- SAKDifference(df, data.col = 1, group.col = 2, id.col = 3, effect.type = "unstandardised")
  pwd <- d$group.difference[[1]]
  expect_equal(pwd$groups[1], "Group")
  expect_equal(pwd$groups[2], "Control")
  expect_lt(pwd$bca[4], realDiff)
  expect_gt(pwd$bca[5], realDiff)

  # Check paired diff
  d <- SAKDifference(df, data.col = 1, group.col = 2, id.col = 3, effect.type = "paired")
  pwd <- d$group.difference[[1]]
  expect_equal(pwd$groups[1], "Group")
  expect_equal(pwd$groups[2], "Control")
  expect_lt(pwd$bca[4], realDiff)
  expect_gt(pwd$bca[5], realDiff)

  # Check Cohen's D
  d <- SAKDifference(df, data.col = 1, group.col = 2, id.col = 3, effect.type = "cohens")
  pwd <- d$group.difference[[1]]
  expect_equal(pwd$groups[1], "Group")
  expect_equal(pwd$groups[2], "Control")
  expect_lt(pwd$bca[4], 0.5) # Should be positive but small
  expect_gt(pwd$bca[5], 0)
  # Swap groups
  d <- SAKDifference(df, groups = c("Group", "Control"), data.col = 1, group.col = 2, id.col = 3, effect.type = "cohens")
  pwd <- d$group.difference[[1]]
  expect_equal(pwd$groups[1], "Control")
  expect_equal(pwd$groups[2], "Group")
  expect_lt(pwd$bca[4], 0) # Should be negative but small
  expect_gt(pwd$bca[5], -0.5)

  # Check Hedge's g
  d <- SAKDifference(df, data.col = 1, group.col = 2, id.col = 3, effect.type = "hedges")
  pwd <- d$group.difference[[1]]
  expect_equal(pwd$groups[1], "Group")
  expect_equal(pwd$groups[2], "Control")
  expect_lt(pwd$bca[4], 0.5) # Should be positive but small
  expect_gt(pwd$bca[5], 0)
  # Swap groups
  d <- SAKDifference(df, groups = c("Group", "Control"), data.col = 1, group.col = 2, id.col = 3, effect.type = "hedges")
  pwd <- d$group.difference[[1]]
  expect_equal(pwd$groups[1], "Control")
  expect_equal(pwd$groups[2], "Group")
  expect_lt(pwd$bca[4], 0) # Should be negative but small
  expect_gt(pwd$bca[5], -0.5)
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
  expect_error(SAKDifference(df, effect.type = "paired", id.col = "id", data.col = 1, group.col = 2), NA)

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
  n <- 20
  groupMean <- round(rnorm(20, mean = 20, sd = 8))
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
                    effect.type = "paired", id.col = "ID")


  # Generate some plots
  l <- layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), nrow = 4, ncol = 5, byrow =  TRUE))
  #layout.show(l)

  #par(mfrow = c(2, 4))
  #a)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = FALSE, box.fill = FALSE,
         central.tendency = "median", error.bars = "CI", ef.size = FALSE,
         points = transparent(c("red", "blue"), .5))

  #b)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "white", box.fill = "white",
         central.tendency = "median", error.bars = "SD", ef.size = FALSE,
         points = transparent(c("red", "blue"), .5),)

  #c)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "white", box.fill = "white",
         central.tendency = "median", error.bars = "SE", ef.size = FALSE,
         points = transparent(c("red", "blue"), .5),)

  #d)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "white", box.fill = "white",
         central.tendency = "mean", error.bars = "CI", ef.size = FALSE,
         points = transparent(c("red", "blue"), .5),)

  #e)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "white", box.fill = "white",
         central.tendency = "mean", error.bars = "SD", ef.size = FALSE,
         points = transparent(c("red", "blue"), .5),)

  #f)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "white", box.fill = "white",
         central.tendency = "mean", error.bars = "SE", ef.size = FALSE,
         points = transparent(c("red", "blue"), .5),)

  #g)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = transparent(c("red", "blue"), .5),
         box.fill = "white",error.bars = "CI",
         central.tendency = "mean", mean = NA, ef.size = FALSE,
         points = FALSE)

  #h)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = transparent(c("red", "blue"), .5),
         box.fill = "white",error.bars = "CI",
         central.tendency = "mean", mean = NA, ef.size = FALSE,
         points = transparent(c("red", "blue"), .5))

  #i)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = transparent(c("red", "blue"), .5),
         box.fill = transparent(c("red", "blue"), .7),error.bars = "CI",
         central.tendency = "mean", mean = NA, ef.size = FALSE,
         points = FALSE)

  #j)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = transparent(c("red", "blue"), .5),
         box.fill = transparent(c("red", "blue"), .7),error.bars = "CI",
         central.tendency = "mean", mean = NA, ef.size = FALSE,
         points = transparent(c("red", "blue"), .5))


  #k)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = "left-half",
         violin_border = transparent(c("red", "blue"), .6),
         violin_fill = transparent(c("red", "blue"), .6),
         box = "white",
         box.fill = "white",
         error.bars = "CI",
         central.tendency = "mean", mean = NA, ef.size = FALSE,
         points = transparent(c("red", "blue"), .5))

  #l)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = "left-half",
         violin_border = transparent(c("red", "blue"), .6),
         violin_fill = transparent(c("red", "blue"), .6),
         box = transparent(c("red", "blue"), .5),
         box.fill = transparent(c("red", "blue"), .7),error.bars = "CI",
         central.tendency = "mean", mean = NA, ef.size = FALSE,
         points = FALSE)

  ##m)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = "left-half",
         violin_border = transparent(c("red", "blue"), .6),
         violin_fill = transparent(c("red", "blue"), .6),
         box = transparent(c("red", "blue"), .5),
         box.fill = transparent(c("red", "blue"), .7),error.bars = "CI",
         central.tendency = "mean", mean = NA, ef.size = FALSE,
         points = transparent(c("red", "blue"), .5))

  #n)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = "right-half",
         violin_border = transparent(c("red", "blue"), .6),
         violin_fill = transparent(c("red", "blue"), .6),
         box = "white",
         box.fill = "white",
         error.bars = "CI",
         central.tendency = "mean", mean = NA, ef.size = FALSE,
         points = transparent(c("red", "blue"), .5))
  #o)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = "right-half",
         violin_border = transparent(c("red", "blue"), .6),
         violin_fill = transparent(c("red", "blue"), .6),
         box = transparent(c("red", "blue"), .5),
         box.fill = transparent(c("red", "blue"), .7),error.bars = "CI",
         central.tendency = "mean", mean = NA, ef.size = FALSE,
         points = FALSE)

  #p)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = "right-half",
         violin_border = transparent(c("red", "blue"), .6),
         violin_fill = transparent(c("red", "blue"), .6),
         box = transparent(c("red", "blue"), .5),
         box.fill = transparent(c("red", "blue"), .7),error.bars = "CI",
         central.tendency = "mean", mean = NA, ef.size = FALSE,
         points = transparent(c("red", "blue"), .5))

  #q)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = "full",
         violin_border = transparent(c("red", "blue"), .6),
         violin_fill = transparent(c("red", "blue"), .6),
         box = transparent(c("red", "blue"), .5),
         box.fill = transparent(c("red", "blue"), .7),error.bars = "CI",
         central.tendency = "mean", mean = NA, ef.size = FALSE,
         points = FALSE)

  #r)
  SAKPlot(es2, bar = FALSE, bar.fill = FALSE, violin = "full",
         violin_border = transparent(c("red", "blue"), .4),
         violin_fill = transparent(c("red", "blue"), .8),
         box = transparent(c("grey10"), .1),
         box.fill = transparent(c("red", "blue"), .7),error.bars = "CI",
         central.tendency = "mean", mean = NA, ef.size = TRUE,
         points = transparent(c("red", "blue"), .5), paired = TRUE)

  #s)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = "full",
         violin_border = transparent(c("red", "blue"), .6),
         violin_fill = transparent(c("red", "blue"), .6),
         box = "white",
         box.fill = "white",
         error.bars = "CI",
         central.tendency = "mean", mean = NA, ef.size = FALSE,
         points = transparent(c("red", "blue"), .5))

  #t)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = "full",
         violin_border = transparent(c("red", "blue"), .6),
         violin_fill = transparent(c("red", "blue"), .6),
         box = "white",
         box.fill = "white",
         error.bars = "CI",
         central.tendency = "mean", mean = NA, ef.size = FALSE,
         points = FALSE)

  }
  expect_equal(1, 1)
})

test_that("box FALSE works", {
  es <- makeES1()
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = FALSE, box.fill = FALSE,
          central.tendency = "median", error.bars = "CI", ef.size = FALSE,
          points = transparent(c("red", "blue"), .5), main = "Violin FALSE, median, no effect size")
  SAKPlot(es, violin = FALSE, central.tendency = FALSE, error.bars = FALSE, ef.size = FALSE,
          main = "No central tendency, error bar, effect size")
  expect_equal(1, 1)
})

test_that("central tendency FALSE works", {
  es <- makeES1()
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "red", box.fill = "blue",
         central.tendency = FALSE, error.bars = "CI", ef.size = FALSE,
         points = transparent(c("red", "blue"), .5), main = "Central tendency FALSE")
  expect_equal(1, 1)
})

test_that("paired works", {
  es <- makePairedData()
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "red", box.fill = "blue",
          central.tendency = FALSE, error.bars = "CI", ef.size = FALSE,
          points = transparent(c("red", "blue"), .5), main = "Paired")
  SAKPlot(es, violin = FALSE, ef.size = FALSE, main = "Paired, no violin, effect size")
  SAKPlot(es, violin = FALSE, ef.size = FALSE, points = FALSE, main = "Paired, no violin, effect size, points")
  expect_equal(1, 1)
})

test_that("paired (reversed groups) works", {
  es <- makePairedData(reverseGroups = TRUE)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "red", box.fill = "blue",
         central.tendency = FALSE, error.bars = "CI", ef.size = FALSE,
         points = transparent(c("red", "blue"), .5), main = "Paired with reversed groups")
  expect_equal(1, 1)
})

test_that("paired with NAs works", {
  es <- makePairedData(addSomeNAs = TRUE)
  SAKPlot(es, bar = FALSE, bar.fill = FALSE, violin = FALSE, box = "red", box.fill = "blue",
         central.tendency = FALSE, error.bars = "CI", ef.size = FALSE,
         points = transparent(c("red", "blue"), .5), main = "Paired with some NAs")
  expect_equal(1, 1)
})

test_that("bar charts work", {
  es <- makeES1()
  SAKPlot(es, bar = TRUE, violin = FALSE, box = FALSE, box.fill = "blue",
         central.tendency = FALSE, error.bars = "CI", ef.size = FALSE,
         points = FALSE, main = "Bar chart")
  expect_equal(1, 1)
})
