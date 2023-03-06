
makeData <- function(N = 40) {
  set.seed(1)
  data <- data.frame(Measurement = c(rnorm(N, mean = 100, sd = 25),
                                     rnorm(N, mean = 120, sd = 25),
                                     rnorm(N, mean = 100, sd = 50),
                                     rnorm(N, mean = 80, sd = 50),
                                     rnorm(N, mean = 100, sd = 12),
                                     rnorm(N, mean = 140, sd = 10)),
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

dataGroups <- function(data, nGroups) {
  groups <- unique(data$Group)
  groups <- head(groups, nGroups)
  groups <- groups[order(sub("[^0-9]*", "", groups))]
  groups <- groups[order(!grepl("Control", groups))]
  groups
}

test_that("Brackets", {
  d <- DurgaDiff(petunia, 1, 2)
  # Don't draw frame because brackets will appear in the upper margin
  ps <- DurgaPlot(d, ef.size = FALSE, frame.plot = FALSE)
  expect_error(DurgaBrackets(ps, lb.cex = 0.8, snap.to = 2), NA)
})

test_that("many bars", {

  data <- makeData()
  groups <- dataGroups(data, 5)
  d <- DurgaDiff(data, "Measurement", "Group", groups = groups, ci.conf = 0.99)
  diffs <- Filter(function(pwes) (pwes$bca[4] > 0 || pwes$bca[5] < 0), d$group.differences)
  col <- sapply(diffs, function(diff) ifelse(diff$t0 < 0, "#f06040", "#7090f0"))

  op <- par(mfrow = c(2, 2), mar = c(3, 4, 5, 0) + 0.1)
  on.exit(par(op))

  ps <- DurgaPlot(d, box = T, box.outline = F, points = T, ylim = c(-20, 340), frame.plot = F, ef.size = F,
                  main = "Differences, snap.to 2")
  expect_error(DurgaBrackets(ps, diffs, labels = "diff"), NA)
  ps <- DurgaPlot(d, contrasts = diffs, box = T, box.outline = F, points = T, ylim = c(-20, 340), frame.plot = F, ef.size = F,
                  main = "CI, no snap.to")
  expect_error(DurgaBrackets(ps, diffs, labels = "CI",
                             lb.cex = 0.6, lb.font = 1,
                             snap.to = 0,
                             br.lwd = 2, br.col = col), NA)


  # Just show the long brackets
  d <- DurgaDiff(data, "Measurement", "Group", groups = groups, effect.type = "cohen")
  diffs <- Filter(function(pwes) (pwes$bca[4] > 0 || pwes$bca[5] < 0), d$group.differences)
  col <- sapply(diffs, function(diff) ifelse(diff$t0 < 0, "#f06040", "#7090f0"))
  diffs <- Filter(function(pwes) (abs(diff(pwes$groupIndices)) > 2), d$group.differences)
  col <- sapply(diffs, function(diff) ifelse(diff$t0 < 0, "#f06040", "#7090f0"))

  ps <- DurgaPlot(d, box = T, box.outline = F, points = T, ylim = c(-20, 270), frame.plot = F, ef.size = F,
                  main = "CI-long, big red labels")
  expect_error(DurgaBrackets(ps, diffs, labels = "level CI",
                             br.lwd = 2, br.col = col, lb.col = "red"), NA)

  ps <- DurgaPlot(d, box = T, box.outline = F, points = T, ylim = c(-20, 270), frame.plot = F, ef.size = F,
                  main = "CI-diff, bold labels")
  expect_error(DurgaBrackets(ps, diffs, labels = "diff CI",
                             lb.cex = 0.8, lb.font = 2,
                             br.lwd = 2, br.col = col), NA)
})

test_that("labels", {
  data <- makeData()
  groups <- dataGroups(data, 100)
  d <- DurgaDiff(data, "Measurement", "Group", groups = groups)
  op <- par(mar = c(3, 4, 1, 1) + 0.1)
  on.exit(par(op))
  p <- DurgaPlot(d, ef.size = FALSE, ylim = c(-10, 290))
  labels <- sapply(d$group.differences, function(d) sprintf("%s - %s", d$groups[1], d$groups[2]))
  DurgaBrackets(p, labels = labels, lb.cex = 0.6, text.pad = 1, vertical.gap = 0.5, tip.length = 1, snap.to = 4, br.col = "blue", lb.col = "purple")
  expect_equal(1, 1)
})

test_that("one group", {
  n <- 40
  df <- data.frame(val = rnorm(n, 10),
                   group = rep("G1", each = n))
  d <- DurgaDiff(df, data.col = 1, group.col = 2)
  p <- expect_error(DurgaPlot(d, main = "1 group in data - no bracket"), NA)
  expect_error(DurgaBrackets(p), NA)
})

test_that("contrasts", {
  d <- DurgaDiff(damselfly, 1, 3)
  p <- expect_error(DurgaPlot(d, main = "Bracket sign consistent", ef.size = FALSE, ylim = c(29.5, 36.5)), NA)
  DurgaBrackets(p)
  DurgaBrackets(p, contrasts = "adult - juvenile", data.gap = 10)
  DurgaBrackets(p, contrasts = "juvenile - adult", data.gap = 17)
})

test_that("round.fn", {
  d <- DurgaDiff(damselfly, 1, 3)
  p <- DurgaPlot(d, main = "Rounding of numbers", ef.size = FALSE, ylim = c(29.5, 36.5))
  DurgaBrackets(p)
  expect_error(DurgaBrackets(p, data.gap = 10, round.fn = round), NA)
})

test_that("default symbology", {
  op <- par(mfrow = c(2, 1))
  on.exit(par(op))
  set.seed(1)
  contrasts <- c("self_fertilised - inter_cross, inter_cross- westerham_cross, self_fertilised - westerham_cross")
  d <- DurgaDiff(petunia, 1, 2, contrasts = contrasts)
  p <- DurgaPlot(d, main = "Default symbology", ef.size = FALSE, bty = "n", ylim = extendrange(petunia$height, f = c(0, 0.5)))
  expect_error(DurgaBrackets(p), NA)

  set.seed(1)
  contrasts <- c("inter_cross - self_fertilised, westerham_cross - inter_cross, westerham_cross - self_fertilised")
  d <- DurgaDiff(petunia, 1, 2, contrasts = contrasts)
  p <- DurgaPlot(d, main = "Default symbology", ef.size = FALSE, bty = "n", ylim = extendrange(petunia$height, f = c(0, 0.5)))
  expect_error(DurgaBrackets(p), NA)
})

test_that("bracket overlap", {
  n <- 10
  ng <- 7
  set.seed(1)
  data <- data.frame(val = c(replicate(ng, rnorm(n, runif(1)))),
                     group = rep(1:ng, each = n))
  d <- DurgaDiff(data, "val", "group")
  p <- DurgaPlot(d, main = "No overlapping brackets", ef.size = FALSE, bty = "n", ylim = extendrange(data$val, f = c(0, 1)))
  expect_error(DurgaBrackets(p, contrasts = "*", labels = "diff", lb.cex = 0.6, tip.length = 1, vertical.gap = 0.6), NA)
})
