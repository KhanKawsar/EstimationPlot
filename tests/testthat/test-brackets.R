
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
  # The example from the doco
  d <- DurgaDiff(petunia, 1, 2)
  # Don't draw frame because brackets will appear in the upper margin
  ps <- DurgaPlot(d, ef.size = FALSE, frame.plot = FALSE)
  expect_error(DurgaBrackets(ps, lb.cex = 0.8, br.lwd = 2, snapTo = 2), NA)
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
                  main = "Differences, snapTo 10")
  expect_error(DurgaBrackets(ps, diffs, labels = "difference",
                             lb.cex = 0.8, lb.font = 1,
                             snapTo = 10,
                             br.lwd = 2, br.col = col), NA)
  ps <- DurgaPlot(d, contrasts = diffs, box = T, box.outline = F, points = T, ylim = c(-20, 340), frame.plot = F, ef.size = F,
                  main = "CI, no snapTo")
  expect_error(DurgaBrackets(ps, diffs, labels = "CI",
                             lb.cex = 0.8, lb.font = 1,
                             snapTo = 0,
                             br.lwd = 2, br.col = col), NA)


  # Just show the long brackets
  d <- DurgaDiff(data, "Measurement", "Group", groups = groups, effect.type = "cohen")
  diffs <- Filter(function(pwes) (pwes$bca[4] > 0 || pwes$bca[5] < 0), d$group.differences)
  col <- sapply(diffs, function(diff) ifelse(diff$t0 < 0, "#f06040", "#7090f0"))
  diffs <- Filter(function(pwes) (abs(diff(pwes$groupIndices)) > 2), d$group.differences)
  col <- sapply(diffs, function(diff) ifelse(diff$t0 < 0, "#f06040", "#7090f0"))

  ps <- DurgaPlot(d, box = T, box.outline = F, points = T, ylim = c(-20, 270), frame.plot = F, ef.size = F,
                  main = "CI-long, big red labels")
  expect_error(DurgaBrackets(ps, diffs, labels = "CI-long",
                             br.lwd = 2, br.col = col, lb.col = "red"), NA)

  ps <- DurgaPlot(d, box = T, box.outline = F, points = T, ylim = c(-20, 270), frame.plot = F, ef.size = F,
                  main = "CI-diff, bold labels")
  expect_error(DurgaBrackets(ps, diffs, labels = "CI-diff",
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
  DurgaBrackets(p, labels = labels, lb.cex = 0.6, textPad = 1, verticalGap = 0.5, tipLength = 1, snapTo = 4, br.col = "blue", lb.col = "purple")
  expect_equal(1, 1)
})
