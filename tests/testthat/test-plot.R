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

##########################################################################
# Tests start here ####


test_that("ef.size.cex", {
  mar <- par("mar")
  op <- par(mfrow = c(1, 2))
  on.exit(par(op))

  # Plots need to be manually checked
  expect_error(DurgaDiff(mass ~ maturity, damselfly) |> DurgaPlot(ef.size.cex = 0.8, ef.size.pch = 8, ef.size = 2, main = "Small ef.size.cex"), NA)
  expect_error(DurgaDiff(height ~ group, petunia) |> DurgaPlot(ef.size.cex = c(0.8, 2), ef.size.pch = 24, ef.size = 2, main = "Variable ef.size.cex"), NA)
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

test_that("custom labels", {
  data <- makeData()
  d <- DurgaDiff(data, 1, 2, groups = c("ZControl1", "Group1", "Group2"))

  # Override effect size label
  d$group.differences[[2]]$label.plot <- expression(italic("Sp. 1") ~ "-" ~ italic("Sp. 2"))
  # Override effect name label
  expect_error(DurgaPlot(d, ef.size.label = expression(bold("Bold name")), x.axis = F), NA)

  # Override print label
  d$group.differences[[2]]$label.print <- "Sp. 1 : Sp. 2"
  s <- capture.output(print(d))
  expect_match(s[10], "^ *Sp. 1 : Sp. 2:")
})

test_that("ef range plot bug", {
  n <- c(20, 30, 50, 30)
  seasons <- c("Spring", "Summer", "Autumn", "Winter")
  df <- data.frame(proportion = c(
    rnorm(n[1], mean = 0.1, sd = 0.01),
    rnorm(n[2], mean = 0.15, sd = 0.015),
    rnorm(n[3], mean = 0.25, sd = 0.02),
    rnorm(n[4], mean = 0.6, sd = 0.03)
  ), season = rep(seasons, n)
  )
  d <- DurgaDiff(df, 1, 2, contrasts = ". - Winter", groups = seasons)
  expect_error(DurgaPlot(d, main = "Effect size ylim correct"), NA)
})

test_that("ef below layout", {
  data <- makeData()
  d <- DurgaDiff(data, 1, 2, groups = c("ZControl1", "Group1", "Group2"))
  op <- par(mfrow = c(1, 2), cex = 0.7)
  on.exit(par(op))
  expect_error(DurgaPlot(d, main = "Default EF layout"), NA)
  expect_error(DurgaPlot(d, ef.size.top.pad = 1.5, ef.size.height = 0.6, main = "Smaller gap, larger height"), NA)
})

test_that("ef size ticks", {
  data <- makeData()
  op <- par(mfrow = c(2, 2), mar = c(5, 4, 4, 1) + 0.1)
  on.exit(par(op))

  d <- DurgaDiff(data, 1, 2, groups = c("ZControl1", "Group1", "Group2"))
  DurgaPlot(d, main = "Custom ef ticks", ef.size.ticks = c(30, 0, -50))
  DurgaPlot(d, contrasts = "Group2 - ZControl1", main = "Custom ef ticks", ef.size.ticks = c(30, 0, -50))

  d <- DurgaDiff(data, 1, 2, groups = c("ZControl1", "Group1", "Group2"), effect.type = "cohens d")
  DurgaPlot(d, main = "Custom ef ticks", ef.size.ticks = c(-2, 0, 1))
  expect_error(DurgaPlot(d, contrasts = "Group2 - ZControl1", main = "Custom ef ticks", ef.size.ticks = c(-2, 0, 1)), NA)
})

test_that("ef size labels", {
  data <- makeData()
  op <- par(mfrow = c(2, 2))
  on.exit(par(op))

  d <- DurgaDiff(data, 1, 2, groups = c("ZControl1", "Group1", "Group2"))
  DurgaPlot(d, main = "Custom ef labels", ef.size.ticks = c("Big" = 30, "None" = 0, "Huge" = -50), ef.size.params = list(las = 1))
  DurgaPlot(d, contrasts = "Group2 - ZControl1", main = "Custom ef labels", ef.size.ticks = c("Big" = 30, "None" = 0, "Huge" = -50), ef.size.params = list(las = 1))

  d <- DurgaDiff(data, 1, 2, groups = c("ZControl1", "Group1", "Group2"), effect.type = "cohens d")
  DurgaPlot(d, main = "Custom ef labels", ef.size.ticks = c("Huge" = -2, "None" = 0, "Big" = 1), ef.size.params = list(las = 1))
  expect_error(DurgaPlot(d, contrasts = "Group2 - ZControl1", main = "Custom ef labels", ef.size.ticks = c("Huge" = -2, "None" = 0, "Big" = 1), ef.size.params = list(las = 1)), NA)
})

test_that("ef size symbology", {
  data <- makeData()
  op <- par(mfrow = c(1, 2), mar = c(5, 4, 4, 1) + 0.1)
  on.exit(par(op))

  d <- DurgaDiff(data, 1, 2, groups = c(Control = "ZControl1", "Group1", "Group2"))
  expect_error(DurgaPlot(d, main = "Custom ef symbology", ef.size = 2:3, ef.size.lty = 3:2, ef.size.lwd = c(4, 2)), NA)
  d <- DurgaDiff(data, 1, 2, groups = c(Control = "ZControl1", "Group1"), effect.type = "cohens d")
  expect_error(DurgaPlot(d, main = "Custom ef symbology", ef.size.lty = 2, ef.size.lwd = 4), NA)
})

