makeData1 <- function() {
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
  difference(data[data$Group %in% c("ZControl1", "Group1"),], data.col = "Measurement", group.col = "Group", R = 1000)
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
                     Gender = rep(c(rep('Male', N/2), rep('Female', N/2)), 6),
                     ID = rep(1:N, 6)
  )
  if (addSomeNAs)
    data[c(1, 4, 10, 65), 1] <- NA

  # Shuffle
  data <- data[sample(nrow(data)), ]
  if (reverseGroups) {
    difference(data[data$Group %in% c("ZControl1", "Group1"),],
               groups = c("ZControl1", "Group1"),
               na.rm = addSomeNAs,
               data.col = "Measurement", group.col = "Group", R = 1000,
               effect.type = "paired", id.col = "ID")
  } else {
    difference(data[data$Group %in% c("ZControl1", "Group1"),],
               na.rm = addSomeNAs,
               data.col = "Measurement", group.col = "Group", R = 1000,
               effect.type = "paired", id.col = "ID")
  }
}

##########################################################################
# Tests start here ####


test_that("difference effect types", {
  n <- 100
  df <- data.frame(val = c(rnorm(n), rnorm(n, mean = 1)),
                   group = c(rep("Control", n), rep("Group", n)),
                   id = c(1:n, 1:n))

  # Check all effect types
  expect_error(difference(df, effect.type = "unstandardised"), NA)
  expect_error(difference(df, effect.type = "cohens"), NA)
  expect_error(difference(df, effect.type = "hedges"), NA)
  expect_error(difference(df, effect.type = "paired",id.col = "id"), NA)

})

test_that("group factors", {
  n <- 100
  df <- data.frame(val = c(rnorm(n), rnorm(n, mean = 1)),
                   group = factor(c(rep("Control", n), rep("Group", n))),
                   id = c(1:n, 1:n))

  # Check all effect types
  expect_error(difference(df, effect.type = "unstandardised"), NA)
  expect_error(difference(df, effect.type = "cohens"), NA)
  expect_error(difference(df, effect.type = "hedges"), NA)
  expect_error(difference(df, effect.type = "paired",id.col = "id"), NA)

})

test_that("difference handles NA", {
  n <- 100
  df <- data.frame(val = c(rnorm(n), rnorm(n, mean = 1)),
                   group = c(rep("Control", n), rep("Group", n)))
  df[c(1, 4, 10, 65), 1] <- NA

  # This should throw an error
  expect_error(difference(df, na.rm = FALSE))
  # This should NOT throw an error
  expect_error(difference(df, na.rm = TRUE), NA)
  # This should throw an error if one of a pair is missing
  expect_error(difference(df, effect.type = "paired", id.col = "id", na.rm = TRUE))
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

  d3 <- difference(df)
  # This should NOT throw an error
  expect_error(SAKPlot(d3, bar = FALSE, box = FALSE), NA)
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

  d3 <- difference(df)
  # This should NOT throw an error
  expect_error(SAKPlot(d3, bar = FALSE, box = FALSE), NA)
})

# test_that("plots work", {
#   N <- 40
#   set.seed(1)
#   data <- data.frame(Measurement = c(rnorm(N, mean = 100, sd = 25),
#                                      rnorm(N, mean = 100, sd = 50),
#                                      rnorm(N, mean = 120, sd = 25),
#                                      rnorm(N, mean = 80, sd = 50),
#                                      rnorm(N, mean = 100, sd = 12),
#                                      rnorm(N, mean = 100, sd = 50)),
#                      Group = c(rep("ZControl1", N),
#                                rep("Control2", N),
#                                rep("Group1", N),
#                                rep("Group2", N),
#                                rep("Group3", N),
#                                rep("Group4", N)),
#                      Gender = rep(c(rep('Male', N/2), rep('Female', N/2)), 6),
#                      ID = rep(1:N, 6)
#   )
#   # Shuffle
#   data <- data[sample(nrow(data)), ]
#   es <- difference(data[data$Group %in% c("ZControl1", "Group1"),], data.col = "Measurement", group.col = "Group", R = 1000)
#   es2 <- difference(data[data$Group %in% c("ZControl1", "Group1"),],
#                     data.col = "Measurement", group.col = "Group", R = 1000,
#                     effect.type = "paired", id.col = "ID")
#
#
#   # Generate some plots
#   l <- layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), nrow=4,ncol=5,byrow =  TRUE))
#   #layout.show(l)
#
#   #par(mfrow = c(2, 4))
#   #a)
#   SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = FALSE, box_fill = FALSE,
#          central_tendency = "median", error_bars = "CI", ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5))
#
#   #b)
#   SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = "white", box_fill = "white",
#          central_tendency = "median", error_bars = "SD", ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5),)
#
#   #c)
#   SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = "white", box_fill = "white",
#          central_tendency = "median", error_bars = "SE", ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5),)
#
#   #d)
#   SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = "white", box_fill = "white",
#          central_tendency = "mean", error_bars = "CI", ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5),)
#
#   #e)
#   SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = "white", box_fill = "white",
#          central_tendency = "mean", error_bars = "SD", ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5),)
#
#   #f)
#   SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = "white", box_fill = "white",
#          central_tendency = "mean", error_bars = "SE", ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5),)
#
#   #g)
#   SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = transparent(c("red", "blue"), .5),
#          box_fill = "white",error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = FALSE)
#
#   #h)
#   SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = transparent(c("red", "blue"), .5),
#          box_fill = "white",error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5))
#
#   #i)
#   SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = transparent(c("red", "blue"), .5),
#          box_fill = transparent(c("red", "blue"), .7),error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = FALSE)
#
#   #j)
#   SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = transparent(c("red", "blue"), .5),
#          box_fill = transparent(c("red", "blue"), .7),error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5))
#
#
#   #k)
#   SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = "left-half",
#          violin_border = transparent(c("red", "blue"), .6),
#          violin_fill = transparent(c("red", "blue"), .6),
#          box = "white",
#          box_fill = "white",
#          error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5))
#
#   #l)
#   SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = "left-half",
#          violin_border = transparent(c("red", "blue"), .6),
#          violin_fill = transparent(c("red", "blue"), .6),
#          box = transparent(c("red", "blue"), .5),
#          box_fill = transparent(c("red", "blue"), .7),error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = FALSE)
#
#   ##m)
#   SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = "left-half",
#          violin_border = transparent(c("red", "blue"), .6),
#          violin_fill = transparent(c("red", "blue"), .6),
#          box = transparent(c("red", "blue"), .5),
#          box_fill = transparent(c("red", "blue"), .7),error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5))
#
#   #n)
#   SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = "right-half",
#          violin_border = transparent(c("red", "blue"), .6),
#          violin_fill = transparent(c("red", "blue"), .6),
#          box = "white",
#          box_fill = "white",
#          error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5))
#   #o)
#   SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = "right-half",
#          violin_border = transparent(c("red", "blue"), .6),
#          violin_fill = transparent(c("red", "blue"), .6),
#          box = transparent(c("red", "blue"), .5),
#          box_fill = transparent(c("red", "blue"), .7),error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = FALSE)
#
#   #p)
#   SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = "right-half",
#          violin_border = transparent(c("red", "blue"), .6),
#          violin_fill = transparent(c("red", "blue"), .6),
#          box = transparent(c("red", "blue"), .5),
#          box_fill = transparent(c("red", "blue"), .7),error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5))
#
#   #q)
#   SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = "full",
#          violin_border = transparent(c("red", "blue"), .6),
#          violin_fill = transparent(c("red", "blue"), .6),
#          box = transparent(c("red", "blue"), .5),
#          box_fill = transparent(c("red", "blue"), .7),error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = FALSE)
#
#   #r)
#   SAKPlot(es2, bar = FALSE, bar_fill = FALSE, violin = "full",
#          violin_border = transparent(c("red", "blue"), .4),
#          violin_fill = transparent(c("red", "blue"), .8),
#          box = transparent(c("grey10"), .1),
#          box_fill = transparent(c("red", "blue"), .7),error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = TRUE,
#          points = transparent(c("red", "blue"), .5), paired = TRUE)
#
#   #s)
#   SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = "full",
#          violin_border = transparent(c("red", "blue"), .6),
#          violin_fill = transparent(c("red", "blue"), .6),
#          box = "white",
#          box_fill = "white",
#          error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5))
#
#   #t)
#   SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = "full",
#          violin_border = transparent(c("red", "blue"), .6),
#          violin_fill = transparent(c("red", "blue"), .6),
#          box = "white",
#          box_fill = "white",
#          error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = FALSE)
#
#   expect_equal(1, 1)
# })

test_that("box FALSE works", {
  es <- makeData1()
  SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = FALSE, box_fill = FALSE,
         central_tendency = "median", error_bars = "CI", ef_size = FALSE,
         points = transparent(c("red", "blue"), .5))
  expect_equal(1, 1)
})

test_that("central tendency FALSE works", {
  es <- makeData1()
  SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = "red", box_fill = "blue",
         central_tendency = FALSE, error_bars = "CI", ef_size = FALSE,
         points = transparent(c("red", "blue"), .5))
  expect_equal(1, 1)
})

test_that("paired works", {
  es <- makePairedData()
  SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = "red", box_fill = "blue",
         central_tendency = FALSE, error_bars = "CI", ef_size = FALSE,
         points = transparent(c("red", "blue"), .5))
  expect_equal(1, 1)
})

test_that("paired (reversed groups) works", {
  es <- makePairedData(reverseGroups = TRUE)
  SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = "red", box_fill = "blue",
         central_tendency = FALSE, error_bars = "CI", ef_size = FALSE,
         points = transparent(c("red", "blue"), .5))
  expect_equal(1, 1)
})

test_that("paired with NAs works", {
  es <- makePairedData(addSomeNAs = TRUE)
  SAKPlot(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = "red", box_fill = "blue",
         central_tendency = FALSE, error_bars = "CI", ef_size = FALSE,
         points = transparent(c("red", "blue"), .5))
  expect_equal(1, 1)
})

test_that("bar charts work", {
  es <- makeData1()
  SAKPlot(es, bar = TRUE, violin = FALSE, box = FALSE, box_fill = "blue",
         central_tendency = FALSE, error_bars = "CI", ef_size = FALSE,
         points = FALSE)
  expect_equal(1, 1)
})
