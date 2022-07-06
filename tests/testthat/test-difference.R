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

makePairedData <- function(addSomeNAs = FALSE) {
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
  difference(data[data$Group %in% c("ZControl1", "Group1"),],
             na.rm = addSomeNAs,
             data.col = "Measurement", group.col = "Group", R = 1000,
             effect.type = "paired", id.col = "ID")
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
#   plotES(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = FALSE, box_fill = FALSE,
#          central_tendency = "median", error_bars = "CI", ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5))
#
#   #b)
#   plotES(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = "white", box_fill = "white",
#          central_tendency = "median", error_bars = "SD", ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5),)
#
#   #c)
#   plotES(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = "white", box_fill = "white",
#          central_tendency = "median", error_bars = "SE", ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5),)
#
#   #d)
#   plotES(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = "white", box_fill = "white",
#          central_tendency = "mean", error_bars = "CI", ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5),)
#
#   #e)
#   plotES(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = "white", box_fill = "white",
#          central_tendency = "mean", error_bars = "SD", ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5),)
#
#   #f)
#   plotES(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = "white", box_fill = "white",
#          central_tendency = "mean", error_bars = "SE", ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5),)
#
#   #g)
#   plotES(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = transparent(c("red", "blue"), .5),
#          box_fill = "white",error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = FALSE)
#
#   #h)
#   plotES(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = transparent(c("red", "blue"), .5),
#          box_fill = "white",error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5))
#
#   #i)
#   plotES(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = transparent(c("red", "blue"), .5),
#          box_fill = transparent(c("red", "blue"), .7),error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = FALSE)
#
#   #j)
#   plotES(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = transparent(c("red", "blue"), .5),
#          box_fill = transparent(c("red", "blue"), .7),error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5))
#
#
#   #k)
#   plotES(es, bar = FALSE, bar_fill = FALSE, violin = "left-half",
#          violin_border = transparent(c("red", "blue"), .6),
#          violin_fill = transparent(c("red", "blue"), .6),
#          box = "white",
#          box_fill = "white",
#          error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5))
#
#   #l)
#   plotES(es, bar = FALSE, bar_fill = FALSE, violin = "left-half",
#          violin_border = transparent(c("red", "blue"), .6),
#          violin_fill = transparent(c("red", "blue"), .6),
#          box = transparent(c("red", "blue"), .5),
#          box_fill = transparent(c("red", "blue"), .7),error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = FALSE)
#
#   ##m)
#   plotES(es, bar = FALSE, bar_fill = FALSE, violin = "left-half",
#          violin_border = transparent(c("red", "blue"), .6),
#          violin_fill = transparent(c("red", "blue"), .6),
#          box = transparent(c("red", "blue"), .5),
#          box_fill = transparent(c("red", "blue"), .7),error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5))
#
#   #n)
#   plotES(es, bar = FALSE, bar_fill = FALSE, violin = "right-half",
#          violin_border = transparent(c("red", "blue"), .6),
#          violin_fill = transparent(c("red", "blue"), .6),
#          box = "white",
#          box_fill = "white",
#          error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5))
#   #o)
#   plotES(es, bar = FALSE, bar_fill = FALSE, violin = "right-half",
#          violin_border = transparent(c("red", "blue"), .6),
#          violin_fill = transparent(c("red", "blue"), .6),
#          box = transparent(c("red", "blue"), .5),
#          box_fill = transparent(c("red", "blue"), .7),error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = FALSE)
#
#   #p)
#   plotES(es, bar = FALSE, bar_fill = FALSE, violin = "right-half",
#          violin_border = transparent(c("red", "blue"), .6),
#          violin_fill = transparent(c("red", "blue"), .6),
#          box = transparent(c("red", "blue"), .5),
#          box_fill = transparent(c("red", "blue"), .7),error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5))
#
#   #q)
#   plotES(es, bar = FALSE, bar_fill = FALSE, violin = "full",
#          violin_border = transparent(c("red", "blue"), .6),
#          violin_fill = transparent(c("red", "blue"), .6),
#          box = transparent(c("red", "blue"), .5),
#          box_fill = transparent(c("red", "blue"), .7),error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = FALSE)
#
#   #r)
#   plotES(es2, bar = FALSE, bar_fill = FALSE, violin = "full",
#          violin_border = transparent(c("red", "blue"), .4),
#          violin_fill = transparent(c("red", "blue"), .8),
#          box = transparent(c("grey10"), .1),
#          box_fill = transparent(c("red", "blue"), .7),error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = TRUE,
#          points = transparent(c("red", "blue"), .5), paired = TRUE)
#
#   #s)
#   plotES(es, bar = FALSE, bar_fill = FALSE, violin = "full",
#          violin_border = transparent(c("red", "blue"), .6),
#          violin_fill = transparent(c("red", "blue"), .6),
#          box = "white",
#          box_fill = "white",
#          error_bars = "CI",
#          central_tendency = "mean", mean = NA, ef_size = FALSE,
#          points = transparent(c("red", "blue"), .5))
#
#   #t)
#   plotES(es, bar = FALSE, bar_fill = FALSE, violin = "full",
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
  plotES(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = FALSE, box_fill = FALSE,
         central_tendency = "median", error_bars = "CI", ef_size = FALSE,
         points = transparent(c("red", "blue"), .5))
  expect_equal(1, 1)
})

test_that("central tendency FALSE works", {
  es <- makeData1()
  plotES(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = "red", box_fill = "blue",
         central_tendency = FALSE, error_bars = "CI", ef_size = FALSE,
         points = transparent(c("red", "blue"), .5))
  expect_equal(1, 1)
})

test_that("paired works", {
  es <- makePairedData()
  plotES(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = "red", box_fill = "blue",
         central_tendency = FALSE, error_bars = "CI", ef_size = FALSE,
         points = transparent(c("red", "blue"), .5))
  expect_equal(1, 1)
})

test_that("paired with NAs works", {
  es <- makePairedData(addSomeNAs = TRUE)
  plotES(es, bar = FALSE, bar_fill = FALSE, violin = FALSE, box = "red", box_fill = "blue",
         central_tendency = FALSE, error_bars = "CI", ef_size = FALSE,
         points = transparent(c("red", "blue"), .5))
  expect_equal(1, 1)
})

test_that("bar charts work", {
  es <- makeData1()
  plotES(es, bar = TRUE, violin = FALSE, box = FALSE, box_fill = "blue",
         central_tendency = FALSE, error_bars = "CI", ef_size = FALSE,
         points = FALSE)
  expect_equal(1, 1)
})
