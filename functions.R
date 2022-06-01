### Private functions

#### TODO handle NA in data !!!
#### Multiple groups - specify pairings
#### Scale of cohens d - scale to fit (-1, 1), limit axis extents (Hegdes same)

transparent <-  function(colour, alpha) {
  rgba.val <- col2rgb(colour, TRUE)
  t.col <- rgb(rgba.val[1, ], rgba.val[2, ], rgba.val[3, ],
               maxColorValue = 255,
               alpha = (100 - alpha * 100) * 255 / 100)
  invisible(t.col)
}


# Two group statistic functions

# Mean difference (group 2 - group 1)
stMeanDiff <- function(x1, x2) { mean(x2) - mean(x1) }

# Cohens d (group 2 - group 1)
stCohensD <- function(x1, x2){
  m1 <- mean(x1) 
  SD1 <- sd(x1)
  N1 <- length(x1)
  m2 <- mean(x2)
  SD2 <- sd(x2)
  N2 <- length(x2)
  x <- sqrt((N1 - 1) * (SD1 * SD1) * (N2 - 1) * (SD2 * SD2) / (N1 + N2 - 2))
  (m2 - m1) / x
}

#############################################################################

#' Generate differences
#' 
#' This is details
#' 
#' @param data A data frame...
#' @param effect.type Type of difference
#' 
#' @return List containing:
#' bootstrapped mean difference + confidence interval of mean difference + individual bootstrapped difference + 
#'         CI of group 1 + CI of group 2 + mean of group 1 + mean of group 2 + median of group 1 + median of group 2 +
#'         raw data
#'         
#' @export
difference <- function(data,
                       effect.type = c("unstandardised", "cohens", "hegens", "paired-diffs"),
                       paired = FALSE, # if true calculate paired mean difference 
                       data.col = 1, group.col = 2, block.col = NULL, id.col,
                       R = 1000,
                       ci.type = "bca",
                       ...        
                       # ci.type = "bca", #default
                       # contrast = c("group 1 - group 2"), # default larger group minus small group
                       # ci.conf = 0.95,
) {
  
  call <- match.call()
  if (!is.function(effect.type))
    effect.type <- match.arg(effect.type)
  
  # Get list of groups
  groups <- unique(data[[group.col]])

  # Function to simplify writing bootstrap statistic functions  
  .wrap2GroupStatistic <- function(statisticFn) {
    if (length(groups) != 2)
      stop(sprintf("Require exactly 2 groups for effect type %s, found %d (%s)", effect.type, length(groups), paste(groups, collapse = ", ")))
    function(data, indices) {
      measurement <- data[indices, data.col]
      group <- data[indices, group.col]
      x1 <- measurement[group == groups[1]]
      x2 <- measurement[group == groups[2]]
      statisticFn(x1, x2)
    }
  }

  # Decide how to calculate the statistic
  bootstrapData <- data
  if (effect.type == "unstandardised") {
    statistic <- .wrap2GroupStatistic(stMeanDiff)
  } else if (effect.type == "cohens") {
    statistic <- .wrap2GroupStatistic(stCohensD)
  } else if (effect.type == "hegens") {
    statistic <- .wrap2GroupStatistic(stHegens)
  } else if (effect.type == "paired-diffs") {
    if (missing(id.col)) stop("id.col must be specified when effect.type = 'paired-diffs'")
    g1 <- data[data[[group.col]] == groups[1], ]
    g2 <- data[data[[group.col]] == groups[2], ]
    # Pair on ID (don't assume they are sorted)
    g2Idx <- match(g1[[id.col]], g2[[id.col]])
    bootstrapData <- g2[[data.col]][g2Idx] - g1[[data.col]]
    statistic <- function(data, subset) {
      mean(data[subset])
    }
  } else {
    # User supplied a bootstrap statistic function
    statistic <- effect.type
  }
  
  # Bootstrap the statistic
  es <- boot::boot(bootstrapData, statistic, R = R, ...)
  
  # Calculate confidence interval
  ci <- boot::boot.ci(es, type = ci.type)
  
  # Save some parts of the CI data into the returned value (don't need values that are already there or misleading)
  es[["ci.type"]] <- ci.type
  for (cin in names(ci)) {
    if (!cin %in% c("R", "t0", "call")) {
      es[[cin]] <- ci[[cin]]
    }
  }
  # Overwrite data in result with the real data (rather than bootstrapped data, e.g. for paired-riffs)
  es$data <- data
  # Add some more stuff to the result
  es$call <- match.call()
  es$data.col <- data.col
  es$group.col <- group.col
  es$groups <- groups
  es$effect.type <- effect.type
  
  # Fill in extra information in the returned list  
  gil <- lapply(groups, function(g) { 
    grpVals <- data[[data.col]][data[[group.col]] == g]
    c(mean = mean(grpVals),
      median = median(grpVals),
      sd = sd(grpVals),
      se = sd(grpVals) / sqrt(length(grpVals))
    )
  })
  df <- do.call(rbind, gil)
  rownames(df) <- groups
  es$groupStatistics <- df

  # Return value has type plotES TODO decide on a name
  class(es) <- c("plotES", class(es))
  
  es
}

#' print bootstrap mean difference, bootstrapped confidence interval (R value, bootstrapped corrections"bca")
#' 
#' @export
print.plotES <- function(result, ...) {
  cat("Bootstrapped effect size\n")
  cat("Groups:\n")
  print(es$groupStatistics)
  cat(sprintf("%s effect size %g, %g%% CI (%s) %g, %g\n",
              es$effect.type, es$t0, es$bca[1], es$ci.type, es$bca[4], es$bca[5]))
}


#' Plot data with effect size
#' 
#' @param box Colour of boxplot. If FALSE or NA, boxplot is not drawn
#' @param box_fill Colour used to fill the bodies of the boxplot. If FALSE or NA, bodies are not filled
#' @param points Vector of colours used to draw data points, one colour per group. If FALSE or NA, points are not drawn
#' @param violin What type of violin plot to display. If FALSE, violin plot is not drawn
#' @param adj Value used to control violin plot smoothness.
#' 
#' 
#' @export
plotES <- function(es, 
                   box = "black", 
                   box_fill = "lightgrey", 
                   points = RColorBrewer::brewer.pal(max(3, length(es$groups)), "Set1"),
                   violin = c("left-half", "right-half", "full"),
                   violin_border = RColorBrewer::brewer.pal(max(3, length(es$groups)), "Set2"),
                   violin_fill = transparent(RColorBrewer::brewer.pal(max(3, length(es$groups)), "Set2"), .8),
                   violin_adj = 1.5,
                   violin_width = 0.35,
                   violin_trunc_at = 0.05,
                   
                   # box = TRUE,# draw boxplot 
                   # box_fill = TRUE,# fill up box colour # if false only border will be drawn 
                   # points = TRUE, #add individual data point 
                   mean = TRUE, #draw mean of the data 
                   CI = TRUE, # draw confidence interval line of the data; if, box, density, violin is TRUE, CI is FALSE
                   median_line = FALSE, # if TRUE horizontal line in median is drawn # if, box, density, violin is TRUE, median_line is FALSE
                   ef_size = TRUE, # if false do not plot effect size
                   ef_size_density = TRUE, #if true draw effect size confidence interval
                   ef_size_position = c("right", "down"),# when Gardner-Altman_plot is chosen effect size plotted right, otherwise down 
                   paired = FALSE, # if true draw lines between paired points
                   xlab = "",
                   left_ylab = "",
                   right_ylab = "",
                   bottom_ylab = "",
                   col = c("col1", "col2", "col3"), opacity = 0.6, #colour of box, violin, box border, density, col 1 = group 1, col2 = group 2, col3 = ef plot, {col = n+1, n = group no) #   
                   points_col = c("col1", "col2", "col3"), points_opacity = 0.4, # points colour 
                   ...
) {
  if (!is(es, "plotES")) 
    stop("data must be a plotES object")
  if (!isFALSE(violin))
    violin <- match.arg(violin)
  
  .show <- function(what) !isFALSE(what) && !is.null(what)
  .isColour <- function(c) tryCatch(is.matrix(col2rgb(c)), error = function(e) FALSE)
  .colour <- function(what) if (isTRUE(what)) { "black" } else if (.isColour(what)) { what } else { NA }

  
  data <- es$data
  groups <- es$groups
  
  # Calculate densities for violin plots
  densities <- lapply(groups, function(g) density(data[[es$data.col]][data[[es$group.col]] == g], adj = violin_adj))
  # Normalise densities heights so they all have desired height (which becomes width)
  densities <- lapply(densities, function(d) { d$y <- d$y / max(d$y) * violin_width; d })
  # Optionally chop off the tails
  if (violin_trunc_at > 0)
    densities <- lapply(densities, function(d) { 
      keep <- which(d$y > violin_trunc_at * violin_width)
      d$y <- c(0, d$y[keep], 0)
      d$x <- c(d$x[keep[1]], d$x[keep], d$x[keep[length(keep)]])
      d
      })
  
  # Calculate plot limits
  xlim <- c(0.5, length(groups) + 0.5)
  ylim <- range(data[[es$data.col]])

  # If needed, extend y range to encompass violin plots
  if (.show(violin)) {
    for (d in densities) {
      ylim <- range(ylim, d$x)
    }
  }
  
  # If needed extend x range to encompass effect size
  if (.show(ef_size) && length(groups) == 2) {
    xlim[2] <- xlim[2] + 0.8
  }
  
  # Extend width if showing effect size on right
  
  # Prepare plot
  plot(NULL, xlim = xlim, ylim = ylim, type = "n", xaxt = "n", yaxt = "n", xlab = es$group.col, ylab = es$data.col, ...)
  
  # Add the various components to the plot
  f <- as.formula(paste(es$data.col, "~", es$group.col))
  
  # Box plot
  if (.show(box)) {
    boxplot(f, data = data, add = TRUE, 
            col = .colour(box_fill), border = .colour(box))
  }
  # Violin plots
  if (.show(violin)) {
    for (i in seq_along(groups)) {
      d <- densities[[i]]
      col <- violin_fill[i]
      border <- violin_border[i]
      if (violin == "left-half") {
        polygon(i - d$y, d$x, col = col, border = border)
      } else if (violin == "right-half") {
        polygon(i + d$y, d$x, col = col, border = border)
      } else {
        polygon(c(i - d$y, rev(i + d$y)), c(d$x, rev(d$x)), col = col, border = border)
      }
    }
  }
  # Scatter plot of data points
  if (.show(points)) {
    stripchart(f, data = data, method = "jitter",
               pch = 19, vertical = TRUE,
               col = .colour(points),
               add = TRUE)
  }
  
  if (.show(ef_size)) {
    # If there are 2 groups..
    if (length(groups) == 2) {
      # Show it to the right
      d <- density(es$t)
      d$y <- d$y / max(d$y) * .2
      # Diff is group2 - group1, so add to group1 mean make it align with group1
      y <- es$groupStatistics[1, 1]
      polygon(d$y + 3.0, d$x + y, 
              col = transparent("black", .8), 
              border = "black")
    }
  } 
    
  
  es
}

###
# Testing

N <- 40
data <- data.frame(Measurement = c(rnorm(N, mean = 100, sd = 25),
                                   rnorm(N, mean = 100, sd = 50),
                                   rnorm(N, mean = 120, sd = 25),
                                   rnorm(N, mean = 80, sd = 50),
                                   rnorm(N, mean = 100, sd = 12),
                                   rnorm(N, mean = 100, sd = 50)),
                   Group = c(rep("Control1", N),
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

par(mfrow = c(2, 1))
es <- difference(data[data$Group %in% c("Control1", "Group1"),], data.col = "Measurement", group.col = "Group", R = 1000)
print(es)
plotES(es, points = transparent(c("red", "blue"), .9), violin = "full")

es <- difference(data[data$Group %in% c("Control1", "Group1"),], effect.type = "paired-diffs", id.col = "ID", data.col = "Measurement", group.col = "Group", R = 1000)
print(es)
plotES(es, points = transparent(c("red", "blue"), .9), violin = "full")


# library(dabestr)
# 
# two.group.unpaired <- dabest(data, Group, Measurement, 
#                              # The idx below passes "Control" as the control group, 
#                              # and "Group1" as the test group. The mean difference
#                              # will be computed as mean(Group1) - mean(Control1).
#                              idx = c("Control1", "Group1"), 
#                              paired = FALSE)
# md <- mean_diff(two.group.unpaired)
# plot(md)
# cd <- cohens_d(two.group.unpaired)
# plot(cd)
