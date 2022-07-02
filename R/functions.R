### Private functions

#### TODO
#### Multiple groups - specify pairings
#### Scale of cohens d - scale to fit (-1, 1), limit axis extents (Hegdes same)
#### Bar jitter -- done
#### Effect size plot below data for multiple or if selected by user
## magic adjacent/at ?
## effect size density optional (FALSE)
## points optional in paired
## CI optional/seperate with mean: to include with meean SD

transparent <-  function(colour, alpha) {
  rgba.val <- col2rgb(colour, TRUE)
  t.col <- rgb(rgba.val[1, ], rgba.val[2, ], rgba.val[3, ],
               maxColorValue = 255,
               alpha = (100 - alpha * 100) * 255 / 100)
  t.col
}

### confidence interval of a group
CI <- function(x){
  alpha <- 0.95
  m <- mean(x)
  sd <- sd(x)
  n <- length(x)
  se <- sd/sqrt(n)
  df <- n - 1
  alpha <- 1 - alpha
  t <- qt(p = alpha/2, df = df, lower.tail = F)
  me <- t * se
  c(m - me, m + me)
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

# Hedges d (group 2 - group 1)

stHedgesD <- function(x1, x2){
  m1 <- mean(x1)
  SD1 <- sd(x1)
  N1 <- length(x1)
  m2 <- mean(x2)
  SD2 <- sd(x2)
  N2 <- length(x2)
  x <- sqrt((N1 - 1) * (SD1 * SD1) * (N2 - 1) * (SD2 * SD2) / (N1 + N2 - 2))
  d = (m2 - m1) / x
  d * (1 - (3 / (4 * (N1 + N2) - 9)))
}

#############################################################################

#' Generate differences
#'
#' This is details
#'
#' @param data A data frame...
#' @param effect.type Type of difference
#' @param na.rm a logical evaluating to TRUE or FALSE indicating whether NA
#'   values should be stripped before the computation proceeds. If NA values are
#'   stripped and `effect.type` is "paired", all rows (observations) for IDs
#'   with missing data are stripped.
#'
#' @return List containing: bootstrapped mean difference + confidence interval
#'   of mean difference + individual bootstrapped difference + CI of group 1 +
#'   CI of group 2 + mean of group 1 + mean of group 2 + median of group 1 +
#'   median of group 2 + raw data
#'
#' @export
difference <- function(data,
                       effect.type = c("unstandardised", "cohens", "hedges", "paired"),
                       #paired = FALSE, # if true calculate paired mean difference
                       data.col = 1, group.col = 2, block.col = NULL, id.col,
                       R = 1000,
                       ci.type = "bca",
                       na.rm = FALSE,
                       ...
                       # ci.type = "bca", #default
                       # contrast = c("group 1 - group 2"), # default larger group minus small group
                       # ci.conf = 0.95,
) {

  if (!is.function(effect.type))
    effect.type <- match.arg(effect.type)

  # Optionally handle NA values
  if (na.rm) {
    toKeep <- !is.na(data[[data.col]]) & !is.na(data[[group.col]])
    if (effect.type == "paired") {
      toKeep <- toKeep & !is.na(data[[id.col]])
      # Also delete pairs with missing data
      badPairIds <- unique(data[[id.col]][!toKeep])
      toKeep <- toKeep & !(data[[id.col]] %in% badPairIds)
    }
    data <- data[toKeep, ]
  }

  # Get list of groups
  groups <- sort(unique(data[[group.col]]))

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
  } else if (effect.type == "hedges") {
    statistic <- .wrap2GroupStatistic(stHedgesD)
  } else if (effect.type == "paired") {
    if (missing(id.col)) stop("id.col must be specified when effect.type = 'paired'")
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
  # Overwrite data in result with the real data (rather than bootstrapped data, e.g. for paired)
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
    ci <- CI(grpVals)
    c(mean = mean(grpVals),
      median = median(grpVals),
      sd = sd(grpVals),
      se = sd(grpVals) / sqrt(length(grpVals)),
      # CI function here (maybe no need to print to confuse with bootstrap CI, but need to be part of es for plotting CI)
      CI.lower = ci[1],
      CI.upper = ci[2]
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
                   points = transparent(RColorBrewer::brewer.pal(max(3, length(es$groups)), "Set2"), .4),
                   violin = c("left-half", "right-half", "full"),
                   violin_border = RColorBrewer::brewer.pal(max(3, length(es$groups)), "Set2"),
                   violin_fill = transparent(RColorBrewer::brewer.pal(max(3, length(es$groups)), "Set2"), .6),
                   violin_adj = 1.5,
                   violin_width = 0.35,
                   violin_trunc_at = 0.05,
                   central_tendency = c("mean", "median"),
                   mean = "grey20", ##take off with FALSE option
                   bar = RColorBrewer::brewer.pal(max(3, length(es$groups)), "Set2"),
                   bar_fill = transparent(RColorBrewer::brewer.pal(max(3, length(es$groups)), "Set2"), .8),
                   mar = c(5, 4, 4, 4) + 0.1, # Default margin
                   xlab = "",
                   error_bars = c("CI", "SD", "SE"), # draw confidence interval line of the data; if, box, density, violin is TRUE, CI is FALSE
                   ef_size = TRUE, # if false do not plot effect size
                   ef_size_density = TRUE, #if true draw effect size confidence interval
                   paired = es$effect.type == "paired", # if true draw lines between paired points
                   # box = TRUE,# draw boxplot
                   # box_fill = TRUE,# fill up box colour # if false only border will be drawn
                   # points = TRUE, #add individual data point
                   #barchart = TRUE, #draw bar chart
                   ef_size_position = c("right", "down"),# when Gardner-Altman_plot is chosen effect size plotted right, otherwise down
                   left_ylab = "",
                   right_ylab = "",
                   bottom_ylab = "",
                   col = c("col1", "col2", "col3"), opacity = 0.6, #colour of box, violin, box border, density, col 1 = group 1, col2 = group 2, col3 = ef plot, {col = n+1, n = group no) #
                   points_col = c("col1", "col2", "col3"), points_opacity = 0.4, # points colour
                   las = 1, ...
) {
  if (!is(es, "plotES"))
    stop("data must be a plotES object")
  if (!isFALSE(violin))
    violin <- match.arg(violin)
  error_bars <- match.arg(error_bars)
  if (!isFALSE(central_tendency))
    central_tendency <- match.arg(central_tendency)

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

  # If needed, extend y range to encompass bar plots
  if (.show(bar)) {
    #message("Bar charts are not implemented yet")
      ylim <- c(0, max(data[[es$data.col]]))
  }

  # If needed extend x range to encompass effect size
  if (.show(ef_size) && length(groups) == 2) {
    xlim[2] <- xlim[2] + 0.95
  }

  # Extend width if showing effect size on right

  # Prepare plot
  par(mar = mar)
  plot(NULL, xlim = xlim, ylim = ylim, type = "n",
       xaxt = "n", xlab = xlab, las = las, ylab = es$data.col,...)
  # Label the groups
  axis(1, at = seq_len(length(groups)), labels = groups)

  # Add the various components to the plot
  f <- as.formula(paste(es$data.col, "~", es$group.col))

  # Box plot
  if (.show(box)) {
    boxplot(f, data = data, add = TRUE, axes = FALSE,
            col = .colour(box_fill), border = .colour(box))
  }

  ## bar chart
  if (.show(bar)) {
    barplot(es$groupStatistics[,1] ~ es$groups,
            width = 0.8, space = c(0.75,0.25),
            col = .colour(bar_fill), border = .colour(bar),
            add = TRUE, axes = FALSE)
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
    method <- ifelse(.show(paired), "overplot", "jitter")
    stripchart(f, data = data, method = method,
               pch = 19, vertical = TRUE,
               col = .colour(points),
               add = TRUE)
  }

  # Draw lines between paired points
  if (.show(paired)) {
    if (es$effect.type != "paired")
      stop("To plot paired lines, effect.type must be \"paired\"")
    p1 <- data[[es$data.col]][data[[es$group.col]] == groups[1]]
    p2 <- data[[es$data.col]][data[[es$group.col]] == groups[2]]
    segments(1.0, p1,
             2.0, p2,
             col = transparent("grey20", .7),
             lty = 1, lwd = 2)
  }

  ##  mean +SD
  if (.show(central_tendency)) {
    for (i in seq_along(groups)) {

      # get mean of group
      y <- es$groupStatistics[i, central_tendency]

      # plot points or lines
      if (central_tendency == "mean")
        points(i, y, pch = 19, cex = 1.5, col = .colour(mean))
      else
        segments(i - violin_width, y, i + violin_width, y, col = .colour(mean), lwd = 2)
    }
}
      ## add CI/SD/SE
  if (.show(error_bars)) {
    for (i in seq_along(groups)) {

      if (error_bars == "CI") {
        bars <- es$groupStatistics[i, c("CI.lower", "CI.upper")]

      } else if (error_bars == "SD") {
        bars <- c(y - es$groupStatistics[i, "sd"],
                  y + es$groupStatistics[i, "sd"])

      } else {
        bars <- c(y - es$groupStatistics[i, "se"],
                  y + es$groupStatistics[i, "se"])

      }
      if (!is.na(error_bars)) {
        segments(i, bars[1], i, bars[2], col = .colour(mean), lty = 1, lwd = 2)
      }
    }
  }

  # effect size
  if (.show(ef_size)) {
    # If there are 2 groups..
    if (length(groups) == 2) {
      # Show it to the right
      d <- density(es$t)
      d$y <- d$y / max(d$y) * violin_width
      # Diff is group2 - group1, so add to group1 mean make it align with group1
      y <- es$groupStatistics[1, 1]
      polygon(d$y + 3.0, d$x + y,
              col = transparent("black", .8),
              border = "black")

      # Draw mean of effect size
      points (3, y + es$t0, pch = 19, col = "grey20", cex = 1.5)
      # Confidence interval of effect size
      segments(3, y + es$bca[4], 3, y + es$bca[5], col = "grey20", lty = 1, lwd = 2.0)

      # Horizontal lines from group means
      segments(1 + 0.4, y, 5, y, col = "grey20", lty = 1, lwd = 1.5)
      segments(2 + 0.4, y + es$t0, 5, y + es$t0, col = "grey20", lty = 1, lwd = 1.5)

      # Axis labels on right-hand
      axis(4, at = pretty(range(es$t)) + y,
           labels = pretty(range(es$t)), las = 1)

      # Add x-axis label for effect size
      axis(1, at = 3, labels = FALSE)
      mtext(sprintf("%s\nminus\n%s", es$groups[2], es$groups[1]), at = 3, side = 1, line = 3)
      mtext("Mean difference",  side = 4, line = 2.5)

    }
  }

  es
}
