### Private functions

#### TODO
#### Unstandardised - y-axis scale should be the same in both components (effect size and data plot)

#### Use of layout for ef_size = "below": leave until later, maybe use gridBase or similar
#### MKK agrees: Discuss useful defaults - suggestions:
####          SAKPlot(diff) should produce a useful plot
####          violin = TRUE and similar should have expected effect
#### Multiple groups - specify pairings, YES TODO
#### Scale of cohens d, hedges - scale to look good, perhaps 1/3 of y range, limit axis extents TODO
#### NO Rename plot function to plot.SAKPlot? Maybe easier to use, harder to locate documentation
#### Bar jitter -- done
## magic adjacent/at ?
####  New options, box_at, box_width, violin_at, points_at, scatter points cex, colour of points, add stripplot methods arg
## effect size density optional (FALSE) TODO implement ef_size_density
## points optional in paired TODO
## CI optional/separate with mean: to include with mean SD
#### TODO how to include outliers in diff calc but not in plot?
#### Allow user control over vipor::offsetX

transparent <-  function(colour, alpha) {
  rgba.val <- col2rgb(colour, TRUE)
  t.col <- rgb(rgba.val[1, ], rgba.val[2, ], rgba.val[3, ],
               maxColorValue = 255,
               alpha = (100 - alpha * 100) * 255 / 100)
  t.col
}

getErrorBars <- function(es, groupIdx, groupMean, error_bars) {
  if (error_bars == "CI") {
    bars <- es$groupStatistics[groupIdx, c("CI.lower", "CI.upper")]

  } else if (error_bars == "SD") {
    bars <- c(groupMean - es$groupStatistics[groupIdx, "sd"],
              groupMean + es$groupStatistics[groupIdx, "sd"])

  } else {
    bars <- c(groupMean - es$groupStatistics[groupIdx, "se"],
              groupMean + es$groupStatistics[groupIdx, "se"])
  }

  bars
}

# Plot a single pairwise effect size
plotEffectSize <- function(pwes, xo, yo, group1y, violin_width) {
  d <- density(pwes$t)
  # Make effect size width half the violin width
  d$y <- d$y / max(d$y) * violin_width / 2
  # Diff is group2 - group1, so add to group1 mean make it align with group2
  polygon(xo + d$y, yo + group1y + d$x,
          col = transparent("black", .8),
          border = "black")

  # Draw mean of effect size
  points(xo, yo + group1y + pwes$t0, pch = 19, col = "grey20", cex = 1.5)
  # Confidence interval of effect size
  segments(xo, yo + group1y + pwes$bca[4], xo, yo + group1y + pwes$bca[5], col = "grey20", lty = 1, lwd = 2.0)
}

plotEffectSizesRight <- function(es, violin_width, right_ylab) {
  # Show it to the right
  pwes <- es$pairwiseDifferences[[1]]
  y <- es$groupStatistics[1, 1]
  plotEffectSize(pwes, 3, 0, y, violin_width)

  # Horizontal lines from group means
  segments(1, y, 5, y, col = "grey50", lty = 1, lwd = 1.5)
  segments(2, 0 + y + pwes$t0, 5, y + pwes$t0, col = "grey50", lty = 1, lwd = 1.5)

  # Axis labels on right-hand
  axis(4, at = y + pretty(range(c(0, pwes$t))),
       labels = pretty(range(c(0, pwes$t))), las = 1)

  # Add x-axis label for effect size
  mtext(sprintf("%s\nminus\n%s", es$groups[2], es$groups[1]), at = 3, side = 1, line = 3)

  # Optionally label the right y-axis
  axis(1, at = 3, labels = FALSE)
  if (!isFALSE(right_ylab)) {
    mtext(es$effect.name,  side = 4, line = 2.5)
  }
}

plotEffectSizesBelow <- function(es, violin_width, xlim, mar) {
  groups <- es$groups
  nGroups <- length(groups)

  if (nGroups > 3) stop("Sorry, unable to plot effect sizes for more than 3 groups")

  # Find and return the pairwise difference for the specified 2 groups
  pwDiff <- function(g1, g2) {
    for (i in seq_len(length(es$pairwiseDifferences))) {
      gi <- es$pairwiseDifferences[[i]]$groups
      if (gi[1] == g1 && gi[2] == g2)
        return(es$pairwiseDifferences[[i]])
      if (gi[1] == g2 && gi[2] == g1)
        return(negatePairwiseDiff(es$pairwiseDifferences[[i]]))
    }
  }

  # What will we plot?
  plotDiffs <- list()
  plotDiffs[[1]] <- NULL
  for (g1 in 2:nGroups) {
    plotDiffs[[g1]] <- pwDiff(groups[g1], groups[1])
  }
  ylim <- range(c(0, sapply(plotDiffs, function(pwes) if (is.null(pwes)) NA else range(pwes$t))), na.rm = TRUE)

  # Prepare new plot (layout 2)
  esmar <- mar
  esmar[3] <- 0
  par(mar = esmar)
  plot(NULL, xlim = xlim, ylim = ylim, type = "n", xlab = "", ylab = es$effect.name, bty = "n", xaxt = "n")
  abline(h = 0, col = "grey50")
  for (i in seq_along(plotDiffs)) {
    pwes <- plotDiffs[[i]]
    if (!is.null(pwes)) {
      gid1 <- which(groups == pwes$groups[1])
      gid2 <- which(groups == pwes$groups[2])
      plotEffectSize(pwes, gid1, 0, 0, violin_width)
      mtext(sprintf("%s\nminus\n%s", pwes$groups[1], pwes$groups[2]), at = gid1, side = 1, line = 2.2)
    }
  }
}

#############################################################################

#' Plot grouped data and effect size in base R.
#'
#' When the effect size plot is below the main plot, (i.e. either \code{ef_size
#' = "below"} or when there are more than two effect sizes to be drawn),
#' \link{\code{graphics::layout} is used internally for laying out the two plot
#' components. This  means that this plot cannot be combined with other plots
#' using \code{par(mfrow = c...))}, \code{\link{graphics::layout}} or
#' \code{\link{graphics::split.screen}}.
#'
#' @param es Data returned from a call to \link{\code{SAKDifference}}
#' @param points Colour of individual points. If FALSE, points are not plotted, TRUE plots applies a default colour
#' @param box Colour of boxplot. If FALSE, boxplot is not drawn
#' @param box_fill Colour used to fill the bodies of the boxplot. If FALSE or
#'   NA, bodies are not filled
#' @param violin What type of violin plot to display. If FALSE, violin plot is
#'   not drawn
#' @param adj Value used to control violin plot smoothness.
#'
#'
#' @export
SAKPlot <- function(es,

                    points = TRUE,

                    violin = c("left-half", "right-half", "full"),
                    violin_border = RColorBrewer::brewer.pal(max(3, length(es$groups)), "Set2"),
                    violin_fill = transparent(RColorBrewer::brewer.pal(max(3, length(es$groups)), "Set2"), .6),
                    violin_adj = 1.5,
                    violin_width = 0.35,
                    violin_trunc_at = 0.05,

                    box = FALSE,
                    box_fill = "lightgrey",

                    bar = FALSE,
                    bar_fill = transparent(RColorBrewer::brewer.pal(max(3, length(es$groups)), "Set2"), .8),

                    ef_size = c(TRUE, "right", "below"), # if !FALSE, plot effect size
                    ef_size_density = TRUE, # if TRUE, draw effect size confidence interval
                    paired = es$effect.type == "paired", # if true draw lines between paired points

                    central_tendency = c("mean", "median"),
                    error_bars = c("CI", "SD", "SE"), # draw confidence interval line of the data; if box, density, violin is TRUE, CI is FALSE

                    mean = "grey20", ##take off with FALSE option
                    mar = c(5, 4, 4, 4) + 0.1, # Default margin
                    xlab = "",
                    left_ylab = es$data.col.name,
                    right_ylab = "",
                    bottom_ylab = "",
                    col = c("col1", "col2", "col3"), opacity = 0.6, #colour of box, violin, box border, density, col 1 = group 1, col2 = group 2, col3 = ef plot, {col = n+1, n = group no) #
                    points_col = c("col1", "col2", "col3"), points_opacity = 0.4, # points colour
                    las = 1, ...
) {
  # Check and process input parameters
  if (!is(es, "SAKDiff"))
    stop("data must be a SAKDiff object")
  if (!isFALSE(violin))
    violin <- match.arg(violin)
  error_bars <- match.arg(error_bars)
  if (!isFALSE(central_tendency))
    central_tendency <- match.arg(central_tendency)
  if (!isFALSE(ef_size)) {
    if (isTRUE(ef_size))
      ef_size <- "right"
    else
      ef_size <- match.arg(ef_size)
  }

  # We allow TRUE/FALSE or colours to be specified for many values. TRUE is equivalent to a default colour
  .boolToDef <- function(arg, def) if (isTRUE(arg)) { def } else { arg }
  box <- .boolToDef(box, "black")
  bar <- .boolToDef(bar, RColorBrewer::brewer.pal(max(3, length(es$groups)), "Set2"))

  .show <- function(what) !isFALSE(what) && !is.null(what)
  .isColour <- function(c) tryCatch(is.matrix(col2rgb(c)), error = function(e) FALSE)
  .colour <- function(what) if (.isColour(what)) { what } else { NA }

  data <- es$data
  groups <- es$groups
  nGroups <- length(groups)

  # Calculate densities for violin plots
  densities <- lapply(groups, function(g) density(data[[es$data.col]][data[[es$group.col]] == g], adj = violin_adj))
  # Normalise densities heights so they all have desired height (which becomes width)
  densities <- lapply(densities, function(d) { d$y <- d$y / max(d$y) * violin_width; d })
  # Optionally chop off the tails
  if (violin_trunc_at > 0) {
    densities <- lapply(densities, function(d) {
      keep <- which(d$y > violin_trunc_at * violin_width)
      d$y <- c(0, d$y[keep], 0)
      d$x <- c(d$x[keep[1]], d$x[keep], d$x[keep[length(keep)]])
      d
    })
  }

  # Calculate plot limits
  xlim <- c(0.5, nGroups + 0.5)
  ylim <- range(data[[es$data.col]])

  # If needed, extend y range to encompass violin plots
  if (.show(violin)) {
    for (d in densities) {
      ylim <- range(ylim, d$x)
    }
  }

  # If needed, extend y range to encompass bar plots
  if (.show(bar)) {
    if (.show(points)) {
      ylim <- c(0, max(data[[es$data.col]]))
    } else {
      # Are we drawing error bars?
      if (.show(error_bars)) {
        ym <- max(sapply(seq_along(groups), function(gi) {
          groupMean <- mean(data[[es$data.col]][data[[es$group.col]] == groups[gi]])
          getErrorBars(es, gi, groupMean, error_bars)[2]
        }))
      } else {
        # Get means of each group
        ym <- max(sapply(groups, function(g) mean(data[[es$data.col]][data[[es$group.col]] == g])))
      }
      ylim <- c(0, ym)
    }
  }

  # If needed extend x range to encompass effect size
  if (.show(ef_size)) {
    if (nGroups < 2) {
      ef_size <- FALSE
    } else if (nGroups == 2 && ef_size != "below") {
      # Default to "right" for two groups and position unspecified
      ef_size <- "right"
    } else {
      # Can't show effect size on the right if there's more than one comparison
      ef_size <- "below"
    }
    if (ef_size == "right") {
      # Extend x-axis to accommodate effect size
      xlim[2] <- xlim[2] + 0.7
    } else if (ef_size == "below") {
      # Need two plotting areas
      layout(matrix(c(1, 2), nrow = 2), heights = c(2, 1))
    }
  }


  # Save current plot parameters and restore on exit. Note that this doesn't
  # work when effect size is "below". Also, don't save all parameters because
  # that confuses things if we are inside a layout created by layout or
  # par(mfrow)
  def.par <- par(mar = mar)
  on.exit(par(def.par))

  ### Prepare plot ###
  plot(NULL, xlim = xlim, ylim = ylim, type = "n",
       xaxt = "n", xlab = xlab, ylab = left_ylab, las = las, ...)
  # Label the groups
  axis(1, at = seq_len(nGroups), labels = groups)

  ### Add the various components to the plot ###
  # Turn group column into a factor so it can be ordered by the user
  data$.group.as.factor <- factor(data[[es$group.col]], levels = groups)
  f <- as.formula(paste(es$data.col.name, "~.group.as.factor"))

  # Box plot
  if (.show(box)) {
    boxplot(f, data = data, add = TRUE, axes = FALSE,
            col = .colour(box_fill), border = .colour(box))
  }

  # bar chart
  if (.show(bar)) {
    barplot(es$groupStatistics[, "mean"] ~ factor(groups, levels = groups),
            width = 0.8, space = c(0.75, rep(0.25, nGroups - 1)),
            col = .colour(bar_fill), border = .colour(bar),
            add = TRUE, axes = FALSE, names.arg = FALSE)
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
    palette <- transparent(RColorBrewer::brewer.pal(max(3, length(es$groups)), "Set2"), .4)
    points <- .boolToDef(points, palette[as.numeric(data$.group.as.factor)])

    x <- as.numeric(data$.group.as.factor)
    # If not showing paired...
    if (!.show(paired)) {
      # Scatter the points
    x <- x + vipor::offsetX(data[[data.col]], as.numeric(data$.group.as.factor), varwidth = TRUE, adjust = 1)
    }
    points(x, data[[es$data.col]], pch = 19, col = .colour(points))
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

  ## add CI/SD/SE error bars
  if (.show(error_bars)) {
    centre <- central_tendency
    if (isFALSE(centre))
      centre <- "mean"
    for (i in seq_along(groups)) {
      y <- es$groupStatistics[i, centre]
      bars <- getErrorBars(es, i, y, error_bars)
      segments(i, bars[1], i, bars[2], col = .colour(mean), lty = 1, lwd = 2)
    }
  }

  # effect size
  if (ef_size == "right") {
    plotEffectSizesRight(es, violin_width, right_ylab)
  } else if (ef_size == "below") {
    plotEffectSizesBelow(es, violin_width, xlim, mar)
  }

  invisible(es)
}
