### Private functions

#### TODO
#### Unstandardised - y-axis scale should be the same in both components (effect size and data plot)

#### Multiple groups - specify pairings, YES TODO
#### Scale of cohens d, hedges - scale to look good, perhaps 1/3 of y range, limit axis extents TODO
#### NO Rename plot function to plot.SAKPlot? Maybe easier to use, harder to locate documentation
#### Bar jitter -- done
## magic adjacent/at ?
####  New options, box_at, box_width, violin_at, points_at, scatter points cex, colour of points, add stripplot methods arg
## effect size density optional (FALSE) TODO implement ef.size.density
## points optional in paired TODO
## CI optional/separate with mean: to include with mean SD
#### TODO how to include outliers in diff calc but not in plot?
#### Allow user control over vipor::offsetX

transparent <-  function(colour, alpha) {
  rgba.val <- grDevices::col2rgb(colour, TRUE)
  t.col <- grDevices::rgb(rgba.val[1, ], rgba.val[2, ], rgba.val[3, ],
               maxColorValue = 255,
               alpha = (100 - alpha * 100) * 255 / 100)
  t.col
}

getErrorBars <- function(es, groupIdx, groupMean, error.bars) {
  if (error.bars == "CI") {
    bars <- es$group.statistics[groupIdx, c("CI.lower", "CI.upper")]

  } else if (error.bars == "SD") {
    bars <- c(groupMean - es$group.statistics[groupIdx, "sd"],
              groupMean + es$group.statistics[groupIdx, "sd"])

  } else {
    bars <- c(groupMean - es$group.statistics[groupIdx, "se"],
              groupMean + es$group.statistics[groupIdx, "se"])
  }

  bars
}

# Plot a single pairwise effect size
plotEffectSize <- function(pwes, xo, yo, group1y, violin.width, mapYFn, xpd = FALSE) {
  d <- stats::density(pwes$t)
  # Make effect size width half the violin width
  d$y <- d$y / max(d$y) * violin.width / 2
  # Diff is group2 - group1, so add to group1 mean make it align with group2
  graphics::polygon(xo + d$y, mapYFn(yo + group1y + d$x),
          col = transparent("black", .8),
          border = "black", xpd = xpd)

  # Draw mean of effect size
  graphics::points(xo, mapYFn(yo + group1y + pwes$t0), pch = 19, col = "grey20", cex = 1.5, xpd = xpd)
  # Confidence interval of effect size
  graphics::segments(xo, mapYFn(yo + group1y + pwes$bca[4]), xo, mapYFn(yo + group1y + pwes$bca[5]),
           col = "grey20", lty = 1, lwd = 2.0, xpd = xpd)
}

plotEffectSizesRight <- function(es, violin.width, right.ylab) {
  # Show it to the right
  pwes <- es$pairwise.differences[[1]]
  y <- es$group.statistics[1, 1]
  plotEffectSize(pwes, 3, 0, y, violin.width, identity)

  # Horizontal lines from group means, only meaningful for unstandardised differences
  if (es$effect.type == "unstandardised") {
    graphics::segments(1, y, 5, y, col = "grey50", lty = 1, lwd = 1.5)
    graphics::segments(2, 0 + y + pwes$t0, 5, y + pwes$t0, col = "grey50", lty = 1, lwd = 1.5)
  }

  # Axis labels on right-hand
  graphics::axis(4, at = y + pretty(range(c(0, pwes$t))),
       labels = pretty(range(c(0, pwes$t))), las = 1)

  # Add x-axis label for effect size
  graphics::mtext(sprintf("%s\nminus\n%s", es$groups[2], es$groups[1]), at = 3, side = 1, line = 3)

  # Optionally label the right y-axis
  graphics::axis(1, at = 3, labels = FALSE)
  if (!isFALSE(right.ylab)) {
    graphics::mtext(es$effect.name,  side = 4, line = 2.5)
  }
}

plotEffectSizesBelow <- function(es, violin.width, xlim) { #, mar) {
  groups <- es$groups
  nGroups <- length(groups)

  if (nGroups > 3) stop("Sorry, unable to plot effect sizes for more than 3 groups")

  # Find and return the pairwise difference for the specified 2 groups
  pwDiff <- function(g1, g2) {
    for (i in seq_len(length(es$pairwise.differences))) {
      gi <- es$pairwise.differences[[i]]$groups
      if (gi[1] == g1 && gi[2] == g2)
        return(es$pairwise.differences[[i]])
      if (gi[1] == g2 && gi[2] == g1)
        return(negatePairwiseDiff(es$pairwise.differences[[i]]))
    }
  }

  # What will we plot?
  plotDiffs <- list()
  plotDiffs[[1]] <- NULL
  for (g1 in 2:nGroups) {
    plotDiffs[[g1]] <- pwDiff(groups[g1], groups[1])
  }
  ylim <- range(c(0, sapply(plotDiffs, function(pwes) if (is.null(pwes)) NA else range(pwes$t))), na.rm = TRUE)


  ### Work out how to map the effect size pseudo region onto user coordinates
  usr <- graphics::par("usr")
  # Height (inches) of margin between effect size and main plot as a proportion character height
  pseudoMargin <- graphics::par("cxy")[2] * 3
  uy1 <- usr[3] - pseudoMargin
  # Height of margin between effect size and main plot as a proportion of plot size
  pseudoHeight <- 0.35
  uy0 <- uy1 - (usr[4] - usr[3]) * pseudoHeight
  # Function to map ylim to c(uy0, uy1)
  mapY <- function(y) {
    stats::approx(ylim, c(uy0, uy1), y)$y
  }

  at <- pretty(ylim)
  graphics::axis(2, at = mapY(at), labels = at, xpd = TRUE)
  graphics::mtext(es$effect.name, side = 2, at = mapY(mean(ylim)), line = 3)

  graphics::lines(usr[1:2], c(mapY(0), mapY(0)), col = "grey50", lty = 3, xpd = TRUE)
  for (i in seq_along(plotDiffs)) {
    pwes <- plotDiffs[[i]]
    if (!is.null(pwes)) {
      gid1 <- which(groups == pwes$groups[1])
      gid2 <- which(groups == pwes$groups[2])
      plotEffectSize(pwes, gid1, 0, 0, violin.width, mapY, xpd = TRUE)
      graphics::text(gid1, mapY(ylim[1]), sprintf("%s\nminus\n%s", pwes$groups[1], pwes$groups[2]), xpd = TRUE, pos = 1)
    }
  }
}

#############################################################################

#' Plot grouped data and effect size in base R.
#'
#' .
#'
#' @param es Data returned from a call to \link{\code{SAKDifference}}
#' @param points Colour of individual points. If FALSE, points are not plotted,
#'   TRUE plots applies a default colour. May be a vector of colours. If length
#'   1, all points are drawn with the specified colour. If length is the number
#'   of groups, points in each group are drawn with the appropriate colour.
#'   Otherwise, \code{points} should be a vector of colours with a value for
#'   each data point.
#' @param points.method Method used to avoid overplotting points. Use
#'   \code{overplot} to overplot points and \code{jitter} to add random noise to
#'   each x-value. See \link{\code{vipor::offsetX}} for remaining methods.
#'
#' @param violin What type of violin plot to display. If FALSE, violin plot is
#'   not drawn.
#' @param violin.border Colours of violin borders.
#' @param violin.fill Colour used to fill violins.
#' @param violin.adj Value used to control violin plot smoothness by adjusting
#'   the kernel density bandwidth. Higher values produce a smoother plot.
#' @param violin.width Width of maximum violin horizontal extents, as a
#'   proportion of the distance between groups.
#' @param violin.trunc.at Numeric value that specifies what vertical proportion
#'   of the violin is truncated.
#'
#' @param box If not FALSE, draw a box-and-whisker plot of the grouped values.
#'   Value may be a colour, in which case the box borders are plotted with the
#'   colour(s). See \link{\code{graphics::boxplot}}.
#' @param box.fill Colour used to fill the bodies of the box-and-whisker plot.
#'   If FALSE or NA, bodies are not filled
#' @param box.notch If TRUE, draws notches in the sides of the boxes. See
#'   \link{\code{graphics::boxplot.stats}} for the calculations used.
#'
#' @seealso \link{\code{vipor::offsetX}},
#'
#' @export
SAKPlot <- function(es,

                    points = TRUE,
                    points.method = c("quasirandom", "pseudorandom", "smiley", "maxout", "frowney", "minout", "tukey",
                                      "tukeyDense", "jitter", "overplot"),

                    violin = c("left-half", "right-half", "full"),
                    violin.border = RColorBrewer::brewer.pal(max(3, length(es$groups)), "Set2"),
                    violin.fill = transparent(RColorBrewer::brewer.pal(max(3, length(es$groups)), "Set2"), .6),
                    violin.adj = 1.5,
                    violin.width = 0.35,
                    violin.trunc.at = 0.05,

                    box = FALSE,
                    box.fill = "lightgrey",
                    box.notch = FALSE,

                    bar = FALSE,
                    bar.fill = transparent(RColorBrewer::brewer.pal(max(3, length(es$groups)), "Set2"), .8),

                    ef.size = c(TRUE, "right", "below"), # if !FALSE, plot effect size
                    ef.size.density = TRUE, # if TRUE, draw effect size confidence interval
                    paired = es$effect.type == "paired", # if true draw lines between paired points

                    central.tendency = c("mean", "median"),
                    error.bars = c("CI", "SD", "SE"), # draw confidence interval line of the data; if box, density, violin is TRUE, CI is FALSE

                    mean = "grey20",
                    xlab = "",
                    left.ylab = es$data.col.name,
                    right.ylab = "",
                    #col = c("col1", "col2", "col3"), opacity = 0.6, #colour of box, violin, box border, density, col 1 = group 1, col2 = group 2, col3 = ef plot, {col = n+1, n = group no) #
                    ...
) {
  # Check and process input parameters
  if (!methods::is(es, "SAKDiff"))
    stop("data must be a SAKDiff object")
  if (!isFALSE(violin))
    violin <- match.arg(violin)
  error.bars <- match.arg(error.bars)
  if (!isFALSE(central.tendency))
    central.tendency <- match.arg(central.tendency)
  if (!isFALSE(ef.size)) {
    if (isTRUE(ef.size))
      ef.size <- "right"
    else
      ef.size <- match.arg(ef.size)
  }
  points.method <- match.arg(points.method)

  # We allow TRUE/FALSE or colours to be specified for many values. TRUE is equivalent to a default colour
  .boolToDef <- function(arg, def) if (isTRUE(arg)) { def } else { arg }
  box <- .boolToDef(box, "black")
  bar <- .boolToDef(bar, RColorBrewer::brewer.pal(max(3, length(es$groups)), "Set2"))

  .show <- function(what) !isFALSE(what) && !is.null(what)
  .isColour <- function(c) tryCatch(is.matrix(grDevices::col2rgb(c)), error = function(e) FALSE)
  .colour <- function(what) if (.isColour(what)) { what } else { NA }

  data <- es$data
  groups <- es$groups
  nGroups <- length(groups)

  # Calculate densities for violin plots
  densities <- lapply(groups, function(g) stats::density(data[[es$data.col]][data[[es$group.col]] == g], adj = violin.adj))
  # Normalise densities heights so they all have desired height (which becomes width)
  densities <- lapply(densities, function(d) { d$y <- d$y / max(d$y) * violin.width; d })
  # Optionally chop off the tails
  if (violin.trunc.at > 0) {
    densities <- lapply(densities, function(d) {
      keep <- which(d$y > violin.trunc.at * violin.width)
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
      if (.show(error.bars)) {
        ym <- max(sapply(seq_along(groups), function(gi) {
          groupMean <- mean(data[[es$data.col]][data[[es$group.col]] == groups[gi]])
          getErrorBars(es, gi, groupMean, error.bars)[2]
        }))
      } else {
        # Get means of each group
        ym <- max(sapply(groups, function(g) mean(data[[es$data.col]][data[[es$group.col]] == g])))
      }
      ylim <- c(0, ym)
    }
  }

  # If needed extend x range to encompass effect size
  if (.show(ef.size)) {
    if (nGroups < 2) {
      ef.size <- FALSE
    } else if (nGroups == 2 && ef.size != "below") {
      # Default to "right" for two groups and position unspecified
      ef.size <- "right"
    } else {
      # Can't show effect size on the right if there's more than one comparison
      ef.size <- "below"
    }
    if (ef.size == "right") {
      # Extend x-axis to accommodate effect size
      xlim[2] <- xlim[2] + 0.7
    } else if (ef.size == "below") {
      # Draw the effect size in an enlarged bottom margin.
      # Get plot height in inches
      plotHeight <- graphics::par("pin")[2]
      # Get current margin sizes in inches
      mai <- graphics::par("mai")
      # Increase the size of the bottom margin to fit the effect size plot
      newMai <- mai
      newMai[1] <- mai[1] + plotHeight * 1/3

      # Save current margin and restore on exit
      def.par <- graphics::par(mai = newMai)
      on.exit(graphics::par(def.par))
    }
  }


  # Save current plot parameters and restore on exit. Note that this doesn't
  # work when effect size is "below". Also, don't save all parameters because
  # that confuses things if we are inside a layout created by layout or
  # graphics::par(mfrow)

  ### Prepare plot ###
  plot(NULL, xlim = xlim, ylim = ylim, type = "n",
       xaxt = "n", xlab = xlab, ylab = left.ylab, ...)
  # Label the groups
  graphics::axis(1, at = seq_len(nGroups), labels = groups)

  ### Add the various components to the plot ###
  # Turn group column into a factor so it can be ordered by the user
  data$.group.as.factor <- factor(data[[es$group.col]], levels = groups)
  f <- stats::as.formula(paste(es$data.col.name, "~.group.as.factor"))

  # Box plot
  if (.show(box)) {
    graphics::boxplot(f, data = data, add = TRUE, axes = FALSE, notch = box.notch,
            col = .colour(box.fill), border = .colour(box))
  }

  # bar chart
  if (.show(bar)) {
    graphics::barplot(es$group.statistics[, "mean"] ~ factor(groups, levels = groups),
            width = 0.8, space = c(0.75, rep(0.25, nGroups - 1)),
            col = .colour(bar.fill), border = .colour(bar),
            add = TRUE, axes = FALSE, names.arg = FALSE)
  }

  # Violin plots
  if (.show(violin)) {
    for (i in seq_along(groups)) {
      d <- densities[[i]]
      col <- violin.fill[i]
      border <- violin.border[i]
      if (violin == "left-half") {
        graphics::polygon(i - d$y, d$x, col = col, border = border)
      } else if (violin == "right-half") {
        graphics::polygon(i + d$y, d$x, col = col, border = border)
      } else {
        graphics::polygon(c(i - d$y, rev(i + d$y)), c(d$x, rev(d$x)), col = col, border = border)
      }
    }
  }

  # Scatter plot of data points
  if (.show(points)) {
    palette <- transparent(RColorBrewer::brewer.pal(max(3, length(es$groups)), "Set2"), .4)
    pointCol <- .boolToDef(points, palette[as.numeric(data$.group.as.factor)])
    if (length(pointCol) == nGroups) {
      # If colours specified for each group, expand out to the colours for each point
      pointCol <- pointCol[as.numeric(data$.group.as.factor)]
    }

    x <- as.numeric(data$.group.as.factor)
    if (.show(paired))
      # If showing paired points, just overplot them
      points.method <- "overplot"
    if (points.method == "overplot") {
      # Do nothing
    } else if (points.method == "jitter") {
        x <- jitter(x, amount = 0.1)
    } else {
      # Scatter the points
      x <- x + vipor::offsetX(data[[es$data.col]], as.numeric(data$.group.as.factor),
                              method = points.method, varwidth = TRUE, adjust = violin.width)
    }
    graphics::points(x, data[[es$data.col]], pch = 19, col = .colour(pointCol))
  }

  # Draw lines between paired points
  if (.show(paired)) {
    if (es$effect.type != "paired")
      stop("To plot paired lines, effect.type must be \"paired\"")
    p1 <- data[[es$data.col]][data[[es$group.col]] == groups[1]]
    p2 <- data[[es$data.col]][data[[es$group.col]] == groups[2]]
    graphics::segments(1.0, p1,
             2.0, p2,
             col = transparent("grey20", .7),
             lty = 1, lwd = 2)
  }

  ##  mean +SD
  if (.show(central.tendency)) {
    for (i in seq_along(groups)) {

      # get mean of group
      y <- es$group.statistics[i, central.tendency]

      # plot points or lines
      if (central.tendency == "mean")
        graphics::points(i, y, pch = 19, cex = 1.5, col = .colour(mean))
      else
        graphics::segments(i - violin.width, y, i + violin.width, y, col = .colour(mean), lwd = 2)
    }
  }

  ## add CI/SD/SE error bars
  if (.show(error.bars)) {
    centre <- central.tendency
    if (isFALSE(centre))
      centre <- "mean"
    for (i in seq_along(groups)) {
      y <- es$group.statistics[i, centre]
      bars <- getErrorBars(es, i, y, error.bars)
      graphics::segments(i, bars[1], i, bars[2], col = .colour(mean), lty = 1, lwd = 2)
    }
  }

  # effect size
  if (ef.size == "right") {
    plotEffectSizesRight(es, violin.width, right.ylab)
  } else if (ef.size == "below") {
    plotEffectSizesBelow(es, violin.width, xlim) #, mar)
  }

  invisible(es)
}
