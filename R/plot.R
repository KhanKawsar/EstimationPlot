### Private functions

#### TODO
#### Unstandardised - y-axis scale should be the same in both components (effect size and data plot)

#### Scale of cohens d, hedges - scale to look good, perhaps 1/3 of y range, limit axis extents TODO
## magic adjacent/at ? See google raincloud plot for examples
####  New options, box_at, box_width, violin_at, points_at, scatter points cex
## points optional in paired TODO
## CI optional/separate with mean: to include with mean SD
#### Licence
#### Document data in R/data.R

transparent <-  function(colour, alpha) {
  rgba.val <- grDevices::col2rgb(colour, TRUE)
  t.col <- grDevices::rgb(rgba.val[1, ], rgba.val[2, ], rgba.val[3, ],
               maxColorValue = 255,
               alpha = (100 - alpha * 100) * 255 / 100)
  t.col
}

# Calculate the probability density of one group, optionally truncating the extents
getGroupDensity <- function(group, es, violin.adj, violin.trunc, violin.width) {
  groupVals <- es$data[[es$data.col]][es$data[[es$group.col]] == group]
  # Calculate density
  d <- stats::density(groupVals, adj = violin.adj)

  # Normalise density height so all the groups have the same maximum display width
  d$y <- d$y / max(d$y) * violin.width

  # Optionally truncate
  keep <- NULL
  if (isTRUE(violin.trunc)) {
    # Truncate to data extents (isTRUE(violin.trunc)). Ensure density completely encloses data points
    from <- utils::tail(which(d$x < min(groupVals)), 1)
    to <- utils::head(which(d$x > max(groupVals)), 1)
    keep <- seq(from, to)
    # Extend by 1 step in each direction so that density completely includes data
    keep <- c(keep[1] - 1, keep, keep[length(keep)] + 1)
  } else if (is.numeric(violin.trunc) && violin.trunc > 0) {
    # Truncate to specified probability
    keep <- which(d$y >= violin.trunc * max(d$y))
  }
  if (!is.null(keep)) {
    d$y <- c(0, d$y[keep], 0)
    d$x <- c(d$x[keep[1]], d$x[keep], d$x[keep[length(keep)]])
  }

  d
}

# Save and restore the random number state. This is done so that we don't
# interfere with the random number generation of package users
preserveRNG <- function(expr) {
  # Save current RNG seed
  # http://www.cookbook-r.com/Numbers/Saving_the_state_of_the_random_number_generator/
  if (exists(".Random.seed", .GlobalEnv))
    oldseed <- .GlobalEnv$.Random.seed
  else
    oldseed <- NULL
  # On exit...
  on.exit({
    # Restore old seed
    if (!is.null(oldseed))
      .GlobalEnv$.Random.seed <- oldseed
    else
      rm(".Random.seed", envir = .GlobalEnv)
  })

  # Evaluate the expression
  expr
}

# Function to return a palette of \code{n} colours, with transparency \code{alpha}.
pickPalette <- function(n, pal = "Set2") {
  # Try to use an RColorBrewer palette
  if (n > RColorBrewer::brewer.pal.info[pal, "maxcolors"]) {
    # This is complicated because we want to "randomly" sample available
    # colours, but in a repeatable way, and without interfering with user's
    # random number generation, hence we wrap the code in a call to preserveRNG
    preserveRNG({
      # Too many colours, concatenate some palettes (thanks to https://stackoverflow.com/a/33144808/1287461)
      qual_col_pals <- RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual', ]
      col_vector <- unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
      # Randomly sample n colours from the whole list
      set.seed(3)
      # Try to sample without replacement, but allow replacement if there are too many colours needed
      sample(col_vector, n, replace = n > length(col_vector))
    })
  } else {
    RColorBrewer::brewer.pal(max(3, n), pal)
  }
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
plotEffectSize <- function(pwes, xo, yo, group1y, ef.size.violin, violin.width, mapYFn, xpd = FALSE) {
  if (ef.size.violin) {
    d <- stats::density(pwes$t)
    # Make effect size width half the violin width
    d$y <- d$y / max(d$y) * violin.width / 2
    # Diff is group2 - group1, so add to group1 mean make it align with group2
    graphics::polygon(xo + d$y, mapYFn(yo + group1y + d$x),
                      col = transparent("black", .8),
                      border = "black", xpd = xpd)
  }

  # Draw mean of effect size
  graphics::points(xo, mapYFn(yo + group1y + pwes$t0), pch = 19, col = "grey20", cex = 1.5, xpd = xpd)
  # Confidence interval of effect size
  graphics::segments(xo, mapYFn(yo + group1y + pwes$bca[4]), xo, mapYFn(yo + group1y + pwes$bca[5]),
           col = "grey20", lty = 1, lwd = 2.0, xpd = xpd)
}

# Plot effect size to the right of the main plot. Only useful when showing a single effect size
plotEffectSizesRight <- function(es, ef.size.violin, violin.width, right.ylab) {
  pwes <- es$group.differences[[1]]
  y <- es$group.statistics[1, 1]
  y2 <- es$group.statistics[2, 1]

  if (es$effect.type %in% c("unstandardised", "paired")) {
    esRange <- range(c(0, pwes$t))
    plotEffectSize(pwes, 3, 0, y, ef.size.violin, violin.width, identity)
  } else {
    cat("===========================================================\n")
    cat("TODO position and scale standardised effect size plots TODO\n")
    cat("===========================================================\n")
    esRange <- c(-3, 3)
    plotEffectSize(pwes, 3, 0, es$group.statistics[2, 1], ef.size.violin, violin.width, identity)
  }

  # Horizontal lines from group means, only meaningful for unstandardised differences
  graphics::segments(1, y, 5, y, col = "grey50", lty = 1, lwd = 1.5)
  graphics::segments(2, 0 + y + pwes$t0, 5, y + pwes$t0, col = "grey50", lty = 1, lwd = 1.5)
  #graphics::segments(2, y2, 5, y2, col = "grey50", lty = 1, lwd = 1.5)

  # Axis labels on right-hand
  graphics::axis(4, at = y + pretty(esRange),
       labels = pretty(esRange), las = 1)

  # Add x-axis label for effect size
  graphics::mtext(sprintf("%s\nminus\n%s", es$groups[2], es$groups[1]), at = 3, side = 1, line = 3)
  graphics::axis(1, at = 3, labels = FALSE) # X-axis tick mark

  # Optionally label the right y-axis
  if (!isFALSE(right.ylab)) {
    graphics::mtext(es$effect.name, side = 4, line = 2.5)
  }
}

# Plot effect size below the main plot. Assumes that bottom margin is large
# enough to accommodate the effect size plot
plotEffectSizesBelow <- function(es, ef.size.violin, violin.width, xlim) {
  groups <- es$groups
  nGroups <- length(groups)

  # Find and return the pairwise difference for the specified 2 groups
  pwDiff <- function(g1, g2) {
    for (i in seq_len(length(es$group.differences))) {
      gi <- es$group.differences[[i]]$groups
      if (gi[1] == g1 && gi[2] == g2)
        return(es$group.differences[[i]])
      if (gi[1] == g2 && gi[2] == g1)
        return(negatePairwiseDiff(es$group.differences[[i]]))
    }
    stop(sprintf("Difference %s - %s not found", g1, g2))
  }

  # What will we plot?
  plotDiffs <- es$group.differences
  ylim <- range(c(0, sapply(plotDiffs, function(pwes) if (is.null(pwes)) NA else range(pwes$t))), na.rm = TRUE)
  ylim <- grDevices::extendrange(ylim)

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
      plotEffectSize(pwes, gid1, 0, 0, ef.size.violin, violin.width, mapY, xpd = TRUE)
      graphics::text(gid1, mapY(ylim[1]), sprintf("%s\nminus\n%s", pwes$groups[1], pwes$groups[2]), xpd = TRUE, pos = 1)
    }
  }
}

#############################################################################

#' Group and effect size plotting in base R.
#'
#' Plot grouped data and effect size in base R, with control over a large range
#' of possible display formats and options. To plot your data, first calculate
#' group differences by calling \code{\link{SAKDifference}}, then pass the
#' result to \code{\link{SAKPlot}}.
#'
#' Group data may be visualised in multiple ways: \code{points}, \code{violin},
#' \code{box} and \code{bar}. Each visualisation type is controlled by a set of
#' parameters. To display a type, for example box plots, specify \code{box =
#' TRUE}. Rather than \code{TRUE}, you may specify a colour, which is used is
#' the border/outline for the boxes. You may also specify a vector of colours,
#' one for each group. For \code{points}, you may specify a colour for each
#' individual point. When colours are not specified, they are selected from an
#' \code{\link{RColorBrewer}} qualitative palette.
#'
#' @param es Data returned from a call to \code{\link{SAKDifference}}
#'
#' @param points Colour of individual points. If FALSE, points are not plotted,
#'   TRUE plots applies a default colour. May be a vector of colours. If length
#'   1, all points are drawn with the specified colour. If length is the number
#'   of groups, points in each group are drawn with the appropriate colour.
#'   Otherwise, \code{points} should be a vector of colours with a value for
#'   each data point.
#' @param points.method Method used to avoid overplotting points. Use
#'   \code{"overplot"} to overplot points and \code{"jitter"} to add random
#'   noise to each x-value. See \code{\link{vipor::offsetX}} for remaining
#'   methods.
#' @param pch Symbol to use for plotting points.
#' @param points.params List of named parameters to pass on to
#'   \code{\link{graphics::points}}, e.g. \code{SAKPlot(es, points = 1, pch =
#'   21, points.params = list(bg = as.numeric(factor(data$Sex)) + 1))}.
#'
#' @param violin What type of violin plot to display. If FALSE, violin plot is
#'   not drawn.
#' @param violin.border Colours of violin borders.
#' @param violin.fill Colour used to fill violins.
#' @param violin.adj Value used to control violin plot smoothness by adjusting
#'   the kernel density bandwidth. Higher values produce a smoother plot.
#' @param violin.width Width of maximum violin horizontal extents, as a
#'   proportion of the distance between groups.
#' @param violin.trunc Numeric value that specifies what vertical proportion of
#'   the violin is truncated.
#' @param violin.shape Desired violin shape - left-half only (\code{"left"}),
#'   right-half only (\code{"right"}), or a full violin (\code{"full"}).
#'
#' @param box If not FALSE, draw a box-and-whisker plot of the grouped values.
#'   Value may be a colour, in which case the box borders are plotted with the
#'   colour(s). See \code{\link{graphics::boxplot}}.
#' @param box.fill Colour used to fill the bodies of the box-and-whisker plot.
#'   If FALSE or NA, bodies are not filled
#' @param box.notch If TRUE, draws notches in the sides of the boxes. See
#'   \code{\link{graphics::boxplot.stats}} for the calculations used.
#' @param box.pars List with additional graphical parameters to control the box
#'   plot. See \code{\link{graphics::bxp}} graphical parameters for a complete
#'   list.
#'
#' @param bar If not FALSE, draw a bar plot of the group means.
#' @param bar.fill Colour used to fill bars.
#'
#' @param ef.size If not FALSE, effect sizes are plotted. Effect sizes are
#'   plotted to the right of the main plot if there is only one effect size to
#'   plot and \code{ef.size != "below"}.
#' @param ef.size.violin If not FALSE, boostrapped effect size estimates are
#'   show as a violin plot.
#'
#' @return \code{es} invisibly.
#'
#' @seealso \code{\link{SAKDifference}}, \code{\link{vipor::offsetX}},
#'   \code{\link{graphics::boxplot}}, \code{\link{graphics::bxp}}
#'
#' @export
SAKPlot <- function(es,

                    points = TRUE,
                    points.method = c("quasirandom", "pseudorandom", "smiley", "maxout", "frowney", "minout", "tukey",
                                      "tukeyDense", "jitter", "overplot"),
                    pch = 19,
                    points.params = list(),

                    violin = isFALSE(box) && isFALSE(bar),
                    violin.shape = c("left-half", "right-half", "full"),
                    violin.border = TRUE,
                    violin.fill = TRUE,
                    violin.adj = 1.5,
                    violin.width = 0.35,
                    violin.trunc = TRUE,

                    box = FALSE,
                    box.fill = TRUE,
                    box.notch = FALSE,
                    box.outline = TRUE,
                    box.pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5),

                    bar = FALSE,
                    bar.fill = TRUE,

                    ef.size = c(TRUE, "right", "below"), # if !FALSE, plot effect size
                    ef.size.violin = TRUE, # if TRUE, draw effect size confidence interval

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
    violin.shape <- match.arg(violin.shape)
  if (!isFALSE(error.bars))
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

  .show <- function(what) !isFALSE(what) && !is.null(what)
  .isColour <- function(c) tryCatch(is.matrix(grDevices::col2rgb(c)), error = function(e) FALSE)
  .colour <- function(what) if (.isColour(what)) { what } else { NA }

  data <- es$data
  groups <- es$groups
  nGroups <- length(groups)

  # Prepare some palettes, the border palette has no transparency, the fill palette is 80% transparent
  defBorderPalette <- pickPalette(nGroups)
  defFillPalette <- transparent(defBorderPalette, 0.8)

  # Calculate densities for violin plots
  densities <- lapply(groups, getGroupDensity, es, violin.adj, violin.trunc, violin.width)

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


  #### Prepare plot ####
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
    box <- .boolToDef(box, defBorderPalette)
    box.fill <- .boolToDef(box.fill, defFillPalette)
    graphics::boxplot(f, data = data, add = TRUE, axes = FALSE, notch = box.notch,
            col = .colour(box.fill), border = .colour(box), pars = box.pars)
  }

  # bar chart
  if (.show(bar)) {
    bar <- .boolToDef(bar, defBorderPalette)
    bar.fill <- .boolToDef(bar.fill, defFillPalette)
    graphics::barplot(es$group.statistics[, "mean"] ~ factor(groups, levels = groups),
            width = 0.8, space = c(0.75, rep(0.25, nGroups - 1)),
            col = .colour(bar.fill), border = .colour(bar),
            add = TRUE, axes = FALSE, names.arg = FALSE)
  }

  # Violin plots
  if (.show(violin)) {
    violin <- .boolToDef(violin, defBorderPalette)
    violin.fill <- .boolToDef(violin.fill, defFillPalette)
    borders <- violin
    if (length(borders) == 1)
      borders <- rep(borders, nGroups)
    for (i in seq_along(groups)) {
      d <- densities[[i]]
      col <- violin.fill[i]
      border <- borders[i]
      if (violin.shape == "left-half") {
        graphics::polygon(i - d$y, d$x, col = col, border = border)
      } else if (violin.shape == "right-half") {
        graphics::polygon(i + d$y, d$x, col = col, border = border)
      } else {
        graphics::polygon(c(i - d$y, rev(i + d$y)), c(d$x, rev(d$x)), col = col, border = border)
      }
    }
  }

  # Scatter plot of data points
  if (.show(points)) {
    palette <- transparent(defBorderPalette, .4)
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
    # Complicated way of calling is to allow user to pass in arbitrary parameters
    do.call(graphics::points, c(list(x = x, y = data[[es$data.col]], pch = pch, col = .colour(pointCol)), points.params))
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
    plotEffectSizesRight(es, ef.size.violin, violin.width, right.ylab)
  } else if (ef.size == "below") {
    plotEffectSizesBelow(es, ef.size.violin, violin.width, xlim)
  }

  invisible(es)
}
