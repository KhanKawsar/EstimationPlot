### Private functions

#### TODO
## Control over visual representation of central tendency
## CI optional/separate with mean: to include with mean SD

# Perhaps don't do these, just require people to create more than 1 plot:
#### How should we handle paired data with more than 2 groups? eg petunia
#### How should we handle more than 1 comparison per group? E.g. all pairwise combinations


#' Returns a transparent version of the specified colour(s).
#'
#' @param colour The R colour (or colours) to be made transparent. May be a
#'   colour name, a hexadecimal string such as \code{"#ffbc48"} or a positive
#'   integer.
#' @param alpha Transparency, from \code{0} meaning fully opaque (\code{colour}
#'   is returned unchanged), through to \code{1} which is completely transparent
#'   (i.e. invisible).
#'
#' @returns A colour or colours that are transparent versions of \code{colour}.
#'
#' @seealso [grDevices]{col2rgb}, [grDevices]{rgb}
#'
#' @export
SAKTransparent <-  function(colour, alpha) {
  rgba.val <- grDevices::col2rgb(colour, TRUE)
  grDevices::rgb(rgba.val[1, ], rgba.val[2, ], rgba.val[3, ],
               maxColorValue = 255,
               alpha = (100 - alpha * 100) * 255 / 100)
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

# Plot density x and y values as a violin. Since the violin is drawn vertically,
# density$x is used for y-axis points and density$y for x-axis points.
plotViolin <- function(shape, centreX, d, ...) {
  if (shape == "left-half") {
    graphics::polygon(centreX - d$y, d$x, ...)
  } else if (shape == "right-half") {
    graphics::polygon(centreX + d$y, d$x, ...)
  } else {
    graphics::polygon(c(centreX - d$y, rev(centreX + d$y)), c(d$x, rev(d$x)), ...)
  }
}

# Plot a single pairwise effect size
#
# @param mapYFn Function to map logical y values to display coordinates. If not
#   specified, not mapping is performed.
plotEffectSize <- function(pwes, xo, centreY, showViolin, violinCol, violin.width, violin.shape, ef.size.col, ef.size.pch, mapYFn = identity, xpd = FALSE) {
  deltaY <- centreY - pwes$t0
  if (showViolin) {
    d <- stats::density(pwes$t, na.rm = TRUE)
    # Make effect size width half the violin width. Note that we rotate the
    # density plot, so x is y and vice versa
    d$y <- d$y / max(d$y) * violin.width / 2
    # Centre vertically on centreY
    d$x <- d$x + deltaY
    # Map y values as required - note that we show density$x on the y-axis
    d$x <- mapYFn(d$x)
    fill <- SAKTransparent(violinCol, .8)
    plotViolin(violin.shape, xo, d, col = fill, border = violinCol, xpd = xpd)
  }

  # Draw mean of effect size
  graphics::points(xo, mapYFn(pwes$t0 + deltaY), pch = ef.size.pch, col = ef.size.col, cex = 1.5, xpd = xpd)
  # Confidence interval of effect size
  graphics::segments(xo, mapYFn(pwes$bca[4] + deltaY), xo, mapYFn(pwes$bca[5] + deltaY),
           col = ef.size.col, lty = 1, lwd = 2.0, xpd = xpd)
}

# Plot effect size to the right of the main plot. Only useful when showing a single effect size
plotEffectSizesRight <- function(es, ef.size.col, ef.size.pch, showViolin, violinCol, violin.width, violin.shape, axisLabel, ticksAt, ef.size.las) {
  pwes <- es$group.differences[[1]]
  y <- es$group.statistics[1, 1]
  y2 <- es$group.statistics[2, 1]

  if (es$effect.type == "unstandardised") {
    esRange <- range(c(0, pwes$t))
    plotEffectSize(pwes, 3, y2, showViolin, violinCol, violin.width, violin.shape, ef.size.col, ef.size.pch)

    # Axis labels on right-hand
    labels <- names(ticksAt)
    if (is.null(ticksAt)) {
      ticksAt <- pretty(esRange)
      labels <- ticksAt
    }
    graphics::axis(4, at = y + ticksAt, labels = labels, las = ef.size.las)
  } else {
    esRange <- range(c(0, pwes$t0))
    ylim <- range(y, y2)
    # Function to map esRange to ylim
    mapY <- function(y) {
      # Interpolate/extrapolate along the line (esRange[1], ylim[1]), (esRange[2], ylim[2])
      ylim[1] + (y - esRange[1]) * (ylim[2] - ylim[1]) / (esRange[2] - esRange[1])
    }
    plotEffectSize(pwes, 3, pwes$t0, showViolin, violinCol, violin.width, violin.shape, ef.size.col, ef.size.pch, mapYFn = mapY)

    # Axis labels on right-hand
    labels <- names(ticksAt)
    if (is.null(ticksAt)) {
      ticksAt <- pretty(esRange)
      labels <- ticksAt
    }
    graphics::axis(4, at = mapY(ticksAt), labels = labels, las = ef.size.las)

    # Preliminary attempt to display effect size interpretation
    # points <- c(0.2, 0.5, 0.8, 1.2)
    # cols <- RColorBrewer::brewer.pal(length(points), "OrRd")
    # # cols <- grDevices::hcl.colors(length(points), "Dynamic")
    # for (i in seq_along(points)) {
    #   graphics::axis(4, at = mapY(points[i]), tcl = points[i], labels = FALSE, col.ticks = cols[i], lwd = 2)
    #   graphics::axis(4, at = mapY(-points[i]), tcl = points[i], labels = FALSE, col.ticks = cols[i], lwd = 2)
    # }
  }

  # Horizontal lines from group means
  graphics::segments(1, y, 5, y, col = "grey50", lty = 1, lwd = 1.5)
  graphics::segments(2, y2, 5, y2, col = "grey50", lty = 1, lwd = 1.5)

  # Add x-axis label for effect size
  graphics::mtext(sprintf("%s\nminus\n%s", pwes$groupLabels[1], pwes$groupLabels[2]), at = 3, side = 1, line = 3)
  graphics::axis(1, at = 3, labels = FALSE) # X-axis tick mark

  # Label the right y-axis
  graphics::mtext(axisLabel, side = 4, line = 2.5)
}

# Plot effect size below the main plot. Assumes that bottom margin is large
# enough to accommodate the effect size plot
plotEffectSizesBelow <- function(es, ef.size.col, ef.size.pch, showViolin, violinCol, violin.width, violin.shape, xlim, central.tendency.dx, ef.size.label, ticksAt, ef.size.las) {
  groups <- es$groups
  nGroups <- length(groups)

  # What will we plot?
  plotDiffs <- es$group.differences
  ylim <- range(c(0, sapply(plotDiffs, function(pwes) if (is.null(pwes)) NA else range(pwes$t, na.rm = TRUE))), na.rm = TRUE)
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

  # Y axis ticks and label
  labels <- names(ticksAt)
  if (is.null(ticksAt)) {
    ticksAt <- pretty(ylim)
    labels <- ticksAt
  }
  graphics::axis(2, at = mapY(ticksAt), labels = labels, xpd = TRUE, las = ef.size.las)
  graphics::mtext(ef.size.label, side = 2, at = mapY(mean(ylim)), line = 3)

  # Difference = 0 line, i.e. no effect
  graphics::lines(usr[1:2], c(mapY(0), mapY(0)), col = "grey50", lty = 3, xpd = TRUE)
  for (i in seq_along(plotDiffs)) {
    pwes <- plotDiffs[[i]]
    if (!is.null(pwes)) {
      gid1 <- which(groups == pwes$groups[1])
      gid2 <- which(groups == pwes$groups[2])
      plotEffectSize(pwes, gid1 + central.tendency.dx[gid1], pwes$t0, showViolin, violinCol, violin.width, violin.shape, ef.size.col, ef.size.pch, mapY, xpd = TRUE)
      graphics::text(gid1 + central.tendency.dx[gid1], mapY(ylim[1]), sprintf("%s\nminus\n%s", pwes$groupLabels[1], pwes$groupLabels[2]), xpd = TRUE, pos = 1)
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
#' parameters with the same prefix. To display a type, for example box plots,
#' specify \code{box = TRUE}. Rather than \code{TRUE}, you may specify a colour,
#' which is used as the border/outline for the boxes. You may also specify a
#' vector of colours, one for each group. For \code{points}, you may specify a
#' colour for each individual point. When colours are not specified, they are
#' selected from an \code{\link{RColorBrewer}} qualitative palette.
#'
#' @param es Data returned from a call to \code{\link{SAKDifference}}
#'
#' @param group.dx Used to shift group centres horizontally. E.g.,
#'   \code{group.dx = c(0.1, -0.1)} will group into pairs. Individual components
#'   can be shifted independently using the appropriate \code{*.dx} parameters.
#'
#' @param points Colour of individual points. If FALSE, points are not plotted,
#'   TRUE plots applies a default colour. May be a vector of colours. If length
#'   1, all points are drawn with the specified colour. If length is the number
#'   of groups, points in each group are drawn with the appropriate colour.
#'   Otherwise, \code{points} should be a vector of colours with a value for
#'   each data point.
#' @param points.method Method used to avoid overplotting points. Use
#'   \code{"overplot"} to overplot points and \code{"jitter"} to add random
#'   noise to each x-value. See [vipor]{offsetX} for remaining methods.
#' @param pch Symbol to use for plotting points.
#' @param points.dx Horizontal shift to be applied to points in each group.
#' @param points.params List of named parameters to pass on to
#'   [graphics]{points}, e.g. \code{SAKPlot(es, points = "black", pch = 21,
#'   points.params = list(bg = as.numeric(factor(data$Sex)) + 1))}.
#'
#' @param violin If FALSE, violin plot is not drawn. Otherwise specifies the
#'   colour of the violin borders.
#' @param violin.fill Colour used to fill violins.
#' @param violin.adj Value used to control violin plot smoothness by adjusting
#'   the kernel density bandwidth. Higher values produce a smoother plot.
#' @param violin.width Width of maximum violin horizontal extents, as a
#'   proportion of the distance between groups.
#' @param violin.trunc Numeric value that specifies what vertical proportion of
#'   the violin is truncated.
#' @param violin.shape Desired violin shape - left-half only (\code{"left"}),
#'   right-half only (\code{"right"}), or a full violin (\code{"full"}).
#' @param violin.dx Horizontal shift to be applied to each violin.
#'
#' @param box If not FALSE, draw a box-and-whisker plot of the grouped values.
#'   Value may be a colour, in which case the box borders are plotted with the
#'   colour(s). See [graphics]{boxplot}.
#' @param box.fill Colour used to fill the bodies of the box-and-whisker plot.
#'   If FALSE or NA, bodies are not filled
#' @param box.notch If TRUE, draws notches in the sides of the boxes. See
#'   [graphics]{boxplot.stats} for the calculations used.
#' @param box.pars List with additional graphical parameters to control the box
#'   plot. See [graphics]{bxp} graphical parameters for a complete list.
#' @param box.dx Horizontal shift to be applied to each box.
#'
#' @param bar If not FALSE, draw a bar plot of the group means or medians,
#'   according to \code{central.tendency}. May be \code{TRUE} or a colour.
#' @param bar.fill Colour used to fill bars.
#' @param bar.width Width of bars.
#' @param bar.dx Horizontal shift to be applied to each bar.
#'
#' @param ef.size If not FALSE, effect sizes are plotted. May be \code{TRUE} or
#'   a colour.
#' @param ef.size.position Effect sizes are plotted to the right of the main
#'   plot if there is only one effect size to plot and \code{ef.size.position !=
#'   "below"}. If the effect size is drawn to the right, you will need to
#'   increase the size of the right margin before plotting (see
#'   \code{\link[graphics:par]{par(mar = ...)}}).
#' @param ef.size.violin If not FALSE, boostrapped effect size estimates are
#'   show as a violin plot. May be a colour, used for the violin border, and a
#'   transparent version is used for the violin fill.
#' @param ef.size.violin.shape Shape of the effect size violin.
#' @param ef.size.pch Symbol to represent mean effect size.
#' @param ef.size.dx Horizontal shift to be applied to each effect size.
#' @param ef.size.ticks Optional locations and labels for ticks on the effect
#'   size y-axis. E.g. to interpret effect size using Cohen's default values,
#'   specif \code{ef.size.ticks = c("Large negative effect" = -0.8, "Medium
#'   negative effect" = -0.5, "Small negative effect" = -0.2, "No effect" = 0,
#'   "Small positive effect" = 0.2, "Medium positive effect" = 0.5, "Large
#'   positive effect" = 0.8)}
#' @param ef.size.las Orientation of tick labels on the effect size axis (0 =
#'   parallel to axis, 1 = horizontal).
#' @param ef.size.label Label to display on y-axis for effect size.
#'
#' @param paired If \code{TRUE}, lines are drawn joining the individual data
#'   points.
#' @param central.tendency If not FALSE, a visual indicator of central tendency
#'   is drawn. May be a colour, in which case it is used for mean/median and
#'   error bars.
#' @param central.tendency.type Should the indicated measure of central tendency
#'   be \code{"mean"} or \code{"median"}?
#' @param central.tendency.dx Horizontal shift to apply to central tendency
#'   indicator and error bars.
#' @param error.bars Should error bars be displayed? May be the colour to be used for error bars.
#' @param error.bars.type Should error bars depict 95%% confidence intervals
#'   (\code{"CI"}), standard deviation (\code{"SD"}) or standard error
#'   (\code{"SE"})?
#'
#' @param axis.dx Horizontal shifts to be applied to each x-axis tick and label.
#'
#' @param left.ylab Left-hand y-axis label.
#' @param left.las Orientation of axis labels on left-hand y-axis label (0 = parallel to axis, 1 = horizontal).
#'
#' @param ... Additional arguments are passed on to the [graphics]{plot} function.
#'
#' @return \code{es} invisibly.
#'
#' @seealso \code{\link{SAKDifference}}, \code{\link[vipor]{offsetX}}, \code{\link[graphics]{boxplot}},
#'   \code{\link[graphics]{bxp}}
#'
#' @references
#'
#' Gardner, M. J., & Altman, D. G. (1986). Confidence intervals rather than P
#' values: estimation rather than hypothesis testing. Br Med J (Clin Res Ed),
#' 292(6522), 746-750. doi:10.1136/bmj.292.6522.746
#'
#' Cumming, G. (2012). Understanding the new statistics : effect sizes,
#' confidence intervals, and meta-analysis (1st edition ed.). New York:
#' Routledge.
#'
#' @export
SAKPlot <- function(es,

                    group.dx = 0,

                    points = TRUE,
                    points.method = c("quasirandom", "pseudorandom", "smiley", "maxout", "frowney", "minout", "tukey",
                                      "tukeyDense", "jitter", "overplot"),
                    pch = 19,
                    points.dx = group.dx,
                    points.params = list(),

                    violin = isFALSE(box) && isFALSE(bar),
                    violin.shape = c("left-half", "right-half", "full"),
                    violin.fill = TRUE,
                    violin.adj = 1.5,
                    violin.width = 0.35,
                    violin.trunc = TRUE,
                    violin.dx = group.dx,

                    box = FALSE,
                    box.fill = TRUE,
                    box.notch = FALSE,
                    box.pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5),
                    box.dx = group.dx,

                    bar = FALSE,
                    bar.fill = TRUE,
                    bar.width = 0.8,
                    bar.dx = group.dx,

                    ef.size = TRUE,
                    ef.size.position = c("right", "below"),
                    ef.size.violin = TRUE, # if TRUE, draw effect size confidence interval
                    ef.size.violin.shape = c("right-half", "left-half", "full"),
                    ef.size.pch = 17,
                    ef.size.dx = group.dx,
                    ef.size.ticks = NULL,
                    ef.size.las = 0,
                    ef.size.label = es$effect.name,

                    paired = es$paired.data, # if true draw lines between paired points

                    central.tendency = TRUE,
                    central.tendency.type = c("mean", "median"),
                    central.tendency.dx = group.dx,
                    error.bars = central.tendency,
                    error.bars.type = c("CI", "SD", "SE"), # draw confidence interval line of the data; if box, density, violin is TRUE, CI is FALSE

                    axis.dx = group.dx,
                    left.ylab = es$data.col.name,
                    left.las = 0,
                    #col = c("col1", "col2", "col3"), opacity = 0.6, #colour of box, violin, box border, density, col 1 = group 1, col2 = group 2, col3 = ef plot, {col = n+1, n = group no) #
                    ...
) {

  # We allow TRUE/FALSE or colours to be specified for many values. TRUE is equivalent to a default colour
  .boolToDef <- function(arg, def) if (isTRUE(arg)) { def } else { arg }
  .show <- function(what) !isFALSE(what) && !is.null(what)
  .isColour <- function(c) tryCatch(is.matrix(grDevices::col2rgb(c)), error = function(e) FALSE)
  .colour <- function(what) if (.isColour(what)) { what } else { NA }
  # Extend a vector so that it has length nGroups
  .extend <- function(x) rep_len(x, nGroups)

  # Check and process input parameters
  if (!methods::is(es, "SAKDiff"))
    stop("data must be a SAKDiff object")
  if (!isFALSE(violin)) {
    # This is tricky - we want to allow multiple shapes, but default to just the
    # first. That's what several.ok = !missing(violin.shape) does
    violin.shape <- match.arg(violin.shape, several.ok = !missing(violin.shape))
  }
  error.bars.type <- match.arg(error.bars.type)
  ef.size.position <- match.arg(ef.size.position)
  ef.size.violin.shape <- match.arg(ef.size.violin.shape)
  # Default for points.method depends on whether plot is paired
  if (missing(points.method) && paired)
    points.method <- "overplot"
  else
    points.method <- match.arg(points.method)
  central.tendency.type <- match.arg(central.tendency.type)

  data <- es$data
  groups <- es$groups
  nGroups <- length(groups)

  # Recycle all the *.dx arguments
  box.dx <- .extend(box.dx)
  bar.dx <- .extend(bar.dx)
  central.tendency.dx <- .extend(central.tendency.dx)
  axis.dx <- .extend(axis.dx)
  violin.dx <- .extend(violin.dx)
  points.dx <- .extend(points.dx)
  ef.size.dx <- .extend(ef.size.dx)

  # Prepare some palettes, the border palette has no transparency, the fill palette is 80% transparent
  defBorderPalette <- pickPalette(nGroups)
  defFillPalette <- SAKTransparent(defBorderPalette, 0.8)

  # Calculate densities for violin plots
  densities <- lapply(groups, getGroupDensity, es, violin.adj, violin.trunc, violin.width)

  # Calculate plot limits
  xlim <- c(0.5, nGroups + 0.5)
  rowsToBePlotted <- data[[es$group.col]] %in% groups
  ylim <- range(data[[es$data.col]][rowsToBePlotted])

  # If needed, extend y range to encompass violin plots
  if (.show(violin)) {
    for (d in densities) {
      ylim <- range(ylim, d$x)
    }
  }

  # If needed, extend y range to encompass bar plots
  if (.show(bar)) {
    if (.show(points)) {
      ylim <- c(0, max(data[[es$data.col]][rowsToBePlotted]))
    } else {
      # Are we drawing error bars?
      if (.show(error.bars)) {
        ym <- max(sapply(seq_along(groups), function(gi) {
          groupMean <- mean(data[[es$data.col]][data[[es$group.col]] == groups[gi]])
          getErrorBars(es, gi, groupMean, error.bars.type)[2]
        }))
      } else {
        # Get means of each group
        ym <- max(sapply(groups, function(g) mean(data[[es$data.col]][data[[es$group.col]] == g])))
      }
      ylim <- c(0, ym)
    }
  }

  # If needed extend x range to encompass effect size
  if (nGroups < 2) {
    ef.size <- FALSE # There's nothing to see here
  } else if (nGroups > 2) {
    # Can't show more than one effect size to the right
    ef.size.position <- "below"
  }
  if (.show(ef.size)) {
    if (ef.size.position == "right") {
      # Extend x-axis to accommodate effect size
      xlim[2] <- xlim[2] + 0.7
    } else {
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
       xaxt = "n", xlab = "", ylab = left.ylab, las = left.las, ...)
  # Label the groups along the x-axis
  graphics::axis(1, at = seq_len(nGroups) + axis.dx, labels = es$group.names)

  ### Add the various components to the plot ###
  # Turn group column into a factor so it can be ordered by the user
  data$.group.as.factor <- factor(data[[es$group.col]], levels = groups)
  f <- stats::as.formula(paste(es$data.col.name, "~.group.as.factor"))

  # Box plot
  if (.show(box)) {
    box <- .boolToDef(box, defBorderPalette)
    box.fill <- .boolToDef(box.fill, defFillPalette)
    graphics::boxplot(f, data = data, at = seq_len(nGroups) + box.dx,
                      add = TRUE, axes = FALSE, notch = box.notch,
                      col = .colour(box.fill), border = .colour(box), pars = box.pars)
  }

  # bar chart
  if (.show(bar)) {
    bar <- .boolToDef(bar, defBorderPalette)
    bar.fill <- .boolToDef(bar.fill, defFillPalette)
    bar.width <- .extend(bar.width)
    space <- c(0.75, rep(0.25, nGroups - 1))
    # reverse engineer space from x positions and widths of bars
    gap <- diff(c(-bar.width[1] / 2, seq_len(nGroups) + bar.dx))
    space <- sapply(seq_along(gap), function(i) (gap[i] - bar.width[i]) / bar.width[i])
    graphics::barplot(es$group.statistics[, central.tendency.type] ~ factor(groups, levels = groups),
                      width = bar.width, space = space, offset = bar.dx,
                      col = .colour(bar.fill), border = .colour(bar),
                      add = TRUE, axes = FALSE, names.arg = FALSE)
  }

  # Violin plots
  if (.show(violin)) {
    violin <- .boolToDef(violin, defBorderPalette)
    borders <- .extend(violin)
    violin.fill <- .extend(.boolToDef(violin.fill, defFillPalette))
    violin.shape <- .extend(violin.shape)
    dx <- violin.dx
    for (i in seq_along(groups)) {
      d <- densities[[i]]
      col <- violin.fill[i]
      border <- borders[i]
      plotViolin(violin.shape[i], i + dx[i], d, col = col, border = border)
    }
  }

  # Scatter plot of data points
  if (.show(points)) {
    palette <- SAKTransparent(defBorderPalette, .4)
    pointCol <- .boolToDef(points, palette[as.numeric(data$.group.as.factor)])
    if (length(pointCol) == nGroups) {
      # If colours specified for each group, expand out to the colours for each point
      pointCol <- pointCol[as.numeric(data$.group.as.factor)]
    }

    x <- as.numeric(data$.group.as.factor)
    # Optional shift
    x <- x + points.dx[x]

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
    if (!es$paired.data)
      stop("To plot paired lines, data must be, i.e. id.col specified to SAKDifferences")
    paired <- .boolToDef(paired, "grey20")
    # TODO what pairs should we join?
    # For now, display all contrasts, which can get very ugly if there's more than one
    for (i in seq_len(length(es$group.differences))) {
      idx1 <- es$group.differences[[i]]$groupIndices[1]
      idx2 <- es$group.differences[[i]]$groupIndices[2]
      p1 <- data[[es$data.col]][data[[es$group.col]] == groups[idx1]]
      p2 <- data[[es$data.col]][data[[es$group.col]] == groups[idx2]]
      graphics::segments(idx1 + points.dx[idx1], p1,
                         idx2 + points.dx[idx2], p2,
                         col = SAKTransparent(paired, .7),
                         lty = 1, lwd = 1)
    }
  }

  ## Mean/median
  central.tendency <- .boolToDef(central.tendency, "grey20")
  error.bars <- .boolToDef(error.bars, if (.isColour(central.tendency)) central.tendency else "grey20")
  if (.show(central.tendency)) {
    for (i in seq_along(groups)) {
      # Get estimate of central tendency
      y <- es$group.statistics[i, central.tendency.type]
      # Plot lines TODO allow symbols? More control of line?
      graphics::segments(i - violin.width + central.tendency.dx[i], y,
                         i + violin.width + central.tendency.dx[i], y,
                         col = .colour(central.tendency), lwd = 2)
    }
  }

  ## add CI/SD/SE error bars
  if (.show(error.bars)) {
    for (i in seq_along(groups)) {
      y <- es$group.statistics[i, central.tendency.type]
      bars <- getErrorBars(es, i, y, error.bars.type)
      graphics::segments(i + central.tendency.dx[i], bars[1], i + central.tendency.dx[i], bars[2], col = .colour(error.bars), lty = 1, lwd = 2)
    }
  }

  # Effect size. Handle default colour
  ef.size.col <- .boolToDef(ef.size, "black")
  violinCol <- .boolToDef(ef.size.violin, "grey40")
  if (.show(ef.size) && ef.size.position == "right") {
    plotEffectSizesRight(es, ef.size.col, ef.size.pch, .show(ef.size.violin), violinCol, violin.width, ef.size.violin.shape, ef.size.label, ef.size.ticks, ef.size.las)
  } else if (.show(ef.size) && ef.size.position == "below") {
    plotEffectSizesBelow(es, ef.size.col, ef.size.pch, .show(ef.size.violin), violinCol, violin.width, ef.size.violin.shape, xlim, ef.size.dx, ef.size.label, ef.size.ticks, ef.size.las)
  }

  invisible(es)
}
