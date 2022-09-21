### Private functions

#### TODO
# Perhaps don't do these, just require people to create more than 1 plot:
#### How should we handle paired data with more than 2 groups? eg petunia
#### How should we handle more than 1 comparison per group? E.g. all pairwise combinations




# Returns the negation of the specified group difference (type DurgaGroupDiff,
# usually a member of es$group.differences). I.e. changes "group1 - group2" to
# "group2 - group1"
negatePairwiseDiff <- function(pwd) {
  pwd$groups <- rev(pwd$groups)
  pwd$groupLabels <- rev(pwd$groupLabels)
  pwd$groupIndices <- rev(pwd$groupIndices)
  pwd$t0 <- -pwd$t0
  pwd$t <- -pwd$t
  pwd$bca[4] <- -pwd$bca[4]
  pwd$bca[5] <- -pwd$bca[5]
  pwd
}

# Given a pair of groups, finds (in diffs) the comparison for pair[1] - pair[2].
# If the comparison pair[2] - pair[1] is found, it is negated and returned.
findDiff <- function(pair, label, diffs) {
  # For each existing comparison...
  for (diff in diffs) {
    # Is this the requested comparison?
    if (diff$groups[1] == pair[1] && diff$groups[2] == pair[2]) {
      attr(diff, "label") <- label
      return(diff)

      # Is this the negation of the requested comparison?
    } else if (diff$groupLabels[1] == pair[2] && diff$groupLabels[2] == pair[1]) {
      attr(diff, "label") <- label
      return(negatePairwiseDiff(diff))
    }
  }
  stop(sprintf("Contrast '%s - %s' has not been estimated, check the contrasts argument in your call to DurgaDiff",
               pair[1], pair[2]))
}

# Returns the label to be used to represent the specified diff
getDiffLabel <- function(pwes) {
  label <- attr(pwes, "label")
  if (is.null(label) || label == "")
    label <- sprintf("%s\nminus\n%s", pwes$groupLabels[1], pwes$groupLabels[2])
  label
}

# Given a string representation of the required contrasts, returns a list of
# DurgaGroupDiff objects, one for each contrast. The DurgaGroupDiff objects are extracted
# from es$group.differences, so must already have been calculated by
# DurgaDiff
buildPlotDiffs <- function(contrasts, es) {
  pairs <- expandContrasts(contrasts, es$groups)
  # For each specified contrast, find the corresponding group difference
  lapply(seq_len(ncol(pairs)), function(i) findDiff(pairs[, i], colnames(pairs)[i], es$group.differences))
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

# Function to return a palette of \code{n} colours, with transparency \code{alpha}.
pickPalette <- function(n, pal = "Set2") {
  # Try to use an RColorBrewer palette
  if (n > RColorBrewer::brewer.pal.info[pal, "maxcolors"]) {
    # This is complicated because we want to "randomly" sample available
    # colours, but in a repeatable way, and without interfering with user's
    # random number generation, hence we hardwire a set of "random" numbers
    i <- c( 86L, 87L, 103L, 120L, 52L, 184L, 154L, 80L, 56L, 69L, 179L,
           79L, 19L, 150L, 170L, 147L, 47L, 27L, 97L, 92L, 182L, 72L, 141L,
           146L, 48L, 162L, 42L, 188L, 104L, 20L, 13L, 128L, 61L, 30L, 121L,
           22L, 110L, 187L, 82L, 1L, 85L, 197L, 39L, 163L, 8L, 35L, 78L,
           164L, 71L, 113L, 153L, 24L, 49L, 173L, 62L, 70L, 45L, 40L, 31L,
           192L, 5L, 95L, 144L, 65L, 14L, 57L, 166L, 149L, 183L, 161L, 108L,
           133L, 125L, 198L, 63L, 131L, 109L, 199L, 175L, 10L, 159L, 200L,
           64L, 102L, 43L, 193L, 77L, 46L, 3L, 6L, 130L, 194L, 171L, 195L,
           145L, 168L, 158L, 111L, 99L, 16L, 185L, 93L, 118L, 59L, 7L, 126L,
           160L, 148L, 23L, 157L, 138L, 37L, 156L, 107L, 105L, 75L, 129L,
           60L, 139L, 33L, 178L, 73L, 41L, 180L, 190L, 54L, 2L, 9L, 143L,
           12L, 55L, 21L, 127L, 136L, 81L, 76L, 67L, 189L, 15L, 88L, 167L,
           135L, 91L, 74L, 174L, 181L, 28L, 165L, 169L, 100L, 155L, 11L, 4L,
           177L, 119L, 17L, 83L, 152L, 191L, 123L, 34L, 53L, 151L, 137L,
           134L, 89L, 96L, 176L, 29L, 44L, 140L, 172L, 186L, 196L, 132L,
           122L, 25L, 68L, 106L, 114L, 116L, 50L, 94L, 51L, 26L, 38L, 115L,
           112L, 101L, 142L, 66L, 84L, 18L, 90L, 32L, 36L, 58L, 117L, 124L,
           98L)
    if (n < length(i))
      i <- Filter(function(x) x <= n, i)
    i <- rep(i, length.out = n)
    # Too many colours, concatenate some palettes (thanks to https://stackoverflow.com/a/33144808/1287461)
    qual_col_pals <- RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual', ]
    col_vector <- unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    col_vector[i]
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

# Function to add text and tick marks to the x-axis, applying consistent spacing
# etc.. Labels are positioned and aligned along the top of the bounding box, so
# multi-line labels descend lower
labelXAxis <- function(at, labels, tick) {
  graphics::axis(1, tick = tick, at = at, labels = labels, padj = 1, mgp = c(3, 0, 0))
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
#   specified, no mapping is performed.
plotEffectSize <- function(pwes, xo, centreY, showViolin, violinCol, violin.width, violin.shape,
                           ef.size.col, ef.size.pch, mapYFn = identity, xpd = FALSE) {
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
    fill <- DurgaTransparent(violinCol, .8)
    plotViolin(violin.shape, xo, d, col = fill, border = violinCol, xpd = xpd)
  }

  # Draw mean of effect size
  graphics::points(xo, mapYFn(pwes$t0 + deltaY), pch = ef.size.pch, col = ef.size.col, cex = 1.5, xpd = xpd)
  # Confidence interval of effect size
  graphics::segments(xo, mapYFn(pwes$bca[4] + deltaY), xo, mapYFn(pwes$bca[5] + deltaY),
           col = ef.size.col, lty = 1, lwd = 2.0, xpd = xpd)
}

# Plot effect size to the right of the main plot. Only useful when showing a single effect size
plotEffectSizesRight <- function(es, pwes, ef.size.col, ef.size.pch,
                                 showViolin, violinCol, violin.width, violin.shape,
                                 axisLabel, ticksAt, ef.size.las,
                                 groupX) {

  # Get the means of the 2 groups
  gid1 <- pwes$groupIndices[1]
  gid2 <- pwes$groupIndices[2]
  y <- es$group.statistics[gid1, 1]
  y2 <- es$group.statistics[gid2, 1]

  x <- length(es$groups) + 1

  if (es$effect.type == "unstandardised") {
    esRange <- range(c(0, pwes$t))
    plotEffectSize(pwes, x, y, showViolin, violinCol, violin.width, violin.shape, ef.size.col, ef.size.pch)

    # Axis labels on right-hand
    labels <- names(ticksAt)
    if (is.null(ticksAt)) {
      ticksAt <- pretty(esRange)
      labels <- ticksAt
    }
    graphics::axis(4, at = y2 + ticksAt, labels = labels, las = ef.size.las)
  } else {
    esRange <- range(c(0, pwes$t0))
    ylim <- range(y, y2)
    # Function to map esRange to ylim
    mapY <- function(y) {
      # Interpolate/extrapolate along the line (esRange[1], ylim[1]), (esRange[2], ylim[2])
      ylim[1] + (y - esRange[1]) * (ylim[2] - ylim[1]) / (esRange[2] - esRange[1])
    }
    plotEffectSize(pwes, x, pwes$t0, showViolin, violinCol, violin.width, violin.shape, ef.size.col, ef.size.pch, mapYFn = mapY)

    # Axis labels on right-hand
    labels <- names(ticksAt)
    if (is.null(ticksAt)) {
      ticksAt <- pretty(esRange)
      labels <- ticksAt
    }
    graphics::axis(4, at = mapY(ticksAt), labels = labels, las = ef.size.las)
  }

  # Horizontal lines from group means
  graphics::segments(groupX[gid1], y, x + 2, y, col = "grey50", lty = 1, lwd = 1.5)
  graphics::segments(groupX[gid2], y2, x + 2, y2, col = "grey50", lty = 1, lwd = 1.5)

  # Add x-axis label and tick mark for effect size
  labelXAxis(at = x, labels = getDiffLabel(pwes), tick = TRUE)

  # Label the right y-axis
  graphics::mtext(axisLabel, side = 4, line = 2.5, cex = graphics::par("cex"))
}

# Plot effect size below the main plot. Assumes that bottom margin is large
# enough to accommodate the effect size plot
plotEffectSizesBelow <- function(es, plotDiffs, ef.size.col, ef.size.pch,
                                 showViolin, violinCol, violin.width, violin.shape,
                                 xlim, central.tendency.dx, ef.size.label, ticksAt, ef.size.las) {
  groups <- es$groups

  # What will we plot?
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
  graphics::mtext(ef.size.label, side = 2, at = mapY(mean(ylim)), line = 3, cex = graphics::par("cex"))

  # Plot the "Difference = 0" line, i.e. no effect
  graphics::lines(usr[1:2], c(mapY(0), mapY(0)), col = "grey50", lty = 3, xpd = TRUE)

  # Plot all diffs
  for (i in seq_along(plotDiffs)) {
    pwes <- plotDiffs[[i]]
    if (!is.null(pwes)) {
      gid1 <- which(groups == pwes$groups[1])
      gid2 <- which(groups == pwes$groups[2])
      plotEffectSize(pwes, gid1 + central.tendency.dx[gid1], pwes$t0, showViolin, violinCol, violin.width, violin.shape, ef.size.col, ef.size.pch, mapY, xpd = TRUE)
      graphics::text(gid1 + central.tendency.dx[gid1], mapY(ylim[1]), getDiffLabel(pwes), xpd = TRUE, pos = 1)
    }
  }
}

# Convenience operator like Ruby's || operator. Returns a if it is not null, otherwise b
`%||%` <- function(a, b) if (!is.null(a)) a else b

#############################################################################

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
DurgaTransparent <-  function(colour, alpha) {
  rgba.val <- grDevices::col2rgb(colour, TRUE)
  grDevices::rgb(rgba.val[1, ], rgba.val[2, ], rgba.val[3, ],
                 maxColorValue = 255,
                 alpha = (100 - alpha * 100) * 255 / 100)
}

#' Group and effect size plotting in base R.
#'
#' Plot grouped data and effect size in base R, with control over a large range
#' of possible display formats and options. To plot your data, first calculate
#' group differences by calling \code{\link{DurgaDiff}}, then pass the result to
#' \code{\link{DurgaPlot}}. Parameters are grouped according to the component
#' they affect, so all parameters that affect box plots are prefixed with
#' \code{box}.
#'
#' Group data may be visualised in multiple ways: \code{points}, \code{violin},
#' \code{box} and \code{bar}. Each visualisation type is controlled by a set of
#' parameters with the same prefix. To display a type, for example box plots,
#' specify \code{box = TRUE}. Rather than \code{box = TRUE}, you may specify a
#' colour (e.g. \code{box \ "blue"}), which is used as the border/outline for
#' the boxes. You may also specify a vector of colours, one for each group. For
#' \code{points}, you may specify a colour for each individual point. When
#' colours are not specified, they are selected from an
#' \code{\link{RColorBrewer}} qualitative palette.
#'
#' Group data annotations are controlled with parameters \code{central.tendency}
#' and \code{error.bars}. \code{central.tendency} visually represents the mean
#' or median (\code{central.tendency.type}) of each group, while
#' \code{error.bars} are vertical bars showing the 95%% CI of the mean, standard
#' deviation or standard error of the groups (\code{error.bars.type}).
#'
#' An effect size (for our purposes) is the difference in means between two
#' groups. Effect size display is controlled by \code{ef.size}. The set of
#' effect sizes (aka "contrasts") to be plotted is controlled by the
#' \code{contrasts} parameter. If a single effect size is displayed, it may be
#' positioned to the right of - or below - the main plot
#' (\code{ef.size.position}). If more than one effect size is displayed, it must
#' be below the main plot. If below, an effect size is drawn underneath its
#' primary group. There is currently no way to display multiple effect sizes for
#' a single primary group.
#'
#' Custom labels for effects can be specified as part of the \code{contrasts}
#' parameter. If \code{contrasts} is a named vector, the names are used as
#' contrast labels, e.g. \code{contrasts = c("Adult change" = "adult - control",
#' "Juvenile change" = "juvenile - control")}.
#'
#' The \code{contrasts} parameter may be a single string, a vector of strings,
#' or a matrix. A single string has a format such as \code{"group1 - group2,
#' group3 - group4"}. A single asterisk, \code{"*"} creates contrasts for all
#' possible pairs of groups. A single string such as \code{".- control"}
#' compares all groups against the \code{"control"} group, i.e. the \code{"."}
#' expands to all groups except the named group. A vector of strings looks like
#' \code{c("group1 - group2", "group3 - group4")}. If a matrix is specified, it
#' must have a column for each contrast, with the first group in row 1 and the
#' second in row 2. See also the \code{contrasts} parameter to
#' \code{\link{DurgaDiff}}. It is an error to attempt to plot a contrast that
#' was not estimated by \code{\link{DurgaDiff}}.
#'
#'
#' @param es Data returned from a call to \code{\link{DurgaDiff}}
#'
#' @param contrasts Set of contrasts (i.e. group comparisons) to be plotted.
#'   Defaults to contrasts passed to \code{\link{DurgaDiff}}, otherwise \code{".
#'   - group1"} (where \code{group1} is the first group). See Details for more
#'   information.
#'
#' @param group.dx Used to shift group centres horizontally. E.g.,
#'   \code{group.dx = c(0.1, -0.1)} will group into pairs. Individual components
#'   can be shifted independently using the appropriate \code{*.dx} parameters.
#'
#' @param points If not FALSE, points are plotted. If \code{TRUE}, points are
#'   displayed with a default colour. You may specify a vector of colours; if
#'   length 1, all points are drawn with the specified colour. If length is less
#'   than the number of data points, points in each group are drawn with the
#'   appropriate colour (extra colours are ignored). Otherwise, \code{points}
#'   should be a vector of colours with a value for each data point.
#' @param points.method Method used to avoid overplotting points. Use
#'   \code{"overplot"} to overplot points and \code{"jitter"} to add random
#'   noise to each x-value. See \code{\link[vipor]{offsetX}} for remaining
#'   methods.
#' @param points.dx Horizontal shift to be applied to points in each group.
#' @param points.params List of named parameters to pass on to
#'   \code{\link[graphics]{points}}, e.g. \code{DurgaPlot(es, points = "black",
#'   points.params = list(pch = 21, bg = as.numeric(factor(data$Sex)) + 1))}.
#'
#' @param violin If not FALSE, violin plots are drawn. If \code{TRUE}, violins
#'   are drawn in default colours. Otherwise specifies the colour of the violin
#'   borders.
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
#'   colour(s). See \code{\link[graphics]{boxplot}}.
#' @param box.fill Colour used to fill the bodies of the box-and-whisker plot.
#'   If FALSE or NA, bodies are not filled.
#' @param box.outline If FALSE, don't draw outliers with the box plot.
#' @param box.notch If TRUE, draws notches in the sides of the boxes. See
#'   \code{\link[grDevices]{boxplot.stats}} for the calculations used.
#' @param box.pars List with additional graphical parameters to control the box
#'   plot. See \code{\link[graphics]{bxp}} graphical parameters for a complete
#'   list.
#' @param box.dx Horizontal shift to be applied to each box.
#'
#' @param bar If not FALSE, draw a bar plot of the group means or medians,
#'   according to \code{central.tendency}. May be \code{TRUE} or a colour.
#' @param bar.fill Colour used to fill bars.
#' @param bar.width Width of bars.
#' @param bar.dx Horizontal shift to be applied to each bar.
#'
#' @param central.tendency If not FALSE, a visual indicator of central tendency
#'   is drawn. May be a colour, in which case it is used for mean/median and
#'   error bars.
#' @param central.tendency.type Should the indicated measure of central tendency
#'   be \code{"mean"} or \code{"median"}?
#' @param central.tendency.symbol Should central tendency be shown as a point or
#'   a horizontal line segment?
#' @param central.tendency.width Width of the central tendency line segment.
#' @param central.tendency.params Additional arguments to be passed to
#'   \code{\link[graphics]{points}} (if \code{central.tendency.symbol ==
#'   "point"}) or \code{\link[graphics]{segments}} (if
#'   \code{central.tendency.symbol == "segment"}).
#' @param central.tendency.dx Horizontal shift to apply to central tendency
#'   indicator and error bars.
#'
#' @param error.bars Should error bars be displayed? May be the colour to be
#'   used for error bars.
#' @param error.bars.type Should error bars depict 95%% confidence intervals of
#'   the mean (\code{"CI"}), standard deviation (\code{"SD"}) or standard error
#'   (\code{"SE"})?
#' @param error.bars.cross.width Length (in inches) of the horizontal crossbars
#'   at the ends of the error bars. If 0, no crossbar is drawn.
#'
#' @param paired If \code{TRUE}, lines are drawn joining the individual data
#'   points.
#' @param paired.lty Line style for pair lines.
#' @param paired.lwd Line width for pair lines.
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
#' @param ef.size.adj.margin If TRUE (the default), the right margin (if ES is
#'   right) or bottom margin (if ES is below) is automatically adjusted to make
#'   room to display the effect size or axis annotations. The margins are
#'   restored before control returns from \code{DurgaPlot}.
#'
#' @param x.axis if TRUE, display the x-axis.
#' @param x.axis.dx Horizontal shifts to be applied to each x-axis tick and
#'   label.
#'
#' @param left.ylab Left-hand y-axis label.
#' @param left.las Orientation of axis labels on left-hand y-axis label (0 =
#'   parallel to axis, 1 = horizontal).
#' @param add If TRUE, the effect size plot is added to the current plot. If
#'   FALSE, a new plot is created.
#' @param xlim,ylim If specified, overrides the default plot extents.
#'
#' @param ... Additional arguments are passed on to the
#'   \code{\link[graphics]{plot}} function.
#'
#' @return A matrix with the x-axis locations and y-axis extents of each
#'   displayed group (returned invisibly).
#'
#' @seealso \code{\link{DurgaDiff}}, \code{\link[vipor]{offsetX}},
#'   \code{\link[graphics]{boxplot}}, \code{\link[graphics]{bxp}}
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
DurgaPlot <- function(es,

                    contrasts,

                    group.dx = 0,

                    points = TRUE,
                    points.method = c("quasirandom", "pseudorandom", "smiley", "maxout", "frowney", "minout", "tukey",
                                      "tukeyDense", "jitter", "overplot"),
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
                    box.outline = TRUE,
                    box.notch = FALSE,
                    box.pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5),
                    box.dx = group.dx,

                    bar = FALSE,
                    bar.fill = TRUE,
                    bar.width = 0.8,
                    bar.dx = group.dx,

                    ef.size = TRUE,
                    ef.size.position = c("right", "below"),
                    ef.size.violin = TRUE,
                    ef.size.violin.shape = c("right-half", "left-half", "full"),
                    ef.size.pch = 17,
                    ef.size.ticks = NULL,
                    ef.size.las = 0,
                    ef.size.label = es$effect.name,
                    ef.size.dx = group.dx,
                    ef.size.adj.margin = TRUE,

                    paired = es$paired.data, # if true draw lines between paired points
                    paired.lty = 1,
                    paired.lwd = 1,

                    central.tendency = isFALSE(box) && isFALSE(bar),
                    central.tendency.type = c("mean", "median"),
                    central.tendency.symbol = c("point", "segment"),
                    central.tendency.width = violin.width,
                    central.tendency.params = list(),
                    central.tendency.dx = group.dx,

                    error.bars = !isFALSE(central.tendency) || !isFALSE(bar),
                    error.bars.type = c("CI", "SD", "SE"),
                    error.bars.cross.width = 0,

                    x.axis = TRUE,
                    x.axis.dx = group.dx,

                    left.ylab = es$data.col.name,
                    left.las = 0,
                    add = FALSE,
                    xlim, ylim,
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
  if (!methods::is(es, "DurgaDiff"))
    stop("data must be a DurgaDiff object")
  if (!isFALSE(violin)) {
    # This is tricky - we want to allow multiple shapes, but default to just the
    # first. That's what several.ok = !missing(violin.shape) does
    violin.shape <- match.arg(violin.shape, several.ok = !missing(violin.shape))
  }
  error.bars.type <- match.arg(error.bars.type)
  ef.size.position <- match.arg(ef.size.position)
  ef.size.violin.shape <- match.arg(ef.size.violin.shape)
  # Default for points.method depends on whether plot is paired
  if (missing(points.method) && .show(paired))
    points.method <- "overplot"
  else
    points.method <- match.arg(points.method)
  central.tendency.type <- match.arg(central.tendency.type)
  central.tendency.symbol <- match.arg(central.tendency.symbol)

  data <- es$data
  groups <- es$groups
  nGroups <- length(groups)

  # What contrasts are to be displayed (if any)?
  plotDiffs <- list()
  if ((.show(ef.size) || .show(paired)) && length(groups) > 1) {
    # If contrasts were specified to DurgaDiff, use them
    if (missing(contrasts)) {
      if (es$explicit.contrasts)
        plotDiffs <- es$group.differences
      else
        # Contrasts were never specified, default to all minus the first group
        plotDiffs <- buildPlotDiffs(paste(".-", groups[1]), es)
    } else if (is.character(contrasts)) {
      # Interpret string description
      plotDiffs <- buildPlotDiffs(contrasts, es)
    } else if (is.list(contrasts) && all(sapply(contrasts, function(x) methods::is(x, "DurgaGroupDiff")))) {
      # Contrasts were passed directly
      plotDiffs <- contrasts
    } else if (!is.null(contrasts)) {
      stop("Invalid plot contrasts argument, must be character string or list of DurgaGroupDiff objects")
    }
  }

  # Recycle all the *.dx arguments
  box.dx <- .extend(box.dx)
  bar.dx <- .extend(bar.dx)
  central.tendency.dx <- .extend(central.tendency.dx)
  x.axis.dx <- .extend(x.axis.dx)
  violin.dx <- .extend(violin.dx)
  points.dx <- .extend(points.dx)
  ef.size.dx <- .extend(ef.size.dx)

  # Prepare some palettes, the border palette has no transparency, the fill palette is 80% transparent
  defBorderPalette <- pickPalette(nGroups)
  defFillPalette <- DurgaTransparent(defBorderPalette, 0.8)

  # Calculate densities for violin plots
  densities <- lapply(groups, getGroupDensity, es, violin.adj, violin.trunc, violin.width)

  # Turn group column into a factor with levels potentially specified by the user so they customise groups order
  data$.group.as.factor <- factor(data[[es$group.col]], levels = groups)
  f <- stats::as.formula(paste(es$data.col.name, "~.group.as.factor"))

  # Where does effect size go?
  if (length(plotDiffs) < 1) {
    ef.size <- FALSE # There's nothing to see here
  } else if (length(plotDiffs) > 1) {
    # Can't show more than one effect size to the right
    ef.size.position <- "below"
  }

  # Calculate plot limits

  # "Natural" xlim
  nxlim <- c(0.5, nGroups + 0.5)

  # If needed extend x range to encompass effect size on right,
  # alternatively, extend bottom margin to display ES below
  if (.show(ef.size)) {
    if (ef.size.position == "right") {
      # Extend x-axis to accommodate effect size
      nxlim[2] <- nxlim[2] + 0.7
      # Extend margin as well so that right axis annotations are visible. This
      # is something of a hack so that the simplest call to DurgaPlot produces
      # useful output
      mar <- graphics::par("mar")
      MAR.RIGHT <- 4.1
      if (ef.size.adj.margin && mar[4] < MAR.RIGHT) {
        mar[4] <- MAR.RIGHT
        # Save current margin and restore on exit
        def.par <- graphics::par(mar = mar)
        on.exit(graphics::par(def.par))
      }
    } else if (ef.size.adj.margin) {
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

  #### Y limits ####

  rowsToBePlotted <- data[[es$group.col]] %in% groups

  # There is a lot of flexibility in box plots, so just use boxplot to
  # determine the extents. Run it once (without plotting) and keep the results
  if (.show(box)) {
    bp <- graphics::boxplot(f, data = data, at = seq_len(nGroups) + box.dx,
                            plot = FALSE, axes = FALSE, notch = box.notch,
                            outline = box.outline,
                            col = .colour(box.fill), border = .colour(box), pars = box.pars)
  }

  # Get vertical range of each group
  groupRange <- lapply(seq_along(groups), function(gi) {

    # Determine group range based on displayed components
    gr <- NA
    groupVals <- data[[es$data.col]][rowsToBePlotted & data[[es$group.col]] == groups[gi]]
    groupMean <- mean(groupVals)
    centralTendency <- es$group.statistics[gi, central.tendency.type]

    # If needed, extend y range to encompass box plots
    if (.show(box)) {
      # There is a lot of flexibility in box plots, so just use boxplot to determine the extents
      gr <- range(gr, bp$stats[, gi], na.rm = TRUE)
    }

    # Bar plots
    if (.show(bar)) {
      # Ensure that y = 0 is visible
      gr <- range(gr, 0, na.rm = TRUE)
      # Cover group mean
      gr <- range(gr, centralTendency, na.rm = TRUE)
    }

    # Violin plots
    if (.show(violin)) {
      gr <- range(gr, densities[[gi]]$x, na.rm = TRUE)
    }

    # Individual data points
    if (.show(points) || .show(paired)) {
      gr <- range(gr, range(groupVals), na.rm = TRUE)
    }

    # Error bars
    if (.show(error.bars)) {
      ebr <- getErrorBars(es, gi, groupMean, error.bars.type)
      gr <- range(gr, ebr, na.rm = TRUE)
    }

    # Central tendency
    if (.show(central.tendency)) {
      gr <- range(gr, centralTendency, na.rm = TRUE)
    }

    gr
  })

  if (missing(ylim)) {
    # Get vertical range of all groups combined
    ylim <- range(groupRange)
  }

  #### X limits ####

  if (missing(xlim)) {
    xlim <- nxlim
  }

  #### Prepare plot ####

  # Positions of group ticks along the x-axis
  groupAt <- seq_len(nGroups) + x.axis.dx

  if (!add) {
    plot(NULL, xlim = xlim, ylim = ylim, type = "n",
         xaxt = "n", xlab = "", ylab = left.ylab, las = left.las, ...)
    # Label the groups along the x-axis
    if (x.axis) {
      labelXAxis(at = groupAt, labels = es$group.names, tick = TRUE)
    }
  }

  ### Add the various components to the plot ###

  # Box plot
  if (.show(box)) {
    box <- .boolToDef(box, defBorderPalette)
    box.fill <- .boolToDef(box.fill, defFillPalette)
    # !!! NOTE if this is changed, it may also be necessary to change the call above that determines ylim
    graphics::boxplot(f, data = data, at = seq_len(nGroups) + box.dx,
                      add = TRUE, axes = FALSE, notch = box.notch,
                      outline = box.outline,
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
    defPalette <- DurgaTransparent(defBorderPalette, .4)
    pointCol <- .boolToDef(points, defPalette[as.numeric(data$.group.as.factor)])
    # If there are less colours than points, assume the colours are intended to be per group
    if (length(pointCol) < nrow(data)) {
      # Recycle to get colours per group
      pointCol <- .extend(pointCol)
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
    pch <- points.params[["pch"]] %||% 19
    points.params[c("pch")] <- NULL
    do.call(graphics::points, c(list(x = x, y = data[[es$data.col]], pch = pch, col = pointCol), points.params))
  }

  # Draw lines between paired points
  if (.show(paired)) {
    if (!es$paired.data)
      stop("To plot paired lines, data must be, i.e. id.col specified to DurgaDiffs")
    col <- .boolToDef(paired, DurgaTransparent("grey20", 0.7))
    # Display all contrasts, which can get very ugly if there's more than one
    for (i in seq_len(length(plotDiffs))) {
      # Get the groups in the comparison
      idx1 <- plotDiffs[[i]]$groupIndices[1]
      idx2 <- plotDiffs[[i]]$groupIndices[2]
      # Get the rows from each group
      g1 <- data[data[[es$group.col]] == groups[idx1], ]
      g2 <- data[data[[es$group.col]] == groups[idx2], ]
      # Match rows on ids - don't assume they are ordered
      g1Idx <- match(g2[[es$id.col]], g1[[es$id.col]])
      p1 <- g1[[es$data.col]][g1Idx]
      p2 <- g2[[es$data.col]]

      graphics::segments(idx1 + points.dx[idx1], p1,
                         idx2 + points.dx[idx2], p2,
                         col = col, lty = paired.lty, lwd = paired.lwd)
    }
  }

  ## add CI/SD/SE error bars
  if (.show(error.bars)) {
    error.bars <- .boolToDef(error.bars, "grey20")
    col <- rep(.colour(error.bars), length = nGroups)
    for (i in seq_along(groups)) {
      y <- es$group.statistics[i, central.tendency.type]
      bars <- getErrorBars(es, i, y, error.bars.type)
      graphics::arrows(i + central.tendency.dx[i], bars[1], i + central.tendency.dx[i], bars[2],
                       code = 3, length = error.bars.cross.width, angle = 90,
                       col = col[i], lty = 1, lwd = 3)
    }
  }

  ## Mean/median
  central.tendency <- .boolToDef(central.tendency, "grey20")
  if (.show(central.tendency)) {
    col <- central.tendency.params[["col"]] %||% .colour(central.tendency)
    col <- rep(col, length = nGroups)
    pch <- central.tendency.params[["pch"]] %||% 19
    cex <- central.tendency.params[["cex"]] %||% 1.1
    lwd <- central.tendency.params[["lwd"]] %||% 2
    central.tendency.params[c("col", "pch", "cex", "lwd")] <- NULL
    for (i in seq_along(groups)) {
      # Get estimate of central tendency
      y <- es$group.statistics[i, central.tendency.type]
      # Plot lines TODO allow symbols? More control of line?
      if (central.tendency.symbol == "point") {
        do.call(graphics::points, c(list(x = i + central.tendency.dx[i], y = y, cex = cex, pch = pch, col = col[i]),
                                    central.tendency.params))
      } else {
        do.call(graphics::segments, c(list(i - central.tendency.width + central.tendency.dx[i], y,
                                           i + central.tendency.width + central.tendency.dx[i], y,
                                           col = col[i], lwd = lwd),
                                      central.tendency.params))
      }
    }
  }

  # Effect size. Handle default colour
  ef.size.col <- .boolToDef(ef.size, "black")
  violinCol <- .boolToDef(ef.size.violin, "grey40")
  if (.show(ef.size) && ef.size.position == "right") {
    plotEffectSizesRight(es, plotDiffs[[1]], ef.size.col, ef.size.pch, .show(ef.size.violin), violinCol, violin.width, ef.size.violin.shape, ef.size.label, ef.size.ticks, ef.size.las, groupAt)
  } else if (.show(ef.size) && ef.size.position == "below") {
    plotEffectSizesBelow(es, plotDiffs, ef.size.col, ef.size.pch, .show(ef.size.violin), violinCol, violin.width, ef.size.violin.shape, xlim, ef.size.dx, ef.size.label, ef.size.ticks, ef.size.las)
  }

  # Return the coordinates of the group tick marks along the x-axis
  result <- cbind(groupAt, do.call(rbind, groupRange))
  colnames(result) <- c("x", "y.min", "y.max")
  rownames(result) <- es$group.names
  invisible(result)
}
