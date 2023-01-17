

#______________________________________________________________#
#### Private functions ####


# For debugging snapTo
# DBG_SNAP_TO <- FALSE

# Converts mm to screen user coordinates
#
# @param horizontal True if inch is measured parallel to the x-axis, otherwise parallel to the y-axis
mmToUser <- function(mm, horizontal) {
  # Work out how to scale inches to user coordinates.
  # Plot size in user coordinates (x1, x2, y1, y2)
  usr <- graphics::par("usr")
  # Plot size in inches
  pin <- graphics::par("pin")
  # Calculate scale factor
  fac <- c((usr[2] - usr[1]) / pin[1],
           (usr[4] - usr[3]) / pin[2])[(!horizontal) + 1]
  # Convert inches to mm
  mm * fac / 25.4
}

# Draw a single confidence bracket across the top of two groups
#
# @param diff Object of class \code{DurgaGroupDiff}
# @param plotStats Object returned by the call to \code{\link{DurgaPlot}}
# @param y Y value of bracket
# @param text Text to display above the bracket
# @param text Text to display above bracket
# @param plot If FALSE, doesn't draw anything, just calculates and returns
#   bounding box
# @param tipLength Length of crossbar tips (mm). May be a vector with
#   length 2; length of tip at groups 1 and 2 respectively
# @param above If \code{TRUE}, tip point downwards and text is above, otherwise
#   tips point upwards and text is below
# @param shorten
#
# @return Bounding box as c(x0, x1, y0, y1)
DrawBracket <- function(diff, plotExtents, y, text, textPad, plot = TRUE,
                        tipLength, above = TRUE, shorten,
                        br.lwd, br.col, br.lty,
                        lb.cex, lb.col, lb.font,
                        xpd = NA, ...) {

  fromGI <- diff$groupIndices[1]
  toGI <- diff$groupIndices[2]

  # Note that fromGI should always be > toGI because otherwise we negated the diff in DurgaBrackets
  if (fromGI <= toGI) stop("Internal error: bracket from LHS to RHS")

  x1 <- plotExtents[fromGI, 1]
  x2 <- plotExtents[toGI, 1]

  if (shorten != 0) {
    # mm to user coordinates in x direction
    shortenU <- mmToUser(shorten, TRUE)
    x1 <- x1 - shortenU
    x2 <- x2 + shortenU
  }

  # Calculate mm to user coordinates vertically
  dy <- mmToUser(tipLength, FALSE) * ifelse(above, 1, -1)
  # Recycle tip length so that both tips can have different lengths
  dy <- rep_len(dy, 2)
  if (plot) {
    # Cross-segment
    graphics::segments(x1, y, x2, y, xpd = xpd, lwd = br.lwd, col = br.col, lty = br.lty)
    # Tips
    graphics::segments(c(x1, x2), y, c(x1, x2), y - dy, xpd = xpd, lwd = br.lwd, col = br.col)
  }

  # Construct bounding box
  bb <- c(range(x1, x2), range(y, y - dy))

  # Draw optional text
  if (text != "") {
    tx <- mean(c(x1, x2))
    posFactor <- ifelse(above, 1, -1)
    yinc <- mmToUser(textPad, FALSE) * posFactor
    if (plot) {
      graphics::text(tx, y + yinc, text, adj = c(0.5, ifelse(above, 0, 1)), xpd = xpd,
                     cex = lb.cex, col = lb.col, font = lb.font, ...)
    }

    # Add text extent to bounding box
    h <- graphics::strheight(text, units = "user", cex = lb.cex, col = lb.col, font = lb.font, ...)
    textHeight <- (yinc + h) * posFactor
    bb[3:4] <- range(bb[3:4], y + textHeight)
  }

  bb
}

# Returns TRUE if the two rectangles intersect
rectsIntersect <- function(r1, r2) {
  r1[1] < r2[2] && r1[2] > r2[1] &&
    r1[3] < r2[4] && r1[4] > r2[3]
}

# Returns rectangle \code{r} shifted by the specified x and y deltas.
shiftRect <- function(r, dx, dy) r + rep(c(dx, dy), each = 2)


# Calculates how to fit confidence brackets depicting a set of group differences without overlaps.
#
# @param es Object of class \code{DurgaDiff}
# @param diffs List of \code{DurgaGroupDiff} objects
# @param plotExtents Extents object from object returned by the call to \code{\link{DurgaPlot}}
#
fitBrackets <- function(plotExtents, diffs, text, shorten, dataGap, verticalGap, snapTo, ...) {
  # Sort shortest brackets first
  xPos <- plotExtents[, 1]
  lengths <- sapply(diffs, function(diff) xPos[diff$groupIndices[1]] - xPos[diff$groupIndices[2]])
  ord <- order(lengths)

  maxValInGroups <- function(idxs) {
    allGrpIdx <- seq(range(idxs)[1], range(idxs)[2])
    max(plotExtents[allGrpIdx, 2:3])
  }

  # Position them in order
  bbs <- matrix(nrow = length(diffs), ncol = 4)

  text <- rep_len(text, length(diffs))

  dataGapU <- mmToUser(dataGap, FALSE)
  verticalGapU <- mmToUser(verticalGap, FALSE)
  snapToU <- mmToUser(snapTo, FALSE)

  # Make the grid pass through the bracket position above the highest group
  gridO <- max(plotExtents[, 2:3]) + dataGapU
  # if (DBG_SNAP_TO) {
  #   abline(h = gridO + -100:100 * snapToU, col = "grey80", xpd = NA)
  # }

  ys <- sapply(seq_along(ord), function(i) {
    diff <- diffs[[ord[i]]]
    # Step 1, fit above data
    testy <- maxValInGroups(diff$groupIndices) + dataGapU
    # Snap upwards to "grid"
    if (snapToU != 0) {
      # Snap to the lowest grid level that is not below the starting point (testy)
      # x * snapTo + gridO >= testy
      # Solve for x: x >= (testy - gridO) / snapTo
      # then the lowest integral value that satisfies the expression is
      # ceiling(x) , then put that back into the original expression

      testy <- ceiling((testy - gridO) / snapToU) * snapToU + gridO
    }

    bb <- DrawBracket(diff, plotExtents, testy, text = text[ord[i]], plot = FALSE, shorten = shorten, ...)
    dy <- testy - bb[3]
    bb <- shiftRect(bb, 0, dy)
    testy <- testy + dy

    # Add a gap above this bracket that we want to keep clear
    bb[4] <- bb[4] + verticalGapU

    # Now check if it overlaps any previous bracket
    if (i > 1) {
      for (ii in 1:(i - 1)) {
        if (rectsIntersect(bb, bbs[ii, ])) {
          # Shift it above this one
          dy <- bbs[ii, 4] - bb[3]
          testy <- testy + dy
          bb <- shiftRect(bb, 0, dy)
        }
      }
    }

    # Keep the bounding box
    bbs[i, ] <<- bb

    # rect(bb[1], bb[3], bb[2], bb[4], col = "#00000008", border = "#00000040", xpd = NA)
    # Return y
    testy
  })

  # Undo the sort
  ys[order(ord)]
}

# Returns an annotation function. Implemented to return a function in case I
# change DurgaPlot to accept a general purpose annotation argument
BracketsAnnot <- function(labels, shorten, dataGap, verticalGap, textPad, tipLength, snapTo,
                          br.lwd, br.col, br.lty, lb.cex, lb.col, lb.font, roundFn, ...) {

  labelFns <- list(
    CI = function(diff) sprintf("[%g, %g]", roundFn(diff$bca[4]), roundFn(diff$bca[5])),
    diff = function(diff) as.character(round(diff$t0, 1)),
    `diff CI` = function(diff) sprintf("%g [%g, %g]", roundFn(diff$t0), roundFn(diff$bca[4]), roundFn(diff$bca[5])),
    `level CI` = function(diff) sprintf("%g%% CI [%g, %g]", diff$bca[1] * 100, roundFn(diff$bca[4]), roundFn(diff$bca[5]))
  )

  function(plotStats, diffs) {
    ng <- length(diffs)
    br.lwd <- rep_len(br.lwd, ng)
    br.col <- rep_len(br.col, ng)
    br.lty <- rep_len(br.lty, ng)
    lb.cex <- rep_len(lb.cex, ng)
    lb.col <- rep_len(lb.col, ng)
    lb.font <- rep_len(lb.font, ng)
    # Handle labels complexity
    if (is.null(labels))
      labels <- ""
    if (is.character(labels) && length(labels) == 1 && nchar(labels) > 0) {
      name <- match.arg(labels, names(labelFns))
      labels <- labelFns[name][[1]]
    }
    if (is.function(labels)) {
      labels <- sapply(diffs, labels)
    } else {
      labels <- rep_len(as.character(labels), ng)
    }

    ys <- fitBrackets(plotStats$extents, diffs, shorten = shorten, text = labels,
                      tipLength = tipLength, dataGap = dataGap, verticalGap = verticalGap, textPad = textPad,
                      snapTo = snapTo,
                      br.lwd = br.lwd, br.col = br.col, br.lty = br.lty,
                      lb.cex = lb.cex, lb.col = lb.col, lb.font = lb.font, ...)

    for (i in seq_along(diffs)) {
      pwes <- diffs[[i]]
      DrawBracket(pwes, plotStats$extents, ys[i], labels[i], shorten = shorten, tipLength = tipLength, textPad = textPad,
                  br.lwd = br.lwd[i], br.col = br.col[i], br.lty = br.lty[i],
                  lb.cex = lb.cex[i], lb.col = lb.col[i], lb.font = lb.font[i], ...)
    }
  }
}

# Returns TRUE if the difference is the right-hand-group - the left-hand-group
isRightToLeft <- function(diff) {
  diff$groupIndices[1] > diff$groupIndices[2]
}

#_________________________________________________________________#
#### Public functions ####

#' Annotate a \code{DurgaPlot} with confidence brackets
#'
#' Brackets are added to a \code{DurgaPlot} that already exists. That means you
#' must ensure there is sufficient space for the brackets above the plot. To do
#' this, either specify \code{ylim} to \code{\link{DurgaPlot}}, or create a
#' large top margin (\code{par(mar = c(...))}) and the turn off the plot frame
#' (\code{DurgaPlot(..., frame.plot = FALSE)}). In either case, experiment with
#' the values until the result is visually pleasing. The annotation can be drawn
#' into the margin as it will not be cropped.
#'
#' Default values for \code{br.lwd}, \code{br.col}, \code{lb.col} and
#' \code{lb.font} depend on the confidence intervals (CI) being plotted. If the
#' CI covers 0, brackets and text are grey. If the CI does not cover 0, text is
#' dark grey and bold, and brackets are dark grey with a line width of 2.
#'
#' @param plotStats Object returned by the call to \code{\link{DurgaPlot}}
#' @param contrasts Set of contrasts (i.e. group comparisons) to be displayed as
#'   brackets. Defaults to contrasts passed to \code{\link{DurgaDiff}}. Can be
#'   specified as a character string (\code{"group 1 - group 2"}) or a list of
#'   \code{DurgaDiff} objects. The bracket label always displays the effect size
#'   for right-hand-group - left-hand-group, regardless of the order that groups
#'   are specified in \code{contrasts}, i.e. \code{contrasts = "G1 - G2"} will
#'   appear the same as \code{contrasts = "G2 - G1"}.
#'
#' @param labels Text to display above each bracket. May be NULL, otherwise one
#'   of: \code{"diff"} (displayed text is "\code{<difference in means>}"),
#'   \code{"CI"} ("\code{[<lower>, <upper>]}"), \code{"level CI"} ("\code{<level>\% CI [<lower>,
#'   <upper>]}") or \code{"diff CI"} ("\code{<difference in means> [<lower>,
#'   <upper>]}"); a vector of texts to display for each element of \code{diffs},
#'   or a function called with one argument; a \code{DurgaGroupDiff} object,
#'   which should return the label to be displayed.
#' @param br.col,br.lwd,br.lty Graphical parameters (colour, line weight and
#'   style) that control the bracket appearance - passed to
#'   \code{\link[graphics]{segments}}. May be a single value or a vector with
#'   one value per bracket. Refer to \code{Details} for default values.
#' @param lb.col,lb.cex,lb.font Graphical parameters (colour, scale and font)
#'   that control the label appearance - passed to \code{\link[graphics]{text}}.
#'   May be a single value or a vector with one value per bracket. Refer to
#'   \code{Details} for default values.
#' @param snapTo Snaps the base of the lowest brackets onto horizontal grid
#'   lines separated by \code{snapTo} mm. Used to improve aesthetics of vertical
#'   alignment.
#' @param shorten Amount (mm) to shrink brackets at each end
#' @param tipLength Length of bracket tips (mm). May be a vector with length 2;
#'   length of tip at groups 1 and 2 respectively
#' @param dataGap Vertical distance (mm) between top-most data point and bottom
#'   of bracket
#' @param verticalGap Vertical distance (mm) between overlapping brackets
#' @param textPad Gap (mm) between bracket and text
#' @param roundFn By default, numbers displayed as text are printed to 2
#'   significant figures. To change this behaviour, set \code{roundFn} to a
#'   function with one argument that converts its argument to the value to be
#'   displayed.
#' @param ... Additional arguments passed to \code{\link[graphics]{text}}
#'
#' @examples
#'
#' d <- DurgaDiff(petunia, 1, 2)
#' # Don't draw frame because brackets will appear in the upper margin
#' p <- DurgaPlot(d, ef.size = FALSE, frame.plot = FALSE)
#' # Add the brackets to the plot
#' DurgaBrackets(p, lb.cex = 0.8)
#'
#' @export
DurgaBrackets <- function(plotStats,
                          contrasts,
                          labels = "level CI",
                          br.lwd = NULL, br.col = NULL, br.lty = 1,
                          lb.col = NULL, lb.font = NULL, lb.cex = 1,
                          snapTo = 1,
                          shorten = 1.5, tipLength = 2,
                          dataGap = 2.5, verticalGap = 1.3, textPad = 1.5,
                          roundFn = function(x) signif(x, 2),
                          ...) {
  if (!is.list(plotStats) || !"extents" %in% names(plotStats))
    stop("plotStats must be an object returned by DurgaPlot")
  if (snapTo < 0)
    plot("snapTo must be zero or positive")

  diffs <- plotDiffsFromContrasts(contrasts, missing(contrasts), plotStats$es, "DurgaBrackets", defaultToAll = TRUE)

  # Display all differences as (right-hand-group) - (left-hand-group)
  diffs <- lapply(diffs, function(diff) if (isRightToLeft(diff)) { diff } else { negatePairwiseDiff(diff) })

  #-- Symbology defaults
  # For each difference, record whether it overlaps 0, is negative or positive
  sign <- sapply(diffs, function(diff) ifelse(diff$bca[5] < 0, -1,
                                              ifelse(diff$bca[4] > 0, 1, 0)))
  if (is.null(br.lwd)) br.lwd <- ifelse(sign == 0, 1, 2)
  if (is.null(br.col)) br.col <- ifelse(sign == 0, "grey60", "grey20")
  if (is.null(lb.font)) lb.font <- ifelse(sign == 0, 1, 2)
  if (is.null(lb.col)) lb.col <- ifelse(sign == 0, "grey60", "grey20")
  #-- End symbology defaults

  ann <- BracketsAnnot(labels = labels,
                       shorten = shorten, dataGap = dataGap, verticalGap = verticalGap, tipLength = tipLength,
                       textPad = textPad, snapTo = snapTo, br.lwd = br.lwd, br.col = br.col, br.lty = br.lty,
                       lb.cex = lb.cex, lb.col = lb.col, lb.font = lb.font, roundFn = roundFn, ...)
  ann(plotStats, diffs)
}
