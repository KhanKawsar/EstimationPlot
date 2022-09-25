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

# Draw a confidence bracket across the top of two groups
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

  x1 <- plotExtents[fromGI, 1]
  x2 <- plotExtents[toGI, 1]

  if (shorten != 0) {
    # mm to user coordinates in x direction
    shortenU <- mmToUser(shorten, TRUE)
    if (x1 < x2) {
      x1 <- x1 + shortenU
      x2 <- x2 - shortenU
    } else {
      x1 <- x1 - shortenU
      x2 <- x2 + shortenU
    }
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

# Returns TRUE if the two rectangles intersect
rectsIntersect <- function(r1, r2) {
  r1[1] < r2[2] && r1[2] > r2[1] &&
    r1[3] < r2[4] && r1[4] > r2[3]
}

# Returns rectangle \code{r} shifted by the specified x and y deltas.
shiftRect <- function(r, dx, dy) r + rep(c(dx, dy), each = 2)

labelFns <- list(
  CI = function(diff) sprintf("[%g, %g]", signif(diff$bca[4], 3), signif(diff$bca[5], 2)),
  diff = function(diff) as.character(round(diff$t0, 1)),
  `diff CI` = function(diff) sprintf("%g [%g, %g]", signif(diff$t0, 3), signif(diff$bca[4], 3), signif(diff$bca[5], 2)),
  `level CI` = function(diff) sprintf("%g%% CI [%g, %g]", diff$bca[1] * 100, signif(diff$bca[4], 3), signif(diff$bca[5], 2))
)

# Returns an annotation function. Implemented to return a function in case I
# change DurgaPlot to accept a general purpose annotation argument
BracketsAnnot <- function(labels, shorten, dataGap, verticalGap, textPad, tipLength, snapTo,
                          br.lwd, br.col, br.lty, lb.cex, lb.col, lb.font, ...) {

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
#' @param plotStats Object returned by the call to \code{\link{DurgaPlot}}
#' @param diffs Set of brackets to be displayed as a list of
#'   \code{DurgaGroupDiff} objects
#' @param labels Text to display above each bracket. May be NULL, otherwise one
#'   of: \code{"diff"}, \code{"CI"}, \code{"level CI"} or \code{"diff CI"}; a
#'   vector of texts to display for each element of \code{diffs}, or a function
#'   called with one argument; a \code{DurgaGroupDiff} object
#' @param br.col,br.lwd,br.lty Graphical parameters (colour, line weight and
#'   style) that control the bracket appearance - passed to
#'   \code{\link[graphics]{segments}}. May be a single value or a vector with
#'   one value per bracket.
#' @param lb.col,lb.cex,lb.font Graphical parameters (colour, scale and font)
#'   that control the label appearance - passed to \code{\link[graphics]{text}}.
#'   May be a single value or a vector with one value per bracket.
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
#' @param ... Additional arguments passed to \code{\link[graphics]{text}}
#'
#' @examples
#'
#' d <- DurgaDiff(petunia, 1, 2)
#' # Don't draw frame because brackets will appear in the upper margin
#' p <- DurgaPlot(d, ef.size = FALSE, frame.plot = FALSE)
#' DurgaBrackets(p, lb.cex = 0.8, br.lwd = 2, snapTo = 1)
#'
#' @export
DurgaBrackets <- function(plotStats,
                          diffs = plotStats$es$group.differences,
                          labels = "CI",
                          br.lwd = 1, br.col = 1, br.lty = 1,
                          lb.cex = 1, lb.col = 1, lb.font = 1,
                          snapTo = 1,
                          shorten = 1.5, tipLength = 2,
                          dataGap = 2.5, verticalGap = 1.3, textPad = 1.5,
                          ...) {
  if (!is.list(plotStats) || !"extents" %in% names(plotStats))
    stop("plotStats must be an object returned by DurgaPlot")
  if (snapTo < 0)
    plot("snapTo must be zero or positive")

  ann <- BracketsAnnot(labels = labels,
                       shorten = shorten, dataGap = dataGap, verticalGap = verticalGap, tipLength = tipLength,
                       textPad = textPad, snapTo = snapTo, br.lwd = br.lwd, br.col = br.col, br.lty = br.lty,
                       lb.cex = lb.cex, lb.col = lb.col, lb.font = lb.font, ...)
  ann(plotStats, diffs)
}
