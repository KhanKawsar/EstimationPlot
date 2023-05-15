# The difference function

#_____________________________________________________________#
#### Private functions ####

# Confidence interval of the mean of a group
CI <- function(x, alpha = 0.95) {
  # Alternatively, perhaps we should bootstrap this so not assuming any particular distribution
  m <- mean(x)
  sd <- stats::sd(x)
  n <- length(x)
  se <- sd/sqrt(n)
  df <- n - 1
  alpha <- 1 - alpha
  t <- stats::qt(p = alpha/2, df = df, lower.tail = F)
  me <- t * se
  c(m - me, m + me)
}

# Two group statistic functions

# Mean difference (group 2 - group 1)
stMeanDiff <- function(x1, x2) { mean(x2) - mean(x1) }

# Cohens d (group 2 - group 1)
stCohensD <- function(x1, x2){
  m1 <- mean(x1)
  SD1 <- stats::sd(x1)
  N1 <- length(x1)
  m2 <- mean(x2)
  SD2 <- stats::sd(x2)
  N2 <- length(x2)
  # Calculate pooled standard deviation
  x <- sqrt(((N1 - 1) * (SD1 * SD1) + (N2 - 1) * (SD2 * SD2)) / (N1 + N2 - 2))
  (m2 - m1) / x
}

# Hedges' g (group 2 - group 1)
stHedgesG <- function(x1, x2){
  N1 <- length(x1)
  N2 <- length(x2)
  stCohensD(x1, x2) * (1 - 3 / (4 * (N1 + N2) - 9))
}

# Cohen's dz, one sample or correlated/paired samples comparison
stCohensDz <- function(x) mean(x) / stats::sd(x)

# TODO IS THIS CORRECT??? Hedges' g for paired data
stHedgesGz <- function(x) stCohensDz(x) * (1 - 3 / (4 * length(x) - 9))

# Calculate the difference statistic for a pair of groups
# @param data Values for the two groups
# @param pair Vector with the two group names
# @param isPaired Boolean, TRUE if data are paired
# @param pairIndices Indices within groups of the two groups (not used here,
#   just added to the return structure)
# @param data.col Name/index of the data column
# @param group.col Name/index of the group column
# @param id.col Name/index of the id column
# @param effect.type Name of the statistic to be calculated
# @param R Number of bootstrap replicates
# @param ci.conf Level for confidence interval
# @param ci.type Value passed to the `boot::boot.ci` `type` parameter
# @param boot.params List of additional argument to pass to boot::boot
# @param boot.ci.params List of additional argument to pass to boot::boot.ci
calcPairDiff <- function(data, pair, isPaired, pairNames, pairIndices, data.col, group.col, id.col,
                         effect.type, R, ci.conf, ci.type, boot.params, boot.ci.params) {

  # Functions to simplify writing bootstrap statistic functions
  .wrap2GroupStatistic <- function(statisticFn) {
    function(data, indices) {
      measurement <- data[indices, data.col]
      group <- data[indices, group.col]
      x2 <- measurement[group == pair[2]]
      x1 <- measurement[group == pair[1]]
      statisticFn(x2, x1)
    }
  }
  .wrapPairedStatistic <- function(statisticFn) {
    function(data, indices) {
      statisticFn(data[indices])
    }
  }

  # Decide how to calculate the statistic
  statistic <- NULL
  if (isPaired) {
    # Paired data
    g1 <- data[data[[group.col]] == pair[1], ]
    g2 <- data[data[[group.col]] == pair[2], ]
    # Pair on ID (don't assume they are sorted)
    g1Idx <- match(g2[[id.col]], g1[[id.col]])
    if (any(is.na(g1Idx))) {
      # Report missing data
      idColName <- id.col
      if (is.numeric(idColName))
        idColName <- names(data)[id.col]
      nBad <- sum(is.na(g1Idx))
      stop(sprintf("%d %s%s are not matched across groups in paired data", nBad, idColName, if (nBad == 1) "" else "s"))
    }
    # Statistic functions operate on differences between paired values
    bootstrapData <- g1[[data.col]][g1Idx] - g2[[data.col]]
    if (is.function(effect.type)) {
      statistic <- .wrapPairedStatistic(effect.type)
    } else if (effect.type == "mean") {
      statistic <- .wrapPairedStatistic(mean)
    } else if (effect.type == "cohens") {
      statistic <- .wrapPairedStatistic(stCohensDz)
    } else if (effect.type == "hedges") {
      statistic <- .wrapPairedStatistic(stHedgesGz)
    }
  } else {
    # Unpaired data
    bootstrapData <- data
    if (is.function(effect.type)) {
      statistic <- .wrap2GroupStatistic(effect.type)
    } else if (effect.type == "mean") {
      statistic <- .wrap2GroupStatistic(stMeanDiff)
    } else if (effect.type == "cohens") {
      statistic <- .wrap2GroupStatistic(stCohensD)
    } else if (effect.type == "hedges") {
      statistic <- .wrap2GroupStatistic(stHedgesG)
    }
  }

  # Bootstrap the statistic
  es <- do.call(boot::boot, c(list(data = bootstrapData, statistic = statistic, R = R, stype = "i"), boot.params))

  if (is.na(es$t0))
    stop(sprintf("Estimate is NA; do you need to specify na.rm = TRUE?"))

  # Calculate confidence interval
  ci <- do.call(boot::boot.ci, c(list(es, type = ci.type, conf = ci.conf), boot.ci.params))

  # Save some parts of the CI data into the returned value (don't need values that are already there or misleading)
  es[["ci.type"]] <- ci.type
  for (cin in names(ci)) {
    if (!cin %in% c("R", "t0", "call")) {
      es[[cin]] <- ci[[cin]]
    }
  }

  # Save group names
  es$groups <- pair                # raw group values
  es$groupLabels <- pairNames      # Display values
  es$groupIndices <- pairIndices   # Indices within es$groups
  # Remove data because it is in the outer structure
  es$data <- NULL
  # Save CI type
  es$ci.type <- ci.type

  # Give it a class
  class(es) <- c("DurgaGroupDiff", class(es))

  es
}


#__________________________________________________________________________#
#### Public functions ####

#' @rdname DurgaDiff
#' @export
DurgaDiff <- function(x, ...) {
  UseMethod("DurgaDiff", x)
}


#' Formula interface for estimating group mean differences
#'
#' Estimates differences between groups in preparation for plotting by
#' \code{\link{DurgaPlot}}. Applies the formula, \code{x}, and a data set,
#' \code{data}, to construct a data frame that is then passed, with all
#' remaining arguments, to the function \code{\link{DurgaDiff.default}}.

#' @inherit DurgaDiff.default
#'
#' @inheritDotParams DurgaDiff.default -data.col -group.col -id.col
#' @param x a formula, such as \code{y ~ grp},
#'   where \code{y} is a numeric vector of data values to be split into groups
#'   according to the grouping variable \code{grp} (usually a categorical
#'   value).
#' @param data a data.frame (or list) from which the variables in formula should be taken.
#'
#' @examples
#'
#' d <- DurgaDiff(sugar ~ treatment, insulin, id.col = "id")
#' print(d)
#'
#' @seealso \code{\link{DurgaDiff.default}}, \code{\link[boot]{boot}},
#'   \code{\link[boot]{boot.ci}}, \code{\link{DurgaPlot}}
#'
#' @export
DurgaDiff.formula <- function(x, data = NULL, id.col, ...) {
  md <- stats::model.frame(x, data)
  if (ncol(md) != 2)
    stop(sprintf("Formula must contain exactly two terms"))
  # If id.col specified...
  if (!missing(id.col) && !is.na(id.col) && !is.null(id.col)) {
    # Add id.col to the model data set
    md <- cbind(md, data[[id.col]])
    d <- DurgaDiff(md, 1, 2, 3, ...)
  } else {
    d <- DurgaDiff(md, 1, 2, ...)
  }
  # Add formula to result
  d$formula <- x
  d
}


#' Estimate group mean differences
#'
#' Estimates differences between groups in preparation for plotting by
#' \code{\link{DurgaPlot}}.
#'
#' If \code{x} is a \code{data.frame} (or similar), it must be in \emph{long
#' format}: one column (\code{data.col}) contains the measurement or value to be
#' compared, and another column (\code{group.col}) the group identity. For
#' repeated measures/paired data, a subject identity column (\code{id.col}) is
#' also required. Alternatively, \code{x} may be a formula; see
#' \code{\link{DurgaDiff.formula}}.
#'
#' The pairs of groups to be compared are defined by the parameter
#' \code{contrasts}. An asterisk (\code{"*"}, the default) creates contrasts for
#' all possible pairs of groups. A single string has a format such as
#' \code{"group1 - group2, group3 - group4"}. A single string such as \code{".-
#' control"} compares all groups against the \code{"control"} group, i.e. the
#' \code{"."} expands to all groups except the named group. A vector of strings
#' looks like \code{c("group1 - group2", "group3 - group4")}. If a matrix is
#' specified, it must have a column for each contrast, with the first group in
#' row 1 and the second in row 2.
#'
#' The formulae for Cohen's d and Hedges' g are from Lakens (2013), equations 1
#' and 4 respectively. The Cohen's d we use is labelled
#' \emph{\out{d<sub>s</sub>}} by Lakens (2013). Hedges' g is a corrected version
#' of Cohen's d, and is more suitable for small sample sizes. For paired (i.e.
#' repeated measures) Cohen's d, we apply equation 6 (Lakens 2013). For paired
#' Hedges' g, we apply Hedges' correction to the paired Cohen's d.
#'
#' Alternative effect types can be estimated by passing a function for
#' \code{effect.type}. For unpaired data, the function must accept two
#' parameters: the values from the two groups to be compared (group 2 and group
#' 1), and return a single numeric value, the effect size. For paired data, the
#' function must accept a single argument; a vector of group 1 values - group 2
#' values, and return a single numeric value.
#'
#' Confidence intervals for the estimate are determined using bootstrap
#' resampling, using the adjusted bootstrap percentile (BCa) method (see
#' \code{\link[boot]{boot}} and \code{\link[boot]{boot.ci}}). Additional
#' arguments can be passed to the \code{\link[boot]{boot}}
#' (\code{\link[boot]{boot.ci}}) by passing a named list of values as the
#' argument \code{boot.params} (\code{boot.ci.params}).
#'
#' @param x A data frame (or similar) containing values to be compared, or a
#'   formula (see \code{\link{DurgaDiff.formula}}).
#' @param data.col Name (character) or index (numeric) of the column within
#'   \code{data} containing the measurement data.
#' @param group.col Name or index of the column within \code{data} containing
#'   the values to group by.
#' @param id.col Specify for paired data/repeated measures only. Name or index
#'   of ID column for repeated measures/paired data. Observations for the same
#'   individual must have the same ID. For non-paired data, do not specify an
#'   \code{id.col}, (or use \code{id.col = NA}).
#' @param groups Vector of group names. Defaults to all groups in \code{data} in
#'   \emph{natural} order. If \code{groups} is a named vector, the names are
#'   used as group labels for plotting or printing.
#' @param contrasts Specify the pairs of groups to be compared. By default, all
#'   pairwise differences are generated. May be a single string, a vector of
#'   strings, or a matrix. Specify \code{NULL} to avoid calculating any
#'   contrasts. See Details for more information.
#' @param effect.type Type of group difference to be estimated. Possible types
#'   are: \code{"mean"}, difference in unstandardised group means;
#'   \code{"cohens"}, Cohen's d; \code{"hedges"}, Hedges' g; or a function. See
#'   Details for further information.
#' @param R The number of bootstrap replicates. The default value of 1000 may
#'   need to be increased for large sample sizes; if \code{R <= nrow(x)}, an
#'   error such as "Error in bca.ci... estimated adjustment 'a' is NA" will be
#'   thrown.
#' @param boot.params Optional list of additional names parameters to pass to
#'   the \code{\link[boot]{boot}} function.
#' @param ci.conf Numeric confidence level of the required confidence interval,
#'   e.g. \code{ci.conf = 0.95} specifies that 95\% confidence intervals should
#'   be calculated. Applies to both CI of effect sizes and CI of group means.
#' @param boot.ci.params Optional list of additional names parameters to pass to
#'   the \code{\link[boot]{boot.ci}} function.
#' @param na.rm a logical evaluating to TRUE or FALSE indicating whether NA
#'   values should be stripped before the computation proceeds. If \code{TRUE}
#'   for "paired" data (i.e. \code{id.col} is specified), all rows
#'   (observations) for IDs with missing data are stripped.
#'
#' @returns A \code{DurgaDiff} object, which is a list containing:
#'
#'   \item{\code{group.statistics}}{Matrix with a row for each group, columns
#'   are: \code{mean}, \code{median}, \code{sd} (standard deviation), \code{se}
#'   (standard error of the mean), \code{CI.lower} and \code{CI.upper} (lower
#'   and upper confidence intervals of the mean, confidence level as set by the
#'   \code{ci.conf} parameter) and \code{n} (group sample size.)}
#'
#'   \item{\code{group.differences}}{List of \code{DurgaGroupDiff} objects,
#'   which are \code{boot} objects with added confidence interval information.
#'   See \code{\link[boot]{boot}} and \code{\link[boot]{boot.ci}}}
#'   \item{\code{groups}}{Vector of group names}
#'   \item{\code{group.names}}{Labels used to identify groups}
#'   \item{\code{effect.type}}{Value of \code{effect.type} parameter}
#'   \item{\code{effect.name}}{Pretty version of \code{effect.type}}
#'   \item{\code{data.col}}{Value of \code{data.col} parameter; may be an index
#'   or a name} \item{\code{data.col.name}}{Name of the \code{data.col} column}
#'   \item{\code{group.col}}{Value of \code{group.col} parameter; may be an
#'   index or a name} \item{\code{group.col.name}}{Name of the \code{group.col}
#'   column} \item{\code{id.col}}{Value of \code{id.col} parameter. May be
#'   \code{NULL}} \item{\code{paired.data}}{\code{TRUE} if paired differences
#'   were estimated} \item{\code{data}}{The input data frame}
#'   \item{\code{call}}{How this function was called}
#'
#'   A \code{DurgaGroupDiff} object is a \code{boot} object (as returned by
#'   \code{\link[boot]{boot}}) with added \code{bootci} components (as returned
#'   by \code{\link[boot]{boot.ci}}) and components identifying the groups used
#'   to estimate the difference. Particularly relevant members are:
#'
#'   \item{\code{t0}}{The observed value of the statistic}
#'   \item{\code{bca[4]}}{The lower endpoint of the confidence interval}
#'   \item{\code{bca[5]}}{The upper endpoint of the confidence interval}
#'   \item{\code{groups}}{The difference is estimated on \code{groups[1]} -
#'   \code{groups[2]}}
#'
#' @param ... Ignored
#'
#' @seealso \code{\link{DurgaDiff.formula}}, \code{\link[boot]{boot}},
#'   \code{\link[boot]{boot.ci}}, \code{\link{DurgaPlot}}
#'
#' @rdname DurgaDiff
#'
#' @examples
#'
#' d <- DurgaDiff(insulin, "sugar", "treatment", "id")
#' print(d)
#'
#' # Change group order and displayed group labels, reverse the
#' # direction of one of the contrasts from the default
#' d <- DurgaDiff(petunia, 1, 2,
#'                groups = c("self-fertilised" = "self_fertilised",
#'                           "intercrossed" = "inter_cross",
#'                           "Westerham-crossed" = "westerham_cross"),
#'                contrasts = c("Westerham-crossed - self-fertilised",
#'                              "Westerham-crossed - intercrossed",
#'                              "intercrossed - self-fertilised"))
#'
#' @references
#'
#' Lakens, D. (2013). Calculating and reporting effect sizes to facilitate
#' cumulative science: a practical primer for t-tests and ANOVAs. Frontiers in
#' Psychology, 4. doi:10.3389/fpsyg.2013.00863
#'
#' @export
DurgaDiff.default <- function(x,
                      data.col, group.col,
                      id.col,
                      groups = sort(unique(x[[group.col]])),
                      contrasts = "*",
                      effect.type = c("mean", "cohens", "hedges"),
                      R = 1000,
                      boot.params = list(),
                      ci.conf = 0.95,
                      boot.ci.params = list(),
                      na.rm = FALSE,
                      ...
) {

  # *******
  # For now, don't allow ci.type to be changed. Quite a lot of code assumes its
  # value is "bca". If we add it in as a parameter, it can be documented as:

  # @param ci.type A single character  string representing the type of bootstrap
  #   interval required. See the \code{type} parameter to the [boot]{boot.ci}
  #   function for details.
  # *******
  ci.type = "bca"

  # If data is a data.table, it breaks things, so convert to a data.frame
  x <- as.data.frame(x)

  pairedData <- !missing(id.col) && !is.null(id.col) && !is.na(id.col)

  effectNames <- c(mean = "Mean difference", cohens = "Cohen's d", hedges = "Hedges' g", median = "Median difference")

  if (!is.function(effect.type))
    effect.type <- match.arg(effect.type)

  # Check column specifications
  .isACol <- function(spec) (is.numeric(spec) && spec >= 1 && spec <= ncol(x)) || (!is.numeric(spec) && spec %in% names(x))
  if (!.isACol(data.col))
    stop(sprintf("data.col '%s' is not a valid column name or index (names are %s)",
                 data.col, paste(names(x), collapse = ", ")))
  if (!is.numeric(x[[data.col]]))
    stop(sprintf("data.col '%s' must be a numeric column, is %s", data.col, class(x[[data.col]])))
  if (!.isACol(group.col))
    stop(sprintf("group.col '%s' is not a valid column name or index (names are %s)",
                 group.col, paste(names(x), collapse = ", ")))
  if (pairedData && !.isACol(id.col))
    stop(sprintf("id.col '%s' is not a valid column name or index (names are %s)",
                 id.col, paste(names(x), collapse = ", ")))

  # Optionally handle NA values
  if (na.rm) {
    toKeep <- !is.na(x[[data.col]]) & !is.na(x[[group.col]])
    if (pairedData) {
      toKeep <- toKeep & !is.na(x[[id.col]])
      # Also delete pairs with missing data
      badPairIds <- unique(x[[id.col]][!toKeep])
      toKeep <- toKeep & !(x[[id.col]] %in% badPairIds)
    }
    x <- x[toKeep, ]
  }

  # Sanity checks
  if (!all(groups %in% unique(x[[group.col]]))) {
    badGroups <- which(!groups %in% unique(x[[group.col]]))
    fmt <- ifelse(length(badGroups) == 1, "Group '%s' is missing from data", "Groups %s are missing from data")
    stop(sprintf(fmt, paste(groups[badGroups], collapse = ", ")))
  }
  if (nrow(x) == 0) stop("No data to analyse!")

  # Create return structure with administrative info
  .colName <- function(col) ifelse(is.numeric(col), names(x)[col], col)
  groupLabels <- names(groups)
  if (is.null(groupLabels)) {
    groupLabels <- as.character(groups)
  } else {
    groupLabels <- ifelse(groupLabels == "", as.character(groups), groupLabels)
  }
  es <- list(data = x,
             call = match.call(),
             data.col = data.col,
             data.col.name = .colName(data.col),
             group.col = group.col,
             group.col.name = .colName(group.col),
             id.col = if (pairedData) id.col else NULL,
             id.col.name = if (pairedData) .colName(id.col) else NULL,
             paired.data = pairedData,
             groups = groups,
             group.names = groupLabels,
             effect.type = effect.type,
             effect.name = ifelse(is.function(effect.type), "Custom effect type", effectNames[effect.type]),
             explicit.contrasts = !missing(contrasts))
  # Return value has type DurgaDiff
  class(es) <- c("DurgaDiff", class(es))

  # Fill in statistical summary about each of the groups
  gil <- lapply(groups, function(g) {
    grpVals <- x[[data.col]][x[[group.col]] == g]
    ci <- CI(grpVals, ci.conf)
    c(mean = mean(grpVals),
      median = stats::median(grpVals),
      sd = stats::sd(grpVals),
      se = stats::sd(grpVals) / sqrt(length(grpVals)),
      # CI of mean
      CI.lower = ci[1],
      CI.upper = ci[2],
      n = length(grpVals)
    )
  })
  df <- do.call(rbind, gil)
  rownames(df) <- groupLabels
  es$group.statistics <- df

  # Interpret the contrasts
  contrasts <- expandContrasts(contrasts, groups, groupLabels)
  if (is.null(contrasts)) {
    es$group.differences <- NULL
  } else {
    # For each pair of groups...
    es$group.differences <- apply(contrasts, 2, function(pair) {
      pairData <- x[as.character(x[[group.col]]) %in% pair, ]
      groupIndices <- c(which(groups == pair[1]), which(groups == pair[2]))
      groupLabels <- c(groupLabels[groupIndices[1]],
                       groupLabels[groupIndices[2]])
      calcPairDiff(pairData, pair, pairedData, groupLabels, groupIndices, data.col, group.col, id.col,
                   effect.type, R, ci.conf, ci.type, boot.params, boot.ci.params)
    })
  }

  es
}

#___________________________________________________________#
# Print methods

#' @export
print.DurgaDiff <- function(x, ...) {
  cat("Bootstrapped effect size\n")

  fs <- deparse(x$formula)
  if (is.null(x$formula)) {
    # Construct a formula description
    fs <- sprintf("%s ~ %s", x$data.col.name, x$group.col.name)
  }
  cat(sprintf("  %s\n", fs))
  cat("Groups:\n")
  print(x$group.statistics)
  cat(sprintf("%s %s (R = %d, bootstrap CI method = %s):\n",
              ifelse(x$paired.data, "Paired", "Unpaired"), x$effect.name,
              # Note R and ci.type should be the same for all comparisons
              x$group.differences[[1]]$R, x$group.differences[[1]]$ci.type))
  for (i in seq_len(length(x$group.differences))) {
    print(x$group.differences[[i]])
  }
}

#' @export
print.DurgaGroupDiff <- function(x, ...) {
  if (!is.null(x$label.print))
    label <- x$label.print
  else
    label <- sprintf("%s - %s", x$groupLabels[1], x$groupLabels[2])
  cat(sprintf("  %s: %g, %g%% CI [%g, %g]\n",
              label,
              x$t0, x$bca[1] * 100, x$bca[4], x$bca[5]))
}

