# The difference function


### confidence interval of a group
CI <- function(x){
  alpha <- 0.95
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

calcPairDiff <- function(data, pair, paired, pairNames, pairIndices, data.col, group.col, id.col, effect.type, R, ci.type, ...) {

  # Function to simplify writing bootstrap statistic functions
  .wrap2GroupStatistic <- function(statisticFn) {
    function(data, indices) {
      measurement <- data[indices, data.col]
      group <- data[indices, group.col]
      x1 <- measurement[group == pair[2]]
      x2 <- measurement[group == pair[1]]
      statisticFn(x1, x2)
    }
  }
  .wrapPairedStatistic <- function(statisticFn) {
    function(data, indices) {
      statisticFn(data[indices])
    }
  }

  # Decide how to calculate the statistic
  statistic <- NULL
  if (paired) {
    # Paired data
    g1 <- data[data[[group.col]] == pair[1], ]
    g2 <- data[data[[group.col]] == pair[2], ]
    # Pair on ID (don't assume they are sorted)
    g1Idx <- match(g2[[id.col]], g1[[id.col]])
    bootstrapData <- g1[[data.col]][g1Idx] - g2[[data.col]]
    if (effect.type == "unstandardised") {
      statistic <- .wrapPairedStatistic(mean)
    } else if (effect.type == "cohens") {
      statistic <- .wrapPairedStatistic(stCohensDz)
    } else if (effect.type == "hedges") {
      statistic <- .wrapPairedStatistic(stHedgesGz)
    }
  } else {
    bootstrapData <- data
    if (effect.type == "unstandardised") {
      statistic <- .wrap2GroupStatistic(stMeanDiff)
    } else if (effect.type == "cohens") {
      statistic <- .wrap2GroupStatistic(stCohensD)
    } else if (effect.type == "hedges") {
      statistic <- .wrap2GroupStatistic(stHedgesG)
    }
  }
  if (is.null(statistic)) {
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

  # Save group names
  es$groups <- pair
  es$groupLabels <- pairNames
  es$groupIndices <- pairIndices
  # Remove data because it is in the outer structure
  es$data <- NULL
  # Save CI type
  es$ci.type <- ci.type

  # Give it a class
  class(es) <- c("SAKPWDiff", class(es))

  es
}

# Takes contrasts as a string, vector of strings, or matrix and returns a matrix
expandContrasts <- function(contrasts, groups) {

  # Special value "*" means all possible pairs
  if (is.character(contrasts) && length(contrasts) == 1 && trimws(contrasts) == "*") {
    contrasts <- utils::combn(rev(groups), 2)
  }

  if (!is.matrix(contrasts)) {
    if (length(contrasts) == 1) {
      # Split on commas
      contrasts <- strsplit(contrasts, ",")[[1]]
    }

    # Assume syntax "group - group"
    contrasts <- sapply(contrasts, function(contrast) {
      # For this contrast, which should look like "group1 - group2",
      # Find the location of group names within the string
      found <- sapply(unname(groups), function(group) regexpr(group, contrast, fixed = TRUE)[[1]][1])
      if (sum(found > 0) != 2)
        stop(sprintf("Invalid contrast '%s'; must be 'group1 - group2, groups are %s",
                     contrast, paste0(groups, collapse = ", ")))
      found <- found[found != -1]
      names(found)[order(found)]
    })
  }

  contrasts
}

# Returns the negation of the specified group difference (type SAKPWDiff,
# usually a member of es$group.differences)
negatePairwiseDiff <- function(pwd) {
  pwd$groups[[1]] <- rev(pwd$groups[[1]])
  pwd$groupLabels[[1]] <- rev(pwd$groupLabels[[1]])
  pwd$t0 <- -pwd$t0
  pwd$t[[1]] <- -pwd$t[[1]]
  pwd$bca[4] <- -pwd$bca[4]
  pwd$bca[5] <- -pwd$bca[5]
  pwd
}


#############################################################################

#' Calculate group mean differences
#'
#' Calculates differences between groups ready for printing or plotting by
#' \code{\link{SAKPlot}}.
#'
#' Data format - long format, one column with measurement (\code{data.col}),
#' another column with group identity (\code{group.col}). For repeated measures,
#' a subject identity column is also required (\code{id.col}).
#'
#' The formulae for Cohen's d and Hedge's g are from Lakens (2013), equations 1
#' and 4 respectively. The Cohen's d we use is labelled
#' \emph{\out{d<sub>s</sub>}} by Lakens (2013). Hedges' g is a corrected version
#' of Cohen's d, and is more suitable for small sample sizes. For paired (i.e.
#' repeated measures) Cohen's d, we apply equation 6 (Lakens 2013). For paired
#' Hedges' g, we simply apply Hedges' correction to the paired Cohen's d.
#'
#' @param data A data frame containing values to be compared.
#' @param data.col Name or index of the column within \code{data} containing the
#'   measurement data.
#' @param group.col Name or index of the column within \code{data} containing
#'   the values to group by.
#' @param id.col Name or index of ID column for repeated measures/paired data.
#'   For non-paired data, do not specify an \code{id.col}.
#' @param effect.type Type of difference
#' @param groups Vector of group names. Defaults to all groups in \code{data} in
#'   \emph{natural} order. If \code{groups} is a named vector, the names are
#'   used to identify groups for printing or plotting.
#' @param contrasts Specify the pairs of groups to be compared. By default, 2nd
#'   and subsequent groups are compared to the first group. May be a single
#'   string, a vector of strings, or a matrix. A single string has a format such
#'   as \code{"group1 - group2, group3 - group4"}. A single asterisk, \code{"*"}
#'   creates contrasts for all possible pairs of groups. A vector of strings
#'   looks like \code{c("group1 - group2", "group3 - group4")}. If a matrix is
#'   specified, it must have a column for each contrast, with the first group in
#'   row 1 and the second in row 2.
#' @param effect.type Type of group difference to be calculated. Possible types
#'   are: \code{unstandardised}, difference in group means; \code{cohens},
#'   Cohen's d; \code{hedges}, Hedge's g.
#' @param R The number of bootstrap replicates.
#' @param ci.type A single character  string representing the type of bootstrap
#'   interval required. See the \code{type} parameter to the [boot]{boot.ci}
#'   function for details.
#' @param na.rm a logical evaluating to TRUE or FALSE indicating whether NA
#'   values should be stripped before the computation proceeds. If \code{TRUE}
#'   for "paired" data (i.e. \code{id.col} is specified), all rows
#'   (observations) for IDs with missing data are stripped.
#' @param ... Any additional parameters are passed to \code{\link[boot]{boot}}.
#'
#' @return List containing: \item{\code{groups}}{Vector of group names}
#'   \item{\code{group.statistics}}{Matrix with a row for each group, columns
#'   are group mean, median, standard deviation, standard error of the mean,
#'   lower and upper 95\% confidence intervals of the mean}
#'   \item{\code{group.differences}}{List of \code{SAKPWDiff} objects, which are
#'   \code{boot} objects with added confidence interval information. See
#'   \code{\link[boot]{boot}} and \code{\link[boot]{boot.ci}}}
#'   \item{\code{effect.type}}{Value of \code{effect.type} argument}
#'   \item{\code{effect.name}}{Pretty version of \code{effect.type}}
#'   \item{\code{data.col}}{Value of \code{data.col} argument}
#'   \item{\code{data.col.name}}{Name of the \code{data.col} column}
#'   \item{\code{group.col}}{Value of \code{group.col} argument}
#'   \item{\code{group.col.name}}{Name of the \code{group.col} column}
#'   \item{\code{data}}{The input data frame} \item{\code{call}}{how this
#'   function was called}
#'
#' @seealso \code{\link[boot]{boot}}, \code{\link[boot]{boot.ci}},
#'   \code{\link{SAKPlot}}, \code{\link{print.SAKDiff}}
#'
#' @references
#'
#' Lakens, D. (2013). Calculating and reporting effect sizes to facilitate
#' cumulative science: a practical primer for t-tests and ANOVAs. Frontiers in
#' Psychology, 4. doi:10.3389/fpsyg.2013.00863
#'
#' @export
SAKDifference <- function(data,
                       data.col, group.col,
                       #TODO what is this for??? block.col = NULL,
                       id.col,
                       groups = sort(unique(data[[group.col]])),
                       contrasts,
                       effect.type = c("unstandardised", "cohens", "hedges"),
                       R = 1000,
                       ci.type = "bca",
                       na.rm = FALSE,
                       ...
                       # ci.conf = 0.95,
) {

  pairedData <- !missing(id.col)

  effectNames <- c(unstandardised = "Mean difference", cohens = "Cohen's d", hedges = "Hedges' g")

  if (!is.function(effect.type))
    effect.type <- match.arg(effect.type)

  # Check column specifications
  .isACol <- function(spec) (is.numeric(spec) && spec >= 1 && spec <= ncol(data)) || (!is.numeric(spec) && spec %in% names(data))
  if (!.isACol(data.col))
    stop(sprintf("data.col %s is not a valid column name or index (names are %s)",
                 data.col, paste(names(data), collapse = ", ")))
  if (!.isACol(group.col))
    stop(sprintf("group.col %s is not a valid column name or index (names are %s)",
                 data.col, paste(names(data), collapse = ", ")))

  # Optionally handle NA values
  if (na.rm) {
    toKeep <- !is.na(data[[data.col]]) & !is.na(data[[group.col]])
    if (pairedData) {
      toKeep <- toKeep & !is.na(data[[id.col]])
      # Also delete pairs with missing data
      badPairIds <- unique(data[[id.col]][!toKeep])
      toKeep <- toKeep & !(data[[id.col]] %in% badPairIds)
    }
    data <- data[toKeep, ]
  }

  # Create return structure with administrative info
  .colName <- function(col) ifelse(is.numeric(col), names(data)[col], col)
  groupLabels <- names(groups)
  if (is.null(groupLabels))
    groupLabels <- groups
  groupLabels <- ifelse(groupLabels == "", groups, groupLabels)
  es <- list(data = data,
             call = match.call(),
             data.col = data.col,
             data.col.name = .colName(data.col),
             group.col = group.col,
             group.col.name = .colName(group.col),
             id.col = if (pairedData) id.col else NULL,
             paired.data = pairedData,
             groups = groups,
             group.names = groupLabels,
             effect.type = effect.type,
             effect.name = effectNames[effect.type])
  # Return value has type SAKDiff
  class(es) <- c("SAKDiff", class(es))

  # Fill in statistical summary about each of the groups
  gil <- lapply(groups, function(g) {
    grpVals <- data[[data.col]][data[[group.col]] == g]
    ci <- CI(grpVals)
    c(mean = mean(grpVals),
      median = stats::median(grpVals),
      sd = stats::sd(grpVals),
      se = stats::sd(grpVals) / sqrt(length(grpVals)),
      # CI of mean
      CI.lower = ci[1],
      CI.upper = ci[2],
      N = length(grpVals)
    )
  })
  df <- do.call(rbind, gil)
  rownames(df) <- groupLabels
  es$group.statistics <- df

  if ((missing("contrasts") || is.null(contrasts)) && length(groups) > 1) {
    # convert groups to character to handle groups that are factors
    gc <- as.character(groups)
    # Compare 2nd and subsequent groups to first
    contrasts <- matrix(c(gc[2:length(gc)], rep(gc[1], length(gc) - 1)),
                        byrow = TRUE, nrow = 2)
  } else {
    contrasts <- expandContrasts(contrasts, groups)
  }

  # For each pair of groups...
  es$group.differences <- apply(contrasts, 2, function(pair) {
    pairData <- data[as.character(data[[group.col]]) %in% pair, ]
    groupIndices <- c(which(groups == pair[1]), which(groups == pair[2]))
    groupLabels <- c(groupLabels[groupIndices[1]],
                    groupLabels[groupIndices[2]])
    calcPairDiff(pairData, pair, pairedData, groupLabels, groupIndices, data.col, group.col, id.col, effect.type, R, ci.type)
  })

  es
}

#######################################################################################
# Print methods

#' Print a summary of a SAK Difference object
#'
#' This is a method for the function \code{print()} for objects of class
#' \code{SAKDiff} created by a call to {SAKDifference}.
#'
#' @param x An object of class \code{SAKDiff}.
#' @param ... Ignored
#'
#' @seealso \code{\link{print.SAKPWDiff}}
#'
#' @export
print.SAKDiff <- function(x, ...) {
  cat("Bootstrapped effect size\n")
  cat(sprintf("  %s ~ %s\n", x$data.col.name, x$group.col.name))
  cat("Groups:\n")
  print(x$group.statistics)
  cat(sprintf("Pairwise %s effect size:\n", x$effect.type))
  for (i in seq_len(length(x$group.differences))) {
    print(x$group.differences[[i]])
  }
}

#' Print a summary of a SAK group difference object
#'
#' This is a method for the function \code{print()} for objects of class
#' \code{SAKPWDiff}, which is a row in the \code{group.differences} matrix
#' belonging to an object returned by a call to \code{\link{SAKDifference}}.
#'
#' @param x An object of class \code{SAKPWDiff}.
#' @param ... Ignored
#'
#' @seealso \code{\link{SAKDifference}}, \code{\link{print.SAKDiff}}
#' @export
print.SAKPWDiff <- function(x, ...) {
  cat(sprintf("  %s - %s: %g, %g%% CI (%s) [%g, %g]\n",
              x$groupLabels[1], x$groupLabels[2],
              x$t0, x$bca[1] * 100, x$ci.type, x$bca[4], x$bca[5]))
}

