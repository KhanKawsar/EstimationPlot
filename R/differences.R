# The difference function


### confidence interval of the mean a group
CI <- function(x, alpha = 0.95){
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

calcPairDiff <- function(data, pair, paired, pairNames, pairIndices, data.col, group.col, id.col, effect.type, R, ci.conf, ci.type, boot.params) {

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
    if (any(is.na(g1Idx))) {
      # Report missing data
      idColName <- id.col
      if (is.numeric(idColName))
        idColName <- names(data)[id.col]
      nBad <- sum(is.na(g1Idx))
      stop(sprintf("%d %s%s are not matched across groups in paired data", nBad, idColName, if (nBad == 1) "" else "s"))
    }
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
  es <- do.call(boot::boot, c(list(data = bootstrapData, statistic = statistic, R = R), boot.params))

  if (is.na(es$t0))
    stop(sprintf("Estimate is NA; do you need to specify na.rm = TRUE?"))

  # Calculate confidence interval
  ci <- boot::boot.ci(es, type = ci.type, conf = ci.conf)

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
  class(es) <- c("DurgaPWDiff", class(es))

  es
}



#############################################################################

#' Calculate group mean differences
#'
#' Calculates differences between groups ready for printing or plotting by
#' \code{\link{DurgaPlot}}.
#'
#' Data format - long format, one column with measurement (\code{data.col}),
#' another column with group identity (\code{group.col}). For repeated measures,
#' a subject identity column is also required (\code{id.col}).
#'
#' The formulae for Cohen's d and Hedges' g are from Lakens (2013), equations 1
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
#' @param id.col Specify for paired data/repeat measures only. Name or index of
#'   ID column for repeated measures/paired data. For non-paired data, do not
#'   specify an \code{id.col}.
#' @param effect.type Type of difference
#' @param groups Vector of group names. Defaults to all groups in \code{data} in
#'   \emph{natural} order. If \code{groups} is a named vector, the names are
#'   used to identify groups for printing or plotting.
#' @param contrasts Specify the pairs of groups to be compared. By default, all
#'   pairwise differences are generated. May be a single string, a vector of
#'   strings, or a matrix. A single string has a format such as \code{"group1 -
#'   group2, group3 - group4"}. A single asterisk, \code{"*"} creates contrasts
#'   for all possible pairs of groups. A single string such as \code{".-
#'   control"} compares all groups against the \code{"control"} group, i.e. the
#'   \code{"."} expands to all groups except the named group. A vector of
#'   strings looks like \code{c("group1 - group2", "group3 - group4")}. If a
#'   matrix is specified, it must have a column for each contrast, with the
#'   first group in row 1 and the second in row 2.
#' @param effect.type Type of group difference to be calculated. Possible types
#'   are: \code{unstandardised}, difference in group means; \code{cohens},
#'   Cohen's d; \code{hedges}, Hedges' g.
#' @param R The number of bootstrap replicates.
#' @param boot.params Optional list of additional names parameters to pass to
#'   the [boot]{boot} function.
#' @param ci.conf Numeric confidence level of the required confidence interval.
#'   Applies to both CI of differences between group means and CI of group means.
#' @param na.rm a logical evaluating to TRUE or FALSE indicating whether NA
#'   values should be stripped before the computation proceeds. If \code{TRUE}
#'   for "paired" data (i.e. \code{id.col} is specified), all rows
#'   (observations) for IDs with missing data are stripped.
#'
#' @return List containing:
#'
#'   \item{\code{groups}}{Vector of group names}
#'   \item{\code{group.statistics}}{Matrix with a row for each group, columns
#'   are group mean, median, standard deviation, standard error of the mean,
#'   lower and upper 95\% confidence intervals of the mean}
#'   \item{\code{group.differences}}{List of \code{DurgaPWDiff} objects, which are
#'   \code{boot} objects with added confidence interval information. See
#'   \code{\link[boot]{boot}} and \code{\link[boot]{boot.ci}}}
#'   \item{\code{effect.type}}{Value of \code{effect.type} parameter}
#'   \item{\code{effect.name}}{Pretty version of \code{effect.type}}
#'   \item{\code{data.col}}{Value of \code{data.col} parameter}
#'   \item{\code{data.col.name}}{Name of the \code{data.col} column}
#'   \item{\code{group.col}}{Value of \code{group.col} parameter}
#'   \item{\code{group.col.name}}{Name of the \code{group.col} column}
#'   \item{\code{data}}{The input data frame} \item{\code{call}}{how this
#'   function was called} \item{\code{groups}}{Value of \code{groups} parameter}
#'   \item{\code{group.names}}{Labels used to identify groups}
#'
#' @seealso \code{\link[boot]{boot}}, \code{\link[boot]{boot.ci}},
#'   \code{\link{DurgaPlot}}, \code{\link{print.DurgaDiff}}
#'
#' @references
#'
#' Lakens, D. (2013). Calculating and reporting effect sizes to facilitate
#' cumulative science: a practical primer for t-tests and ANOVAs. Frontiers in
#' Psychology, 4. doi:10.3389/fpsyg.2013.00863
#'
#' @export
DurgaDiff <- function(data,
                       data.col, group.col,
                       id.col,
                       groups = sort(unique(data[[group.col]])),
                       contrasts = "*",
                       effect.type = c("unstandardised", "cohens", "hedges"),
                       R = 1000,
                       boot.params = list(),
                       ci.conf = 0.95,
                       na.rm = FALSE
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
  data <- as.data.frame(data)

  pairedData <- !missing(id.col)

  effectNames <- c(unstandardised = "Mean difference", cohens = "Cohen's d", hedges = "Hedges' g")

  if (!is.function(effect.type))
    effect.type <- match.arg(effect.type)

  # Check column specifications
  .isACol <- function(spec) (is.numeric(spec) && spec >= 1 && spec <= ncol(data)) || (!is.numeric(spec) && spec %in% names(data))
  if (!.isACol(data.col))
    stop(sprintf("data.col %s is not a valid column name or index (names are %s)",
                 data.col, paste(names(data), collapse = ", ")))
  if (!is.numeric(data[[data.col]]))
    stop(sprintf("data.col %s must be a numeric column, is %s", data.col, class(data[[data.col]])))
  if (!.isACol(group.col))
    stop(sprintf("group.col %s is not a valid column name or index (names are %s)",
                 group.col, paste(names(data), collapse = ", ")))
  if (pairedData && !.isACol(id.col))
    stop(sprintf("id.col %s is not a valid column name or index (names are %s)",
                 id.col, paste(names(data), collapse = ", ")))

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

  # Sanity check
  if (nrow(data) == 0) stop("No data to analyse!")

  # Create return structure with administrative info
  .colName <- function(col) ifelse(is.numeric(col), names(data)[col], col)
  groupLabels <- names(groups)
  if (is.null(groupLabels)) {
    groupLabels <- as.character(groups)
  } else {
    groupLabels <- ifelse(groupLabels == "", as.character(groups), groupLabels)
  }
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
             effect.name = effectNames[effect.type],
             explicit.contrasts = !missing(contrasts))
  # Return value has type DurgaDiff
  class(es) <- c("DurgaDiff", class(es))

  # Fill in statistical summary about each of the groups
  gil <- lapply(groups, function(g) {
    grpVals <- data[[data.col]][data[[group.col]] == g]
    ci <- CI(grpVals, ci.conf)
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

  # Interpret the contrasts
  contrasts <- expandContrasts(contrasts, groups)
  if (is.null(contrasts)) {
    es$group.differences <- NULL
  } else {
    # For each pair of groups...
    es$group.differences <- apply(contrasts, 2, function(pair) {
      pairData <- data[as.character(data[[group.col]]) %in% pair, ]
      groupIndices <- c(which(groups == pair[1]), which(groups == pair[2]))
      groupLabels <- c(groupLabels[groupIndices[1]],
                       groupLabels[groupIndices[2]])
      calcPairDiff(pairData, pair, pairedData, groupLabels, groupIndices, data.col, group.col, id.col, effect.type, R, ci.conf, ci.type, boot.params)
    })
  }

  es
}

#######################################################################################
# Print methods

#' Print a summary of a Durga Difference object
#'
#' This is a method for the function \code{print()} for objects of class
#' \code{DurgaDiff} created by a call to {DurgaDiff}.
#'
#' @param x An object of class \code{DurgaDiff}.
#' @param ... Ignored
#'
#' @seealso \code{\link{print.DurgaPWDiff}}
#'
#' @export
print.DurgaDiff <- function(x, ...) {
  cat("Bootstrapped effect size\n")
  cat(sprintf("  %s ~ %s\n", x$data.col.name, x$group.col.name))
  cat("Groups:\n")
  print(x$group.statistics)
  cat(sprintf("Pairwise %s effect size:\n", x$effect.type))
  for (i in seq_len(length(x$group.differences))) {
    print(x$group.differences[[i]])
  }
}

#' Print a summary of a Durga group difference object
#'
#' This is a method for the function \code{print()} for objects of class
#' \code{DurgaPWDiff}, which is a row in the \code{group.differences} matrix
#' belonging to an object returned by a call to \code{\link{DurgaDiff}}.
#'
#' @param x An object of class \code{DurgaPWDiff}.
#' @param ... Ignored
#'
#' @seealso \code{\link{DurgaDiff}}, \code{\link{print.DurgaDiff}}
#' @export
print.DurgaPWDiff <- function(x, ...) {
  cat(sprintf("  %s - %s: %g, %g%% CI (%s) [%g, %g]\n",
              x$groupLabels[1], x$groupLabels[2],
              x$t0, x$bca[1] * 100, x$ci.type, x$bca[4], x$bca[5]))
}

