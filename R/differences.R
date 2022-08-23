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
  x <- sqrt((N1 - 1) * (SD1 * SD1) * (N2 - 1) * (SD2 * SD2) / (N1 + N2 - 2))
  (m2 - m1) / x
}

# Hedges d (group 2 - group 1)
stHedgesD <- function(x1, x2){
  m1 <- mean(x1)
  SD1 <- stats::sd(x1)
  N1 <- length(x1)
  m2 <- mean(x2)
  SD2 <- stats::sd(x2)
  N2 <- length(x2)
  x <- sqrt((N1 - 1) * (SD1 * SD1) * (N2 - 1) * (SD2 * SD2) / (N1 + N2 - 2))
  d = (m2 - m1) / x
  d * (1 - (3 / (4 * (N1 + N2) - 9)))
}

calcPairDiff <- function(data, pair, data.col, group.col, id.col, effect.type, R, ci.type, ...) {

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

  # Decide how to calculate the statistic
  bootstrapData <- data
  if (effect.type == "unstandardised") {
    statistic <- .wrap2GroupStatistic(stMeanDiff)
  } else if (effect.type == "cohens") {
    statistic <- .wrap2GroupStatistic(stCohensD)
  } else if (effect.type == "hedges") {
    statistic <- .wrap2GroupStatistic(stHedgesD)
  } else if (effect.type == "paired") {
    if (missing(id.col)) stop("id.col must be specified when effect.type = 'paired'")
    g1 <- data[data[[group.col]] == pair[1], ]
    g2 <- data[data[[group.col]] == pair[2], ]
    # Pair on ID (don't assume they are sorted)
    g2Idx <- match(g1[[id.col]], g2[[id.col]])
    bootstrapData <- g2[[data.col]][g2Idx] - g1[[data.col]]
    statistic <- function(data, subset) {
      mean(data[subset])
    }
  } else {
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
  # Remove data because it is in the outer structure
  es$data <- NULL
  # Save CI type
  es$ci.type <- ci.type

  # Give it a class
  class(es) <- c("SAKPWDiff", class(es))

  es
}

# Returns the negation of the specified pairwise difference (type SAKPWDiff,
# usually a member of es$pairwise.differences)
negatePairwiseDiff <- function(pwd) {
  pwd$groups[[1]] <- rev(pwd$groups[[1]])
  pwd$t0 <- -pwd$t0
  pwd$t[[1]] <- -pwd$t[[1]]
  pwd$bca[4] <- -pwd$bca[4]
  pwd$bca[5] <- -pwd$bca[5]
  pwd
}


#############################################################################

#' Generate differences
#'
#' This is details
#'
#' @param data A data frame...
#' @param data.col Name or index of the column within \code{data} containing the
#'   measurement data.
#' @param group.col Name or index of the column within \code{data} containing
#'   the values to group by.
#' @param effect.type Type of difference
#' @param groups Vector of group names. Defaults to all groups in \code{data} in
#'   \emph{natural} order.
#' @param contrast TODO
#' @param effect.type Type of group difference to be calculated. Possible types
#'   are: \code{unstandardised}, difference in group means; \code{cohens},
#'   Cohen's d; \code{hedges}, Hedge's g, \code{pairwise}, pairwise differences.
#' @param R The number of bootstrap replicates.
#' @param ci.type A single character  string representing the type of bootstrap
#'   interval required. See the \code{type} parameter to the
#'   \code{\link{boot::boot.ci}} function for details.
#' @param na.rm a logical evaluating to TRUE or FALSE indicating whether NA
#'   values should be stripped before the computation proceeds. If NA values are
#'   stripped and `effect.type` is "paired", all rows (observations) for IDs
#'   with missing data are stripped.
#'
#' @return List containing:
#'
#' \itemize{
#'   \item{groups}{Vector of group names}
#'   \item{group.statistics}{Matrix with a row for each group, columns are group mean, median, standard deviation, standard error of the mean, lower and upper 95% confidence intervals of the mean}
#'   \item{effect.type}{Value of \code{effect.type} argument}
#'   \item{effect.name}{Pretty version of \code{effect.type}}
#'   \item{data.col}{Value of \code{data.col} argument}
#'   \item{data.col.name}{Name of the \code{data.col} column}
#'   \item{group.col}{Value of \code{group.col} argument}
#'   \item{group.col.name}{Name of the \code{group.col} column}
#'   \item{data}{bootstrapped mean difference}
#'   \item{data}{the input data}
#'   \item{call}{how this function was called}
#' }
#'
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
                       block.col = NULL, id.col,
                       groups = sort(unique(data[[group.col]])),
                       contrast = c("group 1 - group 2"), # default larger group minus small group TODO
                       effect.type = c("unstandardised", "cohens", "hedges", "paired"),
                       R = 1000,
                       ci.type = "bca",
                       na.rm = FALSE,
                       ...
                       # ci.type = "bca", #default
                       # ci.conf = 0.95,
) {

  effectNames <- c(unstandardised = "Mean difference", cohens = "Cohen's d", hedges = "Hedge's g")

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
    if (effect.type == "paired") {
      toKeep <- toKeep & !is.na(data[[id.col]])
      # Also delete pairs with missing data
      badPairIds <- unique(data[[id.col]][!toKeep])
      toKeep <- toKeep & !(data[[id.col]] %in% badPairIds)
    }
    data <- data[toKeep, ]
  }

  # Create return structure with administrative info
  .colName <- function(col) ifelse(is.numeric(col), names(data)[col], col)
  es <- list(data = data,
             call = match.call(),
             data.col = data.col,
             data.col.name = .colName(data.col),
             group.col = group.col,
             group.col.name = .colName(group.col),
             groups = groups,
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
      # CI function here (maybe no need to print to confuse with bootstrap CI, but need to be part of es for plotting CI)
      CI.lower = ci[1],
      CI.upper = ci[2]
    )
  })
  df <- do.call(rbind, gil)
  rownames(df) <- groups
  es$group.statistics <- df

  ### We will calculate effect size of all pairs (maybe rethink this later)
  # For each pair of groups... (convert groups to character to handle groups that are factors)
  es$pairwise.differences <- apply(utils::combn(as.character(groups), 2), 2, function(pair) {
    pair <- rev(pair)
    pairData <- data[as.character(data[[group.col]]) %in% pair, ]
    calcPairDiff(pairData, pair, data.col, group.col, id.col, effect.type, R, ci.type)
  })

  es
}

#######################################################################################
# Print methods

#' Print a summary of a SAK Difference object
#'
#' This is a method for the function \code{print()} for objects of class
#' "\code{SAKDiff}" created by a call to \link{\code{SAKDifference}}.
#'
#' @param x An object of class \link{\code{SAKDifference}}.
#' @param ... Ignored
#'
#' @export
print.SAKDiff <- function(x, ...) {
  cat("Bootstrapped effect size\n")
  cat(sprintf("  %s ~ %s\n", x$data.col, x$group.col))
  cat("Groups:\n")
  print(x$group.statistics)
  cat(sprintf("Pairwise %s effect size:\n", x$effect.type))
  for (i in seq_len(length(x$pairwise.differences))) {
    print(x$pairwise.differences[[i]])
  }
}

#' Print a summary of a SAK pairwise difference object
#'
#' This is a method for the function \code{print()} for objects of class
#' "\code{SAKPWDiff}", which is a row in the \code{pairwise.differences} matrix
#' belonging to an object returned by a call to \link{\code{SAKDifference}}.
#'
#' @param x An object of class \link{\code{SAKPWDiff}}.
#' @param ... Ignored
#' @export
print.SAKPWDiff <- function(x, ...) {
  cat(sprintf("  %s - %s: %g, %g%% CI (%s) %g, %g\n",
              x$groups[1], x$groups[2],
              x$t0, x$bca[1], x$ci.type, x$bca[4], x$bca[5]))
}

