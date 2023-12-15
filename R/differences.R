# The difference function

#_____________________________________________________________#
#### Private functions ####

# Confidence interval of the mean of a group
mean.CI <- function(x, alpha = 0.95) {
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

# Bootstrapped confidence interval of the mean of a group
#
mean.CI.boot <- function(x, alpha = 0.95, R = 1000) {
  bmean <- function(x, i) mean(x[i], na.rm = TRUE) # How should we handle NAs?
  if (length(unique(x)) < 3 || is.na(R)) {
    # Silently return NAs
    c(NA, NA)
  } else {
    b <- boot::boot(x, bmean, R)
    ci <- boot::boot.ci(b, conf = alpha, type = "bca")
    ci[["bca"]][4:5]
  }
}


#---------------------------------------------
# Building blocks for effect size calculations


#### Bias correction ####

# Hedges' bias correction, approximate method. Hedges (1981) pp. 114,  Cumming
# (2012) equation 11.13. The approximate method is approximately twice as fast
# to calculate as the exact method.
#
# @param df Degrees of freedom in standardisation value, i.e. denominator in
#   effect size calculation
esBiasCorrectApprox <- function(df) {
  1 - (3 / (4 * df - 1))
}

# Hedges' bias correction, exact method (Hedges 1981, equation 6e)
#
# @param df Degrees of freedom in standardisation value, i.e. denominator in
#   effect size calculation.
esBiasCorrectExact <- function(df) {
  # By working with logs then taking the exp, this calculation works for large values of df
  exp(lgamma(df / 2) - log(sqrt(df / 2)) - lgamma((df - 1) / 2))
}

# Allow approximate bias correction for testing purposes
#esBiasCorrect <- esBiasCorrectApprox
esBiasCorrect <- esBiasCorrectExact


#### Effect size components ####

# Difference of groups means, mean(group 2) - mean(group 1)
esDiffOfMeans <- function(x1, x2) { mean(x2) - mean(x1) }
# Means of group differences, mean(group 2 - group 1).
esMeanOfDiffs <- function(x1, x2) { mean(x2 - x1) }

# Calculate the pooled and weighted (by sample size) standard deviation of two
# groups. This is the denominator in the original Cohen's d.
esWeightedPooledSD <- function(x1, x2) {
  N1 <- length(x1)
  N2 <- length(x2)
  SD1 <- stats::sd(x1)
  SD2 <- stats::sd(x2)
  sqrt(((N1 - 1) * (SD1 * SD1) + (N2 - 1) * (SD2 * SD2)) / (N1 + N2 - 2))
}

# Calculate the unweighted pooled standard deviation of 2 groups. This is the
# expression for pooled SD when both groups are equal in size, the denominator
# in the original Cohen's d for unpaired data and Cohens d_av for paired data,
# and also the denominator for Cohen's d, where they name it the non-pooled
# standard deviation (which I think is a poor choice of terminology).
esUnweightedPooledSD <- function(x1, x2) {
  SD1 <- stats::sd(x1)
  SD2 <- stats::sd(x2)
  sqrt((SD1 * SD1 + SD2 * SD2) / 2)
}

# Returns the average of the SDs of both groups
esAverageSD <- function(x1, x2) {
  (stats::sd(x1) + stats::sd(x2)) / 2
}

# Calculate degrees of freedom from the two group sizes. The calculation to be used depends on the nature of the standardisation
esDFAsterisk <- function(x1, x2) {
  N1 <- length(x1)
  S1 <- stats::sd(x1)
  N2 <- length(x2)
  S2 <- stats::sd(x2)
  # Equation 16 in Delacre et al., 2021
  (N1 - 1) * (N2 - 1) * (S1^2 + S2^2)^2 / ((N2 - 1) * S1^4 + (N1 - 1) * S2^4)
}

#--------------------------------------------------------------------
#### Effect size calculations ####
# Dep = paired
# Ind = unpaired

# ---- Paired ---- #

# Paired Cohen's d_s
stDepCohensD_z <- function(x1, x2) {
  diffs <- x2 - x1
  mean(diffs) / stats::sd(diffs)
}

# Paired Hedges' g_s
stDepHedgesG_z <- function(x1, x2) {
  diffs <- x2 - x1
  mean(diffs) / stats::sd(diffs) * esBiasCorrect(length(x1) - 1)
}

# Paired Cohen's d_av
# Eqn 10 Lakens (2013)
stDepCohensD_av <- function(x1, x2) {
  esMeanOfDiffs(x1, x2) / esAverageSD(x1, x2)
}

stDepHedgesG_av <- function(x1, x2) {
  esMeanOfDiffs(x1, x2) / esAverageSD(x1, x2) * esBiasCorrect(length(x1) - 1)
}

# Cohen's d_av according to Cumming's (2012) equation (11.10), paired samples comparison
stDepCummingsD_av <- function(x1, x2) esMeanOfDiffs(x1, x2) / esUnweightedPooledSD(x1, x2)

# Cumming's d_av with Hedges' bias correction applied
stDepCummingsG_av <- function(x1, x2) esMeanOfDiffs(x1, x2) / esUnweightedPooledSD(x1, x2) * esBiasCorrect(length(x1) - 1)


# ---- Unpaired ---- #

# Unpaired Cohen's d_s
# Lakens (2013) eqn 1
stIndCohensD_s <- function(x1, x2) {
  esDiffOfMeans(x1, x2) / esWeightedPooledSD(x1, x2)
}

# Unpaired Hedges' g_s
stIndHedgesG_s <- function(x1, x2) {
  esDiffOfMeans(x1, x2) / esWeightedPooledSD(x1, x2) * esBiasCorrect(length(x1) + length(x2) - 2)
}

# Unpaired Cohens d
# Delacre et al., 2021 (equation unnumbered, but located before eqn 15)
stIndCohensDAst <- function(x1, x2) {
  esDiffOfMeans(x1, x2) / esUnweightedPooledSD(x1, x2)
}

# Unpaired Hedges' g
stIndHedgesGAst <- function(x1, x2) {
  esDiffOfMeans(x1, x2) / esUnweightedPooledSD(x1, x2) * esBiasCorrect(esDFAsterisk(x1, x2))
}

# Glass's delta, pre-measurement
stIndGlassDeltaPre <- function(x1, x2) {
  # Standardise with pre-measurement deviation
  esDiffOfMeans(x1, x2) / stats::sd(x2)
}

# Glass's delta, post-measurement
stIndGlassDeltaPost <- function(x1, x2) {
  # Standardise with post-measurement deviation
  esDiffOfMeans(x1, x2) / stats::sd(x1)
}

#### General stuff ####

# Combines multiple variables into one
#
# Simply combines all variables for each row into a single variable, delimited
# by " & " (by default). This is a naive implementation that can fail in
# pathological cases by mapping different combinations of variables to the same
# string. A correct implementation would be much more complicated.
#
# @param x A data frame (or similar) variable columns.
# @param col Names or indices of the columns within \code{x} containing the
#   variables to be combined.
# @param delim The delimiter used to separate values from different variables.
#
# @return Vector of combined variables, with one value for each row in `x`.
combineVariables <- function(x, col, delim = " & ") {
  if (length(col) < 2) {
    x[, col]
  } else {
    apply(x[, col], 1, paste, collapse = delim)
  }
}


# Given an effect type's code and paired/unpaired flag, return a descriptor about the effect type.
# This is where the various effect types are defined
lookupStat <- function(code, paired) {
  # This function just constructs a list, but it also documents and enforces the arguments
  # @param code The code that identifies the effect type, as passed to DurgaDiff
  # @param paired TRUE if data are paired
  # @param fun Function to calculate effect size
  # @param label Label used to describe effect size in plots
  # @param label.print Label used to describe effect size when printing to console
  Stat <- function(code, paired, fun, label, label.print) {
    if (!is.function(fun)) stop("Invalid effect size function")
    list(code = code, paired = paired, label = label, label.print = label.print, fun = fun)
  }

  allStats <- list(
    Stat("mean",         TRUE,  esMeanOfDiffs, "Mean difference", "Mean of differences"),
    Stat("hedges g_av",  TRUE,  stDepHedgesG_av, expression("Hedges' g"[av]), "Hedges' g_av"),
    Stat("cohens d",    TRUE, stDepCummingsD_av, expression("Cohen's d"), "Cohen's d"),
    Stat("hedges g",    TRUE, stDepCummingsG_av, expression("Hedges' g"), "Hedges' g"),
    Stat("cohens d_av",  TRUE,  stDepCohensD_av, expression("Cohen's d"[av]), "Cohen's d_av"),
    Stat("hedges g_z",   TRUE,  stDepHedgesG_z, expression("Hedges' g"[z]), "Hedges' g_z"),
    Stat("cohens d_z",   TRUE,  stDepCohensD_z, expression("Cohen's d"[z]), "Cohen's d_z"),

    Stat("mean",         FALSE, esDiffOfMeans, "Mean difference", "Difference of means"),
    Stat("hedges g",    FALSE, stIndHedgesGAst, expression("Cohen's g"), "Cohen's g"),
    Stat("cohens d",    FALSE, stIndCohensDAst, expression("Cohen's d"), "Cohen's d"),
    Stat("hedges g_s",   FALSE, stIndHedgesG_s, expression("Hedges' g"[s]), "Hedges' g_s"),
    Stat("cohens d_s",   FALSE, stIndCohensD_s, expression("Cohen's d"[s]), "Cohen's d_s"),

    Stat("glass delta_pre",  FALSE, stIndGlassDeltaPre, expression(paste("Glass's " * Delta["pre"])), "Glass's delta_pre"),
    Stat("glass delta_post", FALSE, stIndGlassDeltaPost, expression(paste("Glass's " * Delta["post"])), "Glass's delta_post")
  )

  if (is.function(code)) {
    # Handle custom function.
    Stat("custom function", paired, code, "Custom effect type", "Custom effect type")
  } else {
    # Handle standard effect type. Code comparison is case insensitive
    candidates <- Filter(function(st) st$code == tolower(code) && st$paired == paired, allStats)
    if (length(candidates) != 1) {
      # Try to report available effect types
      types <- sapply(Filter(function(st) st$paired == paired, allStats), function(st) st$code)
      what <- ifelse(paired, "paired", "unpaired")
      stop(sprintf("Unable to calculate effect type '%s' on %s data, available %s effect types:\n\t%s\n",
                   code, what, what, paste(types, collapse = ", ")))
    }
    candidates[[1]]
  }
}


# Calculate the difference statistic for a pair of groups
# @param data Values for the two groups
# @param pair Vector with the two group names
# @param isPaired Boolean, TRUE if data are paired
# @param pairIndices Indices within groups of the two groups (not used here,
#   just added to the return structure)
# @param data.col Name/index of the data column
# @param group.col Name/index of the group column
# @param id.col Name/index of the id column
# @param etDescr Descriptor with details of the statistic to be calculated
# @param R Number of bootstrap replicates
# @param ci.conf Level for confidence interval
# @param ci.type Value passed to the `boot::boot.ci` `type` parameter
# @param boot.params List of additional argument to pass to boot::boot
# @param boot.ci.params List of additional argument to pass to boot::boot.ci
calcPairDiff <- function(data, pair, isPaired, pairNames, pairIndices, data.col, group.col, id.col,
                         etDescr, R, ci.conf, ci.type, boot.params, boot.ci.params) {

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

  # The function should calculate the mean diff as mean(g2 - g1)
  .wrapPairedStatistic <- function(statisticFn) {
    function(data, indices) {
      statisticFn(data$g2[indices], data$g1[indices])
    }
  }

  # Decide how to calculate the statistic. Different types of functions required different arguments
  statistic <- NULL
  if (isPaired) {
    # Paired data
    g1 <- data[data[[group.col]] == pair[1], ]
    g2 <- data[data[[group.col]] == pair[2], ]
    # Check that all specimens occur in both groups
    missing <- unique(c(setdiff(g2[[id.col]], g1[[id.col]]),
                      setdiff(g1[[id.col]], g2[[id.col]])))
    if (length(missing) > 0) {
      # Report missing data
      idColName <- id.col
      if (is.numeric(idColName))
        idColName <- names(data)[id.col]
      nBad <- length(missing)
      stop(sprintf("%d %s%s are not matched across groups '%s' and '%s' in paired data",
                   nBad, idColName, if (nBad == 1) "" else "s", pair[1], pair[2]))
    }
    # Pair on ID (don't assume they are sorted)
    g1Idx <- match(g2[[id.col]], g1[[id.col]])

    # Paired functions accept two arguments, the two groups, however
    # they assume that rows match in the groups
    bootstrapData <- data.frame(g1 = g1[[data.col]][g1Idx], g2 = g2[[data.col]])
    strata <- rep(1, nrow(bootstrapData))

    # Build the statistic function
    statistic <- .wrapPairedStatistic(etDescr$fun)

  } else {
    # Unpaired data. Only bootstrap rows that are in the two groups of interest
    groups <- data[[group.col]]
    bootstrapData <- data[groups %in% pair, ]
    # Sample from each group independently
    strata <- factor(bootstrapData[[group.col]])

    statistic <- .wrap2GroupStatistic(etDescr$fun)
  }

  # Bootstrap the statistic. The strata argument means that we bootstrap within each group separately
  es <- do.call(boot::boot, c(list(data = bootstrapData, strata = strata,
                                   statistic = statistic, R = R, stype = "i"), boot.params))

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

# Convert a wide-format paired data set to long format
wideToLong <- function(x, groups, id.col) {
  # Check columns exist
  badCols <- !(groups %in% names(x))
  if (any(badCols)) {
    stop(sprintf("Invalid group name%s \"%s\"; in wide format, groups must be column names.\nAvailable columns are: %s",
                 ifelse(sum(badCols) == 1, "", "s"), paste(groups[badCols], collapse = ", "), paste(names(x), collapse = ", ")))
  }
  # Pick unique column names so we don't overwrite an existing column
  .pickCol <- function(base) {
    existingNames <- names(x)
    nm <- base
    suf <- 1
    while (nm %in% existingNames) {
      nm <- sprintf("%s.%d", base, suf)
      suf <- suf + 1
    }
    nm
  }
  if (missing(id.col) || is.null(id.col) || id.col == "")
    id.col <- .pickCol("id")
  data.col <- .pickCol("value")
  group.col <- .pickCol("group")
  x <- stats::reshape(x, direction = "long",
                      varying = groups,
                      idvar = id.col,
                      v.names = data.col,
                      timevar = group.col,
                      times = groups
  )
  attr(x, "id.col") <- id.col
  attr(x, "data.col") <- data.col
  attr(x, "group.col") <- group.col
  x
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
#' \code{\link{DurgaPlot}}. The formula interface allows the value and group
#' columns to be specified in a formula, which means, for example, that
#' transformation functions can be applied to columns.
#'
#' Applies the formula, \code{x}, and a data set, \code{data}, to construct a
#' data frame that is then passed, with all remaining arguments, to the function
#' \code{\link{DurgaDiff.default}}.

#' @inherit DurgaDiff.default
#'
#' @inheritDotParams DurgaDiff.default -data.col -group.col -id.col
#' @param x a formula, such as \code{y ~ grp}, where \code{y} is a numeric
#'   vector of data values or measurements to be split into groups according to
#'   the grouping variable \code{grp}, which is typically a categorical value.
#'   Multiple group columns can be separated by `+`, in which case Durga treats
#'   each unique combination of group variables as a distinct group.
#' @param data a data.frame (or list) from which the variables in formula should
#'   be taken.
#'
#' @examples
#'
#' d <- DurgaDiff(log(sugar) ~ treatment, insulin, id.col = "id")
#' print(d)
#'
#' @seealso \code{\link{DurgaDiff.default}}, \code{\link[boot]{boot}},
#'   \code{\link[boot]{boot.ci}}, \code{\link{DurgaPlot}}
#'
#' @export
DurgaDiff.formula <- function(x, data = NULL, id.col, ...) {

  # Interpret the formula
  md <- stats::model.frame(x, data)

  # We assume: response variable is the data.col
  data.col <- 1
  # Remaining variables are groups
  group.col <- seq(2, ncol(md))

  # If id.col specified...
  if (!missing(id.col) && !is.na(id.col) && !is.null(id.col)) {
    # Add id.col to the model data set
    md <- cbind(md, data[[id.col]])
    colnames(md)[ncol(md)] <- ifelse(is.character(id.col), id.col, names(data)[id.col])
    d <- DurgaDiff(md, data.col, group.col, ncol(md), ...)
  } else {
    d <- DurgaDiff(md, data.col, group.col, ...)
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
#' ## Data format
#'
#' \code{x} may be a formula; see \code{\link{DurgaDiff.formula}}.
#'
#' If \code{x} is a \code{data.frame} (or similar) it may be in either _long_ or _wide_
#' format. In long format, one column (`data.col`) contains the measurement or value to be
#' compared, and another column (\code{group.col}) contains the group identity. Repeated
#' measures/paired data/within-subject comparisons in long format require a subject
#' identity column (\code{id.col}).
#'
#' Wide format contains different measurements in different columns of the same row, and
#' is well-suited for repeated measures/paired/within-subject comparison data. To pass
#' wide format data, do not specify the arguments \code{data.col} or
#' \code{group.col}. Instead, you must explicitly specify the groups to be
#' compared in the \code{groups} argument. Each group must be the name of a
#' column in \code{x}. For paired data, you may specify \code{id.col}, although it is not
#' required, as wide format data is assumed to be paired. The \code{id.col} can be a column
#' that already exists and uniquely identifies each specimen, or it can be the name of a
#' column to be created, in which case the specimen ID will be a generated integer sequence.
#' Unpaired data can be in wide format, but it is necessary to inform Durga by passing `id.col = NULL`.
#' Wide format data will be internally converted to long format, then processing continues as
#' for long format input.
#'
#' ## Contrasts
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
#' ## Effect types
#'
#' The \code{effect.type} parameter determines the effect size measure to be
#' calculated. Our terminology generally follows Lakens (2013), with _d_ meaning
#' a biased estimate and _g_ meaning a bias-corrected estimate. Some writers
#' reverse this usage or use alternative terminology. Cumming (2012) recommends
#' always using a bias-corrected estimate (although bias
#' correction is unnecessary for large sample sizes).
#' Durga applies Hedges' exact method for bias correction.
#'
#' The effect type we call
#' \eqn{Cohen's\text{ }d} for unpaired data is called \eqn{Cohen's\text{ }d_s^*}
#' by Delacre et al. (2021). For paired data, our \eqn{Cohen's\text{ }d} is
#' identical to \eqn{Cohen's\text{ }d}
#' for unpaired data (Delacre et al. 2021); it is called \eqn{d_{av}}
#' by Cumming (2012; equation 11.10). For further details, refer to Khan and McLean (2023).
#'
#' The set of possible values for the \code{effect.type} argument, and
#' their meanings, is described below.
#'
#' ### Unpaired effect types
#'
#' | **Code** | **Label** | **Effect type** | **Standardiser** |
#' | --- | --- | --- | --- |
#' | `mean` | \eqn{Mean\text{ }difference} | Unstandardised difference of group means | NA |
#' | `cohens d` | \eqn{Cohen's\text{ }d} | Difference in means standardised by non-pooled average SD (Delacre et al. 2021) | \eqn{\sqrt{({s_1}^2 + {s_2}^2)/2}} |
#' | `hedges g` | \eqn{Hedges'\text{ }g} | Bias-corrected \eqn{Cohen's\text{ }d} (Delacre et al. 2021) | \eqn{\sqrt{({s_1}^2 + {s_2}^2)/2}} |
#' | `cohens`&nbsp;`d_s` | \eqn{Cohen's\text{ }d_s} | Difference in means standardised by the pooled standard deviation (Lakens 2013, equation 1) | \eqn{\sqrt{\frac{(n_1-1){s_1}^2 + (n_2-1){s_2}^2}{{n_1} + {n_2} - 2}}} |
#' | `hedges g_s` | \eqn{Hedges'\text{ }g_s} | Bias-corrected \eqn{Cohen's\text{ }d_s} (Lakens 2013, equation 4) | \eqn{\sqrt{\frac{(n_1-1){s_1}^2 + (n_2-1){s_2}^2}{{n_1} + {n_2} - 2}}} |
#' | `glass`&nbsp;`delta_pre` |\eqn{Glass's\text{ }\Delta_{pre}} | Difference in means standardised by the standard deviation of the pre-measurement group (which is the 2nd group in a contrast). Lakens (2013) recommends using Glass's \eqn{\Delta} whenever standard deviations differ substantially between conditions | \eqn{s_2} |
#' | `glass`&nbsp;`delta_post` | \eqn{Glass's\text{ }\Delta_{post}} | Difference in means standardised by the standard deviation of the post-measurement group (which is the 1st group in a contrast) | \eqn{s_1} |
#'
#' ### Paired effect types
#'
#' | **Code** | **Label** | **Effect type** | **Standardiser** |
#' | --- | --- | --- | --- |
#' | `mean` | \eqn{Mean\text{ }difference} | Unstandardised mean of group differences | NA |
#' | `cohens`&nbsp;`d` | \eqn{Cohen's\text{ }d} | Similar to \eqn{Cohen's\text{ }d_{av}} except that the normaliser is non-pooled average SD rather than mean SD, as recommended by Cummings (2012, eqn 11.9) | \eqn{\sqrt{({s_1}^2 + {s_2}^2)/2}} |
#' | `hedges`&nbsp;`g` | \eqn{Hedges'\text{ }g} | Bias-corrected \eqn{Cohen's\text{ }d} | \eqn{\sqrt{({s_1}^2 + {s_2}^2)/2}} |
#' | `cohens`&nbsp;`d_z` | \eqn{Cohen's\text{ }d_z} | Mean of differences, standardised by the standard deviation of the differences, (Lakens 2013, equation 6). Cummings (2012) recommends against using \eqn{Cohen's\text{ }d_z}, preferring \eqn{Cumming's\text{ }d_{av}}  | \eqn{\sqrt{\frac{\sum{({X_{diff}} - {M_{diff}})^2}}{n-1}}} |
#' | `hedges g_z` | \eqn{Hedges'\text{ }g_z} | Bias-corrected \eqn{Cohen's\text{ }d_z} | \eqn{\sqrt{\frac{\sum{({X_{diff}} - {M_{diff}})^2}}{n-1}}} |
#' | `cohens`&nbsp;`d_av` | \eqn{Cohen's\text{ }d_{av}} | Difference in means standardised by the average standard deviation of the groups (Lakens 2013, equation 10) | \eqn{\dfrac{{s_1} + {s_2}}{2}} |
#' | `hedges`&nbsp;`g_av` | \eqn{Hedges'\text{ }g_{av}} | Bias-corrected \eqn{Cohen's\text{ }d_{av}} | \eqn{\dfrac{{s_1} + {s_2}}{2}} |
#'
#' As a simple rule of thumb, if you want a standardised effect type and you
#' don't know which one to use, use `"hedges g"` for either paired or unpaired data,
#' as it is recommended by Delacre et al., (2021) for unpaired data and cumming (2012)
#' for paired data.
#'
#' Additional effect types can be applied by passing a function for
#' \code{effect.type}. The function must accept two
#' parameters and return a single numeric value, the effect size.
#' Each parameter is a vector of values from one of the two groups to be
#' compared (group 2 and group 1).
#'
#'
#' ## Confidence intervals
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
#'   \code{x} containing the measurement data.
#' @param group.col Name or index of the column within \code{x} containing
#'   the values to group by. May be a vector of column names/indices, in
#'   which case values from each column are concatenated to define groups.
#' @param id.col Specify for paired data/repeated measures/with-subject
#'   comparisons only. Name or index of ID column for repeated measures/paired
#'   data. Observations for the same individual must have the same ID. For
#'   non-paired data, do not specify an \code{id.col}, (or use \code{id.col =
#'   NA}).
#' @param groups Vector of group names. Defaults to all groups in \code{x} in
#'   \emph{natural} order. If \code{groups} is a named vector, the names are
#'   used as group labels for plotting or printing. If \code{data.col} and
#'   \code{group.col} are not specified, \code{x} is assumed be to in \emph{wide
#'   format}, and \code{groups} must be a list of column names identifying the
#'   group/treatment data (see example).
#' @param contrasts Specify the pairs of groups to be compared. By default, all
#'   pairwise differences are generated. May be a single string, a vector of
#'   strings, or a matrix. Specify
#'   \code{NULL} to avoid calculating any contrasts. See Details for more information.
#' @param effect.type Type of group difference to be estimated. Values cannot be
#'   abbreviated. See Details for further information.
#' @param R The number of bootstrap replicates. \code{R} should be larger than
#'   your sample size, so the default value of 1000 may need to be increased for
#'   large sample sizes. If \code{R <= nrow(x)}, an error such as "\code{Error in
#'   bca.ci... estimated adjustment 'a' is NA}" will be thrown. Additionally,
#'   warnings such as "\code{In norm.inter(t, adj.alpha) : extreme order
#'   statistics used as endpoints}" may be avoided by increasing \code{R}.
#'   Specify \code{R = NA} if you do not wish to calculate any CIs, either
#'   for group means for for effect sizes. This may be useful if Durga is
#'   only being used for plotting large data sets.
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
#'   and upper bootstrapped confidence intervals of the mean, confidence level
#'   as set by the \code{ci.conf} parameter) and \code{n} (group sample size).
#'   If there are fewer than 3 distinct values in the group, or if \code{R} is
#'   \code{NA}, the confidence interval will not be calculated and
#'   \code{CI.lower} and \code{CI.upper} will be \code{NA}.}
#'
#'   \item{\code{group.differences}}{List of \code{DurgaGroupDiff} objects,
#'   which are \code{boot} objects with added confidence interval information.
#'   See \code{\link[boot]{boot}} and \code{\link[boot]{boot.ci}}. This element will be missing
#'   if \code{contrasts} is empty or \code{NULL}}
#'
#'   \item{\code{groups}}{Vector of group names}
#'   \item{\code{group.names}}{Labels used to identify groups}
#'   \item{\code{effect.type}}{Value of \code{effect.type} parameter}
#'   \item{\code{effect.name}}{Name of the effect type; may include formatting
#'   such as subscripts} \item{\code{effect.name.print}}{Text-only version of
#'   \code{effect.name} for printing; subscripts are indicated by \code{"_"}}
#'   \item{\code{data.col}}{Value of \code{data.col} parameter; may be an index
#'   or a name} \item{\code{data.col.name}}{Name of the \code{data.col} column}
#'   \item{\code{group.col}}{Value of \code{group.col} parameter; may be an
#'   index or a name}
#'   \item{\code{group.col.name}}{Name of the \code{group.col} column}
#'   \item{\code{id.col}}{Value of \code{id.col} parameter. May be \code{NULL}}
#'   \item{\code{paired.data}}{\code{TRUE} if paired differences
#'   were estimated}
#'   \item{\code{data}}{The input data frame (\code{x}), or the reshaped (long format) data
#'   frame if the input data set was in wide format}
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
#'                groups = c("Self-fertilised" = "self_fertilised",
#'                           "Intercrossed" = "inter_cross",
#'                           "Westerham-crossed" = "westerham_cross"),
#'                contrasts = c("Westerham-crossed - Self-fertilised",
#'                              "Westerham-crossed - Intercrossed",
#'                              "Intercrossed - Self-fertilised"))
#'
#' # Wide format data
#' d <- DurgaDiff(insulin.wide, groups = c("sugar.before", "sugar.after"))
#'
#' @references
#'
#' - Cumming, G. (2012). Understanding the new statistics : effect sizes,
#' confidence intervals, and meta-analysis (1st ed.). New York: Routledge.
#'
#' - Delacre, M., Lakens, D., Ley, C., Liu, L., & Leys, C. (2021). Why
#' Hedges’ g* based on the non-pooled standard deviation should be reported
#' with Welch’s t-test. [doi:10.31234/osf.io/tu6mp](https://doi.org/10.31234/osf.io/tu6mp)
#'
#' - Khan, M. K., & McLean, D. J. (2023). Durga: An R package for effect size estimation
#' and visualisation. bioRxiv, 2023.2002.2006.526960.
#' [doi:10.1101/2023.02.06.526960](https://doi.org/10.1101/2023.02.06.526960)
#'
#' - Lakens, D. (2013). Calculating and reporting effect sizes to facilitate
#' cumulative science: a practical primer for t-tests and ANOVAs. Frontiers in
#' Psychology, 4. [doi:10.3389/fpsyg.2013.00863](https://doi.org/10.3389/fpsyg.2013.00863)
#'
#' @export
DurgaDiff.default <- function(x,
                      data.col, group.col,
                      id.col,
                      groups,
                      contrasts = "*",
                      effect.type = "mean",
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

  # Interpret wide format data by converting to long format
  if (missing(data.col) && missing(group.col) && !missing(groups)) {
    x <- wideToLong(x, groups, id.col)
    data.col <- attr(x, "data.col")
    group.col <- attr(x, "group.col")
    # Allow unpaired wide format with an explicit NULL for id.col
    id.col <- if (!missing(id.col) && is.null(id.col)) { NULL } else { attr(x, "id.col") }
  }

  pairedData <- !missing(id.col) && !is.null(id.col) && !is.na(id.col)

  # Check column specifications
  .isACol <- function(spec) (is.numeric(spec) && all(spec >= 1) && all(spec <= ncol(x))) || (!is.numeric(spec) && all(spec %in% names(x)))
  .colName <- function(col) if (is.numeric(col)) { names(x)[col] } else { col }
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

  # Handle multiple groups ("interactions")
  if (length(group.col) > 1) {
    # Create a new column which contains the combined values from each group column
    groupCol <- combineVariables(x, group.col, ", ")
    groupColName <- paste(.colName(group.col), collapse = ".")
    if (groupColName %in% names(x)) stop(sprintf("Unable to create group interaction column '%s', column already exists", groupColName))
    ocn <- colnames(x)
    x <- cbind(x, groupCol)
    colnames(x) <- c(ocn, groupColName)
    group.col <- groupColName
  }

  # Now assign default groups value
  if (missing(groups)) {
    groups <- sort(unique(x[[group.col]]))
  }

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

  # Lookup effect type and get details about it
  etDescr <- lookupStat(effect.type, pairedData)

  # Create return structure with administrative info
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
             effect.type = etDescr$code,
             effect.name = etDescr$label,             # Plottable label
             effect.name.print = etDescr$label.print, # Printable label
             explicit.contrasts = !missing(contrasts))
  # Return value has type DurgaDiff
  class(es) <- c("DurgaDiff", class(es))

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
                   etDescr, R, ci.conf, ci.type, boot.params, boot.ci.params)
    })
  }

  # Fill in statistical summary about each of the groups. Do this after
  # calculating group differences to preserve backwards compatibility of
  # reproducible bootstraps in version 1.1.0.9000: there are now random numbers
  # generated by this code
  gil <- lapply(groups, function(g) {
    grpVals <- x[[data.col]][x[[group.col]] == g]
    #ci <- mean.CI(grpVals, ci.conf)
    ci <- mean.CI.boot(grpVals, ci.conf, R = R)
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
  print(x$group.statistics, ...)
  cat(sprintf("%s %s (R = %d, bootstrap CI method = %s):\n",
              ifelse(x$paired.data, "Paired", "Unpaired"),
              x$effect.name.print,
              # Note R and ci.type should be the same for all comparisons
              x$group.differences[[1]]$R,
              x$group.differences[[1]]$ci.type))
  for (i in seq_len(length(x$group.differences))) {
    print(x$group.differences[[i]], ...)
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

