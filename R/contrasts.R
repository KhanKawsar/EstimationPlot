# Functions for expanding string representations of contrasts into a matrix


# Returns TRUE if group is the first group in the contrast
.group1 <- function(group, contrast) {
  # Does the string start with the group, ignoring whitespace?
  contrast <- trimws(contrast)
  if (!startsWith(contrast, group)) {
    return(FALSE)
  }
  # It must now be followed by a "-"
  remainder <- trimws(substring(contrast, nchar(group) + 1))
  startsWith(remainder, "-")
}

# Returns TRUE if group is the second group in the contrast
.group2 <- function(group, contrast) {
  # Does the string end with the group, ignoring whitespace?
  contrast <- trimws(contrast)
  if (!endsWith(contrast, group)) {
    return(FALSE)
  }
  # It must now be preceeded by a "-"
  remainder <- trimws(substring(contrast, 1, nchar(contrast) - nchar(group)))
  endsWith(remainder, "-")
}

# Returns 1 if group is the first group in the contrast, 2 if it's the 2nd, otherwise 0
.findGroup <- function(group, contrast) {
  if (.group1(group, contrast)) {
    1
  } else if (.group2(group, contrast)) {
    2
  } else {
    0
  }
}

# Takes contrasts as a string, vector of strings, or matrix and returns a matrix
expandContrasts <- function(contrasts, groups) {

  if (is.null(contrasts))
    return(NULL)

  # This implementation is complicated because we allow any characters in group
  # names, although we will assume they do not start or end in whitespace

  # Need at least 2 groups for a comparison
  if (length(groups) < 2)
    return(NULL)

  # Special value "*" means all possible pairs
  if (is.character(contrasts) && length(contrasts) == 1 && trimws(contrasts) == "*") {
    contrasts <- utils::combn(rev(groups), 2)
  }

  # Handle special value, ". - groupX" or "groupX - .", where "." means all groups except groupX
  if (is.character(contrasts) && length(contrasts) == 1) {
    dotFirst <- .group1(".", contrasts)
    dotSecond <- .group2(".", contrasts)
    # Groups might be a factor, so convert to character
    groups <- unname(as.character(groups))

    if (dotFirst || dotSecond) {
      # Found a dot, is there a group as well?
      gotControl <- sapply(unname(groups), function(group) if (dotFirst) .group2(group, contrasts) else .group1(group, contrasts))
      if (sum(gotControl) == 1) {
        # Now we have a dot and a group
        controlGroup <- groups[gotControl]
        controlIdx <- which(gotControl)
        # Create a matrix (all except control - control)
        contrasts <- rbind(groups[-controlIdx], rep(controlGroup, length(groups) - 1))

        # Check if we need to swap control and groups
        if (dotSecond) {
          contrasts <- rbind(contrasts[2, ], contrasts[1, ])
        }
      }
    }
  }

  # Now handle general string representations
  if (is.character(contrasts) && !is.matrix(contrasts)) {

    # Split on commas, but only if there is a comma
    if (length(contrasts) == 1 && grepl(",", contrasts, fixed = TRUE)) {
      contrasts <- strsplit(contrasts, ",")[[1]]
    }

    contrastNames <- names(contrasts)

    # Assume syntax "group - group"
    contrasts <- sapply(contrasts, function(contrast) {
      # For this contrast, which should look like "group1 - group2",
      # find the location of group names within the string
      found <- sapply(unname(groups), function(group) .findGroup(group, contrast))
      if (sum(found > 0) != 2)
        stop(sprintf("Invalid contrast '%s'; must be 'group1 - group2, groups are %s",
                     contrast, paste0(groups, collapse = ", ")))
      found <- found[found != 0]
      names(found)[order(found)]
    }, USE.NAMES = FALSE)

    colnames(contrasts) <- contrastNames
  }

  # Convert from factor matrix to character matrix
  if (is.factor(contrasts))
    contrasts <- structure(as.character(contrasts), dim = dim(contrasts))

  contrasts
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


# Converts a contrasts argument to a list of DurgaGroupDiff objects.
#
# @return List of DurgaDiff objects. The list will be empty if there is only one
#   group or there are no contrasts to be displayed for some other reason
# @param defaultToAll If TRUE and constrasts were unspecified, returns the list
#   of all calculated group differences, otherwise returns all - the first group
plotDiffsFromContrasts <- function(contrasts, contrastsMissing, es, fnName, defaultToAll) {
  plotDiffs <- list()
  if (length(es$groups) > 1) {
    if (contrastsMissing) {
      # If contrasts were specified to DurgaDiff, use them
      if (es$explicit.contrasts || defaultToAll)
        plotDiffs <- es$group.differences
      else
        # Contrasts were never specified, default to all minus the first group
        plotDiffs <- buildPlotDiffs(paste(".-", es$groups[1]), es)
    } else if (is.character(contrasts)) {
      # Interpret string description
      plotDiffs <- buildPlotDiffs(contrasts, es)
    } else if (is.list(contrasts) && all(sapply(contrasts, function(x) methods::is(x, "DurgaGroupDiff")))) {
      # Contrasts were passed directly
      plotDiffs <- contrasts
    } else if (!is.null(contrasts)) {
      stop(sprintf("Invalid contrasts argument in %s call, must be character string or list of DurgaGroupDiff objects", fnName))
    }
  }
  plotDiffs
}
