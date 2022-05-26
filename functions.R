
#' Generate differences
#' 
#' This is details
#' 
#' @param data Describe data
#' @param type jfgjkdf
#' 
#' @return List containing:
#' bootstrapped mean difference + confidence interval of mean difference + individual bootstrapped difference + 
#'         CI of group 1 + CI of group 2 + mean of group 1 + mean of group 2 + median of group 1 + median of group 2 +
#'         raw data
difference <- function(data,
                       paired = FALSE, # if true calculate paired mean difference 
                       data.col = 1, group.col = 2, block.col = NULL,
                       ...        
                       # ci.type = "bca", #default
                       # effect.type = c("unstandardised", "cohens", "hegens"), # select type of difference #default unstandardised
                       # R = 2000, #default
                       # contrast = c("group 1 - group 2"), # default larger group minus small group
                       # ci.conf = 0.95,
) {
  
  es <- bootES::bootES(data, data.col = data.col, group.col = group.col, block.col = block.col, ..., plot = FALSE)
  
  # Return value has type plotES
  class(es) <- c("plotES", class(es))
  
  # Fill in extra information in the returned list  
  for (gn in unique(data[[group.col]])) {
    grpVals <- data[[data.col]][data[[group.col]] == gn]
    es[[sprintf("mean.%s", gn)]] <- mean(grpVals)
    es[[sprintf("median.%s", gn)]] <- median(grpVals)

    # boot <- bootES::bootES( whatever)  
    # es[[sprintf("CIl.%s", gn)]] <- CI lower
    # es[[sprintf("CIu.%s", gn)]] <- CI upper
  }
  
  es
}

## print bootstrap mean difference, bootstrapped confidence interval (R value, bootstrapped corrections"bca")
print.plotES <- function(result, ...) {
  # For now, just call the bootES method
  NextMethod(result, ...)
}



plotES <- function(data, 
                   box = TRUE,# draw boxplot 
                   box_fill = TRUE,# fill up box colour # if false only border will be drawn 
                   points = TRUE, #add individual data point 
                   violin = c("left-half", "right-half", "full", "none"), # add half violin
                   mean = TRUE, #draw mean of the data 
                   CI = TRUE, # draw confidence interval line of the data; if, box, density, violin is TRUE, CI is FALSE
                   median_line = FALSE, # if TRUE horizontal line in median is drawn # if, box, density, violin is TRUE, median_line is FALSE
                   ef_size =TRUE, # if false do not plot effect size
                   ef_size_density =TRUE, #if true draw effect size confidence interval
                   ef_size_position = c( "right", "down"),# when Gardner-Altman_plot is choosen effect size plotted right, otherwise down 
                   paired = FALSE, # if true draw lines between paired points
                   xlab = "",
                   left_ylab = "",
                   right_ylab = "",
                   bottom_ylab = "",
                   col = c("col1", "col2", "col3"), opacity = 0.6, #colour of box, violin, box border, density, col 1 = group 1, col2 = group 2, col3 = ef plot, {col = n+1, n = group no) #   
                   points_col = c("col1", "col2", "col3"), points_opacity = 0.4 # points colour 
) {
  
}

###
# Testing

N <- 40
data <- data.frame(Measurement = c(rnorm(N, mean = 100, sd = 25),
                                   rnorm(N, mean = 100, sd = 50),
                                   rnorm(N, mean = 120, sd = 25),
                                   rnorm(N, mean = 80, sd = 50),
                                   rnorm(N, mean = 100, sd = 12),
                                   rnorm(N, mean = 100, sd = 50)),
                   Group = c(rep("Control1", N),
                             rep("Control2", N),
                             rep("Group1", N),
                             rep("Group2", N),
                             rep("Group3", N),
                             rep("Group4", N)),
                   Gender = rep(c(rep('Male', N/2), rep('Female', N/2)), 6),
                   ID = rep(1:N, 6)
                   )

es <- difference(data[data$Group %in% c("Group1", "Group2"),], data.col = "Measurement", group.col = "Group", R = 1000)
print(es)
plotES(es)
