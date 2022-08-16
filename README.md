---
output: html_document
---

# SAKPlot: Swiss-army-knife of Estimation Plotting

## Installation

To install the development version (which is the only version available as yet):

    > install.packages("devtools") # If not already installed
    > devtools::install_github("KhanKawsar/EstimationPlot")

## Usage


```{R}
  n <- 100
  df <- data.frame(val = c(rnorm(n), rnorm(n, mean = 1)),
                   group = c(rep("Control", n), rep("Group", n)),
                   id = c(1:n, 1:n))

  # Check all effect types
  d <- difference(df, effect.type = "unstandardised")
  SAKPlot(d, bar = FALSE, box = FALSE)
```
