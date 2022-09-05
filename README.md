<!-- badges: start -->
[![test-coverage](https://github.com/KhanKawsar/EstimationPlot/workflows/test-coverage/badge.svg)](https://github.com/KhanKawsar/EstimationPlot/actions)
[![check-standard](https://github.com/KhanKawsar/EstimationPlot/workflows/check-standard/badge.svg)](https://github.com/KhanKawsar/EstimationPlot/actions)
[![Codecov test coverage](https://codecov.io/gh/KhanKawsar/EstimationPlot/branch/main/graph/badge.svg)](https://app.codecov.io/gh/KhanKawsar/EstimationPlot?branch=main)
<!-- badges: end -->


# SAKPlot: Swiss-army-knife of Estimation Plotting

Effect size estimation and plotting is a component of [estimation statistics](https://en.wikipedia.org/wiki/Estimation_statistics).

## Citation

If you use `SAKPlot` in your research, please cite (and read) the article:

`SAKPlot`: An R package for estimating and plotting effect sizes 

## Installation

To install the development version (which is the only version available as yet):

    > install.packages("devtools") # If not already installed
    > devtools::install_github("KhanKawsar/EstimationPlot")

## Usage


```{R}
  n <- 100
  df <- data.frame(val = c(rnorm(n), rnorm(n, mean = 1)),
                   group = c(rep("Control", n), rep("Group", n)))

  d <- SAKDifference(df, data.col = "val", group.col = "group")
  SAKPlot(d)
```
