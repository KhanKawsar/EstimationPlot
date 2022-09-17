<!-- badges: start -->
[![R-CMD-check](https://github.com/KhanKawsar/EstimationPlot/workflows/R-CMD-check/badge.svg)](https://github.com/KhanKawsar/EstimationPlot/actions)
[![Codecov test coverage](https://codecov.io/gh/KhanKawsar/EstimationPlot/branch/main/graph/badge.svg)](https://app.codecov.io/gh/KhanKawsar/EstimationPlot?branch=main)
<!-- badges: end -->


# DurgaPlot: Handy package forEstimation Plotting in R

Effect size estimation and plotting is a component of [estimation statistics](https://en.wikipedia.org/wiki/Estimation_statistics). `DurgaPlot` is an R package that aims to simplify sophisticated plotting of estimated differences in group means; an important part of communicating estimation statistics.

## Citation

If you use `DurgaPlot` in your research, please cite (and read) the article:

`DurgaPlot`: An R package for estimating and plotting effect sizes 

## Installation

To install the development version (which is the only version available as yet):

    > install.packages("devtools") # If not already installed
    > devtools::install_github("KhanKawsar/EstimationPlot")

## Usage


```{R}
  n <- 100
  df <- data.frame(val = c(rnorm(n), rnorm(n, mean = 1)),
                   group = c(rep("Control", n), rep("Group", n)))

  d <- DurgaDiff(df, data.col = "val", group.col = "group")
  DurgaPlot(d)
```
