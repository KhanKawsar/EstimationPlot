#<img src="man/figures/logo.png" align="right" height="120" />

# Durga: An R package for effect size estimation and visualisation

<!-- badges: start -->
[![R-CMD-check](https://github.com/KhanKawsar/EstimationPlot/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KhanKawsar/EstimationPlot/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/KhanKawsar/EstimationPlot/branch/main/graph/badge.svg)](https://app.codecov.io/gh/KhanKawsar/EstimationPlot?branch=main)
[![CRAN status](https://www.r-pkg.org/badges/version/Durga)](https://CRAN.R-project.org/package=Durga)
<!-- badges: end -->


Effect size estimation and plotting is a component of [estimation statistics](https://en.wikipedia.org/wiki/Estimation_statistics). `Durga` is an R package that aims to simplify sophisticated plotting of estimated differences in group means, which is an important part of communicating estimation statistics.

## Citation

If you use `Durga` in your research, please cite (and read) the article:

Khan, M.K. & McLean, D.J. (2023) Durga: An R package for effect size estimation and visualisation. _bioRxiv_, 2023.2002.2006.526960. https://doi.org/10.1101/2023.02.06.526960


## Installation

To install the released version:

    > install.packages("Durga")

To install the development version:

    > install.packages("devtools") # If not already installed
    > devtools::install_github("KhanKawsar/EstimationPlot", build_vignettes = TRUE)

Refer to [NEWS.md](NEWS.md) for a list of changes in each version, including changes in the current development version.

## Usage
 
Read the paper [Khan & McLean (2023)](https://doi.org/10.1101/2023.02.06.526960) for a detailed description of the package. Once Durga is installed, you can read the vignette named `Introduction to Durga` for a shorter introduction to using Durga, complete with examples and code.

```R
# Display the vignette in a browser
RShowDoc("Durga-intro", package = "Durga")

# Display the vignette in the help window pane
vignette("Durga-intro", package = "Durga")
```

A minimal example using constructed data:

```R
  n <- 100
  df <- data.frame(val = c(rnorm(n), rnorm(n, mean = 1)),
                   group = c(rep("Control", n), rep("Group", n)))

  d <- DurgaDiff(df, data.col = "val", group.col = "group")
  # or equivalently
  d <- DurgaDiff(val ~ group, df)
  DurgaPlot(d)
```

<img src="https://github.com/KhanKawsar/EstimationPlot/blob/main/eg.png?raw=true"></img>

<!-- To create the above PNG, run the example above, then run:
JPlotToPNG("eg.png", { par(mar = c(5, 4, 1, 1) + 0.1); DurgaPlot(d)}, width = 600)
-->


## Development Environment
<!-- Output from devtools::session_info() -->

```
─ Packages ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
 package      * version date (UTC) lib source
 boot           1.3-28  2021-05-03 [2] CRAN (R 4.2.0)
 brio           1.1.3   2021-11-30 [1] CRAN (R 4.2.0)
 cachem         1.0.6   2021-08-19 [1] CRAN (R 4.2.0)
 callr          3.7.0   2021-04-20 [1] CRAN (R 4.2.0)
 cli            3.3.0   2022-04-25 [1] CRAN (R 4.2.0)
 crayon         1.5.1   2022-03-26 [1] CRAN (R 4.2.0)
 data.table     1.14.2  2021-09-27 [1] CRAN (R 4.2.0)
 desc           1.4.1   2022-03-06 [1] CRAN (R 4.2.0)
 devtools       2.4.3   2021-11-30 [1] CRAN (R 4.2.0)
 digest         0.6.29  2021-12-01 [1] CRAN (R 4.2.0)
 Durga          0.1.0   2023-01-26 [1] Github (KhanKawsar/EstimationPlot@ec2aa00)
 ellipsis       0.3.2   2021-04-29 [1] CRAN (R 4.2.0)
 evaluate       0.15    2022-02-18 [1] CRAN (R 4.2.0)
 fansi          1.0.3   2022-03-24 [1] CRAN (R 4.2.0)
 fastmap        1.1.0   2021-01-25 [1] CRAN (R 4.2.0)
 fs             1.5.2   2021-12-08 [1] CRAN (R 4.2.0)
 glue           1.6.2   2022-02-24 [1] CRAN (R 4.2.0)
 htmltools      0.5.2   2021-08-25 [1] CRAN (R 4.2.0)
 knitr          1.39    2022-04-26 [1] CRAN (R 4.2.0)
 lifecycle      1.0.1   2021-09-24 [1] CRAN (R 4.2.0)
 magrittr       2.0.3   2022-03-30 [1] CRAN (R 4.2.0)
 memoise        2.0.1   2021-11-26 [1] CRAN (R 4.2.0)
 pillar         1.7.0   2022-02-01 [1] CRAN (R 4.2.0)
 pkgbuild       1.3.1   2021-12-20 [1] CRAN (R 4.2.0)
 pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.2.0)
 pkgload        1.2.4   2021-11-30 [1] CRAN (R 4.2.0)
 prettyunits    1.1.1   2020-01-24 [1] CRAN (R 4.2.0)
 processx       3.5.3   2022-03-25 [1] CRAN (R 4.2.0)
 ps             1.7.0   2022-04-23 [1] CRAN (R 4.2.0)
 purrr          0.3.4   2020-04-17 [1] CRAN (R 4.2.0)
 R6             2.5.1   2021-08-19 [1] CRAN (R 4.2.0)
 RColorBrewer   1.1-3   2022-04-03 [1] CRAN (R 4.2.0)
 remotes        2.4.2   2021-11-30 [1] CRAN (R 4.2.0)
 rlang          1.0.2   2022-03-04 [1] CRAN (R 4.2.0)
 rmarkdown      2.14    2022-04-25 [1] CRAN (R 4.2.0)
 rprojroot      2.0.3   2022-04-02 [1] CRAN (R 4.2.0)
 sessioninfo    1.2.2   2021-12-06 [1] CRAN (R 4.2.0)
 testthat       3.1.4   2022-04-26 [1] CRAN (R 4.2.0)
 tibble         3.1.7   2022-05-03 [1] CRAN (R 4.2.0)
 usethis        2.1.6   2022-05-25 [1] CRAN (R 4.2.2)
 utf8           1.2.2   2021-07-24 [1] CRAN (R 4.2.0)
 vctrs          0.4.1   2022-04-13 [1] CRAN (R 4.2.0)
 vipor          0.4.5   2017-03-22 [1] CRAN (R 4.2.0)
 withr          2.5.0   2022-03-03 [1] CRAN (R 4.2.0)
 xfun           0.31    2022-05-10 [1] CRAN (R 4.2.0)
 yaml           2.3.5   2022-02-21 [1] CRAN (R 4.2.0)

─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

```
