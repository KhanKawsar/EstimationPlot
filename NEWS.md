# Changes to the `Durga` package


## Durga 2.0.0.9000

### Enhancements

* `DurgaPlot` takes a parameter `ef.size.cex` that controls the size of the effect size central tendency symbol.
* `DurgaPlot` takes a parameter `ef.size.ylim` that sets y-axis limits of the effect size plot. Can only be specified if the effect size plot is below the main plot.

### Bug fixes

* `DurgaBrackets` prevents bracket text from overlapping.
* `DurgaDiff` now calculates CI of group mean for groups with at least 3 values and 2 distinct values. Previously it was only calculated for at least 3 distinct values.
* `DurgaDiff` now correctly handles `R = NA` as documented; does not calculate CIs

## Durga 2.0

### Enhancements

* `DurgaDiff` can now calculate multiple different effect sizes. The old values for `effect.type` of `"cohens"` and `"hedges"` are no longer accepted and generate an error. This backwards-incompatible change has been made so that users are required to consider the effect type they wish to use. See the help file for `DurgaDiff` for more information.
* `DurgaDiff` now calculates CI of group means using BCa bootstrapping rather than using a parametric formula. This means that a group with fewer than 3 distinct values will not have a CI calculated (lower and upper limits will be `NA`).
* `DurgaDiff` now accepts a vector for `group.col`. A single group column is created by concatenating values from each specified group column.
* `DurgaDiff` now accepts data in either long or wide format. Added the data set `insulin.wide` as an example of wide format. 

### Bug fixes

* `DurgaPlot`: Ensure entirety of effect size is visible when on the right.
* `DurgaDiff`: bootstrap implementation of CI samples each group separately. The bug sometimes meant that the effect size axis disappeared in plots for groups with small sample sizes. The confidence intervals will be slightly different now.

## Durga 1.1.0

* Improvements to vignette and online help.
* Added argument `violin.params` to `DurgaPlot` to provide additional control over violin appearance.
* Added arguments `ef.size.lty` and `ef.size.lwd` to `DurgaPlot` to provide control over effect size error bar appearance. 
* Added argument `points.adjust` to `DurgaPlot` to allow control over point layout.
* Bug fix in `DurgaPlot`: `violin.fill = FALSE` now correctly does not fill violins, rather than white fill.
* Bug fix in `DurgaBrackets`; drawing many brackets sometimes resulted in overlapping brackets.
* Extend tick marks on right axis to cover CI when plotting standardised effect size on the right.
* Fixed bug in plotting violins for pathological data; previously failed with error "Error in xy.coords(x, y, setLab = FALSE) : 'x' and 'y' lengths differ"

## Durga 1.0

* First public release
