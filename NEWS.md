# Changes to the `Durga` package

## Durga 1.0.0.9000 (development version)

* Improvements to vignette and online help.
* Added argument `violin.params` to `DurgaPlot` to provide additional control over violin appearance.
* Added arguments `ef.size.lty` and `ef.size.lwd` to `DurgaPlot` to provide control over effect size error bar appearance. 
* Bug fix in `DurgaPlot`: `violin.fill = FALSE` now correctly does not fill violins, rather than white fill.
* Bug fix in `DurgaBrackets`; drawing many brackets sometimes resulted in overlapping brackets.
* Extend tick marks on right axis to cover CI when plotting standardised effect size on the right.

## Durga 1.0

* First public release
