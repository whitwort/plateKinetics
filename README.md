# plateKinetics

This package provides a set of tools for analyzing absorbance and fluorescence intensity data recorded over time in multiwell plate format. Tools are include to help with optimized doubling time calculations as well as data annotation and visualization.

## Installing

To install the latest development version of this package, including vignettes and guides:

```r
devtools::install_github('whitwort/plateKinetics', build_vignettes = TRUE)
```

Run `r install.packages("devtools")` first if you don't have it installed.

## Getting started

For a quick overview, you can open the [getting-started](vignettes/getting-started.Rmd) vignette after loading the package:

```r
library(plateKinetics)
vignette('getting-started')
```

## Versions

0.1 Initial release.  Includes doublingTime calculation and some basic visualization tools.

## License

Copyright © 2016 Gregg Whitworth and licensed under [GPLv2](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html).
