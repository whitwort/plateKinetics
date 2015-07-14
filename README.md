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

Or if you'd rather jump right in, see the help page for the main entry point function `loadExperiment`.

## License

Copyright © 2015 Gregg Whitworth and licensed under [GPLv2](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html).