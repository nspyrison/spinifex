[![Travis build status](https://travis-ci.org/nspyrison/spinifex.svg?branch=master)](https://travis-ci.org/nspyrison/spinifex)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/spinifex)](https://cran.r-project.org/package=spinifex)
<!-- [![Codecov test coverage](https://codecov.io/gh/nspyrison/spinifex/branch/master/graph/badge.svg)](https://codecov.io/gh/nspyrison/spinifex?branch=master) -->
<!-- [![CRAN Downloads Each Month](http://cranlogs.r-pkg.org/badges/spinifex)](http://cran.rstudio.com/web/packages/spinifex/index.html)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing) -->


# spinifex <img src="man/figures/spinifex_hex.png" align="right" />

## Scope

Tours are a class of dynamic linear projection techniques that extend the viewing (of data-space) for numeric multivariate data as the dimensionality increases. 

## Description

Function that generate the path for manual tours. Tours are generally available in the tourr package. The grand tour is an algorithm that shows all possible projections given sufficient time. Guided uses projection pursuit to steer the tour towards interesting projections. The spinifex package implements manual control, where the contribution of a selected variable can be adjusted between -1 to 1, to examine the sensitivity of structure in the data to that variable. The result is an animation where the variable is toured into and out of the projection completely, which can be rendered using the 'gganimate' and 'plotly' packages.

## Installation

You can install spinifex from CRAN:

```
install.packages("spinifex")
```

Or you can install the latest development version on github via `remotes`:

```
remotes::install_github("nspyrison/spinifex")
```

View the vignette to get started!
```
library("spinifiex")
vignette("spinifex")
```

## reporting and issues

Please submit all bug reports, errors, and feature requests to https://github.com/nspyrison/spinifex/issues  

