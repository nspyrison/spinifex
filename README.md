[![Travis build status](https://travis-ci.org/nspyrison/spinifex.svg?branch=master)](https://travis-ci.org/nspyrison/spinifex)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/spinifex)](https://cran.r-project.org/package=spinifex)
[![Codecov test coverage](https://codecov.io/gh/nspyrison/spinifex/branch/master/graph/badge.svg)](https://codecov.io/gh/nspyrison/spinifex?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![CRAN Downloads Each Month](http://cranlogs.r-pkg.org/badges/spinifex)](https://cran.r-project.org/package=spinifex)


# spinifex <img src="man/figures/spinifex_hex.png" align="right" alt="spinifex R package!"/>

## About tours

Data visualization tours are a class of linear projections that are animate small rotations in the basis over time. This is akin to a shadow puppet, or a 3D object being slowly rotated as the 2D shadow is being watched. In the same way a tour watches a 2D scatterplot at the `p`-dimensional data object is being rotated.

This extends the number of dimensions that we can view in data-space! This is applicable to many visualizations across all disciplines. We know that going to numerical summarization alone is dangerous. We need look no further than Anscomb's quartet.

<img src="https://upload.wikimedia.org/wikipedia/commons/e/ec/Anscombe%27s_quartet_3.svg" alt="Anscomb's quartet, Wikipedia" style="width:70%;height:70%;">

## Scope

Such visualizations are applicable to numeric matrices scaling well into 10's of variables. If the variables are numerous PCA initialization may be useful to reduce the intrinsic dimensionality. Continuous quantitative variables are preferred, although ordinal discrete variables are also regularly used. Mapping color and/or shape to a (supervised) class tends to be helpful.

## Description

The `tourr` R package implements grand tours (constrained random walks in the basis), projection pursuit (basis anneals to some objective function), and several other variants and options of visualization tours.

The work and contribution of `spinifex` are primarily two-fold. The addition of manual tours which allow for user defined (and interactive!) control of the basis. Secondly, the extension to `gganimate` and `plotly` graphics packages, which allow for more control over display and exploring .gif and .html widgets of tours. The later can also consume tours produced in `tourr`!

## Installation

Latest CRAN version:
```
install.packages("spinifex")
```

Latest development version:
```
remotes::install_github("nspyrison/spinifex")
```

## Getting started

```
library("spinifiex") ## Load into session
vignette("spinifex") ## View the example code vignette
run_app("intro") ## Run a local shiny app, demonstrating radial manual tours
help(package = "spinifex") ## Review the package contents and documentation
```

## Reporting issues

Please submit bug reports, errors, and feature requests to https://github.com/nspyrison/spinifex/issues

