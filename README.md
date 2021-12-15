[![Travis build status](https://travis-ci.org/nspyrison/spinifex.svg?branch=master)](https://travis-ci.org/nspyrison/spinifex) [![CRAN Status Badge](http://www.r-pkg.org/badges/version/spinifex)](https://cran.r-project.org/package=spinifex) [![Codecov test coverage](https://codecov.io/gh/nspyrison/spinifex/branch/master/graph/badge.svg)](https://codecov.io/gh/nspyrison/spinifex?branch=master) [![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing) [![CRAN Downloads Each Month](http://cranlogs.r-pkg.org/badges/spinifex)](https://cran.r-project.org/package=spinifex)

# spinifex <img src="man/figures/spinifex_hex.png" alt="spinifex R package!" align="right"/>

## About tours

Data visualization *tour* is a class of linear projections that are animate small changes in a projection basis over time. Shadows are an example of a 3D object casting a 2D linear projection. Imagine a bar stool cast a circular shadow of its seat. Watching the shadow alone, a rotation of the objection would quickly give structural information about the object; when the legs are rotated into frame you can be more certain the object is a bar stool. In the same way, a tour watches a lower-dimensional projection of the data object. The object permanence of the observations between frames holds structural information that is revealed as the object is rotated.


<!---
This extends the number of dimensions that we can view in data-space! This is applicable to many visualizations across all disciplines. We know that going to numerical summarization alone is dangerous. We need look no further than Anscomb's quartet.


<img src="https://upload.wikimedia.org/wikipedia/commons/e/ec/Anscombe%27s_quartet_3.svg" alt="Anscomb's quartet, Wikipedia" style="width:70%;height:70%;">
-->

## Scope

Such visualizations are applicable to numeric matrices which could be approximations of component spaces or feature decomposition of non-tabular data. Continuous quantitative variables are preferred, although ordinal discrete variables can also be used. Mapping color and/or shape to a (supervised) class tends to be helpful.

## Description

The `tourr` R package implements grand tours (constrained random walks in the basis), projection pursuit (basis anneals to some objective function), and several other variants and options of visualization tours.

__Grand tour__

![](https://github.com/nspyrison/spinifex/blob/main/buildignore/readme_img/penguins_gt.gif?raw=true)


The work and contribution of `spinifex` are primarily two-fold. The addition of manual tours which allow for user defined (and interactive!) control of the basis. Secondly, a layered API to create tour elements that mirrors the additive creation of `ggplot2` visuals. The extension to `gganimate` and `plotly` graphics packages, which allow for more control over display and exporting .gif, .mp4, and .html widgets of tours. The later can also consume tours produced in `tourr`!

We see that bill length is important for separating the orange and green species. Let's change its contribution with a manual tour:

__Manual tour__
![](https://github.com/nspyrison/spinifex/blob/main/buildignore/readme_img/penguins_mt.gif?raw=true)

## Installation

``` r
install.packages("spinifex") ## Download from CRAN
#remotes::install_github("nspyrison/spinifex") ## Dev version
```

## Getting started
```
library("spinifiex")                      ## Load into session
run_app("radial_tour")                    ## Run a local shiny app, demonstrating radial manual tours
vignette("getting started with spinifex") ## Introduction to tours and spinifex
vignette("ggproto api")                   ## Introduction to the new ggproto api
?ggtour                                   ## Examples and links to ggproto functions
help(package = "spinifex")                ## Review the package contents and documentation
```
## Reporting issues

Please submit bug reports, errors, and feature requests to <https://github.com/nspyrison/spinifex/issues>
