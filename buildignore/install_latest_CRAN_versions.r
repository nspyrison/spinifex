##### USE CLEAN INSTALL:
### Get the package list from DESCRIPTION and GRAB MOST RECENT:

pkgs <- c(
  ##Imports:
  "ggplot2",
  "gganimate",
  "GGally",
  "plotly",
  "shiny",
  "RColorBrewer",
  "gifski",
  "png",
  "dplyr",
  "gridExtra",
  "shinythemes",
  "shinyBS",
  "shinyjs",
  "reactlog",
  "DT",
  ##Suggests: 
  "knitr",
  "rmarkdown",
  "testthat",
  "covr"
)

### grab most recent CRAN ver.
install.packages(pkgs)
