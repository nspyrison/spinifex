##### USE CLEAN INSTALL:
### Get the package list from DESCRIPTION and GRAB MOST RECENT:

pkgs <- c(
  ##Imports:
  "ggplot2",
  "gganimate",
  "plotly",
  "shiny",
  "Rdimtools",
  "magrittr",
  ##Suggests:
  "MASS",
  "ggrepel",
  "patchwork",
  "hexbin",
  "htmlwidgets",
  "gifski",
  "png",
  "dplyr",
  "testthat",
  "lifecycle",
  "covr",
  "spelling"
)

### grab most recent CRAN ver.
pak::pak(pkgs)
