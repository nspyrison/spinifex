#' spinifex
#'
#' `spinifex` is a package that extends the package `tourr`. 
#' It builds the functionality for manual tours and allows other
#' tours to be rendered by `plotly` or `gganimate`. Tours are a class of
#' dynamic linear (orthogonal) projections of numeric multivariate data from 
#' `p` down to `d` dimensions that are viewed as an animation as `p`-space is 
#' rotated. Manual tours manipulate a selected variable, exploring how they 
#' contribute to the sensitivity of the structure in the projection. This is
#' particularly useful after finding an interesting basis, perhaps via a 
#' guided tour optimizing the projection for some objective function.
#' 
#' GitHub: \url{https://github.com/nspyrison/spinifex}
#' @name spinifex
#' @docType package
"_PACKAGE"

.onAttach <- function(...){
  ## Print on Screen
  packageStartupMessage("--------------------------------------------------------")
  packageStartupMessage("spinifex --- version ", utils::packageVersion("spinifex"))
  packageStartupMessage("Please share bugs, suggestions, and feature requests at:")
  packageStartupMessage("https://github.com/nspyrison/spinifex/issues/")
  packageStartupMessage("--------------------------------------------------------")
}

## Import pipe
#' @importFrom magrittr %>%

## Manual tour globals:
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("phi_min",
                           "phi_max",
                           "manip_col",
                           "n_frames",
                           "theta",
                           "angle"))
  ## ggproto globals:
  utils::globalVariables(c(".spinifex_df_basis",
                           ".spinifex_df_data",
                           ".spinifex_map_to",
                           ".df_data",
                           ".df_basis",
                           ".map_to",
                           ".n_frames",
                           ".nrow_df_data",
                           ".n",
                           ".p",
                           ".manip_var"))
  ## ggplot aes globals:
  utils::globalVariables(c("x",
                           "y",
                           "z",
                           "xend",
                           "yend",
                           "x_end",
                           "y_end",
                           "label",
                           "frame",   ## Animation frame
                           "rownum")) ## plotly tooltip
}
