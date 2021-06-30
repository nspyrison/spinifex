#' spinifex
#'
#' spinifex is a package that extends the package `tourr`. 
#' It builds the functionality for manual tours and allows other
#' tours to be rendered by `plotly` or `gganimate`. Tours are a class of
#' dynamic linear (orthogonal) projections of numeric multivariate data from 
#' `p` down to `d` dimensions that are viewed as an animation as `p`-space is 
#' rotated. Manual tours manipulate a selected variable, exploring how they 
#' contribute to the sensitivity of the structure in the projection. This is
#' particularly useful after finding an interesting tour, perhaps via a 
#' guided tour.
#' 
#'
#' Its main functions are:
#' \itemize{
#'   \item [run_app()], running `run_app("radial_tour")` will open a `shiny` app demonstrating radial, manual tours.
#'   \item [play_manual_tour()], performs a manual tour, returning a `plotly` animate by default.
#'   \item [play_tour_path()], turns a tour path into an animation, returning a `plotly` object by default.
#'   \item [view_frame()], plot a basis set on a reference axis.
#'   \item [view_manip_space()], plot a manipulation space highlighting the manip var.
#' }
#'
#' GitHub: \url{https://github.com/nspyrison/spinifex}
#' @seealso tourr (package)
#' @name spinifex
#' @docType package
"_PACKAGE"

.onAttach <- function(...){
  ## Print on Screen
  packageStartupMessage("--------------------------------------------------------")
  packageStartupMessage("spinifex --- version ", packageVersion("spinifex"))
  packageStartupMessage("Please share bugs, suggestions, and feature requests at:")
  packageStartupMessage("https://github.com/nspyrison/spinifex/issues/")
  packageStartupMessage("--------------------------------------------------------")
}

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
