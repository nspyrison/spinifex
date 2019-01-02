#' spinifex
#'
#' spinifex is a package that allows for manual tours. Touring is a linear 
#' dimensionality reducetion method that projects objects from `p` down to `d` 
#' dimensions. Manual tours manipulate one selected variable by a given amount.
#' spinifex can also consume tour paths generated in the tourr package and
#' offers streamlined production of plotly graphics to animate the tour.
#'
#' It's main functions are:
#' \itemize{
#'   \item [manual_tour()]
#'   \item [create_slides()]
#'   \item [render_slideshow()]
#'   \item [spinifex()], A wrapper function for the above.
#'   \item [play_tour()], A wrapper function which tourr tour paths can be used.
#' }
#'
#' GitHub repo: \url{https://github.com/nspyrison/spinifex}
#' @seealso tourr (package)
#' @name spinifex
#' @docType package
"_PACKAGE"

globalVariables(c("cat_var",
                  "manip_type",
                  "theta",
                  "phi_min",
                  "phi_max",
                  "n_slides",
                  "disp_type",
                  "manip_col"
))