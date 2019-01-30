#' spinifex
#'
#' spinifex is a package that is compatiable and extends the package `tourr`. 
#' It builds the functionality for manual tours, and adds the ability for any 
#' tour to be rendered as `plotly` or `gganimate` objects. Touring is a class 
#' of dimensionality reducetion that orthagonally projects data from `p` down 
#' to `d` dimensions, take a small change in `p`-space and interpolates again. 
#' Manual tours manipulate a selected variable to explore the local structure,
#' particaularly effective after finding a interesting tour, perhaps via a 
#' guided tour.
#'
#' It's main functions are:
#' \itemize{
#'   \item [manual_tour()], Performs manual tour and returns a tour path array.
#'   \item [array2df()], Turns a tour path array into a (long) df for ggplot 
#'   graphics.
#'   \item [render_plotly()], Plots tour path df as a plotly object.
#'   \item [render_gganimate()],  Plots tour path df as a gganimate object.
#'   \item [play_manual_tour()], A wrapper function, applies the above for a 
#'   manual tour.
#'   \item [play_tour_path()], A wrapper function, renders a tour path array as 
#'   `render_type`.
#' }
#'
#' GitHub repo: \url{https://github.com/nspyrison/spinifex}
#' @seealso tourr (package)
#' @name spinifex
#' @docType package
"_PACKAGE"

# Manual tour globals:
globalVariables(c("cat_var",
                  "disp_type",
                  "phi_min",
                  "phi_max",
                  "manip_col",
                  "manip_type",
                  "n_slides",
                  "slide",
                  "theta")
)

# ggplot aes globals:
globalVariables(c("V1",
                  "V2",
                  "x",
                  "y",
                  "z",
                  "xend",
                  "yend",
                  "lab",
                  "lab_abbr",
                  "slide")
)
