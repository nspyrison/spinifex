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
#'   \item [play_manual_tour()], performs a manual tour, returning a `plotly` animate by default.
#'   \item [play_tour_path()], turns a tour path into an animatation, returning a `plotly` object by default.
#'   \item [view_basis()], plot a basis set on a reference axis.
#'   \item [view_manip_space()], plot a manipualation space highlighting the manip var.
#' }
#'
#' GitHub repo: \url{https://github.com/nspyrison/spinifex}
#' @seealso tourr (package)
#' @name spinifex
#' @docType package
"_PACKAGE"

# Manual tour globals:
globalVariables(c("col",
                  "pch",
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
