#' Render the slides as a *gganimate* animation
#'
#' Takes the result of `array2df()` and renders them into a 
#' *gganimate* animation.
#'
#' @param slides The result of `array2df()`, a long df of the projected frames.
#' @param manip_col String of the color to highlight the `manip_var`. 
#'   Defaults to "blue".
#' @param col Color of the projected points. Defaults to "black".
#' @param pch Point character of the projected points. Defaults to 20.
#' @param axes Position of the axes: "center", "bottomleft" or "off". Defaults 
#'   to "center".
#' @param fps Frames/slides shown per second. Defaults to 3.
#' @param alpha Opacity of the data points between 0 and 1. Defaults to 1.
#' @param ... Optional, pass addition arguments to 
#'   `gganimate::transition_states()`.
#' @export
#' @examples
#' \dontrun{
#' flea_std <- tourr::rescale(tourr::flea[, 1:6])
#' 
#' rb <- basis_random(n = ncol(flea_std))
#' mtour <- radial_tour(basis = rb, manip_var = 4)
#' sshow <- array2df(array = mtour, data = flea_std)
#' render_gganimate(slides = sshow)
#' 
#' render_gganimate(slides = sshow, col = col_of(flea$species), 
#'   axes = "bottomleft", fps = 2)
#' }
render_gganimate <- function(slides,
                             manip_col = "blue",
                             col   = "black", 
                             pch   = 20,
                             axes  = "center",
                             fps   = 3,
                             alpha = 1,
                             ...) 
{
  # Initialize
  require("gganimate")
  
  gg <- render_(slides = slides, manip_col = manip_col, 
                col = col, pch = pch, axes = axes, alpha = alpha) + 
    ggplot2::coord_fixed()
  
  gga <- gg + gganimate::transition_states(
    slide, transition_length = 0, state_length = 1 / fps, ...)
  
  gga
}

