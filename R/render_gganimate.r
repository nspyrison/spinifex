#' Render the slides as a *gganimate* animation
#'
#' Takes the result of `array2df()` and renders them into a 
#' *gganimate* animation.
#'

#' @param fps Frames/slides shown per second. Defaults to 3.
#' @param ... Optional, pass addition arguments to 
#'   `gganimate::transition_states()`.
#' @export
#' @examples
#' \dontrun{
#' flea_std <- tourr::rescale(tourr::flea[, 1:6])
#' 
#' rb <- basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
#' sshow <- array2df(array = mtour, data = flea_std)
#' render_gganimate(slides = sshow)
#' 
#' render_gganimate(slides = sshow, col = flea$species, pch = flea$species,
#'   axes = "bottomleft", fps = 2, alpha = .6)
#' }
render_gganimate <- function(fps = 3,
                             ...) {
  requireNamespace("gganimate")
  ## TODO: SET OPTIONS WITH THE FOLLOWING. During the animation function call.
  # # Change duration and framerate
  # animate(anim, fps = 20, duration = 15)
  # 
  # # Make the animation pause at the end and then rewind
  # animate(anim, nframes = 100, end_pause = 10, rewind = TRUE)
  #  
  # # Use a different renderer
  # animate(anim, renderer = file_renderer('~/animation/'))[1:6]

  
  gg <- render_(...) + 
    ggplot2::coord_fixed()
  
  gga <- gg + 
    gganimate::transition_states(slide, 
                                 transition_length = 0, 
                                 state_length = 1 / fps)
  
  gga
}

