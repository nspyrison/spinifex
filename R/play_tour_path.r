#' Render display of a provided tour path
#'
#' Takes the result of `tourr::save_history()` or `manual_tour()`, interpolates
#' over the path and renders into a selected `render_type`.
#'
#' @param tour_path The result of `tourr::save_history()` or `manual_tour()`.
#' @param data Optional, number of columns must match that of `tour_path`.
#' @param angle target distance (in radians) between bases.
#' @param render_type Which graphics to render to. Defaults to render_plotly, 
#'   alternative use render_gganimate.
#' @param col Color of the projected points. Defaults to "black".
#' @param pch Point character of the projected points. Defaults to 20.
#' @param axes Position of the axes: "center", "bottomleft" or "off". Defaults 
#'   to "center".
#' @param fps Frames/slides shown per second. Defaults to 3.
#' @param ... Optionally pass additional arguments to `render_type`.
#' @import tourr
#' @export
#' @examples
#' \dontrun{
#' flea_std <- rescale(tourr::flea[,1:6])
#' tpath    <- save_history(flea_std, max = 3)
#' 
#' play_tour_path(tour_path = tpath, data = flea_std, angle = .15)
#' 
#' play_tour_path(tour_path = tpath, data = flea_std, angle = .15, fps = 4,
#'   render_type = render_gganimate, col = col_of(flea$species), axes = "bottomleft")
#' }

play_tour_path <- function(tour_path,
                           data = NULL,
                           angle = .05,
                           render_type = render_plotly,
                           col = "black", 
                           pch = 20,
                           axes = "center",
                           fps = 3,
                           ...) {
  # if data missing, but an attribute, use that.
  if(is.null(data) & !is.null(attributes(tour_path)$data)){ 
    message("data passed as NULL with a tourr object containing attached data; rendering the tour_path data.")
    data <- attributes(tour_path)$data
  }
  
  tour_path <- tourr::interpolate(basis_set = tour_path, angle = angle)
  attr(tour_path, "class") <- "array"
  slides    <- array2df(array = tour_path, data = data)
  disp      <- render_type(slides = slides, manip_col = manip_col, 
                           col = col, pch = pch, axes = axes, fps = fps, ...)
  
  disp
}

