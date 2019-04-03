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

#' Render display of a manual tour
#'
#' Performs the sepicify manual tour and returns an animation of `render_type`.
#' For use with `tourr::save_history()` tour paths see `play_tour_path()`. 
#' A wrapper function for `manual_tour()`, `array2df()`, and `render_()`.
#' 
#' @param data (n, p) dataset to project, consisting of numeric variables.
#' @param basis A (p, d) dim orthonormal numeric matrix. 
#'   If it's left null, random basis will be used.
#' @param angle target distance (in radians) between bases.
#' @param manip_var Number of the column/dimension to rotate.
#' @param theta Angle in radians of "in-plane" rotation, on the XY plane of the 
#'   reference frame.
#'   If left NULL, will initialize the radial angle of the `manip_var`.`
#' @param phi_min Minimum value phi should move to. Phi is angle in radians of 
#'   the "out-of-plane" rotation, the z-axis of the reference frame. 
#'   Required, defaults to 0.
#' @param phi_max Maximum value phi should move to. Phi is angle in radians of 
#'   the "out-of-plane" rotation, the z-axis of the reference frame. 
#'   Required, defaults to pi / 2.
#' @param manip_col String of the color to highlight the `manip_var`.
#' @param render_type Which graphics to render to. Defaults to render_plotly, 
#'   alternative use render_gganimate.
#' @param col Color of the projected points. Defaults to "black".
#' @param pch Point character of the projected points. Defaults to 20.
#' @param axes Position of the axes: "center", "bottomleft" or "off". Defaults 
#'   to "center".
#' @param fps Frames/slides shown per second. Defaults to 3.
#' @param init_rescale_data When TRUE will apply `tourr::rescale()` on the data.
#'   Defaults to FALSE.
#' @param ... Optionally pass additional arguments to the `render_type` for 
#'   plotting options.
#' @return An animation of a manual tour.
#' @import tourr
#' @export
#' @examples
#' \dontrun{
#' flea_std <- tourr::rescale(tourr::flea[,1:6])
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' 
#' play_manual_tour(data = flea_std, basis = rb, manip_var = 4)
#' 
#' play_manual_tour(data = flea_std, basis = rb, manip_var = 6, 
#'   render_type = render_gganimate, col = col_of(flea$species), axes = "bottomleft")
#' }
play_manual_tour <- function(data,
                             basis       = NULL,
                             manip_var,
                             theta       = NULL,
                             phi_min     = 0,
                             phi_max     = .5 * pi,
                             angle       = .05,
                             manip_col   = "blue",
                             render_type = render_plotly,
                             col         = "black", 
                             pch         = 20,
                             axes        = "center",
                             fps         = 3,
                             init_rescale_data = FALSE,
                             ...) {
  if (init_rescale_data) data <- tourr::rescale(data)
  if (is.null(basis)) {
    message("NULL basis passed. Initializing random basis.")
    basis <- tourr::basis_random(n = ncol(data))
  }
  
  m_tour <- manual_tour(basis = basis, manip_var = manip_var, angle = angle,
                        theta = theta, phi_min = phi_min, phi_max = phi_max)
  
  slides <- array2df(array = m_tour, data = data)
  disp   <- render_type(slides = slides, manip_col = manip_col,
                        col = col, pch = pch, axes = axes, fps = fps, ...)
  
  disp
}
