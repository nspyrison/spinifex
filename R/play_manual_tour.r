#' Animate a manual tour
#'
#' Performs the a manual tour and returns an animation of `render_type`.
#' For use with `tourr::save_history()` tour paths see `play_tour_path()`. 
#' 
#' @name play_manual_tour
#' @param data (n, p) dataset to project, consisting of numeric variables.
#' @param basis A (p, d) dim orthonormal numeric matrix. 
#'   If it's left null, random basis will be used.
#' @param render_type Which graphics to render to. Defaults to render_plotly, 
#' @param rescale_data When TRUE scales the data to between 0 and 1.
#' @param ... Optionally pass additional arguments to the `render_type` for 
#'   plotting options.
### #' @param angle target distance (in radians) between bases.
### #' @param manip_var Number of the column/dimension to rotate.
### #' @param theta Angle in radians of "in-plane" rotation, on the XY plane of the 
### #'   reference frame.
### #'   If left NULL, will initialize the radial angle of the `manip_var`.`
### #' @param phi_min Minimum value phi should move to. Phi is angle in radians of 
### #'   the "out-of-plane" rotation, the z-axis of the reference frame. 
### #'   Required, defaults to 0.
### #' @param phi_max Maximum value phi should move to. Phi is angle in radians of 
### #'   the "out-of-plane" rotation, the z-axis of the reference frame. 
### #'   Required, defaults to pi / 2.
### #' @param manip_col String of the color to highlight the `manip_var`.
### #'   alternative use render_gganimate.
### #' @param col Color of the projected points. Defaults to "black".
### #' @param pch Point character of the projected points. Defaults to 20.
### #' @param axes Position of the axes: "center", "bottomleft" or "off". Defaults 
### #'   to "center".
### #' @param fps Frames/slides shown per second. Defaults to 3.
### #'   Defaults to FALSE.
### #' @param alpha Opacity of the data points between 0 and 1. Defaults to 1.
#' @return An animation of a radial tour.
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
play_manual_tour <- function(basis = NULL,
                             data, 
                             render_type = render_plotly,
                             rescale_data = FALSE,
                             ...) {
  if (rescale_data) data <- tourr::rescale(data)
  if (is.null(basis)) {
    message("NULL basis passed. Initializing random basis.")
    basis <- tourr::basis_random(n = ncol(data))
  }
  
  tour_hist <- manual_tour(basis = basis, ...)
  tour_df <- array2df(array = tour_hist, data = data)
  anim <- render_type(slides = tour_df, ...)
  
  return(anim)
}
