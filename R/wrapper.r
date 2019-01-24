#' Render display of a provided tour path
#'
#' Takes the result of `tourr::save_history()` or `manual_tour()`, interpolates
#' over the path and renders into a selected `render_type`.
#'
#' @param tour_path The result of `tourr::save_history()` or `manual_tour()`.
#' @param data Optional, number of columns must match that of `tour_path`.
#' @param angle target distance (in radians) between bases. Defaults to .05.
#' @param render_type Which graphics to render to. Defaults to render_plotly, 
#'   alternative use render_gganimate.
#' @param ... Optionally pass additional arguments to `render_type`.
#' @export
#' @examples
#' flea_std <- rescale(tourr::flea[,1:6])
#' tpath    <- save_history(flea_std, guided_tour(cmass))
#' 
#' play_tour_path(tour_path = tpath, data = flea_std)
#' 
#' play_tour_path(tpath, NULL, render_gganimate, flea$species, 4)
play_tour_path <- function(tour_path,
                           data = NULL,
                           angle = .05,
                           render_type = render_plotly, # alt render_gganimate.
                           cat_var = NULL,
                           fps = 3,
                           ...) {
  # if data missing, but in an attribute, use that.
  if(is.null(data) & !is.null(attributes(tour_path)$data)){ 
    message("data passed as NULL with a tourr object containing attached data; rendering the tour_path data.")
    data <- attributes(tour_path)$data
  }
  
  # if tour_path isn't a normal array, make it an array.
  if(class(tour_path) != "array") {
    attr(tour_path, "class") <- NULL
    tour_path <- as.array(tour_path)
  }
  
  #tourr::interpolate(tour_path, angle)
  ## Error in is_orthonormal(Fa) : is.matrix(x) is not TRUE
  
  tourdf <- array2df(tour_path, data)
  disp   <- render_type(tourdf, manip_col, cat_var, fps, ...)
  
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
#' @param n_slides The number of slide-interpolations to make for the tour.
#' @param manip_col String of the color to highlight the `manip_var`.
#' @param render_type Which graphics to render to. Defaults to render_plotly, 
#'   alternative use render_gganimate.
#' @param cat_var Categorical variable, optionally used to set the data point 
#'   color and shape.
#' @param fps Frames/slides shown per second. Defaults to 3.
#' @param init_rescale_data When TRUE will apply `tourr::rescale()` on the data.
#'   Defaults to FALSE.
#' @param ... Optionally pass additional arguments to the `render_type` for 
#'   plotting options.
#' @return An animation of a manual tour.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[,1:6])
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' 
#' play_manual_tour(data = flea_std, basis = rb, manip_var = 4)
#' 
#' play_manual_tour(tourr::flea[,1:6], NULL, 4, 0, 0, pi, 40, "red", 
#'                  render_gganimate, flea$species, 4, FALSE)
play_manual_tour <- function(data,
                             basis       = NULL,
                             manip_var,
                             theta       = NULL,     # [radians]
                             phi_min     = 0,        # [radians]
                             phi_max     = .5 * pi,  # [radians]
                             n_slides    = 20,
                             manip_col   = "blue",   # color of manip_var
                             render_type = render_plotly, # alt render_gganimate.
                             cat_var     = NULL,
                             fps         = 3,
                             init_rescale_data = FALSE,
                             ...) {
  if (init_rescale_data) data <- tourr::rescale(data)
  if (is.null(basis)) {
    message("NULL basis passed. Initializing random basis.")
    basis <- tourr::basis_random(n = ncol(data))
  }
  
  m_tour <- manual_tour(basis, manip_var, theta, phi_min, phi_max, n_slides)
  
  slides <- array2df(m_tour, data)
  disp   <- render_type(slides, manip_col, cat_var, fps, ...)
  
  disp
}
