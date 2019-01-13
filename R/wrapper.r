#' Render display of a provided tour path and data
#'
#' Takes the result of `tourr::save_history` (or `manual_tour` and optionally 
#' data, renders the tour and passed data into a plotly animation object.
#'
#' @param tour The result of `tourr::save_history` or `manual_tour`.
#' @param data Optional, number of columns must match that of `tour`.
#' @param ... Optionally pass addition arguments to `plotly::animation_opts`.

#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[,1:6])
#' tpath <- tourr::save_history(flea_std, tourr::guided_tour(tourr::cmass))
#' 
#' play_tour(tour = tpath, data = flea_std)
play_tour <- function(tour,
                      data = NULL,
                      ...) {
  # if data missing, but in tour take data from tour.
  if(is.null(data) & class(tour) == "history_array"){ 
    message("data passed as NULL with a tourr object containing attached data; rendering the tour_path data.")
    data <- attributes(tour)$data
  }
  
  # if tour isn't a normal array, make it an array.
  if(class(tour) != "array") {
    attr(tour, "class") <- NULL
    tour <- as.array(tour)
  }
  
  slides <- create_slides(tour, data)
  plotly_slideshow <- render_slideshow(slides, disp_type = "plotly",
                                       cat_var, ...) 
  
  return(plotly_slideshow)
}

#' Render display of a manual tour of the passed data
#'
#' A wrapper function for manual_tour(), create_slides(), and 
#' render_slideshow(). Allows the user to go from data to plotly object of 
#' manual tour in one function. For use with other tour paths see play_tour().
#' 
#' @param data A [n, p] dim data to project, consisting of 
#'   only numeric variables (for coercion into matrix).
#' @param basis A [p, d] dim orthonormal numeric matrix. If it's left null, a
#'   random basis will be created.
#' @param manip_var Integer column number or string exact column name of the.
#'   variable to manipulate. Required, no default.
#' @param manip_type String of the type of manipulation to use. 
#'   Defaults to "radial". Alternatively accepts "horizontal" or "vertical". 
#'   Yields to `theta` if set. Must set either `manip_type` or `theta`.
#' @param theta Angle in radians of "in-plane" rotation, on the XY plane of the 
#'   reference frame. Typically set from manip_type in proj_data(). Supersedes 
#'   `manip_type`. Must set either `manip_type` or `theta`.
#' @param phi_min Minimum value phi should move to. Phi is angle in radians of 
#'   the "out-of-plane" rotation, the z-axis of the reference frame. 
#'   Required, defaults to 0.
#' @param phi_max Maximum value phi should move to. Phi is angle in radians of 
#'   the "out-of-plane" rotation, the z-axis of the reference frame. 
#'   Required, defaults to 2 * pi.
#' @param n_slides The number of slide-interpolations to make for the tour.
#' @param disp_type The graphics system to use. Defaults to 'plotly'.
#' @param manip_col String of the color to highlight the `manip_var`.
#' @param init_rescale_data When TRUE will apply tourr::rescale() on the data.
#'   Defaults to FALSE.
#' @param cat_var Categorical variable, optionally used to set the data point 
#'   color and shape.
#' @param slide_time Time to show each slide for in seconds. essentially 1/fps, 
#'   defaults to .3 seconds.
#' @param ... Optionally pass addition arguments to the `disp_type` options.
#' @return An animation in `disp_type` graphics of the interpolated data and 
#'   the corresponding reference frame.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[,1:6])
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' 
#' spinifex(data = flea_std, basis = rb, manip_var = 4)
#' 
#' spinifex(data = tourr::flea[,1:6], manip_var = 4, init_rescale_data = TRUE)
spinifex <- function(data,
                     basis       = NULL,
                     manip_var,
                     manip_type  = "radial",
                     theta       = NULL,     # [radians]
                     phi_min     = 0,        # [radians]
                     phi_max     = .5 * pi,  # [radians]
                     n_slides    = 20,
                     render_type = render_plotly,
                     manip_col   = "blue",   # color of manip_var
                     cat_var     = NULL,
                     slide_time  = .3,
                     init_rescale_data = FALSE) 
{
  if (init_rescale_data) data <- tourr::rescale(data)
  if (is.null(basis)) {
    message("NULL basis passed. Initializing random basis.")
    basis <- tourr::basis_random(n = ncol(data))
  }
  
  manual_tour <- manual_tour(basis = basis, manip_var = manip_var, 
                             manip_type = manip_type, theta = theta, 
                             phi_min = phi_min, phi_max = phi_max, 
                             n_slides = n_slides)
  
  slides <- create_slides(tour = manual_tour, data = data)
  
  slideshow <- render_type(slides = slides, 
                           manip_col = manip_col, cat_var = cat_var,
                           slide_time = slide_time)
  
  return(slideshow)
}
