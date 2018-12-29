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
#' data(flea)
#' flea_std <- tourr::rescale(flea[,1:6])
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
  
  # if tour isn't a nomal array, make it an array.
  if(class(tour) != "array") {
    attr(tour, "class") <- NULL # remove history_array class attr.
    tour <- as.array(tour[,,]) # set array class attr.
  }
  
  slides <- create_slides(tour = tour, data = data)
  plotly_slideshow <- render_slideshow(slides, disp_type = "plotly",
                                       cat_var, ...) 
  
  return(plotly_slideshow)
}

#' Render display of a manual tour of the passed data
#'
#' A wrapper function for `manual_tour`, `create_slides`, and 
#' `render_slideshow`. Allows the user to go from data to plotly object of 
#' manual tour in one function. For use with other tour paths see `play_tour`.
#' 
#' @param data A [n, p] dim data to project, consisting of 
#'   only numeric variables (for coercion into matrix).
#' @param basis A [p, d] dim orthonormal numeric matrix. If it's left null, a
#'   random basis will be created.
#' @param manip_var Integer column number or string exact column name of the.
#'   variable to manipulate. Required, no default.
#' @param init_rescale_data When TRUE will apply `tourr::rescale`` on the data.
#'   Defaults to FALSE.
#' @param ... Optionally pass other arguments to `manual_tour`, `create_slides`,
#' `render_slideshow`, or `plotly::animation_opts`.`
#' @return An animation in `disp_type` graphics of the interpolated data and 
#'   the corrisponding reference frame.
#' @export
#' @seealso play_tour
#' @examples
#' data(flea)
#' flea_std <- tourr::rescale(flea[,1:6])
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' 
#' (pss <- spinifex(data = flea_std, basis = rb, manip_var = 4))
#' 
#' (pss2 <- spinifex(data = flea[,1:6], manip_var = 4,
#'                   init_rescale_data = TRUE, init_rand_basis = TRUE))
spinifex <- function(data,
                     basis = NULL,
                     manip_var,
                     init_rescale_data = FALSE,
                     ...
) {
  if (init_rescale_data) data <- tourr::rescale(data)
  if (is.null(basis)) {
    message("NULL basis passed. Initializing random basis.")
    basis <- tourr::basis_random(n = ncol(data))
  }
  
  #tour <- manual_tour(...)
  
  # tour <- manual_tour(basis, manip_var, manip_type, theta,
  #                     phi_min, phi_max, n_slides)
  
  # tour <- manual_tour(basis=basis, manip_var = manip_var, manip_type, theta,
  #                     phi_min, phi_max, n_slides)
  
  tour <- manual_tour(basis = basis, manip_var = manip_var) #, ...)
  slides <- create_slides(..., data)
  slideshow <- render_slideshow(...)
  
  return(slideshow)
}