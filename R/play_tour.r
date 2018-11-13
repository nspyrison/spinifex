#' Render display of a provided tour path and data
#'
#' Takes the result of tourr::save_history (or manual_tour()) and optionally 
#' data, renders the tour and passed data into a plotly animation object.
#'
#' @param tour_path The result of tourr::save_history
#' @param data Optional, tour::interpolate()'s the data. p must match the 
#'   tour_path.
#' @param ... Optionally pass addition arguments to plotly::animation_opts().

#' @export
#' @examples
#' data(flea)
#' flea_std <- tourr::rescale(flea[,1:6])
#' tpath <- tourr::save_history(flea_std, tourr::guided_tour(tourr::cmass))
#' 
#' print_tour(tour_path = tpath, data = flea_std)
play_tour <- function(tour_path,
                      data = NULL,
                      ...) {
  # if data missing, but in tour take data from tour.
  if(is.null(data) & class(tour_path) == "history_array"){ 
    message("data passed as NULL with a tourr tour_path containing attached data; rendering the tour_path data.")
    data <- attributes(tour_path)$data
  }
  
  # if tour isn't an array, make it an array.
  if(class(tour_path) != "array") {
    attr(tour_path, "class") <- NULL # remove history_array class attr.
    tour_path <- as.array(tour_path[,,]) # set array class attr.
  }
  
  slides <- create_slides(tour = tour_path, data = data)
  plotly_slideshow <- render_slideshow(slides = slides, disp_type = "plotly",
                                       ... = ...)
  
  return(plotly_slideshow)
}