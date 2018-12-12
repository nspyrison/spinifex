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