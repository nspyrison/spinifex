#' Render display of a provided tour path and data
#'
#' Takes the result of tourr::save_history and data, returns a plotly object.
#'
#' @param tour_path The result of tourr::save_history
#' @param data Optional, tour::interpolate()'s the data. p must match the 
#'   tour_path.
#' @export
#' @examples
#' data(flea)
#' flea_std <- tourr::rescale(flea[,1:6])
#' tpath <- tourr::save_history(flea_std, tourr::guided_tour(tourr::cmass))
#' 
#' print_tour(tour_path = tpath, data = flea_std)
print_tour <- function(tour_path,
                       data = NULL) {
  # Assertions covered in create_slideshow()
  
  # extract data here?
  # attributes(tour_path)$data
  
  attr(tour_path, "class") <- NULL # remove history_array class attr.
  tour_path <- as.array(tour_path[,,]) # set array class attr.
  
  sshow <- create_slideshow(tour = tour_path, data = data) # work done here.
  plotly_slideshow <- render_slideshow(slide_deck = sshow, disp_type = "plotly")
  
  return(plotly_slideshow)
}