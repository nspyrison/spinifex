#' Render display of a provided tour path
#'
#' Takes the result of `tourr::save_history()` or `manual_tour()`, interpolates
#' over the path and renders into a selected `render_type`.
#'
#' @param tour_path The result of `tourr::save_history()` or `manual_tour()`.
#' @param data Optional, number of columns must match that of `tour_path`.
#' @param angle Target distance (in radians) between steps. Defaults to .15.
#' @param render_type Graphics to render to. Defaults to render_plotly, 
#'   alternative use render_gganimate.
#' @param rescale_data When TRUE scales the data to between 0 and 1.
#'   Defaults to FALSE.
#' @param ... Optionally pass additional arguments to `render_type`.
#' @import tourr
#' @export
#' @examples
#' \dontrun{
#' flea_std <- rescale(tourr::flea[,1:6])
#' tpath    <- save_history(flea_std, tour_path = grand_tour(),max = 3)
#' 
#' play_tour_path(tour_path = tpath, data = flea_std)
#' 
#' play_tour_path(tour_path = tpath, data = flea_std, angle = .25, fps = 4,
#'   render_type = render_gganimate, col = col_of(flea$species), axes = "bottomleft")
#' }

play_tour_path <- function(tour_path,
                           data  = NULL,
                           angle = .15,
                           render_type = render_plotly,
                           rescale_data = FALSE,
                           ...) {
  if (rescale_data) data <- tourr::rescale(data)
  # if data missing, but an attribute, use that.
  if(is.null(data) & !is.null(attributes(tour_path)$data)){ 
    message("data passed as NULL with a tourr object containing attached data; rendering the tour_path data.")
    data <- attributes(tour_path)$data
  }
  
  tour_path <- tourr::interpolate(basis_set = tour_path, angle = angle)
  attr(tour_path, "class") <- "array"
  tour_df <- array2df(array = tour_path, data = data)
  disp <- render_type(slides = tour_df, ...)
  
  disp
}

