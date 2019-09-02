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
#' @return An animation of a radial tour.
#' @import tourr
#' @export
#' @examples
#' \dontrun{
#' flea_std <- tourr::rescale(tourr::flea[,1:6])
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' 
#' play_manual_tour(basis = rb, data = flea_std, manip_var = 4)
#' 
#' play_manual_tour(basis = rb, data = flea_std, manip_var = 6, 
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
