#' Create a slideshow of the projected data and basis.
#'
#' Takes the result of data_proj() and uses base graphics to view each index with delay in base grahics
#'
#' @param data [n, p] dim data to project, consisting of 
#'    only numeric variables (for coercion into matrix.)
#' @param bases the output of manual_tour(), list of projection bases by index.
#' @import ggplot2
#' @import ggthemes
#' @import plotly
#' @export
#' @examples
#' 
#' require(tourr)
#' data(flea)
#' flea_std <- rescale(flea[,1:6])
#' 
#' rb <- basis_random(n = ncol(flea_std), d=2)
#' prj <- manual_tour(rb, manip_var=4, manip_type = "radial",
#'  phi_from = 0, phi_to = pi, n_slides = 20)
#' create_slideshow(flea_std, prj)
#' 
create_slideshow <- function(data, bases, ...) {
  # Check that data dimensions equal projection dimensions
  stopifnot(is.matrix(data))
  stopifnot(is.matrix(bases))
  stopifnot(ncol(data) == nrow(bases[, , 1]))
  
  #TODO: assertion here.
  
  #TODO: Consume and fill out the slideshow for bases only.
  # Generate the projected data, indexed by frame
  nframes <- dim(bases)[3]
  proj_data <- NULL
  proj_bases <- NULL
  for (i in 1:nframes) {
    pd <- tibble::as_tibble(data %*% bases[, , i])
    pd$indx <- i
    proj_data <- dplyr::bind_rows(proj_data, pd)
    pb <- tibble::as_tibble(bases[, , i])
    pb$indx <- i
    pb$lab_abbr <- lab_abbr
    proj_bases <- dplyr::bind_rows(proj_bases, pb)
  }
  
  return(slideshow)
}