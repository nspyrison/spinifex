#' Create a slideshow array of the projected bases
#'
#' Takes the result of manual_tour() and uses base graphics to view each index with delay in base grahics
#'
#' @param data [n, p] dim data to project, consisting of 
#'    only numeric variables (for coercion into matrix.)
#' @param bases the output of manual_tour(), list of projection bases by index.
#' @import ggplot2
#' @import ggthemes
#' @import plotly
#' @export
#' @examples
#' data(flea)
#' flea_std <- tourr::rescale(flea[,1:6])
#' 
#' rb <- basis_random(n = ncol(flea_std), d=2)
#' prj <- manual_tour(rb, manip_var=4, manip_type = "radial",
#'   phi_from = 0, phi_to = pi, n_slides = 20)
#' create_slideshow(flea_std, prj)
create_slideshow <- function(data, bases, ...) {
  # Assertions
  stopifnot(is.matrix(as.matrix(data) ) )
  stopifnot(is.matrix(bases))
  stopifnot(ncol(data) == nrow(bases[, , 1]) )
  if (!is.matrix(data)) data <- as.matrix(data)
  
  # Generate the projected data by slide
  n_slides <- dim(bases)[3]
  data_slide <- NULL
  data_slides <- NULL
  basis_slide <- NULL
  bases_slides <- NULL
  lab_abbr = " TODO: " # need manip var from attr. # also capture theta, phi?
  for (slide in n_slides) {
    data_slide  <- tibble::as_tibble(data %*% bases[, , slide])
    data_slide$slide <- slide
    data_slides <-  dplyr::bind_rows(data_slides, data_slide)
    basis_slide <- tibble::as_tibble(bases[, , slide])
    basis_slide$slide <- slide
    basis_slide$lab_abbr <- lab_abbr
    bases_slides <- dplyr::bind_rows(bases_slides, basis_slide)
  }
  
  slide_deck <- list(data_slides = data_slides,
                     bases_slides = bases_slides)
  return(slide_deck)
}