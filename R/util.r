#' Plot projection frame and return the axes table.
#' 
#' Uses base graphics to plot the circle with axes representing
#' the projection frame. Returns the corresponding table.
#' 
#' @param basis A (p, d) basis, XY linear combination of each dimension 
#'   (numeric variable).
#' @param labels Optional, character vector of `p` length, add name to the axes 
#'   in the reference frame, typically the variable names.
#' @param ... Optionally pass additional arguments to `segments`, `lines`, or 
#'   `text`.
#' @return The basis, norm, and theta, also plots the reference frame in base 
#'   graphics.
#' 
#' @examples 
#' rb <- tourr::basis_random(4, 2)
#' view_basis(basis = rb)
#' @export
view_basis <- function(basis = tourr::basis_init(6,2), 
                       labels = paste0("V", 1:nrow(basis)), 
                       ...) {
  graphics::plot(0, asp = 1, type = 'n', axes = FALSE, ann = FALSE,
                 xlim = c(-1, 1), ylim = c(-1, 1))
  graphics::segments(0, 0, basis[, 1], basis[, 2], ...)
  theta <- seq(0, 2 * pi, length = 360)
  graphics::lines(cos(theta), sin(theta), ...)
  graphics::text(basis[, 1], basis[, 2], label = labels, ...)
  
  return(basis)
}