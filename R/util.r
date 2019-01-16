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
#' view_basis(rb)
#' @export
view_basis <- function(basis = create_identity_basis(6), 
                       labels = paste0("V", 1:nrow(basis)), 
                       ...) {
  stopifnot(class(basis) %in% c("matrix", "data.frame"))
  
  basis <- cbind(basis, 
                 norm_XY = sqrt(basis[,1]^2 + basis[,2]^2), 
                 theta   = atan(basis[,2] / basis[,1])
  )
  colnames(basis)[1:2] <- c("X", "Y")
  
  plot(0, asp = 1, type = 'n', axes = FALSE, ann = FALSE,
       xlim = c(-1, 1), ylim = c(-1, 1))
  segments(0, 0, basis[, 1], basis[, 2], ...)
  theta <- seq(0, 2 * pi, length = 360)
  lines(cos(theta), sin(theta), ...)
  text(basis[, 1], basis[, 2], label = labels, ...)
  
  return(basis)
}