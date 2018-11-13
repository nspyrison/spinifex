#' Creates and returns an identity basis
#'
#' Creates a [p, d] dim identity basis; identity matrix followed by rows 0s.
#'
#' @param p number of dimensions of the data.
#' @param d number of dimensions of the basis (ie. the dimensionality of the 
#'   graphic.) Defaults to 2.
#' @return A [p, d] dim identity matrix followed by rows of 0s.
#' 
#' @examples 
#' create_identity_basis()
#' @export
create_identity_basis <- function(p = 6, d = 2) {
  stopifnot(p >= d)
  
  basis <- matrix(0, nrow = p, ncol = d)
  diag(basis) <- 1
  
  return(basis)
}

#' Plot projection frame and return the axes table.
#' 
#' Uses base graphics to plot the circle with axes representing
#' the projection frame. Returns the corrisponding table.
#' 
#' @param basis A [p, d] basis, XY linear combination of each dimension 
#'   (numeric variable).
#' @param labels Optional, character vector of `p`` length, applies colnames 
#'   to the rows of the basis.
#' @param ... Optionally pass additional aruments to segments(), lines(), or 
#'   text().
#' @return Nothing, but cretes a plot of the reference frame in base graphics.
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