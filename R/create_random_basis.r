#' Create and return an orthonormalized random basis
#'
#' Creates a [p, d=2] basis of random values then orthonormalize it
#'
#' @param p number of dimensions of the data
#' @param d number of dimensions of the basis.Defaults to 2.
#' @return orthonormalized basis [p, d=2]
#' @export
#' 
create_random_basis <- function(p, d = 2) {
  mvn <- matrix(rnorm(p * d), ncol = d)
  basis <- qr.Q(qr(mvn)) #orthonormalize
  
  stopifnot(ncol(basis) == d)
  stopifnot(nrow(basis) == p)
  return(basis)
}

