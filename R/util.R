#' Create a random basis
#'
#' Creates a [p, d=2] random orthonormal basis
#'
#' @param p number of dimensions of the data
#' @param d number of dimensions of the basis.Defaults to 2.
#' @return orthonormalized matrix, the [p, d=2] basis
#' @export
#' 
basis_random <- function(p, d = 2) {
  mvn <- matrix(rnorm(p * d), ncol = d)
  basis <- qr.Q(qr(mvn)) #orthonormalize
  return(basis)
}


#' Create an indentity basis
#'
#' Creates a [p, d=2] indentity basis; indentity matrix followed by 0s
#'
#' @param p number of dimensions of the data
#' @param d number of dimensions of the basis, defaulting to 2
#' @return [p, d=2] indentity matrix followed by 0s
#' @export
#' 
basis_identity <- function(p, d = 2){
  identity_basis <- rbind(diag(d), matrix(0, ncol = d, nrow = p - d))
  return(identity_basis)
}


