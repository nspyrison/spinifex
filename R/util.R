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


#' Draw tour axes with base graphics
#' 
#' For internal use mainly 
#' 
#' @param basis [p, d=2] basis, xy contributions of the var. 
#' @param data optional, [n, p], applies colnames to the rows of the basis.
#' 
#' @export
view_basis <- function(basis, data = NULL) {
  
  tmp <- as.data.frame(basis)
  tmp <- cbind(tmp, sqrt(tmp[,1]^2 + tmp[,2]^2), atan(tmp[,2]/tmp[,1]))
  colnames(tmp) <- c("X", "Y", "H_xy", "theta")
  rownames(tmp) <- colnames(data)
  view_basis <- tmp
  
  plot(0,type='n',axes=FALSE,ann=FALSE,xlim=c(-1, 1), ylim=c(-1, 1),asp=1)
  segments(0,0, basis[, 1], basis[, 2], col="grey50")
  theta <- seq(0, 2 * pi, length = 50)
  lines(cos(theta), sin(theta), col = "grey50")
  text(basis[, 1], basis[, 2], label = colnames(data), col = "grey50")

  return(view_basis)
}

