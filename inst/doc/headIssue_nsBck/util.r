#' Rescale a matrix or data frame
#'
#' Standardise each column to have range [0, 1].
#'
#' @param df data frame or matrix
#' @return df rescaled to [0,1]
#' @examples 
#' data(flea)
#' flea[, 1:6] -> ThisDf
#' rescale01(ThisDf)
#' @export
rescale01 <- function(df) {
  apply(df, 2, function(x) {(x - min(x)) / diff(range(x))} )
}

#' Orthonormalise a basis
#' 
#' This function checks if a basis is orthonormal, and if not it
#' does the orthonormalisation there of.
#' 
#' @param basis A [p, d=2] basis, containing the xy contributions of each dimension (numeric variable). 
#' 
#' @examples 
#'  matrix(c(runif(6)), ncol=2, byrow=FALSE) -> ThisBasis
<<<<<<< HEAD
#' is_orthornormal(ThisBasis) # message and returns basis^t <dot product> basis.
#' orthornormalize(ThisBasis)
#' @export
orthornormalise <- function(basis) {
  stopifnot(class(basis) %in% c("matrix", "data.frame"))
  
  if (class(basis) != "matrix") {basis <- as.matrix(basis)}
  if (!is_orthonormal(basis)) {
    return(qr.Q(qr(mat))) #orthonormalize
    else {
      message("basis is already orthonormal.")
      return(basis)
    }
  }
=======
#' is_orthonormal(ThisBasis) # message and returns basis^t <dot product> basis.
#' orthonormalize(ThisBasis)
#' @export
orthonormalise <- function(basis) {
  stopifnot(class(basis) %in% c("matrix", "data.frame"))
  
  if (class(basis) != "matrix") {
    basis <- as.matrix(basis)
  }
  if (!is_orthonormal(basis)) {
    return(qr.Q(qr(mat))) #orthonormalize
  }
  else {
    message("basis is already orthonormal.")
    return(basis)
  }

>>>>>>> 1a1a7d05825d446ff3f39ec809db14ae7c10d334
}

#' Check for orthonormality of a basis (or matrix)
#' 
#' Checks if basis^t <dot product> basis is close to the [d, d] identity matrix.
#' Returns TRUE if each element is withing e-3 of the identity, otherwise throws
#' a message and returns basis^t <dot product> basis.
#' 
#' @param basis A matrix or data.frame to check for orthonormality.
#' @return TRUE, or prints a message and returns basis^t <dot product> basis.
#' 
#' @examples 
#' create_random_basis(p=6) -> ThisBasis
#' is_orthonormal(ThisBasis) # TRUE
#' ThisBasis <- matrix(c(runif(6)), ncol=2, byrow=FALSE)
#' is_orthonormal(ThisBasis) # message and returns basis^t <dot product> basis.
#' @export
is_orthonormal <- function(basis) {
  stopifnot(class(basis) %in% c("matrix", "data.frame"))
  
  mat <- as.matrix(basis)
  mat_t <- t(mat)
  ans <- all.equal(mat_t %*% mat, diag(ncol(basis)), tol=1e-3)
  if (ans != TRUE) {
    message("FALSE, at tol=1e-3. basis^t %*% basis is:")
    return(mat_t %*% mat)
  } 
  else {
    return(TRUE)
  }
}

#' Create and return a random, then orthonormalized basis
#'
#' Creates a [p, d=2] dim basis of random values, orthonormalize it, and returns it.
#'
#' @param p Number of dimensions (numeric variables) of the data.
#' @param d Number of dimensions of the basis. Defaults to 2.
#' @return Orthonormalized basis of dim [p, d=2] made from random values.
#' 
#' @examples 
#' create_random_basis(6)
#' @export
create_random_basis <- function(p, d = 2) {
  stopifnot(class(p) %in% c("integer", "numeric"))
  stopifnot(class(d) %in% c("integer", "numeric"))
  stopifnot(length(p) == 1)
  stopifnot(length(d) == 1)
  
  mvn <- matrix(rnorm(p * d), ncol = d)
  basis <- qr.Q(qr(mvn)) #orthonormalize
  
  stopifnot(class(basis) == "matrix")
  stopifnot(ncol(basis) == d)
  stopifnot(nrow(basis) == p)
  return(basis)
}

#' Creates and returns an identity basis
#'
#' Creates a [p, d=2] dim identity basis; identity matrix followed by rows 0s.
#'
#' @param p number of dimensions of the data. p must be equal to or greater than d.
#' @param d number of dimensions of the basis. Defaults to 2.
#' @return A [p, d=2] dim identity matrix followed by rows of 0s.
#' 
#' @examples 
#' create_identity_basis(6)
#' @export
create_identity_basis <- function(p, d = 2){
  stopifnot(class(as.integer(p) == "integer"))
  stopifnot(class(as.integer(d) == "integer"))
  stopifnot(length(p) == 1)
  stopifnot(length(d) == 1)
  stopifnot(p >= d)
  
  basis <- matrix(0, nrow = p, ncol = d)
  diag(basis) <- 1
  
  stopifnot(dim(basis) == c(p,d))
  return(basis)
}

#' Plot projection frame and return the axes table.
#' 
#' Uses base graphics to plot the circle with axes representing
#' the projection frame. Returns the corrisponding table.
#' 
#' @param basis A [p, d=2] basis, xy contributions of each dimension (numeric variable). 
#' @param data Optional, of [n, p] dim, applies colnames to the rows of the basis.
#' 
#' @examples 
#' create_identity_basis(6) -> ThisBasis
#' view_basis(ThisBasis)
#' @export
view_basis <- function(basis, data = NULL) {
  stopifnot(class(basis) %in% c("matrix", "data.frame"))
  
  tmp <- basis
  tmp <- cbind(tmp, sqrt(tmp[,1]^2 + tmp[,2]^2), atan(tmp[,2]/tmp[,1]))
  colnames(tmp) <- c("X", "Y", "norm_xy", "theta")
  axes <- tibble::as_tibble(tmp)
  
  lab = NULL
  if (!is.null(data)) {
    rownames(tmp) <- colnames(data)
    this_label = colnames(data)
    }
  
  plot(0,type='n',axes=FALSE,ann=FALSE,xlim=c(-1, 1), ylim=c(-1, 1),asp=1)
  segments(0,0, basis[, 1], basis[, 2], col="grey50")
  theta <- seq(0, 2 * pi, length = 50)
  lines(cos(theta), sin(theta), col = "grey50")
  text(basis[, 1], basis[, 2], label = this_label, col = "grey50")
  
  stopifnot(class(axes) == "tibble")
  stopifnot(dim(axes) == (dim(basis) + c(0,2)) )
  return(axes)
}