### c+p from 'is_orthonormal.r':
#' Check for orthonormality of a matrix (or basis)
#' 
#' For internal use mainly. returns TRUE or basis transposed %*% basis, would be identity matrix if basis is orthernormal
#' 
#' @param basis a matrix or data.frame to check for orthonormality
#' 
#' @examples 
#' basis <- matrix(c(0.707, 0, 0.707, 0, 1, 0), ncol=2, byrow=FALSE)
#' is_orthornormal(basis)
#' basis <- matrix(c(runif(6)), ncol=2, byrow=FALSE)
#' is_orthornormal(basis)
#' @export
is_orthornormal <- function(basis) {
  stopifnot(class(basis) %in% c("matrix", "data.frame"))
  
  mat <- as.matrix(basis)
  mat_t <- t(mat)
  ans <- all.equal(mat_t %*% mat, diag(ncol(basis)), tol=1e-3)
  if (ans != "TRUE") {
    message("FALSE, at tol=1e-3. Transpose of the basis %*% basis is =")
    return(mat_t %*% mat)
  } 
  else {
    return(TRUE)
  }
}

#' Create and return an orthonormalized random basis
#'
#' Creates a [p, d=2] basis of random values then orthonormalize it
#'
#' @param p number of dimensions of the data
#' @param d number of dimensions of the basis.Defaults to 2.
#' @return orthonormalized basis [p, d=2]
#' 
#' @examples 
#' create_random_basis()
#' @export
#' 
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

#' View basis axes and table
#' 
#' This function can be used to draw the circle with axes 
#' representing the projection frame. Mainly for internal use. 
#' 
#' @param basis [p, d=2] basis, xy contributions of the var. 
#' @param data optional, [n, p], applies colnames to the rows of the basis.
#' 
#' @export
view_basis <- function(basis, data = NULL) {
  stopifnot(class(basis) %in% c("matrix", "data.frame"))
  
  tmp <- as.data.frame(basis)
  tmp <- cbind(tmp, sqrt(tmp[,1]^2 + tmp[,2]^2), atan(tmp[,2]/tmp[,1]))
  colnames(tmp) <- c("X", "Y", "H_xy", "theta")
  rownames(tmp) <- colnames(data)
  output <- tmp
  
  plot(0,type='n',axes=FALSE,ann=FALSE,xlim=c(-1, 1), ylim=c(-1, 1),asp=1)
  segments(0,0, basis[, 1], basis[, 2], col="grey50")
  theta <- seq(0, 2 * pi, length = 50)
  lines(cos(theta), sin(theta), col = "grey50")
  text(basis[, 1], basis[, 2], label = colnames(data), col = "grey50")
  
  return(output)
}

#' Orthonormalize a basis
#' 
#' This function checks if a basis is orthonormal, and if not it
#' does the orthonormalisation. Mostly for internal use.
#' 
#' @export
orthornormalize <- function(basis) {
  stopifnot(class(basis) %in% c("matrix", "data.frame"))
  
  if (!is_othonormal(basis)) 
    return(qr.Q(qr(mat))) #orthonormalize
  else {
    message("basis is already orthonormal.")
    return(as.matrix(basis))
  }
  
}

