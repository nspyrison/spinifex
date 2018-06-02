#' Check for orthonormality of a matrix (or basis)
#' 
#' For internal use mainly. returns TRUE or basis transposed %*% basis, would be identity matrix if basis is orthernormal
#' 
#' @param basis a matrix to check for orthonormality
#' 
#' @export
is_orthornormal <- function(basis) {
  mat <- basis
  mat_t <- t(mat)
  ans <- all.equal(mat_t %*% mat, diag(ncol(basis)), tol=1e-8)
  if (ans != "TRUE") {ans <- mat_t %*% mat}
  
  return(ans)
}
