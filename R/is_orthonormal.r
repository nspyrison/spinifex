#' Check for orthonormality of a matrix (or basis)
#' 
#' For internal use mainly. returns T/F.
#' 
#' @param basis a matrix to check for orthonormality
#' 
#' @export
is_orthornormal <- function(basis) {
  mat <- basis
  mat_t <- t(mat)
  ans <- all.equal(mat_t %*% mat, diag(ncol(basis)), tol=1e-8)
  
  if (ans != "TRUE") {ans <- mat_t %*% mat}
  #if you want to see floating point error, ~e-16:
  #all.equal(mat_t %*% mat, diag(ncol(basis)), tol=0) 
  
  return(ans)
}
