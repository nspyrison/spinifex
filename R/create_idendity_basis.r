#' Creates and returns an identity basis
#'
#' Creates a [p, d=2] identity basis; identity matrix followed by 0s
#'
#' @param p number of dimensions of the data
#' @param d number of dimensions of the basis. Defaults to 2
#' @return [p, d=2] identity matrix followed by rows of 0s
#' @export
#' 
create_identity_basis <- function(p, d = 2){
  basis <- matrix(0, nrow = p, ncol = d)
  diag(basis) <- 1
  
  stopifnot(dim(basis) == c(p,d))
  return(basis)
}
