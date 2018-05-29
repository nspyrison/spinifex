#' Creates and returns an identity basis
#'
#' Creates a [p, d=2] identity basis; identity matrix followed by 0s
#'
#' @param p number of dimensions of the data
#' @param d number of dimensions of the basis, defaulting to 2
#' @return [p, d=2] identity matrix followed by 0s
#' @export
#' 
create_identity_basis <- function(p, d = 2){
  
  start <- matrix(0, nrow = p, ncol = d)
  (diag(start) <- 1)
  start
  identity_basis <- rbind(diag(d), matrix(0, ncol = d, nrow = p - d))
  return(identity_basis)
}
