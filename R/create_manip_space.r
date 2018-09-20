#' Create a manipulation space
#'
#' Primarily internal use. Create an [p, d+1=3] orthonormal manipulation space from the given basis concatonated with a zero vector, with manip_var set to 1.
#'
#' @param basis [p, d=2] orthonormal basis
#' @param manip_var number of the variable to rotate
#' @return manip_space, a [p, d+1=3] orthonormal manipulation space
#' @export
create_manip_space <- function(basis, manip_var){
  z <- rep(0, len = nrow(basis))
  z[manip_var] <- 1
  manip_space <- orthornormalize(cbind(basis, z) )
  if (ncol(manip_space) == 3) {colnames(manip_space) <- c("x","y","z")}
  if (ncol(manip_space) == 4) {colnames(manip_space) <- c("x","y","z","w")}
  rownames(manip_space) <- colnames(basis)
  
  stopifnot(dim(manip_space) == dim(basis) + c(0, 1))
  return(manip_space)
}
