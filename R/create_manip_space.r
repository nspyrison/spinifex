#' Create a manipulation space
#'
#' Primarily internal use. Create an [p, d+1=3] orthonormal manipulation space from the given [p, d=2] orthonormal basis. This is used before a manipulation, creating the d+1 space to allow for the rotation.
#'
#' @param basis [p, d=2] orthonormal basis
#' @param manip_var number of the variable to rotate
#' @return manip_space, a [p, d+1=3] orthonormal manipulation space
#' @export
create_manip_space <- function(basis, manip_var){
  z <- rep(0, len = nrow(basis))
  z[manip_var] <- 1
  manip_space <- qr.Q(qr(cbind(basis, z))) #orthonormalize
  if (ncol(manip_space)==3) {colnames(manip_space) <- c("x","y","z")}
  rownames(manip_space) <- colnames(basis)
  return(manip_space)
}
