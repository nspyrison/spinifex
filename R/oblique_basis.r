#' Return the basis of an oblique frame
#'
#' Rotates a basis returning (p, 2) basis describing `oblique_frame()` 
#' Used to create an oblique tour by small changes to the rotation.
#' 
#' @param basis A (p, d) dim orthonormal numeric matrix. 
#'   If it's left null, random basis will be used.
#' @param manip_var Number of the column/dimension to rotate.
#' @param theta Angle in radians of "in-plane" rotation, on the XY plane of the 
#'   reference frame. Required, no default.
#'   If left NULL, will initialize the radial angle of the `manip_var`.`
#' @param phi Phi is angle in radians of 
#'   the "out-of-plane" rotation, the z-axis of the reference frame. 
#'   Required, no default.
#' @return (p, 2) matrix of the rotated basis
#' @import tourr
#' @export
#' @examples
#' rb <- tourr::basis_random(n = 6)
#' theta <- runif(1, 0, 2*pi)
#' phi <- runif(1, 0, 2*pi)
#' 
#' oblique_basis(basis = rb, manip_var = 4, theta, phi)

oblique_basis <- function(basis = NULL,
                          manip_var,
                          theta = NULL,
                          phi   = NULL) {
  
  m_sp <- create_manip_space(basis, manip_var)
  ret <- rotate_manip_space(manip_space = m_sp, theta, phi)[, 1:2]
  colnames(ret) <- c("x","y")
  
  ret
}
