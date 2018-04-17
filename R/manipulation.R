#' Create a manipulation space
#'
#' Create an [p, d+1=3] orthonormal manipulation space from the given [p, d=2] orthonormal basis. This is used before a manipulation, creating the d+1 space to allow for the rotation.
#'
#' @param basis [p, 2] orthonormal basis
#' @param manip_var number of the variable to rotate
#' @return [p, d+1=3] orthonormal manipulation space
#' @export
#' @examples
#' this_manip_space <- create_manip_space(basis = basis_random(p = 6), manip_var = 2)
#' 
create_manip_space <- function(basis = basis_random(p = 6, d = 2), manip_var = 1){
  v <- rep(0, len = nrow(basis))
  v[manip_var] <- 1
  manip_space <- qr.Q(qr(cbind(basis, v))) #orthonormalize
  if (ncol(manip_space)==3) {colnames(manip_space) <- c("x","y","z")}
  rownames(manip_space) <- colnames(basis)
  1+1
  return(manip_space)
}

#' Rotate the manipulation space
#'
#' Rotates [p, 3] manipulation space by [3, 3] rotation matrix into a [p, 3] rotated space. Used by manipulations
#'
#' @param manip_space [p, 3] manipulation space to be rotated
#' @param theta angle of rotation w.r.t. the x-y projection
#' @param phi angle corisponding to the magnitude of manipulation
#' @export
#' 
rotate_manip_space <- function(manip_space, theta = 0, phi = 0){
  s_theta <- sin(theta)
  c_theta <- cos(theta)
  s_phi   <- sin(phi)
  c_phi   <- cos(phi)
  #3-d rotation matrix, as a function of theta and phi, [3,3]
  R <- matrix(c(c_theta^2 * c_phi + s_theta^2,
                -c_theta * s_theta * (1 - c_phi),
                -c_theta * s_phi,                      # 3 of 9
                -c_theta * s_theta * (1 - c_phi),
                s_theta^2 * c_phi + c_theta^2,
                -s_theta * s_phi,                      # 6 of 9
                c_theta * s_phi,
                s_theta * s_phi,
                c_phi )                                # 9 of 9
              ,nrow = 3, ncol = 3, byrow = TRUE)
  r_space <- manip_space %*% R
  colnames(r_space) <- colnames(manip_space)
  rownames(r_space) <- rownames(manip_space)
  return(r_space)
}


#' Horizontaly roatate 1 dim of a p-dim basis.
#' 
#' Performs a horizontal rotation on 1 dimension of a p-dimensional manipulation space. Returns the x,y,z contribution from each dimension of the rotated manipulation space [3, p].
#'
#' @param basis starting basis to rotate
#' @param manip_var number of the variable to manipulate
#' @param phi angle changing the magnitude of `manip_var`
#' 
#' @export
#' @examples
#' this_manip_space <- create_manip_space(basis = basis_random(p = 6), manip_var = 2)
#' horizontal_manip(manip_space = this_manip_space, phi = pi/3)
#' 
horizontal_manip <- function(manip_space, phi = 0, ...){
  theta <- 0 # 0 for horizontal
  rotate_manip_space(manip_space, theta, phi) -> r_space
  return(r_space)
}


#' Vertically roatate 1 dim of a p-dim basis.
#' 
#' Performs a vertical rotation on 1 dimension of a given p-dimensional basis.
#' Returns the x,y,z contribution from each dimension of the rotated basis as a (p x 3) matrix.
#'
#' @param basis starting basis to rotate
#' @param manip_var number of the variable to manipulate
#' @param phi angle changing the magnitude of `manip_var`
#' 
#' @export
#' @examples
#' this_manip_space <- create_manip_space(basis = basis_random(p = 6), manip_var = 2)
#' vertical_manip(manip_space = this_manip_space, phi = pi/3)
#' 
vertical_manip <- function(manip_space, phi = 0, ...){
  theta <- pi/2 #pi/2 for vertical.
  rotate_manip_space(manip_space, theta, phi) -> r_space
  return(r_space)
}


#' Radially roatate 1 dim of a p-dim basis.
#' 
#' Performs a radial rotation on 1 dimension of a given p-dimensional basis.
#' Returns the x,y,z contribution from each dimension of the rotated basis as a (p x 3) matrix.
#'
#' @param basis starting basis to rotate
#' @param manip_var number of the variable to manipulate
#' @param phi angle changing the magnitude of `manip_var`
#' @param theta angle change of `manip_var`
#' 
#' @export
#' @examples
#' this_manip_space <- create_manip_space(basis = basis_random(p = 6), manip_var = 2)
#' radial_manip(manip_space = this_manip_space, phi = pi/3, theta = pi/4)
#' 
radial_manip <- function(manip_space, phi = 0, theta = 0){
  rotate_manip_space(manip_space, theta, phi) -> r_space
  return(r_space)
}
