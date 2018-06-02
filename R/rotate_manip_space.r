#' Rotate and return the manipulation space
#'
#' Primarily for internal use. Rotates [p, 3] manipulation space by [3, 3] rotation, Returns [p, 3] orthonormal matrix, XYZ components after roation
#'
#' @param manip_space [p, 3] manipulation space to be rotated
#' @param theta angle of rotation w.r.t. the x-y projection. Typically set from manip_type in proj_data()
#' @param phi angle corisponding to the magnitude of manipulation
#' @export
rotate_manip_space <- function(manip_space, theta, phi){
  stopifnot(ncol(manip_space) == 3)
  stopifnot(is.matrix(manip_space))
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
  stopifnot(dim(r_space) == c(nrow(manip_space),3) )
  stopifnot(is.matrix(r_space))
  return(r_space)
}