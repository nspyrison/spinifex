#' Rotate and return the manipulation space
#'
#' Typically called by `manual_tour()`. Rotates a (p, d+1) manipulation space 
#' matrix by (d+1, d+1) rotation matrix, returning (p, d+1) matrix rotation 
#' space. The first 2 variables of which are the linear combination of the 
#' variables for a 2d projection.
#'
#' @param manip_space A (p, d+1) dim manipulation space to be rotated.
#' @param theta Angle in radians of rotation "in-plane", on the XY plane of the 
#'   reference frame. Typically set from manip_type in `proj_data()`.
#' @param phi Angle in radians of rotation "out-of-plane", the z axis of the 
#'   reference frame. Effectively changes the norm of XY contributions of the 
#'   manip_var.
#' @return A (p, d+1) orthonormal matrix of the rotated (manipulation) space.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[,1:6])
#' 
#' rb  <- basis_random(n = ncol(flea_std))
#' msp <- create_manip_space(basis = rb, manip_var = 4) 
#' rotate_manip_space(msp, theta = runif(1, max = 2 * pi), 
#'                    phi = runif(1, max = 2 * pi) )
rotate_manip_space <- function(manip_space, theta, phi) {
  # Initialize
  s_theta <- sin(theta)
  c_theta <- cos(theta)
  s_phi   <- sin(phi)
  c_phi   <- cos(phi)
  
  # 3D Rotation matrix, a function of theta and phi.
  R3 <- matrix(c(c_theta^2 * c_phi + s_theta^2,
                -c_theta * s_theta * (1 - c_phi),
                -c_theta * s_phi,                      # 3 of 9
                -c_theta * s_theta * (1 - c_phi),
                s_theta^2 * c_phi + c_theta^2,
                -s_theta * s_phi,                      # 6 of 9
                c_theta * s_phi,
                s_theta * s_phi,
                c_phi)                                 # 9 of 9
              ,nrow = 3, ncol = 3, byrow = TRUE)
  
  if (ncol(manip_space) == 3) { 
    rotation_space <- manip_space %*% R3
  } else {
    # 4D Rotation matrix, a function of theta, phi and psi.
    R4 <- rbind(R3, rep(0,3)) %>% 
      cbind(c(rep(0,3),1))
    rotation_space <- manip_space %*% R4
  } 
  
  colnames(rotation_space) <- colnames(manip_space)
  rownames(rotation_space) <- rownames(manip_space)
  
  rotation_space
}
