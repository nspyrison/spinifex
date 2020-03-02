#' Performs a rotation on the manipulation space
#'
#' A specific R3 rotation of the manipulation space for a 2D tour.
#' Typically called by `manual_tour()`. The first 2 columns are x and y in 
#' the projection plane. The 3rd column extends "in the z-direction" orthogonal 
#' to the projection plane.
#'
#' @param manip_space A (p, d+1) dim matrix (manipulation space) to be rotated.
#' @param theta Angle (radians) of "in-projection-plane" rotation (ie. on xy-
#' of the projection). Typically set by the manip_type arg in `proj_data()`.
#' @param phi Angle (radians) of "out-of-porjection-plane" rotation (ie. into
#' the z-direction of the manipulation space. Effectively changes the norm 
#' of the manip_var in the projection plane.
#' @return A (p, d+1) orthonormal matrix of the rotated (manipulation) space. 
#' The first 2 columns are x and y in the projection plane. The 3rd column 
#' extends "in the z-direction" orthogonal to the projection plane.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[,1:6])
#' rb  <- basis_random(n = ncol(flea_std))
#' msp <- create_manip_space(basis = rb, manip_var = 4)
#' 
#' rotate_manip_space(msp, theta = runif(1, max = 2 * pi), 
#'                    phi = runif(1, max = 2 * pi) )
rotate_manip_space <- function(manip_space, theta, phi) {
  ## Initialize
  s_theta <- sin(theta)
  c_theta <- cos(theta)
  s_phi   <- sin(phi)
  c_phi   <- cos(phi)
  
  ## 3D rotation matrix, as a function of theta and phi.
  R <- matrix(c(c_theta^2 * c_phi + s_theta^2,
                -c_theta * s_theta * (1 - c_phi),
                -c_theta * s_phi,                      # 3 of 9
                -c_theta * s_theta * (1 - c_phi),
                s_theta^2 * c_phi + c_theta^2,
                -s_theta * s_phi,                      # 6 of 9
                c_theta * s_phi,
                s_theta * s_phi,
                c_phi)                                 # 9 of 9
              ,nrow = 3, ncol = 3, byrow = TRUE)
  rotated_space <- manip_space %*% R
  
  ## Checks and formating
  stopifnot(tourr::is_orthonormal(rotated_space))
  if (is.null(colnames(manip_space))) {colnames(rotated_space) <- paste0("proj_", c("x", "y", "z"))
  } else {colnames(rotated_space) <- colnames(manip_space)}
  if (is.null(rownames(manip_space))) {rownames(rotated_space) <- paste0("orig_", c("x", "y", "z"))
  } else {rownames(rotated_space) <- rownames(manip_space)}
  
  rotated_space
}
