#' Create a manipulation space
#'
#' Typically called by `manual_tour()`. Creates a (p, d) orthonormal matrix,
#' the manipulation space from the given basis right concatenated with a zero 
#' vector, with manip_var set to 1.
#'
#' @param basis A (p, d) orthonormal matrix.
#' @param manip_var Number of the column/dimension to rotate.
#' @return A (p, d+1) orthonormal matrix, the manipulation space.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[,1:6])
#' 
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' create_manip_space(basis = rb, manip_var = 4)
create_manip_space <- function(basis, 
                               manip_var) {
  if (!is.matrix(basis)) as.matrix(basis)

  e            <- rep(0, len = nrow(basis))
  e[manip_var] <- 1
  manip_space  <- tourr::orthonormalise(cbind(basis, e))
  colnames(manip_space) <- NULL
  
  manip_space
}

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
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' msp <- create_manip_space(basis = rb, manip_var = 4) 
#' rotate_manip_space(msp, theta = runif(1, max = 2 * pi), 
#'                    phi = runif(1, max = 2 * pi) )
rotate_manip_space <- function(manip_space, theta, phi) {
  # Initialize
  s_theta <- sin(theta)
  c_theta <- cos(theta)
  s_phi   <- sin(phi)
  c_phi   <- cos(phi)
  
  # Rotation matrix, [3, 3], a function of theta and phi.
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
  
  rotation_space <- manip_space %*% R
  colnames(rotation_space) <- colnames(manip_space)
  rownames(rotation_space) <- rownames(manip_space)
  
  rotation_space
}

#' Produce the series of projection bases to rotate a variable into and out 
#' of a projection
#'
#' Typically called by `array2af()`. An array of projections, 
#' the manual tour of the `manip_var`, which is rotated from phi's starting 
#' position to `phi_max`, to `phi_min`, and back to the start position.
#'
#' @param basis A (p, d) dim orthonormal matrix. Required, no default.
#' @param manip_var Integer column number or string exact column name of the.
#'   variable to manipulate. Required, no default.
#' @param manip_type String of the type of manipulation to use. 
#'   Defaults to "radial". Alternatively accepts "horizontal" or "vertical". 
#'   Yields to `theta` if set. Must set either `manip_type` or `theta`.
#' @param theta Angle in radians of "in-plane" rotation, on the XY plane of the 
#'   reference frame. Typically set from manip_type in `proj_data`. Supersedes 
#'   `manip_type`. Must set either `manip_type` or `theta`.
#' @param phi_min Minimum value phi should move to. Phi is angle in radians of 
#'   the "out-of-plane" rotation, the z-axis of the reference frame. 
#'   Required, defaults to 0.
#' @param phi_max Maximum value phi should move to. Phi is angle in radians of 
#'   the "out-of-plane" rotation, the z-axis of the reference frame. 
#'   Required, defaults to pi/2.
#' @param n_slides Number of slides to create. Defaults to 20.
#' @return A (p, d, n_slides) dim array of the manual tour. Containing
#'   `n_slides` interpolations varying phi from it's start to `phi_min`, to 
#'   `phi_max`, and back to start.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[,1:6])
#' 
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' manual_tour(basis = rb, manip_var = 4)
manual_tour <- function(basis = NULL,
                        manip_var,  # column number
                        theta = NULL,      # (radians)
                        phi_min = 0,       # (radians)
                        phi_max = .5 * pi, # (radians)
                        n_slides = 20
) {
  
  # Initalize
  basis <- as.matrix(basis)
  if (is.null(theta)) theta <- atan(basis[manip_var, 2] / basis[manip_var, 1])
  manip_space    <- create_manip_space(basis = basis, manip_var = manip_var)
  p              <- nrow(basis)
  d              <- ncol(basis)
  phi_start      <- acos(sqrt(basis[manip_var, 1]^2 + basis[manip_var, 2]^2)) *
    sign(manip_space[manip_var, 1])
  phi_min <- phi_min + phi_start # now relative to proj plane.
  phi_max <- phi_max + phi_start
  stopifnot(phi_min <= phi_start & phi_max >= phi_start)
  
  # Make "history_array", to be tourr::interpolate()'d. See tourr::save_history().
  hist <- array(NA, dim = c(p, d, 4))
  hist[,,1] <- manip_space[, 1:d]
  hist[,,2] <- rotate_manip_space(manip_space, theta, phi_min)[, 1:d]
  hist[,,3] <- rotate_manip_space(manip_space, theta, phi_max)[, 1:d]
  hist[,,4] <- hist[,,1]
  
  attr(hist, "manip_var") <- manip_var
  class(hist) <- c("history_array", class(hist))
  
  hist
}
