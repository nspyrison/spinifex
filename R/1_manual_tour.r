#' Create a manipulation space to rotate the manip variable in.
#'
#' Typically called by `manual_tour()`. Creates a (p, d) orthonormal matrix,
#' the manipulation space from the given basis right concatenated with a zero 
#' vector, with manip_var set to 1.
#'
#' @param basis A (p, d) orthonormal numeric matrix,
#' the linear combination the original variables contribute to projection frame.
#' Required, no default.
#' @param manip_var The number of the variable/column to rotate.
#' @return A (p, d + 1) orthonormal matrix, the manipulation space to 
#' manipulate the projection in.
#' @import tourr
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[, 1:6])
#' 
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' create_manip_space(basis = rb, manip_var = 4)
create_manip_space <- function(basis, manip_var) {
  ## Assumptions
  basis <- as.matrix(basis)
  stopifnot(spinifex::is_orthonormal(basis))
  if(ncol(basis) >= 3){
    warning(paste0("Basis of d = ", ncol(basis), 
                   " used. Spinifex is only implemented for d = 2 at the momment. The basis as been truncated to 2 dimensions."))
    basis <- basis[, 1L:2L]
  }
  ## Add manip variable and orthonormalize
  manip_space <- cbind(basis, rep(0L, len = nrow(basis)))
  manip_space[manip_var, ncol(manip_space)] <- 1L
  manip_space <- tourr::orthonormalise(manip_space)
  ## Check
  stopifnot(spinifex::is_orthonormal(manip_space))
  manip_space
}



#' Performs a rotation on the manipulation space of the given manip var.
#'
#' A specific R3 rotation of the manipulation space for a 2D tour.
#' Typically called by `manual_tour()`. The first 2 columns are x and y in 
#' the projection plane. The 3rd column extends "in the z-direction" orthogonal 
#' to the projection plane.
#'
#' @param manip_space A (p, d+1) dim matrix (manipulation space) to be rotated.
#' @param theta Angle (radians) of "in-projection-plane" rotation (ie. on xy-
#' of the projection). Typically set by the manip_type argument in `proj_data()`.
#' @param phi Angle (radians) of "out-of-projection-plane" rotation (ie. into
#' the z-direction of the manipulation space. Effectively changes the norm 
#' of the manip_var in the projection plane.
#' @return A (p, d+1) orthonormal matrix of the rotated (manipulation) space. 
#' The first 2 columns are x and y in the projection plane. The 3rd column 
#' extends "in the z-direction" orthogonal to the projection plane.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[, 1:6])
#' rb  <- tourr::basis_random(n = ncol(flea_std))
#' msp <- create_manip_space(basis = rb, manip_var = 4)
#' 
#' rotate_manip_space(msp, theta = runif(1, max = 2 * pi), 
#'                    phi = runif(1, max = 2 * pi) )
rotate_manip_space <- function(manip_space, theta, phi) {
  stopifnot(spinifex::is_orthonormal(manip_space))
  ## Initalize
  s_theta <- sin(theta)
  c_theta <- cos(theta)
  s_phi   <- sin(phi)
  c_phi   <- cos(phi)
  
  ## 3D rotation matrix, as a function of theta and phi.
  R3 <- matrix(c(c_theta^2L * c_phi + s_theta^2L,
                 -c_theta * s_theta * (1L - c_phi),
                 -c_theta * s_phi,                  # 3 of 9
                 -c_theta * s_theta * (1 - c_phi),
                 s_theta^2L * c_phi + c_theta^2L,
                 -s_theta * s_phi,                  # 6 of 9
                 c_theta * s_phi,
                 s_theta * s_phi,
                 c_phi),                            # 9 of 9
               nrow = 3L, ncol = 3L, byrow = TRUE)
  rotated_space <- manip_space %*% R3
  
  ## Checks and formating
  stopifnot(spinifex::is_orthonormal(rotated_space))
  colnames(rotated_space) <- paste0("proj_", 1L:ncol(rotated_space))
  rownames(rotated_space) <- paste0("V", 1L:nrow(rotated_space))
  
  ## Return 
  rotated_space
}



#' Produce the series of projection bases to rotate a variable into and out 
#' of a projection.
#'
#' Typically called by `array2af()`. An array of projections, 
#' the radial tour of the `manip_var`, which is rotated from phi's starting 
#' position to `phi_max`, to `phi_min`, and back to the start position.
#'
#' @name manual_tour
#' @param basis A (p, d) orthonormal numeric matrix. 
#' The linear combination the original variables contribute to projection space.
#' Defaults to NULL, generating a random basis.
#' @param manip_var Integer column number or string exact column name of the.
#' variable to manipulate. Required, no default.
#' @param theta Angle in radians of "in-plane" rotation, on the xy plane of the 
#' reference frame. Defaults to theta of the basis for a radial tour.
#' @param phi_min Minimum value phi should move to. Phi is angle in radians of 
#' the "out-of-plane" rotation, the z-axis of the reference frame. 
#' Required, defaults to 0.
#' @param phi_max Maximum value phi should move to. Phi is angle in radians of 
#' the "out-of-plane" rotation, the z-axis of the reference frame. 
#' Required, defaults to pi/2.
#' @param angle Target distance (in radians) between steps. Defaults to .05.
#' @param ... Handles unused arguments that are being also being passed from 
#' `play_manual_tour()` to `render_()`.
#' @return A (p, d, 4) history_array of the radial tour. The bases set for
#' phi_start, `phi_min`, `phi_max`, and back to phi_start.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[, 1:6])
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' 
#' manual_tour(basis = rb, manip_var = 4)
#' 
#' manual_tour(basis = rb, manip_var = 6, 
#'             theta = pi / 2, phi_min = pi / 16, phi_max = pi, angle = .1)
manual_tour <- function(basis,
                        manip_var,
                        theta   = NULL,
                        phi_min = 0L,
                        phi_max = .5 * pi,
                        angle   = .05,
                        ...) {
  ## Initalize
  basis <- as.matrix(basis)
  xArgs <- list(...) ## Terminate args meant for `render_()` also passed in `play_manual_tour()`.
  .theta <- theta
  if (is.null(.theta))
    .theta <- atan(basis[manip_var, 2L] / basis[manip_var, 1L])
  phi_start <- acos(sqrt(basis[manip_var, 1L]^2L + basis[manip_var, 2L]^2L))
  stopifnot(phi_min <= phi_start & phi_max >= phi_start)
  
  ## Find the values of phi for each 'leg' (direction of motion)
  phi_segment  <- function(start, end){
    ## Initalize
    mvar_xsign <- -sign(basis[manip_var, 1L])
    start      <- mvar_xsign * (start - phi_start)
    end        <- mvar_xsign * (end   - phi_start)
    dist       <- abs(end - start)
    remainder  <- dist %% angle
    sign       <- ifelse(end > start, 1L, -1L)
    ## Define segments
    segment <- seq(from = start, to = end - remainder, by = sign * angle)
    ## Add remaining partial step to the end if needed.
    if (remainder != 0L) segment <- c(segment, end)
    ## Return
    segment
  }
  
  ## Find the phi values for the animation frames
  phi_path <- c(phi_segment(start = phi_start, end = phi_min),
                phi_segment(start = phi_min,   end = phi_max),
                phi_segment(start = phi_max,   end = phi_start))
  
  ## Make projected basis array
  n_frames <- length(phi_path)
  i_s  <- 1L:n_frames
  p    <- nrow(basis)
  d    <- 2L #ncol(basis) ## d fixed to 2 atm.
  m_sp <- create_manip_space(basis = basis, manip_var = manip_var)
  basis_set <- array(NA, dim = c(p, d, n_frames))
  for(i in i_s){
    thisProj <-
      rotate_manip_space(manip_space = m_sp, theta = .theta, phi = phi_path[i])
    basis_set[,, i] <- thisProj[, 1L:2L]
  }
  attr(basis_set, "manip_var") <- manip_var
  
  ## Return
  basis_set
}

