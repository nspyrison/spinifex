#' Create a manipulation space
#'
#' Typically called by `manual_tour()`. Creates a (p, d) orthonormal matrix,
#' the manipulation space from the given basis right concatenated with a zero 
#' vector, with manip_var set to 1.
#'
#' @param basis A (p, d) orthonormal numeric matrix.
#' The linear combination the original variables contribute to projection space.
#' Required, no default.
#' @param manip_var Number of the column/dimension to rotate.
#' @return A (p, d+1) orthonormal matrix, the manipulation space.
#' @import tourr
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[,1:6])
#' 
#' rb <- basis_random(n = ncol(flea_std))
#' create_manip_space(basis = rb, manip_var = 4)
create_manip_space <- function(basis, 
                               manip_var) {
  if (is.null(basis) | nrow(basis) == 0) stop("non empty basis required.")
  if (!is.matrix(basis)) as.matrix(basis)
  
  manip_space <- cbind(basis, rep(0, len = nrow(basis) ))
  manip_space[manip_var, 3] <- 1
  
  manip_space  <- tourr::orthonormalise(manip_space)
  colnames(manip_space) <- NULL
  
  manip_space
}





#' Performs a rotation on the manipulation space
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
  R3 <- matrix(c(c_theta^2 * c_phi + s_theta^2,
                 -c_theta * s_theta * (1 - c_phi),
                 -c_theta * s_phi,                      # 3 of 9
                 -c_theta * s_theta * (1 - c_phi),
                 s_theta^2 * c_phi + c_theta^2,
                 -s_theta * s_phi,                      # 6 of 9
                 c_theta * s_phi,
                 s_theta * s_phi,
                 c_phi),                                # 9 of 9
               nrow = 3, ncol = 3, byrow = TRUE)
  rotated_space <- manip_space %*% R3
  
  ## Checks and formating
  stopifnot(spinifex::is_orthonormal(rotated_space))
  if (is.null(colnames(manip_space)) == FALSE) {
    colnames(rotated_space) <- colnames(manip_space)
  } else {colnames(rotated_space) <- paste0("proj_", 1:ncol(rotated_space))}
  if (is.null(rownames(manip_space)) == FALSE) {
    rownames(rotated_space) <- rownames(manip_space)
  } else {rownames(rotated_space) <- paste0("orig_", 1:nrow(rotated_space))}
  
  rotated_space
}




#' Produce the series of projection bases to rotate a variable into and out 
#' of a projection
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
#'   variable to manipulate. Required, no default.
#' @param theta Angle in radians of "in-plane" rotation, on the xy plane of the 
#'   reference frame. Defaults to theta of the basis for a radial tour.
#' @param phi_min Minimum value phi should move to. Phi is angle in radians of 
#'   the "out-of-plane" rotation, the z-axis of the reference frame. 
#'   Required, defaults to 0.
#' @param phi_max Maximum value phi should move to. Phi is angle in radians of 
#'   the "out-of-plane" rotation, the z-axis of the reference frame. 
#'   Required, defaults to pi/2.
#' @param angle Target distance (in radians) between steps. Defaults to .05.
#' @param ... Optionally passes arguments to `play_manual_tour()` and `play_tour_path()`
#' @return A (p, d, 4) history_array of the radial tour. The bases set for
#'   phi_start, `phi_min`, `phi_max`, and back to phi_start. To be called by
#'   `tourr::interpolate()`.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[, 1:6])
#' 
#' rb <- basis_random(n = ncol(flea_std))
#' manual_tour(basis = rb, manip_var = 4)
manual_tour <- function(basis   = NULL,
                        manip_var,
                        theta   = NULL,
                        phi_min = 0,
                        phi_max = .5 * pi,
                        angle   = .05,
                        ...) {
  ## === Test setup:
  # f<-tourr::rescale(tourr::flea[,1:6]);rb<-basis_random(n=ncol(flea_std));
  # basis = rb; manip_var=4;
  # basis = NULL;theta = NULL;phi_min = 0;phi_max = .5 * pi;angle   = .05;
  ## === End test setup
  # Initalize
  if (!is.matrix(basis)) basis <- as.matrix(basis)
  p <- nrow(basis)
  d <- ncol(basis)
  manip_space <- create_manip_space(basis = basis, manip_var = manip_var)
  mv_sp <- manip_space[manip_var, ]
  if (is.null(theta)) {
    theta <- atan(basis[manip_var, 2] / basis[manip_var, 1])
    ### Was trying to incorprate the below this after CRAN ver, but not working. 
    # ang_minor <- atan(mv_sp[2] / mv_sp[1])
    # offset <- 270 + sign(mv_sp[1]) * 90
    # theta <- (offset + sign(mv_sp[1]) * sign(mv_sp[2]) * ang_minor) %% 360
  }
  phi_start <- acos(sqrt(basis[manip_var, 1]^2 + basis[manip_var, 2]^2))
  stopifnot(phi_min <= phi_start & phi_max >= phi_start)
  
  ## Find the values of phi for each 'leg' (direction of motion)
  phi_segment <- function(start, end) {
    mvar_xsign <- -sign(basis[manip_var, 1])
    start <- mvar_xsign * (start - phi_start)
    end   <- mvar_xsign * (end   - phi_start)
    dist  <- abs(end - start)
    remainder <- dist %% angle
    sign  <- ifelse(end > start, 1, -1)
    
    segment <- seq(from = start, to = end - remainder, by = sign * angle)
    if (remainder != 0) segment <- c(segment, end) ## If Add remaining partial step less than a full step to the end
    
    segment
  }
  
  phi_path <- c(phi_segment(start = phi_start, end = phi_min),
                phi_segment(start = phi_min,   end = phi_max),
                phi_segment(start = phi_max,   end = phi_start))
  
  ## Make projected basis array
  n_frames <- length(phi_path)
  
  basis_set <- array(NA, dim = c(p, d, n_frames))
  for (i in 1:n_frames) {
    thisFrame <- rotate_manip_space(manip_space = manip_space, theta = theta,
                                    phi = phi_path[i])
    basis_set[,, i] <- thisFrame[, 1:d]
  }
  
  attr(basis_set, "manip_var") <- manip_var
  
  basis_set
}

