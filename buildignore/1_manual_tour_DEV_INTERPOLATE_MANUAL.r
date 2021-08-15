##
## MANUAL TOUR WORK HORSES -----
##

#' Create a manipulation space to rotate the manipulation variable in.
#'
#' Typically called by `manual_tour()`. Creates a (p, d) orthonormal matrix,
#' the manipulation space from the given basis right concatenated with a zero 
#' vector, with `manip_var` set to 1.
#'
#' @param basis A (p, d) orthonormal numeric matrix,
#' the linear combination the original variables contribute to projection frame.
#' Required, no default.
#' @param manip_var The number of the variable/column to rotate. Defaults to 
#' `manip_var_of(basis)`, the variable with the largest contribution in the basis.
#' @return A (p, d + 1) orthonormal matrix, the manipulation space to 
#' manipulate the projection in.
#' @import tourr
#' @export
#' @examples
#' ## Setup
#' dat_std <- scale_sd(wine[, 2:6])
#' bas <- basis_pca(dat_std)
#' mv <- manip_var_of(bas)
#' create_manip_space(basis = bas, manip_var = mv)
#' 
#' ## d = 1 case 
#' bas1d <- basis_pca(dat_std, d = 1)
#' mv <- manip_var_of(bas1d)
#' create_manip_space(bas1d, mv)
create_manip_space <- function(basis, manip_var = manip_var_of(basis)){
  ## Assumptions
  basis <- as.matrix(basis)
  if(spinifex::is_orthonormal(basis) == FALSE){
    warning("Basis was not orthonormal. Coereced to othronormal with tourr::orthonormalise(basis).")
    basis <- tourr::orthonormalise(basis)
  }
  if(ncol(basis) > 2L){ warning(paste0(
    "Basis of d = ", ncol(basis),
    " used. Spinifex is only implemented for d = 1 | 2 at the momment. The basis as been truncated to 2 dimensions."))
    basis <- basis[, 1L:2L]
  }
  
  ## Add manip variable and orthonormalize
  manip_space <- cbind(basis, rep(0L, len = nrow(basis)))
  manip_space[manip_var, ncol(manip_space)] <- 1L
  manip_space <- tourr::orthonormalise(manip_space)
  
  ## Conserve col/row names
  cn <- colnames(basis)
  if(is.null(cn) == TRUE)
    cn <- paste0("y", 1L:ncol(basis))
  colnames(manip_space) <- c(cn, "manip_sp")
  rn <- rownames(basis)
  if(is.null(cn) == TRUE)
    rn <- paste0("x", 1L:nrow(basis))
  rownames(manip_space) <- rn
  
  return(manip_space)
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
#' ## Setup
#' dat_std <- scale_sd(wine[, 2:6])
#' bas <- basis_pca(dat_std)
#' mv <- manip_var_of(bas)
#' msp <- create_manip_space(basis = bas, manip_var = mv)
#' rotate_manip_space(msp, theta = runif(1, max = 2 * pi),
#'                    phi = runif(1, max = 2 * pi))
#' 
#' ## d = 1 case 
#' bas1d <- basis_pca(dat_std, d = 1)
#' mv <- manip_var_of(bas1d)
#' msp <- create_manip_space(bas1d, mv)
#' rotate_manip_space(msp, phi = runif(1, max = 2 * pi))
rotate_manip_space <- function(manip_space, theta, phi) {
  ## Assumptions
  manip_space <- as.matrix(manip_space)
  if(spinifex::is_orthonormal(manip_space) == FALSE){
    warning("manip_space was not orthonormal. Coereced to othronormal with tourr::orthonormalise(manip_space).")
    manip_space <- tourr::orthonormalise(manip_space)
  }
  
  ### d = 2 case ###
  if(ncol(manip_space) == 3L){
    ## Initialize
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
  }
  ### d = 1 case ###
  if(ncol(manip_space) == 2L){
    ## Initialize
    s_phi   <- sin(phi)
    c_phi   <- cos(phi)
    ## 3D rotation matrix, as a function of theta and phi.
    R2 <- matrix(c(c_phi, -s_phi,
                   s_phi, c_phi),
                 nrow = 2L, ncol = 2L, byrow = TRUE)
    rotated_space <- manip_space %*% R2
  }
  
  ## Conserve colnames (rownames already the same)
  cn <- colnames(manip_space)
  if(is.null(cn) == TRUE)
    cn <- c(paste0("y", 1L:(ncol(manip_space) - 1L)), "manip_sp")
  colnames(rotated_space) <- cn
  
  return(rotated_space)
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
#' @param angle Target angle (in radians) interpolation steps. Defaults to .05.
#' @param data Optionally attach data to the basis path.
#' @param ... Terminate unused arguments that are being also being passed from 
#' `play_manual_tour()` to `render_()`.
#' @return A (p, d, 4) history_array of the radial tour. The bases set for
#' phi_start, `phi_min`, `phi_max`, and back to phi_start.
#' @export
#' @examples
#' ## Setup
#' dat_std <- scale_sd(wine[, 2:6])
#' clas <- wine$Type
#' bas <- basis_pca(dat_std)
#' mv <- manip_var_of(bas)
#' 
#' ## Required arguments
#' manual_tour(basis = bas, manip_var = mv)
#' 
#' ## All arguments
#' manual_tour(basis = bas, manip_var = mv,
#'             theta = pi / 2, phi_min = pi / 16, phi_max = pi, angle = .2)
#' 
#' ## d = 1 case
#' bas1d <- basis_pca(dat_std, d = 1)
#' mv <- manip_var_of(bas1d)
#' manual_tour(basis = bas1d, manip_var = mv, angle = .2)
#' 
#' 
#' ## Animating with ggtour() & proto_*
#' mt <- manual_tour(basis = bas, manip_var = mv)
#' (ggt <- ggtour(mt, dat_std) +
#'     proto_origin() +
#'     proto_point(list(color = clas, shape = clas)) +
#'     proto_basis())
#' # proto_default(list(color = clas, shape = clas)))
#' \dontrun{
#' animate_plotly(ggt)
#' }
manual_tour <- function(basis,
                        manip_var,
                        theta   = NULL,
                        phi_min = 0L,
                        phi_max = pi / 2L,
                        angle   = .05,
                        data = NULL,
                        ...){
  ## Assumptions
  basis <- as.matrix(basis)
  p <- nrow(basis)
  d <- ncol(basis)
  if(length(manip_var) != 1L | manip_var < 1L | manip_var > p)
    stop("manip_var expected as a single integer between 1 and nrow(basis).")
  if(spinifex::is_orthonormal(basis) == FALSE){
    warning("Basis was not orthonormal. Coereced to othronormal with tourr::orthonormalise(basis).")
    basis <- tourr::orthonormalise(basis)
  }
  
  ## Initialize
  ### d = 2 case
  if(d == 2L){
    if(is.null(theta))
      theta <- atan(basis[manip_var, 2L] / basis[manip_var, 1L])
    phi_start <- acos(sqrt(basis[manip_var, 1L]^2L + basis[manip_var, 2L]^2L))
  }
  ### d = 1 case
  if(d == 1L){
    phi_start <- acos(basis[manip_var, 1L])
    theta <- NA
  }
  ### shift phi start in be in-phase between [0, pi / 2]
  while(phi_start > pi / 2L){
    message("phi_start > pi; phi_start <- -phi_start + pi & phi_max <- -phi_max")
    phi_start <- -phi_start + pi
    phi_max <- -phi_max
  }
  while(phi_start < -pi / 2L) 
    message("phi_start < -pi / 2L; phi_start <- phi_start + pi")
    phi_start <- phi_start + pi
  ## Ensure correct order of phi_min, phi_start, phi_max
  if((abs(phi_min) < abs(phi_start)) == FALSE)
    stop("Phi is less than phi_min, please set phi_min below ", round(phi_start, 2L))
  if((abs(phi_max) > abs(phi_start)) == FALSE)
    stop("Phi is greather than phi_max, please set phi_max above ", round(phi_start, 2L))
  ## Terminate args meant for render_*/animate_* passed through play_manual_tour().
  .xArgs <- list(...)
  

  ## Find the phi values for the animation frames
  phi_vals_at_tgt_bases <-
    c(phi_start, phi_min,
      phi_min,   phi_max,
      phi_max,   phi_start) - phi_start
  n_frames <- length(phi_vals_at_tgt_bases)
  
  ## Make projected basis array
  m_sp <- create_manip_space(basis = basis, manip_var = manip_var)
  basis_array <- ## Init
    array(NA, dim = c(p, d, n_frames),
          dimnames = c(dimnames(basis), list(paste0("frame", 1L:n_frames))))
  ## populate tour basis_array
  .m <- sapply(1L:n_frames, function(i){
    this_proj <- rotate_manip_space(m_sp, theta, phi_path[i])
    basis_array[,, i] <<- this_proj[, 1L:d]
  })
  attr(basis_array, "manip_var") <- manip_var
  attr(basis_array, "data") <- data ## Can be Null
  
  return(basis_array)
}

#' ### OLD:
#' @param start WAS PHI current
#' @param end WAS Phi tgt
#' @param angle IMPLICITE in the parent functuion; WAS phi angle, the step size in radians.
#' ###NEW:
#' @param basis_array array, of the target bases, the extrema of the walk/segments.
#' @param manip_var The column number of the manipulation variable.
#' @param angle The step size between interpolated frames, in radians.
#' 
#' ## However, now we have the bases at the ends of the walks.
#' #### I think we want to calculate back to phi and apply similarly??
### Phi interpolation step -----
## Find the values of phi for each 'segment/leg/direction' of the walk
#interpolate_phi <- function(start, end, angle){
interpolate_manual <- function(basis_array, manip_var, angle){
  ## Initialize
  .dim <- dim(basis_array)
  interpolated_array <- array(NA, dim = .dim + c(0L, 0L, 100L))
  phi_of <- function(basis_mv_row)
    return(acos(sqrt(sum(basis_mv_row^2L))))
  .m <- sapply(1L:dim()[3L] - 1L, function(i){
    phi_curr <- phi_of(basis_array[,manip_var, i])
    phi_tgt  <- phi_of(basis_array[,manip_var, i + 1L])
    dist     <- phi_tgt - phi_curr ##TODO abs wanted? may help phi issue.
    remaind  <- dist %% angle
    phi_vect <- seq(phi_curr, phi_tgt - remaind, by = sign(dist) * angle)
    interp_segment <- array(NA, dim = c(.dim[1L:2L], length(phi_vect)))
    .m <- sapply(1L:length(phi_vect), function(j){
      rotate_manip_space(m_sp, theta, phi_path[i])
      interp_segment[,, j] <- 
    })

  })
 
  start     <- start - phi_start
  end       <- end   - phi_start
  dist      <- abs(end - start)
  remainder <- dist %% angle
  direction <- ifelse(end > start, 1L, -1L)
  ## Define segments
  segment <- seq(from = start, to = end - remainder, by = direction * angle)
  ## Add remaining partial step to the end if needed.
  # if(remainder != 0L) segment <- c(segment, end)

  ## C+P, values of phi goin gto basis,
  .m <- sapply(1L:n_frames, function(i){
    this_proj <- rotate_manip_space(m_sp, theta, phi_path[i])
    basis_array[,, i] <<- this_proj[, 1L:d]
  })

  ## Return
  return(segment)
}


## example of Phi issue -----
if(F){ ## NOT RUN:
  dat_std <- scale_sd(wine[, 2:6])
  clas <- wine$Type
  bas <- basis_pca(dat_std)
  mv <- manip_var_of(bas)
  mt <- manual_tour(basis = bas, manip_var = mv)
  ## proto_* api:
  (ggt <- ggtour(mt, dat_std) +
      proto_origin() +
      proto_point(list(color = clas, shape = clas)) +
      proto_basis())
  animate_plotly(ggt)
}