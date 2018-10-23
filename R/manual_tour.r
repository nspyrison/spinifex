#' Create a manipulation space
#'
#' Create a [p, d+1=3] dim orthonormal manipulation space from the given
#' basis concatonated with a zero vector, with manip_var set to 1.
#'
#' @param basis A [p, d=2] dim orthonormal basis.
#' @param manip_var Number of the dimension (numeric variable) to rotate.
#' @return manip_space, a [p, d+1=3] orthonormal manipulation space
#' @export
#' @examples
#' data(flea)
#' flea_std <- tourr::rescale(flea[,1:6])
#' 
#' rb <- tourr::basis_random(n=ncol(flea_std))
#' create_manip_space(basis = rb, manip_var = 4)
create_manip_space <- function(basis, manip_var) {
  stopifnot(class(basis) %in% c("matrix", "data.frame"))
  stopifnot(class(as.integer(manip_var)) != "integer" | 
              class(manip_var) != "character")  
  
  z <- rep(0, len = nrow(basis))
  z[manip_var] <- 1
  manip_space  <- tourr::orthonormalise(cbind(basis, z))
  if (ncol(manip_space) == 3) {colnames(manip_space) <- c("x","y","z")}
  if (ncol(manip_space) == 4) {colnames(manip_space) <- c("x","y","z","w")}
  rownames(manip_space) <- colnames(basis)
  
  return(manip_space)
}

#' Rotate and return the manipulation space
#'
#' This function does the manual rotation work.
#' Rotates a [p, 3] basis matrix manipulation space by [3, 3] rotation, 
#' returning [p, 3] orthonormal matrix, XYZ components after rotation.
#' This matrix is used to make the data projection.
#'
#' @param manip_space A [p, 3] dim manipulation space to be rotated.
#' @param theta Angle in radians to rotation w.r.t. the XY reference frame. 
#' Typically set from manip_type in proj_data().
#' @param phi Angle in radians corresponding to the magnitude of rotation.
#' @return r_space, a [p, 3] dim rotated space.
#' @export
#' @examples
#' data(flea)
#' flea_std <- tourr::rescale(flea[,1:6])
#' 
#' rb <- tourr::basis_random(n=ncol(flea_std))
#' msp <- create_manip_space(rb, 4) 
#' rotate_manip_space(msp, theta = runif(1, max = 2 * pi), 
#'                    phi = runif(1, max = 2 * pi) )
rotate_manip_space <- function(manip_space, theta, phi){
  stopifnot(ncol(manip_space) == 3)
  stopifnot(is.matrix(manip_space))
  
  s_theta <- sin(theta)
  c_theta <- cos(theta)
  s_phi   <- sin(phi)
  c_phi   <- cos(phi)
  
  # Rotation matrix, R dim of [3, 3], a function of theta and phi.
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
  
  return(rotation_space)
}

#' Produce the series of porjection bases to rotate a variable into and out 
#'   of a projection
#'
#' Rotates the manipulation space across `n_slides` increments to `phi_max`
#' and back to `phi_min`.
#'
#' @param basis A [p, 2] dim orthonormal starting basis. Required no default.
#' @param manip_var Integer column number or string exact column name of the.
#'   variable to manipulate. Required, no default.
#' @param manip_type String of the type of manipulation to use. 
#'   Defaults to "radial". Alternatively accepts "horizontal" or "vertical". 
#'   Required, supersedes theta if set.
#' @param theta Optional parameter, yielding to manip_type. Angle in radians 
#'   specifying the angle between the `manip_var` and 0 on bases reference frame. 
#'   Where the 0 is the positive side of the x-axis.
#' @param phi_min Angle in radians specifying the minimum extent that 
#' `manip_var` should extend in the z-axis w.r.t. the bases reference frame. 
#' Required, defaults to 0.
#' @param phi_max Angle in radians specifying the maximum extent that 
#'   `manip_var` should extend in the z-axis w.r.t. the bases reference frame. 
#'   Required, defaults to .5 * pi.
#' @param n_slides Number of slides to create for slideshow(). Defaults to 20.
#' @return `m_tour`, a [p, d, n_slides] dim array of the manual tour
#'   transistion in `n_slides``increments.
#' @export
#' @examples
#' data(flea)
#' flea_std <- tourr::rescale(flea[,1:6])
#' 
#' rb <- tourr::basis_random(n=ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
manual_tour <- function(basis = NULL,
                        manip_var = NULL,
                        manip_type = "radial", #alt: "horizontal" and "vertical"
                        theta = NULL,      # [radians]
                        phi_min = 0,       # [radians]
                        phi_max = pi,      # [radians]
                        n_slides = 20
                        ) { 
  # Assertions
  stopifnot(is.matrix(basis))
  stopifnot(nrow(basis) > 2)
  stopifnot(!is.null(manip_var))
  stopifnot(manip_type %in% c("radial", "horizontal", "vertical") )
  
  # Handle manip_var
  if (is.numeric(manip_var)) stopifnot(manip_var <= nrow(basis) )
  
  # Handle manip_type and theta
  if (!is.null(manip_type)) manip_type <- tolower(manip_type)
  if (!is.null(theta) & !is.null(manip_type) )
    message("Non-null theta used with non-null manip_type. Selecting theta over manip_type.")
  if (manip_type == "horizontal") theta <- 0
  if (manip_type == "vertical") theta <- pi / 2
  if (manip_type == "radial")
    theta <- atan(basis[manip_var, 2] / basis[manip_var, 1])
  phi_start <- acos(sqrt(basis[manip_var, 1]^2 + basis[manip_var, 2]^2))
  
  # Initalize and create a sequence of projection bases
  manip_space <- create_manip_space(basis = basis, manip_var = manip_var)
  p <- nrow(basis) 
  d <- ncol(basis) 
  m_tour <- array(dim=c(p, d, n_slides))
  slide <- 0
  new_slide <- NULL
  phi_inc = 2 * abs(phi_max - phi_min) / (n_slides-3)
  phi_vect = NULL
  
  interpolate_slide <- function(phi){
    slide <<- slide + 1
    new_slide <- rotate_manip_space(manip_space, theta, phi)
    new_slide[,1] <- new_slide[,1] - mean(new_slide[,1])
    new_slide[,2] <- new_slide[,2] - mean(new_slide[,2])
    m_tour[,,slide] <<- new_slide[,1:2]
    phi_vect <<- rbind(phi_vect, phi)
  }
  
  ## walk 1: from phi=phi_start to phi=0
  for (phi in seq(phi_start, phi_min, by = -1 * phi_inc) ) {
    interpolate_slide(phi)
  }
  ## walk 2: from phi=0 to phi=pi/2
  for (phi in seq(phi_min, phi_max, by = phi_inc) ) {
    interpolate_slide(phi)
  }
  ## walk 3: from phi=pi/2 to phi=phi_start
  for (phi in seq(phi_max, phi_start, by = -1 * phi_inc) ) {
    interpolate_slide(phi)
  }
  interpolate_slide(phi_start)
  
  # Add tour attributes
  attr(m_tour, "manip_var")  <- manip_var
  
  return(m_tour)
}