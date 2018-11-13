#' Create a manipulation space
#'
#' Typically called by `manual_tour()`. Creates a [p, d] dim orthonormal matrix,
#' the manipulation space from the given basis right concatonated with a zero 
#' vector, with manip_var set to 1.
#'
#' @param basis A [p, d] dim orthonormal matrix.
#' @param manip_var Number of the dimension (numeric variable) to rotate.
#' @return A [p, d+1] orthonormal matrix, the manipulation space.
#' @export
#' @examples
#' data(flea)
#' flea_std <- tourr::rescale(flea[,1:6])
#' 
#' rb <- tourr::basis_random(n = n col(flea_std))
#' create_manip_space(basis = rb, manip_var = 4)
create_manip_space <- function(basis, manip_var) {
  if (!is.matrix(basis)) as.matrix(basis)
  stopifnot(class(manip_var) == "numeric")
  
  z            <- rep(0, len = nrow(basis))
  z[manip_var] <- 1
  manip_space  <- tourr::orthonormalise(cbind(basis, z))
  if (ncol(manip_space) == 3) {colnames(manip_space) <- c("x","y","z")}
  rownames(manip_space) <- colnames(basis)
  
  return(manip_space)
}

#' Rotate and return the manipulation space
#'
#' Typically called by `manual_tour()`. Rotates a [p, d+1] manipulation space 
#' matrix  by [3, 3] rotation matrix, returning [p, d+1] orthonormal matrix; 
#' the XYZ components of the rotation space. XY are the linear combination of
#' the variables for a 2d projection.
#'
#' @param manip_space A [p, d+1] dim manipulation space to be rotated.
#' @param theta Angle in radians of rotation "in-plane", on the XY plane of the 
#'   reference frame. Typically set from manip_type in proj_data().
#' @param phi Angle in radians of rotation "out-of-plane", the z axis of the 
#'   reference frame. Effectively changes the norm of XY contributions of the 
#'   manip_var.
#' @return A [p, d+1] orthonormal matrix of the rotated (manipulation) space.
#' @export
#' @examples
#' data(flea)
#' flea_std <- tourr::rescale(flea[,1:6])
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
  
  return(rotation_space)
}

#' Produce the series of porjection bases to rotate a variable into and out 
#' of a projection
#'
#' Typically called by `create_slides()`. The manual tour of the `manip_var`.
#' Given a [p, d] orthonormal basis, creates an array of `n_slides` bases 
#' extending the norm of `manip_var`, via cos(phi), from `phi_max`, to 
#' `phi_min`, then back to the starting position (by default: from start, to 0,
#' to 1, to start).
#'
#' @param basis A [p, d] dim orthonormal matrix. Required, no default.
#' @param manip_var Integer column number or string exact column name of the.
#'   variable to manipulate. Required, no default.
#' @param manip_type String of the type of manipulation to use. 
#'   Defaults to "radial". Alternatively accepts "horizontal" or "vertical". 
#'   Yields to `theta` if set. Must set either `manip_type` or `theta`.
#' @param theta Angle in radians of "in-plane" rotation, on the XY plane of the 
#'   reference frame. Typically set from manip_type in proj_data(). Supersedes 
#'   `manip_type`. Must set either `manip_type` or `theta`.
#' @param phi_min Minimun value phi should move to. Phi is angle in radians of 
#'   the "out-of-plane" rotation, the z-axis of the referce frame. 
#'   Required, defaults to 0.
#' @param phi_max Maximum value phi should move to. Phi is angle in radians of 
#'   the "out-of-plane" rotation, the z-axis of the referce frame. 
#'   Required, defaults to 2 * pi.
#' @param n_slides Number of slides to create for slideshow(). Defaults to 20.
#' @return A [p, d, n_slides] dim array of the manual tour. Containing
#'   `n_slides` interpolations varying phi from it's start to `phi_min`, to 
#'   `phi_max`, and back to start.
#' @export
#' @examples
#' data(flea)
#' flea_std <- tourr::rescale(flea[,1:6])
#' 
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
manual_tour <- function(basis = NULL,
                        manip_var = NULL,
                        manip_type = "radial", #alt: "horizontal" and "vertical"
                        theta = NULL,      # [radians]
                        phi_min = 0,       # [radians]
                        phi_max = .5 * pi, # [radians]
                        n_slides = 20) { 
  manip_type <- tolower(manip_type)
  if (!is.matrix(basis)) basis <- as.matrix(basis)
  # Assertions
  stopifnot(is.matrix(basis))
  stopifnot(nrow(basis) > 2)
  stopifnot(!is.null(manip_var))
  stopifnot(manip_type %in% c("radial", "horizontal", "vertical") |
              !is.null(theta) )
  
  # Handle args
  if (is.character(manip_var)) 
    manip_var <- match(manip_var, colnames(data)) # char to num
  if (!is.numeric(manip_var)) 
    stop("manip_var string not matched to a column name, try a column number.")
  stopifnot(manip_var <= nrow(basis))
  if (!is.null(theta) & !is.null(manip_type) )
    message("Non-null theta used with non-null manip_type. Selecting theta over manip_type.")
  if (is.null(theta) & manip_type == "horizontal") theta <- 0
  if (is.null(theta) & manip_type == "vertical")   theta <- pi/2
  if (is.null(theta) & manip_type == "radial")
    theta <- atan(basis[manip_var, 2] / basis[manip_var, 1])
  
  # Initalize
  phi_start   <- acos(sqrt(basis[manip_var, 1]^2 + basis[manip_var, 2]^2))
  walk_min    <- min(phi_min, phi_max, phi_start)
  walk_max    <- max(phi_min, phi_max, phi_start)
  phi_inc     <- 2 * abs(walk_max - walk_min) / (n_slides - 3)
  phi_min_rel <- phi_min - phi_start # relative to phi_start
  phi_max_rel <- phi_max - phi_start # relative to phi_start
  manip_space <- create_manip_space(basis = basis, manip_var = manip_var)
  p           <- nrow(basis)
  d           <- ncol(basis)
  if (phi_start < phi_min) warning("phi_start less than phi_min, tour may look odd.")
  if (phi_start > phi_max) warning("phi_start greater than phi_max, tour may look odd.")
  
  interpolate_slides <- function(seq_start, seq_end){
    # Initalize for interpolate_slides()
    slide         <- 0
    new_slide     <- NULL
    seq_by_sign   <- ifelse(seq_end > seq_start, 1, -1)
    phi_inc_sign  <- seq_by_sign * phi_inc
    len           <- length(seq(seq_start, seq_end, phi_inc_sign))
    interpolation <- array(dim = c(p, d, len))
    
    for (phi in seq(seq_start, seq_end, phi_inc_sign)) {
      slide     <- slide + 1
      new_slide <- rotate_manip_space(manip_space, theta, phi)
      interpolation[,, slide] <- new_slide[, 1:2]
    }
    return(interpolation)
  }
  
  x_mvar_sign    <- sign(manip_space[manip_var, 1])
  phi_start_sign <- x_mvar_sign * phi_start
  ## walk 1: start to near 0
  walk1 <- interpolate_slides(phi_start_sign, phi_min_rel)
  ## walk 2: 0 to near 1
  walk2 <- interpolate_slides(phi_min_rel, phi_max_rel)
  ## walk 3: 1 to near start
  walk3 <- interpolate_slides(phi_max_rel, phi_start_sign)
  ## Add 1 last slide at start
  walk4 <- interpolate_slides(phi_start_sign, phi_start_sign)
  
  m_tour <- array(c(walk1, walk2, walk3, walk4), dim = c(p, d, n_slides))
  attr(m_tour, "manip_var") <- manip_var
  
  return(m_tour)
}