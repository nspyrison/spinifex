#' Create a random basis
#'
#' Creates a [p, d=2] random orthonormal basis
#'
#' @param p number of dimensions of the data
#' @param d number of dimensions of the basis. defaults to 2 for something 2-d projections
#' @export
#' 
basis_random <- function(p, d = 2) {
  mvn <- matrix(rnorm(p * d), ncol = d)
  basis <- qr.Q(qr(mvn)) #orthonormalize
  return(basis)
}


#' Create an indentity basis
#'
#' Creates a [p, d=2] indentity basis; indentity matrix followed by 0s
#'
#' @param p number of dimensions of the data
#' @param d number of dimensions of the basis, defaulting to 2
#' @export
#' 
basis_identity <- function(p, d = 2){
  identity_basis <- rbind(diag(d), matrix(0, ncol = d, nrow = p - d))
  return(identity_basis)
}


#' Create a manipulation space
#'
#' Create an [p, 3] orthonormal manipulation space from the given [p, 2] orthonormal basis
#'
#' @param basis [p, 2] orthonormal basis
#' @param manip_var number of the variable to rotate
#' @export
#' 
create_manip_space <- function(basis = basis_random(p = 5, d = 2), manip_var = 1){
  v <- rep(0, len = nrow(basis))
  v[manip_var] <- 1
  manip_space <- qr.Q(qr(cbind(basis, v)))
    #Q of the QR Decomposition, the orthonormalized manipulation space, [p,3].
  return(manip_space)
}


#' Rotate the manipulation space
#'
#' Rotates [p, 3] manipulation space by [3, 3] rotation matrix into a [p, 3] rotated space
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
  return(r_space)
}


#' Project data onto a rotated space
#'
#' Project [n, p] data onto a [p, 3] rotated manipulation space into the [n, 3] projection
#'
#' @param data [n, p] data to project, consisting of only numeric variables (for coercion into matrix)
#' @param r_space [p, 3] rotated manipulation space 
#' @export
#' @examples
#' this_r_space <- horizontal_manip(manip_space = this_manip_space, phi = i)
#' proj <- data_proj(data = quakes, r_space = this_r_space)
#' plot(proj[,1], proj[,2], main="Projected data")
#' 
#' for (i in seq(0, pi, pi/20)) {
#' this_r_space <- horizontal_manip(manip_space = this_manip_space, phi = i)
#' proj <- data_proj(data = quakes, r_space = this_r_space)
#' plot(proj[,1], proj[,2], main="Projected data")
#' Sys.sleep(time=.5)
#' print(i/pi)
#' }
#' 
data_proj <- function(data = quakes, r_space) {
  if (!is.matrix(data)) data <- as.matrix(data)
  projected_data <- data %*% r_space
  return(projected_data)
}

