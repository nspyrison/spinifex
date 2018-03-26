#' Create a random basis
#'
#' Creates a random orthonormal basis of [p, d] dimensions
#'
#' @param p number of dimensions of the data
#' @param d number of dimensions of the basis. defaults to 2 for something 2-d projections
#' @export
basis_random <- function(p, d = 2) {
  mvn <- matrix(rnorm(p * d), ncol = d)
  basis <- qr.Q(qr(mvn)) #orthonormalize
  return(basis)
}

#' Create an indentity basis
#'
#' Creates an indentity basis of [p, d] dimensions. Indentity matrix, followed by 0s
#'
#' @param p number of dimensions of the data
#' @param d number of dimensions of the basis. defaults to 2 for something 2-d projections
#' @export
basis_identity <- function(p, d = 2){
  identity_basis <- rbind(diag(d), matrix(0, ncol = d, nrow = p - d))
  return(identity_basis)
}

#' Create a manipulation space.
#'
#' Create an orthonormal manipulation space from give [p, 2] basis to [p, 3] manipulation space
#'
#' @param basis orthonormal basis of [p, 2]
#' @param manip_var number of the variable to rotate
#' @export
create_manip_space <- function(basis = basis_random(p = 5, d = 2), # basis [p,2].
                               manip_var = 3){
  v <- rep(0, len = nrow(basis)) # 0 vector [p,1]
  v[manip_var] <- 1
  manip_space <- qr.Q(qr(cbind(basis, v)))
  # Q of the QR Decomposition, the orthonormalized manipulation space, [p,3].
  
  return(manip_space)
}

#' Rotate the manipulation space
#'
#' Rotates [p, 3] manipulation space by a [3, 3] rotation matrix into a [p, 3] rotated space
#'
#' @param manip_space data to project, must be compried of numeric variables (for coersersion into matrix)
#' @param theta angle1, atan(y_dist/x_dist)
#' @param phi angle2, "length of mouse region/size of plot region"
#' @export
rotate_manip_space <- function(manip_space, # manipulation space [p,3]
                               theta,
                               phi = 0){ # angle to rotate
  #theta   <- 0   # 0 for horizontal, pi/2 for vertical. #angular: atan(y_dist/x_dist)
  #phi     <- phi # horizontal, vertical as arg. #angular: h_dist / plot_size
  s_theta <- sin(theta)
  c_theta <- cos(theta)
  s_phi   <- sin(phi)
  c_phi   <- cos(phi)
  if (theta==0 & phi==0) cat("no rotation set!!!")
  
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
  
  r_space <- manip_space %*% R  # rotated (manipulation) space, [p,3] * [3,3] = [p,3]
  
  return(r_space)
}

#' Project data onto a rotated space
#'
#' Project data [n, p] onto a rotated space [p, 2]. Resulting in [n, 2] projection.
#'
#' @param data data to project, must be compried of numeric variables (for coersersion into matrix)
#' @param r_space rotation space, the output of `%_manipulations` 
#' @export
data_proj <- function(data = quakes,  # data [n,p]
                      r_space) {      # rotated space [p,2]
  if (!is.matrix(data)) data <- as.matrix(data)
  if (ncol(r_space) == 1)r_space <- cbind(r_space, rep(0, len = nrow(r_space))) 
  projected_data <- data %*% r_space  #rotated data [n,p] * [p,2] = [n,2]
  
  return(projected_data)
}

