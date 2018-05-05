#' Create a random basis
#'
#' Creates a [p, d=2] random orthonormal basis
#'
#' @param p number of dimensions of the data
#' @param d number of dimensions of the basis.Defaults to 2.
#' @return orthonormalized matrix, the [p, d=2] basis
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
#' @return [p, d=2] indentity matrix followed by 0s
#' @export
#' 
basis_identity <- function(p, d = 2){
  identity_basis <- rbind(diag(d), matrix(0, ncol = d, nrow = p - d))
  return(identity_basis)
}

#' Draw tour axes with base graphics
#' 
#' For internal use mainly
#' 
#' @export
draw_basis_axes <- function(proj, labels, limits, position) {
  position <- match.arg(position, c("center", "bottomleft", "off"))
  if (position == "off") return()
  
  if (position == "center") {
    axis_scale <- 2 * limits / 3
    axis_pos <- 0
  } else if (position == "bottomleft") {
    axis_scale <- limits / 6
    axis_pos <- -2/3 * limits
  }
  
  adj <- function(x) axis_pos + x * axis_scale
  
  segments(adj(0), adj(0), adj(proj[, 1]), adj(proj[, 2]), col="grey50")
  theta <- seq(0, 2 * pi, length = 50)
  lines(adj(cos(theta)), adj(sin(theta)), col = "grey50")
  text(adj(proj[, 1]), adj(proj[, 2]), label = labels, col = "grey50")
}

#' Draw tour axes with base graphics
#' 
#' For internal use mainly
#' 
#' @export
basis_help <- function(basis) {
  
  tmp <- as.data.frame(basis)
  tmp <- cbind(tmp, sqrt(tmp[,1]^2 + tmp[,2]^2), atan(tmp[,2]/tmp[,1]))
  colnames(tmp) <- c("X", "Y", "H", "theta")
  basis_help <- tmp
  
  return(basis_help)
}

