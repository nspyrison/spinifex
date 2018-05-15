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

    start <- matrix(0, nrow = p, ncol = d)
    (diag(start) <- 1)
    start
  identity_basis <- rbind(diag(d), matrix(0, ncol = d, nrow = p - d))
  return(identity_basis)
}


#' View basis axes and table
#' 
#' For internal use mainly 
#' 
#' @param basis [p, d=2] basis, xy contributions of the var. 
#' @param data optional, [n, p], applies colnames to the rows of the basis.
#' 
#' @export
view_basis <- function(basis, data = NULL) {
  
  tmp <- as.data.frame(basis)
  tmp <- cbind(tmp, sqrt(tmp[,1]^2 + tmp[,2]^2), atan(tmp[,2]/tmp[,1]))
  colnames(tmp) <- c("X", "Y", "H_xy", "theta")
  rownames(tmp) <- colnames(data)
  view_basis <- tmp
  
  plot(0,type='n',axes=FALSE,ann=FALSE,xlim=c(-1, 1), ylim=c(-1, 1),asp=1)
  segments(0,0, basis[, 1], basis[, 2], col="grey50")
  theta <- seq(0, 2 * pi, length = 50)
  lines(cos(theta), sin(theta), col = "grey50")
  text(basis[, 1], basis[, 2], label = colnames(data), col = "grey50")

  return(view_basis)
}


#' Draw tour axes in plotly
#' 
#' For internal use mainly
#' 
#' @param basis [p, d=2] basis, xy contributions of the var. 
#' @param data optional, [n, p], applies colnames to the rows of the basis.
#' 
#'DO NOT  @ export
 
#draw_tour_axes(proj, labels, limits = 1, axes)
draw_tour_axes <- function(basis, labels, limits, position) {
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
  
  segments(adj(0), adj(0), adj(basis[, 1]), adj(basis[, 2]), col="grey50")
  theta <- seq(0, 2 * pi, length = 50)
  lines(adj(cos(theta)), adj(sin(theta)), col = "grey50")
  text(adj(basis[, 1]), adj(basis[, 2]), label = labels, col = "grey50")
}


#' Check for orthonormality of a matrix (or basis)
#' 
#' For internal use mainly. returns T/F.
#' 
#' @param basis a matrix to check for orthonormality
#' 
#' @export
is_orthornormal <- function(basis) {
  mat <- basis
  mat_t <- t(mat)
  ans <- all.equal(mat_t %*% mat, diag(ncol(basis)))
  #if you want to see floating point error, ~e-16:
  #all.equal(mat_t %*% mat, diag(ncol(basis)), tol=0) 
  
  return(ans)
}

