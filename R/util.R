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


#' Project data onto a rotated space
#'
#' Project [n, p] data onto a [p, 3] rotated manipulation space into the [n, 3] projection. Stores the data projection(s) in a data structure for ploting
#'
#' @param data [n, p] data to project, consisting of only numeric variables (for coercion into matrix)
#' @param r_space [p, 3] rotated manipulation space 
#' @export
#' @examples
#' REXAMPLE!!!
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
data_proj <- function(data, manip=radial_manip(),
                      from = 0, to = 0, by = pi / 20) {
  tc_seq <- tryCatch(seq(from, to, by)
                     ,error=function(e) e, warning=function(w) w)
  if (any(class(tc_seq) == "error")) stop(tc_seq)
  if (!is.matrix(data)) data <- as.matrix(data)
  
  index <- 0
  projected_data <- NULL
  
  for (i in seq(from, to, by)) {
    index <- index+1
    delta <- cbind(data %*% manip, index)
    rownames(delta) <- rep(colnames(data),index)
    projected_data <- rbind(projected_data, delta)
  }
  colnames(projected_data) <- colnames(manip)
  #rownames(projected_data) <- rep(colnames(data),index) #doesn't work.
  return(projected_data)
}

