#' Horizontaly roatate 1 dim of a p-dim basis.
#' 
#' Performs a horizontal rotation on 1 dimension of a p-dimensional manipulation space. Returns the x,y,z contribution from each dimension of the rotated manipulation sapce [3, p].
#'
#' @param basis starting basis to rotate
#' @param manip_var number of the variable to manipulate
#' @param phi angle changing the magnitude of `manip_var`
#' @export
#' @examples
#' this_manip_space <- create_manip_space(basis = basis_random(p = 5), manip_var = 2)
#' horizontal_manip(manip_space = this_manip_space, phi = pi/3)
#' 
horizontal_manip <- function(manip_space, phi = 0){
  theta <- 0 # 0 for horizontal
  rotate_manip_space(manip_space, theta, phi) -> r_space
return(r_space)
}


#' Vertically roatate 1 dim of a p-dim basis.
#' 
#' Performs a vertical rotation on 1 dimension of a given p-dimensional basis.
#' Returns the x,y,z contribution from each dimension of the rotated basis as a (p x 3) matrix.
#'
#' @param basis starting basis to rotate
#' @param manip_var number of the variable to manipulate
#' @param phi angle changing the magnitude of `manip_var`
#' 
#' @export
#' @examples
#' this_manip_space <- create_manip_space(basis = basis_random(p = 5), manip_var = 2)
#' vertical_manip(manip_space = this_manip_space, phi = pi/3)
#' 
vertical_manip <- function(basis = basis_random(p = 5, d = 2),
                             manip_var = 3,
                             phi = 0){
  theta <- pi/2 #pi/2 for vertical.
  create_manip_space(basis, manip_var) -> manip_space
  rotate_manip_space(manip_space, theta, phi) -> r_space
  return(r_space)
}


#' Radially roatate 1 dim of a p-dim basis.
#' 
#' Performs a radial rotation on 1 dimension of a given p-dimensional basis.
#' Returns the x,y,z contribution from each dimension of the rotated basis as a (p x 3) matrix.
#'
#' @param basis starting basis to rotate
#' @param manip_var number of the variable to manipulate
#' @param phi angle changing the magnitude of `manip_var`
#'  #phi ~= magnitude of change?
#' @param theta angle change of `manip_var`
#'  #theta ~= projected angle of change?
#' 
#' @export
#' @examples
#' this_manip_space <- create_manip_space(basis = basis_random(p = 5), manip_var = 2)
#' radial_manip(manip_space = this_manip_space, phi = pi/3, theta = pi/4)
#' 
radial_manip <- function(manip_space, phi = 0, theta = 0){
  rotate_manip_space(manip_space, theta, phi) -> r_space
  return(r_space)
}

