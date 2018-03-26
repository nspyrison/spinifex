source("./R/util.R")

#' Horizontaly roatate 1 dim of a p-dim basis.
#' 
#' Performs a horizontal rotation on 1 dimension of a given p-dimensional basis.
#' Returns the x,y,z contribution from each dimension of the rotated basis as a (p x 3) matrix.
#'
#' @param basis starting basis to rotate
#' @param manip_var number of the variable to manipulate
#' @param phi angle changing the magnitude of `manip_var`
#' 
#' @export
#' @examples
#' (manip <- horizontal_manip(basis = basis_random(p = 5,d = 2), manip_var = 1, phi = 1))
#' head(proj <- data_proj(data = quakes, r_space = manip))
#' plot(proj[,1], proj[,2], main="Projected data")
#' 
horizontal_manip <- function(basis = basis_random(p = 5, d = 2),
                                manip_var = 3,
                                phi = 0){
  theta <- 0 # 0 for horizontal, pi/2 for vertical. #angular: atan(y_dist/x_dist)
  
  create_manip_space(basis, manip_var) -> manip_space
  rotate_manip_space(manip_space, theta, phi) -> r_space
  
  #r_space <- as.matrix(r_space[,1])# [,1] for horizontal (the x contributions). [p,1]
    ###!!!should r_space really be limited to the x axis? isn't the horizontal aspect inherint to theta=0?
  
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
#' (manip <- vertical_manip(basis = basis_random(p = 5,d = 2), manip_var = 2, phi = 1))
#' head(proj <- data_proj(data = quakes, r_space = manip))
#' plot(proj[,1], proj[,2], main="Projected data")
#' 
vertical_manip <- function(basis = basis_random(p = 5, d = 2),
                             manip_var = 3,
                             phi = 0){
  theta <- pi/2 # 0 for horizontal, pi/2 for vertical. #angular: atan(y_dist/x_dist)
  
  create_manip_space(basis, manip_var) -> manip_space
  rotate_manip_space(manip_space, theta, phi) -> r_space
  
  #r_space <- as.matrix(r_space[,2])# [,2] for vertical (the y contributions). [p,1]
    ###!!!should r_space really be limited to the x axis? isn't the horizontal aspect inherint to theta=0?
  
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
#' (manip <- radial_manip(basis = basis_random(p = 5,d = 2), manip_var = 2, phi = 1, theta = 1))
#' head(proj <- data_proj(data = quakes, r_space = manip))
#' plot(proj[,1], proj[,2], main="Projected data")
#' 
radial_manip <- function(basis = basis_random(p = 5, d = 2),
                           manip_var = 3,
                           phi = 0,
                           theta = 0){
  
  create_manip_space(basis, manip_var) -> manip_space
  rotate_manip_space(manip_space, theta, phi) -> r_space
  
  return(r_space)
}


