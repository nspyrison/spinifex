#' Radialy roatate 1 dim of a p-dim basis.
#' 
#' Performs a radial rotation on 1 dimension of a given p-dimensional basis.
#' Returns the z,y,z contribution from each dimension of the rotated basis as a (p x 3) matrix.
#'
#' @param basis starting basis to rotate
#' @param dim dimension to rotate
#' @param step change the magnetude of \code{dim}
#'
#' @export
#' @examples
#' Plot projection of rotated data, and print the x, y contributions from the dimensions.
#' plot(rotated_data[, 1],rotated_data[, 2], main = "2D projection of rotated data")
#' 

library(roxygen2)
library(devtools)

# dim(flea[,1:6])  # flea[,1:6] gives a n=74,   p=6 example (n x p)
# dim(quakes)      # quakes     gives a n=1000, p=5 example (n x p)


##13 mar note: 
#-change arg to basis, not data.
#-new arg input dim
#-new arg mag [-1,1]
#How does magnitude fit with angel and distance?
#-chart of axes, look at tourr package.
#-Plot on data to example not in the function.

radial <- function(basis = rbind(diag(2), matrix(0, ncol = 2, nrow = 4-2)), 
                     # input 2-d basis (p x 2).
                   dim = 1,
                   step = 1){
  input <<- basis
  v <- matrix(0, ncol = 1, nrow = nrow(basis)) # 0 vector (p x 1)
  v[dim, 1] <- step # set dim to adjust and the value to adjust.  (p x 1)
  #print(v) #debug
  e <<- qr.Q(qr(cbind(basis, v)))  
    # Q of the QR Decomposition, Orthonorm basis of (f[,1], f[,2], e), (p x 3)
  #print(e) #debug
  
  theta  <<- 0#0 for horizontal, 90 for vertical. #angular: atan(y_dist/x_dist)
  phi    <<- 0#angular: h_dist / plot_size
  if (theta==0 & phi==0) print("no rotation set!!!")
  
  #xyz <<- data %*% e  # data in 3 dim, (n x p) * (p x 3) = (n x 3)
  hvd <<- diag(3)  # Tirival orthonormal basis, the Identity Matrix, (3 x 3)
  
  R = matrix(c(cos(theta)^2 * cos(phi) + sin(theta)^2,  # Rotational matrix as a function of theta and phi, (3 x 3)
               -cos(theta) * sin(theta) * (1 - cos(phi)),
               -cos(theta) * sin(phi),                      # 3 of 9
               -cos(theta) * sin(theta) * (1 - cos(phi)),
               sin(theta)^2 * cos(phi) + cos(theta)^2,
               -sin(theta) * sin(phi),                      # 6 of 9
               cos(theta) * sin(phi),
               sin(theta) * sin(phi),
               cos(phi) )                                   # 9 of 9
             ,nrow = 3, ncol = 3)
  
  rotated_hvd  <<- hvd %*% R  # rotated orthonormal basis, (3 x 3) * (3 x 3) = (3 x 3)
  #rotated_basis <<- e %*% rotated_hvd  # rotated data in 3D, (n x 3) * (3 x 3) = (n x 3) # is this the 3D projection from p-D?
  dim_contributions <<- cbind(basis,v) %*% (rotated_hvd)  # (p x 3) * (3 x 3) = (p x 3)
  
  output <<- dim_contributions[,1:2]
  
return(dim_contributions[,1:2])
}

basis_random <- function(n, d = 2) {
  mvn <- matrix(stats::rnorm(n * d), ncol = d)
  return(qr.Q(qr(mvn))) #orthonormalize
}
myBasis <- basis_random(10,5)

radial(basis=myBasis, dim=1, step=1)
abs(input)-abs(output)




