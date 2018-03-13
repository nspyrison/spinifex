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
                   step = .1){
  input <<- basis
  v <- matrix(0, ncol = 1, nrow = nrow(basis)) # 0 vector (p x 1)
  v[dim, 1] <- step # set dim to adjust and the value to adjust.  (p x 1)
  
  e <- qr.Q(qr(cbind(basis, v)))  
    # Q of the QR Decomposition, Orthonorm basis of (f[,1], f[,2], e), (p x 3)
  output <<- e[,1:2]

  
return(e[,1:2])
}

#k <- 5
#tstMat <- array(runif(k), dim=c(k,k))
#tstOrth <- qr.Q(qr(tstMat))
#my_test <- t(tstOrth)%*%tstOrth
#my_basis <- (qr.Q(qr(my_test[,1:2])))


radial(basis=myBasis, dim=2, step=100)

(lastBasis <-radial(dim=3,step=.5))
input
output

is.near()

abs(input)-abs(output)



lastBasis <- radial(lastBasis)

matplot((out[1:2,]), type = "l")
?matplot


