#' Radialy roatate 1 dim of a p-dim basis.
#' 
#' Performs a radial rotation on 1 dimension of a given p-dimensional basis.
#' Returns the z,y,z contribution from each dimension of the rotated basis as a (p x 3) matrix.
#'
#' @param basis starting basis to rotate
#' @param dim dimension to rotate
#' @param mag set the absolute magnetude of \code{dim}
#' @param step change the magnetude of \code{dim}
#'
#' @export
#' @examples
#' Plot projection of rotated data, and print the x, y contributions from the dimensions.
#' plot(rotated_data[, 1],rotated_data[, 2], main = "2D projection of rotated data")


#' Nicholas Spyrison
#' 15/02/2018
#' Radial funtion

#' Goal: Return the projection of the rotated data and the x, y contributions
#'
#' Parameters:
#' data   - A (n x p) numeric matrix
#' x_dist - Horizontal distance w.r.t. the plot
#' y_dist - Verticle distance w.r.t. the plot

#' q <<- qr.Q(qr(data))  # Q of QR decomposition, the othonormal basis of dimensionality of the data.
#' Note that qr() uses Householder reflections, which is more numerically stable than gram-schimdt.


library(roxygen2)
library(devtools)

# library(tourr)
# tourr::animate_xy(flea[, 1:3], grand_tour())

# dim(flea[,1:6])  # flea[,1:6] gives a n=74,   p=6 example (n x p)
# dim(quakes)      # quakes     gives a n=1000, p=5 example (n x p)


##13 mar note: 
#-change arg to basis, not data.
#-new arg input dim
#-new arg mag [-1,1]
#How does magnitude fit with angel and distance?
#-chart of axes, look at tourr package.
#-Plot on data to example not in the function.

radial <- function(basis = as.matrix(flea[, 1:6]),  # flea[,1:6], 74 x 6 (n x p)
                   x_dist = 200,
                   y_dist = 100){
  plot_size <- 1000  # Arbitrary plot size. 1 side of square plot? What happens to phi if plot is a rectangle?
  f  <- rbind(diag(2), matrix(0, ncol = 2, nrow = ncol(data)-2))  # Trivial orthonormal 2D basis, (p x 2)
  e3 <- rbind(matrix(0, ncol = 1, nrow = ncol(data)-1), 1)
  e  <- qr.Q(qr(cbind(f,e3)))  # Q of the QR Deocomposition, the Orthonormal basis of the trivial (f1, f2, e3), (p x 3)

  h_dist <- sqrt(x_dist^2 + y_dist^2)
  theta  <- atan(y_dist/x_dist)
  phi    <- h_dist / plot_size

  #if (abs(x_dist) > plot_size) stop("x_dist shouldn't be longer than the plot_size.")
  #if (abs(y_dist) > plot_size) stop("y_dist shouldn't be longer than the plot_size.")
  if (x_dist==0 & y_dist==0) stop("Function not defined for no rotation.")
  if (is.matrix(data) == FALSE) {
    print("Expected data type is matrix, coersing data as a matrix.")
    data <- as.matrix(data)
  }
  if (is.character(data) == TRUE) stop("data is character, data expected as a numeric matrix.")

  xyz <- data %*% e  # data in 3 dim, (n x p) * (p x 3) = (n x 3)
  hvd <- diag(3)  # Tirival orthonormal basis, the Identity Matrix, (3 x 3)

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

  rotated_hvd  <- hvd %*% R  # rotated orthonormal basis, (3 x 3) * (3 x 3) = (3 x 3)
  rotated_data <- xyz %*% rotated_hvd  # rotated data in 3D, (n x 3) * (3 x 3) = (n x 3) # is this the 3D projection from p-D?
  dim_contributions <- cbind(f,e3) %*% (rotated_hvd)  # (p x 3) * (3 x 3) = (p x 3)

return(dim_contributions)

  cat("x, y contributions from p dimensions after rotation: \n \n") +
    print(dim_contributions[, 1:2])[0]


}

radial()  # Great!

out <- radial()

matplot((out[1:2,]), type = "l")
?matplot
# # Unit testing:
# radial(quakes)  #works; coerses dataframe to matrix
# radial(rbind(c(1,2,3),c(4,5,6),c("a","b","c")))  #fails; character matrix
# radial(rbind(c(1,2,3),c(4,5,6),c(7,NA,NA)))   #works
# radial(rbind(c(NA,2,3),c(4,NA,6),c(7,8,NA)))  #fails; can remove a row or colm but not a diag.
# radial(, 5000, 1)  # x > plot_size
# radial(, 0, 5000)  # y > plot_size
# radial(, 1000, 1000) #works
# radial(, 0, 9)  #works
# radial(, -9, -30)  #works
# radial(, 0, 0)  #fails; not defined for no rotation


