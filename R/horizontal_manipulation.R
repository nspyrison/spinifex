#' horizonaly roatate 1 dim of a p-dim basis.
#' 
#' Performs a horizonal rotation on 1 dimension of a given p-dimensional basis.
#' Returns the x,y,z contribution from each dimension of the rotated basis as a (p x 3) matrix.
#'
#' @param basis starting basis to rotate
#'
#' @export
#' @examples
#' 
#' 


library(roxygen2)
library(devtools)
#library(tourr)

# dim(flea[,1:6])  # flea[,1:6] gives a n=74,   p=6 example (n x p)
# dim(quakes)      # quakes     gives a n=1000, p=5 example (n x p)


basis_random <- function(p, d = 2) {
  mvn <- matrix(stats::rnorm(p * d), ncol = d)
  return(qr.Q(qr(mvn))) #orthonormalize
}

###for testing
basis_trivial <- function(p = 5, d = 2){
  trivial_basis <- rbind(diag(d), matrix(0, ncol = d, nrow = p - d))
  return(trivial_basis)
}


create_manip_space <- function(basis = basis_random(p = 5, d = 2), # basis [p,n]
                               manip_var = 3){ #which variable to changing
  v <- rep(0, len = nrow(basis)) # 0 vector [p,1]
  v[manip_var] <- 1 # set manip_var
  manip_space <- qr.Q(qr(cbind(basis, v)))  
  # Q of the QR Decomposition, the orthonormalized manip space, [p,n+1]
  return(manip_space)
}


horizonal_manip <- function(manip_space, # 3-d space [p,3]
                            phi = 0){ # angle to rotate
  theta   <- 0 #0 for horizontal, pi/2 for vertical. #angular: atan(y_dist/x_dist)
  #phi     <- phi #horizonal as arg. #angular: h_dist / plot_size
  s_theta <- sin(theta)
  c_theta <- cos(theta)
  s_phi   <- sin(phi)
  c_phi   <- cos(phi)
  if (theta==0 & phi==0) cat("no rotation set!!!")
  
  ##2-d rotation
  #R <- matrix(c(c_phi,  # Rotational matrix as a function of phi, [2,2]
  #              s_phi,
  #              -s_phi,
  #              c_phi)    # 4 of 4
  #          ,nrow = 2, ncol = 2, byrow = TRUE)
  
  #3-d rotation
  R <- matrix(c(c_theta^2 * c_phi + s_theta^2,  # Rotational matrix as a function of theta and phi, [3,3]
              -c_theta * s_theta * (1 - c_phi),
              -c_theta * s_phi,                      # 3 of 9
              -c_theta * s_theta * (1 - c_phi),
              s_theta^2 * c_phi + c_theta^2,
              -s_theta * s_phi,                      # 6 of 9
              c_theta * s_phi,
              s_theta * s_phi,
              c_phi )                                # 9 of 9
            ,nrow = 3, ncol = 3, byrow = TRUE)
  
  r_space <- manip_space %*% R  # rotated space, [p,3] * [3,3] = [p,3]
  
  return(as.matrix(r_space[,1])) # [,1] for horizontal (x contributions).
}


data_proj <- function(data = quakes, # data [n,p]
                           r_space){ # rotated space [p,2]
  if (!is.matrix(data)) data <- as.matrix(data)
  if (ncol(r_space) == 1)r_space <- cbind(r_space, rep(0, len = nrow(r_space))) 
  projected_data <- data %*% r_space  #rotated data [n,p] * [p,2] = [n,2]
  
  return(projected_data)
}


##### run
set.seed(5)

my_basis <- basis_random(p = 5,d = 2) #let p=5 for quakes data. d=2 for x,y.
my_basis

my_manip_space <- create_manip_space(basis = my_basis, manip_var = 3)
my_manip_space

my_horizonal_manip <- horizonal_manip(manip_space = my_manip_space, phi = (pi/100))
my_horizonal_manip

my_data_proj <- data_proj(data = quakes[1:100,], r_space = my_horizonal_manip)
head(my_data_proj)
plot(my_data_proj[, 1],my_data_proj[, 2], main = "1D projection of rotated data")


##### spot checks
#basis - space delta. same. so orthonormal(basis+v) goes to basis+orthonormal(v)?
my_manip_space[,1:2] - my_basis
#as.matrix(my_manip_space[,3])

#space+. - space delta. small change, good.
horizonal_manip(manip_space=my_manip_space, phi = (pi/5)+.1) -
  horizonal_manip(manip_space=my_manip_space, phi = (pi/5))

#Plot original vs small phi. Bigger difference than expected.
  #is this cause: basis is [,2]? casue R is [3,3]? orthonormalizing on a non trivial basis?
  #going to basis [,1]wasn't any better.
  #not R [3,3] cause when theta=0, we have a 2-d rotation (but is this the y rotation?)
  #trivial basis is no better.
  ## is this cause the cntributions of the dims are far from 1/p?
par(mfrow=c(2,1))
plot(quakes[1:100, 1], rep(0, len = 100), main = "X of original data")
plot(my_data_proj[, 1],my_data_proj[, 2], main = "1D projection of rotated data")
par(mfrow=c(1,1))

#par(mfrow=c(2,1))
#a <- horizonal_manip(manip_space=my_manip_space, phi = (pi/6))
#b <- horizonal_manip(manip_space=my_manip_space, phi = (pi/5))
#a <- data_proj(data = quakes[1:100,], r_space = a)
#b <- data_proj(data = quakes[1:100,], r_space = b)
#plot(a[, 1],a[, 2], main = "a: 1D projection of rotated data")
#plot(b[, 1],b[, 2], main = "b: 1D projection of rotated data")
#par(mfrow=c(1,1))
#head((a-b)/a)


##In the trivial case orthonormalizing does nothing, stays identity and data.
#. <- basis_trivial(p=5,d=2)
#.
#. <- create_manip_space(basis = ., manip_var = 3)
#.
#. <- horizonal_manip(manip_space = ., phi = (pi/3))
#.
#. <- data_proj(data = quakes[1:100,], r_space = .)
#head(.)
#plot(.[, 1],.[, 2], main = "1D projection of rotated data")
#
#par(mfrow=c(2,1))
#plot(quakes[1:100, 1], rep(0, len = 100), main = "X of original data")
#plot(my_data_proj[, 1],my_data_proj[, 2], main = "1D projection of rotated data")
#par(mfrow=c(1,1))

