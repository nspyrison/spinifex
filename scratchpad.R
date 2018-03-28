#Testing and scratchpad:
library(devtools)
library(roxygen2)
#load_all() #Error: Could not find package root.

setwd("C:/Users/Nick/Dropbox/_Main/R/basis_manipulation")

source("./R/util.R")
source("./R/manipulation.R")

this_manip_space <- create_manip_space(basis = basis_random(p = 5), manip_var = 2)
horizontal_manip(manip_space = this_manip_space, phi = pi/3)
#vertical_manip(manip_space = this_manip_space, phi = pi/3)
#radial_manip(manip_space = this_manip_space, phi = pi/3, theta = pi/4)

for (i in seq(0, pi, pi/20)) {
  this_r_space <- horizontal_manip(manip_space = this_manip_space, phi = i)
  proj <- data_proj(data = quakes, r_space = this_r_space)
  plot(proj[,1], proj[,2], main="Projected data")
  Sys.sleep(time=.5)
  print(i/pi)
}

stop()




#theta # angular rotation: atan(y_dist/x_dist)
#phi   # angular rotation: length of mouse region/size of plot region

###### run
#set.seed(5)
#
#horizontal_manipulation(basis = basis_random(p = 5,d = 2), manip_var = 1, phi = 1)
#my_data_proj <- data_proj(data = quakes[1:100,], r_space = my_horizontal_manip)
#
##(my_basis <- basis_random(p = 5,d = 2)) #let p=5 for quakes data. d=2 for x,y.
##(my_manip_space <- create_manip_space(basis = my_basis, manip_var = 3))
##(my_horizontal_manip <- horizontal_manip(manip_space = my_manip_space, phi = (pi/100)))
#
#my_data_proj <- data_proj(data = quakes[1:100,], r_space = my_horizontal_manip)
#head(my_data_proj)
#plot(my_data_proj[, 1],my_data_proj[, 2], main = "1D projection of rotated data")
#
#
###### spot checks
##basis - space delta. same. so orthonormal(basis+v) goes to basis+orthonormal(v)?
#cbind(my_manip_space[,1:2] - my_basis, as.matrix(my_manip_space[,3]))
#
#a <- horizontal_manip(manip_space=my_manip_space, phi = (pi/6))
#b <- horizontal_manip(manip_space=my_manip_space, phi = (pi/5))
#a-b #space delta; small change, good.
#a <- data_proj(data = quakes[1:100,], r_space = a)
#b <- data_proj(data = quakes[1:100,], r_space = b)
#par(mfrow=c(2,1))
#plot(a[, 1],a[, 2], main = "a: 1D projection of rotated data")
#plot(b[, 1],b[, 2], main = "b: 1D projection of rotated data")
#par(mfrow=c(1,1))
#head((a-b)/a) #rotated data; pct pointwise change.
#
#
#### testing
#my_phi=1
#a=horizontal_rotation(basis_random(3), 1, my_phi+0.0, quakes[1:10,3:5])
#b=horizontal_rotation(basis_random(3), 1, my_phi+0.5, quakes[1:10,3:5])
#c=horizontal_rotation(basis_random(3), 1, my_phi+1.0, quakes[1:10,3:5])
#d=horizontal_rotation(basis_random(3), 1, my_phi+1.5, quakes[1:10,3:5])
#par(mfrow=c(2,2))
#plot(a[, 1],a[, 2], main = "a, 0")
#plot(b[, 1],b[, 2], main = "b, .5")
#plot(c[, 1],c[, 2], main = "c, 1")
#plot(d[, 1],d[, 2], main = "d, 1.5")
#par(mfrow=c(1,1))
#