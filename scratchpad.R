#Testing and scratchpad:
library(devtools)
library(roxygen2)
devtools::load_all()

###for Vingette:
devtools::install_github("nspyrison/spinifex")
library(spinifex)
?create_manip_space #test documentation

data = flea[, 1:6]
p = ncol(data)

this_manip_space <- create_manip_space(basis = basis_random(p = p), manip_var = 3)
horizontal_manip(manip_space = this_manip_space, phi = pi/3, theta = pi/4)
  #passing theta doesn't influence horizontal/vertical_manip
vertical_manip(manip_space = this_manip_space, phi = pi/3)
radial_manip(manip_space = this_manip_space, phi = pi/3, theta = pi/4)

###manual
for (i in seq(0, pi, pi/20)) {
  this_r_space <- radial_manip(manip_space = this_manip_space, phi = i, theta = 1/3*pi)
  if(!is.matrix(data)) data<-as.matrix(data)
  data %*% this_r_space
  plot(proj[,1], proj[,2], main = "Projected data")
  Sys.sleep(time=.5)
  print(i/pi)
}

### data_proj   a work in progress.

data_proj(data=flea[1:3, 1:6], manip="radial", to=2)
ss <- data_proj(data=data, manip="radial", manip_var=3, 
          from=0, to=pi, by=pi/10,
          manip_space = this_manip_space, phi = pi/3, theta = pi/4)

slideshow(ss)






stop()
stop()

#theta # angular rotation: atan(y_dist/x_dist)
#phi   # angular rotation: length of mouse region/size of plot region

plot(1:10, 1:10, ylab = "")


