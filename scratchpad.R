#Testing and scratchpad:
library(devtools)
library(roxygen2)
#load_all() #Error: Could not find package root.


setwd("C:/Users/Nick/Dropbox/_Main/R_dir/spacemanip")

#source("./R/util.R")
#source("./R/manipulation.R")

###Vingette
devtools::install_github("nspyrison/spacemanip")

data = quakes
p = ncol(data)

this_manip_space <- create_manip_space(basis = basis_random(p = p), manip_var = 3)
horizontal_manip(manip_space = this_manip_space, phi = pi/3)
#vertical_manip(manip_space = this_manip_space, phi = pi/3)
#radial_manip(manip_space = this_manip_space, phi = pi/3, theta = pi/4)

for (i in seq(0, pi, pi/20)) {
  this_r_space <- radial_manip(manip_space = this_manip_space, phi = i, theta = 45)
  proj <- data_proj(data = data, r_space = this_r_space)
  plot(proj[,1], proj[,2], main="Projected data")
  Sys.sleep(time=.5)
  print(i/pi)
}


stop()
stop()


#theta # angular rotation: atan(y_dist/x_dist)
#phi   # angular rotation: length of mouse region/size of plot region


