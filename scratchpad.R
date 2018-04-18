#Testing and scratchpad:
library(devtools)
library(roxygen2)
devtools::load_all()
#setwd("\\\\ad.monash.edu/home/User090/nspy0001/Documents/R/spinifex")
#source("./R/util.R")
#source("./R/manipulation.R")

###for Vingette:
devtools::install_github("nspyrison/spinifex")
library(spinifex)
#library(spacemanip)
?create_manip_space #test documentation

data = flea[, 1:6]
p = ncol(data)

this_manip_space <- create_manip_space(basis = basis_random(p = p), manip_var = 3)
horizontal_manip(manip_space = this_manip_space, phi = pi/3, theta = pi/4) 
  #passing theta doesn't break, good.
#vertical_manip(manip_space = this_manip_space, phi = pi/3)
#radial_manip(manip_space = this_manip_space, phi = pi/3, theta = pi/4)

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
#data_proj(), should be analagus to animate(), just give an arbitrary 10 steps?
manip = radial_manip(manip_space = this_manip_space, phi = pi/3, theta = pi/4)
data_proj(data=flea[,1:6], manip="radial")
data_proj(data=flea[1:6,1:6], manip, from=0, to=3, by=1)
colnames(data)
rownames(t)<-colnames(data)


stop()
stop()

#theta # angular rotation: atan(y_dist/x_dist)
#phi   # angular rotation: length of mouse region/size of plot region

##### foo
#foo <- function(to=1,from=5,by=1) {
#  id=0
#  m=NULL
#  
#  for (i in seq(to,from,by)) {
#    id=id+1
#    row=rnorm(1)
#    (delta=cbind(row,id))
#    if (is.null(m)) {m=delta}
#        else {m=rbind(m,delta)}
#  }
#  return(m)
#}
#foo()


