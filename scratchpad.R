### Testing and scratchpad:
library(devtools)
library(roxygen2)
devtools::load_all()
library(ggplot2)
library(plotly)

#### load
#devtools::install_github("nspyrison/spinifex")
#library(spinifex)
#?proj_data #test documentation

###
data <- flea[, 1:3]
p <- ncol(data)
#r_basis <- create_random_basis(p = p)
i_basis <- create_identity_basis(p = p)

#view_basis(r_basis, data)
#basis <- matrix(c(0.707, 0, 0.707, 0, 1, 0), ncol=2, byrow=FALSE)
#basis <- matrix(c(0.99, .01, .01, 0.99), ncol=2, byrow=FALSE)

proj <-
  proj_data(
    data = data,
    basis = r_basis,
    manip_var = 1, 
    manip_type = "horizontal",
    phi_from = 0,
    phi_to = 2*pi,
    n_slides = 10
  )
slideshow(proj)#, col = flea$species) 
  ##SEE SLIDESHOW COLORS.


is_orthornormal(r_basis)

stop()
stop()
tourr::animate_xy(flea[,1:6])
