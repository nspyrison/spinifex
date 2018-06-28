### Testing and scratchpad:
#library(devtools)
#library(roxygen2)
devtools::load_all()
#library(ggplot2)
#library(plotly)

#### load
#devtools::install_github("nspyrison/spinifex")
#library(spinifex)
#?proj_data #test documentation

###
data <- flea[, 1:3]
p <- ncol(data)
r_basis <- create_random_basis(p = p)
#i_basis <- create_identity_basis(p = p)
#basis <- matrix(c(0.707, 0, 0.707, 0, 1, 0), ncol=2, byrow=FALSE)
#view_basis(r_basis, data)

pal <- rainbow(length(levels(flea$species)))
col <- pal[as.numeric(flea$species)]
pch <- flea$species

proj <-
  proj_data(
    data = data,
    basis = r_basis,
    manip_var = 1, 
    manip_type = "horizontal",
    phi_from = 0,
    phi_to = 2*pi,
    n_slides = 20
  )
slideshow(proj, col = col, pch = flea$species)


is_orthornormal(r_basis)
stop()
stop()
