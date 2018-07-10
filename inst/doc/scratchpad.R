### Testing and scratchpad:
# devtools::document()
# devtools::build()
devtools::load_all()

<<<<<<< HEAD

# ### load
# devtools::install_local(.)
# devtools::install_github("nspyrison/spinifex")
# library(spinifex)
# ?proj_data #test documentation

# ###
# i_basis <- create_identity_basis(p = p)
# basis <- matrix(c(0.707, 0, 0.707, 0, 1, 0), ncol=2, byrow=FALSE)
# view_basis(r_basis, data)
# is_orthornormal(r_basis)
=======
# ### load
# devtools::install_github("nspyrison/spinifex")
# library(spinifex)
# ?proj_data #test documentation

<<<<<<< HEAD
###
data <- flea[, 1:6]
p <- ncol(data)
r_basis <- create_random_basis(p = p)
#i_basis <- create_identity_basis(p = p) #NEED TO CHECK IF BASIS IS IDEN_MAT or not
#basis <- matrix(c(0.707, 0, 0.707, 0, 1, 0), ncol=2, byrow=FALSE)
#view_basis(r_basis, data)
>>>>>>> 08c6ad5ee37565db2ed43ba5c35426f5844bae14

flea_std <- 
  apply(flea[,1:6], 2, function(x) ((x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)))
data <- flea_std

proj1 <- proj_data(data, manip_var=3)
slideshow(proj1)

<<<<<<< HEAD
p <- ncol(data)
r_basis <- create_random_basis(p = p)
=======
pal <- rainbow(length(levels(flea$species)))
col <- pal[as.numeric(flea$species)]
=======
# ###
# i_basis <- create_identity_basis(p = p)
# basis <- matrix(c(0.707, 0, 0.707, 0, 1, 0), ncol=2, byrow=FALSE)
# view_basis(r_basis, data)
# is_orthornormal(r_basis)

flea_std <- 
  apply(flea[,1:6], 2, function(x) ((x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)))
data <- flea_std

proj1 <- proj_data(data, manip_var=3)
slideshow(proj1)

p <- ncol(data)
r_basis <- create_random_basis(p = p)
>>>>>>> c20b254ebd19dc7824cdd6729d1bc6bf3a7db8d3
>>>>>>> 08c6ad5ee37565db2ed43ba5c35426f5844bae14
pch <- flea$species
col <- flea$species

proj2 <-
  proj_data(
    data = data,
    basis = r_basis,
    manip_var = 4,
    manip_type = "radial",
    phi_from = 0,
    phi_to = pi,
    n_slides = 20
  )
slideshow(proj2, col = col, pch = pch)


stop()
stop()

load("data/PhysDat_6PC.rda")
load("data/fGT_6d.rda")
phys.end <- matrix(as.numeric(fGT_6d[,,dim(fGT_6d)[3]]),ncol=2)

col <- PhysDat$disID
pch <- PhysDat$disID

proj <-
  proj_data(
    data = PhysDat[, 1:6],
    basis = phys.end,
    manip_var = 1, 
    manip_type = "rad",
    phi_from = 0,
    phi_to = 2*pi,
    n_slides = 20
  )
slideshow(proj,col = col, pch = pch)

