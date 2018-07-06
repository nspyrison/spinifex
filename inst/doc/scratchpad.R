### Testing and scratchpad:
devtools::document()
devtools::build()
devtools::load_all()

#### load
#devtools::install_github("nspyrison/spinifex")
#library(spinifex)
#?proj_data #test documentation

###
data <- flea[, 1:6]
p <- ncol(data)
r_basis <- create_random_basis(p = p)
#i_basis <- create_identity_basis(p = p) #NEED TO CHECK IF BASIS IS IDEN_MAT or not
#basis <- matrix(c(0.707, 0, 0.707, 0, 1, 0), ncol=2, byrow=FALSE)
#view_basis(r_basis, data)

flea_std <- apply(flea[,1:6], 2, function(x) ((x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)))
data <- flea_std
proj1 <- proj_data(data, basis=r_basis, manip_var=3, scale=T,center=T)
slideshow(proj1)

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
    n_slides = 20,
    center=T
  )
slideshow(proj, col = col)


stop()
stop()

load("data/PhysDat_6PC.rda")
load("data/fGT_6d.rda")
phys.end <- matrix(as.numeric(fGT_6d[,,dim(fGT_6d)[3]]),ncol=2)

pal <- rainbow(length(levels(PhysDat$disID)))
col <- pal[as.numeric(PhysDat$disID)]
pch <- as.character(PhysDat$disID)

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
slideshow(proj)#, col = col)#, pch=pch)#, pch = pch)

