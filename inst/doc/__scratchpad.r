### Testing and scratchpad:
devtools::document()
devtools::build()

devtools::load_all()
devtools::install()


### Phys Example
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

