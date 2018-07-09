 devtools::load_all()

 flea_std <- apply(flea[,1:6], 2, function(x) 
   ((x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)))
 data <- flea_std
 
 p <- ncol(data) 
 r_basis <- create_random_basis(p = p)
 proj1 <-
   proj_data(
     data = data,
     basis = r_basis,
     manip_var = 3, 
     manip_type = "radial",
     phi_from = 0,
     phi_to = 1.5 * pi,
     n_slides = 10
   )
 slideshow(proj1)
 
 #pal <- rainbow(length(levels(flea$species)))
 #col <- pal[as.numeric(flea$species)]
 col <- flea[, 7]
 p <- ncol(data)
 r_basis <- create_random_basis(p)
 proj2 <- proj_data(data, manip_type="radial", manip_var="head", basis=r_basis)
 slideshow(proj2, col=col)
 