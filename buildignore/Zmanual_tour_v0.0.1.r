### FROM spinifex v 1.0 
manual_tour <- function(basis = NULL,
                        manip_var,
                        theta = NULL,
                        phi_min = 0,
                        phi_max = .5 * pi,
                        angle = .05
) {
  # Initalize
  basis <- as.matrix(basis)
  p     <- nrow(basis)
  d     <- ncol(basis)
  manip_space <- create_manip_space(basis = basis, manip_var = manip_var)
  if (is.null(theta)) 
    theta <- atan(basis[manip_var, 2] / basis[manip_var, 1])
  phi_start <- acos(sqrt(basis[manip_var, 1]^2 + basis[manip_var, 2]^2)) 
  stopifnot(phi_min <= phi_start & phi_max >= phi_start)
  
  # Find phi's path
  find_path <- function(start, end)
  {
    mvar_xsign <- -sign(basis[manip_var, 1])
    start <- mvar_xsign * (start - phi_start)
    end   <- mvar_xsign * (end - phi_start)
    dist  <- abs(end - start)
    steps <- round(dist/angle)
    angle <- dist/steps
    sign  <- ifelse(end > start, 1, -1)
    
    seq(from=start, to=end, by=sign*angle)
  }
  
  phi_path <- c(find_path(start = phi_start, end = phi_min),
                find_path(start = phi_min,   end = phi_max),
                find_path(start = phi_max, end = phi_start))
  
  ## Make projected basis array
  n_frames <- length(phi_path)
  
  basis_set <- array(NA, dim = c(p, d, n_frames))
  for (i in 1:n_frames) {
    thisFrame <- rotate_manip_space(manip_space = manip_space, theta = theta,
                                    phi = phi_path[i])
    basis_set[,, i] <- thisFrame[, 1:d]
  }
  
  attr(basis_set, "manip_var") <- manip_var
  
  basis_set
}