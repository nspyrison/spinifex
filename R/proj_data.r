#' Project data by the rotated space. Return a list of projected data and basis.
#'
#' Project [n, p] data by [p, 3] rotated manipulation space into [n, 3] projection. 
#'
#' @param data [n, p] data to project, consisting of only numeric variables (for coercion into matrix)
#' @param basis the [p, 2] orthonormal starting basis. Default to the identity basis
#' @param manip_var integer column or string name of the variable to manipulate. Required, does not default
#' @param manip_type character string of the type of manipulation to use. Defaults to "radial". Alternatively use "horizontal" or "vertical" Superseds theta
#' @param theta the angle that the manipvar should be rotated by in parameter space. Superseded by manip_type 
#' @param phi_from value of phi to start the projection. Defaults to 0
#' @param phi_to value of phi to end the projection. Defaults to 0
#' @param n_slides number of slides to create for slideshow(). Defaults to 15
#' @export
#' @examples

proj_data <-
  function(data,
           basis = basis_identity(p = ncol(data)),
           manip_var,
           manip_type = NULL,
           theta = NULL,
           center = TRUE,
           scale = TRUE,
           phi_from = 0,
           phi_to = 0,
           n_slides = 15 
           ) {
    ### SORTING MANIP_TYPE AND THETA
    manip_type3 <- tolower(substr(manip_type, 1, 3))
    if (!is.null(theta) & manip_type3 %in% c("rad", "hor", "ver")) {
      message(
        "Non null theta used with valid manip_type.
        Theta set from the manip_type."
      )
    }
    if (is.null(theta) & is.null(manip_type)) {
      manip_type3 <- "rad"
      message("manip_type and theta not set. Using radial manipulation.")
    }
    if (!manip_type3 %in% c("rad", "hor", "ver")) {
      manip_type3 <- "rad"
      message(manip_type, " manipulation type not found. Defaulting to 
        radial manipulation."
              )
    }

    ### OTHER PARAM
    if (is.character(manip_var))
      manip_var <- match(manip_var, names(data)) #char to num
    if (!is.matrix(data)) data <- as.matrix(data)
    if (center) data <- scale(data, center = T, scale = F)
    if (scale) data <- scale(data, center = F, scale = T)
    if (manip_type3 == "hor") theta <- 0
    if (manip_type3 == "ver") theta <- pi / 2
    if (manip_type3 == "rad")
      theta <- atan(basis[manip_var, 2] / basis[manip_var, 1])
    
    ### PROJ_DATA
    index <- 0
    proj_data <- NULL
    manip_space <- create_manip_space(basis = basis, manip_var = manip_var)
    phi_by <- (phi_to-phi_from) / (n_slides-1)
    for (phi in seq(phi_from, phi_to, phi_by) ) {
      index <- index + 1
      delta <- cbind(data %*% rotate_manip_space(manip_space, theta, phi),
                     index,
                     manip_var,
                     phi,
                     theta)
      
      proj_data <- rbind(proj_data, delta)
    }
    
    ### PROJ_BASIS
    index <- 0
    proj_basis <- NULL
    for (phi in seq(phi_from, phi_to, phi_by) ) {
      index <- index + 1
      delta <- cbind(rotate_manip_space(manip_space, theta, phi), index, phi)
    
      proj_basis <- rbind(proj_basis, delta)
    }
    
    n_index <- max(proj_basis[, 4])
    proj_basis <- cbind(proj_basis, manip_var, theta, "var_num" = 1:ncol(data))
    rownames(proj_basis) <- rep(colnames(data), n_index)
    
    proj_list <- list("proj_data" = as.data.frame(proj_data),
                      "proj_basis" = as.data.frame(proj_basis) )
    return(proj_list)
  }
