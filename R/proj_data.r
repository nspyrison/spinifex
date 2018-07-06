#' Project data by the rotated space. Return a list of projected data and basis.
#'
#' Project [n, p] data by [p, 3] rotated manipulation space, includes some parmeter information. Rotates the manipulation space accross phi. Returns both as a list.
#'
#' @param data [n, p] data to project, consisting of only numeric variables (for coercion into matrix)
#' @param basis the [p, 2] orthonormal starting basis. Default to the identity basis
#' @param manip_var integer column or string name of the variable to manipulate. Required, does not default
#' @param manip_type character string of the type of manipulation to use. Defaults to "radial". Alternatively use "horizontal" or "vertical", superseds theta if set
#' @param phi_from value of phi to start the projection. Defaults to 0
#' @param phi_to value of phi to end the projection. Defaults to 0
#' @param n_slides number of slides to create for slideshow(). Defaults to 15
#' @return list of $proj_data[n*n_slides, 7], $proj_basis[p*n_slides, 9]
#' @export
#' @examples
#'
#' data <- flea[, 1:6]
#' p <- ncol(data) 
#' r_basis <- create_random_basis(p = p)
#' proj <-
#'   proj_data(
#'     data = data,
#'     basis = r_basis,
#'     manip_var = 3, 
#'     manip_type = "radial",
#'     phi_from = 0,
#'     phi_to = 1.5*pi,
#'     n_slides = 10
#'   )

  # data=flea[1:6];manip_var=3;basis=create_random_basis(p=ncol(data));
  # theta=NULL;manip_type=NULL;manip_var=2; center=F; scale=F;
  # phi_from=0;phi_to=pi;n_slides=15;
  # proj <- proj_data(data,manip_var,basis,center=T,scale=T);
  # slideshow(proj,col=flea[,7]);
proj_data <-
  function(data,
           manip_var,
           basis = create_identity_basis(p = ncol(data)),
           manip_type = NULL,
           theta = NULL,
           center = FALSE,
           scale = FALSE,
           phi_from = 0,
           phi_to = 2*pi,
           n_slides = 15
           ) {
    stopifnot(ncol(data) == nrow(basis))
    stopifnot(is.matrix(data) | is.data.frame(data))
    stopifnot(is.matrix(basis))
    
    ### HANDLING MANIP_TYPE AND THETA
    if (is.null(theta) & is.null(manip_type)) {
      manip_type3 <- "rad"
      message("manip_type and theta not set. Using radial manipulation.")
    }
    if (!is.null(manip_type)) manip_type3 <- tolower(substr(manip_type, 1, 3))
    if (!is.null(theta) & !is.null(manip_type) ) {
      message(
        "Non null theta used with manip_type. Using theta over manip_type.")
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
    if (is.null(theta)) {
      if (manip_type3 == "hor") theta <- 0
      if (manip_type3 == "ver") theta <- pi / 2
      if (manip_type3 == "rad")
        theta <- atan(basis[manip_var, 2] / basis[manip_var, 1])
    }
    if (center) {
      for (i in 1:n_slides) {
        data <- apply(data, 2, function(x) (x-mean(x, na.rm=TRUE)) )
      }
    }
    if (scale) {
      for (i in 1:n_slides) {
        data <- apply(data, 2, function(x) (x/sd(x, na.rm=TRUE)) )
      }
    }
    
    ### PROJ_DATA
    index <- 0
    proj_data <- NULL
    manip_space <- create_manip_space(basis = basis, manip_var = manip_var)
    for (phi in seq(phi_from, phi_to, length.out = n_slides) ) {
      index <- index + 1
      delta <- data %*% rotate_manip_space(manip_space, theta, phi)
      delta <- cbind(delta, index, manip_var, phi, theta)

      proj_data <- rbind(proj_data, delta)
    }
    proj_data <- as.data.frame(proj_data)
    
    ### PROJ_BASIS
    index <- 0
    proj_basis <- NULL
    for (phi in seq(phi_from, phi_to, length.out = n_slides) ) {
      index <- index + 1
      delta <- rotate_manip_space(manip_space, theta, phi)
      delta <- cbind(delta, index, phi)
      
      proj_basis <- rbind(proj_basis, delta)
    }
    proj_basis <- as.data.frame(proj_basis)
    proj_basis$manip_var <- manip_var
    proj_basis$theta <- theta
    proj_basis$var_num <- rep(1:ncol(data), n_slides)
    proj_basis$var_name <- rep(colnames(data), n_slides)
    
    proj_basis$color <- "grey50"
    proj_basis$color[proj_basis$var_num == proj_basis$manip_var] <-"black"
    
    proj_list <- list("proj_data" = proj_data,
                      "proj_basis" = proj_basis )
    
    stopifnot(nrow(proj_data) == nrow(data) * n_slides)
    stopifnot(nrow(proj_basis) == nrow(basis) * n_slides)
    stopifnot(length(unique(proj_data$index)) == n_slides)
    stopifnot(length(unique(proj_basis$index)) == n_slides)
    stopifnot(is.data.frame(proj_data))
    stopifnot(is.data.frame(proj_basis))
    stopifnot(is.list(proj_list))
    return(proj_list)
  }
