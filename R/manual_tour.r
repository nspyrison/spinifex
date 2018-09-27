#' Create a manipulation space
#'
#' Create a [p, d+1=3] dim orthonormal manipulation space from the given
#' basis concatonated with a zero vector, with manip_var set to 1.
#'
#' @param basis A [p, d=2] dim orthonormal basis.
#' @param manip_var Number of the dimension (numeric variable) to rotate.
#' @return manip_space, a [p, d+1=3] orthonormal manipulation space
#' 
#' @examples
#' create_random_basis(6) -> ThisBasis
#' create_manip_space(ThisBasis, 4)
#' @export
create_manip_space <- function(basis, manip_var) {
  stopifnot(class(basis) %in% c("matrix", "data.frame"))
  stopifnot(class(as.integer(manip_var)) != "integer" | 
              class(manip_var) != "character")  
  
  z <- rep(0, len = nrow(basis))
  z[manip_var] <- 1
  manip_space <- tourr::orthonormalise(cbind(basis, z))
  if (ncol(manip_space) == 3) {colnames(manip_space) <- c("x","y","z")}
  #if (ncol(manip_space) == 4) {colnames(manip_space) <- c("x","y","z","w")}
  rownames(manip_space) <- colnames(basis)
  
  #stopifnot(dim(manip_space) == dim(basis) + c(0, 1))
  return(manip_space)
}

#' Rotate and return the manipulation space
#'
#' This function does the manual rotation work.
#' Rotates a [p, 3] basis matrix manipulation space by [3, 3] rotation, 
#' returning [p, 3] orthonormal matrix, XYZ components after rotation.
#' This matrix is used to make the data projection.
#'
#' @param manip_space A [p, 3] dim manipulation space to be rotated.
#' @param theta Angle in radians to rotation w.r.t. the XY reference frame. 
#' Typically set from manip_type in proj_data().
#' @param phi Angle in radians corresponding to the magnitude of rotation.
#' @return r_space, a [p, 3] dim rotated space.
#' 
#' @examples
#' require(tourr)
#' prj <- basis_random(6, 2) 
#' msp <- create_manip_space(prj, 4) 
#' rotate_manip_space(msp, theta = 1.58, phi = 6.32) # .5pi,. 2pi.
#' @export
rotate_manip_space <- function(manip_space, theta, phi){
  stopifnot(ncol(manip_space) == 3)
  stopifnot(is.matrix(manip_space))
  
  s_theta <- sin(theta)
  c_theta <- cos(theta)
  s_phi   <- sin(phi)
  c_phi   <- cos(phi)
  
  # A [3, 3] dim rotation matrix, as a function of theta and phi.
  R <- matrix(c(c_theta^2 * c_phi + s_theta^2,
                -c_theta * s_theta * (1 - c_phi),
                -c_theta * s_phi,                      # 3 of 9
                -c_theta * s_theta * (1 - c_phi),
                s_theta^2 * c_phi + c_theta^2,
                -s_theta * s_phi,                      # 6 of 9
                c_theta * s_phi,
                s_theta * s_phi,
                c_phi )                                # 9 of 9
              ,nrow = 3, ncol = 3, byrow = TRUE)
  
  r_space <- manip_space %*% R
  colnames(r_space) <- colnames(manip_space)
  rownames(r_space) <- rownames(manip_space)
  
  stopifnot(dim(r_space) == c(nrow(manip_space),3) )
  stopifnot(is.matrix(r_space))
  return(r_space)
}

#' Produce the series of porjection bases to rotate a variable into and out of a 
#' projection
#'
#' Rotates the manipulation space across n_slides increments from phi_from to 
#' phi_to. Returns an array of bases.
#'
#' @param basis A [p, 2] dim orthonormal starting basis. 
#' Defaults to the identity basis.
#' @param manip_var Integer column or column name of the variable 
#' to manipulate. Required, does not default.
#' @param manip_type Character string of the type of manipulation to use. 
#' Defaults to "radial". Alternatively use "horizontal" or "vertical".
#' supersedes theta if set.
#' @param theta Optional parameter, yields to manip_type. Angle in radians 
#' between the axes on the reference frame the positive side of the x-axis.
#' @param phi_from Angle in radians of phi to start the projection. Defaults to 0.
#' @param phi_to Angle in radians of phi to end the projection. Defaults to 0.
#' @param n_slides Number of slides to create for slideshow(). Defaults to 15.
#' @return proj_list, a list containing $proj_data[n, p, n_slides] dim list and
#' $proj_axes[n, p, n_slides] dim list containing information about projected 
#' data and axes respectively.
#' 
#' @examples
#' require(tourr)
#' rb <- basis_random(n = 6, d = 2)
#' 
#' prj <-
#'   manual_tour(
#'     basis = rb,
#'     manip_var = 4,
#'     manip_type = "radial",
#'     phi_from = 0,
#'     phi_to = pi,
#'     n_slides = 20
#'   )
#' @export
manual_tour <-
  function(manip_var = 3,
           basis = basis_random(n = ncol(data)),
           manip_type = c("radial", "horizontal", "vertical"),
           theta = NULL,  # [in radians]
           phi_from = 0,  # [in radians]
           phi_to = 2*pi, # [in radians]
           n_slides = 15,
           rescale01 = FALSE
           ) {
    # Assertions
    #stopifnot(ncol(data) == nrow(basis))
    #stopifnot(is.matrix(data) | is.data.frame(data))
    stopifnot(is.matrix(basis))
    stopifnot(nrow(basis) > 2)
    stopifnot(manip_type %in% c("radial", "horizontal", "vertical"))
    
    # Set dimensions
    p <- nrow(basis)
    d <- ncol(basis)
    
    # Handle args
    # manip_type and theta
    if (!is.null(manip_type)) {
      manip_type <- tolower(manip_type)
    }
    if (!is.null(theta) & !is.null(manip_type) ) {
      message(
        "Non null theta used with manip_type. Using theta over manip_type.")
    }
    # other parameters
    #if (is.character(manip_var)) {
    #  manip_var <- match(manip_var, colnames(data)) # char to num
      if (!is.numeric(manip_var)) 
        stop("manip_var string not matched to a column name, try a column number.")
    #}
    #if (!is.matrix(data)) {data <- as.matrix(data)}
    if (manip_type == "horizontal") theta <- 0
    if (manip_type == "vertical") theta <- pi / 2
    if (manip_type == "radial")
      theta <- atan(basis[manip_var, 2] / basis[manip_var, 1])

    #if (rescale01) {data <- tourr::rescale(data)}
    
    # Initialise rotation sapce
    manip_space <- 
      create_manip_space(basis = basis, manip_var = manip_var)
    #index <- 0
    #proj_data <- NULL
    ### Create sequence of projected data
    #for (phi in seq(phi_from, phi_to, length.out = n_slides) ) {
    #  index <- index + 1
    #  delta <- data %*% rotate_manip_space(manip_space, theta, phi)
    #  delta <- cbind(delta, index, manip_var, phi, theta)
    #  proj_data[, , index] <- delta
    #}
    #proj_data <- tibble::as_tibble(proj_data)

    ### Create sequence of projected axes
    index <- 0
    proj_axes <- array(dim=c(p, d, n_slides))
    for (phi in seq(phi_from, phi_to, length.out = n_slides) ) {
      index <- index + 1
      delta <- rotate_manip_space(manip_space, theta, phi)

      #delta <- cbind(delta, phi)
      proj_axes[, , index] <- delta[,1:2]
    }
    #proj_axes <- tibble::as_tibble(proj_axes)

    #proj_list <- list("proj_data" = proj_data,
    #                  "proj_axes" = proj_axes )
    
    # Output assertions
    #stopifnot(dim(proj_data$index)[3] == n_slides)
    #stopifnot(dim(proj_axes$index)[3] == n_slides)
    #stopifnot(is.data.frame(proj_data))
    #stopifnot(is.data.frame(proj_axes))
    #stopifnot(is.list(proj_list))
    return(proj_axes)
  }