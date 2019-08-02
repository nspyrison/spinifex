#' Plot a single frame of a manual tour
#'
#' Projects the specified rotation as a 2D ggplot object. Analogous to a
#' slide frame of a radial tour. Used to create an oblique tour by small
#' changes to the rotation.
#' 
#' @param data (n, p) dataset to project, consisting of numeric variables.
#' @param basis A (p, d) dim orthonormal numeric matrix. 
#'   If it's left null, random basis will be used.
#' @param manip_var Number of the column/dimension to rotate.
#' @param theta Angle in radians of "in-plane" rotation, on the XY plane of the 
#'   reference frame. Required, no default.
#'   If left NULL, will initialize the radial angle of the `manip_var`.`
#' @param phi Phi is angle in radians of 
#'   the "out-of-plane" rotation, the z-axis of the reference frame. 
#'   Required, no default.
#' @param manip_col String of the color to highlight the `manip_var`.
#'   Defaults to "blue".
#' @param col Color of the projected points. Defaults to "black".
#' @param pch Point character of the projected points. Defaults to 20.
#' @param axes Position of the axes: "center", "bottomleft" or "off". Defaults 
#'   to "center".
#' @param rescale_data When TRUE scales the data to between 0 and 1.
#'   Defaults to FALSE.
#' @param ... Optionally pass additional arguments to the `render_type` for 
#'   plotting options.
#' @return a ggplot object of the rotated projection.
#' @import tourr
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[,1:6])
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' theta <- runif(1, 0, 2*pi)
#' phi <- runif(1, 0, 2*pi)
#' 
#' oblique_frame(data = flea_std, basis = rb, manip_var = 4, theta, phi)

oblique_frame <- function(data,
                          basis       = NULL,
                          manip_var,
                          theta       = NULL,
                          phi         = NULL,
                          manip_col   = "blue",
                          col         = "black", 
                          pch         = 20,
                          axes        = "center",
                          rescale_data = FALSE,
                          ...) {
  if (rescale_data) data <- tourr::rescale(data)
  if (is.null(basis)) {
    message("NULL basis passed. Initializing random basis.")
    basis <- tourr::basis_random(n = ncol(data))
  }
  
  p <- nrow(basis)
  m_sp <- create_manip_space(basis, manip_var)
  r_m_sp <- rotate_manip_space(manip_space = m_sp, theta, phi)
  
  # misnomer, but following array2df names:
  basis_slides <- cbind(as.data.frame(r_m_sp), slide = 1)
  data_slides  <- cbind(as.data.frame(data %*% r_m_sp), slide = 1)
  data_slides[, 1] <- scale(data_slides[, 1], scale = FALSE)
  data_slides[, 2] <- scale(data_slides[, 2], scale = FALSE)
  
  # Add labels, attribute, and list
  basis_slides$lab_abbr <- if(!is.null(data)) {abbreviate(colnames(data), 3)
  } else paste0("V", 1:p)
  
  attr(basis_slides, "manip_var") <- manip_var
  
  slides <- if(!is.null(data)) {
    list(basis_slides = basis_slides, data_slides = data_slides)
  } else list(basis_slides = basis_slides)
  
  gg <- render_(slides, manip_col, col, pch, axes, ...) +
    ggplot2::coord_fixed()
  
  gg
}


#' Return the basis of an oblique frame
#'
#' Rotates a basis returning (p, 2) basis describing `oblique_frame()` 
#' Used to create an oblique tour by small changes to the rotation.
#' 
#' @param basis A (p, d) dim orthonormal numeric matrix. 
#'   If it's left null, random basis will be used.
#' @param manip_var Number of the column/dimension to rotate.
#' @param theta Angle in radians of "in-plane" rotation, on the XY plane of the 
#'   reference frame. Required, no default.
#'   If left NULL, will initialize the radial angle of the `manip_var`.`
#' @param phi Phi is angle in radians of 
#'   the "out-of-plane" rotation, the z-axis of the reference frame. 
#'   Required, no default.
#' @return (p, 2) matrix of the rotated basis
#' @import tourr
#' @export
#' @examples
#' rb <- tourr::basis_random(n = 6)
#' theta <- runif(1, 0, 2*pi)
#' phi <- runif(1, 0, 2*pi)
#' 
#' oblique_basis(basis = rb, manip_var = 4, theta, phi)

oblique_basis <- function(basis       = NULL,
                          manip_var,
                          theta       = NULL,
                          phi         = NULL) {
  
  m_sp <- create_manip_space(basis, manip_var)
  ret <- rotate_manip_space(manip_space = m_sp, theta, phi)[, 1:2]
  colnames(ret) <- c("x","y")
  
  ret
}
