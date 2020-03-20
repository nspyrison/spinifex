#' geom_ object of a single projection frame of a manual tour
#'
#' One static frame of manual tour. 
#' Returns ggplot geom_point object (ggproto) projecting the specified rotation.
#' Useful for providing user-guided interaction.
#' 
#' @param data A (n, p) dataset to project, consisting of numeric variables.
#' @param basis A (p, d) dim orthonormal numeric matrix. 
#' Defaults to NULL, giving a random basis.
#' @param manip_var Number of the variable to rotate.
#' @param theta Angle in radians of "in-projection plane" rotation, 
#' on the XY plane of the reference frame. Defaults to 0, no rotaion.
#' @param phi Angle in radians of the "out-of-projection plane" rotation, into 
#' the z-direction of the axes. Defaults to 0, no rotaion.
#' @param rescale_data When TRUE scales the data to between 0 and 1.
#' Defaults to FALSE.
#' @param x_offset Numeric value to pan in the x-direction. Defaults to 0.
#' @param y_offset Numeric value to pan in the y-direction. Defaults to 0.
#' @param scale Numeric value to scale/zoom the size for x. Defaults to 1.
#' @param ... Optionally pass additional arguments. ie: pch, col, cex
#' 
#' @return A ggplot2 geom_point object of a object of the rotated projection.
#' @import tourr
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[,1:6])
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' 
#' ggplot2::ggplot() + 
#'   geom_manualTour_frame(data = flea_std, basis = rb, manip_var = 4)
geom_manualTour_frame <- function(basis        = NULL,
                                  data         = NULL, ### TODO: when NULL data gets assigned small numeric 1x1 value, where & why?
                                  manip_var    = NULL,
                                  theta        = 0,
                                  phi          = 0,
                                  rescale_data = FALSE,
                                  x_offset     = 0,
                                  y_offset     = 0,
                                  scale        = 1,
                                  ...) {
  if (is.null(basis) & !is.null(data)) {
    message("NULL basis passed. Initializing random basis.")
    basis <- tourr::basis_random(n = ncol(data))
  }
  if (!is.matrix(data)) {
    messgae("Data is not a matrix, coearsing to matrix.")
    data <- as.matrix(data)
  }
  if(is.null(data)  | length(data)  == 0) stop("Data is missing.")
  if(is.null(basis) | length(basis) == 0) stop("Basis is missing.")
  
  p <- nrow(basis)
  m_sp   <- create_manip_space(basis, manip_var)
  r_m_sp <- rotate_manip_space(manip_space = m_sp, theta, phi)
  if (rescale_data) {data <- tourr::rescale(data)}
  data <- pan_zoom(x = data, x_offset = x_offset, y_offset = y_offset, scale = scale)
  
  projDat  <- as.data.frame(data %*% r_m_sp)
  colnames(projDat) <- c("x", "y", "z")
  
  attr(projDat, "manip_var") <- manip_var
  
  ggplot2::geom_point(data = projDat, 
                      mapping = aes(x = x, y = y, ...)
  )
}
