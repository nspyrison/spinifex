#' Plot a single frame of a manual tour
#'
#' Projects the specified rotation as a 2D ggplot object. One static frame of 
#' manual tour. Useful for providing uaer-guided interaction.
#' 
#' @param data A  (n, p) dataset to project, consisting of numeric variables.
#' @param basis A (p, d) dim orthonormal numeric matrix. 
#'   If it's left null, random basis will be used.
#' @param manip_var Number of the column/dimension to rotate.
#' @param theta Angle in radians of "in-plane" rotation, on the XY plane of the 
#'   reference frame. Required, no default.
#'   If left NULL, will initialize the radial angle of the `manip_var`.`
#' @param phi Phi is angle in radians of 
#'   the "out-of-plane" rotation, the z-axis of the reference frame. 
#'   Required, no default.
#' @param lab Optional, labels for the reference frame of length 1 or the 
#'   number of variables used. Defaults to an abbriviation of the variables.
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

oblique_frame <- function(basis        = NULL,
                          data         = NULL, ### TODO: when NULL data gets assigned small numeric 1x1 value, where & why?
                          manip_var    = NULL,
                          theta        = 0,
                          phi          = 0,
                          lab          = NULL,
                          rescale_data = FALSE,
                          ...) {
  
  if (is.null(basis) & !is.null(data)) {
    message("NULL basis passed. Initializing random basis.")
    basis <- tourr::basis_random(n = ncol(data))
  }
  
  p <- nrow(basis)
  m_sp <- create_manip_space(basis, manip_var)
  r_m_sp <- rotate_manip_space(manip_space = m_sp, theta, phi)
  
  basis_slides <- cbind(as.data.frame(r_m_sp), slide = 1)
  colnames(basis_slides) <- c("x", "y", "z", "slide")
  if(!is.null(data)){
    if (rescale_data) {data <- tourr::rescale(data)}
    data_slides  <- cbind(as.data.frame(data %*% r_m_sp), slide = 1)
    data_slides[, 1] <- scale(data_slides[, 1], scale = FALSE)
    data_slides[, 2] <- scale(data_slides[, 2], scale = FALSE)
    colnames(data_slides) <- c("x", "y", "z", "slide")
  }
  
  # Add labels, attribute, and list
  basis_slides$lab <- 
    if(!is.null(lab)){
      rep(lab, nrow(basis_slides) / length(lab))
    } else {
      if(!is.null(data)) {abbreviate(colnames(data), 3)
      } else {paste0("V", 1:p)}
    }
  
  attr(basis_slides, "manip_var") <- manip_var
  
  slide <- if(!is.null(data)) {
    list(basis_slides = basis_slides, data_slides = data_slides)
  } else list(basis_slides = basis_slides)
  
  gg <- render_(slides = slide, ...) +
    ggplot2::coord_fixed()
  
  gg
}
