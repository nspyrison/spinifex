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

oblique_basis <- function(basis = NULL,
                          manip_var,
                          theta = NULL,
                          phi   = NULL) {
  
  m_sp <- create_manip_space(basis, manip_var)
  ret <- rotate_manip_space(manip_space = m_sp, theta, phi)[, 1:2]
  
  ret
}


#' Plot a single frame of a manual tour
#'
#' Projects the specified rotation as a 2D ggplot object. One static frame of 
#' manual tour. Useful for providing user-guided interaction.
#' 
#' @param data A  (n, p) dataset to project, consisting of numeric variables.
#' @param basis A (p, d) dim orthonormal numeric matrix. 
#' Defaults to NULL, giving a random basis.
#' @param manip_var Number of the variable to rotate.
#' @param theta Angle in radians of "in-projection plane" rotation, 
#' on the XY plane of the reference frame. Defaults to 0, no rotaion.
#' @param phi Angle in radians of the "out-of-projection plane" rotation, into 
#' the z-direction of the axes. Defaults to 0, no rotaion.
#' @param lab Optionally, provide a character vector of length p (or 1) 
#' to label the variable contributions to the axes, Default NULL, 
#' results in a 3 character abbriviation of the variable names.
#' @param rescale_data When TRUE scales the data to between 0 and 1.
#' Defaults to FALSE.
#' @param ... Optionally pass additional arguments `render_`. 
#' Especially to the col, pch, and cex, alpha
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

## TODO: Review spinifex_study app to see if pch, col, cex, alpha belong 
#### within aes or not (for proper legend display). Resolve here if so.

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
  if (!is.matrix(data)) {
    messgae("Data is not a matrix, coearsing to matrix")
    data <- as.matrix(data)
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
  
  ## Add labels, attribute, and list
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
  
  gg <- render_(slides = slide, graphics = "ggplot2", ...) +
    ggplot2::coord_fixed()
  
  gg
}





#' Render display of a provided tour path
#'
#' Takes the result of `tourr::save_history()` or `manual_tour()`, interpolates
#' over the path and renders into a selected `render_type`.
#'
#' @param tour_path The result of `tourr::save_history()` or `manual_tour()`.
#' @param data Optional, number of columns must match that of `tour_path`.
#' @param angle Target distance (in radians) between steps. Defaults to .15.
#' @param render_type Graphics to render to. Defaults to render_plotly, 
#'   alternative use render_gganimate.
#' @param rescale_data When TRUE scales the data to between 0 and 1.
#'   Defaults to FALSE.
#' @param ... Optionally pass additional arguments to `render_type`.
#' @import tourr
#' @export
#' @examples
#' flea_std <- rescale(tourr::flea[,1:6])
#' tpath <- save_history(flea_std, tour_path = grand_tour(),max = 3)
#' class <- tourr::flea$species
#' 
#' \dontrun{
#' play_tour_path(tour_path = tpath, data = flea_std)
#' 
#' play_tour_path(tour_path = tpath, data = flea_std, angle = .25, fps = 4,
#'   render_type = render_gganimate, col = class, pch = class, axes = "bottomleft")
#' }

play_tour_path <- function(tour_path,
                           data  = NULL,
                           angle = .15,
                           render_type = render_plotly,
                           rescale_data = FALSE,
                           ...) {
  # if data missing, but an attribute, use that.
  if(is.null(data) & !is.null(attributes(tour_path)$data)){ 
    message("data passed as NULL with a tourr object containing attached data; rendering the tour_path data.")
    data <- attributes(tour_path)$data
  }
  if (!is.matrix(data)) {
    messgae("Data is not a matrix, coearsing to matrix")
    data <- as.matrix(data)
  }
  if (rescale_data) data <- tourr::rescale(data)
  
  tour_path <- tourr::interpolate(basis_set = tour_path, angle = angle)
  attr(tour_path, "class") <- "array"
  tour_df <- array2df(array = tour_path, data = data)
  disp <- render_type(slides = tour_df, ...)
  
  disp
}




#' Animate a manual tour
#'
#' Performs the a manual tour and returns an animation of `render_type`.
#' For use with `tourr::save_history()` tour paths see `play_tour_path()`. 
#' 
#' @name play_manual_tour
#' @param data (n, p) dataset to project, consisting of numeric variables.
#' @param basis A (p, d) dim orthonormal numeric matrix. 
#'   If it's left null, random basis will be used.
#' @param render_type Which graphics to render to. Defaults to render_plotly, 
#' @param rescale_data When TRUE scales the data to between 0 and 1.
#' @param ... Optionally pass additional arguments to the `render_type` for 
#'   plotting options.
#' @return An animation of a radial tour.
#' @import tourr
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[,1:6])
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' class <- tourr::flea$species
#' 
#' \dontrun{
#' play_manual_tour(basis = rb, data = flea_std, manip_var = 4)
#' 
#' play_manual_tour(basis = rb, data = flea_std, manip_var = 6, theta = .5 * pi,
#'                  render_type = render_gganimate, col = class, pch = class, 
#'                  axes = "bottomleft", fps = 5)
#' }
play_manual_tour <- function(basis = NULL,
                             data, 
                             render_type = render_plotly,
                             rescale_data = FALSE,
                             ...) {
  if (is.null(basis)) {
    message("NULL basis passed. Initializing random basis.")
    basis <- tourr::basis_random(n = ncol(data))
  }
  if (!is.matrix(data)) {
    messgae("Data is not a matrix, coearsing to matrix")
    data <- as.matrix(data)
  }
  if (rescale_data) data <- tourr::rescale(data)
  
  
  tour_hist <- manual_tour(basis = basis, ...)
  tour_df <- array2df(array = tour_hist, data = data)
  anim <- render_type(slides = tour_df, ...)
  
  return(anim)
}

