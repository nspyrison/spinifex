#' Return the manipulation space of the specified rotation
#'
#' Rotates a basis returning (p, 3) manipulation space that projects to 
#' `view_frame()`. Allows for interactive use rather than producing a whole tour.
#' 
#' @param basis A (p, d) orthonormal numeric matrix.
#' The linear combination the original variables contribute to projection space.
#' Defaults to NULL, generating a random basis.
#' @param manip_var Number of the column/dimension to rotate.
#' @param theta Angle in radians of "in-plane" rotation, on the xy plane of the 
#'   reference frame. Required, no default.
#'   If left NULL, will initialize the radial angle of the `manip_var`.`
#' @param phi Phi is angle in radians of 
#'   the "out-of-plane" rotation, the z-axis of the reference frame. 
#'   Required, no default.
#' @return (p, 2) matrix of the rotated basis.
#' @import tourr
#' @export
#' @examples
#' dat <- tourr::flea[, 1:6]
#' bas_pca <- stats::prcomp(dat)$rotation[, 1L:2L]
#' mvar <- which(abs(bas_pca[, 1]) == max(abs(bas_pca[, 1]))) ## Larget var in PC1
#' print_manip_space(bas_pca, mvar)
#' 
#' rb    <- tourr::basis_random(n = 6)
#' rtheta <- runif(1, 0, 2 * pi)
#' rphi   <- runif(1, 0, 2 *pi)
#' print_manip_space(basis = rb, manip_var = 4, rtheta, rphi)
oblique_basis <- print_basis <- function(basis = NULL,
                              manip_var,
                              theta = 0L,
                              phi = 0L){
  m_sp <- create_manip_space(basis, manip_var)
  rotate_manip_space(manip_space = m_sp, theta, phi)[ , 1:2]
}


#' Plot a single frame of a manual tour
#'
#' Projects the specified rotation as a 2D ggplot object. One static frame of 
#' manual tour. Useful for providing user-guided interaction.
#' 
#' @param data A (n, p) dataset to project, consisting of numeric variables.
#' @param basis A (p, d) dim orthonormal numeric matrix.
#' Defaults to NULL, giving a random basis.
#' @param manip_var Optional, number of the variable to rotate. 
#' If NULL, theta and phi must be 0 as is no manip space to rotate. 
#' @param theta Angle in radians of "in-projection plane" rotation, 
#' on the xy plane of the reference frame. Defaults to 0, no rotation.
#' @param phi Angle in radians of the "out-of-projection plane" rotation, into 
#' the z-direction of the axes. Defaults to 0, no rotation.
#' @param lab Optionally, provide a character vector of length p (or 1) 
#' to label the variable contributions to the axes, Default NULL, 
#' results in a 3 character abbreviation of the variable names.
#' @param rescale_data When TRUE scales the data to between 0 and 1.
#' Defaults to FALSE.
#' @param ggproto Accepts a list of gg functions. Think of this as an 
#' alternative format to `ggplot() + ggproto[[1]]`.
#' Intended for passing a `ggplot2::theme_*()` and related aesthetic functions.
#' @param ... Optionally pass additional arguments to the `render_type` for 
#' projection point aesthetics; `geom_point(aes(...))`.
#' @return A ggplot object of the rotated projection.
#' @import tourr
#' @export
#' @examples
#' dat <- tourr::flea[, 1:6]
#' bas_pca <- stats::prcomp(dat)$rotation[, 1L:2L]
#' mvar <- which(abs(bas_pca[, 1]) == max(abs(bas_pca[, 1]))) ## Larget var in PC1
#' view_frame(bas_pca, manip_var = mvar)
#' 
#' rb     <- tourr::basis_random(n = ncol(dat))
#' rtheta <- runif(1, 0, 2 * pi)
#' rphi   <- runif(1, 0, 2 * pi)
#' view_frame(basis = rb, data = dat, manip_var = 4,
#'            theta = rtheta, phi = rphi, lab = paste0("MyNm", 3:8), 
#'            rescale_data = TRUE,
#'            ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")))
oblique_frame <- view_frame <- function(basis = NULL,
                       data = NULL,
                       manip_var = NULL,
                       theta = 0L,
                       phi = 0L,
                       lab = NULL,
                       rescale_data = FALSE,
                       ggproto = ggplot2::theme_void(),
                       ...) {
  if(is.null(basis) & is.null(data)) stop("basis or data must be supplied.")
  if(is.null(manip_var) & (theta != 0L | phi != 0L))
    stop("theta or phi non-zero with a null manip_var. Manip_var required for manual_tour()")
  ## Basis condition handling
  if (is.null(basis) & !is.null(data)) {
    basis <- stats::prcomp(data)$rotation[, 1L:2L]
    message("NULL basis passed. Set to PCA basis.")
  }
  
  ## Initalize
  p <- nrow(basis)
  if(is.null(data) == FALSE)
    data <- as.matrix(data)
  if(is.null(manip_var) == FALSE){
    m_sp   <- create_manip_space(basis, manip_var)
    r_m_sp <- rotate_manip_space(manip_space = m_sp, theta, phi)
    basis <- r_m_sp[, 1L:2L] ## Really rotated basis
  }
  tour_array <- array(basis, dim = c(dim(basis), 1))
  attr(tour_array, "manip_var") <- manip_var
  
  ## Render
  df_frames <- array2df(array = tour_array, data = data, lab = lab)
  gg <- render_(frames = df_frames, ggproto = ggproto, ...)
  
  ## Return
  gg
}



#' Render display of a provided tour path
#'
#' Takes the result of `tourr::save_history()` or `manual_tour()`, interpolates
#' over the path and renders into a selected `render_type`.
#'
#' @param tour_path The result of `tourr::save_history()` or `manual_tour()`.
#' @param data Optional, number of columns must match that of `tour_path`.
#' @param rescale_data When TRUE scales the data to between 0 and 1.
#' Defaults to FALSE.
#' @param angle Target distance (in radians) between steps. Defaults to .05.
#' @param render_type Graphics to render to. Defaults to render_plotly, 
#' alternative use render_gganimate.
#' @param ggproto Accepts a list of gg functions. Think of this as an 
#' alternative format to `ggplot() + ggproto[[1]]`.
#' Intended for passing a `ggplot2::theme_*()` and related aesthetic functions.
#' @param ... Optionally pass additional arguments to the `render_type` for 
#' projection point aesthetics; `geom_point(aes(...))`.
#' @import tourr
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[, 1:6])
#' tpath <- tourr::save_history(flea_std, tour_path = tourr::grand_tour(), max = 3)
#' flea_class <- tourr::flea$species
#' 
#' \dontrun{
#' play_tour_path(tour_path = tpath, data = flea_std)
#' 
#' play_tour_path(tour_path = tpath, data = flea_std,
#'                axes = "bottomleft", angle = .08, fps = 10,
#'                color = flea_class, shape = flea_class, size = 1.5,
#'                ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")),
#'                render_type = render_gganimate)
#' 
#' if (F){ ## Saving output may require additional setup
#'   ## Export plotly .html widget
#'   play_tour_path(tour_path = tpath, data = flea_std,
#'                  axes = "left", angle = .06, fps = 10,
#'                  color = flea_class, shape = flea_class, size = 1.2,
#'                  ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")),
#'                  render_type = render_plotly,
#'                  html_filename = "myRadialTour.html")
#'                
#'   ## Export gganimate .gif
#'   play_tour_path(tour_path = tpath, data = flea_std,
#'                  axes = "left", angle = .04, fps = 6,
#'                  color = flea_class, shape = flea_class, size = 2,
#'                  ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")),
#'                  render_type = render_gganimate,
#'                  gif_path = "myOutput", gif_filename = "myRadialTour.gif")
#' }
#' }
play_tour_path <- function(tour_path = NULL,
                           data  = NULL,
                           angle = .05,
                           render_type = render_plotly,
                           rescale_data = FALSE,
                           ggproto = ggplot2::theme_void(),
                           ...) {
  if (is.null(tour_path) & is.null(data)) stop("tour_path or data must be supplied.")
  ## Data condition handling
  if(is.null(data) & !is.null(attributes(tour_path)$data)){ 
    data <- attributes(tour_path)$data
    message("data is NULL with a tourr object containing data; using its data.")
    
  }
  
  ## Initialization
  data <- as.matrix(data)
  if(rescale_data) data <- tourr::rescale(data)
  
  ## Tour array to tour df
  tour_path <- tourr::interpolate(basis_set = tour_path, angle = angle)
  attr(tour_path, "class") <- "array"
  tour_df <- array2df(array = tour_path, data = data)
  
  ## Render
  anim <- render_type(frames = tour_df, ggproto = ggproto, ...)
  
  ## Return
  anim
}


#' Animate a manual tour
#'
#' Performs the a manual tour and returns an animation of `render_type`.
#' For use with `tourr::save_history()` tour paths see `play_tour_path()`. 
#' 
#' @name play_manual_tour
#' @param basis A (p, d) orthonormal numeric matrix. 
#' The linear combination the original variables contribute to projection space.
#' Defaults to NULL, generating a random basis.
#' @param data (n, p) dataset to project, consisting of numeric variables.
#' @param manip_var Integer column number or string exact column name of the.
#' variable to manipulate. Required, no default.
#' @param rescale_data When TRUE scales the data to between 0 and 1.
#' @param theta Angle in radians of "in-plane" rotation, on the xy plane of the 
#' reference frame. Defaults to theta of the basis for a radial tour.
#' @param phi_min Minimum value phi should move to. Phi is angle in radians of 
#' the "out-of-plane" rotation, the z-axis of the reference frame. 
#' Required, defaults to 0.
#' @param phi_max Maximum value phi should move to. Phi is angle in radians of 
#' the "out-of-plane" rotation, the z-axis of the reference frame. 
#' Required, defaults to pi/2.
#' @param angle Target distance (in radians) between steps. Defaults to .05.
#' @param ggproto Accepts a list of gg functions. Think of this as an 
#' alternative format to `ggplot() + ggproto[[1]]`.
#' Intended for passing a `ggplot2::theme_*()` and related aesthetic functions.
#' @param render_type Graphics to render to. Defaults to render_plotly, 
#' alternative use render_gganimate.
#' @param ... Optionally pass additional arguments to the `render_type` for 
#' projection point aesthetics; `geom_point(aes(...))` or passes optional
#' arguments to `manual_tour`.
#' @param render_type Which graphics to render to. Defaults to render_plotly, 
#' @return An animation of a radial tour.
#' @import tourr
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[, 1:6])
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' flea_class <- tourr::flea$species
#' 
#' \dontrun{
#' play_manual_tour(basis = rb, data = flea_std, manip_var = 4)
#' 
#' play_manual_tour(basis = rb, data = flea_std, manip_var = 6,
#'                  theta = .5 * pi, axes = "right", fps = 5,
#'                  col = flea_class, pch = flea_class, size = 1.5,
#'                  ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")),
#'                  render_type = render_gganimate)
#' 
#' if(F){ ## Saving output may require additional setup
#'   ## Export plotly .html widget
#'   play_manual_tour(basis = rb, data = flea_std, manip_var = 6, 
#'                    theta = .5 * pi, axes = "left", fps = 5,
#'                    col = flea_class, pch = flea_class, size = 1.5,
#'                    ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")),
#'                    render_type = render_plotly,
#'                    html_filename = "myRadialTour.html")
#'   
#'   ## Export gganimate .gif
#'   play_manual_tour(basis = rb, data = flea_std, manip_var = 1,
#'                    theta = 0, axes = "topright", fps = 5,
#'                    ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")),
#'                    render_type = render_gganimate,
#'                    gif_filename = "myRadialTour.gif", gif_path = "./output")
#' }
#' }
play_manual_tour <- function(basis = NULL,
                             data = NULL,
                             manip_var = NULL,
                             rescale_data = FALSE,
                             theta = NULL,
                             phi_min = 0L,
                             phi_max = .5 * pi,
                             angle = .05,
                             ggproto = ggplot2::theme_void(),
                             render_type = render_plotly,
                             ...) {
  if (is.null(basis) & is.null(data)) stop("basis or data must be supplied.")
  ## Basis condition handling
  if (is.null(basis) & !is.null(data)) {
    basis <- stats::prcomp(data)$rotation[, 1L:2L]
    message("NULL basis passed. Set to PCA basis.")
  }
  ## manip_var condition handling
  if (is.null(manip_var) & !is.null(data)) {
    manip_var <- which(abs(basis[, 1]) == max(abs(basis[, 1])))
    message(paste0("NULL manip_var passed. Set to ", manip_var,
                   ", the number of the variable with largest contribution in the first column of the basis."))
  }
  if (is.null(manip_var)) stop("manip_var must be supplied.")
  
  ## Initialization
  data <- as.matrix(data)
  if (rescale_data) data <- tourr::rescale(data)
  
  ## Render
  tour_hist <- manual_tour(basis = basis, manip_var = manip_var, ...)
  tour_df <- array2df(array = tour_hist, data = data)
  anim <- render_type(frames = tour_df, ggproto = ggproto, ...)
  
  ## Return
  anim
}

