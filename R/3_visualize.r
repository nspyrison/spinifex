##
## TARGET WRAPPER FUNCTIONS -----
##

#' Animates the provided tour path.
#'
#' Takes the result of `tourr::save_history()` or `manual_tour()`, interpolates
#' over the path and renders into a specified `render_type`.
#'
#' @param tour_path The result of `tourr::save_history()` or `manual_tour()`.
#' @param data Optional, number of columns must match that of `tour_path`.
#' @param angle Target distance (in radians) between steps. Defaults to .05.
#' @param render_type Graphics to render to. Defaults to `render_plotly``,
#' alternative use `render_gganimate`.
#' @param ... Optionally pass additional arguments to `render_` and the 
#' function used in `render_type`.
#' @import tourr
#' @seealso \code{\link{render_}} For arguments to pass into `...`.
#' @export
#' @examples
#' dat_std <- scale_sd(wine[, 2:14])
#' clas <- wine$Type
#' bas <- basis_pca(dat_std)
#' 
#' ## Tour history from stepwise_history
#' sw_path <- stepwise_history(basis = bas, data = dat_std)
#' 
#' ## Tour history from tourr::save_history
#' g_path <- tourr::save_history(dat_std, tour_path = tourr::grand_tour(), max = 5)
#' 
#' \dontrun{
#' play_tour_path(tour_path = sw_path, data = dat_std)
#' play_tour_path(tour_path = g_path,  data = dat_std)
#' 
#' play_tour_path(tour_path = g_path, data = dat_std,
#'                axes = "bottomleft", angle = .08, fps = 8,
#'                aes_args = list(color = clas, shape = clas),
#'                identity_args = list(size = 1.5, alpha = .7),
#'                ggproto = 
#'                  list(ggplot2::theme_void(), ggplot2::ggtitle("My title")),
#'                render_type = render_gganimate)
#' 
#' if(F){ ## Saving output may require additional setup
#'   ## Export plotly .html widget
#'   play_tour_path(tour_path = tpath, data = dat_std,
#'                  render_type = render_plotly,
#'                  html_filename = "myRadialTour.html")
#'                
#'   ## Export gganimate .gif
#'   play_tour_path(tour_path = tpath, data = dat_std,
#'                  render_type = render_gganimate,
#'                  gif_path = "myOutput", gif_filename = "myRadialTour.gif")
#' }
#' }
play_tour_path <- function(tour_path = NULL,
                           data  = NULL,
                           angle = .05,
                           render_type = render_plotly,
                           ...) {
  if(is.null(tour_path) & is.null(data)) stop("tour_path or data must be supplied.")
  ## Data condition handling
  if(is.null(data) & !is.null(attributes(tour_path)$data)){ 
    data <- attributes(tour_path)$data
  }
  
  ## Initialization
  data <- as.matrix(data)

  ## Tour array to tour df
  tour_path <- tourr::interpolate(basis_set = tour_path, angle = angle)
  attr(tour_path, "class") <- "array"
  tour_df <- array2df(array = tour_path, data = data)
  
  ## Render
  return(render_type(frames = tour_df, ...))
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
#' @param theta Angle in radians of "in-plane" rotation, on the xy plane of the 
#' reference frame. Defaults to theta of the basis for a radial tour.
#' @param phi_min Minimum value phi should move to. Phi is angle in radians of 
#' the "out-of-plane" rotation, the z-axis of the reference frame. 
#' Required, defaults to 0.
#' @param phi_max Maximum value phi should move to. Phi is angle in radians of 
#' the "out-of-plane" rotation, the z-axis of the reference frame. 
#' Required, defaults to pi/2.
#' @param angle Target distance (in radians) between steps. Defaults to .05.
#' @param render_type Graphics to render to. Defaults to render_plotly, 
#' alternative use render_gganimate.
#' @param ... Optionally pass additional arguments to `render_` and the 
#' function used in `render_type`.
#' @param render_type Which graphics to render to. Defaults to render_plotly, 
#' @return An animation of a radial tour.
#' @import tourr
#' @seealso \code{\link{render_}} For arguments to pass into `...`.
#' @export
#' @examples
#' dat_std <- scale_sd(wine[, 2:14])
#' clas <- wine$Type
#' bas <- basis_pca(dat_std)
#' mv <- manip_var_pca(dat_std)
#' 
#' \dontrun{
#' play_manual_tour(basis = bas, data = dat_std, manip_var = mv)
#' 
#' play_manual_tour(basis = bas, data = dat_std, manip_var = mv,
#'                  theta = .5 * pi, axes = "right", fps = 5,
#'                  angle = .08, phi_min = 0, phi_max = 2 * pi,
#'                  aes_args = list(color = clas, shape = clas),
#'                  identity_args = list(size = 1.5, alpha = .7),
#'                  ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")),
#'                  render_type = render_gganimate)
#' 
#' if(F){ ## Saving output may require additional setup
#'   ## Export plotly .html widget
#'   play_manual_tour(basis = bas, data = dat_std, manip_var = 6,
#'                    render_type = render_plotly,
#'                    html_filename = "myRadialTour.html")
#'   
#'   ## Export gganimate .gif
#'   play_manual_tour(basis = bas, data = dat_std, manip_var = 1,
#'                    render_type = render_gganimate,
#'                    gif_filename = "myRadialTour.gif", gif_path = "./output")
#' }
#' }
play_manual_tour <- function(basis = NULL,
                             data = NULL,
                             manip_var = NULL,
                             theta = NULL,
                             phi_min = 0L,
                             phi_max = .5 * pi,
                             angle = .05,
                             render_type = render_plotly,
                             ...){
  if(is.null(basis) & is.null(data)) stop("basis or data must be supplied.")
  ## Basis condition handling
  if(is.null(basis) & !is.null(data)){
    basis <- stats::prcomp(data)$rotation[, 1L:2L]
    message("NULL basis passed. Set to PCA basis.")
  }
  ## manip_var condition handling
  if(is.null(manip_var) & !is.null(data)) {
    manip_var <- which(abs(basis[, 1L]) == max(abs(basis[, 1L])))
    message(paste0("NULL manip_var passed. Set to ", manip_var,
                   ", the number of the variable with largest contribution in the first column of the basis."))
  }
  if(is.null(manip_var)) stop("manip_var must be supplied.")
  
  data <- as.matrix(data)
  tour_hist <- manual_tour(basis = basis, manip_var = manip_var, angle = angle,
                           theta = theta, phi_min = phi_min, phi_max = phi_max)
  tour_df <- array2df(array = tour_hist, data = data)
  return(render_type(frames = tour_df, ...))
}

##
## HELPER & INTERMEDIATE VISUALIZATIONS -----
##



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
#' @param label Optionally, provide a character vector of length p (or 1) 
#' to label the variable contributions to the axes, Default NULL, 
#' results in a 3 character abbreviation of the variable names.
#' @param rescale_data When TRUE scales the data to between 0 and 1.
#' Defaults to FALSE.
#' @param ... Optionally pass additional arguments to the `render_type` for 
#' projection point aesthetics; 
#' @return A ggplot object of the rotated projection.
#' @import tourr
#' @export
#' @examples
#' dat_std <- scale_sd(wine[, 2:14])
#' clas <- wine$Type
#' bas <- basis_pca(dat_std)
#' mv <- manip_var_pca(dat_std)
#' 
#' rtheta <- runif(1, 0, 2 * pi)
#' rphi   <- runif(1, 0, 2 * pi)
#' 
#' view_frame(basis = bas)
#' 
#' view_frame(basis = bas, data = dat_std, manip_var = mv)
#' 
#' view_frame(basis = bas, data = dat_std, manip_var = mv,
#'            theta = rtheta, phi = rphi, label = paste0("MyNm", 1:ncol(dat_std)), 
#'            aes_args = list(color = clas, shape = clas),
#'            identity_args = list(size = 1.5, alpha = .7),
#'            ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")))
view_frame <- function(basis = NULL,
                       data = NULL,
                       manip_var = NULL,
                       theta = 0L,
                       phi = 0L,
                       label = NULL,
                       rescale_data = FALSE,
                       ...){
  if(is.null(basis) & is.null(data)) stop("basis or data must be supplied.")
  if(is.null(manip_var) & (theta != 0L | phi != 0L))
    stop("theta or phi non-zero with a null manip_var. Manip_var required for manual_tour()")
  ## Basis condition handling
  if(is.null(basis) & !is.null(data)) {
    basis <- stats::prcomp(data)$rotation[, 1L:2L]
    message("NULL basis passed. Set to PCA basis.")
  }
  
  ## Initialize
  p <- nrow(basis)
  if(is.null(data) == FALSE)
    data <- as.matrix(data)
  if(is.null(manip_var) == FALSE){
    m_sp <- create_manip_space(basis, manip_var)
    r_m_sp <- rotate_manip_space(manip_space = m_sp, theta, phi)
    basis <- r_m_sp[, 1L:2L] ## Really rotated basis
  }
  tour_array <- array(basis, dim = c(dim(basis), 1))
  attr(tour_array, "manip_var") <- manip_var
  
  ## Render
  df_frames <- array2df(array = tour_array, data = data, label = label)
  return(render_(frames = df_frames, ...))
}

#### Treat past alternative versions as view_frame, will work with fully qualified code.
#' @rdname spinifex-deprecated
#' @section \code{view_basis}:
#' For \code{view_basis}, use \code{\link{view_frame}}.
#' @export
view_basis <- function(...) {
  .Deprecated("view_frame")
  view_frame(...)
}

#' @rdname spinifex-deprecated
#' @section \code{oblique_basis}:
#' For \code{oblique_basis}, use \code{\link{view_frame}}.
#' @export
oblique_basis <- function(...) {
  .Deprecated("view_frame")
  view_frame(...)
}


#' Plot projection frame and return the axes table.
#' 
#' Uses base graphics to plot the circle with axes representing
#' the projection frame. Returns the corresponding table.
#' 
#' @param basis A (p, d) orthonormal numeric matrix.
#' The linear combination the original variables contribute to projection space.
#' Required, no default.
#' @param manip_var Number of the column/dimension to rotate.
#' @param tilt angle in radians to rotate the projection plane.
#' Defaults to .1 * pi.
#' @param label Optional, character vector of `p` length, add name to the axes
#' in the reference frame, typically the variable names.
#' @param manip_col String of the color to highlight the `manip_var`.
#' @param manip_sp_col Color to illustrate the z direction, orthogonal to the
#' projection plane.
#' @param line_size The size of the lines of the unit circle and variable 
#' contributions of the basis. Defaults to 1.
#' @param text_size The size of the text labels of the variable 
#' contributions of the basis. Defaults to 5.
#' @param ggproto A list of ggplot2 function calls.
#' Anything that would be "added" to ggplot(); in the case of applying a theme,
#' `ggplot() + theme_bw()` becomes `ggproto = list(theme_bw())`.
#' Intended for aesthetic ggplot2 functions (not geom_* family).
#' @return ggplot object of the basis.
#' @export
#' @examples
#' dat_std <- scale_sd(wine[, 2:14])
#' bas <- basis_pca(dat_std)
#' mv <- manip_var_pca(dat_std)
#' 
#' view_manip_space(basis = bas, manip_var = mv)
#' 
#' view_manip_space(basis = bas, manip_var = mv,
#'                  tilt = 2/12 * pi, label = paste0("MyNm", 1:ncol(dat_std)),
#'                  manip_col = "purple", manip_sp_col = "orange", 
#'                  ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")))
view_manip_space <- function(basis,
                             manip_var,
                             tilt = .1 * pi,
                             label = paste0("x", 1:nrow(basis)),
                             manip_col = "blue",
                             manip_sp_col = "red",
                             line_size = 1L,
                             text_size = 5L,
                             ggproto = list(
                               theme_spinifex()
                             )
){
  #### Finds the angle between two vectors
  find_angle <- function(a, b)
    acos(sum(a * b) / (sqrt(sum(a * a)) * sqrt(sum(b * b))) )
  #### Makes a df semi-circle, with 1 row per degree
  make_curve <- function(ang_st = 0L,
                         ang_stop = 2L * pi) {
    n_degrees <- round(180L / pi * abs(ang_st - ang_stop))
    angle <- seq(ang_st, ang_stop, length = n_degrees)
    data.frame(x = cos(angle), y = sin(angle), z = sin(angle))
  }
  rot <- function(df, ang = tilt){
    dplyr::mutate(df, x = x * cos(ang), y = y * sin(ang), z = z * cos(ang))
  }
  ## Initialize
  p <- nrow(basis)
  m_sp <- as.data.frame(create_manip_space(basis, manip_var))
  colnames(m_sp) <- c("x", "y", "z")
  m_sp_r <- rot(m_sp)
  mvar   <- m_sp[manip_var, ]
  mvar_r <- m_sp_r[manip_var, ]
  ## Aesthetics
  col_v <- rep("grey80", p)
  col_v[manip_var] <- manip_col
  siz_v <- rep(line_size, p)
  siz_v[manip_var] <- 1L
  ## Axes circle and angles
  circ_r <- rot(make_curve())
  theta_ang <- find_angle(c(mvar$x, mvar$y), c(1L, 0L))
  theta_curve_r <- rot(.5 * make_curve(ang_st = 0L, ang_stop = theta_ang)) 
  mvar_ysign <- ifelse(mvar$y < 0L, -1L, 1L)
  theta_curve_r$y <- theta_curve_r$y * mvar_ysign
  phi_ang <- find_angle(c(m_sp_r$x, m_sp_r$y), c(m_sp_r$x, m_sp_r$z))
  phi_curve <- .4 * make_curve(ang_st = theta_ang, ang_stop = phi_ang)
  phi_curve$y <- phi_curve$y * mvar_ysign
  ### Center and rotate
  start_pt <- phi_curve[1L, 1L:2L]
  phi_curve_r <- rot(data.frame(x = start_pt$x + (phi_curve$y - start_pt$y),
                                y = start_pt$y + (phi_curve$x - start_pt$x),
                                z = phi_curve$z))
  
  ## Render (& implicit return)
  ggplot2::ggplot() +
    ggproto +
    ## Axes circle
    ggplot2::geom_path(
      data = circ_r,
      mapping = ggplot2::aes(x = x, y = y),
      color = manip_col, size = line_size, inherit.aes = FALSE) +
    ## Variable contributions on projection plane:
    ggplot2::geom_segment(
      data = m_sp_r,
      mapping = ggplot2::aes(x = x, y = y, xend = 0L, yend = 0L),
      size = siz_v, colour = col_v) +
    ggplot2::geom_text(
      data = m_sp_r,
      mapping = ggplot2::aes(x = x, y = y, label = label),
      size = text_size, colour = col_v, vjust = "outward", hjust = "outward") +
    ## Red manip space
    ggplot2::geom_path(
      data = circ_r,
      mapping = ggplot2::aes(x = x, y = z),
      color = manip_sp_col, size = line_size, inherit.aes = FALSE) +
    ggplot2::geom_segment(
      data = mvar_r,
      mapping = ggplot2::aes(x = x, y = z, xend = 0L, yend = 0L),
      size = 1, colour = manip_sp_col) +
    ggplot2::geom_segment(
      data = mvar_r,
      mapping = ggplot2::aes(x = x, y = z, xend = x, yend = y),
      size = line_size, colour = "grey80", linetype = 2L) +
    ggplot2::geom_text(
      data = mvar_r,
      mapping = ggplot2::aes(x = x, y = z, label = label[manip_var]),
      size = text_size, colour = manip_sp_col, vjust = "outward", hjust = "outward") +
    ## Label phi and theta
    ggplot2::geom_text(
      data = 1.2 * theta_curve_r[ceiling(nrow(theta_curve_r) / 2L), ],
      mapping = ggplot2::aes(x = x, y = y - .02, label = "theta"),
      color = manip_col, size = text_size, parse = TRUE) +
    ggplot2::geom_path(
      data = theta_curve_r,
      mapping = ggplot2::aes(x = x , y),
      color = manip_col, size = 0.2)
}

