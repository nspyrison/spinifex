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
#' @param rescale_data When TRUE scales the data columns to between 0 and 1.
#' Defaults to FALSE.
#' @param angle Target distance (in radians) between steps. Defaults to .05.
#' @param render_type Graphics to render to. Defaults to `render_plotly``,
#' alternative use `render_gganimate`.
#' @param ... Optionally pass additional arguments to the `render_type`.
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
#'                axes = "bottomleft", angle = .08, fps = 8,
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
  anim <- render_type(frames = tour_df, ...)
  
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
#'                    theta = .5 * pi, axes = "left", fps = 10,
#'                    col = flea_class, pch = flea_class, size = 1.5,
#'                    ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")),
#'                    render_type = render_plotly,
#'                    html_filename = "myRadialTour.html")
#'   
#'   ## Export gganimate .gif
#'   play_manual_tour(basis = rb, data = flea_std, manip_var = 1,
#'                    theta = 0, axes = "topright", fps = 8,
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
                             ggproto = theme_spinifex(),
                             render_type = render_plotly,
                             ...){
  if (is.null(basis) & is.null(data)) stop("basis or data must be supplied.")
  ## Basis condition handling
  if (is.null(basis) & !is.null(data)){
    basis <- stats::prcomp(data)$rotation[, 1L:2L]
    message("NULL basis passed. Set to PCA basis.")
  }
  ## manip_var condition handling
  if (is.null(manip_var) & !is.null(data)) {
    manip_var <- which(abs(basis[, 1L]) == max(abs(basis[, 1L])))
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

##
## HELPER & INTERMEDIATE VISUALIZATIONS -----
##

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
#' @param lab Optional, character vector of `p` length, add name to the axes
#' in the reference frame, typically the variable names.
#' @param manip_col String of the color to highlight the `manip_var`.
#' @param manip_sp_col Color to illustrate the z direction, orthogonal to the
#' projection plane.
#' @param ggproto Accepts a list of gg functions. Think of this as an
#' alternative format to `ggplot() + ggproto[[1]]`.
#' Intended for passing a `ggplot2::theme_*()` and related aesthetic functions.
#' @return ggplot object of the basis.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[, 1:6])
#' rb <- tourr::basis_random(ncol(flea_std))
#' 
#' view_manip_space(basis = rb, manip_var = 4)
#' 
#' view_manip_space(basis = rb, manip_var = 1,
#'                  tilt = 2/12 * pi, lab = paste0("MyLabs", 1:nrow(basis))
#'                  manip_col = "purple", manip_sp_col = "orange", 
#'                  ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")))
view_manip_space <- function(basis,
                             manip_var,
                             tilt = .1 * pi,
                             lab = paste0("V", 1:nrow(basis)),
                             manip_col = "blue",
                             manip_sp_col = "red",
                             ggproto = list(
                               ggplot2::scale_color_brewer(palette = "Dark2"),
                               ggplot2::theme_void(),
                               ggplot2::theme(legend.position = "none")
                             )
){
  #### Finds the angle between two vectors
  find_angle <- function(a, b)
    acos(sum(a * b) / (sqrt(sum(a * a)) * sqrt(sum(b * b))) )
  #### Makes a df semi-circle, with 1 row per degree
  make_curve <- function(ang_st = 0L,
                         ang_stop = 2L * pi) {
    degrees <- round(360L / (2L * pi) * abs(ang_st - ang_stop))
    angle <- seq(ang_st, ang_stop, length = degrees)
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
  siz_v <- rep(0.3, p)
  siz_v[manip_var] <- 1L
  ## Axes circle and angles
  circ_r <- rot(make_curve())
  theta_ang <- find_angle(c(mvar$x, mvar$y),c(1L, 0L))
  theta_curve_r <- .5 * make_curve(ang_st = 0L, ang_stop = theta_ang) %>% rot()
  theta_curve_r$y <- theta_curve_r$y * sign(mvar$y)
  phi_ang <- find_angle(c(m_sp_r$x, m_sp_r$y), c(m_sp_r$x, m_sp_r$z))
  phi_curve <- .4 * make_curve(ang_st = theta_ang, ang_stop = phi_ang)
  phi_curve$y <- phi_curve$y * sign(mvar$y)
  ### Move to origin, rotate about blue manip by 90 degrees, move back
  start_pt <- phi_curve[1, 1:2]
  phi_curve <- phi_curve %>%
    dplyr::mutate(x = x - start_pt$x,
                  y = y - start_pt$y)
  tmp_x <- phi_curve$y + start_pt$x
  tmp_y <- phi_curve$x + start_pt$y
  phi_curve_r <- data.frame(x = tmp_x, y = tmp_y, z = phi_curve$z) %>% rot()
  
  ## Render (& implicit return)
  ggplot2::ggplot() + 
    ggproto +
    ## Axes circle
    ggplot2::geom_path(
      data = circ_r,
      mapping = ggplot2::aes(x = x, y = y),
      color = manip_col, size = 0.3, inherit.aes = FALSE) +
    ## Variable contributions on projection plane:
    ggplot2::geom_segment(
      data = m_sp_r,
      mapping = ggplot2::aes(x = x, y = y, xend = 0L, yend = 0L),
      size = siz_v, colour = col_v) +
    ggplot2::geom_text(
      data = m_sp_r,
      mapping = ggplot2::aes(x = x, y = y, label = lab),
      size = 4, colour = col_v, vjust = "outward", hjust = "outward") +
    ## Red manip space
    ggplot2::geom_path(
      data = circ_r,
      mapping = ggplot2::aes(x = x, y = z),
      color = manip_sp_col, size = 0.3, inherit.aes = FALSE) +
    ggplot2::geom_segment(
      data = mvar_r,
      mapping = ggplot2::aes(x = x, y = z, xend = 0L, yend = 0L),
      size = 1, colour = manip_sp_col) +
    ggplot2::geom_segment(
      data = mvar_r,
      mapping = ggplot2::aes(x = x, y = z, xend = x, yend = y),
      size = 0.3, colour = "grey80", linetype = 2) +
    ggplot2::geom_text(
      data = mvar_r,
      mapping = ggplot2::aes(x = x, y = z, label = lab[manip_var]),
      size = 4, colour = manip_sp_col, vjust = "outward", hjust = "outward") +
    ## Label phi and theta
    ggplot2::geom_text(
      data = 1.2 * theta_curve_r[ceiling(nrow(theta_curve_r)/2), ],
      mapping = ggplot2::aes(x = x, y = y - .02, label = "theta"),
      color = manip_col, size = 4L, parse = TRUE) +
    ggplot2::geom_path(
      data = theta_curve_r,
      mapping = ggplot2::aes(x = x , y),
      color = manip_col, size = 0.2) #+
  # ggplot2::geom_text(
  #   data = 1.2 * phi_curve_r[ceiling(nrow(phi_curve_r) / 2L), ],
  #   mapping = ggplot2::aes(x = x, y = y, label = "phi"),
  #   color = manip_sp_col, size = 4L, parse = TRUE) +
  # ggplot2::geom_path(
  #   data = phi_curve_r,
  #   mapping = ggplot2::aes(x = x, y = y),
  #   color = manip_sp_col, size = 0.2)
  
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
view_frame <- function(basis = NULL,
                       data = NULL,
                       manip_var = NULL,
                       theta = 0L,
                       phi = 0L,
                       lab = NULL,
                       rescale_data = FALSE,
                       ggproto = theme_spinifex(),
                       ...){
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

#' @rdname view_basis
#' @export
view_basis <- function(...){
  .Deprecated("view_frame")
  view_frame(...)
}
  
#' @rdname oblique_basis
#' @export
oblique_basis <- function(...){
  .Deprecated("view_frame")
  view_frame(...)
}


# ##
# ## OLD CODE: VIEW_BASIS -----
# ##

# #' Plot the axes directions of the basis table without data points.
# #' 
# #' ggplot2 object of axes contribution inscribed in a unit circle.
# #' 
# #' @param basis A (p, d) orthonormal numeric matrix.
# #' The linear combination the original variables contribute to projection space.
# #' Required, no default.
# #' @param data Optional (n, p) data to plot on through the projection basis.
# #' @param lab Optional, labels for the reference frame of length 1 or the
# #' number of variables used. By default will abbreviate data if available.
# #' @param axes Position of the axes: "center", "bottomleft" or "off". Defaults
# #' to "center".
# #' @param ggproto Accepts a list of gg functions. Think of this as an 
# #' alternative format to `ggplot() + ggproto[[1]]`.
# #' Intended for passing a `ggplot2::theme_*()` and related aesthetic functions.
# #' @param ... Optionally passes arguments to the projection points inside the
# #' aesthetics; `geom_point(aes(...))`.
# #' @return ggplot object of the basis.
# #' @import tourr
# #' @export
# #' @examples 
# #' flea_std <- tourr::rescale(tourr::flea[, 1:4])
# #' rb <- tourr::basis_random(ncol(flea_std))
# #' flea_class <- tourr::flea$species
# #' view_basis(basis = rb)
# #' 
# #' view_basis(basis = rb, data = flea_std, axes = "bottomleft")
# #'
# #' view_basis(basis = rb, data = flea_std, axes = "right",
# #'            color = flea_class, shape = flea_class, alpha = .7, size = 2,
# #'            ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")))
# view_basis <- function(basis,
#                        data = NULL,
#                        lab  = NULL,
#                        axes = "center",
#                        ggproto = theme_spinifex(),
#                        ...) {
#   .Deprecated("view_frame")
#   ## Initialize
#   p     <- nrow(basis)
#   basis <- as.data.frame(basis) ## for plotting
#   colnames(basis) <- c("x", "y")
#   axes_to <- data.frame(x = c(0L, 1L), y = c(0L, 1L))
#   
#   ## Basis text label conditional handling
#   .lab <- NULL
#   if(is.null(lab) == FALSE){
#     .lab <- rep(lab, p / length(lab))
#   } else { ## lab is NULL
#     if(is.null(data) == FALSE) {.lab <- abbreviate(colnames(data), 3L)
#     } else { ## lab and data NULL
#       .lab <- paste0("V", 1L:p)
#     }
#   }
#   
#   ## Settings and asethetics 
#   gg <- 
#     ggplot2::ggplot() +
#     ggproto
#   ## Initalize data if present
#   if (is.null(data) == FALSE) {
#     proj <- as.data.frame(
#       tourr::rescale(as.matrix(data) %*% as.matrix(basis)) - .5)
#     colnames(proj) <- c("x", "y")
#     axes_to <- proj
#     ## Rendering
#     gg <- gg + 
#       ggplot2::geom_point(data = proj,
#                           mapping = ggplot2::aes(x = x, y = y, ...),
#       )
#   }
#   ## Add axes if needed
#   if(axes != "off") {
#     ## Initialize
#     angle <- seq(0L, 2L * pi, length = 360L)
#     circ <- data.frame(x = cos(angle), y = sin(angle))
#     center <- scale_axes(data.frame(x = 0L, y = 0L),
#                          axes, to = axes_to)
#     circ <- scale_axes(circ, axes, axes_to)
#     disp_basis <- scale_axes(basis, axes, axes_to)
#     ## Append
#     gg <- gg +
#       ## Axes unit cirle path
#       ggplot2::geom_path(
#         data = circ, 
#         mapping = ggplot2::aes(x = x, y = y),
#         color = "grey80", size = .3, inherit.aes = FALSE) +
#       ## Basis axes line segments
#       ggplot2::geom_segment(
#         data = disp_basis, 
#         mapping = ggplot2::aes(x = x, y = y, xend = center[, 1L], yend = center[, 2L])) +
#       ## Basis variable text labels
#       ggplot2::geom_text(
#         data = disp_basis, 
#         mapping = ggplot2::aes(x = x, y = y), label = .lab,
#         size = 4L, hjust = 0L, vjust = 0L, color = "black")
#   }
#   
#   ## Return
#   gg
# }


