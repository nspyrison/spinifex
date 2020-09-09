### DISPLAYS -----

#' Plot the axes directions of the basis table without data points.
#' 
#' ggplot2 object of axes contribution inscribed in a unit circle.
#' 
#' @param basis A (p, d) orthonormal numeric matrix.
#' The linear combination the original variables contribute to projection space.
#' Required, no default.
#' @param data Optional (n, p) data to plot on through the projection basis.
#' @param lab Optional, labels for the reference frame of length 1 or the
#' number of variables used. By default will abbreviate data if available.
#' @param axes Position of the axes: "center", "bottomleft" or "off". Defaults
#' to "center".
#' @param ggproto Accepts a list of gg functions. Think of this as an 
#' alternative format to `ggplot() + ggproto[[1]]`.
#' Intended for passing a `ggplot2::theme_*()` and related aesthetic functions.
#' @param ... Optionally passes arguments to the projection points inside the
#' aesthetics; `geom_point(aes(...))`.
#' @return ggplot object of the basis.
#' @import tourr
#' @export
#' @examples 
#' flea_std <- tourr::rescale(tourr::flea[, 1:4])
#' rb <- tourr::basis_random(ncol(flea_std))
#' flea_class <- tourr::flea$species
#' view_basis(basis = rb)
#' 
#' view_basis(basis = rb, data = flea_std, axes = "bottomleft")
#'
#' view_basis(basis = rb, data = flea_std, axes = "right",
#'            color = flea_class, shape = flea_class, alpha = .7, size = 2,
#'            ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")))
view_basis <- function(basis,
                       data = NULL,
                       lab  = NULL,
                       axes = "center",
                       ggproto = ggplot2::theme_void(),
                       ...) {
  .Deprecated("view_frame")
  ## Initialize
  p     <- nrow(basis)
  basis <- as.data.frame(basis) ## for plotting
  colnames(basis) <- c("x", "y")
  axes_to <- data.frame(x = c(0L, 1L), y = c(0L, 1L))
  
  ## Basis text label conditional handling
  .lab <- NULL
  if(is.null(lab) == FALSE){
    .lab <- rep(lab, p / length(lab))
  } else { ## lab is NULL
    if(is.null(data) == FALSE) {.lab <- abbreviate(colnames(data), 3L)
    } else { ## lab and data NULL
      .lab <- paste0("V", 1L:p)
    }
  }
  
  ## Settings and asethetics 
  gg <- 
    ggplot2::ggplot() +
    ggplot2::coord_fixed() +
    ggproto
  ## Initalize data if present
  if (is.null(data) == FALSE) {
    proj <- as.data.frame(
      tourr::rescale(as.matrix(data) %*% as.matrix(basis)) - .5)
    colnames(proj) <- c("x", "y")
    axes_to <- proj
    ## Rendering
    gg <- gg + 
      ggplot2::geom_point(data = proj,
                          mapping = ggplot2::aes(x = x, y = y, ...),
      )
  }
  ## Add axes if needed
  if(axes != "off") {
    ## Initialize
    angle <- seq(0L, 2L * pi, length = 360L)
    circ <- data.frame(x = cos(angle), y = sin(angle))
    center <- scale_axes(data.frame(x = 0L, y = 0L),
                         axes, to = axes_to)
    circ <- scale_axes(circ, axes, axes_to)
    disp_basis <- scale_axes(basis, axes, axes_to)
    ## Append
    gg <- gg +
      ## Axes unit cirle path
      ggplot2::geom_path(
        data = circ, 
        mapping = ggplot2::aes(x = x, y = y),
        color = "grey80", size = .3, inherit.aes = FALSE) +
      ## Basis axes line segments
      ggplot2::geom_segment(
        data = disp_basis, 
        mapping = ggplot2::aes(x = x, y = y, xend = center[, 1L], yend = center[, 2L])) +
      ## Basis variable text labels
      ggplot2::geom_text(
        data = disp_basis, 
        mapping = ggplot2::aes(x = x, y = y), label = .lab,
        size = 4L, hjust = 0L, vjust = 0L, color = "black")
  }
  
  ## Return
  gg
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
#' Defaults to pi * 5/12.
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
                             tilt = 1 / 12 * pi,
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
    ggplot2::coord_fixed() +
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

# ### FORMATING ------
# #' Return hex color code for a given discrete categorical variable.
# #' 
# #' @param class The discrete categorical variable to return the color of.
# #' @param pallet_name The name of the `RColorBrewer` pallet to get the colors
# #' from. Defaults to "Dark2".
# #' @return Vector of character hex color code of the passed categorical variable.
# #' @export
# #' @examples 
# #' color_of(tourr::flea$species)
# color_of <- function(class, pallet_name = "Dark2") {
#   class <- as.factor(class)
#   .l_lvls <- length(levels(class))
#   if (.l_lvls == 0L) stop("Length of 'class' cannot be zero.")
#   if (.l_lvls > 12L) stop("'class' has more than the expected max of 12 levels.")
#   pal <- suppressWarnings(RColorBrewer::brewer.pal(.l_lvls, pallet_name))
#   pal[as.integer(factor(class))]
# }
# #' Return shape integers for a given discrete categorical variable.
# #' 
# #' @param class The discrete categorical variable to return the shape of.
# #' @return Vector of integer shape values of the discrete categorical variable.
# #' @export
# #' @examples 
# #' shape_of(tourr::flea$species)
# shape_of <- function(class) {
#   class <- as.factor(as.vector(class))
#   .shape_ord <- c(21L:25L, 3L:4L, 7L:14L)
#   .l_shapes  <- length(unique(.shape_ord))
#   class <- as.factor(class)
#   .l_classes <- length(levels(class))
#   if (.l_classes == 0L) stop("Length of 'class' cannot be zero.")
#   if (.l_classes > 12L)
#     stop(paste0("'class' has more than the expected max of ", .l_shapes, " levels."))
#   .int_lvls <- as.integer(class)
#   .shape_ord[.int_lvls]
# }


### MATH AND TRANSFORMS -----
#' Test if a numeric matrix is orthonormal.
#'
#' Handles more cases than tourr::is_orthonormal().
#'
#' @param x Numeric matrix to test the orthonormality of.
#' @param tol Tolerance of (the sum of element-wise) floating point differences.
#' @return Single logical of the orthonormal matrix of the matrix.
#' @export
#' @examples 
#' is_orthonormal(tourr::basis_random(n = 6))
#' is_orthonormal(matrix(1:12, ncol=2), tol = 0.01)
is_orthonormal <- function(x, tol = 0.001) { ## (tol)erance of SUM of element-wise error.
  x <- as.matrix(x)
  actual <- t(x) %*% x ## Collapses to identity matrix IFF x is orthonormal
  expected <- diag(ncol(x))
  if (max(actual - expected) < tol) {TRUE} else {FALSE}
}


#' Returns the axis scale and position.
#' 
#' Typically called, by other functions to scale axes.
#' 
#' @param x Numeric table, first 2 coulmns and scaled and offset relative to 
#' the `to` argument.
#' @param position Text specifiyinh the postision the axes should go to.
#' Defaults to "center" expects one of: "center", "left", "right", 
#' "bottomleft", "topright", or "off".
#' @param to Table to appropriately set the size and position of the axes to.
#' Based on the min/max of the first 2 columns.
#' @return Scaled and offset `x` typically controling axes placement.
#' @seealso \code{\link{pan_zoom}} for more manual control.
#' @export
#' @examples
#' rb <- tourr::basis_random(4, 2)
#' scale_axes(x = rb, position = "bottomleft")
#' scale_axes(x = rb, position = "right", to = wine[, 2:3])
scale_axes <- function(x, 
                       position = c("center", "left", "right", "bottomleft", "topright", "off"), 
                       to = data.frame(x = c(0, 1), y = c(0, 1))
){
  ## Assumptions
  if (position == "off")   return()
  if (is.null(to))     to <- data.frame(x = c(0L, 1L), y = c(0L, 1L))
  stopifnot(ncol(x) == 2L)
  position <- match.arg(tolower(position), several.ok = FALSE,
                    choices = c("center", "bottomleft", "topright", "off", "left", "right"))
  ## Initialize
  x_to <- c(min(to[, 1L]), max(to[, 1L]))
  y_to <- c(min(to[, 2L]), max(to[, 2L]))
  xdiff   <- diff(x_to)
  xcenter <- xdiff / 2L
  ydiff   <- diff(y_to)
  ycenter <- ydiff / 2L
  
  ## Condition handling of axes.
  if (position == "center") {
    xscale <- 2L / 3L * xdiff
    yscale <- 2L / 3L * ydiff
    xoff  <- xcenter
    yoff  <- ycenter
  } else if (position == "bottomleft") {
    xscale <- 1L / 4L * xdiff
    yscale <- 1L / 4L * ydiff
    xoff <- -2L / 3L * xdiff + xcenter
    yoff <- -2L / 3L * ydiff + ycenter
  } else if (position == "topright") {
    xscale <- 1L / 4L * xdiff
    yscale <- 1L / 4L * ydiff
    xoff <- 2L / 3L * xdiff + xcenter
    yoff <- 2L / 3L * ydiff + ycenter
  } else if (position == "left") {
    xscale <- 2L / 3L * xdiff
    yscale <- 2L / 3L * ydiff
    xoff <- -5L / 3L * xdiff + xcenter
    yoff <- ycenter
  } else if (position == "right") {
    xscale <- 2L / 3L * xdiff
    yscale <- 2L / 3L * ydiff
    xoff <- 5L / 3L * xdiff + xcenter
    yoff <- ycenter
  }
  
  ## Apply scale and return
  x[, 1L] <- xscale * x[, 1L] + xoff
  x[, 2L] <- yscale * x[, 2L] + yoff
  return(x)
}



#' Pan (offset) and zoom (scale) a 2 column matrix, dataframe or scalar number.
#' 
#' @param x Numeric data object with 2 columns (or scalar) to scale and offset.
#' @param x_pan Numeric value to offset/pan in the x-direction.
#' @param y_pan Numeric value to offset/pan in the y-direction.
#' @param x_zoom Numeric value to scale/zoom the size for the 1st column of `x`.
#' @param y_zoom Numeric value to scale/zoom the size for the 2nd column of `x`.
#' @return Scaled and offset `x`. A manual variant of `scale_axes()`.
#' @seealso \code{\link{scale_axes}} for preset choices.
#' @export
#' @examples 
#' ib <- tourr::basis_init(6, 2)
#' pan_zoom(x = ib, x_pan = -1, y_pan = 0, x_zoom = 2/3, y_zoom = 1/2)
pan_zoom <- function(x, 
                     x_pan = 0L, 
                     y_pan = 0L, 
                     x_zoom = 1L,
                     y_zoom = 1L
) {
  ## Assumptions
  if (ncol(x) != 2L) stop("pan_zoom is only defined for 2 variables.")
  ## Apply scale and return
  ret <- x
  ret[, 1L] <- ret[, 1L] * x_zoom + x_pan 
  ret[, 2L] <- ret[, 2L] * y_zoom + y_pan
  return(ret)
}
