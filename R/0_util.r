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
#' @param ... Optionally passes arguments to the projection points inside the 
#' aesthetics; `geom_point(aes(...))`.
#' @return ggplot object of the basis.
#' @import tourr
#' @export
#' @examples 
#' rb <- tourr::basis_random(4, 2)
#' view_basis(basis = rb)
#' 
#' flea_std <- tourr::rescale(tourr::flea[, 1:4])
#' view_basis(basis = rb, data = flea_std, axes = "bottomleft")
#'
#' view_basis(basis = rb, data = flea_std, axes = "right", 
#'            col = col_of(tourr::flea[, 7], "Paired"), 
#'            pch = pch_of(tourr::flea[,7]),
#'            alpha = .7, size = 2)
view_basis <- function(basis,
                       data = NULL,
                       lab  = NULL,
                       axes = "center",
                       ...) {
  ## Initialize
  p     <- nrow(basis)
  basis <- as.data.frame(basis) ## for plotting
  colnames(basis) <- c("x", "y")
  ## basis text label conditional handling
  .lab <- NULL
  if(is.null(lab) == FALSE){
    .lab <- rep(lab, p / length(lab))
  } else { ## lab is NULL
    if(is.null(data) == FALSE) {.lab <- abbreviate(colnames(data), 3L)
    } else { ## lab and data NULL
      .lab <- paste0("V", 1L:p)
    }
  }
  ## Ploting initialization
  gg <- 
    ggplot2::ggplot() +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_fixed()
  ## Project data if present
  axes_to <- data.frame(x = c(0, 1), y = c(0, 1))
  if (is.null(data) == FALSE) {
    proj <- as.data.frame(
      tourr::rescale(as.matrix(data) %*% as.matrix(basis)) - .5)
    colnames(proj) <- c("x", "y")
    axes_to <- proj
    ## Project data and plot
    gg <- gg + 
      ggplot2::scale_shape_identity() + ## Allows numeric pch for scale.
      ggplot2::geom_point(data = proj,
                          mapping = ggplot2::aes(x = x, y = y, ...),
      )
  }
  ## Add axes if needed
  if(axes != "off") {
    ## Initialize
    angle <- seq(0L, 2L * pi, length = 360L)
    circ <- data.frame(x = cos(angle), y = sin(angle))
    center <- set_axes_position(0L, axes, to = axes_to)
    circ <- set_axes_position(circ, axes, axes_to)
    disp_basis <- set_axes_position(basis, axes, axes_to)
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
  return(gg)
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
#' @param manip_col String of the color to highlight the `manip_var`.
#' @param tilt angle in radians to rotate the projection plane. 
#' Defaults to pi * 5/12.
#' @param z_col Color to illustrate the z direction or out of the projection 
#' plane.
#' @param lab Optional, character vector of `p` length, add name to the axes 
#' in the reference frame, typically the variable names.
#' @return ggplot object of the basis.
#' @export
#' @examples 
#' flea_std <- tourr::rescale(tourr::flea[, 1:6])
#' rb <- tourr::basis_random(ncol(flea_std), 2)
#' 
#' view_manip_space(basis = rb, manip_var = 4)
view_manip_space <- function(basis,
                             manip_var,
                             tilt = 1/12 * pi,
                             lab = paste0("V", 1:nrow(basis)),
                             manip_col = "blue",
                             z_col = "red") {
  ### NEEDS DOC for external:
  as_xyz_df <- function(mat) {
    colnames(mat) <- c("x", "y", "z")
    as.data.frame(mat)
  }
  
  #### Init local functions
  ## Make a curved line segment, 1 row per degree.
  make_curve <- function(rad_st = 0L, rad_stop = 2L * pi) {
    angle <- seq(rad_st, rad_stop, 
                 length = max(round(360L * abs(rad_st - rad_stop) / (2L * pi)), 3L) )
    as.matrix(data.frame(x = cos(angle), y = sin(angle), z = 0L))
  }
  ## Find the angle [in radians] between 2 vectors
  find_angle <- function(a, b) { 
    acos(sum(a*b) /
           (sqrt(sum(a * a)) * sqrt(sum(b * b))))
  }
  
  ### NEEDS DOC for external:
  R3x_of <- function(mat, angle){ ## https://en.wikipedia.org/wiki/Rotation_matrix#In_three_dimensions
    angle <- -angle ## Orientation as I imagine it defined, double check.
    mat <- as.matrix(mat)
    c <- cos(angle)
    s <- sin(angle)
    rot <- matrix(c(1L, 0L, 0L,
                    0L,  c, -s,
                    0L,  s,  c), ncol = 3L, byrow = TRUE)
    as_xyz_df(mat %*% rot)
  }
  ### NEEDS DOC for external:
  R3y_of <- function(mat, angle){ ## https://en.wikipedia.org/wiki/Rotation_matrix#In_three_dimensions
    angle <- -angle ## Orientation as I imagine it defined, double check.
    mat <- as.matrix(mat)
    c <- cos(angle)
    s <- sin(angle)
    rot <- matrix(c(  c, 0L, s,
                     0L, 1L, 0L,
                     -s, 0L, c), ncol = 3L, byrow = TRUE)
    as_xyz_df(mat %*% rot)
  }
  ### NEEDS DOC for external:
  R3z_of <- function(mat, angle){ ## https://en.wikipedia.org/wiki/Rotation_matrix#In_three_dimensions
    angle <- -angle ## Orientation as I imagine it defined, double check.
    mat <- as.matrix(mat)
    c <- cos(angle)
    s <- sin(angle)
    rot <- matrix(c(c,  -s, 0L,
                    s,   c, 0L,
                    0L, 0L, 1L), ncol = 3, byrow = T)
    as_xyz_df(mat %*% rot)
  }
  
  
  #### Initialize
  p <- nrow(basis)
  m_sp <- as.matrix(create_manip_space(basis, manip_var))
  ## manip var asethetics
  col_v            <- rep("grey80", p)
  col_v[manip_var] <- manip_col
  siz_v            <- rep(0.3, p)
  siz_v[manip_var] <- 1L
  
  theta <- find_angle(m_sp[manip_var, 1L:2L], 
                      c(1L, 0L)) * sign(m_sp[manip_var, 2L])
  phi   <- find_angle(m_sp[manip_var, ], c(m_sp[manip_var, 1L:2L], 0L))
  
  circ          <- make_curve()
  theta_curve_r <- R3x_of(make_curve(0L, theta) / 5L, tilt)
  phi_curve_r   <- R3x_of(make_curve(0L, phi)   / 3L, tilt) 
  ##TODO: needs to be based and oriented from end of V4.
  ## already have this point in the grey projection line, pare back to that?
  phi_curve_r   <- R3z_of(phi_curve_r, theta) ##TODO: theta isn't suppose to be tilt?
  
  midpt_theta   <- round(nrow(theta_curve_r) / 2L)
  midpt_phi     <- round(nrow(phi_curve_r)   / 2L)
  
  m_sp_pp <- cbind(m_sp[, 1L:2L], 0L) ## *_pp; on projection plane
  circ_r  <- R3x_of(circ, tilt)
  m_sp_r  <- R3x_of(m_sp_pp, tilt)
  m_sp_z  <- data.frame(x    = m_sp_pp[manip_var, 1L],
                        y    = m_sp_pp[manip_var, 2L],
                        z    = m_sp_pp[manip_var, 3L],
                        xend =  m_sp_r[manip_var, 1L],
                        yend =  m_sp_r[manip_var, 3L],
                        lab  = lab[manip_var])
  
  
  ## Ploting
  gg <- 
    ## ggplot options
    ggplot2::ggplot() + 
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_fixed() +
    ## xy circle path 
    ggplot2::geom_path(data = circ_r, 
                       mapping = ggplot2::aes(x = x, y = z), 
                       color = manip_col, size = .3, inherit.aes = FALSE) +
    ## xy axes line segments
    ggplot2::geom_segment(data = m_sp_r, 
                          mapping = ggplot2::aes(x = x, y = z, xend = 0L, yend = 0L),
                          size = siz_v, color = col_v) +
    ## xy variable text labels
    ggplot2::geom_text(data = m_sp_r, 
                       mapping = ggplot2::aes(x = x, y = z, label = lab), 
                       size = 4L, color = col_v, 
                       vjust = "outward", hjust = "outward") +
    ## z circle path
    ggplot2::geom_path(data = circ_r,
                       mapping = ggplot2::aes(x = x, y = y),
                       color = z_col, size = .3, inherit.aes = FALSE) +
    ## z red manip axis segment
    ggplot2::geom_segment(data = m_sp_z,
                          mapping = ggplot2::aes(x = x, y = y, xend = 0L, yend = 0L),
                          size = 1L, color = z_col) +
    ## z grey line segment dropping down to prjojection plane
    ggplot2::geom_segment(data = m_sp_z,
                          mapping = ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                          size = .3, color = "grey80", linetype = 3L) +
    ## z red manip axis text label
    ggplot2::geom_text(data = m_sp_z,
                       mapping = ggplot2::aes(x = x, y = y, label = lab),
                       size = 4L, color = z_col,
                       vjust = "outward", hjust = "outward") +
    ## xz theta curve path
    ggplot2::geom_path(data = theta_curve_r,
                       mapping = ggplot2::aes(x = x, y = z),
                       color = manip_col, size = .3, linetype = 3L, inherit.aes = F) +
    ## xz theta text
    ggplot2::geom_text(data = theta_curve_r[midpt_theta, ] * 1.3,
                       mapping = ggplot2::aes(x = x, y = z, label = "theta"),
                       size = 4L, color = manip_col, parse = T,
                       vjust = "outward", hjust = "outward") +
    ## z phi curve path
    ## TODO: needs to go to phi theta_curve_r
    ggplot2::geom_path(data = phi_curve_r,
                       mapping = ggplot2::aes(x = x, y = y),
                       color = z_col, size = .3, linetype = 3L, inherit.aes = FALSE) +
    ## z phi text
    ggplot2::geom_text(data = phi_curve_r[midpt_phi, ] * 1.2,
                       mapping = ggplot2::aes(x = x, y = y, label = "phi"),
                       size = 4L, color = z_col, parse = TRUE,
                       vjust = "outward", hjust = "outward")
  
  gg
}

### FORMATING ------

#' Return hex color code for a given discrete categorical variable.
#' 
#' @param class The discrete categorical variable to return the color of.
#' @param pallet_name The name of the `RColorBrewer` pallet to get the colors
#' from. Defaults to "Dark2".
#' @return Vector of character hex color code of the passed categorical variable.
#' @export
#' @examples 
#' col_of(tourr::flea$species)
col_of <- function(class, pallet_name = "Dark2") {
  class <- as.factor(class)
  .l_lvls <- length(levels(class))
  if (.l_lvls == 0L) stop("Length of 'class' cannot be zero.")
  if (.l_lvls > 12L) stop("'class' has more than the expected max of 12 levels.")
  pal <- suppressWarnings(RColorBrewer::brewer.pal(.l_lvls, pallet_name))
  pal[as.integer(factor(class))]
}
#' Return shape integers for a given discrete categorical variable.
#' 
#' @param class The discrete categorical variable to return the shape of.
#' @return Vector of integer shape values of the discrete categorical variable.
#' @export
#' @examples 
#' pch_of(tourr::flea$species)
pch_of <- function(class) {
  class <- as.factor(class)
  .shape_ord <- c(21L:25L, 3L:4L, 7L:14L)
  .l_shapes  <- length(unique(.shape_ord))
  class <- as.factor(class)
  .l_classes <- length(levels(class))
  if (.l_classes == 0L) stop("Length of 'class' cannot be zero.")
  if (.l_classes > 12L)
    stop(paste0("'class' has more than the expected max of ", .l_shapes, " levels."))
  .int_lvls <- as.integer(class)
  .shape_ord[.int_lvls]
}


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
#' @param x Numeric data object to scale and offset.
#' @param axes Position of the axes: "center", "bottomleft" or "off". Defaults 
#' to "center".
#' @param to output range (numeric vector of length two). Min and max that x is 
#' scaled to.
#' @return Scaled and offset `x` typically controling axes placement.
#' @seealso \code{\link{pan_zoom}} for more manual control.
#' @export
#' @examples
#' rb <- tourr::basis_random(4, 2)
#' set_axes_position(x = rb, axes = "bottomleft")
#' set_axes_position(x = rb, axes = "right", to = mtcars[, 1:2])
set_axes_position <- function(x, 
                              axes = c("center", "bottomleft", "topright", "off", "left", "right"), 
                              to = data.frame(x = c(0, 1), y = c(0, 1))
) {
  ## Assumptions
  if (axes == "off") return()
  if (length(x) == 1L) {x <- data.frame(x = x, y = x)}
  stopifnot(ncol(x) == 2L)
  axes <- match.arg(tolower(axes), several.ok = FALSE,
                    choices = c("center", "bottomleft", "topright", "off", "left", "right"))
  ## Initialize
  x_to <- c(min(to[, 1L]), max(to[, 1L]))
  y_to <- c(min(to[, 2L]), max(to[, 2L]))
  xdiff   <- diff(x_to)
  xcenter <- xdiff / 2L
  ydiff   <- diff(y_to)
  ycenter <- ydiff / 2L
  ## Condition handling of axes.
  if (axes == "center") {
    xscale <- 2L / 3L * xdiff
    yscale <- 2L / 3L * ydiff
    xoff  <- xcenter
    yoff  <- ycenter
  } else if (axes == "bottomleft") {
    xscale <- 1L / 4L * xdiff
    yscale <- 1L / 4L * ydiff
    xoff <- -2L / 3L * xdiff + xcenter
    yoff <- -2L / 3L * ydiff + ycenter
  } else if (axes == "topright") {
    xscale <- 1L / 4L * xdiff
    yscale <- 1L / 4L * ydiff
    xoff <- 2L / 3L * xdiff + xcenter
    yoff <- 2L / 3L * ydiff + ycenter
  } else if (axes == "left") {
    xscale <- 2L / 3L * xdiff
    yscale <- 2L / 3L * ydiff
    xoff <- -5L / 3L * xdiff + xcenter
    yoff <- ycenter
  } else if (axes == "right") {
    xscale <- 2L / 3L * xdiff
    xoff <- 5L / 3L * xdiff + xcenter
    yoff <- ycenter
  }
  ## Apply scale and return
  x[, 1L] <- xscale * x[, 1L] + xoff
  x[, 2L] <- yscale * x[, 2L] + yoff
  return(x)
}



#' Pan (offset) and zoom (scale) a 2 column matrix, dataframe or scaler number.
#' 
#' @param x Numeric data object with 2 columns (or scaler) to scale and offset.
#' @param x_pan Numeric value to offset/pan in the x-direction.
#' @param y_pan Numeric value to offset/pan in the y-direction.
#' @param x_zoom Numeric value to scale/zoom the size for the 1st column of `x`.
#' @param y_zoom Numeric value to scale/zoom the size for the 2nd column of `x`.
#' @return Scaled and offset `x`. A manual variant of `set_axes_position()`.
#' @seealso \code{\link{set_axes_position}} for preset choices.
#' @export
#' @examples 
#' ib <- tourr::basis_init(6, 2)
#' pan_zoom(x = ib, x_pan = -1, y_pan = 0, x_zomm = 2/3, y_zoom = 1/2)
pan_zoom <- function(x, 
                     x_pan = 0L, 
                     y_pan = 0L, 
                     x_zoom = 1L,
                     y_zoom = 1L
) {
  ## Assumptions
  if (length(x) == 1L) {x <- data.frame(x = x, y = x)}
  if (ncol(x) != 2L) stop("pan_zoom is only defined for 2 variables.")
  ## Apply scale and return
  ret[, 1L] <- ret[, 1L] * x_zoom + x_offset 
  ret[, 2L] <- ret[, 2L] * y_zoom + y_offset
  return(ret)
}
