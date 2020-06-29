### DISPLAYS -----

#' Plot the axes directions of the basis table without data points
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
#' @param col Color of the projected points. Defaults to "black".
#' @param pch Point character of the projected points. Defaults to 20.
#' @param cex Point size of the data. Defaults to 1.
#' @param alpha Opacity of the data points between 0 and 1. Defaults to 1.
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
  # Initialize
  p     <- nrow(basis)
  basis <- as.data.frame(basis) ## for plotting
  colnames(basis) <- c("x", "y")
  
  .lab <- NULL
  if(is.null(lab) == FALSE){
    .lab <- rep(lab, p / length(lab))
  } else { ## lab is NULL
    if(is.null(data) == FALSE) {.lab <- abbreviate(colnames(data), 3)
    } else { ## lab and data NULL
      .lab <- paste0("V", 1:p)
    }
  }
  
  ## Plot
  gg <- 
    ggplot2::ggplot() +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_fixed() ## Do not use with plotly!
  
  if(axes != "off") ## Then add axes
  {
    angle <- seq(0, 2 * pi, length = 360)
    circ  <- data.frame(x = cos(angle), y = sin(angle))
    zero  <- set_axes_position(0,     axes)
    disp_basis <- set_axes_position(basis, axes)
    circ  <- set_axes_position(circ,  axes)
    
    gg <- gg +
    ## Axes unit cirle path
    ggplot2::geom_path(
      data = circ, 
      mapping = ggplot2::aes(x = x, y = y),
      color = "grey80", size = .3, inherit.aes = F) +
    ## Basis axes line segments
    ggplot2::geom_segment(
      data = disp_basis, 
      mapping = ggplot2::aes(x = x, y = y, xend = zero[, 1], yend = zero[, 2])) +
    ## Basis variable text labels
    ggplot2::geom_text(
      data = disp_basis, 
      mapping = ggplot2::aes(x = x, y = y), label = .lab,
      size = 4, hjust = 0, vjust = 0, color = "black")
  }
  
  if (is.null(data) == FALSE){ ## Then project data
    proj <- as.data.frame(
      tourr::rescale(as.matrix(data) %*% as.matrix(basis)) - .5)
    colnames(proj) <- c("x", "y")
    
    ## Project data and plot
    gg <- gg + 
      ggplot2::scale_shape_identity() + ## to allow numeric pch for scale.
      ggplot2::geom_point(data = proj,
                          mapping = ggplot2::aes(
                            x = x, y = y),
                          ...
      )
  }
  
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
#'   Defaults to pi * 5/12.
#' @param z_col Color to illustrate the z direction or out of the projection 
#'   plane.
#' @param lab Optional, character vector of `p` length, add name to the axes 
#'   in the reference frame, typically the variable names.
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
  
  # Make a curved line segment between the 2 angles.
  # 
  # @param rad_st Angle in radians to start the curved line segment.
  # @param rad_stop Angle in radians to stop the curved line segment.
  # @return A numeric (n, 3)-D matrix. The curved line segment lies on xy-plane, 
  # z is fixed a 0.
  # @examples 
  # make_curve(rad_st = 1 / 4 * pi, rad_stop = 4/3 * pi)
  # @export
  make_curve <- function(rad_st = 0, rad_stop = 2 * pi){ # 1 obs per degree drawn
    angle <- seq(rad_st, rad_stop, 
                 length = max(round(360 * abs(rad_st - rad_stop) / (2 * pi)), 3) )
    as.matrix(data.frame(x = cos(angle), y = sin(angle), z = 0))
  }
  
  # Find the angle (in radians) between 2 vectors in 2D.
  # 
  # @param a Angle in radians to start the curved line segment.
  # @param b Angle in radians to stop the curved line segment.
  # @return A numeric (n, 3)-D matrix. The curved line segment lies on xy-plane,
  # z is fixed a 0.
  # @examples
  # make_curve(rad_st = 1 / 4 * pi, rad_stop = 4/3 * pi)
  # @export
  find_angle <- function(a, b) { ## Find the angle [radians] between 2 vectors
    acos(sum(a*b) / 
           (sqrt(sum(a * a)) * sqrt(sum(b * b))))
  }
  
  ### NEEDS DOC for external:
  R3x_of <- function(mat, angle){ ## https://en.wikipedia.org/wiki/Rotation_matrix#In_three_dimensions
    angle <- -angle ## Orientation as I imagine it defined, double check.
    mat <- as.matrix(mat)
    c <- cos(angle)
    s <- sin(angle)
    rot <- matrix(c(1, 0,  0,
                    0, c, -s,
                    0, s,  c), ncol = 3, byrow = T)
    as_xyz_df(mat %*% rot)
  }
  ### NEEDS DOC for external:
  R3y_of <- function(mat, angle){ ## https://en.wikipedia.org/wiki/Rotation_matrix#In_three_dimensions
    angle <- -angle ## Orientation as I imagine it defined, double check.
    mat <- as.matrix(mat)
    c <- cos(angle)
    s <- sin(angle)
    rot <- matrix(c( c, 0, s,
                     0, 1, 0,
                     -s, 0, c), ncol = 3, byrow = T)
    as_xyz_df(mat %*% rot)
  }
  ### NEEDS DOC for external:
  R3z_of <- function(mat, angle){ ## https://en.wikipedia.org/wiki/Rotation_matrix#In_three_dimensions
    angle <- -angle ## Orientation as I imagine it defined, double check.
    mat <- as.matrix(mat)
    c <- cos(angle)
    s <- sin(angle)
    rot <- matrix(c(c, -s, 0,
                    s,  c, 0,
                    0,  0, 1),   ncol = 3, byrow = T)
    as_xyz_df(mat %*% rot)
  }
  
  
  ### Initialize
  ## manip space
  m_sp <- as.matrix(create_manip_space(basis, manip_var))
  p <- nrow(m_sp)
  ## manip var asethetics
  col_v            <- rep("grey80", p)
  col_v[manip_var] <- manip_col
  siz_v            <- rep(0.3, p)
  siz_v[manip_var] <- 1
  
  ##### Square space
  theta <- find_angle(m_sp[manip_var, 1:2], c(1, 0)) * sign(m_sp[manip_var, 2])
  phi   <- find_angle(m_sp[manip_var, ],    c(m_sp[manip_var, 1:2], 0))
  
  circ          <- make_curve()
  theta_curve_r <- R3x_of(make_curve(0, theta) / 5, tilt)
  phi_curve_r   <- R3x_of(make_curve(0, phi) / 3, tilt) 
  ##TODO: NEEDS TO BE BASED AND ORIENTED FROM END OF V4
  # but we already have this in the grey projection line, pare back to that?
  phi_curve_r   <- R3z_of(phi_curve_r, theta) ##TODO: theta isn't suppose to be tilt?
  
  midpt_theta   <- round(nrow(theta_curve_r) / 2)
  midpt_phi     <- round(nrow(phi_curve_r) / 2)
  
  m_sp_pp <- cbind(m_sp[,1:2],0) ## Force to projection plane
  circ_r  <- R3x_of(circ, tilt)
  m_sp_r  <- R3x_of(m_sp_pp, tilt)
  m_sp_z  <- data.frame(x    = m_sp_pp[manip_var, 1],
                        y    = m_sp_pp[manip_var, 2],
                        z    = m_sp_pp[manip_var, 3],
                        xend =  m_sp_r[manip_var, 1],
                        yend =  m_sp_r[manip_var, 3],
                        lab  = lab[manip_var])
  
  
  ### Plot
  gg <- 
    ## Ggplot options
    ggplot2::ggplot() + 
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_fixed() + ## Do not use with plotly!
    ## xy Circle path 
    ggplot2::geom_path(data = circ_r, 
                       mapping = ggplot2::aes(x = x, y = z), 
                       color = manip_col, size = .3, inherit.aes = F) +
    ## xy Basis axes line segments
    ggplot2::geom_segment(data = m_sp_r, 
                          mapping = ggplot2::aes(x = x, y = z, xend = 0, yend = 0),
                          size = siz_v, color = col_v) +
    ## xy Basis variable text labels
    ggplot2::geom_text(data = m_sp_r, 
                       mapping = ggplot2::aes(x = x, y = z, label = lab), 
                       size = 4, color = col_v, 
                       vjust = "outward", hjust = "outward") +
    ## Z circle path
    ggplot2::geom_path(data = circ_r,
                       mapping = ggplot2::aes(x = x, y = y),
                       color = z_col, size = .3, inherit.aes = F) +
    ## Z red manip axis segment
    ggplot2::geom_segment(data = m_sp_z,
                          mapping = ggplot2::aes(x = x, y = y, xend = 0, yend = 0),
                          size = 1, color = z_col) +
    ## Z grey line segment projection
    ggplot2::geom_segment(data = m_sp_z,
                          mapping = ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                          size = .3, color = "grey80", linetype = 3) +
    ## Z red manip axis text label
    ggplot2::geom_text(data = m_sp_z,
                       mapping = ggplot2::aes(x = x, y = y, label = lab),
                       size = 4, color = z_col,
                       vjust = "outward", hjust = "outward") +
    ## XZ Theta curve path
    ggplot2::geom_path(data = theta_curve_r,
                       mapping = ggplot2::aes(x = x, y = z),
                       color = manip_col, size = .3, linetype = 3, inherit.aes = F) +
    ## XZ Theta text
    ggplot2::geom_text(data = theta_curve_r[midpt_theta, ] * 1.3,
                       mapping = ggplot2::aes(x = x, y = z, label = "theta"),
                       size = 4, color = manip_col, parse = T,
                       vjust = "outward", hjust = "outward") +
    ## Z Phi curve path
    ## TODO: needs to go to phi theta_curve_r
    ggplot2::geom_path(data = phi_curve_r,
                       mapping = ggplot2::aes(x = x, y = y),
                       color = z_col, size = .3, linetype = 3, inherit.aes = F) +
    ## Z Phi text
    ggplot2::geom_text(data = phi_curve_r[midpt_phi, ] * 1.2,
                       mapping = ggplot2::aes(x = x, y = y, label = "phi"),
                       size = 4, color = z_col, parse = T,
                       vjust = "outward", hjust = "outward")
  
  gg
}

### FORMATING ------

#' Return hex color code for a given discrete categorical variable
#' 
#' @param class The discrete categorical variable to return the color of.
#' @param pallet_name The name of the `RColorBrewer` pallet to get the colors
#' from. Defaults to "Dark2".
#' @return Vector of character hex color code of the passed categorical variable.
#' @export
#' @examples 
#' col_of(tourr::flea$species)
col_of <- function(class, pallet_name = "Dark2") {
  .l_lvls <- length(levels(class))
  if (.l_lvls == 0) stop("Length of 'class' cannot be zero.")
  if (.l_lvls > 12) stop("'class' has more than the expected max of 12 levels.")
  pal <- suppressWarnings(RColorBrewer::brewer.pal(.l_lvls, pallet_name))
  pal[as.integer(factor(class))]
}
#' Return shape integers for a given discrete categorical variable
#' 
#' @param class The discrete categorical variable to return the shape of.
#' @return Vector of integer shape values of the discrete categorical variable.
#' @export
#' @examples 
#' pch_of(tourr::flea$species)
pch_of <- function(class) {
  .shape_ord <- c(21:25, 3:4, 7:14)
  .l_shapes  <- length(unique(.shape_ord))
  if (is.factor(class) == FALSE) class <- as.factor(class)
  .l_classes <- length(levels(class))
  if (.l_classes == 0) stop("Length of 'class' cannot be zero.")
  if (.l_classes > 12)
    stop(paste0("'class' has more than the expected max of ", .l_shapes, " levels."))
  .int_lvls <- as.integer(class)
  .shape_ord[.int_lvls]
}


### MATH AND TRANSFORMS -----
#' Test if a numeric matrix is orthonormal.
#'
#' Handles more cases than tourr::is_orthonormal().
#'
#' @param x Numeric matrix
#' @param tol Tolerance of (the sum of element-wise) floating point differences.
#' @return Single logical of the orthonormal matrix of the matrix.
#' @export
#' @examples 
#' is_orthonormal(tourr::basis_random(n = 6))
#' is_orthonormal(matrix(1:12, ncol=2))
is_orthonormal <- function(x, tol = 0.001) { ## (tol)erance of SUM of element-wise error.
  stopifnot(is.matrix(x))
  
  actual <- t(x) %*% x ## Collapses to identity matrix IFF x is orthonormal
  expected <- diag(ncol(x))
  
  if (max(actual - expected) < tol) {TRUE} else {FALSE}
}


#' Returns the axis scale and position
#' 
#' Typically called, by other functions to scale axes.
#' 
#' @param x Numeric data object to scale and offset.
#' @param axes Position of the axes: "center", "bottomleft" or "off". Defaults 
#'   to "center".
#' @return Scaled and offset `x`` typically controling axes placment.
#' @seealso \code{\link{pan_zoom}} to set the scale and offset.
#' @export
#' @examples 
#' rb <- tourr::basis_random(4, 2)
#' set_axes_position(x = rb, axes = "bottomleft")
set_axes_position <- function(x, axes) {
  if (length(x) == 1) {x <- data.frame(x = x, y = x)}
  stopifnot(ncol(x) == 2)
  stopifnot(axes %in% 
              c("center", "bottomleft", "topright", "off", "left", "right"))
  if (axes == "off") return()
  if (axes == "center") {
    scale <- 2 / 3
    x_off <- y_off <- 0
  } else if (axes == "bottomleft") {
    scale <- 1 / 4
    x_off <- y_off <- -2 / 3
  } else if (axes == "topright") {
    scale <- 1 / 4
    x_off <- y_off <- 2 / 3
  } else if (axes == "left") {
    scale <- 2 / 3
    x_off <- -5 / 3 
    y_off <- 0
  } else if (axes == "right") {
    scale <- 2 / 3
    x_off <- 5 / 3 
    y_off <- 0
  }
  
  ret <- scale * x
  ret[, 1] <- ret[, 1] + x_off
  ret[, 2] <- ret[, 2] + y_off
  return(ret)
}



#' Pan (offset) and zoom (scale) a 2 column matrix, dataframe or scaler number
#' 
#' @param x Numeric data object with 2 columns (or scaler) to scale and offset
#' @param x_offset Numeric value to pan in the x-direction
#' @param y_offset Numeric value to pan in the x-direction
#' @param scale Numeric value to scale/zoom the size for x.
#' @return Scaled and offset `x`. A manual variant of set_axes_position().
#' @seealso \code{\link{set_axes_position}} for preset choices.
#' @export
#' @examples 
#' ib <- tourr::basis_init(6, 2)
#' pan_zoom(x = ib, x_offset = -1, y_offset = 0, scale = 2/3)
pan_zoom <- function(x, 
                     x_offset = 0, 
                     y_offset = 0, 
                     scale = 1) {
  ## assumptions
  if (length(x) == 1) {x <- data.frame(x = x, y = x)}
  if (ncol(x) != 2) stop("pan_zoom is only defined for 2 variables.")
  
  ret      <- x * scale
  ret[, 1] <- ret[, 1] + x_offset
  ret[, 2] <- ret[, 2] + y_offset
  
  return(ret)
}
