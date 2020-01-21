#' Returns the axis scale and position
#' 
#' Typically called, by other functions to scale axes.
#' 
#' @param x numeric data object to scale and offset
#' @param axes Position of the axes: "center", "bottomleft" or "off". Defaults 
#'   to "center".
#' @return axis_scale and axis_scale
#' @examples 
#' rb <- basis_random(4, 2)
#' set_axes_position(x = rb, axes = "bottomleft")
#' @export
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

#' Plot projection frame and return the axes table
#' 
#' Uses base graphics to plot the circle with axes representing
#' the projection frame. Returns the corresponding table.
#' 
#' @param basis A (p, d) basis, XY linear combination of each dimension 
#'   (numeric variable).
#' @param data Optional (n, p) data to plot on through the projection basis.
#' @param lab Optional, labels for the reference frame of length 1 or the 
#'   number of variables used. By default will abbriviate data if available.
#' @param col Color of the projected points. Defaults to "black".
#' @param pch Point character of the projected points. Defaults to 20.
#' @param axes Position of the axes: "center", "bottomleft" or "off". Defaults 
#'   to "center".
#' @param alpha Opacity of the data points between 0 and 1. Defaults to 1.
#' @return ggplot object of the basis.
#' @import tourr
#' @examples 
#' rb <- basis_random(4, 2)
#' view_basis(basis = rb)
#' 
#' flea_std <- tourr::rescale(tourr::flea[, 1:4])
#' view_basis(basis = rb, data = flea_std, axes = "bottomleft", 
#'            col = flea[, 7], pch = flea[,7])
#' @export
view_basis <- function(basis,
                       data = NULL,
                       lab  = NULL,
                       col = "black",
                       pch = 20,
                       axes = "center",
                       alpha = 1
) {
  # Initialize
  basis <- as.data.frame(basis)
  colnames(basis) = c("x", "y")
  angle <- seq(0, 2 * pi, length = 360)
  circ  <- data.frame(x = cos(angle), y = sin(angle))
  p     <- nrow(basis)
  lab   <- if(!is.null(lab)){
    rep(lab, p / length(lab))
  } else {
    if(!is.null(data)) {abbreviate(colnames(data), 3)
    } else {paste0("V", 1:p)}}
  ## scale axes
  zero  <- set_axes_position(0, axes)
  basis <- set_axes_position(basis, axes)
  circ  <- set_axes_position(circ, axes)
  
  gg <- 
    ## Ggplot options
    ggplot2::ggplot() +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_fixed() # Do not use with plotly!
  
  if(axes != "off")
  {
    gg <- gg +
    ## Cirle path
    ggplot2::geom_path(
      data = circ, 
      mapping = ggplot2::aes(x = x, y = y),
      color = "grey80", size = .3, inherit.aes = F) +
    ## Basis axes line segments
    ggplot2::geom_segment(
      data = basis, 
      mapping = ggplot2::aes(x = x, y = y, xend = zero[, 1], yend = zero[, 2])) +
    ## Basis variable text labels
    ggplot2::geom_text(
      data = basis, 
      mapping = ggplot2::aes(x = x, y = y, label = lab),
      size = 4, hjust = 0, vjust = 0, color = "black")
  }
  
  if (!is.null(data))
  {
    ## projection color and point char asethetics
    if(length(col) != 1) {
      if (is.factor(col)) {col <- col_of(col)}
    }
    if(length(pch) != 1) {
      if (is.factor(pch)) {pch <- pch_of(pch)}
    }
    
    # Project data and plot
    proj <- as.data.frame(
      tourr::rescale(as.matrix(data) %*% as.matrix(basis)) - .5)
    colnames(proj) <- c("x", "y")
    gg <- gg + 
      ggplot2::geom_point(data = proj, 
                          shape = pch, color = col, fill = col, alpha = alpha,
                          mapping = ggplot2::aes(x = x, y = y))
  }
  
  gg
}

#' Plot projection frame and return the axes table.
#' 
#' Uses base graphics to plot the circle with axes representing
#' the projection frame. Returns the corresponding table.
#' 
#' @param basis A (p, d) basis, XY linear combination of each dimension 
#'   (numeric variable).
#' @param manip_var Number of the column/dimension to rotate.
#' @param manip_col String of the color to highlight the `manip_var`.
#' @param tilt Angle in radians to rotate the projection plane. 
#'   Defaults to pi * 5/12.
#' @param z_col Color to illustrate the z direction or out of the projection 
#'   plane.
#' @param lab Optional, character vector of `p` length, add name to the axes 
#'   in the reference frame, typically the variable names.
#' @return ggplot object of the basis.
#' 
#' @examples 
#' flea_std <- tourr::rescale(tourr::flea[, 1:6])
#' rb <- basis_random(ncol(flea_std), 2)
#' 
#' view_manip_space(basis = rb, manip_var = 4)
#' @export
view_manip_space <- function(basis,
                             manip_var,
                             tilt = 1/12 * pi,
                             lab = paste0("V", 1:nrow(basis)),
                             manip_col = "blue",
                             z_col = "red") {
  ### Initialize
  ## manip space
  m_sp <- as.matrix(create_manip_space(basis, manip_var))
  p <- nrow(m_sp)
  ## manip var asethetics
  col_v            <- rep("grey80", p)
  col_v[manip_var] <- manip_col
  siz_v            <- rep(0.3, p)
  siz_v[manip_var] <- 1
  ## helper funcs
  make_curve <- function(rad_st = 0, rad_stop = 2 * pi){ # 1 obs per degree drawn
    ang <- seq(rad_st, rad_stop, 
                 length = max(round(360 * abs(rad_st - rad_stop) / (2 * pi)), 3) )
    as.matrix(data.frame(x = cos(ang), y = sin(ang), z = 0))
  }
  find_angle <- function(a,b) acos( sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )
  xyz <- function(mat) {colnames(mat) <- c("x", "y", "z"); as.data.frame(mat)}
  rot3x_of <- function(mat, ang = tilt){ # https://en.wikipedia.org/wiki/Rotation_matrix#In_three_dimensions
    ang <- -ang
    c <- cos(ang)
    s <- sin(ang)
    rot <- matrix(c(1, 0,  0,
                    0, c, -s,
                    0, s,  c), ncol = 3, byrow = T)
    xyz(mat %*% rot)
  }
  rot3z_of <- function(mat, ang){
    mat <- as.matrix(mat)
    ang <- -ang
    c <- cos(ang)
    s <- sin(ang)
    rot <- matrix(c(c, -s, 0,
                    s,  c, 0,
                    0,  0, 1),   ncol = 3, byrow = T)
    xyz(mat %*% rot)
  }
    
  ##### Square space
  theta <- find_angle(m_sp[manip_var, 1:2], c(1, 0)) * sign(m_sp[manip_var, 2])
  phi   <- find_angle(m_sp[manip_var, ], c(m_sp[manip_var, 1:2], 0))
  
  circ          <- make_curve()
  theta_curve_r <- rot3x_of(make_curve(0, theta) / 5)
  phi_curve_r   <- rot3x_of(make_curve(0, phi) / 3) ###TODO: NEEDS TO BE BASED AND ORIENTED FROM END OF V4
  phi_curve_r   <- rot3z_of(phi_curve_r, theta)
  
  midpt_theta   <- round(nrow(theta_curve) / 2)
  midpt_phi     <- round(nrow(phi_curve) / 2)
  
  m_sp_pp <- cbind(m_sp[,1:2],0) # Force to projection plane
  circ_r <- rot3x_of(circ)
  m_sp_r <- rot3x_of(m_sp_pp)
  m_sp_z <- data.frame(x = m_sp_pp[manip_var, 1],
                       y = m_sp_pp[manip_var, 2],
                       z = m_sp_pp[manip_var, 3],
                       xend = m_sp_r[manip_var, 1],
                       yend = m_sp_r[manip_var, 3],
                       lab  = lab[manip_var])
  
  
  ### Plot
  gg <- 
    ## Ggplot options
    ggplot2::ggplot() + 
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_fixed() + # Do not use with plotly!
    ## XY Circle path 
    ggplot2::geom_path(data = circ_r, 
                       mapping = ggplot2::aes(x = x, y = z), 
                       color = manip_col, size = .3, inherit.aes = F) +
    ## XY Basis axes line segments
    ggplot2::geom_segment(data = m_sp_r, 
                          mapping = ggplot2::aes(x = x, y = z, xend = 0, yend = 0),
                          size = siz_v, color = col_v) +
    ## XY Basis variable text labels
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


#' Return `col` values for a given categorical variable
#' 
#' Retruns string `col` values for a passed categorical variable.
#' 
#' @param cat The categorical variable to return the `col` values for.
#' @param pallet_name The name of the `RColorBrewer` pallet to get the colors 
#'   from. Defaults to "Dark2"
#' @return The integer `col` values for a passed categorical variable.
#' 
#' @examples 
#' col_of(tourr::flea$species)
#' @export
col_of <- function(cat, pallet_name = "Dark2")
{
  if (length(cat) == 0) stop("Length cannot be zero.")
  n   <- length(levels(cat))
  pal <- suppressWarnings(RColorBrewer::brewer.pal(n, pallet_name))
  ret <- pal[as.integer(factor(cat))]
  
  ret
}

#' Return `pch` values for a given categorical variable
#' 
#' Retruns integer `pch` values for a passed categorical variable.
#' 
#' @param cat The categorical variable to return the `pch` values for.
#' @return The integer `pch` values for a passed categorical variable.
#' 
#' @examples 
#' pch_of(tourr::flea$species)
#' @export
pch_of <- function(cat)
{
  if (length(cat) == 0) stop("Length cannot be zero.")
  as.integer(factor(cat)) + 20L
}