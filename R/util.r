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
  position <- match.arg(axes, c("center", "bottomleft", "off"))
  if (position == "off") return()
  if (position == "center") {
    axis_scale <- 2 / 3
    axis_pos   <- 0
  } else if (position == "bottomleft") {
    axis_scale <- 1 / 4
    axis_pos   <- -2 / 3
  }
  
  axis_scale * x + axis_pos 
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
      mapping = ggplot2::aes(x = x, y = y, xend = zero, yend = zero)) +
    ## Basis variable text labels
    ggplot2::geom_text(
      data = basis, 
      mapping = ggplot2::aes(x = x, y = y, label = lab),
      size = 4, hjust = 0, vjust = 0, colour = "black")
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
                             manip_col = "blue", # "#1B9E77"
                             tilt = 5/12 * pi,   
                             z_col = "red",      # "#D95F02"
                             lab = paste0("V", 1:nrow(basis))
) {
  # Initialize
  ## manip space
  m_sp <- as.matrix(create_manip_space(basis, manip_var))
  p <- nrow(m_sp)
  find_angle <- function(a,b) acos( sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )
  theta <- find_angle(m_sp[manip_var, ], c(1,0,0)) * sign(m_sp[manip_var, 2])
  phi   <- find_angle(m_sp[manip_var, ], c(m_sp[manip_var, 1:2],0))
  ## manip var asethetics
  col_v            <- rep("grey80", p)
  col_v[manip_var] <- manip_col
  siz_v            <- rep(0.3, p)
  siz_v[manip_var] <- 1
  ## basis rotation
  c <- cos(tilt)
  s <- sin(tilt)
  rot  <- matrix(c(1,0,0, 0,c,-1*s, 0,0,s),
                 ncol = 3, byrow = T)
  ## helper funcs
  make_circ <- function(ang_st = 0, ang_stop = 2 * pi){
    angle <- seq(ang_st, ang_stop, 
                 length = max(round(360 * abs(ang_st - ang_stop) / (2 * pi)),3) )
    as.matrix(data.frame(x = cos(angle), y = sin(angle), z = 0))
  }
  xyz <- function(df) {colnames(df) <- c("x", "y", "z"); as.data.frame(df)}
  ## rotated spaces
  circ_r <- xyz(make_circ() %*% rot)
  m_sp_r <- xyz(m_sp %*% rot) # rotated manip sp
  m_sp_z <- data.frame(x = m_sp[manip_var, 1],
                       y = m_sp[manip_var, 2],
                       z = m_sp[manip_var, 3],
                       xend = m_sp_r[manip_var, "x"],
                       yend = m_sp_r[manip_var, "y"],
                       lab = lab[manip_var])
  ## rotate angle curves, phi & theta
  theta_curve <- xyz(make_circ(0, theta) %*% rot)
  phi_curve   <- xyz(make_circ(0, phi))
  phi_curve$y <- phi_curve$y # * sign(m_sp[manip_var, 2])
  phi_curve$x <- phi_curve$x # * sign(m_sp[manip_var, 1])
  .x <- phi_curve$x
  .y <- phi_curve$y
  ang   <- theta + phi # * sign(m_sp[manip_var, 1])
  phi_curve$x <- .x * cos(ang) - .y * sin(ang)
  phi_curve$y <- .x * sin(ang) + .y * cos(ang)
  mid_theta   <- round(nrow(theta_curve)/2)
  mid_phi     <- round(nrow(phi_curve)/2)
  
  gg <- 
    ## Ggplot options
    ggplot2::ggplot() + 
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_fixed() + # Do not use with plotly!
    ## XY Circle path 
    ggplot2::geom_path(
      data = circ_r, 
      mapping = ggplot2::aes(x = x, y = y), 
      color = manip_col, size = .3, inherit.aes = F) +
    ## Basis axes line segments
    ggplot2::geom_segment(
      data = m_sp_r, 
      mapping = ggplot2::aes(x = x, y = y, xend = 0, yend = 0),
      size = siz_v, colour = col_v) +
    ## Basis variable text labels
    ggplot2::geom_text(
      data = m_sp_r, 
      mapping = ggplot2::aes(x = x, y = y, label = lab), 
      size = 4, colour = col_v, vjust = "outward", hjust = "outward") +
    ## Z circle path
    ggplot2::geom_path(
      data = as.data.frame(circ_r),
      mapping = ggplot2::aes(x = x, y = z),
      color = z_col, size = .3, inherit.aes = F) +
    ## Z manip axis segment
    ggplot2::geom_segment(
      data = m_sp_z, 
      mapping = ggplot2::aes(x = x, y = z, xend = 0, yend = 0),
      size = 1, colour = z_col) + 
    ## Z projection line segment
    ggplot2::geom_segment(
      data = m_sp_z, 
      mapping = ggplot2::aes(x = x, y = z, xend = xend, yend = yend),
      size = .3, colour = "grey80", linetype = 2) +
    ## Z projection axis text label
    ggplot2::geom_text(
      data = m_sp_z, 
      mapping = ggplot2::aes(x = x, y = z, label = lab),
      size = 4, colour = z_col, vjust = "outward", hjust = "outward") +
    ## Theta curve path
    ggplot2::geom_path(
      data = theta_curve/5, 
      mapping = ggplot2::aes(x = x, y = y), 
      color = manip_col, size = .1, linetype = 1, inherit.aes = F) +
    ## Theta text
    ggplot2::geom_text(
      data = theta_curve[mid_theta, ]/5 *1.33, 
      mapping = ggplot2::aes(x = x/5, y = y/5, label = "theta"), 
      size = 4, colour = manip_col, parse = T, 
      vjust = "outward", hjust = "outward") +
    ## Phi curve path
    ggplot2::geom_path(
      data = phi_curve/3, 
      mapping = ggplot2::aes(x = x, y = y), 
      color = z_col, size = .1, linetype = 2, inherit.aes = F) +
    ## Phi text 
    ggplot2::geom_text(
      data = phi_curve[mid_phi, ]/3 *1.2, 
      mapping = ggplot2::aes(x = x, y = y, label = "phi"), 
      size = 4, colour = z_col, parse = T)
    
    
    
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
  n   <- length(levels(cat))
  pal <- RColorBrewer::brewer.pal(n, pallet_name)
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
  as.integer(factor(cat))
}