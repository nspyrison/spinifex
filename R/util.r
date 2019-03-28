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

#' Plot projection frame and return the axes table.
#' 
#' Uses base graphics to plot the circle with axes representing
#' the projection frame. Returns the corresponding table.
#' 
#' @param basis A (p, d) basis, XY linear combination of each dimension 
#'   (numeric variable).
#' @param labels Optional character vector of `p` length, add name to the axes 
#'   in the reference frame, typically the variable names.
#' @param data Optional (n, p) data, plots xy scatterplot on the frame
#' @param axes Position of the axes: "center", "bottomleft" or "off". Defaults 
#'   to "center".
#' @return ggplot object of the basis.
#' @import tourr
#' @examples 
#' rb <- basis_random(4, 2)
#' view_basis(basis = rb)
#' 
#' flea_std <- tourr::rescale(tourr::flea[, 1:4])
#' view_basis(basis = rb, data = flea_std)
#' view_basis(basis = rb, data = flea_std, axes = "bottomleft")
#' @export
view_basis <- function(basis,
                       labels = paste0("V", 1:nrow(basis)),
                       data = NULL,
                       axes = "center"
) {
  # Initialize
  basis <- as.data.frame(basis)
  angle <- seq(0, 2 * pi, length = 360)
  circ  <- data.frame(x = cos(angle), y = sin(angle))
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
      mapping = ggplot2::aes(x = V1, y = V2, xend = zero, yend = zero)) +
    ## Basis variable text labels
    ggplot2::geom_text(
      data = basis, 
      mapping = ggplot2::aes(x = V1, y = V2, label = labels),
      size = 4, hjust = 0, vjust = 0, colour = "black")
  }
  
  if (!is.null(data))
  {
    # Project data and all to ggplot
    proj <- as.data.frame(
      tourr::rescale(as.matrix(data) %*% as.matrix(basis))-.5)
    gg <- gg + ggplot2::geom_point(
        data = proj,
        mapping = ggplot2::aes(x = V1, y = V2))
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
#' @param theta Angle in radians to rotate the manip space. 
#'   Defaults to pi * 5/12.
#' @param z_col Color to illustrate the z direction or out of the projection 
#'   plane.
#' @param labels Optional, character vector of `p` length, add name to the axes 
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
                             manip_col = "blue",
                             theta = pi * 5/12,
                             z_col = "red",
                             labels = paste0("V", 1:nrow(basis))
) {
  # Initialize
  xyz <- function(df) {colnames(df) <- c("x", "y", "z"); df}
  ## manip space
  m_sp <- xyz(data.frame(create_manip_space(basis, manip_var)))
  p <- nrow(m_sp)
  ## manip var asethetics
  col_v            <- rep("grey80", p)
  col_v[manip_var] <- manip_col
  siz_v            <- rep(0.3, p)
  siz_v[manip_var] <- 1
  ## circle
  angle <- seq(0, 2 * pi, length = 360)
  circ  <- data.frame(x = cos(angle), y = sin(angle), z = 0)
  ## basis rotation
  c <- cos(theta)
  s <- sin(theta)
  rot  <- matrix(c(1,0,0, 0,c,-1*s, 0,0,s),
                 ncol = 3, byrow = T)
  ## rotation spaces
  circ_r <- xyz(data.frame(as.matrix(circ) %*% rot))
  m_sp_r <- xyz(data.frame(as.matrix(m_sp) %*% rot))
  m_sp_z <- data.frame(x = m_sp[manip_var, "x"],
                       z = m_sp[manip_var, "z"] * sin(theta),
                       xend = m_sp_r[manip_var, "x"],
                       yend = m_sp_r[manip_var, "y"],
                       lab = labels[manip_var])
  
  gg <- 
    ## Ggplot options
    ggplot2::ggplot() + 
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_fixed() + # Do not use with plotly!
    ## Circle path
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
      mapping = ggplot2::aes(x = x, y = y, label = labels), 
      size = 4, colour = col_v, vjust = "outward", hjust = "outward") +
    ## z circle path
    ggplot2::geom_path(
      data = circ_r,
      mapping = ggplot2::aes(x = x, y = z),
      color = z_col, size = .3, inherit.aes = F) +
    ## z manip axis segment
    ggplot2::geom_segment(
      data = m_sp_z, 
      mapping = ggplot2::aes(x = x, y = z, xend = 0, yend = 0),
      size = 1, colour = z_col) + 
    ## z projection line segment
    ggplot2::geom_segment(
      data = m_sp_z, 
      mapping = ggplot2::aes(x = x, y = z, xend = xend, yend = yend),
      size = .3, colour = "grey80", linetype = 2) +
    ## z projection axis text label
    ggplot2::geom_text(
      data = m_sp_z, 
      mapping = ggplot2::aes(x = x, y = z, label = lab),
      size = 4, colour = z_col, vjust = "outward", hjust = "outward")
  
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