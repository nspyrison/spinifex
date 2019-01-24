#' Plot projection frame and return the axes table.
#' 
#' Uses base graphics to plot the circle with axes representing
#' the projection frame. Returns the corresponding table.
#' 
#' @param basis A (p, d) basis, XY linear combination of each dimension 
#'   (numeric variable).
#' @param labels Optional, character vector of `p` length, add name to the axes 
#'   in the reference frame, typically the variable names.
#' @return ggplot object of the basis.
#' 
#' @examples 
#' rb <- tourr::basis_random(4, 2)
#' view_basis(basis = rb)
#' @export
view_basis <- function(basis,
                      labels = paste0("V", 1:nrow(basis))
) {
  # Initialize
  p <- nrow(basis)
  if (!is.data.frame(basis)) basis <- as.data.frame(basis)
  ## circle
  angle <- seq(0, 2 * pi, length = 360)
  circ  <- data.frame(x = cos(angle), y = sin(angle))
  
  # graphics (reference frame)
  ## circle and set options
  gg1 <- ggplot2::ggplot() + ggplot2::geom_path(
    data = circ, color = "grey80", size = .3, inherit.aes = F,
    mapping = ggplot2::aes(x = circ$x, y = circ$y)
  ) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_fixed() # Do not use with plotly!
  ## Axes line segments
  gg2 <- gg1 +
    ggplot2::geom_segment(
      data = basis, 
      mapping = ggplot2::aes(x = basis[,1], y = basis[,2], xend = 0, yend = 0)
    )
  ## Text labels
  gg3 <- gg2 + ggplot2::geom_text(
    data = basis, size = 4, hjust = 0, vjust = 0, colour = "black",
    mapping = ggplot2::aes(x = basis[, 1], y = basis[, 2], label = labels)
  )
  
  gg3
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
#' view_basis3d(basis = rb, manip_var = 4)
#' @export
view_manip_sp <- function(basis,
                         manip_var,
                         manip_col = "blue",
                         theta = pi * 5/12,
                         z_col = "red",
                         labels = paste0("V", 1:nrow(basis)) 
) {
  # Initialize
  if (!is.null(colnames(basis))) labels <- abbreviate(colnames(basis), 3)
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
  rot   <- matrix(c(1,0,1, 0,cos(theta),sinpi(theta)), # ,0,0,sin(theta))
                  ncol = 3, byrow = T)
  ## rotation spaces
  circ_r <- xyz(data.frame(as.matrix(circ[, c(1, 2)]) %*% rot))
  m_sp_r <- xyz(data.frame(as.matrix(m_sp[, c(1, 2)]) %*% rot))
  circ_z <- data.frame(x = circ$x, y = circ$y * sin(theta))
  m_sp_z <- data.frame(x = m_sp[manip_var, "x"],
                       y = m_sp[manip_var, "z"] * sin(theta))
  
  # Graphics (reference frame)
  ## xy circle and options
  gg1 <- ggplot2::ggplot() + 
    ggplot2::geom_path(
      data = circ_r, color = manip_col, size = .3,
      mapping = ggplot2::aes(circ_r$x, circ_r$y), inherit.aes = F) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_fixed() # Do not use with plotly!
  ## Axes line segments
  gg2 <- gg1 +
    ggplot2::geom_segment(
      data = m_sp_r, size = siz_v, colour = col_v,
      mapping = ggplot2::aes(m_sp_r$x, m_sp_r$y, xend = 0, yend = 0)
    )
  ## Text labels
  gg3 <- gg2 + ggplot2::geom_text(
    data = m_sp_r, size = 4, colour = col_v,
    vjust = "outward", hjust = "outward",
    mapping = ggplot2::aes(m_sp_r$x, m_sp_r$y, label = labels)
  )
  ## zx circle
  gg4 <- gg3 + ggplot2::geom_path(
    data = circ_z, color = z_col, size = .3, inherit.aes = F,
    mapping = ggplot2::aes(x = x, y = y)
  )
  ## z manip sp segments, projection line, label
  gg5 <- gg4 + ggplot2::geom_segment(
    data = m_sp_z, size = 1, colour = z_col,
    mapping = ggplot2::aes(m_sp_z$x, m_sp_z$y, xend = 0, yend = 0)
  ) + ggplot2::geom_segment(
    data = m_sp_z, size = 1, colour = "grey80", linetype = 2,
    mapping = ggplot2::aes(m_sp_z$x, m_sp_z$y, xend = x, yend = m_sp_r[manip_var, "y"] )
  ) + ggplot2::geom_text(
    data = m_sp_z, size = 4, colour = z_col,
    vjust = "outward", hjust = "outward",
    mapping = ggplot2::aes(m_sp_z$x, m_sp_z$y, label = labels[manip_var])
  )
  
  gg5
}
