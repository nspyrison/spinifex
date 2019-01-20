#' Plot projection frame and return the axes table.
#' 
#' Uses base graphics to plot the circle with axes representing
#' the projection frame. Returns the corresponding table.
#' 
#' @param basis A (p, d) basis, XY linear combination of each dimension 
#'   (numeric variable).
#' @param labels Optional, character vector of `p` length, add name to the axes 
#'   in the reference frame, typically the variable names.
#' @param ... Optionally pass additional arguments to `segments`, `lines`, or 
#'   `text`.
#' @return The basis, norm, and theta, also plots the reference frame in base 
#'   graphics.
#' 
#' @examples 
#' rb <- tourr::basis_random(4, 2)
#' view_basis(basis = rb)
#' @export
view_basis <- funtion(basis,
                      labels = paste0("V", 1:nrow(basis)) ) {
  # Initialize
  p <- nrow(basis)
  basis <- as.data.frame(basis)
  colnames(basis) <- paste0("C", 1:ncol(basis))
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
      data = basis, mapping = ggplot2::aes(x = C1, y = C2, xend = 0, yend = 0)
    )
  ## Text labels
  gg3 <- gg2 + ggplot2::geom_text(
    data = basis, size = 4, hjust = 0, vjust = 0, colour = "black",
    mapping = ggplot2::aes(x = C1, y = C2, label = labels)
  )
  
  gg3
}