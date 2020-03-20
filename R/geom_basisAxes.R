#' Depicts the axes contribution described by the basis
#'
#' Composite ggplot2 geom_ object (ggproto) that illustrated variable 
#' directions and magnitudes of the projection basis.
#' 
#' @param basis A (p, d) dim orthonormal numeric matrix.
#' Defaults to NULL, giving a random basis.
#' @param manip_var Number of the variable to rotate, 
#' defaults to NULL, no variable highlighted.
#' @param manip_col
#' @param theta Angle in radians of "in-projection plane" rotation, 
#' on the XY plane of the reference frame. Defaults to 0, no rotaion.
#' @param phi Angle in radians of the "out-of-projection plane" rotation, into 
#' the z-direction of the axes. Defaults to 0, no rotaion.
#' @param x_offset Numeric value to pan in the x-direction. Defaults to -1.
#' @param y_offset Numeric value to pan in the x-direction. Defaults to 0.
#' @param scale Numeric value to scale/zoom the size for x. Defaults to 2/3.
#' @param ... Optionally pass additional arguments. ie: pch, col, cex
#' 
#' @return a ggplot object of the rotated projection.
#' @import tourr
#' @export
#' @examples
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' 
#' ggplot2::ggplot() + 
#'   geom_basisAxes(basis = rb, manip_var = 2, manip_col = "purple")
geom_basisAxes <- function(basis     = NULL,
                           manip_var = NULL,
                           manip_col = "blue",
                           theta     = 0,
                           phi       = 0,
                           lab       = NULL,
                           x_offset  = -1,
                           y_offset  = 0,
                           scale     = 2/3,
                           ...) {
  if(is.null(basis) | length(basis) == 0) stop("Basis is missing.")
  
  ## Initialize
  p      <- nrow(basis)
  angle  <- seq(0, 2 * pi, length = 360)
  circ   <- data.frame(x = cos(angle), y = sin(angle))
  circ   <- pan_zoom(x = circ, x_offset = x_offset, y_offset = y_offset, scale = scale)
  origin <- pan_zoom(x = 0, x_offset = x_offset, y_offset = y_offset, scale = scale)
  basis  <- pan_zoom(x = basis, x_offset = x_offset, y_offset = y_offset, scale = scale)
  basis  <- as.data.frame(basis)
  colnames(basis) <- c("x", "y")
  
  ## Labels
  if (is.null(lab)){
    lab <- paste0("V", 1:p)
  }
  
  ## Asethetics
  axes_col <- "grey50"
  axes_siz <- 0.3
  if(!is.null(manip_var)) {
    axes_col            <- rep("grey50", p) 
    axes_col[manip_var] <- manip_col
    axes_siz            <- rep(0.3, p)
    axes_siz[manip_var] <- 1
  }
  
  ret <- list(
    ## Unit circle geom_path 
    ggplot2::geom_path(
      data = circ, color = "grey80", size = .3, inherit.aes = F,
      mapping = ggplot2::aes(x = x, y = y)
    ),
    ## Basis axes geom_segment
    ggplot2::geom_segment( 
      data = basis, size = axes_siz, colour = axes_col,
      mapping = ggplot2::aes(x = x, y = y, 
                             xend = origin[, 1], yend = origin[, 2])
    ),
    ## Basis axes geom_text labels
    ggplot2::geom_text(
      data = basis, 
      mapping = ggplot2::aes(x = x, y = y, label = lab),
      colour = axes_col, size = 4, vjust = "outward", hjust = "outward")
  )
  
  ret
}

