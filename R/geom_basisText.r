#' Creates a grob of the specified basis
#'
#' Creates a grob of the specified basis. Can be added with 
#' gridExtra::grid.arrange(), but not layered on like a geom_ (ggproto) object.
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
#' @param x_offset Numeric value to pan in the x-direction. Defaults to -2.
#' @param y_offset Numeric value to pan in the x-direction. Defaults to 0.
#' @param scale Numeric value to scale/zoom the size for x. Defaults to 2/3.
#' @param ... Optionally pass additional arguments. ie: pch, col, cex
#' 
#' @return A grob of the specified basis. 
#' @import tourr
#' @export
#' @examples
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' 
#' geom_basisText(basis = rb, manip_var = 2, manip_col = "purple")
grob_basisText <- function(basis     = NULL,
                           manip_var = NULL,
                           manip_col = "blue",
                           theta     = 0,
                           phi       = 0,
                           lab       = NULL,
                           x_offset  = -2,
                           y_offset  = 0,
                           scale     = 2/3,
                           ...) {
  if(is.null(basis) | length(basis) == 0) stop("Basis is missing.")
  require("gridExtra")
  
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
  row.names(basis) <- lab
  
  if(is.null(manip_var)) {
    table_theme <- ttheme_minimal()
  } else { ## Format manip var highlighting
    n_before <- manip_var - 1
    n_after  <- p - manip_var
    table_theme <- ttheme_minimal(core = list(
      fg_params = list(fontface = c(rep("plain", n_before), 
                                    "bold.italic",
                                    rep("plain", n_after))
      ),
      bg_params = list(fill = c(rep(c("grey95", "grey90"), 
                                    length.out = n_before), 
                                manip_col,
                                rep(c("grey95", "grey90"), 
                                    length.out = n_after)
      ))
    ))
  }
    
  tableGrob(round(basis, 2), theme = table_theme)
}

