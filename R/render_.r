#' Render the ggplot before the animation package
#'
#' Typically called by `render_plotly()` or `render_gganimate()`. Takes the 
#' result of `array2df()`, and renders them into a ggplot2 object. 
#'
#' @param slides The result of `array2df()`, a long df of the projected frames.
#' @param manip_col String of the color to highlight the `manip_var`.
#'   Defaults to "blue".
#' @param col Color of the projected points. Defaults to "black".
#' @param pch Point character of the projected points. Defaults to 20.
#' @param axes Position of the axes: "center", "bottomleft" or "off". Defaults 
#'   to "center".
#' @param alpha Opacity of the data points between 0 and 1. Defaults to 1.
#' @return A ggplot2 object ready to be called by `render_plotly()` or 
#'   `render_gganimate()`.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[, 1:6])
#' 
#' rb <- basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
#' sshow <- array2df(array = mtour, data = flea_std)
#' render_(slides = sshow)
#' 
#' render_(slides = sshow, axes = "bottomleft")
render_ <- function(slides,
                    manip_col = "blue",
                    col = "black", 
                    pch = 20,
                    axes = "center",
                    alpha = 1
) {
  # Initialize
  if (length(slides) == 2)
    data_slides  <- data.frame(slides[[2]])
  basis_slides   <- data.frame(slides[[1]])
  manip_var      <- attributes(slides$basis_slides)$manip_var
  n_slides       <- max(basis_slides$slide)
  p              <- nrow(basis_slides) / n_slides
  d              <- ncol(basis_slides) - 2
  ## Circle
  angle          <- seq(0, 2 * pi, length = 360)
  circ           <- data.frame(x = cos(angle), y = sin(angle))
  ## Scale basis axes
  if (axes != "off"){
    zero         <- set_axes_position(0, axes)
    circ         <- set_axes_position(circ, axes)
    basis_slides <- data.frame(set_axes_position(basis_slides[, 1:d], axes), 
                                basis_slides[, (d+1):ncol(basis_slides)])
  }
  ## manip var axes asethetics
  axes_col <- "grey50"
  axes_siz <- 0.3
  if(!is.null(manip_var)) {
    axes_col            <- rep("grey50", p) 
    axes_col[manip_var] <- manip_col
    axes_col            <- rep(axes_col, n_slides)
    axes_siz            <- rep(0.3, p)
    axes_siz[manip_var] <- 1
    axes_siz            <- rep(axes_siz, n_slides)
  }
  ## projection color and point char asethetics
  if(length(col)!=1) {
    if (is.factor(col)) {col <- col_of(col)}
    col <- rep_len(col, nrow(data_slides))
  }
  if(length(pch)!=1) {
    if (is.factor(pch)) {pch <- pch_of(pch)}
    pch <- rep_len(pch, nrow(data_slides))
  }
  
  xy_min <- min(circ[, 1:2], data_slides[, 1:2]) -.1
  xy_max <- max(circ[, 1:2], data_slides[, 1:2]) +.1
  gg <- 
    ## ggplot settings
    ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::xlim(xy_min, xy_max) +
    ggplot2::ylim(xy_min, xy_max) +
    ## Projected data points
    suppressWarnings( # Suppress for unused aes "frame".
      ggplot2::geom_point( 
        data = data_slides, size = 3, 
        shape = pch, color = col, fill = col, alpha = alpha,
        mapping = ggplot2::aes(x = V1, y = V2, frame = slide)
      )
    )
  
  if (axes != "off"){
    gg <- gg +
      ## Circle path 
      ggplot2::geom_path(
        data = circ, color = "grey80", size = .3, inherit.aes = F,
        mapping = ggplot2::aes(x = x, y = y)
      ) +
      ## Basis axes segments
      suppressWarnings( # Suppress for unused aes "frame".
        ggplot2::geom_segment( 
          data = basis_slides, size = axes_siz, colour = axes_col,
          mapping = ggplot2::aes(x = V1,
                                 y = V2, 
                                 xend = zero, yend = zero, 
                                 frame = slide)
        )
      ) +
      ## Basis axes text labels
      suppressWarnings( # Suppress for unused aes "frame".
        ggplot2::geom_text(
          data = basis_slides, 
          mapping = ggplot2::aes(x = V1,
                                 y = V2, 
                                 frame = slide, label = lab_abbr),
          colour = axes_col, size = 4, vjust = "outward", hjust = "outward")
      )
  }
  
  gg
}

