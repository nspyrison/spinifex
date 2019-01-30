#' Turns a tour path array into a long data frame (tibble)
#'
#' Typically called by a wrapper function, `play_manual_tour` or `play_tourr` 
#' Takes the result of `tourr::save_history()` or `manual_tour()` and 
#' restuctures the data from an array to a long data frame (tibble) for use in
#' ggplots.
#'
#' @param array A (p, d, n_slides) array of a tour, the output of `manual_tour()`.
#' @param data Optional, (n, p) dataset to project, consisting of numeric variables.
#' @param angle target distance (in radians) between bases
#' @return A list containing the (p, d, n_slides) basis slides array, and
#'   the (n, d, n_slides) data slides array.
#' @export
#' @examples
#' flea_std <- tourr::rescale(flea[, 1:6])
#' 
#' rb <- basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
#' array2df(array = mtour, data = flea_std)
array2df <- function(array, data = NULL) {
  # Initialize
  manip_var <- attributes(array)$manip_var
  p <- nrow(array[,, 1])
  n_slides     <- dim(array)[3]
  
  # basis; array to long df (tibble)
  basis_slides <- NULL
  for (slide in 1:n_slides) {
    bas_slide <- dplyr::as_tibble(cbind(array[,, slide], slide))
    basis_slides <- rbind(basis_slides, bas_slide)
  }
  
  # data; if exists,  array to long df (tibble)
  if(!is.null(data)) {
    data <- as.matrix(data)
    data_slides <- NULL
    for (slide in 1:n_slides) {
      dat_slide <- cbind(data %*% array[,, slide], slide)
      dat_slide[, 1] <- scale(dat_slide[, 1], scale = FALSE)
      dat_slide[, 2] <- scale(dat_slide[, 2], scale = FALSE)
      data_slides <- dplyr::as_tibble(rbind(data_slides, dat_slide))
    }
  }
  
  # Add labels, attribute, and list
  lab_abbr <- if(!is.null(data)) abbreviate(colnames(data), 3) 
  else paste0("V", 1:p)
  basis_slides$lab_abbr <- rep(lab_abbr, n_slides)
  
  attr(basis_slides, "manip_var") <- manip_var
  
  slides <- if(!is.null(data)) {
    list(basis_slides = basis_slides, data_slides = data_slides)
  } else list(basis_slides = basis_slides)
  
  slides
}

#' Render the ggplot before the animation package
#'
#' Typically called by `render_plotly()` or `render_gganimate()`. Takes the 
#' result of `array2df()`, and renders them into a ggplot2 object. 
#'
#' @param slides The result of `array2df()`.
#' @param manip_col String of the color to highlight the `manip_var`.
#' @param cat_var Categorical variable, optionally used to set the data point 
#'   color and shape.
#' @param axes position of the axes: center, bottomleft or off.
#' @return A ggplot2 object to be called by `render_plotly()` or 
#'   `render_gganimate()`.
#' @export
#' @examples
#' flea_std <- tourr::rescale(flea[, 1:6])
#' 
#' rb <- basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
#' sshow <- array2df(array = mtour, data = flea_std)
#' render_(slides = sshow)
#' 
#' render_(slides = sshow, cat_var = flea$species, axes = "bottomleft")
render_ <- function(slides,
                    manip_col = "blue",
                    cat_var   = NULL,
                    axes      = "center"
) {
  # Initialize
  if (length(slides) == 2)
    data_slides <- data.frame(slides[[2]])
  basis_slides  <- data.frame(slides[[1]])
  manip_var     <- attributes(slides$basis_slides)$manip_var
  n_slides      <- max(basis_slides$slide)
  p             <- nrow(basis_slides) / n_slides
  ## Circle
  angle         <- seq(0, 2 * pi, length = 360)
  circ          <- data.frame(x = cos(angle), y = sin(angle))
  ## Scale basis axes
  zero          <- set_axes_position(0, axes)
  circ          <- set_axes_position(circ, axes)
  basis_slides  <- data.frame(set_axes_position(basis_slides[, 1:2], axes), 
                              basis_slides[, 3:ncol(basis_slides)])
  ## manip var asethetics
  col_v <- "black"
  siz_v <- 0.3
  if(!is.null(manip_var)) {
    col_v            <- rep("grey50", p) 
    col_v[manip_var] <- manip_col
    col_v            <- rep(col_v, n_slides)
    siz_v            <- rep(0.3, p)
    siz_v[manip_var] <- 1
    siz_v            <- rep(siz_v, n_slides)
  }
  ## cat_var asethetics
  cat_v <- 1
  if(!is.null(cat_var)) {
    if (!is.numeric(cat_var)) cat_var <- as.numeric(as.factor(cat_var))
    cat_v <- rep(cat_var, n_slides)
  }
  
  gg <- 
    ## Ggplot settings
    ggplot2::ggplot() +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ## Projected data points
    suppressWarnings( # suppress for unused aes "frame".
      ggplot2::geom_point( 
        data = data_slides, size = .7, 
        color = cat_v, shape = cat_v + 15,
        mapping = ggplot2::aes(x = V1, y = V2, frame = slide))
    )
  
  if (axes != "off")
  {
    gg <- gg +
      ## Circle path 
      ggplot2::geom_path(
        data = circ, color = "grey80", size = .3, inherit.aes = F,
        mapping = ggplot2::aes(x = x, y = y)) +
      ## Basis axes segments
      suppressWarnings( # Supress for unused aes "frame".
        ggplot2::geom_segment( 
          data = basis_slides, size = siz_v, colour = col_v,
          mapping = ggplot2::aes(x = V1, y = V2, 
                                 xend = zero, yend = zero, 
                                 frame = slide))) +
      ## Basis axes text labels
      suppressWarnings( # suppress for unused aes "frame".
        ggplot2::geom_text(
          data = basis_slides, colour = col_v, size = 4, vjust = "outward", hjust = "outward",
          mapping = ggplot2::aes(x = V1, y = V2, 
                                 frame = slide, label = lab_abbr)))
  }
  
  gg
}

#' Render the slides as a *plotly* animation
#'
#' Takes the result of `array2df()` and renders them into a 
#' *plotly* animation.
#'
#' @param slides The result of `array2df()`.
#' @param manip_col String of the color to highlight the `manip_var`.
#' @param cat_var Categorical variable, optionally used to set the data point 
#'   color and shape.
#' @param axes position of the axes: center, bottomleft or off.
#' @param fps Frames/slides shown per second. Defaults to 3.
#' @param ... Optional, pass addition arguments into `plotly::animation_opts()`.
#' @export
#' @examples
#' flea_std <- tourr::rescale(flea[, 1:6])
#' 
#' rb <- basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
#' sshow <- array2df(array = mtour, data = flea_std)
#' render_plotly(slides = sshow)
#' 
#' render_plotly(slides = sshow, cat_var = flea$species, axes = "bottomleft")
render_plotly <- function(slides,
                          manip_col = "blue",
                          cat_var   = NULL,
                          axes      = "center",
                          fps       = 3,
                          ...) 
{
  # Initialize
  gg <- render_(slides = slides, manip_col = manip_col, 
                cat_var = cat_var, axes = axes)
  
  ggp <- plotly::ggplotly(gg)
  ggp <- plotly::animation_opts(p = ggp, frame = 1 / fps * 1000, 
                                transition = 0, redraw = FALSE, ...)
  ggp <- plotly::layout(
    ggp, showlegend = F, yaxis = list(showgrid = F, showline = F),
    xaxis = list(scaleanchor = "y", scaleratio = 1, showgrid = F, showline = F),
    ...
  )
  
  ggp
}

#' Render the slides as a *gganimate* animation
#'
#' Takes the result of `array2df()` and renders them into a 
#' *gganimate* animation.
#'
#' @param slides The result of `array2df()`.
#' @param manip_col String of the color to highlight the `manip_var`.
#' @param cat_var Categorical variable, optionally used to set the data point 
#'   color and shape.
#' @param axes position of the axes: center, bottomleft or off.
#' @param fps Frames/slides shown per second. Defaults to 3.
#' @param ... Optional, pass addition arguments into `plotly::animation_opts()`.
#' @export
#' @examples
#' flea_std <- tourr::rescale(flea[, 1:6])
#' 
#' rb <- basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
#' sshow <- array2df(array = mtour, data = flea_std)
#' render_gganimate(slides = sshow)
#' 
#' render_gganimate(slides = sshow, cat_var = flea$species)
render_gganimate <- function(slides,
                             manip_col = "blue",
                             cat_var   = NULL,
                             axes      = "center",
                             fps       = 3,
                             ...) 
{
  # Initialize
  gg <- render_(slides = slides, manip_col = manip_col, 
                cat_var = cat_var, axes = axes)
  
  gga <- 
    gg + gganimate::transition_states(
      slide, transition_length = 0, state_length = 1 / fps
  )
  
  gga
}