#' Create a slideshow array of the projected bases
#'
#' Takes the result of `manual_tour()` and projects the data over the 
#' interpolated tour path of the reference frame.
#'
#' @param tour A (p, d, n_slides) array of a tour, the output of `manual_tour`.
#' @param data Optional, (n, p) dataset to project, consisting of numeric variables.
#' @return A list containing the (p, d, n_slides) basis slides array, and
#'   the (n, d, n_slides) data slides array.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[,1:6])
#' 
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(rb, manip_var = 4)
#' create_slides(tour = mtour, data = flea_std)
create_slides <- function(tour,
                          data = NULL
) {
  # Initialize
  if (!is.null(data) & !is.matrix(data)) data <- as.matrix(data)
  p <- nrow(tour[,, 1])
  n_slides     <- dim(tour)[3]
  basis_slides <- NULL
  data_slides  <- NULL
  
  # basis; array to long df (tibble)
  for (slide in 1:n_slides) {
    bas_slide <- dplyr::as_tibble(cbind(tour[,, slide], slide))
    basis_slides <- rbind(basis_slides, bas_slide)
  }
  
  # data; if exists,  array to long df (tibble)
  if(!is.null(data)) {
    for (slide in 1:n_slides) {
      dat_slide <- cbind(data %*% tour[,, slide], slide)
      dat_slide[, 1] <- scale(dat_slide[, 1], scale = FALSE)
      dat_slide[, 2] <- scale(dat_slide[, 2], scale = FALSE)
      data_slides <- dplyr::as_tibble(rbind(data_slides, dat_slide))
    }
  }
  
  # Add labels, attribute, and list
  lab_abbr <- 
    if(!is.null(data)) {
      abbreviate(colnames(data), 3)
    } else paste0("V", 1:p)
  basis_slides$lab_abbr <- rep(lab_abbr, n_slides)
  
  attr(basis_slides, "manip_var") <- attributes(tour)$manip_var
  
  slides <- if(!is.null(data)) {
    list(basis_slides = basis_slides, data_slides = data_slides)
  } else list(basis_slides = basis_slides)
  
  return(slides)
}

#' Render the ggplot before the animation package
#'
#' Typically called by `render_plotly()` or `render_gganimate()`. Takes the 
#' result of `create_slides()`, and renders them into a ggplot2 object. 
#'
#' @param slides The result of `create_slides()`.
#' @param manip_col String of the color to highlight the `manip_var`.
#' @param cat_var Categorical variable, optionally used to set the data point 
#'   color and shape.
#' @param ... Optional, pass addition arguments into `plotly::animation_opts()`.
#' @return A ggplot2 object to be called by `render_plotly()` or 
#'   `render_gganimate()`.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[, 1:6])
#' 
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
#' sshow <- create_slides(tour = mtour, data = flea_std)
#' render_(slides = sshow)
#' 
#' render_(slides = sshow, cat_var = flea$species)
render_ <- function(slides,
                    manip_col  = "blue", # string of color name
                    cat_var    = NULL   # cat var to color data and pch
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
  ## manip var asethetics
  if(!is.null(manip_var)) {
    col_v            <- rep("black", p) 
    col_v[manip_var] <- manip_col
    col_v            <- rep(col_v, n_slides)
    siz_v            <- rep(0.3, p)
    siz_v[manip_var] <- 1
    siz_v            <- rep(siz_v, n_slides)
  } else {
    col_v <- "black"
    siz_v <- 0.3
  }
  ## cat_var asethetics
  if(!is.null(cat_var)) {
    if (!is.numeric(cat_var)) cat_var <- as.numeric(as.factor(cat_var))
    cat_v <- rep(cat_var, n_slides)
  } else cat_v <- 1
  
  # Graphics (reference frame)
  ## Circle and options
  gg1 <- 
    ggplot2::ggplot() + ggplot2::geom_path(
      data = circ, color = "grey80", size = .3, inherit.aes = F,
      mapping = ggplot2::aes(x = circ$x, y = circ$y)
    ) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
  ## Axes line segments
  gg2 <- gg1 + suppressWarnings( # Supress for unused aes "frame".
    ggplot2::geom_segment( 
    data = basis_slides, size = siz_v, colour = col_v,
    mapping = ggplot2::aes(x = basis_slides[, 1], y = basis_slides[, 2], 
                           xend = 0, yend = 0, frame = basis_slides$slide))
  )
  # Text labels
  gg3 <- gg2 # + suppressWarnings(ggplot2::geom_text( # for unused aes "frame".
  # data = basis_slides, size = 4, vjust = "outward", hjust = "outward",
  # mapping = ggplot2::aes(x = basis_slides[, 1], y = basis_slides[, 2], 
  #                        frame = basis_slides$slide, label = lab_abbr)
  # ))
  # Projected data scatterplot
  gg4 <- gg3 + suppressWarnings(ggplot2::geom_point( # for unused aes "frame".
    data = data_slides, size = .7, 
    color = cat_v, shape = cat_v + 15,
    mapping = ggplot2::aes(x = data_slides[, 1], y = data_slides[, 2],
                           frame = data_slides$slide))
  )
  
  return(gg4)
}

#' Render the slides as a *plotly* animation
#'
#' Takes the result of `create_slides()` and renders them into a 
#' *plotly* animation.
#'
#' @param slides The result of `create_slides()`.
#' @param manip_col String of the color to highlight the `manip_var`.
#' @param cat_var Categorical variable, optionally used to set the data point 
#'   color and shape.
#' @param fps Frames/slides shown per second. Defaults to 3.
#' @param ... Optional, pass addition arguments into `plotly::animation_opts()`.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[,1:6])
#' 
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
#' sshow <- create_slides(tour = mtour, data = flea_std)
#' render_plotly(slides = sshow)
#' 
#' render_plotly(slides = sshow, cat_var = flea$species)
render_plotly <- function(slides,
                          manip_col = "blue", # string of color name
                          cat_var   = NULL,   # cat var to color data and pch
                          fps       = 3,      # frame/slide per second
                          ...) 
{
  # Initialize
  gg <- render_(slides, manip_col, cat_var)
  
  ggp <- plotly::ggplotly(gg)
  ggp <- plotly::animation_opts(p = ggp, frame = 1 / fps * 1000, 
                                transition = 0, redraw = FALSE, ...)
  ggp <- plotly::layout(
    ggp, showlegend = F, yaxis = list(showgrid = F, showline = F),
    xaxis = list(scaleanchor = "y", scaleratio = 1, showgrid = F, showline = F),
    ...
  )

return(ggp)
}

#' Render the slides as a *gganimate* animation
#'
#' Takes the result of `create_slides()` and renders them into a 
#' *gganimate* animation.
#'
#' @param slides The result of `create_slides()`.
#' @param manip_col String of the color to highlight the `manip_var`.
#' @param cat_var Categorical variable, optionally used to set the data point 
#'   color and shape.
#' @param fps Frames/slides shown per second. Defaults to 3.
#' @param ... Optional, pass addition arguments into `plotly::animation_opts()`.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[,1:6])
#' 
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
#' sshow <- create_slides(tour = mtour, data = flea_std)
#' render_gganimate(slides = sshow)
#' 
#' render_gganimate(slides = sshow, cat_var = flea$species)
render_gganimate <- function(slides,
                             manip_col = "blue", # string of color name
                             cat_var   = NULL,   # cat var to color data and pch
                             fps       = .3,     # frames per second
                             ...) 
{
  # Initialize
  gg <- render_(slides, manip_col, cat_var, ...)
  
  gganim <- gg + gganimate::transition_states(
    slide, transition_length = 0, state_length = 1 / fps
  )
  
  return(gganim)
}