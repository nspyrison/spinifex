#' Create a slideshow array of the projected bases
#'
#' Takes the result of `manual_tour()` and projects the data over the 
#' interpolated tour path of the reference frame.
#'
#' @param data A [n, p] dim data to project, consisting of only numeric 
#' variables (for coercion into matrix).
#' @param tour A [p, d, n_slides] dim array of a tour. Ie) The output of 
#' `manual_tour`.
#' @return A list containing the [p, d, n_slides] dim basis slides array, and
#' the [n, d, n_slides] dim data slides array.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[,1:6])
#' 
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(rb, manip_var = 4)
#' create_slides(tour = mtour, data = flea_std)

create_slides <- function(tour,
                          data = NULL) {
  # Assertions
  p <- nrow(tour[,, 1])
  if (!is.null(data)) stopifnot(ncol(data) == p)
  if (!is.null(data) & !is.matrix(data)) data <- as.matrix(data)
  stopifnot(is.array(tour))
  
  # Initialize
  n_slides     <- dim(tour)[3]
  basis_slides <- NULL
  
  if(!is.null(data)) { # IF data exists (ie. spinifex case) THEN: 
    data_slides <- NULL
    for (slide in 1:n_slides) {
      # make bases slides, and
      curr_slide <- dplyr::as_tibble(tour[,, slide])
      curr_slide$slide <- slide
      basis_slides <- rbind(basis_slides, curr_slide)
      # make data slides
      curr_slide <- dplyr::as_tibble(data %*% tour[,, slide])
      curr_slide$V1 <- curr_slide$V1 - mean(curr_slide$V1)
      curr_slide$V2 <- curr_slide$V2 - mean(curr_slide$V2)
      curr_slide$slide <- slide
      data_slides <- rbind(data_slides, curr_slide)
    }
  } else {# ELSE, (if data is NULL), make bases slides:
    for (slide in 1:n_slides) {
      curr_slide <- dplyr::as_tibble(tour[,, slide])
      curr_slide$slide <- slide
      basis_slides <- rbind(basis_slides, curr_slide)
    }
  }
  
  # Initiate labels
  lab_abbr <- if(!is.null(data)) {abbreviate(colnames(data), 3)
  } else paste0("V", 1:p)
  lab_abbr <- rep(lab_abbr, n_slides)
  basis_slides$lab_abbr <- lab_abbr
  
  # Keep manip_var as attribute if it's not NULL
  if (!is.null(attributes(tour)$manip_var)) {
    manip_var <- attributes(tour)$manip_var
    attr(basis_slides, "manip_var") <- manip_var
  }
  
  slides <- if(!is.null(data)) {
    list(basis_slides = basis_slides, data_slides = data_slides)
  } else list(basis_slides = basis_slides)
  
  return(slides)
}

#' Render the ggplot before the animation package
#'
#' Typically called by `render_plotly()` or `render_gganimate()`. Takes 
#' `slides`, the result of `create_slides()`, and renders them into a ggplot2 
#' object. 
#'
#' @param slides The result of `create_slides()`.
#' @param manip_col String of the color to highlight the `manip_var`.
#' @param cat_var Categorical variable, optionally used to set the data point 
#' color and shape.
#' @param ... Optional, pass addition arguments into `plotly::animation_opts()`.
#' @return A ggplot2 object to be called by `render_plotly()` or 
#' `render_gganimate()`.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[,1:6])
#' 
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
#' sshow <- create_slides(tour = mtour, data = flea_std)
#' render_gg(slides = sshow)
render_ <- function(slides,
                    manip_col  = "blue", # string of color name
                    cat_var    = NULL,   # cat var to color data and pch
                    ...) 
{
  # Initialize
  if (length(slides) == 2)
    data_slides <- slides[[2]]
  basis_slides  <- slides[[1]]
  angle         <- seq(0, 2 * pi, length = 360)
  circ          <- data.frame(x = cos(angle), y = sin(angle))
  
  ### Graphics
  # Plot reference frame circle
  gg1 <- 
    ggplot2::ggplot() + ggplot2::geom_path(
      data = circ, color = "grey80", size = .3, inherit.aes = F,
      mapping = ggplot2::aes(x = circ$x, y = circ$y)
    ) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
  
  # Given manip_var, format reference frame accordingly
  # Initialize
  n_slides  <- length(unique(basis_slides$slide))
  p         <- nrow(basis_slides[,, 1]) / n_slides
  manip_var <- attributes(basis_slides)$manip_var
  
  # Size and color of manip_var on the ref frame
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
  
  # Plot refrence frame axes
  gg2 <- gg1 + suppressWarnings(ggplot2::geom_segment( # for unused aes "frame".
    data = basis_slides, size = siz_v, colour = col_v,
    mapping = ggplot2::aes(x = basis_slides$V1, y = basis_slides$V2, 
                           xend = 0, yend = 0, frame = basis_slides$slide)
  ))
  
  # Reference frame text
  gg3 <- gg2 # + suppressWarnings(ggplot2::geom_text( # for unused aes "frame".
  # data = basis_slides, size = 4, hjust = 0, vjust = 0, colour = "black",
  # mapping = ggplot2::aes(x = basis_slides$V1, y = basis_slides$V2, 
  #                        frame = basis_slides$slide, label = lab_abbr)
  # ))
  
  # Given cat_var, set dat point color and shape
  if(!is.null(cat_var)) {
    if (!is.numeric(cat_var)) cat_var <- as.numeric(as.factor(cat_var))
    cat_var <- rep(cat_var, n_slides)
  } else cat_var <- 1
  
  # Plot data projection scatterplot
  gg4 <- gg3 + suppressWarnings(ggplot2::geom_point( # for unused aes "frame".
    data = data_slides, size = .7, 
    color = cat_var, shape = cat_var + 15,
    mapping = ggplot2::aes(x = data_slides$V1, y = data_slides$V2,
                           frame = data_slides$slide)
  ))
  
  return(gg4)
}

#' Render the slides as a *plotly* animation
#'
#' Takes `slides`, the result of `create_slides()`, and renders them into a 
#' *plotly* animation.
#'
#' @param slides The result of `create_slides()`.
#' @param manip_col String of the color to highlight the `manip_var`.
#' @param cat_var Categorical variable, optionally used to set the data point 
#' color and shape.
#' @param slide_time Time to show each slide for in seconds. essentially 1/fps, 
#' defaults to .3 seconds.
#' @param ... Optional, pass addition arguments into `plotly::animation_opts()`.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[,1:6])
#' 
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
#' sshow <- create_slides(tour = mtour, data = flea_std)
#' render_plotly(slides = sshow)
render_plotly <- function(slides,
                      manip_col  = "blue", # string of color name
                      cat_var    = NULL,   # cat var to color data and pch
                      slide_time = .3,     # seconds to show each slide for.
                      ...) 
{
  # Initialize
  gg <- render_gg(slides, manip_col, cat_var, ...)
  
  ggp <- plotly::ggplotly(gg)
  ggp <- plotly::animation_opts(p = ggp, frame = slide_time * 1000, 
                                transition = 0, redraw = FALSE, ...)
  ggp <- plotly::layout(
    ggp, showlegend = F, yaxis = list(showgrid = F, showline = F),
    xaxis = list(scaleanchor = "y", scaleratio = 1, showgrid = F, showline = F)
  )

return(ggp)
}

  
#' Render the slides as a *gganimate* animation
#'
#' Takes `slides`, the result of `create_slides()`, and renders them into a 
#' *gganimate* animation.
#'
#' @param slides The result of `create_slides()`.
#' @param manip_col String of the color to highlight the `manip_var`.
#' @param cat_var Categorical variable, optionally used to set the data point 
#' color and shape.
#' @param slide_time Time to show each slide for in seconds. essentially 1/fps, 
#' defaults to .3 seconds.
#' @param ... Optional, pass addition arguments into `plotly::animation_opts()`.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[,1:6])
#' 
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
#' sshow <- create_slides(tour = mtour, data = flea_std)
#' render_gganimate(slides = sshow)
render_gganimate <- function(slides,
                             manip_col  = "blue", # string of color name
                             cat_var    = NULL,   # cat var to color data and pch
                             slide_time = .3,     # seconds to show each slide for.
                             ...) 
{
  # Initialize
  gg <- render_gg(slides, manip_col, cat_var, ...)
  
  gganim <- gg + gganimate::transition_states(slide,
                                              transition_length = 0,
                                              state_length = slide_time
  )
  
  return(gganim)
}

