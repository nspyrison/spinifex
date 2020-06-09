#' Turns a tour path array into a long data frame
#'
#' Typically called by a wrapper function, `play_manual_tour` or 
#' `play_tour_path`. Takes the result of `tourr::save_history()` or 
#' `manual_tour()` and restuctures the data from an array to a long data frame 
#' for use in ggplots.
#'
#' @param array A (p, d, n_slides) array of a tour, the output of 
#'   `manual_tour()`.
#' @param data Optional, (n, p) dataset to project, consisting of numeric 
#'   variables.
#' @param lab Optional, labels for the reference frame of length 1 or the 
#'   number of variables used. Defaults to an abbriviation of the variables.
#' @return A list containing the (p, d, n_slides) basis slides array, and
#'   the (n, d, n_slides) data slides array.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[, 1:6])
#' 
#' rb <- basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
#' array2df(array = mtour, data = flea_std)
array2df <- function(array, 
                     data = NULL,
                     lab = NULL) {
  # Initialize
  manip_var <- attributes(array)$manip_var
  p <- nrow(array[,, 1])
  n_slides <- dim(array)[3]
  
  # basis; array to long df
  basis_slides <- NULL
  for (slide in 1:n_slides) {
    basis_rows <- data.frame(cbind(array[,, slide], slide))
    basis_slides <- rbind(basis_slides, basis_rows)
  }
  colnames(basis_slides) <- c("x", "y", "slide")
  
  # data; if exists,  array to long df
  if(!is.null(data)) {
    data <- as.matrix(data)
    data_slides <- NULL
    for (slide in 1:n_slides) {
      data_rows <- cbind(data %*% array[,, slide], slide)
      data_rows[, 1] <- scale(data_rows[, 1], scale = FALSE)
      data_rows[, 2] <- scale(data_rows[, 2], scale = FALSE)
      data_slides <- data.frame(rbind(data_slides, data_rows))
    }
    colnames(data_slides) <- c("x", "y", "slide")
  }
  
  # Add labels, attribute, and list
  lab <- 
    if(!is.null(lab)){
      rep(lab, nrow(basis_slides) / length(lab))
    } else {
      if(!is.null(data)) {abbreviate(colnames(data), 3)
      } else {paste0("V", 1:p)}}
  basis_slides$lab <- rep(lab, n_slides)
  
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
#' @param slides The result of `array2df()`, a long df of the projected frames.
#' @param manip_col String of the color to highlight the `manip_var`.
#'   Defaults to "blue".
#' @param col Point color and fill of the data. Defaults to "black".
#' @param pch Point shape of the data. Defaults to 20.
#' @param cex Point size of the data. Defaults to 1.
#' @param axes Position of the axes: "center", "bottomleft", "off", "left", 
#' "right". Defaults to "center".
#' @param alpha Opacity of the data points between 0 and 1. Defaults to 1.
#' @param ... Recieves arguments from `play_manual_tour()` and `play_tour_path()`
#' @param repel Try to use ggrepel::geom_text_repel() for labels on the variable
#' contributions. Defaults to FALSE
#' @param graphics Graphics package to render with expects "plotly" or "gganimate".
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
#' render_(slides = sshow, axes = "bottomleft", 
#'         col = col_of(flea$species), pch = pch_of(flea$species), cex = 2, alpha = .5)
render_ <- function(slides,
                    manip_col = "blue",
                    col = "black", 
                    pch = 20,
                    cex = 1,
                    axes = "center",
                    alpha = 1,
                    repel = FALSE,
                    graphics = "plotly",
                    ...) {
  ## Initialize
  if (length(slides) == 2)
    data_slides <- data.frame(slides[[2]])
  basis_slides  <- data.frame(slides[[1]])
  manip_var     <- attributes(slides$basis_slides)$manip_var
  n_slides      <- max(basis_slides$slide)
  p             <- nrow(basis_slides) / n_slides
  d             <- 2
  angle         <- seq(0, 2 * pi, length = 360)
  circ          <- data.frame(x = cos(angle), y = sin(angle))
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
  if(length(col) != 1) {
    if (is.factor(col)) {col <- col_of(col)}
    col <- rep_len(col, nrow(data_slides))
  }
  if(length(pch) != 1) {
    if (is.factor(pch)) {pch <- pch_of(pch)}
    pch <- rep_len(pch, nrow(data_slides))
  }
  
  x_min <- min(c(circ[, 1], data_slides[, 1])) - .1
  x_max <- max(c(circ[, 1], data_slides[, 1])) + .1
  y_min <- min(c(circ[, 2], data_slides[, 2])) - .1
  y_max <- max(c(circ[, 2], data_slides[, 2])) + .1
  gg <- 
    ## ggplot settings
    ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::xlim(x_min, x_max) +
    ggplot2::ylim(y_min, y_max) +
    ## Projected data points
    suppressWarnings( ## Suppress for unused aes "frame".
      ggplot2::geom_point( 
        data = data_slides,
        shape = pch, color = col, fill = col, size = cex, alpha = alpha,
        mapping = ggplot2::aes(x = x, y = y, frame = slide)
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
      suppressWarnings( ## Suppress for unused aes "frame".
        ggplot2::geom_segment( 
          data = basis_slides, size = axes_siz, colour = axes_col,
          mapping = ggplot2::aes(x = x,
                                 y = y, 
                                 xend = zero[, 1], yend = zero[, 2], 
                                 frame = slide)
        )
      )
    
    if (graphics %in% c("plotly", "gganimate", "ggplot2")){
      if (graphics == "plotly" | repel == F)
        text_func <- ggplot2::geom_text
      if (graphics %in% c("gganimate", "ggplot2") & repel == T)
        text_func <- ggrepel::geom_text_repel
      gg <- gg +
        ## Basis axes text labels (ggrepel not supported in plotly)
        suppressWarnings( ## Suppress for unused aes "frame".
          text_func(data = basis_slides, 
                    mapping = ggplot2::aes(x = x, y = y, frame = slide, label = lab),
                    vjust = "outward", hjust = "outward",
                    colour = axes_col, size = 4)
        )
    } else ## !(graphics %in% c("plotly", "gganimate", "ggplot2")) ;
      warning("graphics package not specified, axes text labels will be off.")
  } ## end of ploting axes -- if (axes != "off"){}
  
  gg
}


#' Render the slides as a *gganimate* animation
#'
#' Takes the result of `array2df()` and renders them into a 
#' *gganimate* animation.
#'
#' @param fps Frames/slides shown per second. Defaults to 3.
#' @param ... Optional, pass addition arguments to 
#'   `gganimate::transition_states()`.
#' @export
#' @examples
#' \dontrun{
#' flea_std <- tourr::rescale(tourr::flea[, 1:6])
#' 
#' rb <- basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
#' sshow <- array2df(array = mtour, data = flea_std)
#' render_gganimate(slides = sshow)
#' 
#' render_gganimate(slides = sshow, col = flea$species, pch = flea$species,
#'   axes = "bottomleft", fps = 2, alpha = .6)
#' }
render_gganimate <- function(fps = 3,
                             ...) {
  requireNamespace("gganimate")
  ## TODO: SET OPTIONS WITH THE FOLLOWING. During the animation function call.
  # # Change duration and framerate
  # animate(anim, fps = 20, duration = 15)
  # 
  # # Make the animation pause at the end and then rewind
  # animate(anim, nframes = 100, end_pause = 10, rewind = TRUE)
  #  
  # # Use a different renderer
  # animate(anim, renderer = file_renderer('~/animation/'))[1:6]
  
  
  gg <-  render_(graphics = "gganimate", ...) + 
    ggplot2::coord_fixed()
  gga <- gg + 
    gganimate::transition_states(slide, 
                                 transition_length = 0, 
                                 state_length = 1 / fps)
  
  gga
}



#' Render the slides as a *plotly* animation
#'
#' Takes the result of `array2df()` and renders them into a 
#' *plotly* animation.
#'
#' @param fps Frames/slides shown per second. Defaults to 3.
#' @param ... Optional, pass addition arguments to `plotly::animation_opts()` 
#'   and `plotly::layout()`.
#' @export
#' @examples
#' \dontrun{
#' flea_std <- tourr::rescale(tourr::flea[, 1:6])
#' 
#' rb <- basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
#' sshow <- array2df(array = mtour, data = flea_std)
#' render_plotly(slides = sshow)
#' 
#' render_plotly(slides = sshow, col = flea$species, pch = flea$species, 
#'   axes = "bottomleft", fps = 2, alpha = .6)
#' }
render_plotly <- function(fps = 3,
                          ...) {
  requireNamespace("plotly")
  
  gg  <- render_(graphics = "plotly", ...)
  ggp <- plotly::ggplotly(p = gg, tooltip = "none") 
  ggp <- plotly::animation_opts(p = ggp, frame = 1 / fps * 1000, 
                                transition = 0, redraw = FALSE)
  ggp <- plotly::layout(
    ggp, showlegend = F, yaxis = list(showgrid = F, showline = F),
    xaxis = list(scaleanchor = "y", scaleratio = 1, showgrid = F, showline = F))
  
  ggp
}

