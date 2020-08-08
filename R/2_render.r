#' Turns a tour path array into a long data frame.
#'
#' Typically called by a wrapper function, `play_manual_tour` or 
#' `play_tour_path`. Takes the result of `tourr::save_history()` or 
#' `manual_tour()` and restructures the data from an array to a long data frame 
#' for use in ggplots.
#'
#' @param array A (p, d, n_slides) array of a tour, the output of 
#' `manual_tour()`.
#' @param data Optional, (n, p) dataset to project, consisting of numeric 
#' variables.
#' @param lab Optional, labels for the reference frame of length 1 or the 
#' number of variables used. Defaults to an abbreviation of the variables.
#' @return A list containing an array of basis slides (p, d, n_slides) and
#' an array of data slides (n, d, n_slides) if data is present.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[, 1:6])
#' 
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
#' array2df(array = mtour, data = flea_std)
array2df <- function(array, 
                     data = NULL,
                     lab = NULL) {
  ## Initialize
  manip_var <- attributes(array)$manip_var
  p <- nrow(array[,, 1L])
  n_slides <- dim(array)[3L]
  
  ## basis; array to long df
  basis_slides <- NULL
  for (slide in 1:n_slides) {
    basis_rows <- data.frame(cbind(array[,, slide], slide))
    basis_slides <- rbind(basis_slides, basis_rows)
  }
  colnames(basis_slides) <- c("x", "y", "slide")
  
  ## Data; if exists, array to long df
  if(!is.null(data)) {
    data <- as.matrix(data)
    data_slides <- NULL
    for (slide in 1L:n_slides) {
      data_rows <- cbind(data %*% array[,, slide], slide)
      data_rows[, 1L] <- scale(data_rows[, 1L], scale = FALSE)
      data_rows[, 2L] <- scale(data_rows[, 2L], scale = FALSE)
      data_slides <- data.frame(rbind(data_slides, data_rows))
    }
    colnames(data_slides) <- c("x", "y", "slide")
  }
  
  ## Add labels, attribute, and list
  basis_slides$lab <- NULL
  if(!is.null(lab)){
    basis_slides$lab <- rep(lab, nrow(basis_slides) / length(lab))
  } else {
    if(!is.null(data)) {basis_slides$lab <- abbreviate(colnames(data), 3L)
    } else {
      basis_slides$lab <- paste0("V", 1L:p)
    }
  }
  
  attr(basis_slides, "manip_var") <- manip_var
  
  slides <- if(!is.null(data)) {
    list(basis_slides = basis_slides, data_slides = data_slides)
  } else list(basis_slides = basis_slides)
  
  slides
}



#' Prepate the ggplot object before passing to either animation package.
#'
#' Typically called by `render_plotly()` or `render_gganimate()`. Takes the 
#' result of `array2df()`, and renders them into a ggplot2 object. 
#'
#' @param slides The result of `array2df()`, a long df of the projected frames.
#' @param manip_col String of the color to highlight the `manip_var` with.
#' Defaults to "blue".
#' @param axes Position of the axes: "center", "bottomleft", "off", "left", 
#' "right". Defaults to "center".
#' @param ... Optionally passes arguments to the projection points inside the 
#' aesthetics; `geom_point(aes(...))`.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[, 1:6])
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
#' sshow <- array2df(array = mtour, data = flea_std)
#' 
#' render_(slides = sshow)
#' 
#' render_(slides = sshow, axes = "bottomleft", manip_col = "purple",
#'         col = tourr::flea$species, pch = tourr::flea$species, cex = 2, alpha = .5)
render_ <- function(slides,
                    axes = "center",
                    manip_col = "blue",
                    ...) {
  if(axes == "off" & length(slides) == 1) stop("render_ called with no data and axes = 'off'")
  
  ## Initialize
  basis_slides <- data.frame(slides[["basis_slides"]])
  manip_var    <- attributes(slides$basis_slides)$manip_var
  n_slides     <- length(unique(basis_slides$slide))
  p            <- nrow(basis_slides) / n_slides
  d            <- 2L ## Hardcoded assumtion for 2D display
  ## If data exsists
  data_slides  <- NULL
  if (length(slides) == 2L) {
    data_slides <- data.frame(slides[["data_slides"]])
    ##### Bare with me here,:
    args    <- list(...) ## Empty list behaves well too.
    tgt_len <- nrow(data_slides)
    ## Replicate aesthetic args to correct length
    tform_args  <- lapply(X = args, FUN = function(x) {rep_len(x, tgt_len)})
    my_geom_pts <- function(...) {
      suppressWarnings( ## Suppress for unused aes "frame", AND potential others from the ellipsis '...'
        ggplot2::geom_point(data = data_slides,
                            mapping = ggplot2::aes(x = x, 
                                                   y = y, 
                                                   frame = slide,
                                                   ...)
        )
      )
    }
    proj_pts <- do.call(my_geom_pts, args = tform_args)
  }
  ## Axes unit circle
  angle         <- seq(0L, 2L * pi, length = 360L)
  circ          <- data.frame(x = cos(angle), y = sin(angle))
  ## Scale basis axes/circle
  if (axes != "off"){
    zero         <- set_axes_position(0L, axes)
    circ         <- set_axes_position(circ, axes)
    basis_slides <- data.frame(set_axes_position(basis_slides[, 1L:d], axes), 
                               basis_slides[, (d + 1L):ncol(basis_slides)])
  }
  ## manip var axes asethetics
  axes_col <- "grey50"
  axes_siz <- 0.3
  if(!is.null(manip_var)) {
    axes_col            <- rep("grey50", p) 
    axes_col[manip_var] <- manip_col
    axes_col            <- rep(axes_col, n_slides)
    axes_siz            <- rep(0.3, p)
    axes_siz[manip_var] <- 1L
    axes_siz            <- rep(axes_siz, n_slides)
  }
  
  x_min <- min(c(circ[, 1L], data_slides[, 1L])) - .1
  x_max <- max(c(circ[, 1L], data_slides[, 1L])) + .1
  y_min <- min(c(circ[, 2L], data_slides[, 2L])) - .1
  y_max <- max(c(circ[, 2L], data_slides[, 2L])) + .1
  
  ## Ploting
  gg <- 
    ggplot2::ggplot() +
    ggplot2::xlim(x_min, x_max) +
    ggplot2::ylim(y_min, y_max)
    
  ## Project data points, if data exsists 
  if (!is.null(data_slides)) {
    gg <- gg + proj_pts
  }
  
  ## Add axes directions if needed:
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
          mapping = ggplot2::aes(x = x, y = y, frame = slide,
                                 xend = zero[, 1L], yend = zero[, 2L])
        )
      ) +
      ## Basis axes text labels
      suppressWarnings( ## Suppress for unused aes "frame".
        ggplot2::geom_text(data = basis_slides, 
                           mapping = ggplot2::aes(x = x, y = y, 
                                                  frame = slide, label = lab),
                           vjust = "outward", hjust = "outward",
                           colour = axes_col, size = 4L)
      )
  }
  
  gg
}


#' Render the slides as a *gganimate* animation.
#'
#' Takes the result of `array2df()` and renders them into a 
#' *gganimate* animation.
#'
#' @param fps Frames/slides shown per second. Defaults to 3.
#' @param rewind Logical, should the animation play backwards after reaching 
#' the end? Default to FALSE.
#' @param start_pause Number of seconds to pause on the first frame for.
#' Defaults to 1.
#' @param end_pause Number of seconds to pause on the last frame for.
#' Defaults to 3.
#' @param gif_filename Optional, saves the animation as a GIF to this string 
#' (without folderpath) . Defaults to NULL (no GIF saved). For more control call 
#' `gganimate::anim_save()` on a return object of `render_gganimate()`.
#' @param gif_path Optional, A string of the directory path (without filename) 
#' to save a GIF to. Defaults to NULL (current work directory).
#' @param ... Optionally passes arguments to the projection points inside the 
#' aesthetics; `geom_point(aes(...))`.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[, 1:6])
#' flea_class <- tourr::flea$species
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
#' sshow <- array2df(array = mtour, data = flea_std)
#' \dontrun{
#' render_gganimate(slides = sshow)
#' 
#' render_gganimate(slides = sshow, axes = "bottomleft", fps = 2, rewind = TRUE,
#'   col = flea_class, pch = flea_class, size = 2, alpha = .6)
#'   
#' if(F){ ## Don't run, saves a .gif of the animation.
#'   render_gganimate(slides = sshow, axes = "right", fps = 4, rewind = TRUE,
#'     col = flea_class, pch = flea_class, size = 2,
#'     gif_filename = "myRadialTour.gif", gif_path = "./output")
#' }
#' }
render_gganimate <- function(fps = 3L,
                             rewind = FALSE,
                             start_pause = 1L,
                             end_pause = 3L,
                             gif_filename = NULL,
                             gif_path = NULL,
                             ...) {
  requireNamespace("gganimate")
  
  gg  <- render_(...) + ggplot2::coord_fixed()
  gga <- gg + gganimate::transition_states(slide, 
                                           transition_length = 0L)
  anim <- gganimate::animate(gga, 
                             fps = fps,
                             rewind = rewind,
                             start_pause = fps * start_pause,
                             end_pause = fps * end_pause)
  
  if(is.null(gif_filename) == FALSE)
    gganimate::anim_save(gif_filename, anim, gif_path)
  if(is.null(gif_path)) 
    warning("gif_path supplied with no gif_filename. Add a gif_filename to save a .gif.")
  
  anim
}



#' Render the slides as a *plotly* animation.
#'
#' Takes the result of `array2df()` and renders them into a 
#' *plotly* animation.
#'
#' @param fps Frames/slides shown per second. Defaults to 3.
#' @param tooltip Character vector of aesthetic mappings to show in the `plotly`
#' hover-over tooltip. Defaults to "none". "all" shows all the 
#' aesthetic mappings. The order of variables controls the order they appear. 
#' For example, tooltip = c("id", "frame", "x", "y", "category", "color").
#' @param html_filename Optional, saves the plotly object as an HTML widget to this string 
#' (without folderpath). Defaults to NULL (not saved). For more control call 
#' `htmlwidgets::saveWidget()` on a return object of `render_plotly()`.
#' @param ... Optionally passes arguments to the projection points inside the 
#' aesthetics; `geom_point(aes(...))`.
#' @export
#' @examples
#' flea_std   <- tourr::rescale(tourr::flea[, 1:6])
#' flea_class <- tourr::flea$species
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
#' sshow <- array2df(array = mtour, data = flea_std)
#' \dontrun{
#' render_plotly(slides = sshow)
#' 
#' render_plotly(slides = sshow, axes = "bottomleft", fps = 2, tooltip = "all",
#'               col = flea_class, pch = flea_class, size = 2, alpha = .6)
#' 
#' if(F){ ## Don't run, saves a html widget of the animation.
#'   render_plotly(slides = sshow, axes = "right", fps = 4.5,
#'                 col = flea_class, pch = flea_class, size = 1.5,
#'                 html_filename = "myRadialTour.html")
#' }
#' }
render_plotly <- function(fps = 3L,
                          tooltip = "none",
                          html_filename = NULL,
                          ...) {
  requireNamespace("plotly")
  
  gg  <- render_(...)
  ggp <- plotly::ggplotly(p = gg, tooltip = tooltip)
  ggp <- plotly::animation_opts(p = ggp, 
                                frame = 1L / fps * 1000L, 
                                transition = 0L, 
                                redraw = FALSE)
  ggp <- plotly::layout(ggp, showlegend = FALSE, 
                        yaxis = list(showgrid = FALSE, showline = FALSE),
                        xaxis = list(scaleanchor = "y", scaleratio = 1L,
                                     showgrid = FALSE, showline = FALSE)
  )
  
  if (is.null(html_filename) == FALSE)
    htmlwidgets::saveWidget(ggp, html_filename)
  
  ggp
}



#' Aesthetic settings that can be applied to a ggplot object.
#'
#' A `ggplot2` theme (group of aesthetic settings), that can be added to a ggplot. 
#' @export
#' @examples
#' require(ggplot2)
#' df <- data.frame(x = rnorm(10), y = rnorm(10))
#' ggplot(df, aes(x, y)) + geom_point() + theme_spinifex()
#' 
#' rb    <- tourr::basis_random(n = 6)
#' theta <- runif(1, 0, 2*pi)
#' phi   <- runif(1, 0, 2*pi)
#' 
#' oblique_basis(basis = rb, manip_var = 4, theta, phi) + theme_spinifex()

theme_spinifex <- function(){
  ggplot2::theme_minimal() + 
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text  = ggplot2::element_blank(),
                   legend.position = "none") 
}

