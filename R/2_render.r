#' Turns a tour path array into a long data frame.
#'
#' Typically called by a wrapper function, `play_manual_tour` or 
#' `play_tour_path`. Takes the result of `tourr::save_history()` or 
#' `manual_tour()` and restructures the data from an array to a long data frame 
#' for use in ggplots.
#'
#' @param array A (p, d, n_frames) array of a tour, the output of 
#' `manual_tour()`.
#' @param data Optional, (n, p) dataset to project, consisting of numeric 
#' variables.
#' @param lab Optional, labels for the reference frame of length 1 or the 
#' number of variables used. Defaults to an abbreviation of the variables.
#' @return A list containing an array of basis frames (p, d, n_frames) and
#' an array of data frames (n, d, n_frames) if data is present.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[, 1:6])
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' single_frame <- array(rb, dim = c(dim(rb), 1))
#' attr(single_frame, "manip_var") <- sample(1:ncol(flea_std), 1)
#' array2df(array = single_frame)
#' 
#' tour_array <- manual_tour(basis = rb, manip_var = 4)
#' array2df(array = tour_array, data = flea_std,
#'          lab = paste0("MyLabs", 1:nrow(rb)))
array2df <- function(array, 
                     data = NULL,
                     lab = NULL) {
  ## Initialize
  manip_var <- attributes(array)$manip_var
  p <- dim(array)[1L]
  n_frames <- dim(array)[3L]
  
  ## Basis condition handling
  basis_frames <- NULL
  for (frame in 1:n_frames) {
    basis_rows <- data.frame(cbind(array[,, frame], frame))
    basis_frames <- rbind(basis_frames, basis_rows)
  }
  colnames(basis_frames) <- c("x", "y", "frame")
  
  ## Data; if exists, array to long df
  if(is.null(data) == FALSE){
    data <- as.matrix(data)
    data_frames <- NULL
    for (frame in 1L:n_frames){
      new_frame <- data %*% array[,, frame]
      ## Center the new frame
      new_frame[, 1] <- new_frame[, 1] - mean(new_frame[, 1])
      new_frame[, 2] <- new_frame[, 2] - mean(new_frame[, 2])
      new_frame <- cbind(new_frame, frame)
      data_frames <- rbind(data_frames, new_frame)
    }
    data_frames <- as.data.frame(data_frames)
    colnames(data_frames) <- c("x", "y", "frame")
  }
  
  ## Labels and attribute condition handling
  basis_frames$lab <- NULL
  if(is.null(lab) == FALSE){
    basis_frames$lab <- rep(lab, nrow(basis_frames) / length(lab))
  } else {
    if(!is.null(data)) {basis_frames$lab <- abbreviate(colnames(data), 3L)
    } else {
      basis_frames$lab <- paste0("V", 1L:p)
    }
  }
  attr(basis_frames, "manip_var") <- manip_var
  
  ## Frame condition handling
  df_frames <- list(basis_frames = basis_frames)
  if(is.null(data) == FALSE) {
    df_frames <- list(basis_frames = basis_frames, data_frames = data_frames)
  }
  
  ## Return
  df_frames
}



#' Prepair the ggplot object before passing to either animation package.
#'
#' Typically called by `render_plotly()` or `render_gganimate()`. Takes the 
#' result of `array2df()`, and renders them into a ggplot2 object. 
#'
#' @param frames The result of `array2df()`, a long df of the projected frames.
#' @param axes Position of the axes: "center", "bottomleft", "off", "left", 
#' "right". Defaults to "center".
#' @param manip_col String of the color to highlight the `manip_var`, if used.
#' Defaults to "blue".
#' @param ggproto Accepts a list of gg functions. Think of this as an 
#' alternative format to `ggplot() + ggproto[[1]]`.
#' Intended for passing a `ggplot2::theme_*()` and related aesthetic functions.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[, 1:6])
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' array_frames <- manual_tour(basis = rb, manip_var = 4)
#' df_frames <- array2df(array = array_frames, data = flea_std)
#' 
#' render_(frames = df_frames)
#' 
#' ## Mapping color, shape, size, and alpha 
#' flea_class <- tourr::flea$species
#' render_(frames = df_frames, axes = "bottomleft", manip_col = "purple",
#'         color = flea_class, shape = flea_class,
#'         size = .1, alpha = .5)
#' 
#' ## Using ggproto; a list of ggplot2 calls to add to ggplot().
#' render_(frames = df_frames,
#'         color = flea_class, shape = flea_class,
#'         ggproto = list(ggplot2::theme_bw(), 
#'                        ggplot2::ggtitle("My radial tour"),
#'                        ggplot2::scale_color_brewer(palette = "Dark2")))
render_ <- function(frames,
                    axes = "center",
                    manip_col = "blue",
                    aes_args = list(),
                    identity_args = list(),
                    ggproto = list(ggplot2::theme_void()),
                    ...) {
  if(axes == "off" & length(frames) == 1L) stop("render_ called with no data and axes = 'off'")
  
  #### Initialize
  basis_frames  <- data.frame(frames[["basis_frames"]])
  manip_var     <- attributes(frames$basis_frames)$manip_var
  n_frames      <- length(unique(basis_frames$frame))
  p             <- nrow(basis_frames) / n_frames
  d             <- 2L ## Hardcoded assumtion for 2D display
  aes_args      <- as.list(aes_args)
  identity_args <- as.list(identity_args)
  ggproto       <- as.list(ggproto)
  
  ## If data exists; fix arg length and plot MUST COME BEFORE AXES
  data_frames <- NULL ## If NULL, scale_axes defaults to df(x=c(0,1), y=c(0,1))
  if (length(frames) == 2L) data_frames <- data.frame(frames[["data_frames"]])
  
  #### Continue initialization
  ## Axes unit circle
  angle          <- seq(0L, 2L * pi, length = 360L)
  circ           <- data.frame(x = cos(angle), y = sin(angle))
  ## Scale basis axes/circle
  if (axes != "off"){
    center       <- scale_axes(data.frame(x = 0L, y = 0L),
                               axes, to = data_frames)
    circ         <- scale_axes(circ, axes, to = data_frames)
    basis_frames <-
      data.frame(scale_axes(basis_frames[, 1L:d], axes, to = data_frames), ## scaled Proj XY
                 basis_frames[, (d + 1L):ncol(basis_frames)]) ## fram number
  }
  ## Manip var axes asethetics
  axes_col <- "grey50"
  axes_siz <- 0.3
  if(is.null(manip_var) == FALSE){
    axes_col            <- rep("grey50", p)
    axes_col[manip_var] <- manip_col
    axes_col            <- rep(axes_col, n_frames)
    axes_siz            <- rep(0.3, p)
    axes_siz[manip_var] <- 1L
    axes_siz            <- rep(axes_siz, n_frames)
  }
  ## Scaling
  x_min <- min(c(circ[, 1L], data_frames[, 1L])) - .1
  x_max <- max(c(circ[, 1L], data_frames[, 1L])) + .1
  y_min <- min(c(circ[, 2L], data_frames[, 2L])) - .1
  y_max <- max(c(circ[, 2L], data_frames[, 2L])) + .1
  #### End initialize
  
  #### Recycle/replicate args if needed, then apply
  if(length(frames) == 2L){ ## If data exists
    tgt_len <- nrow(data_frames)
    aes_args_out <- aes_args
    identity_args_out <- identity_args
    ## If AES_args exist, try to replicate
    if(length(aes_args) > 0L){
      for(i in 1:length(aes_args)){
        if(length(aes_args[[i]]) > 1L & ## Length more than 1 and vector
           is.vector(as.vector(aes_args[[i]])) == TRUE)
          aes_args_out[[i]] <- as.factor(rep_len(aes_args[[i]], tgt_len))
      }
    } ## End if AES_args exist
    ## If IDENTITY_args args exist, try to replicate
    if (length(identity_args) > 0L){
      for(i in 1:length(identity_args)){
        if(length(identity_args[[i]]) > 1L & ## Length more than 1 and vector
           is.vector(as.vector(identity_args[[i]])) == TRUE)
          identity_args_out[[i]] <-
            as.factor(rep_len(identity_args[[i]], tgt_len))
      }
    } ## End if IDENTITY_args exist
    ## aes() call
    aes_func <- function(...)
      with(data_frames, ggplot2::aes(x = x, y = y, frame = frame, ...))
    aes_call <- do.call(aes_func, args = aes_args_out)
    ## geom_point() call
    geom_point_func <- function(aes_call, ...)
      suppressWarnings(
        ggplot2::geom_point(aes_call, data = data_frames, ...))
    geom_point_call <-
      do.call(geom_point_func, c(list(aes_call), identity_args_out))
  }else{ ## Else, no data exists
    geom_point_call <- suppressWarnings(
      ggplot2::geom_point(ggplot2::aes(x = x, y = y, frame = frame), 
                          data_frames))
  } ## End if data exist
  
  ## Ploting
  gg <-
    ggplot2::ggplot() +
    ggplot2::xlim(x_min, x_max) +
    ggplot2::ylim(y_min, y_max) +
    ggproto
  
  ## Project data points, if data exists
  if (is.null(data_frames) == FALSE){
    gg <- gg + geom_point_call
  }
  
  ## Add axes directions if needed:
  if (axes != "off"){
    gg <- gg +
      ## Circle path
      ggplot2::geom_path(
        data = circ, color = "grey80", size = .3, inherit.aes = FALSE,
        mapping = ggplot2::aes(x = x, y = y)
      ) +
      ## Basis axes segments
      suppressWarnings( ## Suppress for unused aes "frame".
        ggplot2::geom_segment(
          data = basis_frames, size = axes_siz, colour = axes_col,
          mapping = ggplot2::aes(x = x, y = y, frame = frame,
                                 xend = center[, 1L], yend = center[, 2L])
        )
      ) +
      ## Basis axes text labels
      suppressWarnings( ## Suppress for unused aes "frame".
        ggplot2::geom_text(data = basis_frames,
                           mapping = ggplot2::aes(x = x, y = y,
                                                  frame = frame, label = lab),
                           vjust = "outward", hjust = "outward",
                           colour = axes_col, size = 4L)
      )
  }
  
  ## Return
  gg
}


#' Render the frames as a *gganimate* animation.
#'
#' Takes the result of `array2df()` and renders them into a 
#' *gganimate* animation.
#'
#' @param fps Frames animated per second. Defaults to 8.
#' @param rewind Logical, should the animation play backwards after reaching 
#' the end? Default to FALSE.
#' @param start_pause Number of seconds to pause on the first frame for.
#' Defaults to .5.
#' @param end_pause Number of seconds to pause on the last frame for.
#' Defaults to 1.
#' @param gif_filename Optional, saves the animation as a GIF to this string 
#' (without the folder path). Defaults to NULL (no GIF saved). For more control 
#' call `gganimate::anim_save()` on a return object of `render_gganimate()`.
#' @param gif_path Optional, A string of the directory path (without filename) 
#' to save a GIF to. Defaults to NULL (current work directory).
#' @param ... Optionally passes arguments to the projection points inside the 
#' aesthetics; `geom_point(aes(...))`.
#' @seealso \code{\link{render_}} for more manual control.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[, 1:6])
#' flea_class <- tourr::flea$species
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' array_frames <- manual_tour(basis = rb, manip_var = 4)
#' df_frames <- array2df(array = array_frames, data = flea_std)
#' 
#' \dontrun{
#' render_gganimate(frames = df_frames)
#' 
#' render_gganimate(frames = df_frames, axes = "bottomleft", 
#'                  color = flea_class, shape = flea_class, size = 20, alpha = .6,
#'                  ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")),
#'                  fps = 10, rewind = TRUE)
#'   
#' if(F){ ## Saving .gif may require additional setup
#'   render_gganimate(frames = df_frames, axes = "right",
#'                    color = flea_class, shape = flea_class, size = 2,
#'                    ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")),
#'                    fps = 6, rewind = TRUE,
#'                    gif_filename = "myRadialTour.gif", gif_path = "./output")
#' }
#' }
render_gganimate <- function(fps = 8L,
                             rewind = FALSE,
                             start_pause = .5,
                             end_pause = 1L,
                             gif_filename = NULL,
                             gif_path = NULL,
                             ...) {
  requireNamespace("gganimate")
  ## Render and animate
  gg  <- render_(...)
  gga <- gg + gganimate::transition_states(frame, transition_length = 0L)
  anim <- gganimate::animate(gga, 
                             fps = fps,
                             rewind = rewind,
                             start_pause = fps * start_pause,
                             end_pause = fps * end_pause)
  ## Save condition handling
  if(is.null(gif_filename) == FALSE)
    gganimate::anim_save(gif_filename, anim, gif_path)
  if(is.null(gif_path) == FALSE & is.null(gif_filename) == TRUE) 
    warning("gif_path supplied with no gif_filename. Add a gif_filename to save a .gif.")
  anim
}



#' Render the frames as a *plotly* animation.
#'
#' Takes the result of `array2df()` and renders them into a 
#' *plotly* animation.
#'
#' @param fps Frames animated per second. Defaults to 8.
#' @param rewind Logical, should the animation play backwards after reaching 
#' the end? Default to FALSE.
#' @param tooltip Character vector of aesthetic mappings to show in the `plotly`
#' hover-over tooltip. Defaults to "none". "all" shows all the 
#' aesthetic mappings. The order of variables controls the order they appear. 
#' For example, tooltip = c("id", "frame", "x", "y", "category", "color").
#' @param html_filename Optional, saves the plotly object as an HTML widget to this string 
#' (without folderpath). Defaults to NULL (not saved). For more control call 
#' `htmlwidgets::saveWidget()` on a return object of `render_plotly()`.
#' @param ... Passes arguments to `render_(aes(...))`.
#' @seealso \code{\link{render_}} for more manual control.
#' @export
#' @examples
#' flea_std   <- tourr::rescale(tourr::flea[, 1:6])
#' flea_class <- tourr::flea$species
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' array_frames <- manual_tour(basis = rb, manip_var = 4)
#' df_frames <- array2df(array = array_frames, data = flea_std)
#' 
#' \dontrun{
#' render_plotly(frames = df_frames)
#' 
#' render_plotly(frames = df_frames, axes = "bottomleft", 
#'               col = flea_class, pch = flea_class, size = 2, alpha = .6,
#'               ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")),
#'               fps = 10, tooltip = "all")
#' 
#' if(F){ ## Saving .html widget may require additional setup
#'   render_plotly(frames = df_frames, axes = "right", fps = 6,
#'                 col = flea_class, pch = flea_class, size = 1.5,
#'                 ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")),
#'                 html_filename = "myRadialTour.html")
#' }
#' }
render_plotly <- function(fps = 8L,
                          tooltip = "none",
                          html_filename = NULL,
                          ...) {
  requireNamespace("plotly")
  ## Render
  gg  <- render_(...)
  ggp <- plotly::ggplotly(p = gg, tooltip = tooltip)
  ggp <- plotly::animation_opts(p = ggp, 
                                frame = 1L / fps * 1000L, 
                                transition = 0L, 
                                redraw = FALSE)
  ggp <- plotly::layout(ggp, showlegend = FALSE, 
                        yaxis = list(showgrid = FALSE, showline = FALSE),
                        xaxis = list(scaleanchor = "y", scalaratio = 1L,
                                     showgrid = FALSE, showline = FALSE)
  )
  ## Save condition handling
  if (is.null(html_filename) == FALSE)
    htmlwidgets::saveWidget(ggp, html_filename)
  ## Return
  ggp
}

