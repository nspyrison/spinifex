##
## RENDERING -----
## ggplot2, gganimate, and plotly respectively
##

#' Prepair the ggplot object before passing to either animation package.
#'
#' Typically called by `render_plotly()` or `render_gganimate()`. Takes the
#' result of `array2df()`, and renders them into a ggplot2 object.
#'
#' @param frames The result of `array2df()`, a long df of the projected frames.
#' @param axes Position of the axes: "center", "bottomleft", "off", "left",
#' "right". Defaults to "center". 
#' For more control pass a pan_zoom call with NULL x.
#' @param manip_col String of the color to highlight the `manip_var`, if used.
#' Defaults to "blue".
#' @param aes_args A list of aesthetic arguments assigned to a vector passed to
#' an aes() call. Anything that would be put inside of the `geom_point(aes(X))`.
#' Typically a class vector mapped to color and shape.
#' For example, `geom_point(aes(color = myClass, shape = myClass))` becomes
#' `aes_args = list(color = myClass, shape = myClass)`.
#' @param identity_args A list of arguments assigned to a vector passe outside
#' of an aes() call. Anything that would be put in `geom_point(aes(), X)`.
#' Typically a single numeric for point size, alpha, or similar
#' For example, `geom_point(aes(), size = 2, alpha = .7)` becomes
#' `identity_args = list(size = 2, alpha = .7)`.
#' @param ggproto A list of ggplot2 function calls.
#' Anything that would be "added" to ggplot(); in the case of applying a theme,
#' `ggplot() + theme_bw()` becomes `ggproto = list(theme_bw())`.
#' Intended for aesthetic ggplot2 functions (not geom_* family).
#' @export
#' @examples
#' ## Setup
#' dat_std <- tourr::rescale(wine[, 2:14])
#' clas <- wine$Type
#' bas <- basis_lda(dat_std, clas)
#' mv <- manip_var_lda(dat_std, clas)
#' tour_array <- manual_tour(basis = bas, manip_var = mv)
#' tour_df <- array2df(array = tour_array, data = dat_std)
#' 
#' ## Required arguments
#' render_(frames = tour_df)
#' 
#' ## Full arguments
#' render_(frames = tour_df, axes = "left", manip_col = "purple",
#'         aes_args = list(color = clas, shape = clas),
#'         identity_args = list(size = .8, alpha = .7)),
#'         ggproto = list(theme_spinifex(),
#'                        ggtitle("My title"),
#'                        scale_color_brewer(palette = "Set2")))
render_ <- function(frames,
                    axes = "center",
                    manip_col = "blue",
                    aes_args = list(),
                    identity_args = list(),
                    ggproto = theme_spinifex()
){
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
  
  ## Axes setup
  angle <- seq(0L, 2L * pi, length = 360L)
  circ  <- data.frame(x = cos(angle), y = sin(angle))
  ## Scale basis axes/circle
  if (axes != "off"){
    center <- scale_axes(data.frame(x = 0L, y = 0L), axes, to = data_frames)
    circ <- scale_axes(circ, axes, to = data_frames)
    ## Rejoin frmae number to the scaled bases frames
    basis_frames <-
      data.frame(scale_axes(basis_frames[, 1L:d], axes, to = data_frames),
                 basis_frames[, (d + 1L):ncol(basis_frames)])
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
  
  ## Recycle/replicate args if needed
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
      ggplot2::aes(x = x, y = y, frame = frame, ...)
    aes_call <- do.call(aes_func, args = aes_args_out)
    ## geom_point() call
    geom_point_func <- function(aes_call, ...)
      suppressWarnings( ## Suppressed unknown args: frame
        ggplot2::geom_point(aes_call, data = data_frames, ...))
    geom_point_call <-
      do.call(geom_point_func, c(list(aes_call), identity_args_out))
  }else{ ## Else, no data exists
    geom_point_call <- suppressWarnings( ## Suppressed unknown args: frame
      ggplot2::geom_point(ggplot2::aes(x = x, y = y, frame = frame),
                          data_frames))
  } ## End if data exist
  
  ## Ploting
  gg <-
    ggplot2::ggplot() +
    ggproto
  ## Project data points, if data exists
  if (is.null(data_frames) == FALSE){
    gg <- gg + geom_point_call
  }
  ## Add axes directions if needed
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
#' (without the folder path). Defaults to NULL (no GIF saved). 
#' For more output control, call `gganimate::anim_save()` on a return object of
#' `render_gganimate()`.
#' @param gif_path Optional, A string of the directory path (without filename) 
#' to save a GIF to. Defaults to NULL (current work directory).
#' @param ... Optionally passes arguments to the projection points inside the 
#' aesthetics; `geom_point(aes(...))`.
#' @seealso \code{\link{render_}} for `...` arguments.
#' @seealso \code{\link{gganimate::anim_save}} for more control of .gif output.
#' @export
#' @examples
#' ## Setup
#' dat_std <- tourr::rescale(wine[, 2:14])
#' clas <- wine$Type
#' bas <- basis_lda(dat_std, clas)
#' mv <- manip_var_lda(dat_std, clas)
#' tour_array <- manual_tour(basis = bas, manip_var = mv)
#' tour_df <- array2df(array = tour_array, data = dat_std)
#' 
#' \dontrun{
#' ## Required arguments
#' render_gganimate(frames = tour_df)
#' 
#' ## Full arguments (without save)
#' render_gganimate(frames = df_frames, axes = "bottomleft",
#'                  fps = 10, rewind = TRUE, start_pause = 1, end_pause = 1.5,
#'                  aes_args = list(color = clas, shape = clas),
#'                  identity_args = list(size = .8, alpha = .7)),
#'                  ggproto = list(theme_spinifex(),
#'                                 ggtitle("My title"),
#'                                 scale_color_brewer(palette = "Set2")))
#'   
#' if(F){ ## Save as a .gif (may require additional setup)
#'   render_gganimate(frames = df_frames, axes = "bottomleft",
#'                    fps = 10, rewind = TRUE, start_pause = 1, end_pause = 1.5,
#'                    aes_args = list(color = clas, shape = clas),
#'                    identity_args = list(size = .8, alpha = .7)),
#'                    ggproto = list(theme_spinifex(),
#'                                   ggtitle("My title"),
#'                                   scale_color_brewer(palette = "Set2")),
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



#' Animation the frames as a HTML widget.
#'
#' Takes the result of `array2df()` and animations them via `{plotly}`
#' into a  self-contained HTML widget.
#'
#' @param fps Frames animated per second. Defaults to 8.
#' @param tooltip Character vector of aesthetic mappings to show in the
#' hover-over tooltip (passed to `plotly::ggplot()`).
#' Defaults to "none". "all" shows all the aesthetic mappings.
#' The order of text controls the order they appear.
#' For example, tooltip = c("id", "frame", "x", "y", "category", "color").
#' @param html_filename Optional, saves the plotly object as an HTML widget to
#' this string (without folderpath).
#' Defaults to NULL (not saved). For more output control call
#' `htmlwidgets::saveWidget()` on a return object of `render_plotly()`.
#' @param ... Passes arguments to `render_(aes(...))`.
#' @seealso \code{\link{render_}} for `...` arguments.
#' @seealso \code{\link{plotly::ggplotly}} for source documentation of `tooltip`.
#' @seealso \code{\link{htmlwidgets::saveWidget}} for more control of .gif output.

#' @export
#' @examples
#' ## Setup
#' dat_std <- tourr::rescale(wine[, 2:14])
#' clas <- wine$Type
#' bas <- basis_lda(dat_std, clas)
#' mv <- manip_var_lda(dat_std, clas)
#' tour_array <- manual_tour(basis = bas, manip_var = mv)
#' tour_df <- array2df(array = tour_array, data = dat_std)
#' 
#' \dontrun{
#' ## Required arguments
#' render_plotly(frames = df_frames)
#' 
#' ## Full arguments (without save)
#' render_plotly(frames = df_frames, axes = "bottomleft", fps = 10,
#'               tooltip = c("lab", "frame", "x", "y"),
#'               aes_args = list(color = clas, shape = clas),
#'               identity_args = list(size = .8, alpha = .7),
#'               ggproto = list(theme_spinifex(),
#'                              ggtitle("My title"),
#'                              scale_color_brewer(palette = "Set2")))
#' 
#' if(F){ ## Saving .html widget (may require additional setup)
#'   render_plotly(frames = df_frames, , axes = "bottomleft", fps = 10, 
#'                 tooltip = c("id", "frame", "x", "y", "category", "color"),
#'                 aes_args = list(color = clas, shape = clas),
#'                 identity_args = list(size = .8, alpha = .7),
#'                 ggproto = list(theme_spinifex(),
#'                                ggtitle("My title"),
#'                                scale_color_brewer(palette = "Set2")),
#'                 html_filename = "myRadialTour.html")
#' }
#' }
render_plotly <- function(fps = 8L,
                          tooltip = "none",
                          html_filename = NULL,
                          ...){
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

