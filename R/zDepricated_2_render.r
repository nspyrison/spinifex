##
## RENDERING -----
## ggplot2, gganimate, and plotly respectively
##

#' Prepare the ggplot object before passing to either animation package.
#'
#' @description
#' `r lifecycle::badge('superseded')`, see \code{\link{ggtour}}.
#' Typically called by `render_plotly()` or `render_gganimate()`. Takes the
#' result of `array2df()`, and renders them into a ggplot2 object.
#'
#' @param frames The result of `array2df()`, a long df of the projected frames.
#' @param axes Position of the axes, expects one of: 
#' "center", "left", "right", "bottomleft", "topright", "off", or a 
#' map_absolute() call. Defaults to "center".
#' @param manip_col String of the color to highlight the `manip_var`, if used.
#' Defaults to "blue".
#' @param line_size The size of the lines of the unit circle and variable 
#' contributions of the basis. Defaults to .6.
#' @param text_size The size of the text labels of the variable 
#' contributions of the basis. Defaults to 4.
#' @param aes_args A list of aesthetic arguments to passed to 
#' `geom_point(aes(X)`. Any mapping of the data to an aesthetic,
#' for example, `geom_point(aes(color = myCol, shape = myCol))` becomes
#' `aes_args = list(color = myCol, shape = myCol)`.
#' @param identity_args A list of static, identity arguments passed into 
#' `geom_point()`, but outside of `aes()`; `geom_point(aes(), X)`.
#' Typically a single numeric for point size, alpha, or similar.
#' For example, `geom_point(aes(), size = 2, alpha = .7)` becomes
#' `identity_args = list(size = 2, alpha = .7)`.
#' @param ggproto A list of ggplot2 function calls.
#' Anything that would be "added" to ggplot(); in the case of applying a theme,
#' `ggplot() + theme_bw()` becomes `ggproto = list(theme_bw())`.
#' Intended for aesthetic ggplot2 functions (not geom_* family).
#' @export
#' @examples
#' library(spinifex)
#' message("It's suggested to switch to the proto api, see `?ggtour` to get started.")
#' 
#' ## Setup
#' dat_std <- scale_sd(wine[, 2:6])
#' clas    <- wine$Type
#' bas     <- basis_pca(dat_std)
#' mv      <- manip_var_of(bas)
#' 
#' mt_array <- manual_tour(basis = bas, manip_var = mv)
#' mt_df_ls <- array2df(basis_array = mt_array, data = dat_std)
#' 
#' ## Required arguments
#' render_(frames = mt_df_ls)
#' 
#' ## Full arguments
#' require("ggplot2")
#' render_(frames = mt_df_ls, axes = "left", manip_col = "purple",
#'         aes_args = list(color = clas, shape = clas),
#'         identity_args = list(size = 1.5, alpha = .7),
#'         ggproto = list(theme_minimal(),
#'                        ggtitle("My title"),
#'                        scale_color_brewer(palette = "Set2")))
render_ <- function(frames,
                    axes = "center",
                    manip_col = "blue",
                    line_size = .6,
                    text_size = 4,
                    aes_args = list(),
                    identity_args = list(),
                    ggproto = list(theme_spinifex())
){
  #lifecycle::deprecate_warn("0.3.0", "render_()", "spinifex::ggtour()") ## warn higher.
  if(axes == "off" & length(frames) == 1L) stop("render_ called with no data and axes = 'off'")
  #### Initialize
  basis_frames  <- frames$basis_frames
  manip_var     <- attributes(frames$data_frames)$manip_var
  n_frames      <- length(unique(basis_frames$frame))
  p             <- nrow(basis_frames) / n_frames
  d             <- 2L ## Hard-coded assumption for 2D display
  aes_args      <- as.list(aes_args)
  identity_args <- as.list(identity_args)
  ggproto       <- as.list(ggproto)
  
  ## If data exists; fix arg length and plot MUST COME BEFORE AXES
  data_frames <- frames$data_frames ## May be null.
  
  ## Axes setup
  angle <- seq(0L, 2L * pi, length = 360L)
  circ  <- data.frame(x = cos(angle), y = sin(angle))
  ## Scale basis axes/circle
  if(axes != "off"){
    center <- map_relative(data.frame(x = 0L, y = 0L), axes, to = data_frames)
    circ <- map_relative(circ, axes, to = data_frames)
    ## Rejoin frame number to the scaled bases frames
    basis_frames <- map_relative(basis_frames, axes, to = data_frames)
  }
  ## Manip var axes aesthetics
  axes_col <- "grey50"
  axes_siz <- line_size
  if(is.null(manip_var) == FALSE){
    axes_col            <- rep("grey50", p)
    axes_col[manip_var] <- manip_col
    axes_col            <- rep(axes_col, n_frames)
    axes_siz            <- rep(line_size, p)
    axes_siz[manip_var] <- 1.5 * line_size
    axes_siz            <- rep(axes_siz, n_frames)
  }
  
  ## Recycle/replicate args if needed
  if(is.null(data_frames) == FALSE){ ## If data exists
    aes_args_out <- aes_args
    identity_args_out <- identity_args
    tgt_len <- nrow(data_frames)
    ## If AES_args exist, try to replicate
    if(length(aes_args) > 0L){
      .mute <- sapply(1L:length(aes_args), function(i){
        if(length(aes_args[[i]]) > 1L & ## Length more than 1 and vector
           is.vector(as.vector(aes_args[[i]])) == TRUE)
          aes_args_out[[i]] <<- as.factor(rep_len(aes_args[[i]], tgt_len))
      })
    } ## End if AES_args exist
    ## If IDENTITY_args args exist, try to replicate
    if(length(identity_args) > 0L){
      .mute <- sapply(1L:length(identity_args), function(i){
        if(length(identity_args[[i]]) > 1L & ## Length more than 1 and vector
           is.vector(as.vector(identity_args[[i]])) == TRUE)
          identity_args_out[[i]] <<-
            as.factor(rep_len(identity_args[[i]], tgt_len))
      })
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
      ggplot2::geom_point(ggplot2::aes(x = x, y = y, frame = frame, tooltip = tooltip),
                          data_frames))
  } ## End if data exist
  
  ## Render
  gg <- ggplot2::ggplot() + ggproto
  ## Project data points, if data exists
  if(is.null(data_frames) == FALSE) gg <- gg + geom_point_call
  ## Add axes directions if needed
  if(axes != "off"){
    gg <- gg +
      ## Circle path
      ggplot2::geom_path(
        data = circ, color = "grey80", size = line_size, inherit.aes = FALSE,
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
        ggplot2::geom_text(
          data = basis_frames,
          mapping = ggplot2::aes(x = x, y = y,
                                 frame = frame, label = tooltip),
          vjust = "outward", hjust = "outward",
          colour = axes_col, size = text_size)
      )
  }
  
  return(gg)
}


#' Render the frames as a *gganimate* animation.
#'
#' @description
#' `r lifecycle::badge('superseded')`, see \code{\link{ggtour}}.
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
#' (without the directory path). Defaults to NULL (no GIF saved). 
#' For more output control, call `gganimate::anim_save()` on a return object of
#' `render_gganimate()`.
#' @param gif_path Optional, A string of the directory path (without the
#' filename) to save a GIF to. Defaults to NULL (current work directory).
#' @param gganimate_args A list of arguments assigned to a vector passe outside
#' of an aes() call. Anything that would be put in `geom_point(aes(), X)`.
#' Typically a single numeric for point size, alpha, or similar
#' For example, `geom_point(aes(), size = 2, alpha = .7)` becomes
#' `identity_args = list(size = 2, alpha = .7)`.
#' @param ... Passes arguments to `render_(...)`.
#' @seealso \code{\link{render_}} for `...` arguments.
#' @seealso \code{\link[gganimate:anim_save]{gganimate::anim_save}} for more control of .gif output.
#' @export
#' @examples
#' library(spinifex)
#' message("It's suggested to switch to the proto api, see `?ggtour` to get started.")
#' 
#' ## Setup
#' dat_std  <- scale_sd(wine[, 2:6])
#' clas     <- wine$Type
#' bas      <- basis_pca(dat_std)
#' mv       <- manip_var_of(bas)
#' mt       <- manual_tour(basis = bas, manip_var = mv)
#' mt_df_ls <- array2df(basis_array = mt, data = dat_std)
#' 
#' \dontrun{
#' render_gganimate(frames = mt_df_ls)
#' 
#' require("ggplot2")
#' render_gganimate(
#'   frames = mt_df_ls, axes = "bottomleft",
#'   fps = 10, rewind = TRUE, start_pause = 1, end_pause = 1.5,
#'   aes_args = list(color = clas, shape = clas),
#'   identity_args = list(size = 2, alpha = .7),
#'   ggproto = list(theme_void(),
#'                  ggtitle("My title"),
#'                  scale_color_brewer(palette = "Set2")))
#' 
#' ## Saving a .gif(may require additional setup)
#' if(FALSE) ## Don't run by mistake
#'   render_gganimate(frames = mt_df_ls, axes = "bottomleft",
#'                    gif_filename = "myRadialTour.gif", gif_path = "./output")}
render_gganimate <- function(
  fps = 8,
  rewind = FALSE,
  start_pause = .5,
  end_pause = 1,
  gif_filename = NULL,
  gif_path = NULL,
  gganimate_args = list(),
  ...
){
  requireNamespace("gganimate")
  ## Render and animate
  gg  <- render_(...)
  gga <- gg + gganimate::transition_states(frame, transition_length = 0L)
  
  anim_func <- function(...) 
    gganimate::animate(gga, 
                       fps = fps,
                       rewind = rewind,
                       start_pause = fps * start_pause,
                       end_pause = fps * end_pause,
                       ...)
  anim <- do.call(anim_func, args = gganimate_args)
  
  ## Save condition handling
  if(is.null(gif_filename) == FALSE)
    gganimate::anim_save(gif_filename, anim, gif_path)
  if(is.null(gif_path) == FALSE & is.null(gif_filename) == TRUE)
    warning("gif_path supplied with no gif_filename. Add a gif_filename to save a .gif.")
  
  return(anim)
}



#' Animation the frames as a HTML widget.
#'
#' @description
#' `r lifecycle::badge('superseded')`, see \code{\link{ggtour}}.
#' Takes the result of `array2df()` and animations them via `{plotly}`
#' into a  self-contained HTML widget.
#'
#' @param fps Frames animated per second. Defaults to 8.
#' @param html_filename Optional, saves the plotly object as an HTML widget to
#' this string (without the directory path).
#' Defaults to NULL (not saved). For more output control use `save_widget_args` 
#' or call `htmlwidgets::saveWidget()` on a return object of `render_plotly()`.
#' @param save_widget_args A list of arguments to be called in 
#' `htmlwidgets::saveWidget()` when used with a `html_filename`.
#' @param ... Passes arguments to `render_(...)`.
#' @seealso \code{\link{render_}} for `...` arguments.
#' @seealso \code{\link[plotly]{ggplotly}} for source documentation of `tooltip`.
#' @seealso \code{\link[htmlwidgets]{saveWidget}} for more control of .html output.
#' @export
#' @examples
#' library(spinifex)
#' message("It's suggested to switch to the proto api, see `?ggtour` to get started.")
#' 
#' ## Setup
#' dat_std  <- scale_sd(wine[, 2:6])
#' clas     <- wine$Type
#' bas      <- basis_pca(dat_std)
#' mv       <- manip_var_of(bas)
#' mt_array <- manual_tour(basis = bas, manip_var = mv)
#' mt_df_ls <- array2df(basis_array = mt_array, data = dat_std)
#' 
#' \donttest{
#' render_plotly(frames = mt_df_ls)
#' 
#' require("ggplot2")
#' render_plotly(
#'   frames = mt_df_ls, axes = "bottomleft", fps = 10,
#'   aes_args = list(color = clas, shape = clas),
#'   identity_args = list(size = 1.5, alpha = .7),
#'   ggproto = list(theme_bw(), scale_color_brewer(palette = "Set2")))
#' 
#' ## Saving a .gif, may require additional setup
#' if(FALSE) ## Don't accidentally save file
#'   render_plotly(frames = mt_df_ls, axes = "bottomleft", fps = 10,
#'                 html_filename = "myRadialTour.html")}
render_plotly <- function(
  fps = 8,
  html_filename = NULL,
  save_widget_args = list(),
  ...
){
  requireNamespace("plotly")
  ## Render
  gg  <- render_(...) +
    ## Mute plotly not being able to handle horizontal legend
    ggplot2::theme(legend.position = "right",     ## Of plot
                   legend.direction = "vertical", ## With-in aesthetic
                   legend.box = "horizontal")     ## Between aesthetic
    
  ggp <- plotly::ggplotly(p = gg, tooltip = "tooltip")
  ggp <- plotly::animation_opts(p = ggp,
                                frame = 1L / fps * 1000L,
                                transition = 0L,
                                redraw = FALSE)
  ggp <- plotly::layout(
    ggp, showlegend = FALSE,
    yaxis = list(showgrid = FALSE, showline = FALSE), ## note that fixedrange=TRUE is a curse.
    xaxis = list(showgrid = FALSE, showline = FALSE,
                 scaleanchor = "y", scalaratio = 1L)
  )
  ggp <- plotly::config(ggp, displayModeBar = FALSE)
  
  ## Save condition handling
  if(is.null(html_filename) == FALSE){
    saveWidget_func <- function(...)
      htmlwidgets::saveWidget(widget = ggp, file = html_filename, ...)
    do.call(saveWidget_func, args = save_widget_args)
  }
  
  return(ggp)
}


