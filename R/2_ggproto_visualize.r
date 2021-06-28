### UTIL -----


#' Prepare a new ggtour
#'
#' `ggtour()` initializes a ggplot object for a tour, to be animated with 
#' `animate_plotly()` or `animate_ggtour()`. 
#'
#' @param basis_array An array of projection bases for the tour, as produced
#' with `manual_tour()` or `tour::save_history()`.
#' @param data Numeric data to project. If left NULL, will check if it data is 
#' stored as an attribute of the the `basis_array`.
#' @param angle Target angle (in radians) for interpolation for
#' `tour::save_history()` generated `basis_array`. Defaults to .05.
#' @export
#' @family ggtour proto
#' @examples
#' dat <- scale_sd(tourr::flea[, 1:6])
#' bas <- basis_pca(dat)
#' mv <- manip_var_of(bas)
#' mt_path <- manual_tour(bas, manip_var = mv, angle = .1)
#' 
#' ggtour(mt_path, dat) ## Returns headless ggplot(), but required for other spinifex protos
#' 
#' ggt <- ggtour(mt_path, dat, angle = .1) +
#'   proto_basis() +
#'   proto_points(list(color = clas, shape = clas),
#'                     list(size = 1.5))
#' \dontrun{
#' animate_plotly(ggt)
#' }
ggtour <- function(basis_array,
                   data = NULL,
                   angle = .05
){
  if(is.null(data) == TRUE)
    data <- attr(basis_array, "data") ## Can be NULL
  manip_var <- attr(basis_array, "manip_var") ## NULL if not a manual tour
  if(any(class(basis_array) %in% c("matrix", "data.frame"))) ## Format for array2df (_list)
    basis_array <- array(as.matrix(basis_array), dim = c(dim(basis_array), 1L))
  
  ## Interpolate if needed and apply array2df
  if(is.null(manip_var)) ## If tourr::save_history(), interpolate it quietly
    capture.output(basis_array <- tourr::interpolate(basis_array, angle = angle))
  df_ls <- array2df(basis_array, data)
  df_basis <- df_ls$basis_frames
  df_data  <- df_ls$data_frames
  attr(df_basis, "manip_var") <- manip_var ## NULL if not a manual tour
  
  ## map_to condition handling: 
  #### NULL data == unit box, >2d basis == data, 1d data == density and data.
  if(is.null(data)){
    map_to <- data.frame(x = c(-1L, 1L), y = c(-1L, 1L))
  }else{
    d <- ncol(df_basis) - 2L
    map_to <- df_data
  } ## Note: to be basis dim agnostic need to calculate density height in basis_1d for example.
  
  ## Assign hidden prepared dataframes
  assign(x = ".spinifex_df_basis", value = df_basis, envir = globalenv())
  assign(x = ".spinifex_df_data",  value = df_data,  envir = globalenv())
  assign(x = ".spinifex_map_to",   value = map_to,   envir = globalenv())
  
  ## Return ggplot head with theme, 3x .spinifex_* obj assign globally above.
  ggplot2::ggplot() + spinifex::theme_spinifex()
}
# ## Print method
# #### Was a good idea, but ggplot stops working when you change the first class, 
# #### and doesn't effect if you append.
# print.ggtour <- function(x, ...){
#   class(x) <- c("gg", "ggplot")
#   x +
#     proto_basis() +
#     proto_background(gridline_probs = FALSE) +
#     proto_points()
# }


#' Replicate all vector elements of a list
#' 
#' Internal function. To be applied to `aes_args` and `identity_args`, 
#' replicates vectors of length data to length of data*frames for animation.
#'
#' @param list A list of arguments such as those passed in `aes_args` and 
#' `identity_args`.
#' @param nrow_frames Scalar number of rows in the data frames to replicate to.
#' @param nrow_data Scalar number of rows in the data to replicate.
#' @export
#' @family Internal utility
#' @examples
#' ## This function is not meant for external use
lapply_rep_len <- function(list,
                           nrow_frames,
                           nrow_data
){
  .nms <- names(list)
  .mute <- lapply(seq_along(list), function(i){
    .this_vector <- list[[i]]
    if(length(.this_vector) != 1L){
      if(length(.this_vector) != nrow_data)
        warning(paste0("aes_arg '", .nms[i], "' not of length 1 or data."))
      .replicated_vector <- rep_len(.this_vector, nrow_frames)
      list[[i]] <<- .replicated_vector ## Replace the value with the string of the original
    }
  })
  return(list)
}

#' Initialize common obj from .global `ggtour()` objects & test their existence
#' 
#' Internal function. Creates local .objects to be commonly consumed by spinifex
#' proto_* functions
#'
#' @export
#' @family Internal utility
#' @examples
#' ## This function is not meant for external use.
.init4proto <- function(){
  ## Assumption
  if(exists(".spinifex_df_basis") == FALSE) 
    stop("`.spinifex_df_basis` does not exsist, have you run `ggtour()` yet?")
  
  ## Initialization, littering hidden objects 1 level up, not in global.
  .df_basis <<- .spinifex_df_basis ## Give alterable local copies of _basis and _data
  .df_data  <<- .spinifex_df_data
  .map_to   <<- .spinifex_map_to
  .n_frames <<- length(unique(.df_basis$frame))
  .nrow_df_data <<- nrow(.df_data)
  .p <<- nrow(.df_basis) / .n_frames
  .n <<- .nrow_df_data   / .n_frames
  .manip_var <<- attr(.df_basis, "manip_var") ## NULL if not a manual tour
  
  ## Note: Cannot replicate ls args here; passing to wrong environment/scope?
  return()
}

### ANIMATE_* ------

#' Animate a `ggtour()` with the use of `{gganimate}`
#'
#' Animates the static `ggtour()` and added `proto_*()` functions as a 
#' `{gganimate}` animation, .gif without interaction.
#'
#' @param ggtour The return of a `ggtour()`and added `proto_*()` functions.
#' @param fps Scalar number of Frames Per Second, the speed the animation should 
#' play at. 
#' @param rewind Whether or not the animation should play backwards,
#' in reverse order once reaching the end. Defaults to FALSE.
#' @param start_pause The duration in seconds to wait before starting the 
#' animation. Defaults to 1 second.
#' @param end_pause The duration in seconds to wait after ending the animation,
#' before it restarts from the first frame. Defaults to 1 second.
#' @param ... other arguments to pass to `gganimate::animate()`.
#' @export
#' @family ggtour animator
#' @examples
#' dat <- scale_sd(tourr::flea[, 1:6])
#' clas <- tourr::flea$species
#' bas <- basis_pca(dat)
#' mv <- manip_var_of(bas)
#' mt_path <- manual_tour(bas, manip_var = mv, angle = .1)
#' 
#' ggt <- ggtour(mt_path, dat) +
#'   proto_basis() +
#'   proto_background() +
#'   proto_points(aes_args = list(color = clas, shape = clas),
#'                     identity_args = list(size = 1.5, alpha = .7)) +
#' 
#' \dontrun{
#' animate_gganimate(ggtour)
#' 
#' (anim <- animate_gganimate(ggtour, fps = 10, rewind = TRUE,
#'                            start_pause = 1, end_pause = 2))
#' 
#' if(F) ## Example saving gganime to a .gif, may require additional setup.
#'   gganimate::anim_save("my_tour.gif",
#'                        animation = anim,
#'                        path = "./figures")
#' }
animate_gganimate <- function(
  ggtour,
  fps = 8,
  rewind = FALSE,
  start_pause = 1,
  end_pause = 1,
  knit_pdf_anim = FALSE,
  ## Do gganimate::knit_print.gganim() instead of gganimate::animate? If so, ignore above named arguments.
  ... ## Passed to gganimate::animate or gganimate::knit_print.gganim
){
  ## Assumptions
  n_frames <- length(unique(.spinifex_df_basis$frame))
  if(n_frames == 1L) stop("df_basis only has 1 frame, stopping animation.")
  
  ## Discrete jump between frames, no linear interpolation.
  gga <- ggtour + gganimate::transition_states(frame, transition_length = 0L)
  
  ## Normal animation, with applied options, knit_pdf_anim == FALSE
  anim <- gganimate::animate(
    gga, fps = fps, rewind = rewind,
    start_pause = fps * start_pause,
    end_pause = fps * end_pause,
    ...)
  return(anim)
}


#' Animate a `ggtour()` via `{plotly}`
#'
#' Animates the static `ggtour()` and added `proto_*()` functions as a `{plotly}`
#' animation, an .html widget with slider and hover tooltip showing 
#' the row number.
#'
#' @param ggtour The return of a `ggtour()`and added `proto_*()` functions.
#' @param fps Scalar number of Frames Per Second, the speed the animation should 
#' play at. 
#' @param rewind Whether or not the animation should play backwards,
#' in reverse order once reaching the end. Defaults to FALSE.
#' @param start_pause The duration in seconds to wait before starting the 
#' animation. Defaults to 1 second.
#' @param end_pause The duration in seconds to wait after ending the animation,
#' before it restarts from the first frame. Defaults to 1 second.
#' @param ... other arguments to pass to `gganimate::animate()`.
#' @export
#' @family ggtour animator
#' @examples
#' dat <- scale_sd(tourr::flea[, 1:6])
#' clas <- tourr::flea$species
#' bas <- basis_pca(dat)
#' mv <- manip_var_of(bas)
#' mt_path <- manual_tour(bas, manip_var = mv, angle = .1)
#' 
#' ggt <- ggtour(mt_path, dat) +
#'   proto_background() +
#'   proto_basis() +
#'   proto_points(aes_args = list(color = clas, shape = clas),
#'                     identity_args = list(size = 1.5, alpha = .7))
#' 
#' \dontrun{
#' animate_plotly(ggtour)
#' 
#' (anim <- animate_plotly(ggtour, fps = 10))
#' 
#' if(F) ## Example saving ploty to a .html widget, may require additional setup.
#'   htmlwidgets::saveWidget(widget = anim, file = "./figures/my_tour.html",
#'                           selfcontained = TRUE)
#' }
animate_plotly <- function(ggtour,
                           fps = 8,
                           #use_rowname_tooltip = TRUE,
                           ... ## Passed to plotly::layout().
){
  ## Assumptions
  n_frames <- length(unique(.spinifex_df_basis$frame))
  if(n_frames == 1L){
    warning("ggtour df_basis only has 1 frame, printing ggtour ggplot2 object instead.")
    return(print(ggtour))
  }
  
  ## Block plotly.js warning: lack of support for horizontal legend;
  #### https://github.com/plotly/plotly.js/issues/53
  return(
    suppressWarnings(
      plotly::ggplotly(p = ggtour, tooltip = "tooltip") |>
        plotly::animation_opts(frame = 1L / fps * 1000L,
                               transition = 0L, redraw = FALSE) |>
        plotly::layout(showlegend = FALSE,
                       #, fixedrange = TRUE ## is a curse, do not use.
                       yaxis = list(showgrid = FALSE, showline = FALSE),
                       xaxis = list(showgrid = FALSE, showline = FALSE,
                                    scaleanchor = "y", scalaratio = 1L),
                       ...) |>
        plotly::config(displayModeBar = FALSE)
    )
  )
}


### animate_gganimate_knit2pdf -----
# #' Animate a `ggtour()` to be used in knit into a .pdf format.
# #'
# #' Animates the static `ggtour()` and added `proto_*()` functions as `{gganimate}`
# #' animation to be knit into a .pdf format. Will be compatible with Adobe Reader, 
# #' but not all .pdf applications. See 
# #' \url{https://github.com/nspyrison/spinifex/buildignore/animiation_knit2pdf.rmd}
# #' for the required YAML and chunk setings.
# #'
# #' @param ggtour The return of a `ggtour()`and added `proto_*()` functions.
# #' @param fps Scalar number of Frames Per Second, the speed the animation should 
# #' play at. 
# #' @param rewind Whether or not the animation should play backwards,
# #' in reverse order once reaching the end. Defaults to FALSE.
# #' @param start_pause The duration in seconds to wait before starting the 
# #' animation. Defaults to 1 second.
# #' @param end_pause The duration in seconds to wait after ending the animation,
# #' before it restarts from the first frame. Defaults to 1 second.
# #' @param ... other arguments to pass to `gganimate::animate()`.
# #' @export
# #' @family ggtour animator
# #' #' @examples
# #' dat <- scale_sd(tourr::flea[, 1:6])
# #' clas <- tourr::flea$species
# #' bas <- basis_pca(dat)
# #' mv <- manip_var_of(bas)
# #' mt <- manual_tour(bas, manip_var = mv, angle = .1)
# #' 
# #' ggt <- ggtour(mt_path, dat) +
# #'   proto_basis() +
# #'   proto_background() +
# #'   proto_points(aes_args = list(color = clas, shape = clas),
# #'                     identity_args = list(size = 1.5, alpha = .7))
# #' 
# #' \dontrun{
# #' animate_gganimate_knit2pdf(ggtour)
# #' }
# animate_gganimate_knit2pdf <- function(ggtour,
#                                        ... ## Passed gganimate::knit_print.gganim
# ){
#   ## Assumptions
#   n_frames <- length(unique(.spinifex_df_basis$frame))
#   if(n_frames == 1L) stop("df_basis only has 1 frame, stopping animation.")
#   
#   ## Discrete jump between frames, no linear interpolation.
#   gga <- ggtour + gganimate::transition_states(frame, transition_length = 0L)
#   
#   ## Return
#   gganimate::knit_print.gganim(gga, ...)
# }

### PRROTO_BASIS_* ------
#' Tour proto for a 2D basis axes
#'
#' Adds 2D basis axes to the animation, the direction and magnitude of 
#' contributions of the variables to the projection space inscribed in a unit 
#' circle.
#'
#' @param position The position, to place the basis axes relative to the centered 
#' data. Expects one of c("left", "center", "right", "bottomleft", "topright", 
#' "off"), defaults to "left".
#' @param manip_col The color to highlight the manipulation variable with. Not
#' applied if the tour isn't a manual tour. Defaults to "blue".
#' @param line_size Thickness of the lines used to make the axes and unit 
#' circle.
#' @param text_size Size of the text label of the variables.
#' @export
#' @aliases proto_basis
#' @family ggtour proto
#' @examples
#' dat <- scale_sd(tourr::flea[, 1:6])
#' clas <- tourr::flea$species
#' bas <- basis_pca(dat)
#' mv <- manip_var_of(bas)
#' mt_path <- manual_tour(bas, manip_var = mv, angle = .1)
#' 
#' ggt <- ggtour(mt_path, dat) +
#'   proto_basis()
#' \dontrun{
#' animate_plotly(ggt)
#' }
#' 
#' ggt2 <- ggtour(mt_path, dat) +
#'   proto_basis(position = "right", manip_col = "red", 
#'                  line_size = .8, text_size = 8)
#' \dontrun{
#' animate_plotly(ggt2)
#' }
proto_basis <- function(position = c("left", "center", "right", "bottomleft",
                                     "topright", "off"),
                        manip_col = "blue",
                        line_size = 1,
                        text_size = 5
){
  ## Assumptions
  position = match.arg(position) 
  if(position == "off") return()
  
  ## Initialize
  .init4proto()
  
  ## Setup and transform
  .angles <- seq(0L, 2L * pi, length = 360L)
  .circle <- data.frame(x = cos(.angles), y = sin(.angles))
  .center <- map_relative(data.frame(x = 0L, y = 0L), position, .map_to)
  .circle <- map_relative(.circle, position, .map_to)
  .df_basis <- map_relative(.df_basis, position, .map_to)
  ## Aesthetics for the axes segments.
  .axes_col <- "grey50"
  .axes_siz <- line_size
  if (is.null(.manip_var) == FALSE) {
    .axes_col <- rep("grey50", .p)
    .axes_col[.manip_var] <- manip_col
    .axes_col <- rep(.axes_col, .n_frames)
    .axes_siz <- rep(line_size, .p)
    .axes_siz[.manip_var] <- 1.5 * line_size
    .axes_siz <- rep(.axes_siz, .n_frames)
  }
  
  ## Return proto
  return(
    list(
      ggplot2::geom_path(data = .circle, color = "grey80",
                         size = line_size, inherit.aes = FALSE,
                         mapping = ggplot2::aes(x = x, y = y)),
      suppressWarnings(ggplot2::geom_segment( ## Suppress unused arg: frames
        data = .df_basis,
        size = .axes_siz, colour = .axes_col,
        mapping = ggplot2::aes(x = x, y = y, frame = frame,
                               xend = .center[, 1L], yend = .center[, 2L])
      )),
      suppressWarnings(ggplot2::geom_text(
        data = .df_basis,
        colour = .axes_col, size = text_size,
        vjust = "outward", hjust = "outward",
        mapping = ggplot2::aes(x = x, y = y, frame = frame, label = label)
      ))
    )
  )
}


#' Tour proto for a 1D basis axes
#'
#' Adds 1D basis axes to the animation, the direction and magnitude of 
#' contributions of the variables to the projection space.
#'
#' @param manip_col The color to highlight the manipulation variable with. Not
#' applied if the tour isn't a manual tour. Defaults to "blue".
#' @param segment_size Thickness of the bars used to make the axes.
#' @param text_size Size of the text label of the variables.
#' @export
#' @family ggtour proto
#' @examples
#' dat <- scale_sd(tourr::flea[, 1:6])
#' gt <- tourr::save_history(dat, grand_tour(d = 1), max_bases = 3)
#' 
#' ggt <- ggtour(gt, dat) +
#'   proto_basis1d()
#' \dontrun{
#' animate_plotly(ggt)
#' }
#' 
#' ggt2 <- ggtour(gt, dat) +
#'   proto_basis1d(position = "right",
#'                 segment_size = .8, 
#'                 text_size = 8)
#' \dontrun{
#' animate_plotly(ggt2)
#' }
proto_basis1d <- function(position = c("left", "center", "right", "bottomleft",
                                       "topright", "off"),
                          manip_col = "blue",
                          segment_size = 2,
                          text_size = 5
){
  ## Assumptions
  position = match.arg(position)
  if(position == "off") return()
  
  ## Initialize
  .init4proto()
  
  ## Find the height of density to map_to
  .den <- density(.df_data[, 1L])
  .map_to1d <- data.frame(x = quantile(.df_data[, 1L], probs = c(.01, .99)),
                        y = 1.8 * range(.den[[2L]]))
  
  ## Aesthetics for the axes segments
  .axes_col <- "grey50"
  .axes_siz <- segment_size
  if(is.null(.manip_var) == FALSE) {
    .axes_col <- rep("grey50", .p)
    .axes_col[.manip_var] <- manip_col
    .axes_col <- rep(.axes_col, .n_frames)
    .axes_siz <- rep(segment_size, .p)
    .axes_siz[.manip_var] <- 1.5 * segment_size
    .axes_siz <- rep(.axes_siz, .n_frames)
  }
  ## Initialize data.frames, before scaling
  .frame1  <- .df_basis[.df_basis$frame == 1L, ]
  .df_zero <- data.frame(x = 0L, y = 0L)
  .df_seg  <- data.frame(x = .df_basis$x,
                         y = rep_len(.p:1L, length.out = nrow(.df_basis)),
                         frame = .df_basis$frame,
                         label = .df_basis$label)
  .df_txt  <- data.frame(x = -1.33, y = .p:1L, label = .frame1$label)
  .df_rect <- data.frame(x = c(-1L, 1L), y = c(.5, .p + .5))
  .df_seg0 <- data.frame(x = 0L, y = c(.5, .p + .5))
  ## Scale them
  .df_zero <- map_relative(.df_zero, position, .map_to1d)
  .df_seg  <- map_relative(.df_seg,  position, .map_to1d)
  .df_txt  <- map_relative(.df_txt,  position, .map_to1d)
  .df_rect <- map_relative(.df_rect, position, .map_to1d)
  .df_seg0 <- map_relative(.df_seg0, position, .map_to1d)
  
  ## Return proto
  return(list(
    ggplot2::geom_segment(
      ggplot2::aes(x = min(x), y = min(y), xend = max(x), yend = max(y)),
      .df_seg0, color = "grey80", linetype = 2L),
    ggplot2::geom_rect(
      ggplot2::aes(xmin = min(x), xmax = max(x), ymin = min(y), ymax = max(y)),
      .df_rect, fill = NA, color = "grey60"),
    ggplot2::geom_text(
      ggplot2::aes(x, y, label = label), .df_txt, size = text_size, color = .axes_col),
    suppressWarnings(ggplot2::geom_segment(
      ggplot2::aes(x, y, xend = .df_zero[, 1L], yend = y, frame = frame),
      .df_seg, color = .axes_col, size = .axes_siz))
  ))
}


### PROTO_* for data obs ----
#' Tour proto for data point
#'
#' Adds `geom_point()` of the projected data.
#'
#' @param position The position, to place the basis axes relative to the centered 
#' data. Expects one of c("left", "center", "right", "bottomleft", "topright", 
#' "off"), defaults to "left".
#' @param manip_col The color to highlight the manipulation variable with. Not
#' applied if the tour isn't a manual tour. Defaults to "blue".
#' @param line_size Thickness of the lines used to make the axes and unit 
#' circle.
#' @param text_size Size of the text label of the variables.
#' @export
#' @aliases proto_points
#' @family ggtour proto
#' @examples
#' dat <- scale_sd(tourr::flea[, 1:6])
#' clas <- tourr::flea$species
#' bas <- basis_pca(dat)
#' mv <- manip_var_of(bas)
#' gt_path <- tourr::save_history(dat, grand_tour(), max_bases = 5)
#' 
#' ggt <- ggtour(mt_path, dat) +
#'   proto_point()
#' \dontrun{
#' animate_plotly(ggt)
#' }
#' 
#' ggt2 <- ggtour(mt_path, dat) +
#'   proto_point(list(color = clas, shape = clas),
#'                list(size = 2, alpha = .7))
#' \dontrun{
#' animate_plotly(ggt2)
#' }
proto_point <- function(aes_args = list(),
                        identity_args = list()
){
  ## Assumptions
  if(is.null(.spinifex_df_data) == TRUE) return()
  position <- "center" ## Data assumed center.
  aes_args <- as.list(aes_args)
  identity_args <- as.list(identity_args)
  
  ## Initialize, replicate arg lists.
  .init4proto()
  aes_args <- lapply_rep_len(aes_args, .nrow_df_data, .n)
  identity_args <- lapply_rep_len(identity_args, .nrow_df_data, .n)
  .df_data$tooltip <- paste0("rownum: ", rep_len(1L:.n, .nrow_df_data))
  
  ## do.call aes() over the aes_args
  .aes_func <- function(...)
    ggplot2::aes(x = x, y = y, frame = frame, tooltip = tooltip, ...)
  .aes_call <- do.call(.aes_func, aes_args)
  ## do.call geom_point() over the identity_args
  .geom_func <- function(...) suppressWarnings(
    ggplot2::geom_point(mapping = .aes_call, data = .df_data, ...))
  
  ## Return proto
  return(do.call(.geom_func, identity_args))
}


#' Tour proto for data origin zero mark
#'
#' Adds a zero mark showing the location of the origin for the central data area.
#'
#' @param position The position, to place the basis axes relative to the centered 
#' data. Expects one of c("left", "center", "right", "bottomleft", "topright", 
#' "off"), defaults to "left".
#' @export
#' @family ggtour proto
#' @examples
#' dat <- scale_sd(tourr::flea[, 1:6])
#' clas <- tourr::flea$species
#' bas <- basis_pca(dat)
#' mv <- manip_var_of(bas)
#' gt_path <- tourr::save_history(dat, grand_tour(), max_bases = 5)
#' 
#' ggt <- ggtour(mt_path, dat) +
#'   proto_origin() +
#'   proto_points()
#' \dontrun{
#' animate_plotly(ggt)
#' }
##TODO: gridlines still not lined up wit zero mark as it's powered from the extrema, not 0.
##TODO: Tune the right line_size for the grids.
proto_origin <- function(){
  ## Assumptions
  if(is.null(.spinifex_df_data) == TRUE) return()
  position <- "center" ## Assumes data is in the center.
  
  ## Initialize
  .init4proto()
  
  #### Setup zero mark, 5% on each side.
  .center <- map_relative(data.frame(x = 0L, y = 0L), "center", .map_to)
  .min <- min(min(.map_to[, 1L]), min(.map_to[, 2L]))
  .max <- max(max(.map_to[, 1L]), max(.map_to[, 2L]))
  .tail <- .05 * (.max - .min)
  
  .df_zero_mark <- data.frame(x     = c(.center[, 1L] - .tail, .center[, 1L]),
                              x_end = c(.center[, 1L] + .tail, .center[, 1L]),
                              y     = c(.center[, 2L], .center[, 2L] - .tail),
                              y_end = c(.center[, 2L], .center[, 2L] + .tail)
  )
  
  ## Return
  return(
    ggplot2::geom_segment(
      data = .df_zero_mark,
      color = "grey60", size = 1L, alpha = .7,
      mapping = ggplot2::aes(x = x, y = y, xend = x_end, yend = y_end)
    )
  )
}

#' Tour proto for data, 1D density, with rug marks
#'
#' Adds `geom_density()` and `geom_rug` of the projected data. Density 
#' `postion = "stack"` does not work with animate_plotly, GH issue is open. 
#' 
#' @param aes_args A list of aesthetic arguments to passed to 
#' `geom_point(aes(X)`. Any mapping of the data to an aesthetic,
#' for example, `geom_point(aes(color = myCol, shape = myCol))` becomes
#' `aes_args = list(color = myCol, shape = myCol)`.
#' @param identity_args A list of static, identity arguments passed into 
#' `geom_point()`, but outside of `aes()`; `geom_point(aes(), X)`.
#' Typically a single numeric for point size, alpha, or similar.
#' @export
#' @aliases proto_density1d
#' @family ggtour proto
#' @examples
#' dat <- scale_sd(tourr::flea[, 1:6])
#' clas <- tourr::flea$species
#' bas <- basis_pca(dat)
#' gt_path <- save_history(dat, grand_tour(), max = 3, start = bas)
#' 
#' ggt <- ggtour(gt_path, dat) +
#'   proto_density(aes_args = list(color = clas, fill = clas)) + 
#'   proto_basis1d()
#' 
#' animate_plotly(ggt)
proto_density <- function(aes_args = list(),
                          identity_args = list(),
                          density_position = c("identity", "stack")
){
  ## Assumptions
  position <- "center" ## Data assumed center.
  density_position <- match.arg(density_position)
  ## "identity" is the only position working right now. 
  ## see: https://github.com/ropensci/plotly/issues/1544
  aes_args <- as.list(aes_args)
  identity_args <- as.list(identity_args)
  
  ## Initialize, replicate list args
  .init4proto()
  aes_args <- lapply_rep_len(aes_args, .nrow_df_data, .n)
  identity_args <- lapply_rep_len(identity_args, .nrow_df_data, .n)
  
  ## do.call aes() over the aes_args
  .aes_func <- function(...)
    ggplot2::aes(x = x, frame = frame, ...)
  .aes_call <- do.call(.aes_func, aes_args)
  ## do.call geom over identity_args
  .geom_func1 <- function(...) suppressWarnings(
    ggplot2::geom_density(mapping = .aes_call, data = .df_data, ...,
                          position = density_position, color = "black")
  )
  .geom_call1 <- do.call(.geom_func1, identity_args)
  ## do.call geom over identity_args #2:
  .geom_func2 <- function(...) suppressWarnings(
    ggplot2::geom_rug(mapping = .aes_call, data = .df_data, ...)
  )
  .geom_call2 <- do.call(.geom_func2, identity_args)
  
  ## Return proto
  return(list(.geom_call1, .geom_call2))
}


#' Tour proto for data, text labels
#'
#' Adds `geom_text()` of the projected data.
#'
#' @param label A character vector, the texts that should be in the location of
#' the data. Default is NULL, will b e
#' @param aes_args A list of aesthetic arguments to passed to 
#' `geom_point(aes(X)`. Any mapping of the data to an aesthetic,
#' for example, `geom_point(aes(color = myCol, shape = myCol))` becomes
#' `aes_args = list(color = myCol, shape = myCol)`.
#' @param identity_args A list of static, identity arguments passed into 
#' `geom_point()`, but outside of `aes()`; `geom_point(aes(), X)`.
#' Typically a single numeric for point size, alpha, or similar.
#' @export
#' @family ggtour proto
#' @examples
#' dat <- scale_sd(tourr::flea[, 1:6])
#' clas <- tourr::flea$species
#' bas <- basis_pca(dat)
#' mv <- manip_var_of(bas)
#' gt_path <- tourr::save_history(dat, grand_tour(), max_bases = 5)
#' 
#' ggt <- ggtour(mt_path, dat) +
#'   proto_text(list(color = clas))
#' \dontrun{
#' animate_plotly(ggt)
#' }
proto_text <- function(aes_args = list(),
                       identity_args = list(),
                       label = NULL
){
  ## Assumptions
  if(is.null(.spinifex_df_data) == TRUE) return()
  position <- "center" ## Data assumed center.
  aes_args <- as.list(aes_args)
  identity_args <- as.list(identity_args)
  
  ## Initialize, replicate arg lists.
  .init4proto()
  if(is.null(label)) label <- 1L:.n
  aes_args <- lapply_rep_len(aes_args, .nrow_df_data, .n)
  identity_args <- lapply_rep_len(identity_args, .nrow_df_data, .n)
  
  ## do.call aes() over the aes_args
  .aes_func <- function(...)
    ggplot2::aes(x = x, y = y, frame = frame, ...,
                 label = rep_len(label, .nrow_df_data))
  .aes_call <- do.call(.aes_func, aes_args)
  ## do.call geom_point() over the identity_args 
  .geom_func <- function(...)suppressWarnings(
    ggplot2::geom_text(mapping = .aes_call, data = .df_data, ...)
  )
  
  ## Return proto
  return(do.call(.geom_func, identity_args))
}

#' Tour proto for data, hexagonal heatmap
#'
#' Adds `geom_hex()` of the projected data.
#'
#' @param bins Numeric vector giving number of bins in both vertical and 
#' horizontal directions. Defaults to 30.
#' @param aes_args A list of aesthetic arguments to passed to 
#' `geom_point(aes(X)`. Any mapping of the data to an aesthetic,
#' for example, `geom_point(aes(color = myCol, shape = myCol))` becomes
#' `aes_args = list(color = myCol, shape = myCol)`.
#' @param identity_args A list of static, identity arguments passed into 
#' `geom_point()`, but outside of `aes()`; `geom_point(aes(), X)`.
#' Typically a single numeric for point size, alpha, or similar.
#' @export
#' @family ggtour proto
#' @examples
#' diam_sub <- diamonds[sample(nrow(diamonds), 1000),]
#' dat <- scale_sd(diam_sub[, c(1, 5:6, 8:10)])
#' gt_path <- save_history(dat, grand_tour(), max = 3)
#' 
#' ggp <- ggtour(gt_path, dat) +
#'   proto_basis() +
#'   proto_hex(bins = 20)
#' 
#' ## Doesn't work with animate_plotly()
#' animate_gganimate(ggp)
proto_hex <- function(aes_args = list(),
                      identity_args = list(),
                      bins = 30
){
  ## Assumptions
  if(is.null(.spinifex_df_data) == TRUE) return()
  position <- "center" ## Data assumed center.
  aes_args <- as.list(aes_args)
  identity_args <- as.list(identity_args)
  
  ## Initialize, replicate arg lists.
  .init4proto()
  aes_args <- lapply_rep_len(aes_args, .nrow_df_data, .n)
  identity_args <- lapply_rep_len(identity_args, .nrow_df_data, .n)
  
  ## do.call aes() over the aes_args
  .aes_func <- function(...)
    ggplot2::aes(x = x, y = y, frame = frame, group = frame, ...)
  .aes_call <- do.call(.aes_func, aes_args)
  ## do.call geom_point() over the identity_args
  .geom_func <- function(...) suppressWarnings(
    ggplot2::geom_hex(mapping = .aes_call, data = .df_data, bins = bins, ...)
  )
  
  ## Return proto
  return(do.call(.geom_func, identity_args))
}
