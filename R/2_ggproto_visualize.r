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
#' clas <- tourr::flea$species
#' bas <- basis_pca(dat)
#' mv <- manip_var_of(bas)
#' mt_path <- manual_tour(bas, manip_var = mv, angle = .1)
#' 
#' ggtour(mt_path, dat) ## Returns headless ggplot(), but required for other spinifex protos
#' 
#' ggt <- ggtour(mt_path, dat, angle = .1) +
#'   proto_basis() +
#'   proto_point(list(color = clas, shape = clas),
#'               list(size = 1.5))
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
    .mute <- utils::capture.output(
      basis_array <- tourr::interpolate(basis_array, angle = angle))
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
  assign(x = ".spinifex_df_basis", value = df_basis)
  assign(x = ".spinifex_df_data",  value = df_data )
  assign(x = ".spinifex_map_to",   value = map_to  )
  
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
#     proto_origin(gridline_probs = FALSE) +
#     proto_point()
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
#' Internal expression. Creates local .objects to be commonly consumed by 
#' spinifex proto_* functions.
#'
#' @export
#' @family Internal utility
#' @examples
#' ## This expression. is not meant for external use.
### .init4proto expression -----
.init4proto <- expression({ ## expression, not function
  ## Assumption
  if(exists(".spinifex_df_basis") == FALSE) 
    stop("`.spinifex_df_basis` does not exsist, have you run `ggtour()` yet?")
  
  ## Initialization, littering hidden objects 1 level up, not in global.
  .df_basis <- .spinifex_df_basis ## Give alterable local copies of _basis and _data
  .df_data  <- .spinifex_df_data
  .map_to   <- .spinifex_map_to
  .n_frames <- length(unique(.df_basis$frame))
  .nrow_df_data <- nrow(.df_data)
  .p <- nrow(.df_basis) / .n_frames
  .n <- .nrow_df_data   / .n_frames
  .manip_var <- attr(.df_basis, "manip_var") ## NULL if not a manual tour
})

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
#'   proto_origin() +
#'   proto_point(aes_args = list(color = clas, shape = clas),
#'               identity_args = list(size = 1.5, alpha = .7))
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
  ... ## Passed to gganimate::animate or gganimate::knit_print.gganim
){
  ## Assumptions
  if(length(ggtour$layers) == 0L) stop("No layers found, did you forget to add a proto_*?")
  n_frames <- length(unique(.spinifex_df_basis$frame))
  if(n_frames == 1L){
    warning("ggtour df_basis only has 1 frame, printing ggtour ggplot2 object instead.")
    return(print(ggtour))
  }
  
  ## Discrete jump between frames, no linear interpolation.
  gga <- ggtour + gganimate::transition_states(frame, transition_length = 0L)
  
  ## Normal animation, with applied options, knit_pdf_anim == FALSE
  return(
    gganimate::animate(
      gga, fps = fps, rewind = rewind,
      start_pause = fps * start_pause,
      end_pause = fps * end_pause, ...)
  )
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
#'   proto_origin() +
#'   proto_basis() +
#'   proto_point(aes_args = list(color = clas, shape = clas),
#'               identity_args = list(size = 1.5, alpha = .7))
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
                           ... ## Passed to plotly::layout().
){
  ## Assumptions
  if(length(ggtour$layers) == 0L) stop("No layers found, did you forget to add a proto_*?")
  n_frames <- length(unique(.spinifex_df_basis$frame))
  if(n_frames == 1L){
    warning("ggtour df_basis only has 1 frame, applying just plotly::ggplotly instead.")
    return(plotly::ggplotly(p = ggtour, tooltip = "rownum"))
  }
  
  ## Block plotly.js warning: lack of support for horizontal legend;
  #### https://github.com/plotly/plotly.js/issues/53
  return(
    suppressWarnings(
      plotly::ggplotly(p = ggtour, tooltip = "rownum") |>
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
# #'   proto_origin() +
# #'   proto_point(aes_args = list(color = clas, shape = clas),
# #'               identity_args = list(size = 1.5, alpha = .7))
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
#' Tour proto for a 2D and 1D basis axes respectively
#'
#' Adds basis axes to the animation, the direction and magnitude of 
#' contributions of the variables to the projection space inscribed in a unit 
#' circle for 2D or rectangle of unit width for 1D.
#'
#' @param position The position, to place the basis axes relative to the centered 
#' data. Expects one of c("left", "center", "right", "bottomleft", "topright", 
#' "off"), defaults to "left".
#' @param manip_col The color to highlight the manipulation variable with. Not
#' applied if the tour isn't a manual tour. Defaults to "blue".
#' @param line_size (2D bases only) the thickness of the lines used to make the 
#' axes and unit circle. Defaults to 1.
#' @param text_size Size of the text label of the variables.
#' @export
#' @aliases proto_basis
#' @family ggtour proto
#' @examples
#' ## 2D case:
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
#'   proto_basis(position = "right", manip_col = "green",
#'               line_size = .8, text_size = 8)
#' \dontrun{
#' animate_plotly(ggt2)
#' }
proto_basis <- function(position = c("left", "center", "right",
                                     "bottomleft", "topright", "off"),
                        manip_col = "blue",
                        line_size = 1,
                        text_size = 5
){
  ## Assumptions
  position = match.arg(position) 
  if(position == "off") return()
  
  ## Initialize
  eval(.init4proto)
  if(is.null(.df_data)) stop("Data missing. ggtour() on a manual tour without passing data?")
  if(is.null(.df_data$y)) stop("Projection y not found. Did you apply to a 1D tour?")
  
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


#' @rdname proto_basis
#' @param segment_size (1D bases only) the width thickness of the rectangle bar
#' showing variable magnitude on the axes. Defaults to 2.
#' @export
#' @family ggtour proto
#' @examples
#' ## 1D case:
#' gt_path1d <- tourr::save_history(dat, grand_tour(d = 1), max_bases = 3)
#' 
#' ggt <- ggtour(gt_path1d, dat) +
#'   proto_basis1d()
#' \dontrun{
#' animate_plotly(ggt)
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
  eval(.init4proto)
  
  ## Find the height of density to map_to
  .den <- stats::density(.df_data[, 1L])
  .map_to1d <- data.frame(x = stats::quantile(.df_data[, 1L], probs = c(.01, .99)),
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
#' @param aes_args A list of aesthetic arguments to passed to 
#' `geom_point(aes(X)`. Any mapping of the data to an aesthetic,
#' for example, `geom_point(aes(color = myCol, shape = myCol))` becomes
#' `aes_args = list(color = myCol, shape = myCol)`.
#' @param identity_args A list of static, identity arguments passed into 
#' `geom_point()`, but outside of `aes()`; `geom_point(aes(), X)`.
#' Typically a single numeric for point size, alpha, or similar.
#' @export
#' @aliases proto_points
#' @family ggtour proto
#' @examples
#' dat <- scale_sd(tourr::flea[, 1:6])
#' clas <- tourr::flea$species
#' gt_path <- tourr::save_history(dat, grand_tour(), max_bases = 5)
#' 
#' ggt <- ggtour(gt_path, dat) +
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
  eval(.init4proto)
  if(is.null(.df_data)) stop("Data missing. ggtour() on a manual tour without passing data?")
  if(is.null(.df_data$y)) stop("Projection y not found. Did you apply to a 1D tour?")
  aes_args <- lapply_rep_len(aes_args, .nrow_df_data, .n)
  identity_args <- lapply_rep_len(identity_args, .nrow_df_data, .n)
  .df_data$rownum <- rep_len(1L:.n, .nrow_df_data)
  
  ## do.call aes() over the aes_args
  .aes_func <- function(...)
    ggplot2::aes(x = x, y = y, frame = frame, rownum = rownum, ...) ## rownum for tooltip
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
#' @param fraction (2D case only) how long the origin mark should extend ]
#' relative to the observations. Defaults to .05, 5% of the data space.
#' @export
#' @aliases proto_origin2d
#' @family ggtour proto
#' @examples
#' dat <- scale_sd(tourr::flea[, 1:6])
#' clas <- tourr::flea$species
#' 
#' ## 2D case:
#' gt_path <- tourr::save_history(dat, grand_tour(), max_bases = 5)
#' 
#' ggt <- ggtour(gt_path, dat) +
#'   proto_origin() +
#'   proto_point()
#' \dontrun{
#' animate_plotly(ggt)
#' }
proto_origin <- function(fraction = .05){
  ## Assumptions
  if(is.null(.spinifex_df_data) == TRUE) return()
  position <- "center" ## Assumes data is in the center.
  
  ## Initialize
  eval(.init4proto)
  if(is.null(.df_data)) stop("Data missing. ggtour() on a manual tour without passing data?")
  if(is.null(.df_data$y)) stop("Projection y not found. Did you apply to a 1D tour?")
  
  #### Setup origin, zero mark, 5% on each side.
  .center <- map_relative(data.frame(x = 0L, y = 0L), "center", .map_to)
  .min <- min(min(.map_to[, 1L]), min(.map_to[, 2L]))
  .max <- max(max(.map_to[, 1L]), max(.map_to[, 2L]))
  .tail <- fraction / 2L  * (.max - .min)
  
  .df_origin <- data.frame(x     = c(.center[, 1L] - .tail, .center[, 1L]),
                           x_end = c(.center[, 1L] + .tail, .center[, 1L]),
                           y     = c(.center[, 2L], .center[, 2L] - .tail),
                           y_end = c(.center[, 2L], .center[, 2L] + .tail))
  
  ## Return
  return(
    ggplot2::geom_segment(
      data = .df_origin, color = "grey60", size = 1L, alpha = .7,
      mapping = ggplot2::aes(x = x, y = y, xend = x_end, yend = y_end)
    )
  )
}


#' @rdname proto_origin
#' @export
#' @family ggtour proto
#' @examples
#' ## 1D case:
#' gt_path1d <- tourr::save_history(dat, grand_tour(d = 1), max_bases = 5)
#' 
#' ggt <- ggtour(gt_path, dat) +
#'   proto_origin1d() +
#'   proto_density()
#' \dontrun{
#' animate_plotly(ggt)
#' }
proto_origin1d <- function(){
  ## Assumptions
  if(is.null(.spinifex_df_data) == TRUE) return()
  position <- "center" ## Assumes data is in the center.
  
  ## Initialize
  eval(.init4proto)
  
  #### Setup origin, zero mark, 5% along y axis.
  .den <- stats::density(.df_data[, 1L])
  .map_to1d <- data.frame(x = stats::quantile(.df_data[, 1L], probs = c(.01, .99)),
                          y = 1.8 * range(.den[[2L]]))
  
  .center <- map_relative(data.frame(x = 0L, y = 0L), "center", .map_to1d)
  
  ## Return
  return(
    ggplot2::geom_vline(
      xintercept = .center[, 1L],
      color = "grey60", size = 1L, alpha = .7
    )
  )
}

#' Tour proto for data, 1D density, with rug marks
#'
#' Adds `geom_density()` and `geom_rug()` of the projected data. Density 
#' `postion = "stack"` does not work with `animate_plotly()`, GH issue is open. 
#' 
#' @param aes_args A list of aesthetic arguments to passed to 
#' `geom_point(aes(X)`. Any mapping of the data to an aesthetic,
#' for example, `geom_point(aes(color = myCol, shape = myCol))` becomes
#' `aes_args = list(color = myCol, shape = myCol)`.
#' @param identity_args A list of static, identity arguments passed into 
#' `geom_point()`, but outside of `aes()`; `geom_point(aes(), X)`.
#' Typically a single numeric for point size, alpha, or similar.
#' @param density_position The `ggplot2` position of `geom_density()`. Either 
#' c("identity", "stack"), defaults to "identity". Warning: "stack" does not 
#' work with `animate_plotly()` at the moment.
#' @export
#' @aliases proto_density1d
#' @family ggtour proto
#' @examples
#' dat <- scale_sd(tourr::flea[, 1:6])
#' clas <- tourr::flea$species
#' gt_path <- save_history(dat, grand_tour(), max = 3)
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
  .nms <- names(aes_args)
  if(any(c("color", "colour", "col") %in% .nms) & !("fill" %in% .nms))
    warning("aes_args: color used without fill in, did you mean to use 'fill' with density?")
  
  ## Initialize, replicate list args
  eval(.init4proto)
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
#' the data. Default is NULL, which goes to the rownumber.
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
  eval(.init4proto)
  if(is.null(.df_data)) stop("Data missing. ggtour() on a manual tour without passing data?")
  if(is.null(.df_data$y)) stop("Projection y not found. Did you apply to a 1D tour?")
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
#' diam_sub <- diamonds[sample(nrow(diamonds), 1000), ]
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
  eval(.init4proto)
  if(is.null(.df_data)) stop("Data missing. ggtour() on a manual tour without passing data?")
  if(is.null(.df_data$y)) stop("Projection y not found. Did you apply to a 1D tour?")
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


#' Wrapper function for default 2D/1D tours respectively.
#' 
#' An easier way to get to default 2D tour settings.
#' Returns a list of proto_origin(), proto_point(...), proto_basis() for 2D.
#' Returns a list of proto_origin1d(), proto_density(...), proto_basis1d() for 1D.

#'
#' @param aes_args A list of aesthetic arguments to passed to 
#' `geom_point(aes(X)`. Any mapping of the data to an aesthetic,
#' for example, `geom_point(aes(color = myCol, shape = myCol))` becomes
#' `aes_args = list(color = myCol, shape = myCol)`.
#' @param identity_args A list of static, identity arguments passed into 
#' `geom_point()`, but outside of `aes()`; `geom_point(aes(), X)`.
#' Typically a single numeric for point size, alpha, or similar.
#' @export
#' @aliases proto_default2d, proto_def, proto_def2d
#' @family ggtour proto
#' @examples
#' dat <- scale_sd(tourr::flea[, 1:6])
#' clas <- tourr::flea$species
#' 
#' ## 2D case:
#' bas <- basis_pca(dat)
#' mv <- manip_var_of(bas)
#' mt_path <- manual_tour(bas, mv)
#' 
#' ggt <- ggtour(mt_path, dat) +
#'   proto_default(list(color = clas, shape = clas))
#' \dontrun{
#' animate_plotly(ggt)
#' }
proto_default <- function(aes_args = list(),
                          identity_args = list()
){
  return(list(
    proto_origin(),
    proto_point(aes_args, identity_args),
    proto_basis()
  ))
}


#' @rdname proto_default
#' @export
#' @aliases proto_def1d
#' @family ggtour proto
#' @examples
#' ## 1D case:
#' gt_path <- tourr::save_history(dat, grand_tour(d = 1), max_bases = 3)
#' 
#' ggt <- ggtour(gt_path, dat) +
#'   proto_default1d(list(fill = clas))
#' \dontrun{
#' animate_plotly(ggt)
#' }
proto_default1d <- function(aes_args = list(),
                            identity_args = list()
){
  return(list(
    proto_origin1d(),
    proto_density(aes_args, identity_args),
    proto_basis1d()
  ))
}
