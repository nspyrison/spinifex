### UTIL -----
#' Prepare a new grammar of graphics tour
#'
#' `ggtour()` initializes a ggplot object for a tour. `proto_*` functions are
#' added to the tour, analogous to `ggplot() + geom_*`. The final tour object is
#' then animated with `animate_plotly()` or `animate_ggtour()`, or passed to
#' `filmstrip()` for static plot faceting on frames.
#'
#' @param basis_array An array of projection bases for the tour, as produced
#' with `manual_tour()` or `tour::save_history()`.
#' @param data Numeric data to project. If left NULL, will check if it data is 
#' stored as an attribute of the the `basis_array`.
#' @param angle Target angle (in radians) for interpolation for
#' `tour::save_history()` generated `basis_array`. Defaults to .05.
#' @param facet_by Optionally, add a vector to facet the ggtour on. Similar to
#' adding `facet_grid(rows = facet_by)` to the tour.
#' @export
#' @family ggtour proto
#' @examples
#' dat <- scale_sd(tourr::flea[, 1:6])
#' clas <- tourr::flea$species
#' bas <- basis_pca(dat)
#' mv <- manip_var_of(bas)
#' mt_path <- manual_tour(bas, manip_var = mv, angle = .1)
#' 
#' ## Returns headless ggplot(), but required for proto_* functions.
#' ggtour(mt_path, dat)
#' 
#' ## d = 2 case
#' ggt <- ggtour(mt_path, dat) +
#'   proto_basis() +
#'   proto_point(list(color = clas, shape = clas),
#'               list(size = 1.5))
#' \dontrun{
#' animate_plotly(ggt)
#' }
#' 
#' ## d = 1 case
#' bas1d <- basis_pca(dat, d = 1)
#' mt_path1d <- manual_tour(basis = bas1d, manip_var = mv, angle = .2)
#' ggt1d <- ggtour(mt_path1d, dat) +
#'   proto_default1d(list(fill = clas))
#' \dontrun{
#' animate_plotly(ggt1d)
#' }
#' 
#' ## d = 2, with facet
#' ggt <- ggtour(mt_path, dat, facet_by = clas) +
#'   proto_default(list(color = clas, shape = clas), list(size = 1.5))
#' \dontrun{
#' animate_plotly(ggt)
#' }
#' 
#' ## d = 1, with facet
#' ggt1d <- ggtour(mt_path1d, dat, facet_by = clas) +
#'   proto_default1d(list(color = clas, shape = clas, fill = clas))
#' \dontrun{
#' ## faceted 1d doesn't work the best with plotly; esp rug, and basis segments.
#' animate_gganimate(ggt1d)
#' }
ggtour <- function(basis_array,
                   data = NULL,
                   angle = .05,
                   facet_by = NULL
){
  if(is.null(data) == TRUE) data <- attr(basis_array, "data") ## Could be NULL
  manip_var <- attr(basis_array, "manip_var") ## NULL if not a manual tour
  if(any(class(basis_array) %in% c("matrix", "data.frame"))) ## Format for array2df (_list)
    basis_array <- array(as.matrix(basis_array), dim = c(dim(basis_array), 1L))
  
  ## Interpolate {tourr} tours
  if(is.null(manip_var) == TRUE) ## if manip_var is null; IE. from tourr
    if(dim(basis_array)[3L] != 1L) ## AND more than 1 basis.
      .m <- utils::capture.output(
        basis_array <- tourr::interpolate(basis_array, angle))
  ## Interpolate manual tours
  if(is.null(manip_var) == TRUE)
    basis_array <- interpolate_manual_tour(basis_array, angle)
  
  df_ls <- array2df(basis_array, data)
  df_basis <- df_ls$basis_frames
  df_data  <- df_ls$data_frames
  attr(df_basis, "manip_var") <- manip_var ## NULL if not a manual tour
  
  ## map_to handling cases::
  #### If data is NULL then map_to unit box
  map_to <- data.frame(x = c(-1L, 1L), y = c(-1L, 1L))
  if(is.null(data) == FALSE){ 
    d <- dim(basis_array)[2L]
    #### If data 2d then map_to data
    if(d == 2L)
      map_to <- data.frame(x = range(df_data[, 1L]), y = range(df_data[, 2L]))
    #### If data 1d then map_to X of data, and [01] (scaled density)
    if(d == 1L) ## 1D non-NULL basis
      map_to <- data.frame(x = range(df_data[, 1L]), y = c(0L, 1L))
  }
  
  nrow_df_data <- nrow(df_data)

  ## Append facet_by to df_basis and df_data if needed.
  if(is.null(facet_by) == FALSE){
    df_data <- .bind_elements2df(
      list(facet_by = rep_len(facet_by, nrow_df_data)), df_data)
    df_basis <- .bind_elements2df( ## basis facet always on first/top level
      list(facet_by = rep_len("_basis_", nrow(df_basis))), df_basis)
  }
  ## Assign list to last_ggtour().
  n_frames <- length(unique(df_basis$frame))
  p <- nrow(df_basis) / n_frames
  n <- nrow_df_data   / n_frames
  .set_last_ggtour(list(df_basis = df_basis,
                        df_data = df_data,
                        map_to = map_to,
                        n_frames = n_frames,
                        nrow_df_data = nrow_df_data,
                        n = n,
                        p = p,
                        manip_var = manip_var,
                        facet_by = facet_by))
  
  ## Return ggplot head, theme, and facet if used
  ## by product: last_ggtour() was set above.
  ret <- ggplot2::ggplot(df_basis) + spinifex::theme_spinifex()
  if(is.null(facet_by) == FALSE){
    ret <- ret +
      ggplot2::facet_grid(rows = ggplot2::vars(facet_by)) +
      ggplot2::theme(strip.text = ggplot2::element_text(
        margin = ggplot2::margin(b = 0L, t = 0L)),  ## tighter facet labels
        panel.spacing = ggplot2::unit(0L, "lines")) ## tighter facet spacing
  }
  return(ret)
}
# ## Print method ->> proto_default()
# #### Was a good idea, but ggplot stops working when you change the first class, 
# #### and doesn't effect if you append.
# print.ggtour <- function(x, ...){
#   class(x) <- c("gg", "ggplot")
#   x +
#     proto_basis() +
#     proto_origin(gridline_probs = FALSE) +
#     proto_point()
# }


.store <- new.env(parent = emptyenv())
#' Retrieve/set a list from the last `ggtour()`, required for the use 
#' of `proto_*` functions.
#'
#' @seealso [ggtour()]
#' @export
#' @keywords internal
last_ggtour <- function(){.store$ggtour_ls}
#' @rdname last_ggtour
#' @export
.set_last_ggtour <- function(ggtour_list) .store$ggtour_ls <- ggtour_list




#' Replicate all vector elements of a list
#' 
#' Internal function. To be applied to `aes_args` and `identity_args`, 
#' replicates vectors of length data to length of data*frames for animation.
#'
#' @param list A list of arguments such as those passed in `aes_args` and 
#' `identity_args`.
#' @param to_length Scalar number, length of the output vector;
#' the number of rows in the data frames to replicate to.
#' @param expected_length Scalar number, the expected length of the each element 
#' of `list`.
#' @family Internal utility
#' @examples
#' ## This function is not meant for external use
.lapply_rep_len <- function(list,
                            to_length,
                            expected_length
){
  list <- as.list(list)
  .nms <- names(list)
  .m <- lapply(seq_along(list), function(i){
    .this_vector <- list[[i]]
    if(length(.this_vector) != 1L & typeof(.this_vector) != "environment"){
      if(length(.this_vector) != expected_length)
        warning(paste0(".lapply_rep_len: `", .nms[i], "` not of length 1 or data."))
      ret_vect <- rep_len(.this_vector, to_length)
    }else ret_vect <- .this_vector
    list[[i]] <<- ret_vect
  })
  return(list)
}

#' Binds replicated elements of a list as columns of a data frame.
#' 
#' Internal function. To be applied to `aes_args` 
#' replicates elements to length data
#'
#' @param list A list of arguments such as those passed in `aes_args` and 
#' `identity_args`.
#' @param df A data.frame to column bind the elements of `list` to.
#' @family Internal utility
#' @examples
#' ## This function is not meant for external use
.bind_elements2df <- function(list, df){
  .list <- as.list(list)
  ret <- as.data.frame(df)
  .ret_nms <- names(ret)
  .l_nms <- names(list)
  .m <- lapply(seq_along(.list), function(i){
    ret <<- cbind(ret, .list[[i]])
  })
  names(ret) <- c(.ret_nms, .l_nms)
  return(ret)
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
## _.init4proto expression -----
.init4proto <- expression({ ## expression, not function
  .ggt <- last_ggtour()
  if(is.null(.ggt)) stop("last_ggtour() is NULL, have you run ggtour() yet?")
  
  ## Assign elements of last_ggtour() into the scope of a ggproto func.
  .df_basis     <- .ggt$df_basis ## Give operable local copies
  .df_data      <- .ggt$df_data
  .map_to       <- .ggt$map_to
  .n_frames     <- .ggt$n_frames
  .nrow_df_data <- .ggt$nrow_df_data
  .n            <- .ggt$n
  .p            <- .ggt$p
  .manip_var    <- .ggt$manip_var ## Can be NULL; a tourr tour.
  .facet_by     <- .ggt$facet_by  ## Usually NULL; no facet used.
  
  ## subset, if rownum_index exists
  if(exists("rownum_index")){
    if(is.null(rownum_index) == FALSE){
      ## subset arg_lists
      if(exists("aes_args"))
        if(length(aes_args) > 0L)
          aes_args <- lapply(aes_args, function(arg)arg[rownum_index])
      if(exists("identity_args"))
        if(length(aes_args) > 0L)
          identity_args <- lapply(identity_args, function(arg)
            if(length(arg) == .n) arg[rownum_index] else arg)
      ## Subset .df_data
      .idx <- NULL## NEED SOME MOD MATH:
      .m <- sapply(1L:.n_frames - 1L, function(i){
        .idx <<- c(.idx, rownum_index + i * .n)
      })
      .n <- length(rownum_index)
      .df_data <- .df_data[.idx, ]
      .nrow_df_data <- nrow(.df_data)
    }
  }
  ## Replicate arg, if they exist
  if(exists("aes_args")){
    if(length(aes_args) > 0L){
      aes_args <- .lapply_rep_len(aes_args, .nrow_df_data, .n)
      ## binding aes_args to .df_data, but then need to find another method to replace do.call.
      # .df_data <- .bind_elements2df(aes_args, .df_data)
      # aes_args <- ## TODO>>>>, go to aes_string("mpg") or aes_(quote(mpg))?
    }
  }
  if(exists("identity_args")){
    if(length(identity_args) > 0L)
      identity_args <- .lapply_rep_len(identity_args, .nrow_df_data, .n)
  }
  .m <- gc() ## Mute garbage collection
})

### ANIMATE_* ------

#' Animate a ggtour as a .gif via `{gganimate}`
#'
#' Animates the ggplot return of `ggtour()` and added `proto_*()` functions as a 
#' .gif without interaction, through use of `{gganimate}`.
#'
#' @param ggtour A grammar of graphics tour with appended protos added. 
#' A return from `ggtour() + proto_*()`.
#' @param fps Scalar number of Frames Per Second, the speed the animation should 
#' play at.
#' @param rewind Whether or not the animation should play backwards,
#' in reverse order once reaching the end. Defaults to FALSE.
#' @param start_pause The duration in seconds to wait before starting the 
#' animation. Defaults to 1 second.
#' @param end_pause The duration in seconds to wait after ending the animation,
#' before it restarts from the first frame. Defaults to 1 second.
#' @param ... Other arguments passed to 
#' \code{\link[gganimate:animate]{gganimate::animate}}.
#' @seealso \code{\link[gganimate:animate]{gganimate::animate}}
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
#' animate_gganimate(ggt)
#' 
#' (anim <- animate_gganimate(ggt, fps = 10, rewind = TRUE,
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
  ... ## Passed to gganimate::animate
){
  ## Assumptions
  requireNamespace("gifski")
  requireNamespace("png")
  ## Early out, print ggplot if oonly 1 frame. 
  if(length(ggtour$layers) == 0L) stop("No layers found, did you forget to add a proto_*?")
  n_frames <- length(unique(last_ggtour()$df_basis$frame))
  if(n_frames == 1L){
    warning("ggtour df_basis only has 1 frame, printing ggtour ggplot2 object instead.")
    return(print(ggtour))
  }
  
  ## Discrete jump between frames, no linear interpolation.
  gga <- ggtour + gganimate::transition_states(
    frame, transition_length = 0L)
  ## Normal animation, with applied options, knit_pdf_anim == FALSE
  anim <- gganimate::animate(
    gga, fps = fps, rewind = rewind,
    start_pause = fps * start_pause,
    end_pause = fps * end_pause, ...)
  
  ## Clean up
  .set_last_ggtour(NULL) ## Clears last tour
  ## this should prevent some errors from not running ggtour() right before animating it.
  .m <- gc() ## Mute garbage collection
  
  return(anim)
}


#' Animate a ggtour as and HTML widget via `{plotly}`
#'
#' Animates the static `ggtour()` and added `proto_*()` functions as a 
#' `{plotly}` animation, an .html widget with slider and hover tooltip showing 
#' the row number.
#'
#' @param ggtour A grammar of graphics tour with appended protos added. 
#' A return from `ggtour() + proto_*()`.
#' @param fps Scalar number of Frames Per Second, the speed the animation should 
#' play at.
#' @param ... Other arguments passed to 
#' \code{\link[plotly:ggplotly]{plotly::ggplotly}}.
#' @seealso \code{\link[plotly:ggplotly]{plotly::ggplotly}}
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
animate_plotly <- function(
  ggtour,
  fps = 8,
  ... ## Passed to plotly::layout().
){
  ## Assumptions
  if(length(ggtour$layers) == 0L) stop("No layers found, did you forget to add a proto_*?")
  n_frames <- length(unique(last_ggtour()$df_basis$frame))
  if(n_frames == 1L){
    warning("ggtour df_basis only has 1 frame, applying just plotly::ggplotly instead.")
    return(plotly::ggplotly(p = ggtour, tooltip = "tooltip"))
  }
  
  ## Block plotly.js warning: lack of support for horizontal legend;
  #### https://github.com/plotly/plotly.js/issues/53
  anim <- suppressWarnings(
    plotly::ggplotly(p = ggtour, tooltip = "tooltip") %>%
      plotly::animation_opts(frame = 1L / fps * 1000L,
                             transition = 0L, redraw = FALSE) %>%
      plotly::layout(showlegend = FALSE,
                     #, fixedrange = TRUE ## This is a curse, do not use.
                     yaxis = list(showgrid = FALSE, showline = FALSE),
                     xaxis = list(showgrid = FALSE, showline = FALSE,
                                  scaleanchor = "y", scalaratio = 1L),
                     ...) %>%
      plotly::config(displayModeBar = FALSE))
  
  
  ## Clean up
  .set_last_ggtour(NULL) ## Clears last tour
  ## this should prevent some errors from not running ggtour() right before animating it.
  .m <- gc() ## Mute garbage collection
  
  return(anim)
}


# ### animate_gganimate_knit2pdf -----#
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
# #' mt_path <- manual_tour(bas, manip_var = mv, angle = .1)
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
#   n_frames <- length(unique(last_ggtour()$df_basis$frame))
#   if(n_frames == 1L) stop("df_basis only has 1 frame, stopping animation.")
#   
#   ## Discrete jump between frames, no linear interpolation.
#   gga <- ggtour + gganimate::transition_states(frame, transition_length = 0L)
#   
#   ## Return
#   gganimate::knit_print.gganim(gga, ...)
# }


#' Create a "filmstrip" of the frames of a ggtour.
#'
#' Appends `facet_wrap(vars(frame_number))` & minor themes to the ggtour. If the
#' number of frames is more than desired, try increasing the `angle` argument on
#' the tour.
#' 
#' @param ggtour A grammar of graphics tour with appended protos added. 
#' A return from `ggtour() + proto_*()`.
#' @export
#' @family ggtour proto
#' @examples
#' dat <- scale_sd(tourr::flea[, 1:6])
#' clas <- tourr::flea$species
#' bas <- basis_pca(dat)
#' mv <- manip_var_of(bas)
#' 
#' ## d = 2 case
#' mt_path <- manual_tour(bas, manip_var = mv, angle = .25)
#' ggt <- ggtour(mt_path, dat) +
#'   proto_basis() +
#'   proto_point(list(color = clas, shape = clas),
#'               list(size = 1.5))
#' filmstrip(ggt)
#' 
#' ## d = 1 case
#' bas1d <- basis_pca(dat, d = 1)
#' mt_path1d <- manual_tour(basis = bas1d, manip_var = mv, angle = .3)
#' ggt1d <- ggtour(mt_path1d, dat) +
#'   proto_default1d(list(fill = clas))
#' 
#' ggt1d <- ggtour(mt_path1d, dat) +
#'   proto_default1d(list(fill = clas))
#' filmstrip(ggt1d)
filmstrip <- function(ggtour){
  ret <- ggtour +
    ggplot2::facet_wrap(ggplot2::vars(frame)) + ## facet on frame
    ggplot2::theme(strip.text = ggplot2::element_text(
      margin = ggplot2::margin(b = 0L, t = 0L)),  ## tighter facet labels
      panel.spacing = ggplot2::unit(0L, "lines")) ## tighter facet spacing
  
  ## Remove last_ggtour?
  #.set_last_ggtour(NULL) ## Clears last tour
  .m <- gc() ## Mute garbage collection
  
  return(ret)
}


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
#' dat <- scale_sd(tourr::flea[, 1:6])
#' clas <- tourr::flea$species
#' bas <- basis_pca(dat)
#' mv <- manip_var_of(bas)
#' 
#' ## 2D case:
#' mt_path <- manual_tour(bas, manip_var = mv, angle = .2)
#' 
#' ggt <- ggtour(mt_path, dat) +
#'   proto_basis()
#' \dontrun{
#' animate_plotly(ggt)
#' }
#' 
#' ## Customize basis
#' ggt2 <- ggtour(mt_path, dat) +
#'   proto_basis(position = "right", manip_col = "green",
#'               line_size = .8, text_size = 8)
#' \dontrun{
#' animate_plotly(ggt2)
#' }
#' 
#' ## 1D case:
#' bas1d <- basis_pca(dat, d = 1)
#' mv <- manip_var_of(bas, 3)
#' mt_path1d <- manual_tour(bas1d, manip_var = mv, angle = .2)
#' 
#' ggt1d <- ggtour(mt_path1d, dat) +
#'   proto_basis1d()
#' \dontrun{
#' animate_plotly(ggt1d)
#' }
proto_basis <- function(
  position = c("left", "center", "right", "bottomleft", "topright", "off"),
  manip_col = "blue",
  line_size = 1,
  text_size = 5
){
  ## Initialize
  eval(.init4proto)
  if(is.null(.df_basis$y))
    stop("proto_basis: Basis `y` not found, expected a 2D tour. Did you mean to call `proto_basis1d`?")
  position = match.arg(position)
  if(position == "off") return()
  if(is.null(.facet_by) == FALSE) position = "center"
  
  ## Setup and transform
  .angles <- seq(0L, 2L * pi, length = 360L)
  .circle <- data.frame(x = cos(.angles), y = sin(.angles))
  .center <- map_relative(data.frame(x = 0L, y = 0L), position, .map_to)
  .circle <- map_relative(.circle, position, .map_to)
  .df_basis <- map_relative(.df_basis, position, .map_to)
  if(is.null(.facet_by) == FALSE)
    .circle <- .bind_elements2df(
      list(facet_by = rep_len("_basis_", nrow(.circle))), .circle)
  
  ## Aesthetics for the axes segments.
  .axes_col <- "grey50"
  .axes_siz <- line_size
  if(is.null(.manip_var) == FALSE) {
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
        size = .axes_siz, color = .axes_col,
        mapping = ggplot2::aes(x = x, y = y, frame = frame,
                               xend = .center[, 1L], yend = .center[, 2L])
      )),
      suppressWarnings(ggplot2::geom_text(
        data = .df_basis,
        color = .axes_col, size = text_size,
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
proto_basis1d <- function(
  position = c("top1d", "floor1d", "off"),
  manip_col = "blue",
  segment_size = 2,
  text_size = 5
){
  ## Initialize
  eval(.init4proto)
  position = match.arg(position)
  if(position == "off") return()
  if(is.null(.facet_by) == FALSE) position = "floor1d"
  
  ## Aesthetics for the axes segments
  .axes_col <- .text_col <- "grey50"
  .axes_siz <- segment_size
  if(is.null(.manip_var) == FALSE){
    .axes_col <- rep("grey50", .p)
    .axes_col[.manip_var] <- manip_col
    .text_col <- .axes_col
    .axes_col <- rep(.axes_col, .n_frames)
    .axes_siz <- rep(segment_size, .p)
    .axes_siz[.manip_var] <- 1.5 * segment_size
    .axes_siz <- rep(.axes_siz, .n_frames)
  }
  
  ## Initialize data.frames, before scaling
  .df_zero <- data.frame(x = 0L, y = 0L)
  .df_seg  <- data.frame(x = .df_basis$x,
                         y = rep(.p:1L, .n_frames),
                         frame = .df_basis$frame,
                         label = .df_basis$label)
  ## Do note replicate across frames, it won't work any better with plotly
  .df_txt  <- data.frame(x = -1L,
                         y = .p:1L,
                         label = .df_basis[.df_basis$frame == 1L, "label"])
  .df_rect <- data.frame(x = c(-1L, 1L), y = c(.5, .p + .5))
  .df_seg0 <- data.frame(x = 0L, y = c(.5, .p + .5))
  ## Scale them
  .df_zero <- map_relative(.df_zero, position, .map_to)
  .df_seg  <- map_relative(.df_seg,  position, .map_to)
  .df_txt  <- map_relative(.df_txt,  position, .map_to)
  .df_rect <- map_relative(.df_rect, position, .map_to)
  .df_seg0 <- map_relative(.df_seg0, position, .map_to)
  if(is.null(.facet_by) == FALSE){
    .basis_ls <- list(facet_by = rep_len("_basis_", nrow(.df_zero)))
    .df_zero <- .bind_elements2df(.basis_ls, .df_zero)
    .df_seg  <- .bind_elements2df(.basis_ls, .df_seg)
    .df_txt  <- .bind_elements2df(.basis_ls, .df_txt)
    .df_rect <- .bind_elements2df(.basis_ls, .df_rect)
    .df_seg0 <- .bind_elements2df(.basis_ls, .df_seg0)
  }
  
  ## Return proto
  return(list(
    ## Middle line, grey, dashed
    ggplot2::geom_segment(
      ggplot2::aes(x = min(x), y = min(y), xend = max(x), yend = max(y)),
      .df_seg0, color = "grey80", linetype = 2L),
    ## Outside rectangle, Grey, unit-width, (height = p+1)
    ggplot2::geom_rect(
      ggplot2::aes(xmin = min(x), xmax = max(x), ymin = min(y), ymax = max(y)),
      .df_rect, fill = NA, color = "grey60"),
    ## Variable abbreviation text
    ggplot2::geom_text(
      ggplot2::aes(x, y, label = label), .df_txt,
      hjust = 1L, size = text_size, color = "grey60",
      nudge_x = -.08 * max(nchar(.df_txt$label))),
    ## Contribution segments of current basis, changing with frame
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
#' `geom_point()`, but outside of `aes()`, for instance 
#' `geom_point(aes(...), size = 2, alpha = .7)` becomes 
#' `identity_args = list(size = 2, alpha = .7)`.
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
#' ggt2 <- ggtour(gt_path, dat) +
#'   proto_point(list(color = clas, shape = clas),
#'               list(size = 2, alpha = .7))
#' \dontrun{
#' animate_plotly(ggt2)
#' }
proto_point <- function(aes_args = list(),
                        identity_args = list()
){
  ## Initialize
  eval(.init4proto)
  if(is.null(.df_data) == TRUE)
    stop("proto_point: Data is NULL. Did you call ggtour() on a manual tour without passing data?")
  if(is.null(.df_data$y))
    stop("proto_point: Projection y not found, expected a 2D tour.")
  
  ## do.call aes() over the aes_args
  .aes_func <- function(...)
    ggplot2::aes(x = x, y = y, frame = frame, tooltip = label, ...) ## tooltip for plotly on hover tip
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
#' @param tail_size How long the origin mark should extended
#' relative to the observations. Defaults to .05, 5% of the projection space.
#' @param identity_args A list of static, identity arguments passed into 
#' `geom_point()`, but outside of `aes()`, for instance 
#' `geom_point(aes(...), size = 2, alpha = .7)` becomes 
#' `identity_args = list(size = 2, alpha = .7)`.
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
proto_origin <- function(
  identity_args = list(color = "grey60", size = .5, alpha = .9),
  tail_size = .05
){
  ## Initialize
  eval(.init4proto)
  if(is.null(.df_data) == TRUE) stop("Data is NULL, proto not applicable.")
  if(is.null(.df_basis$y))
    stop("proto_origin: Basis y not found, expects a 2D tour. Did you mean to call `proto_origin1d`?")
  
  #### Setup origin, zero mark, 5% on each side.
  .center <- map_relative(data.frame(x = 0L, y = 0L), "center", .map_to)
  .min    <- min(min(.map_to[, 1L]), min(.map_to[, 2L]))
  .max    <- max(max(.map_to[, 1L]), max(.map_to[, 2L]))
  .tail   <- tail_size / 2L * (.max - .min)
  .df_origin <- data.frame(x     = c(.center[, 1L] - .tail, .center[, 1L]),
                           x_end = c(.center[, 1L] + .tail, .center[, 1L]),
                           y     = c(.center[, 2L], .center[, 2L] - .tail),
                           y_end = c(.center[, 2L], .center[, 2L] + .tail))
  
  ## do.call geom_point() over the identity_args
  .geom_func <- function(...)
    ggplot2::geom_segment(
      ggplot2::aes(x = x, y = y, xend = x_end, yend = y_end),
      data = .df_origin, ...)
  ## Return
  return(do.call(.geom_func, identity_args))
}


#' @rdname proto_origin
#' @export
#' @family ggtour proto
#' @examples
#' ## 1D case:
#' gt_path1d <- tourr::save_history(dat, grand_tour(d = 1), max_bases = 5)
#' 
#' ggt <- ggtour(gt_path1d, dat) +
#'   proto_origin1d() +
#'   proto_density(list(fill = clas, color = clas))
#' \dontrun{
#' animate_plotly(ggt)
#' }
proto_origin1d <- function(
  identity_args = list(color = "grey60", size = .5, alpha = .9)
){
  ## Initialize
  eval(.init4proto)
  if(is.null(last_ggtour()$df_data) == TRUE) return()
  .center <- map_relative(data.frame(x = 0L, y = 0L), "center", .map_to)
  .ymin <- min(.map_to[, 2L])
  .ymax <- max(.map_to[, 2L])
  .tail <- diff(c(.ymin, .ymax)) * .6
  .df_origin <- data.frame(x     = c(.center[, 1L], .center[, 1L]),
                           x_end = c(.center[, 1L], .center[, 1L]),
                           y     = c(.center[, 2L], .center[, 2L]),
                           y_end = c(.center[, 2L] - .tail, .center[, 2L] + .tail))
  
  ## do.call geom_point() over the identity_args
  .geom_func <- function(...)
    ggplot2::geom_segment(
      ggplot2::aes(x = x, y = y, xend = x_end, yend = y_end),
      data = .df_origin, ...)
  ## Return
  return(do.call(.geom_func, identity_args))
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
#' `geom_point()`, but outside of `aes()`, for instance 
#' `geom_point(aes(...), size = 2, alpha = .7)` becomes 
#' `identity_args = list(size = 2, alpha = .7)`.
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
#' \dontrun{
#' animate_plotly(ggt)
#' }
proto_density <- function(aes_args = list(),
                          identity_args = list(alpha = .7),
                          density_position = c("identity", "stack", "fill")
){
  ## Initialize
  eval(.init4proto)
  if(class(transformr::tween_polygon) != "function")
    stop("proto_density requires {transformr}::tween_polygon")
  if(is.null(.df_data))
    stop("proto_density: data missing. Did you call ggtour() on a manual tour without passing data?")
  density_position <- match.arg(density_position)
  
  ## "identity" is the only position working in {plotly} right now.
  ## see: https://github.com/ropensci/plotly/issues/1544
  .nms <- names(aes_args)
  if(any(c("color", "colour", "col") %in% .nms) & !("fill" %in% .nms))
    warning("proto_density: aes_args contains color without fill, did you mean to use fill to color below the curve?")
  
  ## geom_density do.call
  .aes_func <- function(...)
    ggplot2::aes(x = x, y = ..scaled.., frame = frame, ...)
  .aes_call <- do.call(.aes_func, aes_args)
  .geom_func <- function(...) suppressWarnings(
    ggplot2::geom_density(mapping = .aes_call, data = .df_data, ...,
                          position = density_position, color = "black", n = 256L))
  .geom_call_den <- do.call(.geom_func, identity_args)
  ## geom_rug do.call
  .aes_func <- function(...)
    ggplot2::aes(x = x, frame = frame, ...)
  .aes_call <- do.call(.aes_func, aes_args)
  .geom_func <- function(...) suppressWarnings(
    ggplot2::geom_rug(mapping = .aes_call, data = .df_data,
                      length = ggplot2::unit(0.08, "npc"), ...))
  .geom_call_rug <- do.call(.geom_func, identity_args)
  
  ## Return proto
  return(list(.geom_call_den, .geom_call_rug))
}


#' Tour proto for data, text labels
#'
#' Adds `geom_text()` of the projected data.
#'
#' @param aes_args A list of aesthetic arguments to passed to 
#' `geom_point(aes(X)`. Any mapping of the data to an aesthetic,
#' for example, `geom_point(aes(color = myCol, shape = myCol))` becomes
#' `aes_args = list(color = myCol, shape = myCol)`.
#' @param identity_args A list of static, identity arguments passed into 
#' `geom_point()`, but outside of `aes()`, for instance 
#' `geom_point(aes(...), size = 2, alpha = .7)` becomes 
#' `identity_args = list(size = 2, alpha = .7)`.
#' @param rownum_index One or more integers, the row numbers of the to 
#' subset to. Should be within 1:n, the rows of the original data. Defaults to
#' NULL, labeling all rows.
#' @export
#' @family ggtour proto
#' @examples
#' dat <- scale_sd(tourr::flea[, 1:6])
#' clas <- tourr::flea$species
#' bas <- basis_pca(dat)
#' mv <- manip_var_of(bas)
#' gt_path <- save_history(dat, tourr::grand_tour(), max_bases = 5)
#' 
#' ggt <- ggtour(gt_path, dat, angle = .2) +
#'   proto_text(list(color = clas))
#' \dontrun{
#' animate_plotly(ggt)
#' }
#' 
#' ## Custom labels, subset of points
#' ggt2 <- ggtour(gt_path, dat) +
#'   proto_text(list(color = clas, size = as.integer(clas)),
#'              list(alpha = .7),
#'              rownum_index = 1:15)
#' \dontrun{
#' animate_plotly(ggt2)
#' }
proto_text <- function(aes_args = list(),
                       identity_args = list(nudge_x = 0.05),
                       rownum_index = NULL
){
  ## Initialize
  eval(.init4proto)
  if(is.null(.df_data) == TRUE) stop("Data is NULL, proto not applicable.")
  if(is.null(.df_data$y))
    stop("proto_text: Projection y not found, expected a 2D tour.")
  
  ## do.call aes() over the aes_args
  .aes_func  <- function(...)
    ggplot2::aes(x = x, y = y, frame = frame, label = label, ...)
  .aes_call  <- do.call(.aes_func, aes_args)
  ## do.call geom_point() over the identity_args 
  .geom_func <- function(...)suppressWarnings(
    ggplot2::geom_text(mapping = .aes_call, data = .df_data, ...))
  
  ## Return proto
  return(do.call(.geom_func, identity_args))
}

#' Tour proto for data, hexagonal heatmap
#'
#' Adds `geom_hex()` of the projected data. Does not display hexagons in plotly
#' animations; will not work with `animate_plotly()`.
#'
#' @param bins Numeric vector giving number of bins in both vertical and 
#' horizontal directions. Defaults to 30.
#' @param aes_args A list of aesthetic arguments to passed to 
#' `geom_point(aes(X)`. Any mapping of the data to an aesthetic,
#' for example, `geom_point(aes(color = myCol, shape = myCol))` becomes
#' `aes_args = list(color = myCol, shape = myCol)`.
#' @param identity_args A list of static, identity arguments passed into 
#' `geom_point()`, but outside of `aes()`, for instance 
#' `geom_point(aes(...), size = 2, alpha = .7)` becomes 
#' `identity_args = list(size = 2, alpha = .7)`.
#' @export
#' @family ggtour proto
#' @examples
#' raw <- ggplot2::diamonds
#' dat <- scale_sd(raw[1:10000, c(1, 5:6, 8:10)])
#' gt_path <- save_history(dat, grand_tour(), max = 3)
#' 
#' ## 10000 rows is quite heavy to animate.
#' ## Improve performance with aggregation similar to proto_hex()!
#' ggp <- ggtour(gt_path, dat) +
#'   proto_basis() +
#'   proto_hex(bins = 20)
#' 
#' ## Hexagons don't show up in plotly animation.
#' \dontrun{
#' animate_gganimate(ggp)
#' }
proto_hex <- function(aes_args = list(),
                      identity_args = list(),
                      bins = 30
){
  ## Initialize
  requireNamespace("hexbin")
  eval(.init4proto)
  if(is.null(.df_data)) 
    stop("proto_hex: Data is missing. Did you call ggtour() on a manual tour without passing data?")
  if(is.null(.df_basis$y)) 
    stop("proto_hex: Basis `y` not found, expected a 2D tour.")
  
  ## do.call aes() over the aes_args
  .aes_func  <- function(...)
    ggplot2::aes(x = x, y = y, frame = frame, group = frame, ...)
  .aes_call  <- do.call(.aes_func, aes_args)
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
#' `geom_point()`, but outside of `aes()`, for instance 
#' `geom_point(aes(...), size = 2, alpha = .7)` becomes 
#' `identity_args = list(size = 2, alpha = .7)`.
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
                          identity_args = list(alpha = .9)
){
  return(list(
    proto_point(aes_args, identity_args),
    proto_origin(),
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
#'   proto_default1d(list(fill = clas, color = clas))
#' \dontrun{
#' animate_plotly(ggt)
#' }
proto_default1d <- function(aes_args = list(),
                            identity_args = list(alpha = .7)
){
  return(list(
    proto_density(aes_args, identity_args),
    proto_origin1d(),
    proto_basis1d()
  ))
}




## ggproto api extension 1 -----
### should move into spinifex v0.3.1
#' Tour proto highlighing specified points
#'
#' A `geom_point` or `geom_segment`(*1d) call to draw attention to a subset of 
#' points. Subset the projected data frames to the specified `rownum_index` of 
#' the original data.frame with specified highlighting aesthetics. The order you
#' apply highlighting is import when using with other `proto_*` functions.
#'
#' @param rownum_index One or more integers, the row numbers of the to 
#' highlight. Should be within 1:n, the rows of the original data.
#' @param aes_args A list of aesthetic arguments to passed to 
#' `geom_point(aes(X)`. Any mapping of the data to an aesthetic,
#' for example, `geom_point(aes(color = myCol, shape = myCol))` becomes
#' `aes_args = list(color = myCol, shape = myCol)`.
#' @param identity_args A list of static, identity arguments passed into 
#' `geom_point()`, but outside of `aes()`, for instance 
#' `geom_point(aes(...), size = 2, alpha = .7)` becomes 
#' `identity_args = list(size = 2, alpha = .7)`.
#' Typically a single numeric for point size, alpha, or similar.
#' @param mark_initial Logical, whether or not to leave a fainter mark at the 
#' subset's initial position. By default: TRUE if single row number given else 
#' FALSE.
#' @export
#' @aliases proto_highlight_2d
#' @family ggtour proto
#' @examples
#' dat <- scale_sd(tourr::flea[, 1:6])
#' clas <- tourr::flea$species
#' gt_path <- tourr::save_history(dat, grand_tour(), max_bases = 5)
#' 
#' #' ## d = 2 case, Highlighting 1 obs defaults mark_initial to TRUE.
#' ggt <- ggtour(gt_path, dat) +
#'   proto_highlight(rownum_index = 5) +
#'   proto_point()
#' \dontrun{
#' animate_plotly(ggt)
#' }
#' 
#' ## Custom aesthetics, highlighting multiple points defaults mark_initial to FALSE
#' ggt2 <- ggtour(gt_path, dat) +
#'   proto_highlight(rownum_index = c( 2, 6, 19),
#'                   identity_args = list(color = "blue", size = 4, shape = 2)) +
#'   proto_point(list(color = clas, shape = clas),
#'               list(size = 2, alpha = .7))
#' \dontrun{
#' animate_plotly(ggt2)
#' }
#' 
#' ## 
#' ggt2 <- ggtour(gt_path, dat) +
#'   proto_highlight(rownum_index = c( 2, 6, 19),
#'                   aes_args = list(color = clas)) +
#'   proto_point(list(color = clas, shape = clas),
#'               list(size = 2, alpha = .7))
#' \dontrun{
#' animate_plotly(ggt2)
#' }
proto_highlight <- function(
  rownum_index,
  aes_args = list(),
  identity_args = list(color = "red", size = 5, shape = 8),
  mark_initial = if(length(rownum_index) == 1) TRUE else FALSE
){
  ## Initialize
  eval(.init4proto)
  if(is.null(.df_data) == TRUE) stop("Data is NULL, proto not applicable.")
  if(is.null(.df_data$y))
    stop("proto_highlight: Projection y not found, expecting a 2D tour. Did you mean to call `proto_highlight1d`?")
  
  ## do.call aes() over the aes_args
  .aes_func <- function(...)
    ggplot2::aes(x = x, y = y, frame = frame, tooltip = label, ...) ## rownum for tooltip
  .aes_call <- do.call(.aes_func, aes_args)
  ## do.call geom_point() over the identity_args
  .geom_func <- function(...) suppressWarnings(
    ggplot2::geom_point(mapping = .aes_call, data = .df_data, ...))
  ret <- do.call(.geom_func, identity_args)
  
  ## Initial mark, if needed, hard-coded some aes, no frame.
  if(mark_initial == TRUE){
    .aes_func   <- function(...)
      ggplot2::aes(x = x, y = y, ...)
    .aes_call   <- do.call(.aes_func, aes_args)
    ## do.call geom_vline over highlight obs
    .geom_func  <- function(...) suppressWarnings(ggplot2::geom_point(
      mapping = .aes_call, .df_data[1L, ], ## only the first row, should be frame 1.
      ..., alpha = .5)) ## Hard-coded alpha
    inital_mark <- do.call(.geom_func, identity_args)
    ret <- list(inital_mark, ret)
  }
  
  ## Return proto
  return(ret)
}


#' @rdname proto_highlight
#' @export
#' @family ggtour proto
#' @examples
#' ## 1D case:
#' gt_path <- tourr::save_history(dat, grand_tour(d = 1), max_bases = 3)
#' 
#' ggt <- ggtour(gt_path, dat) +
#'   proto_default1d(list(fill = clas, color = clas)) +
#'   proto_highlight1d(rownum_index = 7)
#' \dontrun{
#' animate_plotly(ggt)
#' }
#' 
#' ggt2 <- ggtour(gt_path, dat) +
#'   proto_default1d(list(fill = clas, color = clas)) +
#'   proto_highlight1d(rownum_index = c(2, 6, 7))
#' \dontrun{
#' animate_plotly(ggt2)
#' }
proto_highlight1d <- function(
  rownum_index,
  aes_args = list(),
  identity_args = list(color = "red", linetype = 2, alpha = .9),
  mark_initial = if(length(rownum_index) == 1) TRUE else FALSE
){
  ## Initialize
  eval(.init4proto)
  if(is.null(last_ggtour()$df_data) == TRUE) return()
  .center <- map_relative(data.frame(x = 0L, y = 0L), "center", .map_to)
  
  ## geom_segment do.calls, moving with frame
  .ymin <- min(.map_to[, 2L])
  .ymax <- max(.map_to[, 2L])
  .segment_tail <- diff(c(.ymin, .ymax)) * .1
  .aes_func <- function(...)
    ggplot2::aes(x = x, xend = x,  y = .ymin - .segment_tail,
                 yend = .ymax + .segment_tail,
                 frame = frame, tooltip = label, ...) ## rownum for tooltip
  .aes_call <- do.call(.aes_func, aes_args)
  .geom_func <- function(...) suppressWarnings(ggplot2::geom_segment(
    mapping = .aes_call, .df_data, ...))
  ret <- do.call(.geom_func, identity_args)
  
  ## Initial mark, if needed, no frame, some hard-coded aes.
  if(mark_initial == TRUE){
    .aes_func <- function(...)
      ggplot2::aes(x = x, xend = x, y = .ymin - .segment_tail,
                   yend = .ymax + .segment_tail, ...)
    .aes_call <- do.call(.aes_func, aes_args)
    ## do.call geom_segment for highlight obs
    .geom_func <- function(...) suppressWarnings(ggplot2::geom_segment(
      mapping = .aes_call, .df_data[1L, ], ## Only the first row, should be frame 1.
      ..., alpha = .5)) ## Hard coded alpha
    inital_mark <- do.call(.geom_func, identity_args)
    ret <- list(inital_mark, ret)
  }
  
  ## Return
  return(ret)
}


if(FALSE){ ## DONT RUN
  proto_hdr <- function(
    aes_args = list(),
    identity_args = list(),
    levels = c(1, 50, 99),
    kde.package = c("ash", "ks"),
    noutliers = NULL,
    label = NULL
  ){
    ## Initialize
    eval(.init4proto)
    if(is.null(.df_data) == TRUE) stop("Data is NULL, proto not applicable.")
    if(is.null(.df_data$y)) stop("proto_hdr: Projection y not found, expects a 2D tour.")
    
    ##TODO: DENSITY WORK & SEGMENT.
    #### each segment will need it's own .aes and .geom do.calls.
    #### All but the lowest density regions will want to go to geom_density or geom_hexbin.
    if(F)
      ?hdrcde::hdrscatterplot
    
    ## do.call aes() over the aes_args
    .aes_func <- function(...)
      ggplot2::aes(x = x, y = y, frame = frame, tooltip = label, ...) ## tooltip for plotly on hover tip
    .aes_call <- do.call(.aes_func, aes_args)
    ## do.call geom_point() over the identity_args
    .geom_func <- function(...) suppressWarnings(
      ggplot2::geom_point(mapping = .aes_call, data = .df_data, ...))
    
    ## Return proto
    return(do.call(.geom_func, identity_args))
  }
}