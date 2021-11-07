### UTIL -----
#' Prepare a new grammar of graphics tour
#'
#' `ggtour()` initializes a ggplot object for a tour. `proto_*` functions are
#' added to the tour, analogous to `ggplot() + geom_*`. The final tour object is
#' then animated with `animate_plotly()` or `animate_ggtour()`, or passed to
#' `filmstrip()` for static plot faceting on frames.
#'
#' @param basis_array An array of projection bases for the tour, as produced
#' with `manual_tour()` or `tour::save_history()`, or a single basis.
#' @param data Numeric data to project. If left NULL, will check if it data is 
#' stored as an attribute of the the `basis_array`.
#' @param angle Target angle (in radians) for interpolation for
#' `tour::save_history()` generated `basis_array`. Defaults to .05.
#' @param basis_label Labels for basis display, a character 
#' vector with length equal to the number of variables.
#' Defaults to the 3 character abbreviation of the original variables names.
#' @param data_label Labels for `plotly` tooltip display. 
#' Defaults to the row number, (and rownames of the data if they contain 
#' characters).
#' @export
#' @family ggtour proto functions
#' @examples
#' library(spinifex)
#' dat     <- scale_sd(flea[, 1:6])
#' clas    <- flea$species
#' bas     <- basis_pca(dat)
#' mv      <- manip_var_of(bas)
#' mt_path <- manual_tour(bas, manip_var = mv)
#' 
#' ## d = 2 case
#' ggt <- ggtour(basis_array = mt_path, data = dat, angle = .3) +
#'   proto_default(aes_args = list(color = clas, shape = clas),
#'                 identity_args = list(size = 1.5, alpha = .8))
#' \dontrun{
#' animate_plotly(ggt)
#' }
#' 
#' ## Finer control calling individual proto_* functions
#' ggt <- ggtour(basis_array = mt_path, data = dat, angle = .3) +
#'   proto_basis(position = "right", 
#'               manip_col = "red",
#'               text_size = 7L) +
#'   proto_point(aes_args = list(color = clas, shape = clas),
#'               identity_args = list(size = 1.5, alpha = .8),
#'               row_index = which(clas == levels(clas)[1])) +
#'   proto_origin()
#' \dontrun{
#' animate_plotly(ggt)
#' }
#' 
#' ## d = 1 case
#' bas1d <- basis_pca(dat, d = 1)
#' mt_path1d <- manual_tour(basis = bas1d, manip_var = mv)
#' 
#' ggt1d <- ggtour(basis_array = mt_path1d, data = dat, angle = .3) +
#'   proto_default1d(aes_args = list(fill= clas, color = clas))
#' \dontrun{
#' animate_plotly(ggt1d)
#' }
#' 
#' ## basis_array is a single matrix, returning
#' ### As static ggplot2 object
#' ggt <- ggtour(basis_array = bas, data = dat) +
#'   proto_default(aes_args = list(fill= clas, color = clas))
#' ggt
#' ### As html widget with tooltips
#' animate_plotly(ggt)
ggtour <- function(basis_array,
                   data = NULL,
                   angle = .05,
                   ##TESTING:
                   basis_label = if(is.null(data) == FALSE) abbreviate(colnames(data), 3) else paste0("v", 1:nrow(basis_array)),
                   data_label = 1:nrow(basis_array)
){
  ## If data missing, check if data is passed to the basis_array
  if(is.null(data) == TRUE) data <- attr(basis_array, "data") ## Could be NULL
  .manip_var <- attr(basis_array, "manip_var") ## NULL if not a manual tour
  
  ## If characters are used in the rownames, add them to tooltip
  if(is.null(data) == FALSE &
     suppressWarnings(any(is.na(as.numeric(as.character(rownames(data)))))))
    data_label <- paste0("row: ", 1L:nrow(data), ", ", rownames(data))
  
  ## Single basis, no manip var as a matrix coerce to array.
  if(is.null(.manip_var) == TRUE) ## if manip_var is null; IE. single basis, not tour
    if(length(dim(basis_array)) == 2L) ## AND 1 basis.
      basis_array <- array(
        as.matrix(basis_array), dim = c(dim(basis_array), 1L))
  ## Interpolate {tourr} tours
  if(is.null(.manip_var) == TRUE) ## if manip_var is null; IE. from tourr
    .m <- utils::capture.output(
      .interpolated_basis_array <- tourr::interpolate(basis_array, angle))
  ## Interpolate manual tours
  if(is.null(.manip_var) == FALSE)
    ## Basis_array from manual tours is only 1 basis.
    .interpolated_basis_array <- interpolate_manual_tour(basis_array, angle)
  
  df_ls <- array2df(.interpolated_basis_array, data, basis_label, data_label)
  .df_basis <- df_ls$basis_frames
  attr(.df_basis, "manip_var") <- .manip_var ## NULL if not a manual tour
  .n_frames <- length(unique(.df_basis$frame))
  .d <- ncol(.df_basis) - 2L
  .p <- nrow(.df_basis) / .n_frames
  eval(.fortify_data)
  
  ## BYPRODUCT: Assign list to last_ggtour().
  .set_last_ggtour(list(
    interpolated_basis_array = .interpolated_basis_array,
    df_basis = .df_basis, df_data = .df_data, map_to_unitbox = .map_to_unitbox,
    map_to_density = .map_to_density, map_to_data = .map_to_data,
    n_frames = .n_frames, nrow_df_data = .nrow_df_data, n = .n, p = .p, d = .d,
    manip_var = .manip_var, is_faceted = FALSE))
  
  ## Return ggplot head, theme, and facet if used
  return(ggplot2::ggplot(.df_basis) + spinifex::theme_spinifex())
}
# ## Print method for ggtours ?? using proto_default()
# #### Was a good idea, but ggplot stops working when you change the first class, 
# #### and doesn't effect if you change the last class.
# print.ggtour <- function(x, ...){
#   class(x) <- c("gg", "ggplot")
#   x +
#     proto_basis() +
#     proto_origin(gridline_probs = FALSE) +
#     proto_point()
# }

#' Wrap a 1d ribbon of panels into 2d for animation
#' 
#' Create and wrap a 1d ribbon of panels in 2d. 
#' Because of byproducts of `ggtour` and `facet_wrap_tour` this wants to be 
#' applied after `ggtour` and before any `proto_*` functions.
#' `plotly` may not display well with with faceting.
#' 
#' @param facet_var Expects a single variable to facet the levels of. 
#' Should be a vector not formula, `~cyl`, or `ggplot2::vars()` call.
#' @param nrow Number of rows. Defaults to NULL; set by display dim.
#' @param ncol Number of columns. Defaults to NULL; set by display dim.
#' @param dir Direction of wrapping: either "h" horizontal by rows, 
#' or "v", for vertical by columns. Defaults to "h"
#' @export
#' @family ggtour proto functions
#' @examples
#' library(spinifex)
#' dat     <- scale_sd(flea[, 1:6])
#' clas    <- flea$species
#' bas     <- basis_pca(dat)
#' mv      <- manip_var_of(bas)
#' mt_path <- manual_tour(bas, manip_var = mv)
#' 
#' ## d = 2 case
#' message("facet_wrap_tour wants be called early, so that other proto's adopt the facet_var.")
#' ggt <- ggtour(mt_path, dat, angle = .3) +
#'   facet_wrap_tour(facet_var = clas, ncol = 2, nrow = 2) +
#'   proto_default(list(color = clas, shape = clas),
#'                 list(size = 1.5))
#' \dontrun{
#' animate_gganimate(ggt) ## Faceting not likely to play well with `plotly`
#' }
facet_wrap_tour <- function(
  facet_var = NULL, nrow = NULL, ncol = NULL, dir = "h"
){
  eval(.init4proto)
  
  ## Append facet_var to df_basis and df_data if needed.
  if(is.null(facet_var) == FALSE){
    .df_data <- .bind_elements2df(
      list(facet_var = rep_len(facet_var, .nrow_df_data)), .df_data)
    ## "_basis_" becomes an honorary level of facet_var
    .df_basis <- .bind_elements2df( ## basis facet always on first/top level
      list(facet_var = rep_len("_basis_", nrow(.df_basis))), .df_basis)
  }
  
  ## BYPRODUCT:
  #### Changes: .df_basis & .df_data have facet_var bound, is_faceted == TRUE
  .ggt$df_basis   <- .df_basis
  .ggt$df_data    <- .df_data
  .ggt$facet_var  <- facet_var
  .ggt$is_faceted <- TRUE
  .set_last_ggtour(.ggt)
  
  ## Return
  return(list(
    ggplot2::facet_wrap(facets = ggplot2::vars(facet_var),
                        nrow = nrow, ncol = ncol, dir = dir),
    ggplot2::theme(strip.text = ggplot2::element_text(
      margin = ggplot2::margin(b = 0L, t = 0L)),  ## tighter facet labels
      panel.spacing = ggplot2::unit(0L, "lines")) ## tighter facet spacing
  ))
}


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
  ## Check names
  if(is.null(.nms) | length(.nms) > length(unique(.nms)))
    stop(".lapply_rep_len: args list was unamed or had none unique names. Please ensure that elements of aes_args and indentity_args have unique names.")
  .m <- lapply(seq_along(list), function(i){
    .elem <- list[[i]]
    ## Check cycling
    if(length(.elem) != 1L & typeof(.elem) != "environment"){
      if(length(.elem) != expected_length) 
        warning(paste0(".lapply_rep_len: `", .nms[i], "` not of length 1 or data; liable to cause cycling issues. Should it be of length 1 or data?"))
      ret_vect <- rep_len(.elem, to_length)
    }else ret_vect <- .elem
    list[[i]] <<- ret_vect
  })
  return(list)
}

#' Binds replicated elements of a list as columns of a data frame.
#' 
#' Internal function. To be applied to `aes_args` 
#' replicates elements to the length of the data and bind as a column.
#'
#' @param list A list of arguments such as those passed in `aes_args` and 
#' `identity_args`.
#' @param df A data.frame to column bind the elements of `list` to.
#' @family Internal utility
#' @examples
#' ## This function is not meant for external use
.bind_elements2df <- function(list, df){
  .list <- as.list(list)
  .ret <- as.data.frame(df)
  .ret_nms <- names(.ret)
  .l_nms <- names(list)
  .m <- lapply(seq_along(.list), function(i){
    .ret <<- cbind(.ret, .list[[i]])
  })
  names(.ret) <- c(.ret_nms, .l_nms)
  return(.ret)
}


#' Prep data, especially for local data passes in `proto_*`
#' 
#' Internal expression. Creates local .objects to be commonly consumed by 
#' spinifex proto_* functions.
#'
#' @export
#' @family Internal utility
#' @examples
#' ## This expression. is not meant for external use.
## _.fortify_data expression -----
.fortify_data <- expression({ ## Expects df_ls
  .df_data <- df_ls$data_frames ## Can be NULL
  ## Make several df bounding boxs to map_relative to, but let the proto_* select.
  .map_to_unitbox <- data.frame(x = c(-1L, 1L), y = c(-1L, 1L))
  if(is.null(data) == FALSE){
    .map_to_density <- data.frame(x = range(.df_data[, 1L]), y = c(0L, 1L))
    if(ncol(.interpolated_basis_array) >= 1L)
      .map_to_data <- data.frame(x = range(.df_data[, 1L]),
                                 y = range(.df_data[, 2L]))
  }
  .nrow_df_data <- nrow(.df_data) ## NULL if data is NULL
  .n <- .nrow_df_data / .n_frames ## NULL if data is NULL
})


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
.init4proto <- expression({ ## An expression, not a function
  .ggt <- spinifex::last_ggtour() ## Self-explicit for use in cheem
  if(is.null(.ggt)) stop(".init4proto: last_ggtour() is NULL, have you run ggtour() yet?")
  
  ## Assign elements ggtour list as quiet .objects in the environment
  .env <- environment()
  .nms <- names(.ggt)
  .m <- sapply(seq_along(.ggt), function(i){
    assign(paste0(".", .nms[i]), .ggt[[i]], envir = .env)
  })
  
  ## row_index, if exists
  if(exists("row_index")){
    if(is.null(row_index) == FALSE)
      if(identical(row_index, TRUE) == FALSE){
        
        ## Coerce index to full length logical index
        if(is.numeric(row_index) == TRUE){
          .rep_f <- rep(FALSE, .n)
          .rep_f[row_index] <- TRUE
          row_index <- .rep_f
        }
        
        ### Background case:
        if(exists("bkg_color") == TRUE)
          if(is.null("bkg_color") == FALSE)
            if(bkg_color != FALSE){
              #### Subset (but not replicate) bkg_aes_args, bkg_identity_args:
              if(exists("aes_args"))
                if(length(aes_args) > 0L)
                  bkg_aes_args <- lapply(aes_args, function(arg)arg[!row_index])
              if(exists("identity_args"))
                if(length(aes_args) > 0L)
                  bkg_identity_args <- lapply(identity_args, function(arg)
                    if(length(arg) == .n) arg[!row_index] else arg)
              #### Subset .df_data_bkg
              .df_data_bkg <- .df_data[
                rep(!row_index, .n_frames),, drop = FALSE]
            }
        
        ### Foreground case:
        #### Subset (but not replicate) aes_args, identity_args
        if(exists("aes_args"))
          if(length(aes_args) > 0L)
            aes_args <- lapply(aes_args, function(arg)arg[row_index])
        if(exists("identity_args"))
          if(length(aes_args) > 0L)
            identity_args <- lapply(identity_args, function(arg)
              if(length(arg) == .n) arg[row_index] else arg)
        
        #### Subset .df_data, update .n & .nrow_df_data
        .df_data <- .df_data[
          rep(row_index, .n_frames),, drop = FALSE]
        .n <- sum(row_index) ## n rows _slecected_
        .nrow_df_data <- nrow(.df_data)
      }
  } ## end row_index, if exists
  ## If data is passed locally to data arg, update it
  if(is.null(data) == FALSE & identical(data, utils::data) == FALSE){
    df_ls <- array2df(.interpolated_basis_array, data)
    eval(.fortify_data)
  }
  
  ##Possible dev: to fix the legend issue of color and shape mapped to class:
  # would need to bind args to the .df_data, and then remake a true aes(), 
  # based on the names of the lists. will need to quote/or symb, probably. 
  # .df_data <- .bind_elements2df(aes_args, .df_data)
  # aes_args <- ## TODO>>>>, go to aes_string("mpg") or aes_(quote(mpg))?
  
  ## Replicate argument lists, if they exist
  if(exists("row_index"))
    if(is.null(row_index) == FALSE){
      if(exists("bkg_aes_args"))
        if(length(bkg_aes_args) > 0L)
          bkg_aes_args <- spinifex:::.lapply_rep_len(
            bkg_aes_args, nrow(.df_data_bkg), sum(!row_index))
      if(exists("bkg_identity_args"))
        if(length(identity_args) > 0L)
          bkg_identity_args <- spinifex:::.lapply_rep_len(
            bkg_identity_args, nrow(.df_data_bkg), sum(!row_index))
    }
  if(exists("aes_args"))
    if(length(aes_args) > 0L)
      aes_args <- spinifex:::.lapply_rep_len(aes_args, .nrow_df_data, .n)
  if(exists("identity_args"))
    if(length(identity_args) > 0L)
      identity_args <- spinifex:::.lapply_rep_len(identity_args, .nrow_df_data, .n)
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
#' library(spinifex)
#' dat     <- scale_sd(flea[, 1:6])
#' clas    <- flea$species
#' bas     <- basis_pca(dat)
#' mv      <- manip_var_of(bas)
#' mt_path <- manual_tour(bas, manip_var = mv)
#' 
#' ggt <- ggtour(mt_path, dat, angle = .3) +
#'   proto_default(aes_args = list(color = clas, shape = clas),
#'                 identity_args = list(size = 1.5, alpha = .7))
#' \dontrun{
#' animate_gganimate(ggt)
#' 
#' if(F){ ## Don't accidentally save file
#'   ## Alternative arguments storing to a variable for saving
#'   anim <- animate_gganimate(
#'     ggt, fps = 10, rewind = TRUE,
#'     start_pause = 1, end_pause = 2,
#'     height = 6, width = 10, units = "cm", ## "px", "in", "cm", or "mm."
#'     res = 150)
#'   ## Save rendered animation as .gif
#'   gganimate::anim_save("my_tour.gif",
#'                        animation = anim,
#'                        path = "./figures")
#'   
#'   ## Alternative renderer saving directly as an .mp4
#'   animate_gganimate(ggt,
#'     height = 10, width = 18, units = "cm", ## "px", "in", "cm", or "mm."
#'     res = 150, ## resolution in dpi (dots per inch)
#'     render = gganimate::av_renderer("./my_tour.mp4")) ## Alternative render
#'   }
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
  
  .set_last_ggtour(NULL) ## Clears last tour
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
#' \code{\link[plotly:layout]{plotly::layout}}.
#' @seealso \code{\link[plotly:ggplotly]{plotly::ggplotly}}
#' @export
#' @family ggtour animator
#' @examples
#' library(spinifex)
#' dat     <- scale_sd(flea[, 1:6])
#' clas    <- flea$species
#' bas     <- basis_pca(dat)
#' mv      <- manip_var_of(bas)
#' mt_path <- manual_tour(bas, manip_var = mv)
#' 
#' ggt <- ggtour(mt_path, dat, angle = .3) +
#'   proto_origin() +
#'   proto_basis() +
#'   proto_point(aes_args = list(color = clas, shape = clas),
#'               identity_args = list(size = 1.5, alpha = .7))
#' \dontrun{
#' animate_plotly(ggt, width = 700, height = 450) ## pixels only, no resolution argument
#' 
#' ## Example saving to a .html widget, may require additional setup.
#' if(F){
#'   anim <- animate_plotly(ggt, fps = 10, width = 700, height = 450)
#'   
#'   htmlwidgets::saveWidget(widget = anim,
#'                           file = "./figures/my_tour.html",
#'                           selfcontained = TRUE)}
#' }
animate_plotly <- function(
  ggtour,
  fps = 8,
  ... ## Passed to plotly::layout().
){
  ## Frame asymmetry issue: https://github.com/ropensci/plotly/issues/1696
  #### Adding many protos is liable to break plotly animations, see above url.
  ## Assumptions
  if(length(ggtour$layers) == 0L) stop("No layers found, did you forget to add a proto_*?")
  n_frames <- length(unique(last_ggtour()$df_basis$frame))
  ## 1 Frame only:
  if(n_frames == 1L){
    warning("ggtour df_basis only has 1 frame, applying just plotly::ggplotly instead.")
    anim <- plotly::ggplotly(p = ggtour, tooltip = "tooltip") %>%
      ## Remove button bar and zoom box
      plotly::config(displayModeBar = FALSE,
                     modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d")) %>%
      ## Remove legends and axis lines
      plotly::layout(showlegend = FALSE, dragmode = FALSE,
                     #, fixedrange = TRUE ## This is a curse, do not use.
                     yaxis = list(showgrid = FALSE, showline = FALSE),
                     xaxis = list(showgrid = FALSE, showline = FALSE,
                                  scaleanchor = "y", scalaratio = 1L),
                     ...)
  }else{
    ## More than 1 frame:
    
    ## Block plotly.js warning: lack of support for horizontal legend;
    #### https://github.com/plotly/plotly.js/issues/53
    anim <- suppressWarnings(
      plotly::ggplotly(p = ggtour, tooltip = "tooltip") %>%
        plotly::animation_opts(frame = 1L / fps * 1000L,
                               transition = 0L, redraw = TRUE) %>%
        plotly::animation_slider(
          active = 0L, ## 0 indexed first frame
          currentvalue = list(prefix = "Frame: ", font = list(color = "black"))
        ) %>%
        ## Remove button bar and zoom box
        plotly::config(displayModeBar = FALSE,
                       modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d"))) %>%
      ## Remove legends and axis lines
      plotly::layout(showlegend = FALSE, dragmode = FALSE,
                     #, fixedrange = TRUE ## This is a curse, do not use.
                     yaxis = list(showgrid = FALSE, showline = FALSE),
                     xaxis = list(showgrid = FALSE, showline = FALSE,
                                  scaleanchor = "y", scalaratio = 1L),
                     ...)
  }
  
  ## Clean up
  .set_last_ggtour(NULL) ## Clears last tour
  ## This should prevent some errors from not running ggtour() right before animating it.
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
# #' dat     <- scale_sd(flea[, 1:6])
# #' clas    <- flea$species
# #' bas     <- basis_pca(dat)
# #' mv      <- manip_var_of(bas)
# #' mt_path <- manual_tour(bas, manip_var = mv)
# #' 
# #' ggt <- ggtour(mt_path, dat, angle = .1) +
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
#' @family ggtour animator
#' @examples
#' dat  <- scale_sd(flea[, 1:6])
#' clas <- flea$species
#' bas  <- basis_pca(dat)
#' mv   <- manip_var_of(bas)
#' 
#' ## d = 2 case
#' mt_path <- manual_tour(bas, manip_var = mv)
#' ggt <- ggtour(mt_path, dat, angle = .3) +
#'   proto_point(list(color = clas, shape = clas),
#'               list(size = 1.5)) +
#'   proto_basis()
#' filmstrip(ggt)
#' 
#' ## Keep select frames, d = 1 case
#' bas1d     <- basis_pca(dat, d = 1)
#' mt_path1d <- manual_tour(basis = bas1d, manip_var = mv)
#' ggt1d <- ggtour(mt_path1d, dat, angle = .3) +
#'   proto_default1d(list(fill = clas))
#' filmstrip(ggt1d)
filmstrip <- function(
  ggtour
){
  #frame_nums_kept = NULL, 
  ## Not going to work; would have to modify all mappings and identity args.
  # if(is.null(frame_nums_kept) == FALSE){
  #   ggtour$data <- ggtour$data[ggtour$data$frame %in% frame_nums_kept, ]
  #   .n_layers <- length(ggtour$layers)
  #   .m <- sapply(1L:.n_layers, function(i){
  #     ggtour$layers[[i]]$data <<-
  #       ggtour$layers[[i]]$data[
  #         ggtour$layers[[i]]$data$frame %in% frame_nums_kept, ]
  #   })
  # }
  
  ret <- ggtour +
    ggplot2::facet_wrap(ggplot2::vars(factor(frame))) + ## facet on frame
    ggplot2::theme(strip.text = ggplot2::element_text(
      margin = ggplot2::margin(b = 0L, t = 0L)),  ## tighter facet labels
      panel.spacing = ggplot2::unit(0L, "lines")) ## tighter facet spacing
  ## filmstrip does NOT clear last tour
  .m <- gc() ## Mute garbage collection
  return(ret)
}


### BASIS Protos ------
#' Tour proto for a 2D and 1D basis axes respectively
#'
#' Adds basis axes to the animation, the direction and magnitude of 
#' contributions of the variables to the projection space inscribed in a unit 
#' circle for 2D or rectangle of unit width for 1D.
#'
#' @param position The position, to place the basis axes relative to the centered 
#' data. `_basis` Expects one of c("left", "center", "right", "bottomleft", "topright", 
#' "off"), defaults to "left". `_basis1d` Expects one of 
#' c("top1d", "floor1d", "top2d", "floor2d", "off"), 
#' defaults to "top1d".
#' @param manip_col The color to highlight the manipulation variable with. Not
#' applied if the tour isn't a manual tour. Defaults to "blue".
#' @param line_size (2D bases only) the thickness of the lines used to make the 
#' axes and unit circle. Defaults to 1.
#' @param text_size Size of the text label of the variables.
#' @export
#' @aliases proto_basis
#' @family ggtour proto functions
#' @examples
#' library(spinifex)
#' dat  <- scale_sd(flea[, 1:6])
#' clas <- flea$species
#' bas  <- basis_pca(dat)
#' mv   <- manip_var_of(bas)
#' 
#' ## 2D case:
#' mt_path <- manual_tour(bas, manip_var = mv)
#' ggt <- ggtour(mt_path, dat, angle = .3) +
#'   proto_default()
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
#' bas1d     <- basis_pca(dat, d = 1)
#' mv        <- manip_var_of(bas, 3)
#' mt_path1d <- manual_tour(bas1d, manip_var = mv)
#' 
#' ggt1d <- ggtour(mt_path1d, dat, angle = .3) +
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
  if(.is_faceted == TRUE) position = "center"
  
  ## Setup and transform
  .angles <- seq(0L, 2L * pi, length = 360L)
  .circle <- data.frame(x = cos(.angles), y = sin(.angles))
  .center <- map_relative(data.frame(x = 0L, y = 0L), position, .map_to_data)
  .circle <- map_relative(.circle, position, .map_to_data)
  .df_basis <- map_relative(.df_basis, position, .map_to_data)
  if(.is_faceted == TRUE)
    .circle <- .bind_elements2df(
      list(facet_var = rep_len("_basis_", nrow(.circle))), .circle)
  
  ## Aesthetics for the axes segments.
  .axes_col <- "grey50"
  .axes_siz <- line_size
  if(is.null(.manip_var) == FALSE){
    .axes_col <- rep("grey50", .p)
    .axes_col[.manip_var] <- manip_col
    .axes_col <- rep(.axes_col, .n_frames)
    .axes_siz <- rep(line_size, .p)
    .axes_siz[.manip_var] <- 1.5 * line_size
    .axes_siz <- rep(.axes_siz, .n_frames)
  }
  
  ## Return proto
  return(list(
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
  ))
}


#' @rdname proto_basis
#' @param segment_size (1D bases only) the width thickness of the rectangle bar
#' showing variable magnitude on the axes. Defaults to 2.
#' @export
#' @family ggtour proto functions
proto_basis1d <- function(
  position = c("top1d", "floor1d", "top2d", "floor2d", "off"),
  manip_col = "blue",
  segment_size = 2,
  text_size = 5
){
  ## Initialize
  eval(.init4proto)
  position = match.arg(position)
  if(position == "off") return()
  if(.is_faceted == TRUE) position = "floor1d"
  
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
  .df_txt <- data.frame(
    x = -1.25, y = .p:1L, label = .df_basis[.df_basis$frame == 1L, "label"])
  .df_rect <- data.frame(x = c(-1L, 1L), y = c(.5, .p + .5))
  .df_seg0 <- data.frame(x = 0L, y = c(.5, .p + .5))
  ## Scale them
  #### if basis 1D map to density, else map to data (ie. cheem)
  if(.d == 1L){
    .map_to_tgt <- .map_to_density
  }else .map_to_tgt <- .map_to_data
  .df_zero <- map_relative(.df_zero, position, .map_to_tgt)
  .df_seg  <- map_relative(.df_seg,  position, .map_to_tgt)
  .df_txt  <- map_relative(.df_txt,  position, .map_to_tgt)
  .df_rect <- map_relative(.df_rect, position, .map_to_tgt)
  .df_seg0 <- map_relative(.df_seg0, position, .map_to_tgt)
  if(.is_faceted == TRUE){
    .basis_ls <- list(facet_var = rep_len("_basis_", nrow(.df_zero)))
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
      size = text_size, color = "grey60",
      hjust = 1L),
    ## Contribution segments of current basis, changing with frame
    suppressWarnings(ggplot2::geom_segment(
      ggplot2::aes(x, y, xend = .df_zero[, 1L], yend = y, frame = frame),
      .df_seg, color = .axes_col, size = .axes_siz))
  ))
}

#' draw a basis on a static ggplot
#' 
#' Aditively draws a basis on a static ggplot.
#' Not a formal `geom`, nor does it use the setup from `ggtour` that 
#' `proto_*` functions expect.
#' 
#' @param basis A (p*d) basis to draw. Draws the first two components. 
#' If facet is used cbind the facet variable to a specidic facet level 
#' (2nd example), otherwise the basis prints on all facet levels.
#' @param map_to A data.frame to scale the basis to. 
#' Defaults to a unitbox; data.frame(x=c(0,1), y=c(0,1)).
#' @param position The position, to place the basis axes relative to the centered 
#' data. `_basis` Expects one of c("left", "center", "right", "bottomleft", 
#' "topright", "off"), defaults to "left".
#' @param manip_col The color to highlight the manipulation variable with. Not
#' applied if the tour isn't a manual tour. Defaults to "blue".
#' @param line_size (2D bases only) the thickness of the lines used to make the 
#' axes and unit circle. Defaults to 1.
#' @param text_size Size of the text label of the variables.
#' @param label The text labels of the data variables. 
#' Defaults to the 3 character abbreviation of the rownames of the basis.
#' @export
#' @examples
#' library(spinifex)
#' library(ggplot2)
#' dat  <- scale_sd(flea[, 1:6])
#' clas <- flea$species
#' bas  <- basis_pca(dat)
#' proj <- as.data.frame(dat %*% bas)
#' 
#' ggplot() +
#'   geom_point(aes(PC1, PC2), proj) +
#'   draw_basis(bas, proj, "left") +
#'   coord_fixed()
#'   
#' ## Aesthetics and facet
#' proj <- cbind(proj, clas = flea$species)
#' bas <- cbind(as.data.frame(bas), clas = levels(clas)[2])
#' ggplot() +
#'   facet_wrap(vars(clas)) +
#'   geom_point(aes(PC1, PC2, color = clas, shape = clas), proj) +
#'   draw_basis(bas, proj, "left") +
#'   theme_bw() +
#'   coord_fixed()
#' # To repeat basis in all facet levels don't cbind a facet variable.
draw_basis <- function(
  basis, ## WITH APPENDED FACET LEVEL
  map_to = data.frame(x = c(0, 1), y = c(0, 1)),
  position = c("left", "center", "right", "bottomleft", "topright", "off"),
  manip_col = "blue",
  line_size = 1,
  text_size = 5,
  label = abbreviate(rownames(basis), 3L)
){
  ## Initialize
  d <- ncol(basis)
  if(d < 2L)
    stop("geom_basis: expects a basis of 2 or more columns.")
  position = match.arg(position)
  if(position == "off") return()
  
  ## Setup and transform
  .angles <- seq(0L, 2L * pi, length = 360L)
  .circle <- data.frame(x = cos(.angles), y = sin(.angles))
  .center <- map_relative(data.frame(x = 0L, y = 0L), position, map_to)
  .circle <- map_relative(.circle, position, map_to)
  
  ## Handle facet var if used:
  # Assuming a char/fct in last col is the facet_var
  .p <- ncol(basis) 
  if(is.numeric(basis[, .p]) == FALSE){
    .circle$facet_var <- basis[, .p]
    colnames(.circle) <- c("x", "y", colnames(basis)[.p])
  }
  .df_basis <- as.data.frame(map_relative(basis, position, map_to))
  colnames(.df_basis)[1L:2L] <- c("x", "y")
  
  ## Aesthetics for the axes segments.
  .axes_col <- "grey50"
  .axes_siz <- line_size
  .manip_var <- attr(basis, "manip_var")
  if(is.null(.manip_var) == FALSE){
    .axes_col <- rep("grey50", .p)
    .axes_col[.manip_var] <- manip_col
    .axes_col <- rep(.axes_col, .n_frames)
    .axes_siz <- rep(line_size, .p)
    .axes_siz[.manip_var] <- 1.5 * line_size
    .axes_siz <- rep(.axes_siz, .n_frames)
  }
  
  ## Return proto
  return(list(
    ggplot2::geom_path(data = .circle, color = "grey80",
                       size = line_size, inherit.aes = FALSE,
                       mapping = ggplot2::aes(x = x, y = y)),
    suppressWarnings(ggplot2::geom_segment( ## Suppress unused arg: frames
      data = .df_basis,
      size = .axes_siz, color = .axes_col,
      mapping = ggplot2::aes(
        x = x, y = y, xend = .center[, 1L], yend = .center[, 2L])
    )),
    suppressWarnings(ggplot2::geom_text(
      data = .df_basis,
      color = .axes_col, size = text_size,
      vjust = "outward", hjust = "outward",
      mapping = ggplot2::aes(x = x, y = y, label = label)
    ))
  ))
}





### DATA Protos ----
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
#' @param data Optionally pass a dataframe to this proto, superceeds data 
#' passed to `ggtour`. Analogous to ggplot2 where data passed into a `geom_*` 
#' is used independent of data passed to `ggplot`.
#' @param bkg_color The character color by name or hexidemical to display
#' backgrund observations, those not identified in `row_index`. 
#' Defaults to "grey80". Use FALSE or NULL to skip rendering background points.
#' Other aesthetic values such as shape and alpha are set adopted from 
#' `aes_args` and `identity_args`.
#' @export
#' @aliases proto_points
#' @family ggtour proto functions
#' @examples
#' library(spinifex)
#' dat     <- scale_sd(flea[, 1:6])
#' clas    <- flea$species
#' gt_path <- save_history(dat, grand_tour(), max_bases = 5)
#' 
#' ggt <- ggtour(gt_path, dat, angle = .3) +
#'   proto_point(aes_args = list(color = clas, shape = clas),
#'               identity_args = list(size = 2, alpha = .7))
#' \dontrun{
#' animate_plotly(ggt)
#' }
#' 
#' ## Select/highlight observations with `row_index`
#' ggt <- ggtour(gt_path, dat, angle = .3) +
#'   proto_point(aes_args = list(color = clas, shape = clas),
#'               identity_args = list(size = 2, alpha = .7),
#'               row_index = which(clas == levels(clas)[1]),
#'               bkg_color = "grey80") ## FALSE or NULL to skip ploting background
#' \dontrun{
#' animate_plotly(ggt)
#' }
proto_point <- function(
  aes_args = list(),
  identity_args = list(),
  row_index = TRUE,
  bkg_color = "grey80"
){
  ## Initialize
  eval(.init4proto)
  if(is.null(.df_data) == TRUE)
    stop("proto_point: Data is NULL. Was data passed to the basis array or ggtour?")
  if(is.null(.df_data$y))
    stop("proto_point: Projection y not found, expected a 2D tour.")
  
  ## do.call aes() over the aes_args
  .aes_func <- function(...)
    ggplot2::aes(x = x, y = y, frame = frame, tooltip = label, ...) ## tolltip for plotly hover tt.
  .aes_call <- do.call(.aes_func, aes_args)
  ## do.call geom_point() over the identity_args
  .geom_func <- function(...) suppressWarnings(
    ggplot2::geom_point(mapping = .aes_call, data = .df_data, ...))
  ret <- do.call(.geom_func, identity_args)
  

  if(is.null(bkg_color) == FALSE)
    if(bkg_color != FALSE)
      if(exists("bkg_aes_args") & exists("bkg_identity_args") & exists("bkg_aes_args")){
        ## do.call aes() over the bkg_aes_args
        .aes_func <- function(...)
          ggplot2::aes(x = x, y = y, frame = frame, tooltip = label, ...) 
        .aes_call <- suppressWarnings(do.call(.aes_func, bkg_aes_args))
        ## do.call geom_point() over the bkg_identity_args
        .geom_func <- function(...) suppressWarnings(
          ggplot2::geom_point(mapping = .aes_call, data = .df_data_bkg, 
                              color = bkg_color, ...)) ## Trumps color set in aes_args
        ret <- list(do.call(.geom_func, bkg_identity_args), ret)
      }
  ## Return
  return(ret)
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
#' @param data Optionally pass a dataframe to this proto, superceeds data 
#' passed to `ggtour`. Analogous to ggplot2 where data passed into a `geom_*` 
#' is used independent of data passed to `ggplot`.
#' @param density_position The `ggplot2` position of `geom_density()`. Either 
#' c("identity", "stack"), defaults to "identity". Warning: "stack" does not 
#' work with `animate_plotly()` at the moment.
#' @param do_add_rug Logical, weather or not to add the rug marks below the 
#' density curves.
#' @export
#' @aliases proto_density1d
#' @family ggtour proto functions
#' @examples
#' library(spinifex)
#' dat     <- scale_sd(flea[, 1:6])
#' clas    <- flea$species
#' gt_path <- save_history(dat, grand_tour(), max = 3)
#' 
#' ggt <- ggtour(gt_path, dat) +
#'   proto_density(aes_args = list(color = clas, fill = clas)) +
#'   proto_basis1d()
#' \dontrun{
#' animate_plotly(ggt)
#' }
proto_density <- function(
  aes_args = list(),
  identity_args = list(alpha = .7),
  row_index = TRUE,
  density_position = c("identity", "stack", "fill"),
  do_add_rug = TRUE
){
  ## Initialize
  eval(.init4proto)
  
  if(class(transformr::tween_polygon) != "function")
    stop("proto_density requires {transformr}::tween_polygon")
  if(is.null(.df_data))
    stop("proto_density: data missing. Did you call ggtour() on a manual tour without passing data?")
  .nms <- names(aes_args)
  if(any(c("color", "colour", "col") %in% .nms) & !("fill" %in% .nms))
    warning("proto_density: aes_args contains color without fill, did you mean to use fill to color below the curve?")
  density_position <- match.arg(density_position)
  ## "identity" is the only position working in {plotly} right now.
  ## see: https://github.com/ropensci/plotly/issues/1544
  
  ## geom_density do.call
  .aes_func <- function(...)
    ggplot2::aes(x = x, y = ..ndensity.., frame = frame, ...)
  .aes_call <- do.call(.aes_func, aes_args)
  .geom_func <- function(...)suppressWarnings(
    ggplot2::geom_density(mapping = .aes_call, data = .df_data, ...,
                          position = density_position, color = "black", n = 128L))
  ret <- do.call(.geom_func, identity_args)
  
  ## geom_rug do.call
  if(do_add_rug == TRUE){
    .rug_len <- .02
    .aes_func <- function(...)
      ggplot2::aes(x = x, frame = frame, ...)
    .aes_call <- do.call(.aes_func, aes_args)
    .geom_func <- function(...) suppressWarnings(
      ggplot2::geom_rug(mapping = .aes_call, data = .df_data,
                        length = ggplot2::unit(.rug_len, "native"), ...))
    ret <- list(ret, do.call(.geom_func, identity_args))
  }
  
  ## Return
  return(ret)
}

#' @param fixed_y Vector of length of the data, values to fix vertical height.
#' Typically related to but not an explanatory variable, for instance,
#' predicted Y, or residuals.
#' @rdname proto_density
#' @export
#' @family ggtour proto functions
#' @examples
#' 
#' ## proto_point.1d_fix_y:
#' # Fixed y values are useful for related values that are 
#' # not in the X variables, _eg_ predictions or residuals of you X space.
#' message("don't forget to scale your fixed_y.")
#' dummy_y <- (as.integer(clas) + rnorm(nrow(dat))) %>% scale_sd
#' gt_path <- save_history(dat, grand_tour(), max_bases = 5)
#' 
#' message("proto_point.1d_fix_y wants to be called early so other proto's adopt the fixed_y.")
#' ggt <- ggtour(gt_path, dat, angle = .3) +
#'   proto_point.1d_fix_y(list(fill = clas, color = clas),
#'                        fixed_y = dummy_y) +
#'   proto_basis1d("top2d") +
#'   proto_origin()
#' \dontrun{
#' animate_plotly(ggt)
#' }
proto_point.1d_fix_y <- function(
  aes_args = list(),
  identity_args = list(),
  row_index = TRUE,
  fixed_y
){
  ## Initialize
  eval(.init4proto)
  if(is.null(.df_data) == TRUE)
    stop("proto_point1d_fixed_y: Data is NULL. Was data passed to the basis array or ggtour?")
  
  ## Add fixed y
  .df_data <- .bind_elements2df(
    list(fixed_y = rep_len(fixed_y, .nrow_df_data)), .df_data)
  .df_data$y <- .df_data$fixed_y
  
  ## BYPRODUCT: pass data back to .store
  # to calm some oddities; like proto_origin() complaining about y being missing
  .ggt$df_data <- .df_data
  .set_last_ggtour(.ggt)
  
  ## do.call aes() over the aes_args
  .aes_func <- function(...)
    ## tooltip for plotly on hover tooltip
    ggplot2::aes(x = x, y = y, frame = frame, tooltip = label, ...) 
  .aes_call <- do.call(.aes_func, aes_args)
  ## do.call geom_point() over the identity_args
  .geom_func <- function(...) suppressWarnings(
    ggplot2::geom_point(mapping = .aes_call, data = .df_data, ...))
  ## Return
  return(do.call(.geom_func, identity_args))
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
#' @param data Optionally pass a dataframe to this proto, superceeds data 
#' passed to `ggtour`. Analogous to ggplot2 where data passed into a `geom_*` 
#' is used independent of data passed to `ggplot`.
#' @export
#' @family ggtour proto functions
#' @examples
#' library(spinifex)
#' dat     <- scale_sd(flea[, 1:6])
#' clas    <- flea$species
#' bas     <- basis_pca(dat)
#' mv      <- manip_var_of(bas)
#' gt_path <- save_history(dat, grand_tour(), max_bases = 5)
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
#'              row_index = 1:15)
#' \dontrun{
#' animate_plotly(ggt2)
#' }
proto_text <- function(aes_args = list(),
                       identity_args = list(nudge_x = 0.05),
                       row_index = TRUE
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
#' @param aes_args A list of aesthetic arguments to passed to 
#' `geom_point(aes(X)`. Any mapping of the data to an aesthetic,
#' for example, `geom_point(aes(color = myCol, shape = myCol))` becomes
#' `aes_args = list(color = myCol, shape = myCol)`.
#' @param identity_args A list of static, identity arguments passed into 
#' `geom_point()`, but outside of `aes()`, for instance 
#' `geom_point(aes(...), size = 2, alpha = .7)` becomes 
#' `identity_args = list(size = 2, alpha = .7)`.
#' @param data Optionally pass a dataframe to this proto, superceeds data 
#' passed to `ggtour`. Analogous to ggplot2 where data passed into a `geom_*` 
#' is used independent of data passed to `ggplot`.
#' @param bins Numeric vector giving number of bins in both vertical and 
#' horizontal directions. Defaults to 30.
#' @export
#' @family ggtour proto functions
#' @examples
#' library(spinifex)
#' raw     <- ggplot2::diamonds
#' dat     <- scale_sd(raw[1:10000, c(1, 5:6, 8:10)])
#' gt_path <- save_history(dat, grand_tour(), max = 3)
#' 
#' ## 10000 rows is quite heavy to animate.
#' ## Decrease the points drawn in each frame by aggregating many points into
#' #### a hexegon heatmap, using geom_hex!
#' ggp <- ggtour(gt_path, dat) +
#'   proto_basis() +
#'   proto_hex(bins = 20)
#' 
#' ## Hexagons don't show up in plotly animation.
#' \dontrun{
#' animate_gganimate(ggp)
#' }
proto_hex <- function(
  aes_args = list(),
  identity_args = list(),
  row_index = TRUE,
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
  .aes_func <- function(...)
    ggplot2::aes(x = x, y = y, frame = frame, group = frame, ...)
  .aes_call  <- do.call(.aes_func, aes_args)
  ## do.call geom_point() over the identity_args
  .geom_func <- function(...) suppressWarnings(
    ggplot2::geom_hex(mapping = .aes_call, data = .df_data, bins = bins, ...)
  )
  
  ## Return proto
  return(do.call(.geom_func, identity_args))
}



#' Tour proto highlighing specified points
#'
#' A `geom_point` or `geom_segment`(1d case) call to draw attention to a subset 
#' of points. This is mostly redundant `proto_point` with the implementation 
#' of the `row_index` argument on data protos, still helpful in the 1d case and
#' for `mark_initial`, does not use bkg_row_color
#'
#' @param aes_args A list of aesthetic arguments to passed to 
#' `geom_point(aes(X)`. Any mapping of the data to an aesthetic,
#' for example, `geom_point(aes(color = myCol, shape = myCol))` becomes
#' `aes_args = list(color = myCol, shape = myCol)`.
#' @param identity_args A list of static, identity arguments passed into 
#' `geom_point()`, but outside of `aes()`, for instance 
#' `geom_point(aes(...), size = 2, alpha = .7)` becomes 
#' `identity_args = list(size = 2, alpha = .7)`.
#' #' Typically a single numeric for point size, alpha, or similar.
#' @param row_index A numeric or logical index of rows to subset to. 
#' Defaults to TRUE, all observations.
#' @param mark_initial Logical, whether or not to leave a fainter mark at the 
#' subset's initial position. Defaults to FALSE.
#' @export
#' @aliases proto_highlight_2d
#' @family ggtour proto functions
#' @examples
#' library(spinifex)
#' dat     <- scale_sd(flea[, 1:6])
#' clas    <- flea$species
#' gt_path <- save_history(dat, grand_tour(), max_bases = 5)
#' 
#' ## d = 2 case
#' ggt <- ggtour(gt_path, dat, angle = .3) +
#'   proto_default(list(color = clas, shape = clas)) +
#'   proto_highlight(row_index = 5)
#' \dontrun{
#' animate_plotly(ggt)
#' }
#' 
#' ## Highlight multiple observations
#' ggt2 <- ggtour(gt_path, dat, angle = .3) +
#'   proto_default(list(color = clas, shape = clas)) +
#'   proto_highlight(row_index = c( 2, 6, 19),
#'                   identity_args = list(color = "blue", size = 4, shape = 4))
#' \dontrun{
#' animate_plotly(ggt2)
#' }
proto_highlight <- function(
  aes_args = list(),
  identity_args = list(color = "red", size = 5, shape = 8),
  row_index = 1,
  mark_initial = FALSE
){
  if(length(row_index) == 0L) stop("proto_highlight: length of row_index is 0.")
  if(sum(row_index) == 0L) return() ## Early out adding nothing
  ## Initialize
  eval(.init4proto) ## aes_args/identity_args/df_data subset in .init4proto.
  if(is.null(.df_data) == TRUE) stop("Data is NULL, proto not applicable.")
  if(is.null(.df_data$y))
    stop("proto_highlight: Projection y not found, expecting a 2D tour. Did you mean to call `proto_highlight1d`?")
  
  ## do.call aes() over the aes_args
  .aes_func <- function(...)
    ggplot2::aes(x = x, y = y, frame = frame, tooltip = label, ...) ## rownum for tooltip
  .aes_call <- do.call(.aes_func, aes_args)
  ## do.call geom_point() over the identity_args
  .geom_func <- function(...) suppressWarnings(ggplot2::geom_point(
    mapping = .aes_call, data = .df_data, ...))
  ret <- do.call(.geom_func, identity_args)
  
  ## Initial mark, if needed, hard-coded some aes, no frame.
  if(mark_initial == TRUE){
    .aes_func <- function(...)
      ggplot2::aes(x = x, y = y, ...)
    .aes_call <- do.call(.aes_func, aes_args)
    ## do.call geom_vline over highlight obs
    .geom_func <- function(...) suppressWarnings(ggplot2::geom_point(
      mapping = .aes_call, .df_data[1L, ], ## only the first row, should be frame 1.
      ..., alpha = .5)) ## Hard-coded alpha
    inital_mark <- do.call(.geom_func, identity_args[row_index])
    ret <- list(inital_mark, ret)
  }
  
  ## Return proto
  return(ret)
}


#' @rdname proto_highlight
#' @export
#' @family ggtour proto functions
#' @examples
#' ## 1D case:
#' gt_path1d <- save_history(dat, grand_tour(d = 1), max_bases = 3)
#' 
#' ggt <- ggtour(gt_path1d, dat, angle = .3) +
#'   proto_default1d(list(fill = clas, color = clas)) +
#'   proto_highlight1d(row_index = 7)
#' \dontrun{
#' animate_plotly(ggt)
#' }
#' 
#' ## Highlight multiple observations, mark_initial defaults to off
#' ggt2 <- ggtour(gt_path1d, dat, angle = .3) +
#'   proto_default1d(list(fill = clas, color = clas)) +
#'   proto_highlight1d(row_index = c(2, 6, 7),
#'                     identity_args = list(color = "green", linetype = 1))
#' \dontrun{
#' animate_plotly(ggt2)
#' }
proto_highlight1d <- function(
  aes_args = list(),
  identity_args = list(color = "red", linetype = 2, alpha = .9),
  row_index = 1,
  mark_initial = FALSE
){
  ## Initialize
  eval(.init4proto)
  if(is.null(last_ggtour()$df_data) == TRUE) return()
  .center <- map_relative(data.frame(x = 0L, y = 0L), "center", .map_to_density)
  
  ## geom_segment do.calls, moving with frame
  .ymin <- min(.map_to_density[, 2L])
  .ymax <- max(.map_to_density[, 2L])
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


### Guides & QoL Protos -----
#' Tour proto for frames square correlation
#'
#' Adds text to the animation, the frame and its specified correlation.
#'
#' @param position Vector x and y position relative to the unit data position; 
#' (0, 1) in each direction. Defaults to c(.7, -.1).
#' @param tail_size How long the origin mark should extended
#' relative to the observations. Defaults to .05, 5% of the projection space.
#' @param aes_args A list of aesthetic arguments to passed to 
#' `geom_point(aes(X)`. Any mapping of the data to an aesthetic,
#' for example, `geom_point(aes(color = myCol, shape = myCol))` becomes
#' `aes_args = list(color = myCol, shape = myCol)`.
#' @param identity_args A list of static, identity arguments passed into 
#' `geom_point()`, but outside of `aes()`, for instance 
#' `geom_point(aes(...), size = 2, alpha = .7)` becomes 
#' `identity_args = list(size = 2, alpha = .7)`.
#' @param ... Optionally, pass additional arguments to 
#' \code{\link[stats:cor]{stats::cor}}, specifying the type of
#' within frame correlation.
#' @seealso \code{\link[stats:cor]{stats::cor}}
#' @export
#' @examples
#' library(spinifex)
#' dat     <- scale_sd(flea[, 1:6])
#' clas    <- flea$species
#' gt_path <- save_history(dat, grand_tour(), max_bases = 5)
#' 
#' ggt <- ggtour(gt_path, dat, angle = .3) +
#'   proto_default(list(color = clas, shape = clas)) +
#'   proto_frame_cor2(position = c(.5, 1.1))
#' \dontrun{
#' animate_plotly(ggt)
#' }
proto_frame_cor2 <- function(
  aes_args = list(),
  identity_args = list(size = 4),
  row_index = TRUE,
  #stat2d = stats::cor, ## hardcoded stats::cor atm
  position = c(.7, -.1),
  ... ## passed to stats::cor
){
  ## Initialize
  eval(.init4proto)
  if(is.null(.df_data) == TRUE)
    stop("proto_frame_stat: Data is NULL; stat not applicable. Was data passed to the basis array or ggtour?")
  
  ## Find aggregated values, stat within the frame
  .agg <- .df_data %>%
    dplyr::group_by(frame) %>%
    dplyr::summarise(value = round(stats::cor(x, y, ...)^2L, 2L)) %>%
    dplyr::ungroup()
  
  ## Set position
  .x_ran <- range(.df_data[, 1L])
  .x_dif <- diff(.x_ran)
  .y_ran <- range(.df_data[, 2L])
  .y_dif <- diff(.y_ran)
  .x <- min(.x_ran) + position[1L] * .x_dif
  .y <- min(.y_ran) + position[2L] * .y_dif
  ## Prefix text:
  # ## Removes namespace; ie. 'stats::cor' to 'cor'
  # .stat_nm  <- substitute(stat2d)
  # .last_pos <- regexpr("\\:[^\\:]*$", s) + 1L
  # .stat_nm  <- substr(.stat_nm, .last_pos, nchar(.stat_nm))
  .stat_nm <- "cor^2"
  ## Create df with position and string label
  .txt_df <- data.frame(
    x = .x,
    y = .y,
    frame = .agg$frame,
    label = paste0(#"frame: ", .agg$frame, ", ", ## frame is redundant
                   .stat_nm, ": ",
                   sprintf("%3.2f", .agg$value)))
  if(.is_faceted == TRUE) ## Append first facet_var value.
    .txt_df <- data.frame(.txt_df, facet_var = .facet_var[1L])
  
  ## do.call aes() over the aes_args
  .aes_func <- function(...)
    ggplot2::aes(x = x, y = y, frame = frame, label = label, ...)
  .aes_call <- do.call(.aes_func, aes_args)
  ## do.call geom_point() over the identity_args
  .geom_func <- function(...)suppressWarnings(
    ggplot2::geom_text(mapping = .aes_call, data = .txt_df, ...))
  
  ## Return
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
#' @family ggtour proto functions
#' @examples
#' dat  <- scale_sd(flea[, 1:6])
#' clas <- flea$species
#' 
#' ## 2D case:
#' gt_path <- save_history(dat, grand_tour(), max_bases = 5)
#' ggt <- ggtour(gt_path, dat, angle = .1) +
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
  .center <- map_relative(data.frame(x = 0L, y = 0L), "center", .map_to_data)
  .min    <- min(.map_to_data[, 1L:2L])
  .max    <- max(.map_to_data[, 1L:2L])
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
#' @family ggtour proto functions
#' @examples
#' 
#' ## 1D case:
#' gt_path1d <- save_history(dat, grand_tour(d = 1), max_bases = 5)
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
  .center <- map_relative(data.frame(x = 0L, y = 0L), "center", .map_to_density)
  .ymin <- min(.map_to_density[, 2L])
  .ymax <- max(.map_to_density[, 2L])
  .tail <- diff(c(.ymin, .ymax)) * .6
  .df_origin <- data.frame(
    x     = c(.center[, 1L], .center[, 1L]),
    x_end = c(.center[, 1L], .center[, 1L]),
    y     = c(.center[, 2L], .center[, 2L]),
    y_end = c(.center[, 2L] - .tail, .center[, 2L] + .tail))
  
  ## do.call geom_segment() over the identity_args
  .geom_func <- function(...)
    ggplot2::geom_segment(
      ggplot2::aes(x = x, y = y, xend = x_end, yend = y_end),
      data = .df_origin, ...)
  ## Return
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
#' @param ... Optionally pass additional arguments to `proto_point` or 
#' `proto_density`.
#' @export
#' @aliases proto_default2d, proto_def, proto_def2d
#' @family ggtour proto functions
#' @examples
#' dat  <- scale_sd(flea[, 1:6])
#' clas <- flea$species
#' 
#' ## 2D case:
#' bas     <- basis_pca(dat)
#' mv      <- manip_var_of(bas)
#' mt_path <- manual_tour(bas, mv)
#' 
#' ggt <- ggtour(mt_path, dat) +
#'   proto_default(list(color = clas, shape = clas),
#'                 list())
#' \dontrun{
#' animate_plotly(ggt)
#' }
proto_default <- function(
  aes_args = list(),
  identity_args = list(alpha = .9),
  ...
){
  return(list(
    proto_point(aes_args, identity_args, ...),
    proto_basis(),
    proto_origin()
  ))
}


#' @rdname proto_default
#' @export
#' @aliases proto_def1d
#' @family ggtour proto functions
#' @examples
#' ## 1D case:
#' gt_path <- save_history(dat, grand_tour(d = 1), max_bases = 3)
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
    proto_basis1d(),
    proto_origin1d()
  ))
}




### UNAPPLIED IDEA DRAFTS -----
if(FALSE){ ## DONT RUN
  proto_basis_table <- function(){}
  proto_chull <- function(){}
  proto_ahull <- function(){}
  
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