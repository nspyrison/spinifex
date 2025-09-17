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
#' @param angle Target angle (radians) for interpolation frames between 
#' frames of the `basis_array`. Defaults to .05. 
#' To opt out of interpolation set to NA or 0.
#' @param basis_label `plotly` tooltip labels for the basis. 
#' Defaults to NULL; 3 character abbreviation of the rownames of basis.
#' @param data_label `plotly` tooltip labels for the data. 
#' Defaults to the NULL, rownames and/or numbers of data.
#' @param do_center_frame Whether or not to center the mean within each 
#' animation frame. Defaults to TRUE.
#' @export
#' @family ggtour proto functions
#' @examples
#' library(spinifex)
#' dat     <- scale_sd(penguins_na.rm[, 1:4])
#' clas    <- penguins_na.rm$species
#' bas     <- basis_pca(dat)
#' mv      <- manip_var_of(bas)
#' mt_path <- manual_tour(bas, manip_var = mv)
#' 
#' ## d = 2 case
#' ggt <- ggtour(basis_array = mt_path, data = dat, angle = .3) +
#'   proto_default(aes_args = list(color = clas, shape = clas),
#'                 identity_args = list(size = 1.5, alpha = .8))
#' \donttest{
#' animate_plotly(ggt)
#' }
#' 
#' ## Finer control calling individual proto_* functions
#' ggt <- ggtour(basis_array = mt_path, data = dat, angle = .3) +
#'   proto_point(aes_args = list(color = clas, shape = clas),
#'               identity_args = list(size = 1.5, alpha = .8),
#'               row_index = which(clas == levels(clas)[1])) +
#'   proto_basis(position = "right",
#'               manip_col = "red",
#'               text_size = 7L) +
#'   proto_origin()
#' \donttest{
#' animate_plotly(ggt)
#' }
#' 
#' ## d = 1 case
#' bas1d <- basis_pca(dat, d = 1)
#' mt_path1d <- manual_tour(basis = bas1d, manip_var = mv)
#' 
#' ggt1d <- ggtour(basis_array = mt_path1d, data = dat, angle = .3) +
#'   proto_default1d(aes_args = list(fill= clas, color = clas))
#' \donttest{
#' animate_plotly(ggt1d)
#' }
#' 
#' ## Single basis
#' ggt <- ggtour(basis_array = bas, data = dat) +
#'   proto_default(aes_args = list(fill= clas, color = clas))
#' ## ggtour() returns a static ggplot2 plot
#' \donttest{
#' ggt
#' ### or as html widget with tooltips
#' animate_plotly(ggt)
#' }
ggtour <- function(
  basis_array,
  data            = NULL,
  angle           = .05,
  basis_label     = NULL,
  data_label      = NULL,
  do_center_frame = TRUE
){
  ## If data missing, check if data is passed to the basis_array
  if(is.null(data)) data <- attr(basis_array, "data") ## Could be NULL
  .phi_start <- attr(basis_array,"phi_start")  ## NULL if not a manual tour
  .manip_var <- attr(basis_array, "manip_var") ## For highlighting indepentant of 
  ## Basis label condition handling
  if(is.null(basis_label)){
    if(is.null(data) == FALSE){
      basis_label <- abbreviate(
        gsub("[^[:alnum:]=]", "", colnames(data), 3L))
    }else basis_label <- abbreviate(
      gsub("[^[:alnum:]=]", "", rownames(basis_array), 3L))
    if(length(basis_label) == 0L) basis_label <- paste0("v", 1L:nrow(basis_array))
  }
  ## Data label condition handling
  if(is.null(data) == FALSE &
     suppressWarnings(any(is.na(as.numeric(as.character(rownames(data)))))))
    data_label <- paste0("row: ", 1L:nrow(data), ", ", rownames(data))
  ## Single basis matrix, coerce to array
  if(length(dim(basis_array)) == 2L) ## AND 1 basis.
    basis_array <- array(
      as.matrix(basis_array), dim = c(dim(basis_array), 1L))
  # Interpolation frames
  if(is.null(angle) | is.na(angle) | angle == 0L){
    ## Opt out of interpolate, angle was na, null, or 0, esp for 
    .interpolated_basis_array <- basis_array
  }else if(is.null(.phi_start) == FALSE){
    ## manual tours
    .interpolated_basis_array <- interpolate_manual_tour(basis_array, angle)
  }else if(is.null(.phi_start)){ ## if manip_var is null; IE. from tourr
    ## tourr tours
    .m <- utils::capture.output(
      .interpolated_basis_array <- tourr::interpolate(basis_array, angle))
  }
  
  ## df_basis & df_data list
  .df_ls <- array2df(
    .interpolated_basis_array, data, basis_label, data_label, do_center_frame)
  .df_basis <- .df_ls$basis_frames
  attr(.df_basis, "manip_var") <- .manip_var ## NULL if not a manual tour
  .n_frames     <- dim(.interpolated_basis_array)[3L]
  .d            <- ncol(.interpolated_basis_array)
  .p            <- nrow(.interpolated_basis_array)
  .map_to       <- data.frame(x = c(0L, 1L), y = c(0L, 1L))
  .df_data      <- .df_ls$data_frames ## Can be NULL
  .nrow_df_data <- nrow(.df_data) ## NULL if data is NULL
  .n            <- nrow(data) ## NULL if data is NULL
  
  ## SIDE EFFECT: Assign list to last_ggtour_env().
  .set_last_ggtour_env(list(
    interpolated_basis_array = .interpolated_basis_array,
    df_basis = .df_basis, df_data = .df_data, map_to = .map_to,
    n_frames = .n_frames, nrow_df_data = .nrow_df_data, n = .n, p = .p,
    d = .d, manip_var = .manip_var, is_faceted = FALSE))
  ## Return ggplot head, theme, and facet if used
  ggplot2::ggplot(.df_basis) + theme_spinifex() +
    ggplot2::labs(x = NULL, y = NULL, color = NULL, shape = NULL, fill = NULL) +
    ggplot2::coord_fixed(clip = "off") ## aspect.ratio = 1L fixes unit size, not axes size
}
## Print method for ggtours ?? using proto_default()
#### Was a good idea, but ggplot stops working when you change the first class, 
#### and doesn't effect if you change the last class.


#' Wrap a 1d ribbon of panels into 2d for animation
#' 
#' Create and wrap a 1d ribbon of panels in 2d. 
#' Because of the side effects of `ggtour` and `facet_wrap_tour` this wants to be 
#' applied after `ggtour` and before any `proto_*` functions.
#' `plotly` may not display well with with faceting.
#' 
#' @param facet_var Expects a single variable to facet the levels of. 
#' Should be a vector, not a formula (`~cyl`) or `ggplot2::vars()` call.
#' @param nrow Number of rows. Defaults to NULL; set by display dim.
#' @param ncol Number of columns. Defaults to NULL; set by display dim.
#' @param dir Direction of wrapping: either "h" horizontal by rows, 
#' or "v", for vertical by columns. Defaults to "h".
#' @export
#' @family ggtour proto functions
#' @examples
#' library(spinifex)
#' dat     <- scale_sd(penguins_na.rm[, 1:4])
#' clas    <- penguins_na.rm$species
#' bas     <- basis_pca(dat)
#' mv      <- manip_var_of(bas)
#' mt_path <- manual_tour(bas, manip_var = mv)
#' 
#' ## d = 2 case
#' message("facet_wrap_tour wants be called early, so that other proto's adopt the facet_var.")
#' ggt <- ggtour(mt_path, dat, angle = .3) +
#'   facet_wrap_tour(facet_var = clas, ncol = 2, nrow = 2) +
#'   proto_default(aes_args = list(color = clas, shape = clas),
#'                 identity_args = list(size = 1.5))
#' \dontrun{
#' animate_gganimate(ggt) ## May not always play well with plotly
#' }
facet_wrap_tour <- function(
  facet_var, nrow = NULL, ncol = NULL, dir = "h"
){
  eval(.init4proto)
  ## Append facet_var to df_basis and df_data if needed.
  if(is.null(facet_var) == FALSE){
    if(is.null(.df_data) == FALSE)
      .df_data <- .bind_elements2df(
        list(facet_var = rep_len(facet_var, .nrow_df_data)), .df_data)
    ## "_basis_" becomes an honorary level of facet_var
    .df_basis  <- .bind_elements2df(list(facet_var = "_basis_"), .df_basis)
  }
  
  ## SIDE EFFECT:
  #### Changes: .df_basis & .df_data have facet_var bound, is_faceted = TRUE
  .ggt$df_basis   <- .df_basis
  .ggt$df_data    <- .df_data
  .ggt$facet_var  <- facet_var
  .ggt$is_faceted <- TRUE
  .set_last_ggtour_env(.ggt)
  
  ## Return
  list(
    ggplot2::facet_wrap(facets = ggplot2::vars(facet_var),
                        nrow = nrow, ncol = ncol, dir = dir),
    ggplot2::theme( ## Note; strip spacing and position in theme_spinifex()
      ## Border introduced only with facet. 
      panel.border = element_rect(linewidth = .4, color = "grey20", fill = NA))
  )
}

#' Append a fixed vertical height
#' 
#' Adds/overwrites the y of the projected data. Usefully for 1D projections and
#' appending information related to, but independent from the projection; 
#' model predictions or residuals for instance. 
#' Wants to be called early so that the following proto calls adopt the changes.
#' 
#' @param fixed_y Vector of length of the data, values to fix vertical height.
#' Typically related to but not an explanatory variable, for instance,
#' predicted Y, or residuals.
#' @export
#' @family ggtour proto functions
#' @examples
#' library(spinifex)
#' dat     <- scale_sd(penguins_na.rm[, 1:4])
#' clas    <- penguins_na.rm$species
#' bas     <- basis_pca(dat)
#' mv      <- manip_var_of(bas)
#' mt_path <- manual_tour(bas, manip_var = mv)
#' 
#' # Fixed y height with related information, independent of a 1D tour 
#' # _eg_ predictions or residuals.
#' message("don't forget to scale your fixed_y.")
#' dummy_y <- scale_sd(as.integer(clas) + rnorm(nrow(dat), 0, .5))
#' gt_path <- save_history(dat, grand_tour(d = 1), max_bases = 5)
#' 
#' message("append_fixed_y wants to be called early so other proto's adopt the fixed_y.")
#' ggt <- ggtour(gt_path, dat, angle = .3) +
#'   append_fixed_y(fixed_y = dummy_y) + ## insert/overwrites vertical values.
#'   proto_point(list(fill = clas, color = clas)) +
#'   proto_basis1d() +
#'   proto_origin()
#' \donttest{
#' animate_plotly(ggt)
#' }
append_fixed_y <- function(
  fixed_y
){
  ## Initialize
  eval(.init4proto)
  
  ## Add fixed y
  .df_data$y <- rep_len(fixed_y, .nrow_df_data)
  .df_data$y <- .df_data$y - min(.df_data$y)
  
  ## SIDE EFFECT: pass data back to .store
  # to calm some oddities; like proto_origin() complaining about y being missing
  .ggt$df_data  <- .df_data
  .ggt$map_to$y <- range(.df_data$y)
  .ggt$d        <- 2L
  .set_last_ggtour_env(.ggt)
  
  ## Return
  NULL
}


.store <- new.env(parent = emptyenv())
#' Retrieve/set last `ggtour()` information
#' 
#' Internal information, not related to the ggplot2 object, but used in ggtour
#' composition by the `proto_*` functions to share/set changes.
#'
#' @seealso [ggtour()]
# #' @export
#' @keywords internal
last_ggtour_env <- function(){.store$ggtour_ls}
#' @rdname last_ggtour_env
# #' @export
.set_last_ggtour_env <- function(ggtour_list) .store$ggtour_ls <- ggtour_list


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
        warning(paste0(
          ".lapply_rep_len: argument `", .nms[i], "` (length = ", length(.elem), 
          ") not of length 1 or data (nrow = ", expected_length,
          "); liable to cause cycling issues. Should it be of length 1 or data?"
        ))
      ret_vect <- rep_len(.elem, to_length)
    }else ret_vect <- .elem
    list[[i]] <<- ret_vect
  })
  list
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
  .list    <- as.list(list)
  .ret     <- as.data.frame(df)
  .ret_nms <- names(.ret)
  .l_nms   <- names(list)
  .m <- lapply(seq_along(.list), function(i){
    .ret <<- cbind(.ret, .list[[i]])
  })
  names(.ret) <- c(.ret_nms, .l_nms)
  .ret
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
.init4proto <- expression({ ## An expression, not a function
  .ggt <- spinifex:::last_ggtour_env() ## Self-explicit for use in cheem
  if(is.null(.ggt)) stop(".init4proto: spinifex:::last_ggtour_env() is NULL, have you run ggtour() yet?")
  
  ## Assign elements ggtour list as quiet .objects in the environment
  .env <- environment()
  .nms <- names(.ggt)
  .m <- sapply(seq_along(.ggt), function(i){
    assign(paste0(".", .nms[i]), .ggt[[i]], envir = .env)
  })
  
  ## row_index, if exists
  if(exists("row_index")){
    if(is.null(row_index) == FALSE){
      ## Coerce index to full length logical index
      if(is.numeric(row_index)){
        .rep_f <- rep(FALSE, .n)
        .rep_f[row_index] <- TRUE
        row_index <- .rep_f
      }
      ### Background:
      if(exists("bkg_color")) ## Only proto_point atm
        if(sum(!row_index) > 0L)
          if(is.null("bkg_color") == FALSE)
            if(bkg_color != FALSE){
              .bkg_aes_args <- .bkg_identity_args <- list()
              #### Subset .df_data_bkg
              .df_data_bkg <- .df_data[rep(!row_index, .n_frames),, drop = FALSE]
              #### Subset (but not replicate) .bkg_aes_args, bkg_identity_args:
              if(exists("aes_args"))
                if(length(aes_args) > 0L)
                  .bkg_aes_args <- lapply(aes_args, function(arg)arg[!row_index])
              if(exists("identity_args"))
                if(length(identity_args) > 0L)
                  .bkg_identity_args <- lapply(identity_args, function(arg)
                    if(length(arg) == .n) arg[!row_index] else arg)
            }
      
      ### Foreground:
      #### Subset (but not replicate) aes_args, identity_args
      if(exists("aes_args"))
        if(length(aes_args) > 0L)
          aes_args <- lapply(aes_args, function(arg)arg[row_index])
      if(exists("identity_args"))
        if(length(identity_args) > 0L)
          identity_args <- lapply(identity_args, function(arg)
            if(length(arg) == .n) arg[row_index] else arg)
      
      #### Subset .df_data, update .n & .nrow_df_data
      .df_data      <- .df_data[rep(row_index, .n_frames),, drop = FALSE]
      .n            <- sum(row_index) ## n rows _slecected_
      .nrow_df_data <- nrow(.df_data)
    }
  } ## end row_index, if exists
  
  ##Possible dev: to fix the legend issue of color and shape mapped to class:
  # would need to bind args to the .df_data, and then remake a true aes(), 
  # based on the names of the lists. will need to quote/or symb, probably. 
  # .df_data <- .bind_elements2df(aes_args, .df_data)
  # aes_args <- ## TODO>>>>, go to aes_string("mpg") or aes_(quote(mpg))?
  
  ## Replicate argument lists, if they exist
  if(exists("row_index"))
    if(sum(row_index) != length(row_index)){
      if(exists(".bkg_aes_args"))
        if(length(.bkg_aes_args) > 0L)
          .bkg_aes_args <- spinifex:::.lapply_rep_len(
            .bkg_aes_args, nrow(.df_data_bkg), sum(!row_index))
      if(exists(".bkg_identity_args"))
        if(length(identity_args) > 0L)
          .bkg_identity_args <- spinifex:::.lapply_rep_len(
            .bkg_identity_args, nrow(.df_data_bkg), sum(!row_index))
    }
  if(exists("aes_args"))
    if(length(aes_args) > 0L)
      aes_args      <- spinifex:::.lapply_rep_len(aes_args, .nrow_df_data, .n)
  if(exists("identity_args"))
    if(length(identity_args) > 0L)
      identity_args <- spinifex:::.lapply_rep_len(identity_args, .nrow_df_data, .n)
})

### ANIMATE_* ------

#' Animate a ggtour as a .gif via `{gganimate}`
#'
#' Animates the ggplot return of `ggtour()` and added `proto_*()` functions as a 
#' .gif without interaction, through use of `{gganimate}`.
#'
#' @param ggtour A grammar of graphics tour with appended protos added. 
#' A return from `ggtour() + proto_*()`.
#' @param fps Number of Frames Per Second, the speed resulting animation.
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
#' dat     <- scale_sd(penguins_na.rm[, 1:4])
#' clas    <- penguins_na.rm$species
#' bas     <- basis_pca(dat)
#' mv      <- manip_var_of(bas)
#' mt_path <- manual_tour(bas, manip_var = mv)
#' 
#' ggt <- ggtour(mt_path, dat, angle = .3) +
#'   proto_default(aes_args = list(color = clas, shape = clas),
#'                 identity_args = list(size = 1.5, alpha = .7))
#' \dontrun{
#' ## Default .gif rendering
#' animate_gganimate(ggt)
#' 
#' if(FALSE){ ## Don't accidentally save file
#'   ## Option arguments, rendering to default .gif
#'   anim <- animate_gganimate(
#'     ggt, fps = 10, rewind = TRUE,
#'     start_pause = 1, end_pause = 2,
#'     height = 10, width = 15, units = "cm", ## "px", "in", "cm", or "mm."
#'     res = 200 ## resolution, pixels per dimension unit I think
#'   )
#'   ## Save rendered animation
#'   gganimate::anim_save("my_tour.gif",
#'                        animation = anim,
#'                        path = "./figures")
#'   
#'   ## Alternative renderer saving directly to .mp4
#'   animate_gganimate(ggt, fps = 5,
#'     height = 4, width = 6, units = "in", ## "px", "in", "cm", or "mm."
#'     res = 200, ## resolution, pixels per dimension unit I think
#'     renderer = gganimate::av_renderer("./my_tour.mp4"))
#' }
#' }
animate_gganimate <- function(
  ggtour,
  fps = 8,
  rewind = FALSE,
  start_pause = 1,
  end_pause = 1,
  ... ## Passed to gganimate::animate
){
  if(requireNamespace("gifski", quietly = TRUE) == FALSE)
    stop("animate_gganimate: package 'gifski' was not found, please install to render to gif with animate_gganimate")
  
  
  ## Early out, print ggplot if only 1 frame.
  if(length(ggtour$layers) == 0L) stop("No layers found, did you forget to add a proto_*?")
  n_frames <- last_ggtour_env()$n_frames
  if(n_frames == 1L){
    ## Static ggplot2, 1 frame
    message("ggtour df_basis only has 1 frame, returning ggplot2 object instead.")
    return(ggtour)
    ## return(), don't try to send to clean up; 
    #### Being in if/else env makes transition_states think frame is
    #### graphics::frame (causes error) and not vars(frame). 
    #### Using ggtour$data$frame runs, but all data points show in each frame.
  }
  # Animate
  ## Discrete jump between frames, no linear interpolation.
  gga <- ggtour + gganimate::transition_states(frame, transition_length = 0L)
  ## Normal animation, with applied options, knit_pdf_anim == FALSE
  gganimate::animate(gga, fps = fps, rewind = rewind, 
                     start_pause = fps * start_pause,
                     end_pause = fps * end_pause, ...)
}


#' Animate a ggtour as and HTML widget via `{plotly}`
#'
#' Animates the static `ggtour()` and added `proto_*()` functions as a 
#' `{plotly}` animation, an .html widget with slider and hover tooltip showing 
#' the row number.
#'
#' @param ggtour A grammar of graphics tour with appended protos added.
#' A return from `ggtour() + proto_*()`.
#' @param fps Number of Frames Per Second, the speed resulting animation.
#' @param ... Other arguments passed to 
#' \code{\link[plotly:ggplotly]{plotly::ggplotly}}.
#' @export
#' @family ggtour animator
#' @examples
#' library(spinifex)
#' dat     <- scale_sd(penguins_na.rm[, 1:4])
#' clas    <- penguins_na.rm$species
#' bas     <- basis_pca(dat)
#' mv      <- manip_var_of(bas)
#' mt_path <- manual_tour(bas, manip_var = mv)
#' 
#' ggt <- ggtour(mt_path, dat, angle = .3) +
#'   proto_origin() +
#'   proto_basis() +
#'   proto_point(aes_args = list(color = clas, shape = clas),
#'               identity_args = list(size = 1.5, alpha = .7))
#' \donttest{
#' animate_plotly(ggt, width = 700, height = 450) ## pixels only, no resolution argument
#' 
#' ## Example saving to a .html widget, may require additional setup.
#' if(FALSE){
#'   anim <- animate_plotly(ggt, fps = 10,
#'                          width = 700, height = 450) ## in pixels
#'   
#'   htmlwidgets::saveWidget(widget = anim,
#'                           file = "./figures/my_tour.html",
#'                           selfcontained = TRUE)}
#' }
animate_plotly <- function(
  ggtour,
  fps = 8,
  ... ## Passed to plotly::ggplotly(). can always call layout/config again
){
  if(is_any_layer_class(ggtour, "GeomTextRepel"))
    stop("geom_text_repel not implemented in plotly, try animate_gganimate().")
  
  ## If ggplot
  if(length(ggtour$layers) == 0L) ## plotly subplots, have NULL layers
    stop("No layers found, did you forget to add a proto_*?")
  ## Frame asymmetry issue: https://github.com/ropensci/plotly/issues/1696
  #### Adding many protos is liable to break plotly animations, see above url.
  ggtour <- ggtour + ggplot2::theme(
    ## Avoid plotly warnings
    legend.direction = "vertical", ## horizontal legends not supported
    aspect.ratio     = NULL)       ## aspect.ratio not supported
  ## ggplotly without animation settings
  ggp <- plotly::ggplotly(ggtour, tooltip = "tooltip", ...)
  ## If density used widen
  if(is_any_layer_class(ggtour, class_nm = "GeomDensity"))
    #<add plotly aspect ratio to ggp>
    ggp <- plotly::layout(
      ggp, xaxis = list(scaleratio = 2)) ## 2x width
  
  
  ## ggplotly settings, animated or static from ggplot or subplot
  ggp <- ggp %>%
    ## Remove button bar and zoom box
    plotly::config(displayModeBar = FALSE,
                   modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d")) %>%
    ## Remove legends and axis lines
    plotly::layout(dragmode = FALSE, legend = list(x = 100L, y = 0.5),
                   #fixedrange = TRUE, ## This is a curse, never use it.
                   yaxis = list(showgrid = FALSE, showline = FALSE),
                   xaxis = list(showgrid = FALSE, showline = FALSE))
                                #scaleanchor = "y", scalaratio = 1L
  
  ## Multiple frames/animation condition handling
  n_frames <- last_ggtour_env()$n_frames
  if(n_frames == 1L){
    ## Static ggplot, 1 Frame only or possibly last_ggtour_env missing:
    message("ggtour df_basis only has 1 frame, no animation options.")
    ret <- ggp
  }else{
    ## Animation options, more than 1 frame
    ## Block plotly.js warning: lack of support for horizontal legend;
    #### https://github.com/plotly/plotly.js/issues/53
    ret <- ggp %>% plotly::animation_opts(
      frame = 1L / fps * 1000L, transition = 0L, redraw = TRUE) %>%
      plotly::animation_slider(
        active = 0L, ## 0 indexed first frame
        currentvalue = list(prefix = "Frame: ", font = list(color = "black")))
  }
  ret
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
# #' @param fps Number of Frames Per Second, the speed resulting animation.
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
# #' dat     <- scale_sd(penguins_na.rm[, 1:4])
# #' clas    <- penguins_na.rm$species
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
#   n_frames <- length(unique(last_ggtour_env()$df_basis$frame))
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
#' the tour. Is very demanding on the plots pane, works better with ggsave().
#' 
#' @param ggtour A grammar of graphics tour with appended protos added. 
#' A return from `ggtour() + proto_*()`
#' @param ... optionally pass arguments to ggplot2::facet_wrap, such as
#' nrow = 3, ncol = 2, scales = "free".
#' @export
#' @family ggtour animator
#' @examples
#' library(spinifex)
#' dat  <- scale_sd(penguins_na.rm[, 1:4])
#' clas <- penguins_na.rm$species
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
#' ## d = 1 case & specify facet dim
#' bas1d     <- basis_pca(dat, d = 1)
#' mt_path1d <- manual_tour(basis = bas1d, manip_var = mv)
#' ggt1d <- ggtour(mt_path1d, dat, angle = 99) +
#'   proto_default1d(aes_args = list(fill = clas, color = clas))
#' filmstrip(ggt1d, nrow = 12, ncol = 3)
filmstrip <- function(
  ggtour, ...
){
  ggtour +
    ## Display level of previous facet (if applicable) next level of frame.
    ggplot2::facet_wrap(c("frame", names(ggtour$facet$params$facets)), ...) +
    ggplot2::theme( ## Note; strip spacing and position in theme_spinifex()
      ## Border introduced only with facet.
      panel.border = element_rect(linewidth = .4, color = "grey20", fill = NA))
}


### BASIS Protos ------
#' Tour proto for a 2D and 1D basis axes respectively
#'
#' Adds basis axes to the animation, the direction and magnitude of 
#' contributions of the variables to the projection space inscribed in a unit 
#' circle for 2D or rectangle of unit width for 1D.
#'
#' @param position The position, to place the basis axes relative to the 
#' data. `proto_basis` expects one of c("left", "center", "right", "bottomleft", "topright", 
#' "off"), defaults to "left". `proto_basis1d` expects one of 
#' c("bottom1d", "floor1d", "top1d", "off"). Defaults to "bottom1d".
#' @param manip_col The color to highlight the manipulation variable with. Not
#' applied if the tour isn't a manual tour. Defaults to "blue".
#' @param line_size (2D bases only) the thickness of the lines used to make the 
#' axes and unit circle. Defaults to .6.
#' @param text_size Size of the text label of the variables. Defaults to 4.
#' @export
#' @aliases proto_basis
#' @family ggtour proto functions
#' @examples
#' library(spinifex)
#' dat  <- scale_sd(penguins_na.rm[, 1:4])
#' clas <- penguins_na.rm$species
#' bas  <- basis_pca(dat)
#' mv   <- manip_var_of(bas)
#' 
#' ## 2D case:
#' mt_path <- manual_tour(bas, manip_var = mv)
#' ggt <- ggtour(mt_path, dat, angle = .3) +
#'   proto_point() +
#'   proto_basis()
#' \donttest{
#' animate_plotly(ggt)
#' }
#' 
#' ## Customize basis
#' ggt2 <- ggtour(mt_path, dat) +
#'   proto_basis(position = "right", manip_col = "green",
#'               line_size = .8, text_size = 8)
#' \donttest{
#' animate_plotly(ggt2)
#' }
#' 
#' ## 1D case:
#' bas1d     <- basis_pca(dat, d = 1)
#' mv        <- manip_var_of(bas1d, 3)
#' mt_path1d <- manual_tour(bas1d, manip_var = mv)
#' 
#' ggt1d <- ggtour(mt_path1d, dat, angle = .3) +
#'   proto_density() +
#'   proto_basis1d()
#' \donttest{
#' animate_plotly(ggt1d)
#' }
#' 
#' ## Customized basis1d
#' ggt1d <- ggtour(mt_path1d, dat, angle = .3) +
#'   proto_density() +
#'   proto_basis1d(position     = "bottom",
#'                 manip_col    = "pink",
#'                 segment_size = 3,
#'                 text_size    = 6,
#'                 text_offset  = 1.2)
#' \donttest{
#' animate_plotly(ggt1d)
#' }
proto_basis <- function(
  position  = c("left", "center", "right", "bottomleft", "topright", "full", "off"),
  manip_col = "blue",
  line_size = .6,
  text_size = 4
){
  ## Initialize
  eval(.init4proto)
  if(is.null(.df_basis$y))
    stop("proto_basis: Basis `y` not found, expected a 2D tour. Did you mean to call `proto_basis1d`?")
  position <- match.arg(position)
  if(position == "off") return()
  
  ## Setup and transform
  .angles <- seq(0L, 2L * pi, length = 360L)
  .circle <- data.frame(x = cos(.angles), y = sin(.angles))
  if(.is_faceted){
    position <- "full"
    .circle  <- .bind_elements2df(list(facet_var = "_basis_"), .circle)
  }
  ## Scale to data
  .center    <- map_relative(data.frame(x = 0L, y = 0L), position, .map_to)
  .circle    <- map_relative(.circle, position, .map_to)
  .df_basis  <- map_relative(.df_basis, position, .map_to)
  
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
  list(
    ggplot2::geom_path(data = .circle, color = "grey80",
                       linewidth = line_size, inherit.aes = FALSE,
                       mapping = ggplot2::aes(x = x, y = y)),
    suppressWarnings(ggplot2::geom_segment( ## Suppress unused arg: frames
      data = .df_basis,
      size = .axes_siz, color = .axes_col,
      mapping = ggplot2::aes(x = x, y = y, frame = frame,
                             xend = .center$x, yend = .center$y)
    )),
    suppressWarnings(ggplot2::geom_text(
      data = .df_basis,
      color = .axes_col, size = text_size,
      mapping = ggplot2::aes(x = x, y = y, frame = frame, label = tooltip,
                             vjust = "outward", hjust = "inward")
    ))
  )
}


#' @rdname proto_basis
#' @param segment_size (1D bases only) the width thickness of the rectangle bar
#' showing variable magnitude on the axes. Defaults to 2.
#' @param text_offset The horizontal offset of the text labels relative to the
#' variable contributions in the basis between (-1, 1). Defaults to -1.15.
#' @export
proto_basis1d <- function(
  position     = c("bottom1d", "floor1d",  "top1d", "full", "off"),
  manip_col    = "blue",
  segment_size = 2,
  text_size    = 4,
  text_offset  = -1.15
){
  ## Initialize
  eval(.init4proto)
  position <- match.arg(position)
  if(position == "off") return()
  
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
                         y = rep(.p:1L, .n_frames) / .p,
                         frame = .df_basis$frame,
                         tooltip = .df_basis$tooltip)
  .df_txt  <- data.frame(x = text_offset, y = .p:1L/.p,
                         tooltip = .df_basis[.df_basis$frame == 1L, "tooltip"])
  .df_rect <- data.frame(x = c(-1L, 1L), y = c(.5, .p + .5) / .p)
  .df_seg0 <- data.frame(x = 0L, y = c(.5, .p + .5) / .p)
  if(.is_faceted){
    position   <- "floor1d"
    .facet_var <- list(facet_var = "_basis_")
    .df_zero   <- .bind_elements2df(.facet_var, .df_zero)
    .df_seg    <- .bind_elements2df(.facet_var, .df_seg)
    .df_txt    <- .bind_elements2df(.facet_var, .df_txt)
    .df_rect   <- .bind_elements2df(.facet_var, .df_rect)
    .df_seg0   <- .bind_elements2df(.facet_var, .df_seg0)
  }
  ## Scale them
  .df_zero <- map_relative(.df_zero, position, .map_to)
  .df_seg  <- map_relative(.df_seg,  position, .map_to)
  .df_txt  <- map_relative(.df_txt,  position, .map_to)
  .df_rect <- map_relative(.df_rect, position, .map_to) %>%
    dplyr::summarise(xmin = min(x), ymin = min(y), xmax = max(x), ymax = max(y))
  .df_seg0 <- map_relative(.df_seg0, position, .map_to) %>%
    dplyr::summarise(x = min(x), y = min(y), xend = max(x), yend = max(y))
  
  ## Return proto
  list(
    ## Middle line, grey, dashed
    ggplot2::geom_segment(
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      .df_seg0, color = "grey80", linetype = 2L),
    ## Outside rectangle, grey60, unit-width, (height = p+1)
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
      .df_rect, fill = NA, color = "grey60"),
    ## Variable abbreviation text
    ggplot2::geom_text(
      ggplot2::aes(x, y, label = tooltip, 
                   hjust = if(text_offset < 0L) 1L else 0L), 
      .df_txt, size = text_size, color = "grey60"),
    ## Contribution segments of current basis, changing with frame
    suppressWarnings(ggplot2::geom_segment(
      ggplot2::aes(x = .df_zero$x, y, xend = x, yend = y, frame = frame),
      .df_seg, color = .axes_col, size = .axes_siz))
  )
}




#' Draw a basis on a static ggplot
#' 
#' Additively draws a basis on a static ggplot.
#' Not a `geom` or `proto`. Expects 
#' 
#' @param basis A (p*d) basis to draw. Draws the first two components. 
#' If facet is used cbind the facet variable to a specific facet level 
#' (2nd example), otherwise the basis prints on all facet levels.
#' @param map_to A data.frame to scale the basis to. 
#' Defaults to a unitbox; data.frame(x = c(0,1), y = c(0,1)).
#' @param position The position, to place the basis axes relative to the centered 
#' data. `_basis` Expects one of c("left", "center", "right", "bottomleft", 
#' "topright", "off"), defaults to "left".
#' @param manip_col The color to highlight the manipulation variable with. Not
#' applied if the tour isn't a manual tour. Defaults to "blue".
#' @param line_size (2D bases only) the thickness of the lines used to make the 
#' axes and unit circle. Defaults to 0.6.
#' @param text_size Size of the text label of the variables. Defaults to 4.
#' @param basis_label The text labels of the data variables. 
#' Defaults to the 3 character abbreviation of the rownames of the basis.
#' @export
#' @examples
#' library(spinifex)
#' library(ggplot2)
#' dat  <- scale_sd(penguins_na.rm[, 1:4])
#' clas <- penguins_na.rm$species
#' bas  <- basis_pca(dat)
#' proj <- as.data.frame(dat %*% bas)
#' 
#' ggplot() +
#'   geom_point(aes(PC1, PC2), proj) +
#'   draw_basis(bas, proj, "left") +
#'   coord_fixed()
#'   
#' ## Aesthetics and basis on specific facet levels
#' proj <- cbind(proj, clas = penguins_na.rm$species)
#' bas <- cbind(as.data.frame(bas), clas = levels(clas)[2])
#' ggplot() +
#'   facet_wrap(vars(clas)) +
#'   geom_point(aes(PC1, PC2, color = clas, shape = clas), proj) +
#'   draw_basis(bas, proj, "left") +
#'   theme_spinifex()
#' ## To repeat basis in all facet levels don't cbind a facet variable.
draw_basis <- function(
  basis, ## WITH APPENDED FACET LEVEL
  map_to = data.frame(x = c(0, 1), y = c(0, 1)),
  position = c("left", "center", "right", "bottomleft", "topright", "off"),
  manip_col = "blue",
  line_size = .6,
  text_size = 4,
  basis_label = abbreviate(gsub("[^[:alnum:]=]", "", rownames(basis), 3L))
){
  ## Initialize
  d <- ncol(basis)
  if(d < 2L) stop("draw_basis: expects a basis of 2 or more columns.")
  position <- match.arg(position)
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
  
  if(is.null(.df_basis$tooltip)){
    tooltip <- abbreviate(gsub("[^[:alnum:]=]", "", rownames(basis), 3L))
    if(is.null(tooltip)) tooltip <- paste0("v", 1L:nrow(basis))
    .df_basis$tooltip <- tooltip
  }
  
  ## Aesthetics for the axes segments.
  .axes_col   <- "grey50"
  .axes_siz   <- line_size
  .manip_var  <- attr(basis, "manip_var")
  if(is.null(.manip_var) == FALSE){
    .axes_col <- rep("grey50", .p)
    .axes_col[.manip_var] <- manip_col
    .axes_col <- rep(.axes_col, .n_frames)
    .axes_siz <- rep(line_size, .p)
    .axes_siz[.manip_var] <- 1.5 * line_size
    .axes_siz <- rep(.axes_siz, .n_frames)
  }
  
  ## Return proto
  list(
    ggplot2::geom_path(data = .circle, color = "grey80",
                       size = line_size, inherit.aes = FALSE,
                       mapping = ggplot2::aes(x = x, y = y)),
    suppressWarnings(ggplot2::geom_segment( ## Suppress unused arg: frames
      data = .df_basis, color = .axes_col, size = .axes_siz,
      mapping = ggplot2::aes(
        x = x, y = y, xend = .center$x, yend = .center$y)
    )),
    suppressWarnings(ggplot2::geom_text(
      data = .df_basis, color = .axes_col, size = text_size,
      vjust = "outward", hjust = "outward",
      mapping = ggplot2::aes(x = x, y = y, label = basis_label)
    ))
  )
}





### DATA Protos ----
#' Tour proto for data point
#'
#' Adds `geom_point()` of the projected data.
#'
#' @param aes_args A list of arguments to call inside of aes().
#' aesthetic mapping of the primary geom. For example,
#' `geom_point(aes(color = my_fct, shape = my_fct))` becomes
#' `aes_args = list(color = my_fct, shape = my_fct)`.
#' @param identity_args A list of static, identity arguments passed into 
#' the primary geom. For instance,
#' `geom_point(size = 2, alpha = .7)` becomes 
#' `identity_args = list(size = 2, alpha = .7)`. 
#' Also passes more foundational arguments such as stat and position, though 
#' these have been tested less.
#' @param row_index A numeric or logical index of rows to subset to. 
#' Defaults to NULL, all observations.
#' @param bkg_color The character color by name or hexadecimal to display
#' background observations, those not in the `row_index`. 
#' Defaults to "grey80". Use FALSE or NULL to skip rendering background points.
#' Other aesthetic values such as shape and alpha are set adopted from 
#' `aes_args` and `identity_args`.
#' @export
#' @aliases proto_points
#' @family ggtour proto functions
#' @examples
#' library(spinifex)
#' dat     <- scale_sd(penguins_na.rm[, 1:4])
#' clas    <- penguins_na.rm$species
#' gt_path <- save_history(dat, grand_tour(), max_bases = 5)
#' 
#' ggt <- ggtour(gt_path, dat, angle = .3) +
#'   proto_point(aes_args = list(color = clas, shape = clas),
#'               identity_args = list(size = 2, alpha = .7))
#' \donttest{
#' animate_plotly(ggt)
#' }
#' 
#' ## Select/highlight observations with `row_index`
#' ggt <- ggtour(gt_path, dat, angle = .3) +
#'   proto_point(aes_args = list(color = clas, shape = clas),
#'               identity_args = list(size = 2, alpha = .7),
#'               row_index = which(clas == levels(clas)[1]),
#'               bkg_color = "grey80") ## FALSE or NULL to skip plotting background
#' \donttest{
#' animate_plotly(ggt)
#' }
proto_point <- function(
  aes_args = list(),
  identity_args = list(alpha = .9),
  row_index = NULL,
  bkg_color = "grey80"
){
  ## Initialize
  eval(.init4proto)
  if(is.null(.df_data))
    stop("proto_point: Data is NULL. Was data passed to the basis array or ggtour?")
  if(is.null(.df_data$y))
    stop("proto_point: Projection y not found, expected a 2D tour.")
  
  ## do.call aes() over the aes_args
  .aes_func <- function(...)
    ggplot2::aes(x = x, y = y, frame = frame, tooltip = tooltip, ...) ## tooltip for plotly hover tt.
  .aes_call <- do.call(.aes_func, aes_args)
  ## do.call geom_point() over the identity_args
  .geom_func <- function(...) suppressWarnings(
    ggplot2::geom_point(mapping = .aes_call, data = .df_data, ...))
  ret <- do.call(.geom_func, identity_args)
  
  if(exists(".df_data_bkg"))
     if(is.null(bkg_color) == FALSE)
       if(bkg_color != FALSE){
         ## do.call aes() over the .bkg_aes_args
         .aes_func <- function(...)
           ggplot2::aes(x = x, y = y, frame = frame, ...) 
         .aes_call <- suppressWarnings(do.call(.aes_func, .bkg_aes_args))
         ## do.call geom_point() over the .bkg_identity_args
         .geom_func <- function(...) suppressWarnings(
           ggplot2::geom_point(mapping = .aes_call, data = .df_data_bkg,
                               color = bkg_color, ...)) ## Trumps color set in aes_args
         ret <- list(do.call(.geom_func, .bkg_identity_args), ret)
       }
  ## Return
  ret
}



#' Tour proto for data, 1D density, with rug marks
#'
#' Adds `geom_density()` and `geom_rug()` of the projected data. Density 
#' `postion = "stack"` does not work with `animate_plotly()`, GH issue is open. 
#' 
#' @param aes_args A list of arguments to call inside of aes().
#' aesthetic mapping of the primary geom. For example,
#' `geom_point(aes(color = my_fct, shape = my_fct))` becomes
#' `aes_args = list(color = my_fct, shape = my_fct)`.
#' @param identity_args A list of static, identity arguments passed into 
#' the primary geom. For instance,
#' `geom_point(size = 2, alpha = .7)` becomes 
#' `identity_args = list(size = 2, alpha = .7)`. 
#' Also passes more foundational arguments such as stat and position, though 
#' these have been tested less.
#' @param row_index A numeric or logical index of rows to subset to. 
#' Defaults to NULL, all observations.
#' @param density_position The `ggplot2` position of `geom_density()`. Either 
#' c("identity", "stack"), defaults to "identity". Warning: "stack" does not 
#' work with `animate_plotly()` at the moment.
#' @param rug_shape Numeric, the number of the shape to make rug marks.
#' Expects either 3 142, 124 or NULL, '+', '|' (plotly), '|' (ggplot2) 
#' respectively. Defaults to 3.
#' @export
#' @aliases proto_density1d
#' @family ggtour proto functions
#' @examples
#' library(spinifex)
#' dat  <- scale_sd(penguins_na.rm[, 1:4])
#' clas <- penguins_na.rm$species
#' 
#' ## Manual tour
#' bas <- basis_olda(dat, clas)
#' mt  <- manual_tour(bas, manip_var = 2)
#' ggt <- ggtour(mt, dat, angle = .3) +
#'   proto_density(aes_args = list(color = clas, fill = clas)) +
#'   proto_basis1d() +
#'   proto_origin1d()
#' \donttest{
#' animate_plotly(ggt)
#' }
#' 
#' ## Grand tour
#' gt_path <- save_history(dat, grand_tour(), max = 3)
#' ggt <- ggtour(gt_path, dat, angle = .3) +
#'   proto_density(aes_args = list(color = clas, fill = clas)) +
#'   proto_basis1d() +
#'   proto_origin1d()
#' \donttest{
#' animate_plotly(ggt)
#' }
proto_density <- function(
  aes_args = list(),
  identity_args = list(alpha = .7),
  row_index = NULL,
  density_position = c("identity", "stack", "fill"),
  ## plotly only renders position = "identity" atm.
  rug_shape = c(3, 142, 124, NULL)
){
  ## Initialize
  # if("function" %in% is(transformr::tween_polygon))
  #   stop("proto_density requires the {transformr} package, please try install.packages('transformr')")
  eval(.init4proto)
  if(is.null(.df_data))
    stop("proto_density: Data is NULL. Was data passed to the basis array or ggtour?")
  .nms <- names(aes_args)
  if(any(c("color", "colour", "col") %in% .nms) & !("fill" %in% .nms))
    warning("proto_density: aes_args contains color without fill, did you mean to use fill instead?")
  density_position <- match.arg(density_position)
  rug_shape <- rug_shape[1L]
  ## plotly only renders position = "identity" atm.
  ## see: https://github.com/ropensci/plotly/issues/1544
  
  ## geom_density do.call
  y_coef <- diff(range(.map_to$y))
  .aes_func <- function(...)
    ggplot2::aes(x = x, y = y_coef * after_stat(ndensity), frame = frame, ...)
  .aes_call <- do.call(.aes_func, aes_args)
  .geom_func <- function(...)suppressWarnings(
    ggplot2::geom_density(mapping = .aes_call, data = .df_data, ...,
                          position = density_position, color = "black", n = 128L))
  ret <- list(do.call(.geom_func, identity_args),
              ggplot2::theme(legend.position  = "right",
                             legend.direction = "vertical",
                             legend.box       = "vertical",
                             aspect.ratio     = 1L / 2L)) ## y/x, 2x width
  
  ## geom_rug do.call
  if(is.null(rug_shape) == FALSE){
    .aes_func <- function(...)ggplot2::aes(
      x = x, y = -.02 * y_coef, frame = frame, tooltip = tooltip,  ...)
    .aes_call <- do.call(.aes_func, aes_args)
    .geom_func <- function(...) suppressWarnings(
      ggplot2::geom_point(.aes_call, .df_data, shape = rug_shape, ...))
    ret <- c(ret, do.call(.geom_func, identity_args))
  }
  
  ## Return
  ret
}



#' Tour proto for data, 1D density, with rug marks
#'
#' Adds `geom_density_2d()` of the projected data.
#' 
#' @param aes_args A list of arguments to call inside of aes().
#' aesthetic mapping of the primary geom. For example,
#' `geom_point(aes(color = my_fct, shape = my_fct))` becomes
#' `aes_args = list(color = my_fct, shape = my_fct)`.
#' @param identity_args A list of static, identity arguments passed into 
#' the primary geom. For instance,
#' `geom_point(size = 2, alpha = .7)` becomes 
#' `identity_args = list(size = 2, alpha = .7)`. 
#' Also passes more foundational arguments such as stat and position, though 
#' these have been tested less.
#' @param row_index A numeric or logical index of rows to subset to. 
#' Defaults to NULL, all observations.
#' @export
#' @aliases proto_density2d
#' @family ggtour proto functions
#' @examples
#' library(spinifex)
#' dat     <- scale_sd(penguins_na.rm[, 1:4])
#' clas    <- penguins_na.rm$species
#' gt_path <- save_history(dat, grand_tour(), max = 3)
#' 
#' ## geom_density_2d args can be passed in identity_args (bins, binwidth, breaks) 
#' ggt <- ggtour(gt_path, dat, angle = .3) +
#'   proto_density2d(aes_args = list(color = clas, fill = clas),
#'                   identity_args = list(binwidth = .3)) +
#'   proto_point(aes_args = list(color = clas, shape = clas),
#'               identity_args = list(alpha = .2)) +
#'   proto_basis()
#' \donttest{
#' animate_plotly(ggt)
#' }
proto_density2d <- function(
  aes_args = list(),
  identity_args = list(bins = 4),
  row_index = NULL
){
  ## Initialize
  eval(.init4proto)
  
  if(is.null(.df_data))
    stop("proto_point: Data is NULL. Was data passed to the basis array or ggtour?")
  if(is.null(.df_data$y))
    stop("proto_point: Projection y not found, expected a 2D tour.")
  
  ## do.call aes() over the aes_args
  .aes_func <- function(...)
    ggplot2::aes(x = x, y = y, frame = frame, ...)
  .aes_call <- do.call(.aes_func, aes_args)
  ## do.call geom_point() over the identity_args
  .geom_func <- function(...) suppressWarnings(
    ggplot2::geom_density_2d(
      mapping = .aes_call, data = .df_data, contour_var = "ndensity",
      #bins = bins, binwidth = binwidth, breaks = breaks, 
       ...))
  do.call(.geom_func, identity_args)
}


#' Tour proto for data, text labels
#'
#' Adds `geom_text()` of the projected data.
#'
#' @param aes_args A list of arguments to call inside of aes().
#' aesthetic mapping of the primary geom. For example,
#' `geom_point(aes(color = my_fct, shape = my_fct))` becomes
#' `aes_args = list(color = my_fct, shape = my_fct)`.
#' @param identity_args A list of static, identity arguments passed into 
#' the primary geom. For instance,
#' `geom_point(size = 2, alpha = .7)` becomes 
#' `identity_args = list(size = 2, alpha = .7)`. 
#' Also passes more foundational arguments such as stat and position, though 
#' these have been tested less.
#' @param row_index A numeric or logical index of rows to subset to. 
#' Defaults to NULL, all observations.
#' @export
#' @family ggtour proto functions
#' @examples
#' library(spinifex)
#' dat     <- scale_sd(penguins_na.rm[, 1:4])
#' clas    <- penguins_na.rm$species
#' bas     <- basis_pca(dat)
#' mv      <- manip_var_of(bas)
#' gt_path <- save_history(dat, grand_tour(), max_bases = 5)
#' 
#' ggt <- ggtour(gt_path, dat, angle = .2) +
#'   proto_text(list(color = clas))
#' \donttest{
#' animate_plotly(ggt)
#' }
#' 
#' ## Custom labels, subset of points
#' dat     <- mtcars[c("mpg", "disp", "hp", "drat", "wt")]
#' clas    <- as.factor(mtcars$cyl)
#' bas     <- basis_olda(dat, clas)
#' lab     <- abbreviate(rownames(mtcars))
#' gt_path <- save_history(dat, grand_tour(), max_bases = 5)
#' 
#' ggt2 <- ggtour(gt_path, dat) +
#'   proto_text(list(color = clas, label = lab),
#'              list(alpha = .7),
#'              row_index = 1:15)
#' \donttest{
#' animate_plotly(ggt2)
#' }
proto_text <- function(
  aes_args = list(vjust = "outward", hjust = "outward"),
  identity_args = list(nudge_x = 0.05),
  row_index = NULL
){
  ## Initialize
  eval(.init4proto)
  if(is.null(.df_data))
    stop("proto_text: Data is NULL. Was data passed to the basis array or ggtour?")
  if(is.null(.df_data$y))
    stop("proto_text: Projection y not found, expected a 2D tour.")
  if(is.null(aes_args$label))
    aes_args$label <- abbreviate(
      gsub("[^[:alnum:]=]", "", .df_data$tooltip, 3L))

  
  ## do.call aes() over the aes_args
  .aes_func  <- function(...)
    ggplot2::aes(x = x, y = y, frame = frame, ...)
  .aes_call  <- do.call(.aes_func, aes_args)
  ## do.call geom_point() over the identity_args
  .geom_func <- function(...)suppressWarnings(
    ggplot2::geom_text(mapping = .aes_call, data = .df_data, ...))
  
  ## Return proto
  do.call(.geom_func, identity_args)
}


#' Tour proto for data, text labels that repel
#'
#' Adds `ggrepel::geom_text_repel()` of the projected data.
#'
#' @param aes_args A list of arguments to call inside of aes().
#' aesthetic mapping of the primary geom. For example,
#' `geom_point(aes(color = my_fct, shape = my_fct))` becomes
#' `aes_args = list(color = my_fct, shape = my_fct)`.
#' @param identity_args A list of static, identity arguments passed into 
#' the primary geom. For instance,
#' `geom_point(size = 2, alpha = .7)` becomes 
#' `identity_args = list(size = 2, alpha = .7)`. 
#' Also passes more foundational arguments such as stat and position, though 
#' these have been tested less.
#' @param row_index A numeric or logical index of rows to subset to. 
#' Defaults to NULL, all observations.
#' @export
#' @family ggtour proto functions
#' @examples
#' library(spinifex)
#' dat     <- scale_sd(penguins_na.rm[, 1:4])
#' clas    <- penguins_na.rm$species
#' bas     <- basis_pca(dat)
#' mv      <- manip_var_of(bas)
#' gt_path <- save_history(dat, grand_tour(), max_bases = 5)
#' 
#' ggt <- ggtour(gt_path, dat, angle = .2) +
#'   proto_text_repel(list(color = clas)) +
#'   proto_point(list(color = clas, shape = clas),
#'               list(size = 1))
#' \dontrun{
#' animate_gganimate(ggt)
#' }
#' 
#' ## Custom labels, subset of points
#' dat     <- mtcars[c("mpg", "disp", "hp", "drat", "wt")]
#' clas    <- as.factor(mtcars$cyl)
#' bas     <- basis_olda(dat, clas)
#' lab     <- abbreviate(rownames(mtcars))
#' gt_path <- save_history(dat, grand_tour(), max_bases = 3)
#' 
#' ggt2 <- ggtour(gt_path, dat) +
#'   proto_text_repel(list(color = clas, label = lab),
#'                    list(size = 1),
#'                    row_index = 1:30) +
#'   proto_point(list(color = clas, shape = clas),
#'               list(size = 1),
#'               row_index = 1:30)
#' \dontrun{
#' animate_gganimate(ggt2)
#' }
proto_text_repel <- function(
    aes_args = list(vjust = "outward", hjust = "outward"),
    identity_args = list(nudge_x = 0.05),
    row_index = NULL
){
  ## Initialize
  requireNamespace("ggrepel")
  eval(.init4proto)
  if(is.null(.df_data))
    stop("proto_text: Data is NULL. Was data passed to the basis array or ggtour?")
  if(is.null(.df_data$y))
    stop("proto_text: Projection y not found, expected a 2D tour.")
  if(is.null(aes_args$label))
    aes_args$label <- abbreviate(
      gsub("[^[:alnum:]=]", "", .df_data$tooltip, 3L))
  
  ## do.call aes() over the aes_args
  .aes_func  <- function(...)
    ggplot2::aes(x = x, y = y, frame = frame, ...)
  .aes_call  <- do.call(.aes_func, aes_args)
  ## do.call geom_point() over the identity_args
  .geom_func <- function(...)suppressWarnings(
    ggrepel::geom_text_repel(mapping = .aes_call, data = .df_data, ...))
  
  ## Return proto
  do.call(.geom_func, identity_args)
}


#' Tour proto for data, hexagonal heatmap
#'
#' Adds `geom_hex()` of the projected data. Does not display hexagons in plotly
#' animations; will not work with `animate_plotly()`.
#'
#' @param aes_args A list of arguments to call inside of aes().
#' aesthetic mapping of the primary geom. For example,
#' `geom_point(aes(color = my_fct, shape = my_fct))` becomes
#' `aes_args = list(color = my_fct, shape = my_fct)`.
#' @param identity_args A list of static, identity arguments passed into 
#' the primary geom. For instance,
#' `geom_point(size = 2, alpha = .7)` becomes 
#' `identity_args = list(size = 2, alpha = .7)`. 
#' Also passes more foundational arguments such as stat and position, though 
#' these have been tested less.
#' @param row_index A numeric or logical index of rows to subset to. 
#' Defaults to NULL, all observations.
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
#' ## Increase performance by aggregating many points into few hexagons
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
  row_index = NULL,
  bins = 30
){
  ## Initialize
  requireNamespace("hexbin")
  eval(.init4proto)
  if(is.null(.df_data))
    stop("proto_hex: Data is missing. Did you call ggtour() on a manual tour without passing data?")
  if(is.null(.df_data$y))
    stop("proto_hex: Projection y not found, expected a 2D tour.")
  
  ## do.call aes() over the aes_args
  .aes_func <- function(...)
    ggplot2::aes(x = x, y = y, frame = frame, group = frame, ...)
  .aes_call  <- do.call(.aes_func, aes_args)
  ## do.call geom_point() over the identity_args
  .geom_func <- function(...) suppressWarnings(
    ggplot2::geom_hex(mapping = .aes_call, data = .df_data, bins = bins, ...)
  )
  
  ## Return proto
  do.call(.geom_func, identity_args)
}



#' Tour proto highlighing specified points
#'
#' A `geom_point` or `geom_segment`(1d case) call to draw attention to a subset 
#' of points. This is mostly redundant `proto_point` with the implementation 
#' of the `row_index` argument on data protos, still helpful in the 1d case and
#' for `mark_initial`, does not use bkg_row_color
#'
#' @param aes_args A list of arguments to call inside of aes().
#' aesthetic mapping of the primary geom. For example,
#' `geom_point(aes(color = my_fct, shape = my_fct))` becomes
#' `aes_args = list(color = my_fct, shape = my_fct)`.
#' @param identity_args A list of static, identity arguments passed into 
#' `geom_point()`, but outside of `aes()`, for instance 
#' `geom_point(aes(...), size = 2, alpha = .7)` becomes 
#' `identity_args = list(size = 2, alpha = .7)`.
#' #' Typically a single numeric for point size, alpha, or similar.
#' @param row_index A numeric or logical index of rows to subset to. 
#' Defaults to 1, highlighting the first row.
#' @param mark_initial Logical, whether or not to leave a fainter mark at the 
#' subset's initial position. Defaults to FALSE.
#' @export
#' @aliases proto_highlight_2d
#' @family ggtour proto functions
#' @examples
#' library(spinifex)
#' dat     <- scale_sd(penguins_na.rm[, 1:4])
#' clas    <- penguins_na.rm$species
#' gt_path <- save_history(dat, grand_tour(), max_bases = 5)
#' 
#' ## d = 2 case
#' ggt <- ggtour(gt_path, dat, angle = .3) +
#'   proto_default(aes_args = list(color = clas, shape = clas)) +
#'   proto_highlight(row_index = 5)
#' \donttest{
#' animate_plotly(ggt)
#' }
#' 
#' ## Highlight multiple observations
#' ggt2 <- ggtour(gt_path, dat, angle = .3) +
#'   proto_default(aes_args = list(color = clas, shape = clas)) +
#'   proto_highlight(row_index = c( 2, 6, 19),
#'                   identity_args = list(color = "blue", size = 4, shape = 4))
#' \donttest{
#' animate_plotly(ggt2)
#' }
proto_highlight <- function(
  aes_args      = list(),
  identity_args = list(color = "red", size = 5, shape = 8),
  row_index     = 1,
  mark_initial  = FALSE
){
  ## Initialize
  if(is.null(row_index)) return() ## Must be handle this NULL gracefully
  eval(.init4proto) ## aes_args/identity_args/df_data subset in .init4proto.
  if(is.null(.df_data))
    stop("proto_highlight: Data is NULL. Was data passed to the basis array or ggtour?")
  if(is.null(.df_data$y))
    stop("proto_highlight: Projection y not found, expecting a 2D tour. Did you mean to call `proto_highlight1d`?")
  
  ## do.call aes() over the aes_args
  .aes_func <- function(...)
    ggplot2::aes(x = x, y = y, frame = frame, tooltip = tooltip, ...) ## rownum for tooltip
  .aes_call <- do.call(.aes_func, aes_args)
  ## do.call geom_point() over the identity_args
  .geom_func <- function(...) suppressWarnings(ggplot2::geom_point(
    mapping = .aes_call, data = .df_data, ...))
  ret <- do.call(.geom_func, identity_args)
  
  ## Initial mark, if needed, hard-coded some aes, no frame.
  if(mark_initial){
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
  ret
}

#' @rdname proto_highlight
#' @export
#' @examples
#' ## 1D case:
#' gt_path1d <- save_history(dat, grand_tour(d = 1), max_bases = 3)
#' 
#' ggt <- ggtour(gt_path1d, dat, angle = .3) +
#'   proto_default1d(aes_args = list(fill = clas, color = clas)) +
#'   proto_highlight1d(row_index = 7)
#' \donttest{
#' animate_plotly(ggt)
#' }
#' 
#' ## Highlight multiple observations, mark_initial defaults to off
#' ggt2 <- ggtour(gt_path1d, dat, angle = .3) +
#'   proto_default1d(aes_args = list(fill = clas, color = clas)) +
#'   proto_highlight1d(row_index = c(2, 6, 7),
#'                     identity_args = list(color = "green", linetype = 1))
#' \donttest{
#' animate_plotly(ggt2)
#' }
proto_highlight1d <- function(
  aes_args = list(),
  identity_args = list(color = "red", linetype = 2, alpha = .9),
  row_index = 1,
  mark_initial = FALSE
){
  ## Initialize
  if(is.null(row_index)) return() ## Must be handle this NULL gracefully.
  eval(.init4proto)
  if(is.null(.df_data))
    stop("proto_highlight1d: Data is NULL. Was data passed to the basis array or ggtour?")
  
  ## geom_segment do.calls, moving with frame
  .ymin <- min(.map_to$y)
  .ymax <- max(.map_to$y)
  .segment_tail <- diff(c(.ymin, .ymax)) * .05
  .aes_func <- function(...)
    ggplot2::aes(x = x, xend = x,  y = .ymin - .segment_tail,
                 yend = .ymax + .segment_tail,
                 frame = frame, tooltip = tooltip, ...)
  .aes_call <- do.call(.aes_func, aes_args)
  .geom_func <- function(...) suppressWarnings(
    ggplot2::geom_segment(.aes_call, .df_data, ...))
  ret <- do.call(.geom_func, identity_args)
  
  ## Initial mark, if needed, no frame, some hard-coded aes.
  if(mark_initial){
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
  ret
}


### Guides & QoL Protos -----
#' Tour proto for frames square correlation
#'
#' Adds text to the animation, the frame and its specified correlation.
#'
#' @param xy_position Vector of the x and y position, the fraction of the 
#' range of the data in each direction. The projection data is contained in
#' (0, 1) in each direction. Defaults to c(.7, -.1), in the bottom right.
#' @param text_size Size of the text. defaults to 4.
#' @param row_index A numeric or logical index of rows to subset to. 
#' Defaults to NULL, all observations.
#' @param ... Optionally, pass additional arguments to 
#' \code{\link[stats:cor]{stats::cor}}, specifying the type of
#' within frame correlation.
#' @seealso \code{\link[stats:cor]{stats::cor}}
#' @export
#' @examples
#' library(spinifex)
#' dat     <- scale_sd(penguins_na.rm[, 1:4])
#' clas    <- penguins_na.rm$species
#' gt_path <- save_history(dat, grand_tour(), max_bases = 5)
#' 
#' ggt <- ggtour(gt_path, dat, angle = .3) +
#'   proto_default(aes_args = list(color = clas, shape = clas)) +
#'   proto_frame_cor2(xy_position = c(.5, 1.1))
#' \donttest{
#' animate_plotly(ggt)
#' }
proto_frame_cor2 <- function(
  text_size = 4,
  row_index = NULL,
  #stat2d = stats::cor, ## hardcoded stats::cor atm
  xy_position = c(.7, -.1),
  ... ## passed to stats::cor
){
  ## Initialize
  eval(.init4proto)
  if(is.null(.df_data))
    stop("proto_frame_stat: Data is NULL. Was data passed to the basis array or ggtour?")
  
  ## Find aggregated values, stat within the frame
  if(.is_faceted){.gb <- .df_data %>% dplyr::group_by(frame, facet_var)
  }else{.gb <- .df_data %>% dplyr::group_by(frame)}
  .agg <- .gb %>%
    dplyr::summarise(value = round(stats::cor(x, y, ...)^2L, 2L)) %>%
    dplyr::ungroup()
  
  ## Set xy_position
  .x_ran <- range(.df_data$x)
  .x_dif <- diff(.x_ran)
  .y_ran <- range(.df_data$y)
  .y_dif <- diff(.y_ran)
  .x <- .x_ran[1L] + xy_position[1L] * .x_dif
  .y <- .y_ran[1L] + xy_position[2L] * .y_dif
  
  ## Prefix text:
  # ## Removes namespace; ie. 'stats::cor' to 'cor'
  # .stat_nm  <- substitute(stat2d)
  # .last_pos <- regexpr("\\:[^\\:]*$", s) + 1L
  # .stat_nm  <- substr(.stat_nm, .last_pos, nchar(.stat_nm))
  
  ## Create the final df with position, frame, facet_var, label
  .txt_df <- data.frame(
    x = .x, y = .y, .agg,
    tooltip = paste0("cor^2: ", sprintf("%3.2f", .agg$value)))
  
  ## Return
  suppressWarnings(ggplot2::geom_text(
    ggplot2::aes(x = x, y = y, frame = frame, label = tooltip),
    data = .txt_df, ...))
}

#' Tour proto for data origin zero mark
#'
#' Adds a zero mark showing the location of the origin for the central data area.
#'
#' @param tail_size How long the origin mark should extended
#' relative to the observations. Defaults to .05, 5% of the projection space.
#' @param identity_args A list of static, identity arguments passed into 
#' the primary geom. For instance,
#' `geom_point(size = 2, alpha = .7)` becomes 
#' `identity_args = list(size = 2, alpha = .7)`. 
#' Also passes more foundational arguments such as stat and position, though 
#' these have been tested less.
#' @export
#' @aliases proto_origin2d
#' @family ggtour proto functions
#' @examples
#' library(spinifex)
#' dat  <- scale_sd(penguins_na.rm[, 1:4])
#' clas <- penguins_na.rm$species
#' 
#' ## 2D case:
#' gt_path <- save_history(dat, grand_tour(), max_bases = 5)
#' ggt <- ggtour(gt_path, dat, angle = .1) +
#'   proto_point(list(color = clas, shape = clas)) +
#'   proto_origin() ## `+` in center
#' \donttest{
#' animate_plotly(ggt)
#' }
proto_origin <- function(
  identity_args = list(color = "grey60", size = .5, alpha = .9),
  tail_size = .05){
  ## Initialize
  eval(.init4proto)
  if(is.null(.df_data$y))
    stop("proto_origin: data y not found, expects a 2D tour.")
  
  #### Setup origin, zero mark, 5% on each side.
  .df_0range <- data.frame(x = c(0L, range(.df_data$x)),
                           y = c(0L, range(.df_data$y)))
  .zero <- map_relative(.df_0range, "full", .map_to)[1L,, drop = FALSE]
  .tail <- tail_size / 2L * max(diff(range(.map_to$x)),
                                diff(range(.map_to$y)))
  .df_origin <- data.frame(x     = c(.zero$x - .tail, .zero$x),
                           x_end = c(.zero$x + .tail, .zero$x),
                           y     = c(.zero$y, .zero$y - .tail),
                           y_end = c(.zero$y, .zero$y + .tail))
  
  if(.is_faceted){
    .df_u_facet_lvls <- data.frame(facet_var = factor(unique(.facet_var)))
    .df_origin       <- merge(.df_origin, .df_u_facet_lvls)
  }
  
  ## do.call geom_point() over the identity_args
  .geom_func <- function(...)
    ggplot2::geom_segment(
      ggplot2::aes(x = x, y = y, xend = x_end, yend = y_end),
      data = .df_origin, ...)
  ## Return
  do.call(.geom_func, identity_args)
}


#' @rdname proto_origin
#' @export
#' @examples
#' 
#' ## 1D case:
#' gt_path1d <- save_history(dat, grand_tour(d = 1), max_bases = 5)
#' 
#' ggt <- ggtour(gt_path1d, dat) +
#'   proto_density(list(fill = clas, color = clas)) +
#'   proto_origin1d() ## Adds line at 0.
#' \donttest{
#' animate_plotly(ggt)
#' }
proto_origin1d <- function(
  identity_args = list(color = "grey60", size = .5, alpha = .9)
){
  ## Initialize
  eval(.init4proto)
  if(is.null(.df_data))
    stop("proto_origin1d: Data is NULL. Was data passed to the basis array or ggtour?")
  
  .df_0range <- data.frame(x = c(0L, range(.df_data$x)),
                           y = c(0L))
  .zero <- map_relative(.df_0range, "full", .map_to)[1L,, drop = FALSE]
  .tail <- diff(range(.map_to$y)) * .55
  .df_origin <- data.frame(
    x     = c(.zero$x, .zero$x),
    x_end = c(.zero$x, .zero$x),
    y     = c(.zero$y, .zero$y),
    y_end = c(.zero$y - .tail, .zero$y + .tail))
  
  if(.is_faceted){
    .df_u_facet_lvls <- data.frame(facet_var = factor(unique(.facet_var)))
    .df_origin <- merge(.df_origin, .df_u_facet_lvls)
  }
  
  ## do.call geom_segment() over the identity_args
  .geom_func <- function(...)
    ggplot2::geom_segment(
      ggplot2::aes(x = x, y = y, xend = x_end, yend = y_end),
      data = .df_origin, ...)
  ## Return
  do.call(.geom_func, identity_args)
}

#' Tour proto adding a vertical/horizontal line
#'
#' Adds a vertical/horizontal line with an intercept of 0, scaled to the data 
#' frame.
#'
#' @param identity_args A list of static, identity arguments passed into 
#' the primary geom. For instance,
#' `geom_point(size = 2, alpha = .7)` becomes 
#' `identity_args = list(size = 2, alpha = .7)`. 
#' Also passes more foundational arguments such as stat and position, though 
#' these have been tested less.
#' @export
#' @aliases proto_hline
#' @family ggtour proto functions
#' @examples
#' library(spinifex)
#' dat  <- scale_sd(penguins_na.rm[, 1:4])
#' clas <- penguins_na.rm$species
#' 
#' ## 2D case:
#' gt_path <- save_history(dat, grand_tour(), max_bases = 5)
#' ggt <- ggtour(gt_path, dat, angle = .1) +
#'   proto_point(list(color = clas, shape = clas)) +
#'   proto_hline0() + ## horizonatal line at 0
#'   proto_vline0()   ## vertical line at 0
#' \donttest{
#' animate_plotly(ggt)
#' }
proto_hline0 <- function(
  identity_args = list(color = "grey80", size = .5, alpha = .9)
){
  ## Initialize
  eval(.init4proto)
  if(is.null(.df_data)) 
    stop("proto_hline0: Data is NULL. Was data passed to the basis array or ggtour?")
  if(is.null(.df_data$y))
    stop("proto_hline0: Projection y not found, expects a 2D tour.")
  
  ## Dataframe
  .df_zero <- map_relative(data.frame(x = c(0L, range(.df_data$x)),
                                      y = c(0L, range(.df_data$y))),
                           "center", .map_to)[1L,, drop = FALSE]
  if(.is_faceted){
    .df_u_facet_lvls <- data.frame(facet_var = factor(unique(.facet_var)))
    .df_zero <- merge(.df_zero, .df_u_facet_lvls)
  }
  
  ## do.call geom_point() over the identity_args
  .geom_func <- function(...)
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = y),
      data = .df_zero, ...)
  ## Return
  do.call(.geom_func, identity_args)
}

#' @rdname proto_hline0
#' @export
#' @aliases proto_vline
proto_vline0 <- function(
  identity_args = list(color = "grey80", size = .5, alpha = .9)
){
  ## Initialize
  eval(.init4proto)
  if(is.null(.df_data)) 
    stop("proto_vline0: Data is NULL. Was data passed to the basis array or ggtour?")
  if(is.null(.df_data$y))
    stop("proto_vline0: Projection y not found, expects a 2D tour. Did you mean to call `proto_origin1d`?")
  
  ## dataframe
  .df_zero <- map_relative(data.frame(x = c(0L, range(.df_data$x)),
                                      y = c(0L, range(.df_data$y))),
                           "center", .map_to)[1L,, drop = FALSE]
  if(.is_faceted){
    .df_u_facet_lvls <- data.frame(facet_var = factor(unique(.facet_var)))
    .df_zero <- merge(.df_zero, .df_u_facet_lvls)
  }
  
  ## do.call geom_point() over the identity_args
  .geom_func <- function(...)
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = x),
      data = .df_zero, ...)
  ## Return
  do.call(.geom_func, identity_args)
}


#' Wrapper function for default 2D/1D tours respectively.
#' 
#' An easier way to get to default 2D tour settings.
#' Returns a list of proto_origin(), proto_point(...), proto_basis() for 2D.
#' Returns a list of proto_origin1d(), proto_density(...), proto_basis1d() for 1D.
#'
#' @param position The position, to place the basis axes relative to the 
#' data. `proto_basis` expects one of c("left", "center", "right", "bottomleft", "topright", 
#' "off"), defaults to "left". `proto_basis1d` expects one of 
#' c("bottom1d", "floor1d", "top1d", "off"). Defaults to "bottom1d".
#' @param ... Optionally pass additional arguments to `proto_point` or 
#' `proto_density`.
#' @export
#' @aliases proto_default2d proto_def proto_def2d
#' @family ggtour proto functions
#' @examples
#' library(spinifex)
#' dat  <- scale_sd(penguins_na.rm[, 1:4])
#' clas <- penguins_na.rm$species
#' 
#' ## 2D case:
#' bas     <- basis_pca(dat)
#' mv      <- manip_var_of(bas)
#' mt_path <- manual_tour(bas, mv)
#' 
#' ggt <- ggtour(mt_path, dat) +
#'   proto_default(aes_args = list(color = clas, shape = clas))
#' \donttest{
#' animate_plotly(ggt)
#' }
proto_default <- function(
  position = c("left", "center", "right", "bottomleft", "topright", "off"),
  ...
){
  position <- match.arg(position)
  list(
    proto_point(...),
    proto_basis(position),
    proto_origin()
  )
}


#' @rdname proto_default
#' @export
#' @aliases proto_def1d
#' @examples
#' library(spinifex)
#' 
#' ## 1D case:
#' gt_path <- save_history(dat, grand_tour(d = 1), max_bases = 3)
#' 
#' ggt <- ggtour(gt_path, dat) +
#'   proto_default1d(aes_args = list(fill = clas, color = clas))
#' \donttest{
#' animate_plotly(ggt)
#' }
proto_default1d <- function(
  position = c("bottom1d", "floor1d", "top1d", "off"),
  ...
){
  position <- match.arg(position)
  list(
    proto_density(...),
    proto_basis1d(position),
    proto_origin1d()
  )
}


# PCA Helpers -----


#' Plot 2 components of Principal Component Analysis
#' 
#' Performs PCA on the data and used `proto_default` to plot with percent 
#' variation labels.
#' 
#' @param data Numeric matrix or data.frame of the observations.
#' @param components The 2 numbers of the principal components to use.
#' @param ... Optionally pass arguments to `proto_default`
#' @export
#' @seealso [proto_default()]
#' @examples
#' dat <- scale_sd(wine[, 2:6])
#' plot_pca(data = dat)
#' 
#' ## Different components, class coloring
#' clas <- as.factor(wine$Type)
#' plot_pca(data = dat, components = c(1, 3), position = "center",
#'          aes_args = list(color = clas, shape = clas))
plot_pca <- function(data, components = c(1, 2), ...){
  .pca <- stats::prcomp(data)
  bas <- .pca$rotation[, components]
  pct_var_exp <- round(100 * .pca$sdev^2/sum(.pca$sdev^2), 1)[components]
  
  ggtour(bas, data) +
    proto_default(...) +
    ggplot2::labs(x = paste0("PC", components[1], ", ", pct_var_exp[1], "% var"),
                  y = paste0("PC", components[2], ", ", pct_var_exp[2], "% var"),)
}

#' Plot 2 components of Principal Component Analysis
#' 
#' Performs PCA on the data and used `proto_default` to plot with percent 
#' variation labels. Also appends a screeplot of the component variances.
#' 
#' @param data Numeric matrix or data.frame of the observations.
#' @param components The 2 numbers of the principal components to use.
#' @param ... Optionally pass arguments to `proto_default`
#' @export
#' @examples
#' dat <- scale_sd(wine[, 2:6])
#' plot_pca_scree(data = dat)
#' 
#' ## Different components, class coloring
#' clas <- as.factor(wine$Type)
#' plot_pca_scree(data = dat, components = c(1, 3), position = "center",
#'                aes_args = list(color = clas, shape = clas))
plot_pca_scree <- function(data, components = c(1, 2), ...){
  requireNamespace("patchwork")
  .pca <- stats::prcomp(data)
  bas <- .pca$rotation[, components]
  var_df <- data.frame(
    comp = 1:ncol(data),
    class = 1:ncol(data) %in% components,
    var = .pca$sdev^2,
    pct_exp = round(100 * .pca$sdev^2/sum(.pca$sdev^2), 1),
    cumvar = round(100 * cumsum(.pca$sdev^2/sum(.pca$sdev^2)), 2)
  )
  
  g1 <- ggtour(bas, data) +
    proto_default(...) +
    ggplot2::labs(x = paste0("PC", components[1], ", ", var_df$pct_exp[1], "% var"),
                  y = paste0("PC", components[2], ", ", var_df$pct_exp[2], "% var"),)
  g2 <- ggplot(var_df) +
    geom_col(aes(comp, pct_exp, fill = class), color = "black") +
    geom_line(aes(comp, cumvar)) +
    geom_point(aes(comp, cumvar)) +
    ggplot2::labs(y = "Percent var explained", x = "Principal component") +
    theme_minimal() +
    theme(legend.position = "none")
  g1 + g2
}


### UNAPPLIED IDEA DRAFTS -----
if(FALSE){ ## DONT RUN -- DEV IDEAS
  ## Geom_table won't work with plotly or gganimate animation frames.
  #### Recreate manually with geom_text...
  if(FALSE){
    # #' @rdname proto_basis
    # #' @param segment_size (1D bases only) the width thickness of the rectangle bar
    # #' showing variable magnitude on the axes. Defaults to 2.
    # # #' @export
    # #' @examples
    # #' ## basis_table
    # #' ggt <- ggtour(mt_path, dat, angle = .3) +
    # #'   proto_default(aes_args = list(color = clas, shape = clas)) +
    # #'   proto_basis_text()
    # #' \donttest{
    # #' animate_plotly(ggt)
    # #' }
    proto_basis_table <- function(
      position = c("right"),
      text_size = 5
    ){
      ## Initialize
      eval(.init4proto)
      
      ## make positions to be joined to .df_basis
      .u_frame <- data.frame(frame = unique(.df_basis$frame))
      d <- 0L:.d; p <- 1L:.p
      .pos <- merge(d, p) %>%
        map_relative(position, .map_to) %>%
        merge(.u_frame)
      colnames(.pos) <- c("d", "p", "frame")
      ## round basis contributions
      .df_basis[, c("x", "y")] <- round(.df_basis[, c("x", "y")], 2L)
      .bas_longer <- .df_basis %>%
        tidyr::pivot_longer(!c(frame, tooltip), names_to = "element", values_to = "text")
      ## Note this is the dynamic part of the text, 
      ## also need a static geom_text for min(.pos$p) for the static header column
      .df_pos_frames <- dplyr::left_join(.df_basis, .pos[.pos$p != min(.pos$p),], by = "frame")
      .df_pos_frames
      
      .pos[.pos$p == min(.pos$p), ]
      
      ## Return proto
      return(list(
        ggpp::geom_table(
          data = .df_pos_frames, 
          aes(x = .pos$x, y = .pos$y, label = rownames(.df_basis)),
          table.rownames = TRUE)
      ))
    }
    
  }
  
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
    if(is.null(.df_data))
      stop("proto_hdr: Data is NULL. Was data passed to the basis array or ggtour?")
    if(is.null(.df_data$y))
      stop("proto_hdr: Projection y not found, expects a 2D tour.")
    
    ##TODO: DENSITY WORK & SEGMENT.
    #### each segment will need it's own .aes and .geom do.calls.
    #### All but the lowest density regions will want to go to geom_density or geom_hexbin.
    if(F)
      ?hdrcde::hdrscatterplot
    
    ## do.call aes() over the aes_args
    .aes_func <- function(...)
      ggplot2::aes(x = x, y = y, frame = frame, tooltip = tooltip, ...) ## tooltip for plotly on hover tip
    .aes_call <- do.call(.aes_func, aes_args)
    ## do.call geom_point() over the identity_args
    .geom_func <- function(...) suppressWarnings(
      ggplot2::geom_point(mapping = .aes_call, data = .df_data, ...))
    
    ## Return proto
    do.call(.geom_func, identity_args)
  }
}
