## 0_util_ggproto


#' Replicate a list of arguments
#' 
#' Replicates each element of a list, to a target length for animating across
#' many frames.
#'
#' @param arg_ls A list of arguments that need to be replicated.
#' @param length.out The target length to recycle to, 
#' typically rows of the data times the number of basis in an array.
### NOT @export
#' @examples
#' ## Setup
#' dat_std <- scale_sd(tourr::flea[, 1:6])
#' clas <- tourr::flea[, 7]
#' t1 <- tourr::save_history(dat_std, max = 3)
#' ls_args <- list(col = clas, shape = clas)
#' tgt_len <- nrow(dat_std) * dim(t1)[3]
#' 
#' spinifex:::rep_arg_ls(ls_args, tgt_len)
rep_arg_ls <- function(arg_ls, length.out){
  ret <- list()
  if(length(arg_ls) > 1L){
    ret <- list()
    .mute <- sapply(seq_along(arg_ls), function(i){
      .this_arg <- arg_ls[[i]]
      if(length(.this_arg) == 1L){ret[[i]] <<- .this_arg
      }else{
        ret[[i]] <<- rep_len(as.factor(.this_arg), length.out)
      }
    })
  }
  return(ret)
}


#' ggproto for data point projection
#' 
#' Returns headless (without ggplot()) geom_point call for projected data points. 
#' 
### NOT @export
#' @examples
#' ## See use in
#' if(interactive())
#'   ?render_
ggproto_data_point <- function(data_frames, aes_args, identity_args){
  geom_point_call <- NULL ## Init
  if(is.null(data_frames) == FALSE){ ## If data exists
    tgt_len <- nrow(data_frames)
    ## Recycle/replicate args if needed
    aes_args_out <- rep_arg_ls(aes_args, tgt_len)
    identity_args_out <- rep_arg_ls(identity_args, tgt_len)
    
    ## aes() call
    aes_func <- function(...)
      ggplot2::aes(x = x, y = y, frame = frame, ...)
    aes_call <- do.call(aes_func, args = aes_args_out)
    
    ## geom_point() call
    geom_point_func <- function(aes_call, ...)
        ggplot2::geom_point(aes_call, data = data_frames, ...)
    geom_point_call <-
      do.call(geom_point_func, c(list(aes_call), identity_args_out))
  }
  return(geom_point_call)
}


#' ggproto for the basis axes unit circle
#' 
#' Returns headless (without ggplot()) list of geoms that make up the basis
#' aces unit circle (namely the geoms: line, segment and text)
#' 
### NOT @export
#' @examples
#' ## See use in
#' if(interactive())
#'   ?render_
ggproto_basis_axes <- function(basis_frames,
                               data_frames = NULL, ## for scaling to.
                               manip_var = NULL,
                               axes = "center",
                               manip_col = "blue",
                               line_size = 1L,
                               text_size = 5L){
  ls_basis_axes_calls <- list(NULL) ## Init
  if(axes != "off"){
    p <- sum(basis_frames$frame == 1L)
    
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
    
    ## Axes setup
    angle <- seq(0L, 2L * pi, length = 360L)
    circ  <- data.frame(x = cos(angle), y = sin(angle))
    ## Scale basis axes/circle
    center <- scale_axes(data.frame(x = 0L, y = 0L), axes, to = data_frames)
    circ <- scale_axes(circ, axes, to = data_frames)
    ## Rejoin frame number to the scaled bases frames
    basis_frames <- scale_axes(basis_frames, axes, to = data_frames)
    
    ls_basis_axes_calls <- list(
      ## Circle path
      ggplot2::geom_path(
        data = circ, color = "grey80", size = line_size, inherit.aes = FALSE,
        mapping = ggplot2::aes(x = x, y = y)
      ),
      ## Basis axes segments
      suppressWarnings( ## Suppress for unused aes "frame".
        ggplot2::geom_segment(
          data = basis_frames, size = axes_siz, colour = axes_col,
          mapping = ggplot2::aes(x = x, y = y, frame = frame,
                                 xend = center[, 1L], yend = center[, 2L])
        )
      ),
      ## Basis axes text labels
      suppressWarnings( ## Suppress for unused aes "frame".
        ggplot2::geom_text(
          data = basis_frames,
          mapping = ggplot2::aes(x = x, y = y,
                                 frame = frame, label = label),
          vjust = "outward", hjust = "outward",
          colour = axes_col, size = text_size)
      )
    )
  }
  return(ls_basis_axes_calls)
}

