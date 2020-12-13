##
## MATH AND TRANSFORMS -----
##

#' Test if a numeric matrix is orthonormal.
#'
#' Handles more cases than tourr::is_orthonormal().
#'
#' @param x Numeric matrix to test the orthonormality of.
#' @param tol Tolerance of (the sum of element-wise) floating point differences.
#' @return Single logical of the orthonormal matrix of the matrix.
#' @export
#' @examples 
#' is_orthonormal(tourr::basis_random(n = 6))
#' is_orthonormal(matrix(1:12, ncol=2), tol = 0.01)
is_orthonormal <- function(x, tol = 0.001) { ## (tol)erance of SUM of element-wise error.
  x <- as.matrix(x)
  actual <- t(x) %*% x ## Collapses to identity matrix IFF x is orthonormal
  expected <- diag(ncol(x))
  if(max(actual - expected) < tol) {TRUE} else {FALSE}
}

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
#' @param label Optional, labels for the reference frame of length 1 or the 
#' number of variables used. Defaults to an abbreviation of the variables.
#' @return A list containing an array of basis frames (p, d, n_frames) and
#' an array of data frames (n, d, n_frames) if data is present.
#' @export
#' @examples
#' ## Setup
#' dat_std <- tourr::rescale(wine[, 2:14])
#' clas <- wine$Type
#' bas <- basis_pca(dat_std)
#' mv <- manip_var_pca(dat_std)
#' 
#' ## Radial tour array to long df, as used in play_manual_tour()
#' tour_array <- manual_tour(basis = bas, manip_var = mv)
#' str(
#'   array2df(array = tour_array, data = dat_std,
#'            label = paste0("MyLabs", 1:nrow(bas)))
#' )
#' 
#' ## tourr::save_history tour array to long df, as used in play_tour_path()
#' hist_array <- tourr::save_history(data = dat_std, max_bases = 10)
#' str(
#'   array2df(array = hist_array, data = dat_std,
#'            label = paste0("MyLabs", 1:nrow(bas)))
#' )
array2df <- function(array,
                     data = NULL,
                     label = NULL){
  if(class(array) == "history_array") class(array) <- "array"
  ## Initialize
  manip_var <- attributes(array)$manip_var
  p <- dim(array)[1L]
  n_frames <- dim(array)[3L]
  
  ## Basis condition handling
  basis_frames <- NULL
  for(frame in 1:n_frames){
    basis_rows <- data.frame(cbind(array[,, frame], frame))
    basis_frames <- rbind(basis_frames, basis_rows)
  }
  colnames(basis_frames) <- c("x", "y", "frame")
  
  ## Data; if exists, array to long df
  if(is.null(data) == FALSE){
    data <- as.matrix(data)
    data_frames <- NULL
    for(frame in 1L:n_frames){
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
  basis_frames$label <- NULL
  if(is.null(label) == FALSE){
    basis_frames$label <- rep(label, nrow(basis_frames) / length(label))
  }else{
    if(!is.null(data)){basis_frames$label <- abbreviate(colnames(data), 3L)
    }else{
      basis_frames$label <- paste0("V", 1L:p)
    }
  }
  attr(basis_frames, "manip_var") <- manip_var
  
  ## Frame condition handling
  df_frames <- list(basis_frames = basis_frames)
  if(is.null(data) == FALSE){
    df_frames <- list(basis_frames = basis_frames, data_frames = data_frames)
  }
  
  ## Return
  df_frames
}


#' Returns the axis scale and position.
#' 
#' Typically called, by other functions to scale axes.
#' 
#' @param x Numeric table, first 2 columns and scaled and offset relative to 
#' the `to` argument.
#' @param position Text specifying the position the axes should go to.
#' Defaults to "center" expects one of: "center", "left", "right", 
#' "bottomleft", "topright", or "off".
#' @param to Table to appropriately set the size and position of the axes to.
#' Based on the min/max of the first 2 columns.
#' @return Scaled and offset `x` typically controlling axes placement.
#' @seealso \code{\link{pan_zoom}} for more manual control.
#' @export
#' @examples
#' rb <- tourr::basis_random(4, 2)
#' 
#' scale_axes(x = rb, position = "bottomleft")
#' scale_axes(x = rb, position = "right", to = wine[, 2:3])
scale_axes <- function(x,
                       position = c("center", "left", "right", "bottomleft",
                                    "topright", "off", "pan_zoom() call;",
                                    pan_zoom(c(-1L, 0L), c(.7, .7))),
                       to = data.frame(x = c(-1L, 1L), y = c(-1L, 1L))
){
  ## If position is pan_zoom call with x = NULL;
  if(is.list(position) & length(position) == 2L){ 
    return(pan_zoom(pan = position$pan, zoom = position$zoom, x = x))
  }
  ## Assumptions
  if(position == "off") return()
  if(ncol(x) != 2L) warning("pan_zoom is only defined for 2 variables. x has more than 2 columns")
  if(is.null(to)) to <- data.frame(x = c(-1L, 1L), y = c(-1L, 1L))
  
  ## Initialize
  position <-
    match.arg(tolower(position), several.ok = FALSE, choices = 
                c("center", "bottomleft", "topright", "off", "left", "right"))
  x_to <- c(min(to[, 1L]), max(to[, 1L]))
  y_to <- c(min(to[, 2L]), max(to[, 2L]))
  xdiff   <- diff(x_to)
  ydiff   <- diff(y_to)
  xcenter <- mean(x_to)
  ycenter <- mean(y_to)
  
  ## Condition handling of position
  if(position == "center"){
    scale <- .3 * ydiff
    xoff  <- xcenter
    yoff  <- ycenter
  } else if(position == "bottomleft"){
    scale <- .25 * ydiff
    xoff <- -.25 * xdiff + xcenter
    yoff <- -.5 * ydiff + ycenter
  } else if(position == "topright"){
    scale <- .25 * ydiff
    xoff <- .25 * xdiff + xcenter
    yoff <- .5 * ydiff + ycenter
  } else if(position == "left"){
    scale <- .3 * ydiff
    xoff <- -.7 * xdiff + xcenter
    yoff <- ycenter
  } else if(position == "right"){
    scale <- .3 * ydiff
    xoff <- .7 * xdiff + xcenter
    yoff <- ycenter
  }
  
  ## Apply scale and return
  x[, 1L] <- scale * x[, 1L] + xoff
  x[, 2L] <- scale * x[, 2L] + yoff
  return(x)
}



#' Pan (offset) and zoom (scale) a 2 column matrix or dataframe.
#' 
#' A manual variant of `scale_axes()`. Can be used as the `axes` argument 
#' to manually set the size and locations of the axes.
#' 
#' @param pan 2 Numeric value to offset/pan the first 2 dimensions of `x`.
#' @param zoom 2 Numeric value to scale/zoom the first 2 dimensions of `x`.
#' @param x Numeric data object with 2 columns to scale and offset.
#' Defaults to NULL, passing arguments to scale_axes for use internally.
#' @return Scaled and offset `x`.
#' @seealso \code{\link{scale_axes}} for preset choices.
#' @export
#' @examples 
#' rb <- tourr::basis_random(6, 2)
#' pan_zoom(pan = c(-1, 0), zoom = c(2/3, 2/3), x = rb)
pan_zoom <- function(pan = c(0L, 0L),
                     zoom = c(1L, 1L),
                     x = NULL
){
  if(is.null(x)) return(list(pan = pan, zoom = zoom))
  ## Assumptions
  if(ncol(x) != 2L) warning("pan_zoom is only defined for 2 variables. x has more than 2 columns")
  ## Apply scale and return
  ret <- x
  ret[, 1L] <- ret[, 1L] * zoom[1L] + pan[1L]
  ret[, 2L] <- ret[, 2L] * zoom[2L] + pan[2L]
  return(ret)
}

##
## GGPLOT2 AESTHETICS ------
##

#' A ggplot2 theme containing theme_void and coord_fixed.
#' The default value for ggproto arguments in spinifex functions.
#' 
#' @export
#' @examples 
#' theme_spinifex()
#' 
#' require("ggplot2")
#' ggplot(mtcars, aes(wt, mpg, color = as.factor(cyl))) +
#'   geom_point() + theme_spinifex()

theme_spinifex <- function(){
  list(ggplot2::theme_void(),
       ggplot2::scale_color_brewer(palette = "Dark2"),
       ggplot2::coord_fixed()
  )
}



##
## BASIS AND MANIP VAR HELPERS -----
##

#' The basis of Principal Component Analysis (PCA)
#' 
#' @param data Numeric matrix or data.frame of the observations.
#' @param p Number of dimensions in the projection space.
#' @export
#' @examples 
#' basis_pca(data = wine[, 2:14])
basis_pca <- function(data, p = 2L){
  stats::prcomp(data)$rotation[, 1L:p]
}


# #' The basis of Linear Discriminant Analysis (LDA)
# #' 
# #' Returns a numeric matrix of the first `p` columns of the MASS::lda for the
# #' given class. MASS::lda()$scaling is not orthonromal (!?); coerced
# #' with tourr::orthonormalise().
# #' 
# #' @param data Numeric matrix or data.frame of the observations, coerced to matrix.
# #' @param class The class for each observation, coerced to a factor.
# #' @param p Number of dimensions in the projection space.
# #' @return Numeric matrix of the last basis of a guided tour.
# #' @seealso \code{\link[MASS]{lda}}
# #' @export
# #' @examples 
# #' basis_lda(data = wine[, 2:14], class = wine$Type)
# basis_lda <- function(data, class, p = 2L){
#   lda <- MASS::lda(x = as.matrix(data), grouping = as.factor(class))
#   ## MASS::lda is not giving orthonormal (!?)
#   tourr::orthonormalise(lda$scaling[, 1L:p])
# }


#' The last basis of a guided tour
#' 
#' @param data Numeric matrix or data.frame of the observations.
#' @param index_f The index function to optimize.
#' `{tourr}` exports `holes()`, `cmass()`, and `lda_pp(class)`.
#' @param p Number of dimensions in the projection space.
#' @param ... Optional, other arguments to pass to `tourr::guided_tour`.
#' @return Numeric matrix of the last basis of a guided tour.
#' @seealso \code{\link[tourr]{guided_tour}} for annealing arguments.
#' @export
#' @examples 
#' basis_guided(data = wine[, 2:14], index_f = tourr::holes())
#' 
#' basis_guided(data = wine[, 2:14], index_f = tourr::cmass(),
#'              alpha = .4, cooling = .9, max.tries = 30)
basis_guided <- function(data, index_f = tourr::holes(), p = 2L, ...){
  invisible(utils::capture.output(
    hist <- tourr::save_history(data, guided_tour(index_f = index_f, d = p, ...))
  ))
  matrix(hist[, , length(hist)], ncol = p)
}

#' The number of the variable that has the `max/min absolute value in the first
#' Principal Component (of PCA)`rank`-th largest contribution to projection 
#' space. Useful for setting the manip_var argument.
#' 
#' @param data Numeric matrix or data.frame of the observations.
#' @param rank The number, specifying the variable with the `rank`-th largest 
#' contribution. Defaults to 1.
#' @export
#' @examples 
#' manip_var_pca(data = wine[, 2:14])
manip_var_pca <- function(data, rank = 1L){
  abs_pc1 <- abs(stats::prcomp(data)$rotation[, 1L])
  order(abs_pc1, decreasing = TRUE)[rank]
}

# #' The number of the variable that has the max/min absolute value in the first
# #' Linear Discriminant (of LDA). Useful for setting the manip_var argument.
# #' 
# #' @param data Numeric matrix or data.frame of the observations, coerced to matrix
# #' @param class The class for each observation, coerced to a factor.
# #' @param func The function to be applied, expects max or min.
# #' @return Numeric matrix of the last basis of a guided tour.
# #' @seealso \code{\link[MASS]{lda}}
# #' @export
# #' @examples 
# #' manip_var_lda(data = wine[, 2:14], class = wine$Type)
# manip_var_lda <- function(data, class, func = max){
#   lda <- MASS::lda(x = as.matrix(data), grouping = as.factor(class))
#   is_orthonormal(lda$scaling)
#   ## MASS::lda is not giving orthonormal (!?)
#   abs_ld1 <- abs(tourr::orthonormalise(lda$scaling[, 1L]))
#   which(abs_ld1 == func(abs_ld1))
# }


#' The number of the variable that has the max/min absolute norm in final basis.
#' Useful for setting the manip_var argument.
#' 
#' @param data Numeric matrix or data.frame of the observations.
#' @param index_f The index function to optimise.
#' {tourr} exports holes(), cmass(), and lda_pp(class).
#' @param p Number of dimensions in the projection space.
#' @param rank The number, specifying the variable with the `rank`-th largest 
#' contribution. Defaults to 1.
#' @param ... Optional, other arguments to pass to `tourr::guided_tour`.
#' @return Numeric matrix of the last basis of a guided tour.
#' @seealso \code{\link[tourr]{guided_tour}} for annealing arguments.
#' @export
#' @examples
#' manip_var_guided(data = wine[, 2:14], index_f = tourr::holes())
#' 
#' manip_var_guided(data = wine[, 2:14], index_f = tourr::cmass(), rank = 2,
#'                  alpha = .4, cooling = .9, max.tries = 30)
manip_var_guided <- function(data, index_f = tourr::holes(), p = 2L,
                             rank = 1L, ...){
  invisible(utils::capture.output( ## Mute the noisy function
    hist <- tourr::save_history(data, guided_tour(index_f = index_f, d = p, ...))
  ))
  bas <- hist[, , length(hist)]
  ## Row-wise norms
  norm <- apply(bas, 1L, FUN = function(row) sqrt(sum(row)))
  order(norm, decreasing = TRUE)[rank]
}

#' Centers by mean and scales by the standard deviation of each column of data.
#' 
#' @param data Numeric matrix or data.frame of the observations.
#' @export
#' @examples 
#' scale_sd(data = wine[, 2:14])
scale_sd <- function(data){
  apply(data, 2, function(c) (c - mean(c)) / stats::sd(c))
}

#' Standarize each column of data to have a range of (0, 1).
#' 
#' @param data Numeric matrix or data.frame of the observations.
#' @export
#' @examples 
#' scale_10(data = wine[, 2:14])
scale_10 <- function(data){
  tourr::rescale(data)
}
