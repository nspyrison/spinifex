##
## MATH AND TRANSFORMS -----
##
#' Orthonormality of a matrix
#' 
#' Test if a numeric matrix is orthonormal, that is, each column is orthogonal,
#' at a right angle with the others, and each column has a norm 
#' length of 1. This must be true for a projection to be linear.
#'
#' @param x Numeric matrix to test the orthonormality of.
#' @param tol Max tolerance of floating point differences of the
#' element-wise distance of t(x) %*% x from the identity matrix.
#' @return Single logical, whether or not the matrix is orthonormal.
#' @export
#' @examples 
#' is_orthonormal(tourr::basis_random(n = 6))
#' is_orthonormal(matrix(1:12, ncol = 2), tol = 0.01)
is_orthonormal <- function(x, tol = 0.001) {
  if(is.numeric(x) == FALSE) stop("'x', expected to be numeric and coercable to matrix.")
  x <- as.matrix(x)
  actual <- t(x) %*% x ## Collapses to identity matrix IFF x is orthonormal
  expected <- diag(ncol(x))
  if(max(abs(actual - expected)) < tol) TRUE else FALSE
}


#' Turns a tour path array into a long data frame.
#'
#' Internal function, many end users will not need this. Takes the result of 
#' `manual_tour()` or `tourr::save_history()`. Restructures the array of 
#' interpolated bases into a long data frame for use in ggplots.
#'
#' @param basis_array A full (p, d, n_frames) interpolated basis array of a 
#' tour, the output of `manual_tour` or `save_history(*_tour())`.
#' @param data Optional, (n, p) dataset to project, consisting of numeric 
#' variables.
#' @param basis_label Labels for basis display, a character 
#' vector with length equal to the number of variables.
#' Defaults to NULL; 3 character abbreviation from colnames of data or
#' rownames of basis.
#' @param data_label Labels for `plotly` tooltip display. 
#' Defaults to the rownames of data. If null, initializes to 1:nrow(data).
#' @param do_center_frame Whether or not to center the mean within each 
#' animation frame. Defaults to TRUE.
#' @export
#' @examples
#' ## !!This function is not meant for external use!!
#' dat_std <- scale_sd(wine[, 2:6])
#' clas <- wine$Type
#' bas <- basis_pca(dat_std)
#' mv <- manip_var_of(bas)
#' 
#' ## Radial tour array to long df, as used in play_manual_tour()
#' mt_array <- manual_tour(basis = bas, manip_var = mv)
#' ls_df_frames <- array2df(basis_array = mt_array, data = dat_std,
#'                          basis_label = paste0("MyLabs", 1:nrow(bas)))
#' str(ls_df_frames)
#' 
#' ## tourr::save_history tour array to long df, as used in play_tour_path()
#' gt_array <- tourr::save_history(data = dat_std, max_bases = 10)
#' ls_df_frames2 <- array2df(basis_array = gt_array, data = dat_std)
#' str(ls_df_frames2)
array2df <- function(
  basis_array,
  data            = NULL,
  basis_label     = NULL,
  data_label      = rownames(data),
  do_center_frame = TRUE
){
  ## Initialize
  p <- nrow(basis_array)
  d <- ncol(basis_array)
  n_frames <- dim(basis_array)[3L]
  if("history_array" %in% class(basis_array)) class(basis_array) <- "array"
  manip_var <- attributes(basis_array)$manip_var ## NULL means tourr tour
  ## Condition handle basis labels.
  if(is.null(basis_label)){
    if(is.null(data) == FALSE){
      basis_label <- abbreviate(
        gsub("[^[:alnum:]=]", "", colnames(data)), 3L)
    }else basis_label <- abbreviate(
      gsub("[^[:alnum:]=]", "", rownames(basis_array)), 3L)
    if(is.null(basis_label)) basis_label <- paste0("v", 1L:p)
  }
  
  ## Basis frames df
  basis_frames <- NULL 
  .mute <- sapply(1L:n_frames, function(i){
    basis_rows <- cbind(basis_array[,, i], i)
    basis_frames <<- rbind(basis_frames, basis_rows)
  })
  basis_frames <- as.data.frame(basis_frames)
  .nms <- c("x", "y", "z", "w")
  if(d > 4L) .nms <- c(.nms, paste0("y"))
  colnames(basis_frames) <- c(.nms[1L:d], "frame")
  ## Basis label and manip_var attribute.
  if(length(basis_label) > 0L)
    basis_frames$tooltip <- rep_len(basis_label, nrow(basis_frames))
  attr(basis_frames, "manip_var") <- manip_var
  
  ## Basis frames df; if exists
  if(is.null(data) == FALSE){
    if(ncol(data) != p)
      stop(paste0(
        "array2df: Non-conformable matrices; data has ", ncol(data),
        " columns while basis has ", p, " rows."))
    data <- as.matrix(data)
    data_frames <- NULL
    .mute <- sapply(1L:n_frames, function(i){
      new_frame <- data %*% matrix(basis_array[,, i], p, d)
      if(do_center_frame)
        new_frame <- apply(new_frame, 2L, function(c){(c - mean(c))})
      new_frame <- cbind(new_frame, i)
      data_frames <<- rbind(data_frames, new_frame) ## Add rows to df
    })
    data_frames[, 1L:d] <- data_frames[, 1L:d] %>% scale_01()
    data_frames <- as.data.frame(data_frames)
    colnames(data_frames) <- c(.nms[1L:ncol(basis_array)], "frame")
    ## Data label rep if applicable
    if(is.null(data_label)) data_label <- 1L:nrow(data)
    if(length(data_label) > 0L)
      data_frames$tooltip <- rep_len(data_label, nrow(data_frames))
  }
  
  ## Return, include data if it exists
  if(exists("data_frames")){
    ret <- list(basis_frames = basis_frames, data_frames = data_frames)
  } else
    ret <- list(basis_frames = basis_frames)
  ret
}


#' Returns the axis scale and position.
#' 
#' Internal function. Typically called by other functions to scale the position
#' of the axes data.frame or another data.frame to plot relative to the data.
#' 
#' @param x Numeric matrix or data.frame, first 2 columns and scaled and offset 
#' the `to` object.
#' @param position Text specifying the position the axes should go to.
#' Defaults to "center" expects one of: c("center", "left", "right", 
#' "bottomleft", "topright", "off", "full", "top1d", "floor1d", "bottom1d",
#' "full", "facetleft", "facetright", "facettop", "facetbottom").
#' @param to Data.frame to scale to.
#' Based on the min/max of the first 2 columns. If left NULL defaults to 
#' data.frame(x = c(0, 1), y = c(0, 1).
#' @return Transformed values of `x`, dimension and class unchanged.
#' @seealso \code{\link{map_absolute}} for more manual control.
#' @export
#' @family linear mapping functions
#' @examples
#' ## !!This function is not meant for external use!!
#' rb <- tourr::basis_random(4, 2)
#' 
#' map_relative(x = rb, position = "bottomleft")
#' map_relative(x = rb, position = "right", to = wine[, 2:3])
map_relative <- function(
  x,
  position = c("center", "left", "right",
               "bottomleft", "topright", "off",
               "top1d", "floor1d", "bottom1d",
               "full", "facetleft", "facetright",
               "facettop", "facetbottom"),
  to = NULL
){
  ## Assumptions
  position <- match.arg(position)
  if(position == "off" | is.null(x)) return()
  if(is.null(to)) to <- data.frame(x = c(0L, 1L), y = c(0L, 1L))
  
  ## Initialize
  xrange  <- range(to[, 1L])
  yrange  <- range(to[, 2L])
  xdiff   <- diff(xrange)
  ydiff   <- diff(yrange)
  xcenter <- xrange[1L] + .5 * xdiff
  ycenter <- yrange[1L] + .5 * ydiff
  
  ## Condition handling of position
  if(position == "center"){
    scale <- .4 * min(xdiff, ydiff)
    xoff  <- xcenter
    yoff  <- ycenter
  } else if(position == "left"){
    scale <- .3   * min(xdiff, ydiff)
    ## .5 + scale coef will have the circle touch lower range
    xoff  <- -.85 * xdiff + xcenter 
    yoff  <- ycenter
  } else if(position == "right"){
    scale <- .3  * min(xdiff, ydiff)
    xoff  <- .85 * xdiff + xcenter
    yoff  <- ycenter
  } else if(position %in% c("top1d")){
    xscale <- .3 * xdiff
    yscale <- ydiff
    xoff   <- xcenter
    yoff   <- .5 * ydiff + ycenter
  } else if(position %in% c("floor1d")){
    xscale <- .3 * xdiff
    yscale <- ydiff
    xoff   <- xcenter
    yoff   <- -.6 * ydiff + ycenter
  } else if(position %in% c("bottom1d")){
    xscale <- .3 * xdiff
    yscale <- ydiff
    xoff   <- xcenter
    yoff   <- -1.8 * ydiff + ycenter
  } else if(position == "bottomleft"){
    scale <- .25  * min(xdiff, ydiff)
    xoff  <- -.25 * xdiff + xcenter
    yoff  <- -.5  * ydiff + ycenter
  } else if(position == "topright"){
    scale <- .25 * min(xdiff, ydiff)
    xoff  <- .25 * xdiff + xcenter
    yoff  <- .5  * ydiff + ycenter
  } else if(position == "full"){
    scale <- .5 * min(xdiff, ydiff)
    xoff  <- xcenter
    yoff  <- ycenter
  } else if(position == "facetleft"){
    scale <- .5 * min(xdiff, ydiff)
    xoff  <- xcenter - .5025 * xdiff
    yoff  <- ycenter
  } else if(position == "facetright"){
    scale <- .5 * min(xdiff, ydiff)
    xoff  <- xcenter + .5025 * xdiff
    yoff  <- ycenter
  } else if(position == "facettop"){
    scale <- .5 * min(xdiff, ydiff)
    xoff  <- xcenter
    yoff  <- ycenter + .5025 * ydiff
  } else if(position == "facetbottom"){
    scale <- .5 * min(xdiff, ydiff)
    xoff  <- xcenter
    yoff  <- ycenter - .5025 * ydiff
  } else stop(paste0("position: ", position, " not defined."))
  
  ## Apply scale and return
  if(position %in% c("top1d", "floor1d", "bottom1d")){
    ## 1d basis with x&y scales
    x[, 1L] <- xscale * x[, 1L] + xoff
    x[, 2L] <- yscale * x[, 2L] + yoff
  }else{
    ## 2d basis with 1 scale
    x[, 1L] <- scale * x[, 1L] + xoff
    x[, 2L] <- scale * x[, 2L] + yoff
  }
  
  x
}

#' @rdname spinifex-deprecated
#' @section \code{scale_axes}:
#' For \code{scale_axes}, use \code{\link{map_relative}}.
#' @export
scale_axes <- function(...) {
  .Deprecated("map_relative")
  map_relative(...)
}

#' Manually offset and scale the first 2 columns of a matrix or data.frame.
#' 
#' A manual variant of `map_relative()`. Can be used as the `axes` argument 
#' to manually set the size and locations of the axes.
#' 
#' @param offset 2 Numeric values to offset/pan the first 2 dimensions of `x`.
#' @param scale 2 Numeric values to scale/zoom to the first 2 dimensions of `x`.
#' @param x Numeric data object with 2 columns to scale and offset.
#' Defaults to NULL, passing arguments to scale_axes for use internally.
#' @return Scaled and offset `x`.
#' @seealso \code{\link{scale_axes}} for preset choices.
#' @export
#' @family linear mapping functions
#' @examples 
#' bas <- tourr::basis_random(4, 2)
#' 
#' map_absolute(bas, offset = c(-2, 0), scale = c(2/3, 2/3))
map_absolute <- function(x,
                         offset = c(0, 0),
                         scale  = c(1, 1)
){
  ret <- x
  ret[, 1L] <- ret[, 1L] * offset[1L] + scale[1L]
  ret[, 2L] <- ret[, 2L] * offset[2L] + scale[2L]
  ret
}

#' @rdname spinifex-deprecated
#' @section \code{pan_zoom}:
#' For \code{pan_zoom}, use \code{\link{map_absolute}}.
#' @export
pan_zoom <- function(x, pan = c(0, 0), zoom = c(1, 1)) {
  .Deprecated("map_absolute")
  map_absolute(x, pan, zoom)
}

#' Preprocess numeric variables
#' 
#' Centers and scales each column by standard deviation (sd) or to the 
#' interval (0, 1).
#' 
#' @param data Numeric matrix or data.frame of the observations.
#' @export
#' @examples
#' scale_sd(data = wine[, 2:6])
scale_sd <- function(data){
  if(is.null(dim(data))) data <- matrix(data) ## As columnar matrix
  apply(data, 2L, function(c){(c - mean(c)) / stats::sd(c)})
}

#' @rdname scale_sd
#' @export
#' @examples
#' scale_01(data = wine[, 2:6])
scale_01 <- function(data){
  if(is.null(dim(data))) data <- matrix(data) ## As columnar matrix
  if(nrow(data) == 1L) return(matrix(0L, ncol = ncol(data)))
  apply(data, 2L, function(c) (c - min(c)) / diff(range(c)))
}

# ## <<experimental>> Mutes verbose functions, without suppressing warnings or error,
# ## wrapper function for .mute <- capture.output(x <- value)
# #' @examples 
# #' ## mute assignment
# #' mute(gt <- tourr::save_history(mtcars, max_bases = 3))
# mute <- function(...){
#   .mute <- capture.output(
#     ret <- for (i in seq_len(...length())) {
#       out <- withVisible(...elt(i))
#       if (out$visible)
#         print(out$value)
#     }
#   )
# }




## attempt 2 with %+replace%;
# doesn't resolve needless warnings: 
# as scale_color_brewer is not a theme obj, can't use %+replace%
###
# #' @import ggplot2
# theme_spinifex2 <- function(){
#   .theme <- theme_void() %+replace%
#     theme(legend.position = "bottom",
#           legend.direction = "horizontal", ## Levels within aesthetic
#           legend.box = "vertical",         ## Between aesthetic
#           legend.margin = margin(0L,0L,0L,0L), ## Tighter legend margin
#           axis.title = element_text() ## Allow axis titles, though defaulted to blank
#     )
#   list(.theme,
#        scale_color_brewer(palette = "Dark2"),
#        scale_fill_brewer(palette = "Dark2"),
#        coord_fixed(),
#        labs(x = "", y = ""))
# }





##
## BASIS AND MANIP VAR HELPERS -----
##

#' The basis of Principal Component Analysis (PCA)
#' 
#' The orthogonal linear components of the variables in the next largest 
#' direction of variance.
#' 
#' @param data Numeric matrix or data.frame of the observations.
#' @param d Number of dimensions in the projection space.
#' @seealso \code{\link[Rdimtools:do.pca]{Rdimtools::do.pca}}
#' @export
#' @family basis producing functions
#' @examples
#' dat_std <- scale_sd(wine[, 2:6])
#' basis_pca(data = dat_std)
basis_pca <- function(data, d = 2){
  #ret <- stats::prcomp(data)$rotation[, 1L:d, drop = FALSE]
  ret <- Rdimtools::do.pca(X = as.matrix(data), ndim = d)$projection
  rownames(ret) <- colnames(data)
  colnames(ret) <- paste0("PC", 1:d)
  ret
}


#' The basis of Orthogonal Linear Discriminant Analysis (OLDA)
#' 
#' Orthogonal LDA (OLDA) is an extension of classical LDA where the discriminant 
#' vectors are orthogonal to each other.
#' 
#' @param data Numeric matrix or data.frame of the observations, coerced to matrix.
#' @param class The class for each observation, coerced to a factor.
#' @param d Number of dimensions in the projection space.
#' @return A numeric matrix, an orthogonal basis that best distinguishes the 
#' group means of `class`.
#' @seealso \code{\link[Rdimtools:do.olda]{Rdimtools::do.olda}}
#' @references
#' Ye J (2005). "Characterization of a Family of Algorithms for Generalized 
#' Discriminant Analysis on Undersampled Problems." J. Mach. Learn. Res., 
#' 6, 483-502. ISSN 1532-4435.
#' @export
#' @family basis producing functions
#' @examples 
#' dat_std <- scale_sd(wine[, 2:6])
#' clas    <- wine$Type
#' basis_olda(data = dat_std, class = clas)
basis_olda <- function(data, class, d = 2){
  #lda <- MASS::lda(class ~ ., data = data.frame(data, class))$scaling
  #ret <- tourr::orthonormalise(lda)[, 1L:d, drop = FALSE]
  ret <- Rdimtools::do.olda(X = as.matrix(data),
                            label = as.factor(class),
                            ndim = d)$projection
  rownames(ret) <- colnames(data)
  colnames(ret) <- paste0("oLD", 1:d)
  ret
}

#' The basis of Orthogonal Discriminant Projection (ODP)
#' 
#' Orthogonal Discriminant Projection (ODP) is a linear dimension reduction 
#' method with class supervision. It maximizes weighted difference between local
#' and non-local scatter while local information is also preserved by 
#' constructing a neighborhood graph.
#' 
#' @param data Numeric matrix or data.frame of the observations, coerced to matrix.
#' @param class The class for each observation, coerced to a factor.
#' @param d Number of dimensions in the projection space.
#' of `class`.
#' @param type A vector specifying the neighborhood graph construction. 
#' Expects; `c("knn", k)`, `c("enn", radius)`, or `c("proportion",ratio)`. 
#' Defaults to `c("knn", sqrt(nrow(data)))`, nearest neighbors equal to the 
#' square root of observations.
#' @param ... Optional, other arguments to pass to \code{\link[Rdimtools:do.odp]{Rdimtools::do.odp}}.
#' @seealso \code{\link[Rdimtools:do.odp]{Rdimtools::do.odp}} for locality
#' preservation arguments.
#' @seealso \code{\link[Rdimtools:aux.graphnbd]{Rdimtools::aux.graphnbd}} for 
#' details on `type`.
#' @references
#' Li B, Wang C, Huang D (2009). "Supervised feature extraction based on 
#' orthogonal discriminant projection." Neurocomputing, 73(1-3), 191-196.
#' @export
#' @family basis producing functions
#' @examples 
#' dat_std <- scale_sd(wine[, 2:6])
#' clas <- wine$Type
#' basis_odp(data = dat_std, class = clas)
basis_odp <- function(data, class, d = 2, type = c("proportion", 0.1), ...){
  ret <- Rdimtools::do.odp(X = as.matrix(data),
                           label = as.factor(class),
                           ndim = d,
                           type = type,
                           ...)$projection
  rownames(ret) <- colnames(data)
  colnames(ret) <- paste0("ODP", 1L:d)
  ret
}

#' The basis of Orthogonal Neighborhood Preserving Projection (OLPP)
#' 
#' Orthogonal Neighborhood Preserving Projection (ONPP) is an unsupervised 
#' linear dimension reduction method. It constructs a weighted data graph from 
#' LLE method. Also, it develops LPP method by preserving the structure of local 
#' neighborhoods. For the more details on `type` see 
#' \code{\link[Rdimtools:aux.graphnbd]{Rdimtools::aux.graphnbd()}}.
#' 
#' @param data Numeric matrix or data.frame of the observations, coerced to matrix.
#' @param d Number of dimensions in the projection space.
#' @param type A vector specifying the neighborhood graph construction. 
#' Expects; `c("knn", k)`, `c("enn", radius)`, or `c("proportion",ratio)`. 
#' Defaults to `c("knn", sqrt(nrow(data)))`, nearest neighbors equal to the 
#' square root of observations.
#' @return Orthogonal matrix basis that distinguishes the levels of `class` 
#' based on local and non-local variation as weighted against the neighborhood 
#' graph.
#' @seealso \code{\link[Rdimtools:do.onpp]{Rdimtools::do.onpp}}
#' @seealso \code{\link[Rdimtools:aux.graphnbd]{Rdimtools::aux.graphnbd}} for 
#' details on `type`.
#' @references
#' He X (2005). Locality Preserving Projections. PhD Thesis, 
#' University of Chicago, Chicago, IL, USA.
#' @export
#' @family basis producing functions
#' @examples
#' dat_std <- scale_sd(wine[, 2:6])
#' basis_onpp(data = dat_std)
basis_onpp <- function(data, d = 2, type = c("knn", sqrt(nrow(data)))){
  ret <- Rdimtools::do.onpp(X = as.matrix(data),
                            ndim = d,
                            type = type)$projection
  rownames(ret) <- colnames(data)
  colnames(ret) <- paste0("ONPP", 1L:d)
  ret
}


#' Solve for the last basis of a guided tour.
#' 
#' Performs simulated annealing on the index function, solving for it's local
#' extrema. Returns only the last identified basis of the optimization. A 
#' truncated, muted extension of tourr::save_history(guided_tour())).
#' 
#' @param data Numeric matrix or data.frame of the observations.
#' @param index_f The index function to optimize.
#' `{tourr}` exports `holes()`, `cmass()`, and `lda_pp(class)`.
#' @param d Number of dimensions in the projection space.
#' @param ... Optional, other arguments to pass to 
#' \code{\link[tourr:guided_tour]{tourr::guided_tour}}
#' @return Numeric matrix of the last basis of a guided tour.
#' @seealso \code{\link[tourr:guided_tour]{tourr::guided_tour}} for annealing 
#' arguments.
#' @export
#' @family basis producing functions
#' @examples 
#' dat_std <- scale_sd(wine[, 2:6])
#' basis_guided(data = dat_std, index_f = tourr::holes())
#' 
#' basis_guided(data = dat_std, index_f = tourr::cmass(),
#'              alpha = .4, cooling = .9, max.tries = 10, n_sample = 4)
basis_guided <- function(data, index_f = tourr::holes(), d = 2, ...){
  .mute <- utils::capture.output(
    hist <- tourr::save_history(
      data,
      tourr::guided_tour(index_f = index_f, d = d, ...))
  )
  ret <- matrix(hist[,, length(hist)], ncol = d)
  rownames(ret) <- colnames(data)
  ret
}


#' Create a basis that gives uniform contribution in a circle
#' 
#' Orthonormalizes uniform variable contributions on a unit circle. This
#' serves as a NULL basis, one that is variable agnostic while spacing the
#' variables to have minimize variable dependence.
#' 
#' @param data The data to create a basis for.
#' @export
#' @family basis producing functions
#' @examples 
#' dat_std <- scale_sd(wine[, 2:6])
#' bas <- basis_half_circle(dat_std)
basis_half_circle <- function(data){
  pp1 <- ncol(data) + 1L ## p++
  arc <- seq(0L, pi, length.out = pp1)[-pp1]
  u_circ <- as.matrix(data.frame(y1 = sin(arc), y2 = cos(arc)))
  bas <- tourr::orthonormalise(u_circ)
  rownames(bas) <- colnames(data)
  colnames(bas) <- paste0("half_circ", 1L:2L)
  bas
}


#' Suggest a manipulation variable.
#' 
#' Find the column number of the variable with the `rank`-ith largest 
#' contribution of the `basis`. 
#' Useful for identifying a variable to change the contribution of in a manual 
#' tour, it's `manip_var` argument.
#' 
#' @param basis Numeric matrix (p x d), orthogonal liner combinations of the 
#' variables.
#' @param rank The number, specifying the variable with the `rank`-th largest 
#' contribution. Defaults to 1.
#' @return Numeric scalar, the column number of a variable.
#' @export
#' @family manual tour adjacent functions
#' @examples 
#' ## Setup
#' dat_std <- scale_sd(wine[, 2:6])
#' bas <- basis_pca(dat_std)
#' 
#' manip_var_of(basis = bas) ## Variable with the largest contribution
#' manip_var_of(basis = bas, rank = 5) ## Variable with 5th-largest contribution
manip_var_of <- function(basis, rank = 1){
  if(spinifex::is_orthonormal(basis) == FALSE)
    warning("Supplied basis isn't orthonormal.")
  row_norm <- sqrt(apply(basis, 1L, function(c) {sum(c^2L)}))
  ret <- order(abs(row_norm), decreasing = TRUE)[rank]
  names(ret) <- rownames(basis)[rank]
  ret
}



#' Save a tour basis array.
#' 
#' Save a tour path so it can later be displayed in many different ways.
#' A wrapper function can mute the noisy text side effects of 
#' \code{\link[tourr:save_history]{tourr::save_history}}. Changes a few argument 
#' defaults differ: doesn't scale data columns to (0, 1), max_bases = 10, 
#' appends the start basis if tour_path is grand, it isn't already there, 
#' and has correct dim.
#'
#' @param data Matrix, or data frame containing complete numeric columns
#' @param tour_path Tour path generator. 
#' Defaults to \code{\link[tourr:grand_tour]{tourr::grand_tour}}.
#' @param max_bases The maximum number of new bases to generate. 
#' Some tour paths (like the guided tour) may generate less than the maximum. 
#' Defaults to 10.
#' @param start First basis, is appended as first frame grand tour if possible.
#' @param rescale Whether or not to rescale all variables to range (0,1). 
#' Defaults to FALSE.
#' @param sphere Whether or not to sphere (whiten) covariance matrix to the 
#' identity matrix. Defaults to FALSE. 
#' @param step_size Distance (in radians) between target frames (not 
#' interpolated frames). Defaults to Inf, forcing new basis generation at each 
#' step.
#' @param verbose Whether or not to suppress the text output side effects from 
#' \code{\link[tourr:save_history]{tourr::save_history}}. Defaults to FALSE.
#' @param ... Additional arguments passed to 
#' \code{\link[tourr:new_tour]{tourr::new_tour}}.
#' @seealso \code{\link[tourr:save_history]{tourr::save_history}} 
#' \code{\link[tourr:new_tour]{tourr::new_tour}}
#' \code{\link[tourr:grand_tour]{tourr::grand_tour}}
#' @export
#' @examples 
#' library(spinifex)
#' 
#' dat <- scale_sd(penguins_na.rm[, 1:4])
#' ## A grand tour path
#' gt_path <- save_history(data = dat, tour_path = grand_tour(), max_bases = 10)
#' dim(gt_path)
#' 
#' ## A 1d grand tour path
#' gt1d_path <- save_history(dat, grand_tour(d = 1), 10)
#' dim(gt1d_path)
#' 
#' ## A holes guided tour path
#' holes_path <- save_history(dat, guided_tour(holes(), max.tries = 100))
#' dim(holes_path)
save_history <- function(
  data,
  tour_path = tourr::grand_tour(),
  max_bases = 10,
  start     = NULL,
  rescale   = FALSE,
  sphere    = FALSE,
  step_size = Inf,
  verbose   = getOption("verbose"),
  ...){
  .expr <- expression(tourr::save_history(
    data, tour_path, max_bases, start, rescale, sphere, step_size, ...))
  ## Mutable version of tourr::save_history, with slightly different arg defaults
  if(verbose == FALSE){
    .mute <- utils::capture.output(
      ret <- eval(.expr)
    )
  } else ret <- eval(.expr)
  
  ## Append start as first target basis
  #### if tour_path is a grand tour, start isn't first frame, and dim match
  if(is.null(start) == FALSE)
    if(attr(tour_path, "name") == "grand" &
       #any(matrix(ret[,, 1L], ncol = 2) != start) & ## Check for same
       all(dim(start) == dim(ret)[1L:2L]))
      ret <- array(c(start, ret), dim = dim(ret) + c(0L, 0L, 1L))
  
  ret
}



## UTILITY ----

#' Set default color & fill for discrete variables
#' 
#' Masks ggplot2's default color/fill color palette for discrete variables.
#' 
#' @param ... Passes arguments to ggplot2::scale_colour/fill_brewer.
#' @export
scale_colour_discrete <- function(...){
  ggplot2::scale_colour_brewer(..., palette = "Dark2")
}
#' @rdname scale_colour_discrete
#' @export
scale_fill_discrete <- function(...){
  ggplot2::scale_fill_brewer(..., palette = "Dark2")
}
### continuous cases is not clear what you would have to do, 
##### also see cheem::color_scale_of

#' Theme spinifex
#' 
#' A ggplot2 theme suggested for linear projections with spinifex.
#' The default theme in spinifex functions.
#' 
#' @param ... Optionally pass arguments to `ggplot2::theme()`.
#' @seealso \code{\link[ggplot2:theme]{ggplot2::theme}} for all theme options.
#' @export
#' @import ggplot2
#' @examples 
#' theme_spinifex()
#' 
#' require("ggplot2")
#' ggplot(mtcars, aes(wt, mpg, color = as.factor(cyl))) +
#'   geom_point() + theme_spinifex()
theme_spinifex <- function(...){
  ## Color/fill discrete also masked to reduced warnings/messages
  list(
    theme_minimal(),
    theme(
      axis.text        = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.direction = "horizontal",        ## Levels within an aesthetic
      legend.box       = "vertical",          ## Between aesthetics
      legend.margin    = margin(0L,0L,0L,0L), ## Tighter legend margin
      panel.spacing    = unit(0L, "points"),  ## Facet spacing
      strip.text       = element_text(        ## Facet strip spacing
        margin = margin(b = 3L, t = 3L)),
      strip.background =                      ## Facet strip
        element_rect(size = .4, color = "grey20", fill = "grey90"),
      ...)                                    ## Ellipsis trumps defaults
  )
}


#' Check ggplot layers for use of a specific geom
#' 
#' Checks if any of the layers of a ggplot contain a specific class.
#' 
#' @param ggplot Check the layers of this ggplot object
#' @param class_nm The class name to check, note this differs slightly from
#' the name of the geom function. Defaults to "GeomDensity", checking to see if
#' geom_density was used in any of the layers.
#' @seealso \code{\link[ggplot2:theme]{ggplot2::theme}} for all theme options.
#' @export
#' @examples 
#' library(ggplot2)
#' library(spinifex)
#' 
#' g <- ggplot(mtcars, aes(disp, color = factor(cyl))) + 
#'   geom_density() + geom_histogram()
#' is_any_layer_class(g, "GeomDensity")
#' is_any_layer_class(g, "GeomPoint")
is_any_layer_class <- function(ggplot, class_nm = "GeomDensity"){
  any(
    sapply(seq_along(ggplot$layers), function(i){
      class_nm %in% class(ggplot$layers[[i]]$geom)
    })
  )
}


#' Development message
#' 
#' Send a message if the 4th chunk of the package version is 9000.
#' @param text A character string to message() if package version is _9000.
devMessage <- function(text){
  version4 <-  utils::packageVersion(pkg = "spinifex")[1L, 4L]
  if(is.na(version4) == FALSE)
    if(version4 == 9000L)
      message(paste0("devMessage: ", text))
}


# ### as_history_array ----
# #' Changes an array of bases into a "history_array" class for use 
# #' in `tourr::interpolate()`.
# #' 
# #' Internal function, many end users will not need this. Attaches data to an array and assigns the custom class "history_array" as 
# #' used in `tourr`. Typically called by basis arrays from `spinifex` functions.
# #' 
# #' @param basis_array An array of bases.
# #' @param data The data matrix to be projected through the basis. This is
# #' `tourr::save_history` objects, but not consumed downstream in `spinifex`.
# #' @return An array of numeric bases with custom class "history_array" for 
# #' consumption by `tourr::interpolate`.
# #' 
# #' @seealso \code{\link[tourr:save_history]{tourr::save_history}} for preset choices.
# #' @export
# #' @examples 
# #' ## !!This function is not meant for external use!!
# #' dat_std <- scale_sd(wine[, 2:6])
# #' bas <- basis_pca(dat_std)
# #' mv <- manip_var_of(bas)
# #' mt_array <- manual_tour(basis = bas, manip_var = mv)
# #' as_history_array(mt_array, dat_std)
# as_history_array <- function(basis_array, data = NULL){
#   if(length(data) > 0L)
#     attr(basis_array, "full") <- as.matrix(data)
#   class(basis_array) <- "history_array"
#   basis_array
# }