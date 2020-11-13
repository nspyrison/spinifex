## spinifex-deprecated.r
#' @title Deprecated functions in package \pkg{spinifex}.
#' @description The functions listed below are deprecated and will be defunct in
#'   the near future. When possible, alternative functions with similar
#'   functionality are also mentioned. Help pages for deprecated functions are
#'   available at \code{help("<function>-deprecated")}.
#' @name spinifex-deprecated
#' @keywords internal
NULL







##
## Below is just comments, not live roxygen2 documentation -----
##


##
## color_of() AND shape_of() DEPRICATED with use of aes_args and identity_args
##

# #' Return hex color code for a given discrete categorical variable.
# #' 
# #' @param class The discrete categorical variable to return the color of.
# #' @param pallet_name The name of the `RColorBrewer` pallet to get the colors
# #' from. Defaults to "Dark2".
# #' @return Vector of character hex color code of the passed categorical variable.
# #' @export
# #' @examples 
# #' color_of(tourr::flea$species)
# color_of <- function(class, pallet_name = "Dark2") {
#   class <- as.factor(class)
#   .l_lvls <- length(levels(class))
#   if (.l_lvls == 0L) stop("Length of 'class' cannot be zero.")
#   if (.l_lvls > 12L) stop("'class' has more than the expected max of 12 levels.")
#   pal <- suppressWarnings(RColorBrewer::brewer.pal(.l_lvls, pallet_name))
#   pal[as.integer(factor(class))]
# }
# #' Return shape integers for a given discrete categorical variable.
# #' 
# #' @param class The discrete categorical variable to return the shape of.
# #' @return Vector of integer shape values of the discrete categorical variable.
# #' @export
# #' @examples 
# #' shape_of(tourr::flea$species)
# shape_of <- function(class) {
#   class <- as.factor(as.vector(class))
#   .shape_ord <- c(21L:25L, 3L:4L, 7L:14L)
#   .l_shapes  <- length(unique(.shape_ord))
#   class <- as.factor(class)
#   .l_classes <- length(levels(class))
#   if (.l_classes == 0L) stop("Length of 'class' cannot be zero.")
#   if (.l_classes > 12L)
#     stop(paste0("'class' has more than the expected max of ", .l_shapes, " levels."))
#   .int_lvls <- as.integer(class)
#   .shape_ord[.int_lvls]
# }



## SHAPE_OF AND COLOR_OF Depricated with aes_args, identity_args
# #' Return hex color code for a given discrete categorical variable.
# #' 
# #' @param class The discrete categorical variable to return the color of.
# #' @param pallet_name The name of the `RColorBrewer` pallet to get the colors
# #' from. Defaults to "Dark2".
# #' @return Vector of character hex color code of the passed categorical variable.
# #' @export
# #' @examples 
# #' color_of(tourr::flea$species)
# color_of <- function(class, pallet_name = "Dark2"){
#   class <- as.factor(class)
#   .l_lvls <- length(levels(class))
#   if (.l_lvls == 0L) stop("Length of 'class' cannot be zero.")
#   if (.l_lvls > 12L) stop("'class' has more than the expected max of 12 levels.")
#   pal <- suppressWarnings(RColorBrewer::brewer.pal(.l_lvls, pallet_name))
#   pal[as.integer(factor(class))]
# }
# #' Return shape integers for a given discrete categorical variable.
# #' 
# #' @param class The discrete categorical variable to return the shape of.
# #' @return Vector of integer shape values of the discrete categorical variable.
# #' @export
# #' @examples 
# #' shape_of(tourr::flea$species)
# shape_of <- function(class) {
#   class <- as.factor(as.vector(class))
#   .shape_ord <- c(21L:25L, 3L:4L, 7L:14L)
#   .l_shapes  <- length(unique(.shape_ord))
#   class <- as.factor(class)
#   .l_classes <- length(levels(class))
#   if (.l_classes == 0L) stop("Length of 'class' cannot be zero.")
#   if (.l_classes > 12L)
#     stop(paste0("'class' has more than the expected max of ", .l_shapes, " levels."))
#   .int_lvls <- as.integer(class)
#   .shape_ord[.int_lvls]
# }
