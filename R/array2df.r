#' Turns a tour path array into a long data frame
#'
#' Typically called by a wrapper function, `play_manual_tour` or 
#' `play_tour_path`. Takes the result of `tourr::save_history()` or 
#' `manual_tour()` and restuctures the data from an array to a long data frame 
#' for use in ggplots.
#'
#' @param array A (p, d, n_slides) array of a tour, the output of 
#'   `manual_tour()`.
#' @param data Optional, (n, p) dataset to project, consisting of numeric 
#'   variables.
#' @param lab Optional, labels for the reference frame of length 1 or the 
#'   number of variables used. Defaults to an abbriviation of the variables.
#' @return A list containing the (p, d, n_slides) basis slides array, and
#'   the (n, d, n_slides) data slides array.
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[, 1:6])
#' 
#' rb <- basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
#' array2df(array = mtour, data = flea_std)
array2df <- function(array, 
                     data = NULL,
                     lab = NULL) {
  # Initialize
  manip_var <- attributes(array)$manip_var
  p <- nrow(array[,, 1])
  n_slides <- dim(array)[3]
  
  # basis; array to long df
  basis_slides <- NULL
  for (slide in 1:n_slides) {
    basis_rows <- data.frame(cbind(array[,, slide], slide))
    basis_slides <- rbind(basis_slides, basis_rows)
  }
  colnames(basis_slides) <- c("x", "y", "slide")
  
  # data; if exists,  array to long df
  if(!is.null(data)) {
    data <- as.matrix(data)
    data_slides <- NULL
    for (slide in 1:n_slides) {
      data_rows <- cbind(data %*% array[,, slide], slide)
      data_rows[, 1] <- scale(data_rows[, 1], scale = FALSE)
      data_rows[, 2] <- scale(data_rows[, 2], scale = FALSE)
      data_slides <- data.frame(rbind(data_slides, data_rows))
    }
    colnames(data_slides) <- c("x", "y", "slide")
  }
  
  # Add labels, attribute, and list
  lab <- 
    if(!is.null(lab)){
      rep(lab, nrow(basis_slides) / length(lab))
    } else {
      if(!is.null(data)) {abbreviate(colnames(data), 3)
      } else {paste0("V", 1:p)}}
  basis_slides$lab <- rep(lab, n_slides)
  
  attr(basis_slides, "manip_var") <- manip_var
  
  slides <- if(!is.null(data)) {
    list(basis_slides = basis_slides, data_slides = data_slides)
  } else list(basis_slides = basis_slides)
  
  slides
}

