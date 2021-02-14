#' Creates the array of target basis for a stepwise tour
#' 
#' The basis array starts with the supplied `basis` and removes the variable 
#' with the next lowest contribution to the univariate `measure`.
#' 
#' @param basis A full (p x 2) basis to start removing variables from.
#' @param data The data to project.
#' @param measure A univariate function to apply to each of the variables of 
#' the data. Defaults to variance, stats::var.
#' @param decreasing Whether or not the measure should be in decreasing order.
#' Defaults to TRUE. Removing a variable will operate in the opposite order.
#' @return A basis array containing the target bases of a stepwise tour, 
#' data and measure table are included as attributes. Given the custom classes
#' "history_array" for use by `tourr::interpolate` and "stepwise_array" to 
#' differentiate from returns of `tourr::save_history`.
#' @seealso \code{\link[spinifex]{stepwise_add}}
#' \code{\link[spinifex]{stepwise_remove}}
#' @export
#' 
#' @examples
#' dat_std <- scale_sd(wine[, 2:14])
#' bas <- basis_pca(dat)
#' 
#' sw_path <- stepwise_history(basis = bas, data = dat_std)
#' str(sw_path)
#' 
#' sw_path <- stepwise_history(basis = bas, data = dat_std,
#'                             measure = e1071::skewness, decreasing = FALSE)
#' str(sw_path)
#' 
#' \dontrun{
#'   play_tour_path(tour_path = sw_path, data = dat_std)
#' }
stepwise_history <- function(basis = NULL,
                          data = NULL,
                          measure = stats::var,
                          decreasing = TRUE){
  if(is.null(basis) == TRUE){
    message("basis is NULL, initalizing random basis.")
    basis <- tourr::basis_random(curr_dim, 2L)
  }
  
  ## Initialize
  data <- as.matrix(data)
  p <- ncol(data)
  if(all(dim(basis) != c(p, 2L)) == TRUE)
    stop("Dimension of the starting basis not [p x 2].")
  
  ## Measure table, ordered
  m   <- apply(data, 2L, measure)
  ord <- order(m, decreasing = decreasing)
  m   <- m[ord]
  cn  <- colnames(data)[ord]
  ## Reorder basis, so we can operate on the last var without issue.
  basis_ord <- basis[ord, ]
  measure_tbl <-
    data.frame(var_nm = factor(cn, levels = cn),
               var_ord = ord,
               var_measure = m,
               cumsum_measure = cumsum(m) / sum(m)
    )
  
  ## The work, making the basis list
  basis_array <- array(0L, dim = c(p, 2L, p))
  basis_array[,, p] <- as.matrix(basis_ord)
  mute_apply <- sapply((p - 1L):1L, function(i){ ## i is the current basis, this_* operates on i + 1
    this_basis <- basis_array[,, i + 1L]
    this_mv <- i + 1L
    this_theta <- atan(this_basis[this_mv, 2L] / this_basis[this_mv, 1L]) ## Radial
    this_phi   <- .5 * pi + acos(sqrt(this_basis[this_mv, 1L]^2L + this_basis[this_mv, 2L]^2L)) ## Negative of the current phi
    this_m_sp  <- create_manip_space(basis = this_basis, manip_var = this_mv)
    new_bas    <- rotate_manip_space(manip_space = this_m_sp,
                                     theta = this_theta, phi = this_phi)[, 1L:2L]
    basis_array[,, i] <<- tourr::orthonormalise(new_bas)
  }) 
  
  basis_array <- as_historty_array(basis_array, dat)
  attr(basis_array, "measure_tbl") <- measure_tbl
  class(basis_array) <- c(class(basis_array), "stepwise_array")
  return(basis_array)
}

##TODO, WHEN THERE IS EXCLUSIVELY 1 NEG PROJ COMPONENT IS DOESN"T GET ZEROED CORRECTLY.
### MANUAL TESTING ------
if(F){ 
  library(spinifex);
  dat = tourr::flea[, 2:6];
  bas = basis_pca(dat);
  sw_hist < stepwise_history(basis = bas, data = dat)
  
  ## aede2(+/+) zeros,
  ## head(-/+) doesn't zero (which then causes an issue)
  ## tars2(-/+ !?!) zeros, why? may not be as simple as quadrant.
  
  ###TODO need to look at phi and theta 
  ## consider .$basis_set[,, 2]; head didn't 0 out correctly;
  tgt <- as.data.frame(sw_hist$basis_set[,, 2])
}

