#' The last basis of a guided tour
#' 
#' @param basis A full (p x 2) basis to start removing variables from.
#' @param data The data to project.
#' @param measure A univariate function to apply to each of the variables of 
#' the data. Defaults to the variance, stats::var.
#' @param curr_dim The target number of variables left with non-zero 
#' contributions. Defaults to the full dimensionality, ncol(data).
#' @param decreasing Whether or not the measure should be in decreasing order.
#' Defaults to TRUE. Removing a variable will operate in the opposite order.
#' @return A list, containing the data, measure table, basis list, and current 
#' dimensionality. This is the object tracks the current and previous history of 
#' the stepwise tour.
#' @seealso \code{\link[spinifex]{stepwise_add}}
#' \code{\link[spinifex]{stepwise_remove}}
#' @export
#' 
#' @examples
#' dat <- wine[, 2:14]
#' bas <- basis_pca(dat)
#' 
#' sw_hist <- stepwise_path(basis = bas, data = dat)
#' str(sw_hist)
#' 
#' sw_hist <- stepwise_path(basis = bas, data = dat, measure = median,
#'                          curr_dim = 5, decreasing = FALSE)
#' str(sw_hist)

stepwise_path <- function(basis = NULL,
                          data = NULL,
                          measure = stats::var,
                          curr_dim = ncol(data), ##needed or no? maybe a start dim?
                          decreasing = TRUE){
  if(is.null(basis) == TRUE){basis <- tourr::basis_random(curr_dim, 2)}
  
  ## Initialize
  data <- as.matrix(data)
  p <- ncol(data)
  if(all(dim(basis) != c(p, 2L)) == TRUE)
    stop("Dimension of the starting basis not [p x 2].")
  
  ## Measure table, ordered
  m   <- apply(data, 2, measure)
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
  basis_set <- array(0L, dim = c(p, 2L, p))
  basis_set[,, p] <- basis_ord
  mute_apply <- sapply((p - 1L):1L, function(i){ ## i is the dim of the new_bas, this_* operates on i + 1
  #for(i in (p - 1L):2L){
    this_basis <- basis_set[,, i + 1L]
    this_mv <- i + 1L
    ##TODO THE LINE ABOVE IS GOING TO BE OFF AT SOME PT BECUSE WE ARE CHANGING THE DIM FO THE BASIS, ALSO NEED TO UPDATE THE MEASURE_TBL.
    this_theta <- atan(this_basis[this_mv, 2L] / this_basis[this_mv, 1L]) ## Radial
    this_phi   <- .5 * pi + acos(sqrt(this_basis[this_mv, 1L]^2L + this_basis[this_mv, 2L]^2L)) ## Negative of the current phi
    this_m_sp  <- create_manip_space(basis = this_basis, manip_var = this_mv)
    new_bas    <- rotate_manip_space(manip_space = this_m_sp,
                                     theta = this_theta, phi = this_phi)[, 1L:2L]
    
    basis_set[,, i] <<- round(new_bas, 5)
  }) ##TODO, WHEN THERE IS EXCLUSIVELY 1 NEG PROJ COMPONENT IS DOESN"T GET ZEROED CORRECTLY.
  
  list(data = data,
       measure_tbl = measure_tbl,
       basis_set = basis_set,
       curr_dim = curr_dim)
}

### MANUAL TESTING ------
if(F){ 
  library(spinifex);
  dat = tourr::flea[, 2:6];
  bas = basis_pca(dat);
  stepwise_path <- stepwise_path(basis = bas, data = dat)
  
  ## aede2(+/+) zeros,
  ## head(-/+) doesn't zero (which then causes an issue)
  ## tars2(-/+ !?!) zeros, why? may not be as simple as quadrant.
  
  ###TODO need to look at phi and theta 
  ## consider .$basis_set[,, 2]; head didn't 0 out correctly;
  tgt <- as.data.frame(sw_hist$basis_set[,, 2])
  
} 