#' @param basis A full (p x 2) basis to start removing variables from.
#' @param data The data to project.
#' @param measure A univariate function to apply to each of the variables of 
#' the data.
#' @param curr_dim 
#' 
#' @examples
#' dat <- wine[, 2:14]
#' bas <- basis_pca(dat)
#' 
#' sw_ret <- stepwise_hist(basis = bas, data = dat)
#' 
#' 

if(F){
  library(spinifex); data = wine[, 2:14]; basis = basis_pca(data);
  measure = var; curr_dim = ncol(data); decreasing = TRUE;
}
stepwise_hist <- function(basis = NULL,
                          data = NULL,
                          measure = var,
                          curr_dim = ncol(data), ##needed or no? maybe a start dim?
                          decreasing = TRUE){
  if(is.null(basis) == TRUE){basis <- tourr::basis_random(curr_dim, 2)}
  
  ## Initialize
  data <- as.matrix(data)
  p <- ncol(data)
  if(all(dim(basis) != c(p, 2)) == TRUE)
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
  basis_ls <- list(NULL)
  basis_ls[[p]] <- basis_ord
  mute_apply <- sapply((p - 1L):1L, function(i){ ## i is the dim of the new_bas, this_* operates on i + 1
  #for(i in (p - 1L):2L){
    this_basis <- basis_ls[[i + 1L]]
    this_mv <- i + 1L
    ##TODO THE LINE ABOVE IS GOING TO BE OFF AT SOME PT BECUSE WE ARE CHANGING THE DIM FO THE BASIS, ALSO NEED TO UPDATE THE MEASURE_TBL.
    this_theta <- atan(this_basis[this_mv, 2L] / this_basis[this_mv, 1L]) ## Radial
    this_phi   <- .5 * pi + acos(sqrt(this_basis[this_mv, 1L]^2L + this_basis[this_mv, 2L]^2L)) ## Negative of the current phi
    this_m_sp  <- create_manip_space(basis = this_basis, manip_var = this_mv)
    new_bas    <- rotate_manip_space(manip_space = this_m_sp,
                                     theta = this_theta, phi = this_phi)[, 1L:2L]
    
    basis_ls[[i]] <<- round(new_bas, 5) ## Add next lvl assignment when vectorizing
  }) ##TODO, WHEN THERE IS EXCLUSIVELY 1 NEG PROJ COMPONENT IS DOESN"T GET ZEROED CORRECTLY.
  
  ret <- list(data = data,
              measure_tbl = measure_tbl,
              basis_ls = basis_ls,
              curr_dim = curr_dim)
  return(ret)
}



if(F){ ### MANUAL TESTING ------
  library(spinifex); library(spinifex); 
  dat = tourr::flea[, 2:6];
  bas = basis_pca(dat);
  stepwise_hist(basis = bas, data = dat)
  
  ## aede2(+/+) zeros, 
  ## head(-/+) doesn't zero (which then causes an issue)
  ## tars2(-/+ !?!) zeros, why? may not be as simple as quadrant.
  
  ##TODO need to look at phi and theta 
} 