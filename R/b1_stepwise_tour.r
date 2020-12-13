#' @examples
#' dat <- wine[, 2:14]
#' bas <- basis_pca(dat)
#' 
#' sw_ret <- stepwise_hist(start_basis = bas, data = dat)
#' 
#' 

if(F){
  start_basis = bas; data = dat;
  measure = var; curr_dim = ncol(data); decreasing = TRUE
}
stepwise_hist <- function(start_basis = NULL, 
                          data = NULL, 
                          measure = var,
                          curr_dim = ncol(data), ##needed or no? maybe a start dim?
                          decreasing = TRUE
){
  #measure <- match.arg(measure)
  if(is.null(start_basis) == TRUE){start_basis <- tourr::basis_random(curr_dim, 2)}
  
  ## Initialize
  data <- as.matrix(data)
  p <- ncol(data)
  if(all(dim(start_basis) != c(p, 2)) == TRUE) 
    stop("Dimension of the starting basis not [p x 2].")
  
  ## Measure table, ordered
  m <- apply(data, 2, measure)
  ord <- order(m, decreasing = decreasing)
  m <- m[ord]
  cn <- colnames(data)[ord]
  measure_tbl <- 
    data.frame(var_nm = factor(cn, levels = cn),
               var_ord = ord, 
               var_measure = m,
               cumsum_measure = cumsum(m) / sum(m)
  )
  
  ## The work, making the basis list
  basis_ls <- list(NULL)
  basis_ls[[p]] <- start_basis
  sapply((p - 1L):2L, function(i){ ## i is the dim of the new_bas, this_* operates on i + 1
    #for(i in p-1L:2L){
    this_basis <- basis_ls[[i + 1L]]
    this_mv    <- measure_tbl$var_ord[i + 1L] 
    ##TODO THE LINE ABOVE IS GOING TO BE OFF AT SOME PT BECUSE WE ARE CHANGING THE DIM FO THE BASIS, ALSO NEED TO UPDATE THE MEASURE_TBL.
    this_theta <- atan(this_basis[this_mv, 2L] / this_basis[this_mv, 1L])
    
    this_m_sp  <- create_manip_space(basis = this_basis, manip_var = this_mv)
    new_bas    <- rotate_manip_space(manip_space = this_m_sp,
                                     theta = this_theta, phi = .5* pi)[-this_mv, 1L:2L]
    
    basis_ls[[i]] <<- new_bas
    #}
  })
  
  ret <- list(data = data,
              measure_tbl = measure_tbl,
              basis_ls = basis_ls,
              curr_dim = curr_dim
  )
  return(ret)
}

