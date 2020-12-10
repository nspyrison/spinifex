####

####
stepwise_hist <- function(start_basis = NULL, 
                          data = NULL, 
                          measure = var,
                          curr_dim = ncol(data), ##needed or no? maybe a start dim?
                          decreasing = TRUE
){
  measure <- match.arg(measure)
  if(is.null(start_basis) == TRUE){start_basis <- tourr::basis_random(curr_dim, 2)}
  
  data <- as.data.frame(data)
  p <- ncol(data)
  if(dim(start_basis) != c(p, 2)) stop("Dimension of the starting basis not [p x 2].")

  ## Measure table, ordered
  m <- apply(data, 2, measure)
  ord <- order(m, decreasing = decreasing)
  m <- m[ord]
  cn <- colnames(data)[ord]
  measure_tbl <- 
    data.frame(var_nm = factor(cn, levels = cn),
               var_measure = m,
               cumsum_measure = cumsum(m) / sum(m)
  )
  
  ##TODO: BUILD OUT THE possible basis
  basis_ls <- lapply(start_basis) ## FULL BASIS IN, 
  
  
  ret <- list(data = data,
              measure_tbl = measure_tbl,
              basis_ls = list(0), # made in the todo above.
              curr_dim = curr_dim
  )
  return(ret)
}

