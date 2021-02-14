
### DEPRICATED, stepwise_history can go straight into play_tour_path


### MANUAL TESTING ------
if(F){ 
  require(spinifex)
  source("./R/b1_stepwise_tour.r")
  ?play_tour_path
  play_tour_path
  require(tourr)
  ?tourr::interpolate
  tourr::interpolate
  t1 <- save_history(flea[, 1:6], grand_tour(1), max = 10)
  dim(t1)
  dim(interpolate(t1, 0.05))
  
}

if(F){
  play_stepwise_tour <- function(stepwise_list = NULL,
                                 angle = .05,
                                 from = ncol(stepwise_list$data),
                                 to = 2L,
                                 render_type = render_plotly,
                                 ...){
    dat <- as.matrix(stepwise_list$data)
    bas_array <- as_historty_array(stepwise_list$basis_array, dat)
    #bas_array <- stepwise_list$basis_array
    
    ## trouble shooting is_orthonormal(Fa) : is.matrix(x) is not TRUE
    # from:to
    # dim(bas_array)
    # browser()
    # mute <- sapply(from:to, function(i){
    #   print(paste0("page 'i' is orthornormal: ", tourr::is_orthonormal(as.matrix(bas_array[,,i])), ", page: ", i))
    # })
    ## end
    
    interp_path <- tourr::interpolate(basis_set = bas_array, angle = angle)
    
    ## debugging check
    # dim(interp_path)
    # str(interp_path)
    # class(interp_path)
    ## end
    
    render_df <- array2df(array = interp_path, data = dat)
    return(render_type(frames = render_df, ...))
  }
}