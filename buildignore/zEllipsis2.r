my_f <- function(x, ...){
  require("ggplot2")
  require("rlang")
  quos <- enquos(...)
  str(quos)
  rep_len( UQ(quos[[1]]), length.out = 10)
  bare <- as.list(quos)
  quos$a <- rep_len(args[[1]], length.out = 10)
  
  args <- list(...)
  args[[1]] <-rep_len(args[[1]], length.out = 10)
  quos <- enquos(unlist(args))
  str(ls_quos)
  
  
  rlang::is_vector(quos)
  is.vector(quos)
  rlang::names2(quos)
  
  #print(!!!quos)
  browser()
  print("ended")
}


my_f(x = 1, a=1:3, b = mtcars, d = mean)
