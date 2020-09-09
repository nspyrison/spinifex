library(spinifex); library(ggplot2)
my_gg <- function(aes_args = list(), 
                  static_args = list(), 
                  ...){
  aes_args    <- as.list(aes_args)
  static_args <- as.list(static_args)
  
  aes_func <- function(...){
    with(mtcars, aes(x=mpg, y=cyl, ...))
  }
  aes_call <- do.call(aes_func, args = c(, aes_args))
  
  geom_pt_func <- function(aes_call, ...){
    geom_point(aes_call, data = mtcars, ...)
  }
  geom_pt_call <- do.call(geom_pt_func, c(list(aes_call), static_args))
  
  ggplot() + geom_pt_call
}
my_gg(aes_args = list(color = mtcars$disp),
      static_args = list(size = 2, alpha = .7),
      size=10, alpha = .5) ## Ignored need to be passed to static_args

