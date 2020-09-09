

ns1 <- function(...) {
  arguments <- list(...)
  paste(arguments)
}

ns1("Hello", "World", "!")

ns2 <- function(...) {
  browser()
  cat(paste(...))
}
ns2("Hello", "World", "!")

ns_p1 <- function(...) {
  args <- lapply(list(...), function(x) x+1)
  args
}
ns_p1(1, 2)

.ls <- list(a=c("a", "b", "c"), col = c("red","yellow"))
lapply(.ls, function(x) rep_len(x, 9))

do.call(function(x){rep_len(x , length.out =  9)}, .ls)

unlist(lapply(.ls, function(x) rep_len(x, 9)))
#################
my_ellipsis_function <- function(...) {
  input_list <- list(...)
  output_list <- lapply(X=input_list, function(x) {str(x);summary(x)})
  return(output_list)
}
my_ellipsis_function(a=1:10,b=11:20,c=21:30)

my_ellipsis_function2 <- function(...) {
  input_list <- as.list(substitute(list(...)))
  output_list <- lapply(X=input_list, function(x) {str(x);summary(x)})
  return(output_list)
}
my_ellipsis_function2(a=1:10,b=11:20,c=21:30)


my_ellipsis_function <- function(...) {
  input_list <- as.list(substitute(list(...)))
  output_list <- lapply(X=input_list, function(x) {str(x);summary(x)})
  return(output_list)
}
my_ellipsis_function(a=1:10,b=11:20,c=21:30)
#############
mylist = list()
mylist[[1]] = sample(1:10, 10)
mylist[[2]] = sample(1:10, 10)
mylist[[3]] = sample(1:10, 10)
mylist[[4]] = sample(1:10, 10)
mylist[[5]] = sample(1:10, 10)

mymatrix = do.call(cbind, mylist)

##########
library("ggplot2")
my... <- list(colour = c("red", "blue"), size = c(1, 2, 3))
my_tform_... <- lapply(X = my..., FUN = function(x) {rep_len(x, nrow(mtcars))})
my_gg_pt <- function(...) ggplot() + geom_point(data = mtcars, mapping = aes(x=cyl, y=mpg), ...)
do.call(my_gg_pt, args = my_tform_...)

do.call(my_gg_pt, args = list())

  
?geom_point

