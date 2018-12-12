# FUNC-CEPTION
maths <- function (c=0, d=0, ...) {
  add <- function (a,b,...){
    sum <- a+b
    return(sum)
  }
  sub <- function (a,b,...){
    diff <- a-b
    return(diff)
  }
  s<-add(a,b,...)
  d<-sub(a,b,...)
  math <- c(s, d)
  return(math)
}

a=1
b=2
maths(1,a=2,3,b=4)
