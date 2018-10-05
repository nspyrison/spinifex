# create a 2 by 5 matrix
x <- 1:10
attr(x,"MyAttr") <- c(2, 5, "foo")
dim(x) # vectors don't have dim.
str(x)
attributes(x)
class(x) # integer vector
str(x[1]) # changing or referencing easily breaks attributes.

attr(x, exact = "class")

food = as.data.frame(c(apple=2, grape=3))
attr(food[, ], "foodType") <- "fruit"
food
mt <- tibble::as.tibble(mtcars)
mt

attr(mt$cyl, "has6Cyl") <- ifelse(mt$cyl==6, TRUE, FALSE) # can set column attrs
mt
mt$cyl
str(mt)
str(mt$cyl)
attributes(mt)
attributes(mt$cyl)
attributes(str(mt))
attributes(str(mt$cyl))

ray <- array(data = 1:12, dim = c(2,2,3))
dim(ray)
str(ray)
attributes(ray) # just dim
class(ray) # array
str(ray[,1,1]) # changing or referencing easily breaks attributes

attr(ray, "data_CrDate") <- as.Date("2018-10-03")
attr(ray, "page") <- c(rep(1, 4), rep(2, 4), rep(3, 4))
dim(ray)
str(ray)
str(ray[,1,1]) # changing or referencing easily breaks attributes
