func <- function(df = mtcars, var = cyl){
  try(with(df, print(eval(substitute(var)))))
  try(with(df, print(eval(enquote(var)))))
  try(with(df, print(eval(quote(var)))))
  try(with(df, print(!!enquo(var))))
  try(with(df, print(!!expr(var))))
}
func()

with(mtcars, print(mpg))
dplyr::select(mtcars, mpg)


with(mtcars, print(mpg))
with(mtcars, eval(quoteq_var))

func2 <- function(x = 1){
  ## Subsistute is about the value
  sq <- substitute(x)
  print(sq)
  print(class(sq))
  ## Quote is able the argument/variable
  qx <- quote(x)
  print(qx)
  print(class(qx))
  return(x + 1)
}
func2()
func2(matrix(1:4, 2))
