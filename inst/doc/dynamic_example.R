# var
num1 <- 10
num2 <- 5
operators <- c('+','-')

# string of code
mycode1 <- paste(num1,operators[1],num2)
mycode2 <- paste(num1,operators[2],num2)

# execute with eval(parse(code))
eval(parse(text=mycode1))
eval(parse(text=mycode2))

