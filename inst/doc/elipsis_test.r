# FUNC-CEPTION
a=1
b=2
c="three"
d= as.Date("2018-01-18")
func1 <- function (a, b, ...) {
  bot=0
  func11 <- function (c=a, d, ...){bot <- c+...
    message(paste0("bot: bot = ",bot))
    return(bot)}
  message(paste0("top: a = ",a))
  message(paste0("top: b = ",b))
  message(paste0("top: bot = ",bot))
  message("func11 is done, closing func 1 now.")  
  NULL
}

func1(b=b,a=a,c=3,d=4,7)
