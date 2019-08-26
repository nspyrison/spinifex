#x=rcosθ,y=rsinθ

theta <- seq(0,pi, length.out = 1000)
theta_pct <- theta/(1/2*pi)
x <- round(cos(theta),2)
x_c <- 1-x
(df<-data.frame(theta,theta_pct,x,x_c))
z  <- 0.2145*sin(theta*(1/5*pi^2))+x_c

plot(x=theta, y=x_c,type = "l", col="red") #nls
lines(x=theta, y=theta_pct, col="blue")
lines(x=theta, y = z, col="purple")

xx <- 0.2145*sin(theta*(1/5*pi^2))+1-(theta*2*pi)
theta -xx
plot(xx)

max(x_c-theta_pct)
max(z-theta_pct)

options("max.print"=500)

