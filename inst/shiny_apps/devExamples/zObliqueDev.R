# Initialize ----
library(spinifex)
#?oblique_frame

flea_std <- tourr::rescale(tourr::flea[,1:6])
rb <- tourr::basis_random(n = ncol(flea_std))
manip_var <- 4
basis <- rb

m_sp <- create_manip_space(basis, manip_var)
a<-m_sp[manip_var,]
print(oblique_frame(flea_std, rb, manip_var, 0, 0))


### horizontal, x motion ----
x_zero <- atan(m_sp[manip_var, 3] / m_sp[manip_var, 1]) - (pi/2*sign(m_sp[manip_var, 1]))
p1 <- x_zero + pi/2 # x=1
n1 <- x_zero - pi/2 # x=-1
x_i <- -x_zero / (pi/2)

phi <- p1
theta <- 0
#view_manip_space(basis, manip_var)
print(oblique_frame(data = flea_std, basis = rb, manip_var, theta, phi))

### key ----
#x_zero, the value of phi where the x_slider = 0
#p1, the value of phi where x_slider = +1
#n1, the value of phi where x_slider = -1
#x_i, the value of x_slider when phi=0
#phi(x) = x * pi/2 + x_zero ## value of phi as a function of x given x in [-1, 1].

# vertical, y motion ----
#same idea, but with atan(z/y)
y_zero <- atan(m_sp[manip_var, 3] / m_sp[manip_var, 2]) - (pi/2*sign(m_sp[manip_var, 2]))
p1 <- y_zero + pi/2 # y=1
n1 <- y_zero - pi/2 # y=-1

y_i <- -y_zero / (pi/2)
#phi <- y1 * pi/2 + y_zero # phi as a function of x.
theta <- pi/2
phi <- p1
#view_manip_space(basis, manip_var)
print(oblique_frame(data = flea_std, basis = rb, manip_var, theta, phi))
