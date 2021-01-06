z1 <- data.frame(
  x = seq(0, 1, length.out = 10),
  y = seq(0, 1, length.out = 10),
  gp = 1
)
z2 <- data.frame(
  x = seq(0, 10, length.out = 10),
  y = seq(0, 1, length.out = 10),
  gp = 2
)
z3 <- data.frame(
  x = seq(0, 1, length.out = 10),
  y = seq(0, 10, length.out = 10),
  gp = 3
)
z4 <- data.frame(
  x = seq(0, 10, length.out = 10),
  y = seq(0, 10, length.out = 10),
  gp = 4
)
#z14 <- rbind(z1, z2, z3, z4)

## ===
require(spinifex); require(ggplot2)

add_basis <- function(df_to, axes = "left"){
  angle <- seq(0L, 2L * pi, length = 360L)
  circ  <- data.frame(x = cos(angle), y = sin(angle))
  ## Scale basis axes/circle
  if(axes != "off"){
    circ <- scale_axes(circ, axes, to = df_to)
  }
  
  ## Circle path
  ggplot2::geom_path(
    data = circ, color = "grey80", size = 1, inherit.aes = FALSE,
    mapping = ggplot2::aes(x = x, y = y)
  )
}

## ===
ggplot() +
  geom_point(aes(x, y), z1) +
  add_basis(z1) +
  ggtitle("z1")

ggplot() +
  geom_point(aes(x, y), z2) +
  add_basis(z2) +
  ggtitle("z2")

ggplot() +
  geom_point(aes(x, y), z3) +
  add_basis(z3) +
  ggtitle("z3")


ggplot() +
  geom_point(aes(x, y), z4) +
  add_basis(z4) +
  ggtitle("z4")


dat_std <- scale_sd(wine[, 2:14])
clas <- wine$Type
bas10 <- basis_pca(dat_std, d = 10)
bas <- cbind(bas10[, 1], bas10[, 10])
mv <- manip_var_pca(dat_std)

#play_manual_tour(basis = bas, data = dat_std, manip_var = mv)
play_manual_tour(basis = bas, data = dat_std, manip_var = mv,
                 render_type = render_gganimate, axes = "left")
