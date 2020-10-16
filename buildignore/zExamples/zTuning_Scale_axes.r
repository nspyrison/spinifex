library(spinifex)
dat <- mtcars
bas <- basis_pca(dat)
mv  <- manip_var_pca(dat)
clas <- dat$cyl

## 1) mildly rectanular -----
play_manual_tour(basis = bas, data = dat, manip_var = mv,
                 theta = .5 * pi, axes = "left", fps = 5,
                 aes_args = list(color = clas, pch = clas), 
                 identity_args = list(size = 1.5),
                 ggproto = list(theme_spinifex(), ggplot2::ggtitle("")),
                 )#render_type = render_gganimate)
## WHEN Scale axes sets 2d scales, is an ellipse, try just y axis

## 2) extreme rectanular ------
library(spinifex)
dat <- wine[, 2:14]
bas <- basis_pca(dat)
mv  <- manip_var_pca(dat)
clas <- wine$Type

play_manual_tour(basis = bas, data = dat, manip_var = mv,
                 theta = .5 * pi, axes = "left", fps = 5,
                 aes_args = list(color = clas, pch = clas), 
                 identity_args = list(size = 1.5),
                 ggproto = list(theme_spinifex(), ggplot2::ggtitle("")),
)#render_type = render_gganimate)
## WHEN Scale axes sets 2d scales, is an ellipse, try just y axis

## 2) extreme rectanular OTHER WAY ------
library(spinifex)
dat <- wine[, 2:14]
bas <- basis_pca(dat)
bas <- cbind(bas[,2], bas[,2])
mv  <- manip_var_pca(dat)
clas <- wine$Type

play_manual_tour(basis = bas, data = dat, manip_var = mv,
                 theta = .5 * pi, axes = "left", fps = 5,
                 aes_args = list(color = clas, pch = clas), 
                 identity_args = list(size = 1.5),
                 ggproto = list(theme_spinifex(), ggplot2::ggtitle("")),
)#render_type = render_gganimate)
## WHEN Scale axes sets 2d scales, is an ellipse, try just y axis

## 3) tourr::rescale() ------
library(spinifex)
dat <- tourr::rescale(wine[, 2:14])
bas <- basis_pca(dat)
mv  <- manip_var_pca(dat)
clas <- wine$Type

play_manual_tour(basis = bas, data = dat, manip_var = mv,
                 theta = .5 * pi, axes = "left", fps = 5,
                 aes_args = list(color = clas, pch = clas), 
                 identity_args = list(size = 1.5),
                 ggproto = list(theme_spinifex(), ggplot2::ggtitle("")),
)#render_type = render_gganimate)
## WHEN Scale axes sets 2d scales, is an ellipse, try just y axis

## 4) normalized by sd -----
library(spinifex)
dat <- apply(wine[, 2:14], 2, function(col) {(col-mean(col))/sd(col)})
bas <- basis_pca(dat)
mv  <- manip_var_pca(dat)
clas <- wine$Type

play_manual_tour(basis = bas, data = dat, manip_var = mv,
                 theta = .5 * pi, axes = "left", fps = 5,
                 aes_args = list(color = clas, pch = clas), 
                 identity_args = list(size = 1.5),
                 ggproto = list(theme_spinifex(), ggplot2::ggtitle("")),
)#render_type = render_gganimate)
## WHEN Scale axes sets 2d scales, is an ellipse, try just y axis

