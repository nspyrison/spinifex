library("spinifex")
library("testthat")
dat_std <- scale_sd(wine[, 2:14])
bas <- basis_pca(dat_std)
clas <- wine$Type
mv <- manip_var_pca(bas)

##
## RENDERING -----
## ggplot2, gganimate, and plotly respectively
##

require("ggplot2")
array_manual <- manual_tour(basis = bas, manip_var = mv)
df_manual <- array2df(array = array_manual, data = dat_std,
                         label = paste0("MyLabs", 1:nrow(bas)))

### render_ -----


ret <- render_(frames = df_manual, axes = "left", manip_col = "purple",
               aes_args = list(color = clas, shape = clas),
               identity_args = list(size = .8, alpha = .7),
               ggproto = list(theme_spinifex(),
                              ggtitle("My title"),
                              scale_color_brewer(palette = "Set2")))


test_that("render_, class and dim", {
  expect_is(ret, c("gg", "ggplot"))
  expect_equal(length(ret), 9L)
})

### render_gganimate -----


ret <- render_gganimate(frames = df_manual, axes = "left", manip_col = "purple",
                        aes_args = list(color = clas, shape = clas),
                        identity_args = list(size = .8, alpha = .7),
                        ggproto = list(theme_spinifex(),
                                       ggtitle("My title"),
                                       scale_color_brewer(palette = "Set2")))

test_that("render_gganimate, class and dim", {
  expect_is(ret, "gif_image")
  expect_equal(length(ret), 9L)
})


### render_plotly -----

ret <- render_plotly(frames = df_frames, axes = "bottomleft", fps = 10,
              tooltip = c("label", "frame", "x", "y"),
              aes_args = list(color = clas, shape = clas),
              identity_args = list(size = .8, alpha = .7),
              ggproto = list(theme_classic(),
                             ggtitle("My title"),
                             scale_color_brewer(palette = "Set2")))


