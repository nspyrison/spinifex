library("spinifex")
library("testthat")
dat_std <- scale_sd(wine[1:10, 2:5]) ## small chunk for speed.
bas <- basis_pca(dat_std)
clas <- wine$Type
mv <- manip_var_of(bas)

##
## RENDERING -----
## ggplot2, gganimate, and plotly respectively
##
mt_array <- manual_tour(basis = bas, manip_var = mv)
mt_df_ls <- array2df(basis_array = mt_array, data = dat_std,
                      basis_label = paste0("MyLabs", 1:nrow(bas)),
                      data_label = paste0("obs# ", 1:nrow(dat_std)))

### render_ -----
library("ggplot2")

suppressWarnings( ## suppress 8hr deprecation warning
  ret <- render_(frames = mt_df_ls, axes = "left", manip_col = "purple",
                 aes_args = list(color = clas, shape = clas),
                 identity_args = list(size = .8, alpha = .7),
                 ggproto = list(theme_spinifex(), ggtitle("My title")))
)

test_that("render_, class and dim", {
  expect_is(ret, c("gg", "ggplot"))
  expect_equal(length(ret), 9L)
})

### render_gganimate -----
suppressWarnings( ## suppress 8hr deprecation warning
  ret <- render_gganimate(
    frames = mt_df_ls, axes = "left", manip_col = "purple",
    aes_args = list(color = clas, shape = clas),
    identity_args = list(size = .8, alpha = .7),
    ggproto = list(theme_spinifex(), ggtitle("My title")))
)

test_that("render_gganimate, class and dim", {
  expect_true(class(ret)  %in% c("gif_image", "character"))
  expect_true(length(ret) %in% c(1L, 100L))
})


### render_plotly -----

ret <- render_plotly(
  frames = mt_df_ls, axes = "bottomleft", fps = 10, 
  aes_args = list(color = clas, shape = clas),
  identity_args = list(size = .8, alpha = .7),
  ggproto = list(theme_classic(), ggtitle("My title"),
                 scale_color_brewer(palette = "Set2")))

test_that("render_gganimate, class and dim", {
  expect_is(ret, c("plotly", "htmlwidget"))
  expect_equal(length(ret), 9L)
})
