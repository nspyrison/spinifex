{
  library("spinifex")
  library("testthat")
  library("ggplot2")
  dat_std <- scale_sd(wine[1L:10L, 2L:5L]) ## small chunk for speed.
  bas <- basis_pca(dat_std)
  clas <- wine$Type
  mv <- manip_var_of(bas)
}

##
## RENDERING -----
## ggplot2, gganimate, and plotly respectively
##
mt_array <- manual_tour(basis = bas, manip_var = mv)
mt_df_ls <- array2df(basis_array = mt_array, data = dat_std,
                      basis_label = paste0("MyLabs", 1L:nrow(bas)),
                      data_label = paste0("obs# ", 1L:nrow(dat_std)))

### render_ -----
suppressWarnings( ## suppress 8hr deprecation warning
  ret <- render_(frames = mt_df_ls, axes = "left", manip_col = "purple",
                 aes_args = list(color = clas, shape = clas),
                 identity_args = list(size = .8, alpha = .7),
                 ggproto = list(theme_spinifex(), ggtitle("My title")))
)

test_that("render_, class and dim", {
  expect_equal(class(ret) , c("gg", "ggplot"))
  expect_equal(length(ret), length(ggplot2::ggplot()))
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
  expect_equal(class(ret) , "gif_image")
  expect_equal(length(ret), 1L)
})


### render_plotly -----

ret <- render_plotly(
  frames = mt_df_ls, axes = "bottomleft", fps = 10L, 
  aes_args = list(color = clas, shape = clas),
  identity_args = list(size = .8, alpha = .7),
  ggproto = list(theme_classic(), ggtitle("My title"),
                 scale_color_brewer(palette = "Set2")))

test_that("render_gganimate, class and dim", {
  expect_equal(class(ret) , c("plotly", "htmlwidget"))
  expect_equal(length(ret), 9L)
})
