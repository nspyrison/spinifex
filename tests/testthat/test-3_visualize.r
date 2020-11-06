library("spinifex")
library("testthat")
dat_std <- scale_sd(wine[1:10, 2:5]) ## small chunk for speed.
bas <- basis_pca(dat_std)
clas <- wine$Type
mv <- manip_var_pca(bas)

##
## TARGET WRAPPER FUNCTIONS -----
##


### play_tour_path -------
tpath <- tourr::save_history(dat_std, tour_path = tourr::grand_tour(), max = 5)

ret_light <- play_tour_path(tour_path = tpath, data = dat_std)
 
ret_heavy <- play_tour_path(tour_path = tpath, data = dat_std,
                            axes = "bottomleft", angle = .08, fps = 8,
                            aes_args = list(color = clas, shape = clas),
                            identity_args = list(size = .8, alpha = .7),
                            ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")),
                            render_type = render_gganimate)

test_that("play_tour_path: gganimate class and length", {
  expect_is(ret_light, c("plotly", "htmlwidget"))
  expect_equal(length(ret_light), 9)
  expect_true(class(ret_heavy) %in% c("gif_image", "character"))
  expect_true(length(ret_heavy) %in% c(1L, 99L, 100L))
})


### play_manual_tour -----


ret_light <- play_manual_tour(basis = bas, data = dat_std, manip_var = mv)

ret_heavy <- play_manual_tour(basis = bas, data = dat_std, manip_var = mv,
                              theta = .5 * pi, axes = "right", fps = 5,
                              aes_args = list(color = clas, shape = clas),
                              identity_args = list(size = .8, alpha = .7),
                              ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")),
                              render_type = render_gganimate)

test_that("play_manual_tour: gganimate class and length", {
  expect_is(ret_light, c("plotly", "htmlwidget"))
  expect_true(class(ret_heavy) %in% c("gif_image", "character"))
  expect_equal(length(ret_light), 9)
  expect_true(length(ret_heavy) %in% c(1L, 99L, 100L))
})

##
## HELPER & INTERMEDIATE VISUALIZATIONS -----
##

### view_frame -----

rtheta <- runif(1, 0, 2 * pi)
rphi   <- runif(1, 0, 2 * pi)

ret_light <- view_frame(basis = bas, data = dat_std, manip_var = mv)

ret_heavy <- view_frame(basis = bas, data = dat_std, manip_var = mv,
                        theta = rtheta, phi = rphi, label = paste0("MyNm", 1:ncol(dat_std)),
                        aes_args = list(color = clas, shape = clas),
                        identity_args = list(size = .8, alpha = .7),
                        ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")))

test_that("view_frame: gganimate class and length", {
  expect_is(ret_light, c("gg", "ggplot"))
  expect_is(ret_heavy, c("gg", "ggplot"))
  expect_equal(length(ret_light), 9)
  expect_equal(length(ret_heavy), 9)
})


### view_manip_space -----


ret_light <- view_manip_space(basis = bas, manip_var = mv)
ret_heavy <- view_manip_space(basis = bas, manip_var = mv,
                              tilt = 2/12 * pi, label = paste0("MyNm", 1:ncol(dat_std)),
                              manip_col = "purple", manip_sp_col = "orange", 
                              ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")))


test_that("view_manip_space: gganimate class and length", {
  expect_is(ret_light, c("gg", "ggplot"))
  expect_is(ret_heavy, c("gg", "ggplot"))
  expect_equal(length(ret_light), 9)
  expect_equal(length(ret_heavy), 9)
})

