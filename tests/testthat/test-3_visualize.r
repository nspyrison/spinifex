library("spinifex")
library("testthat")
dat_std <- scale_sd(wine[1:10, 2:5])
bas <- basis_pca(dat_std)
clas <- wine$Type
mv <- manip_var_pca(bas)


### _play_tour_path -------
tpath <- tourr::save_history(dat_std, tour_path = tourr::grand_tour(), max = 5)

ret_light <- play_tour_path(tour_path = tpath, data = dat_std)
 
ret_heavy <- play_tour_path(tour_path = tpath, data = dat_std,
                            axes = "bottomleft", angle = .08, fps = 8,
                            aes_args = list(color = clas, shape = clas),
                            identity_args = list(size = .8, alpha = .7),
                            ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")),
                            render_type = render_gganimate)

test_that("view_basis: gganimate class and length", {
  expect_is(ret_light, "gg")
  expect_is(ret_light, "ggplot")
  expect_is(ret_heavy, "gif_image")
  expect_equal(length(ret_light), 9)
  expect_equal(length(ret_heavy), 1)
})


## play_manual_tour -----
