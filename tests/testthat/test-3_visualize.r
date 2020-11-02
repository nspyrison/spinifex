### _view_basis -------


ret_light <- view_basis(basis = bas)
ret_med   <- view_basis(basis = bas, data = flea_std, axes = "bottomleft")
ret_heavy <- view_basis(basis = bas, data = dat, axes = "right",
                        aes_args = list(color = clas, shape = clas),
                        identity_args = list(size = 2, alpha = .8),
                        ggproto = list(theme_spinifex(),
                                       ggtitle("My title"),
                                       scale_color_brewer(palette = "Set2")))

test_that("view_basis: gganimate class and length", {
  expect_is(ret_light, "gg")
  expect_is(ret_light, "ggplot")
  expect_is(ret_med, "gg")
  expect_is(ret_med, "ggplot")
  expect_is(ret_heavy, "gg")
  expect_is(ret_heavy, "ggplot")
  expect_equal(length(ret_light), 9)
  expect_equal(length(ret_medium), 9)
  expect_equal(length(ret_heavy), 9)
})
