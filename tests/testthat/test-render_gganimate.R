context("render_gganimate")

flea_std <- tourr::rescale(tourr::flea[, 1:6])
rb <- basis_random(n = ncol(flea_std))
mtour <- manual_tour(basis = rb, manip_var = 4)
sshow <- array2df(array = mtour, data = flea_std)
ret <- render_gganimate(slides = sshow)


test_that("gganimate class and length", {
  expect_is(ret, "gganim")
  expect_is(ret, "gg")
  expect_is(ret, "ggplot")
  expect_equal(length(ret), 14)
})
