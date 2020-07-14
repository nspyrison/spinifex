context("play_manual_tour")

library("spinifex")
flea_std <- tourr::rescale(tourr::flea[1:2, 1:6])
rb <- tourr::basis_random(n = ncol(flea_std))
ret <- play_manual_tour(basis = rb, data = flea_std, manip_var = 4)
# class(ret)

test_that("plotly class and length", {
  expect_is(ret, "plotly")
  expect_is(ret, "htmlwidget")
  expect_equal(length(ret), 9)
})


ret <- play_manual_tour(basis = rb, data = flea_std, manip_var = 4,
                        render_type = render_gganimate)
# class(ret)

test_that("gganimate type", {
  expect_type(ret, "character")
})


category <- tourr::flea$species[1:2]
ret <- play_manual_tour(basis = rb, data = flea_std, manip_var = 4,
                        color = category, shape = category, 
                        lab = paste0("a", 1:6), axes = "off")

test_that("col, pch, lab, axes args returns plotly object", {
  expect_is(ret, "plotly")
  expect_is(ret, "htmlwidget")
  expect_equal(length(ret), 9)
})

