context("play_manual_tour")
library("testthat")

library("spinifex")
flea_std <- tourr::rescale(tourr::flea[1:2, 1:6])
rb <- tourr::basis_random(n = ncol(flea_std))
ret <- play_manual_tour(basis = rb, data = flea_std, manip_var = 4)

test_that("plotly class and length", {
  expect_is(ret, "plotly")
  expect_is(ret, "htmlwidget")
  expect_equal(length(ret), 9)
})

ret <- play_manual_tour(basis = rb, data = flea_std, manip_var = 4,
                        render_type = render_gganimate)

cat(paste0("class(ret): ", class(ret)))
cat(paste0("length(ret): ", length(ret)))

test_that("gganimate class and length", {
  expect_is(ret, "gif_image")
  expect_equal(length(ret), 1)
})

category <- tourr::flea[1:2, 7]
ret <- play_manual_tour(basis = rb, data = flea_std, manip_var = 4,
                        col = category, pch = category, 
                        lab = paste0("a", 1:6), axes = "off")

test_that("col, pch, lab, axes args returns plotly object", {
  expect_is(ret, "plotly")
  expect_is(ret, "htmlwidget")
  expect_equal(length(ret), 9)
})

