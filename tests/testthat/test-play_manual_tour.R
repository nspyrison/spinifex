context("play_manual_tour")

library("spinifex")
flea_std <- tourr::rescale(tourr::flea[1:2, 1:6])
rb <- tourr::basis_random(n = ncol(flea_std))
ret      <- play_manual_tour(basis = rb, data = flea_std, manip_var = 4)

test_that("plotly class and length", {
  expect_is(ret, "plotly")
  expect_is(ret, "htmlwidget")
  expect_equal(length(ret), 9)
})

ret <- play_manual_tour(basis = rb, data = flea_std, manip_var = 4,
                        render_type = render_gganimate)

test_that("gganimate class and length", {
  expect_is(ret, "gganim")
  expect_is(ret, "gg")
  expect_is(ret, "ggplot")
  expect_equal(length(ret), 14)
})

cat <- flea[1:2, 7]
ret <- play_manual_tour(basis = rb, data = flea_std, manip_var = 4,
                        col = cat, pch = cat, 
                        lab = paste0("a", 1:6), axes = "off")

test_that("col, pch, lab, axes args returns plotly object", {
  expect_is(ret, "plotly")
  expect_is(ret, "htmlwidget")
  expect_equal(length(ret), 9)
})

