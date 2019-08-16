context("play_tour_path")

flea_std <- rescale(tourr::flea[1:2,1:6])
tpath    <- save_history(flea_std, tour_path = grand_tour(),max = 1)
ret      <- play_tour_path(tour_path = tpath, data = flea_std, angle = .15)

test_that("plotly class and length", {
  expect_is(ret, "plotly")
  expect_is(ret, "htmlwidget")
  expect_equal(length(ret), 9)
})

ret <- play_tour_path(tour_path = tpath, data = flea_std, 
                      render_type = render_gganimate)

test_that("gganimate class and length", {
  expect_is(ret, "gganim")
  expect_is(ret, "gg")
  expect_is(ret, "ggplot")
  expect_equal(length(ret), 14)
})
