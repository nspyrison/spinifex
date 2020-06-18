context("play_tour_path")

library("spinifex")
flea_std <- tourr::rescale(tourr::flea[1:2, 1:6])
tpath    <- tourr::save_history(flea_std, tour_path = tourr::grand_tour(), max = 1)
ret      <- play_tour_path(tour_path = tpath, data = flea_std, angle = .15)

test_that("plotly class and length", {
  expect_is(ret, "plotly")
  expect_is(ret, "htmlwidget")
  expect_equal(length(ret), 9)
})

ret <- play_tour_path(tour_path = tpath, data = flea_std, 
                      render_type = render_gganimate)

test_that("gganimate type", {
  expect_type(ret, "character")
})

cat <- tourr::flea[1:2, 7]
ret <- play_tour_path(tour_path = tpath, data = flea_std, 
                      col = cat, pch = cat, lab <- paste0("a", 1:6), axes="off")

test_that("col, pch, lab, axes args returns plotly object", {
  expect_is(ret, "plotly")
  expect_is(ret, "htmlwidget")
  expect_equal(length(ret), 9)
})
