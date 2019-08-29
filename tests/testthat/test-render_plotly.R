context("render_plotly")

flea_std <- tourr::rescale(tourr::flea[1:2, 1:6])
rb    <- basis_random(n = ncol(flea_std))
mtour <- manual_tour(basis = rb, manip_var = 4)
sshow <- array2df(array = mtour, data = flea_std)
ret   <- render_plotly(slides = sshow)

test_that("plotly class and length", {
  expect_is(ret, "plotly")
  expect_is(ret, "htmlwidget")
  expect_equal(length(ret), 9)
})

cat <- flea[1:2, 7]
ret   <- render_plotly(slides = sshow, col = cat, pch = cat, 
                       lab = paste0("a", 1:6), axes = "off")

test_that("col, pch, lab, axes arguments return plotly object", {
  expect_is(ret, "plotly")
  expect_is(ret, "htmlwidget")
  expect_equal(length(ret), 9)
})