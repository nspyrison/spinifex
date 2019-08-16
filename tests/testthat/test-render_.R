context("render_")

flea_std <- tourr::rescale(tourr::flea[, 1:6])
rb <- basis_random(n = ncol(flea_std))
mtour <- manual_tour(basis = rb, manip_var = 4)
sshow <- array2df(array = mtour, data = flea_std)
ret <- render_(slides = sshow)

test_that("ggplot class and length", {
  expect_is(ret, "gg")
  expect_is(ret, "ggplot")
  expect_equal(length(ret), 9)
})
