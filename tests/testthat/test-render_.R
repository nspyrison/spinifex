context("render_")

library("spinifex")
flea_std <- tourr::rescale(tourr::flea[1:2, 1:6])
rb <- tourr::basis_random(n = ncol(flea_std))
mtour <- manual_tour(basis = rb, manip_var = 4)
sshow <- array2df(array = mtour, data = flea_std)
ret <- render_(slides = sshow)

test_that("ggplot class and length", {
  expect_is(ret, "gg")
  expect_is(ret, "ggplot")
  expect_equal(length(ret), 9)
})

cat <- tourr::flea[1:2, 7]
ret <- render_(slides = sshow, col = cat, pch = cat, 
               lab = paste0("a", 1:6), axes = "off")

test_that("col, pch, lab, axes args returns gg object", {
  expect_is(ret, "gg")
  expect_is(ret, "ggplot")
  expect_equal(length(ret), 9)
})

