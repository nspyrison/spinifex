context("util")

library("spinifex")
flea_std <- tourr::rescale(tourr::flea[1:2, 1:6])
rb <- basis_random(ncol(flea_std), 2)

ret <- set_axes_position(x = rb, axes = "bottomleft")

test_that("set_axes_position: class and dim", {
  expect_is(ret, "matrix")
  expect_equal(dim(ret), c(6, 2))
})

ret <- view_basis(basis = rb, data = flea_std, axes = "bottomleft")

test_that("view_basis: gganimate class and length", {
  expect_is(ret, "gg")
  expect_is(ret, "ggplot")
  expect_equal(length(ret), 9)
})

ret <- view_manip_space(basis = rb, manip_var = 4)

test_that("view_manip_space: gganimate class and length", {
  expect_is(ret, "gg")
  expect_is(ret, "ggplot")
  expect_equal(length(ret), 9)
})

ret <- col_of(tourr::flea$species)

test_that("col_of: class and length", {
  expect_is(ret, "character")
  expect_equal(length(ret), 74)
})

ret <- pch_of(tourr::flea$species)

test_that("pch_of: class and length", {
  expect_is(ret, "integer")
  expect_equal(length(ret), 74)
})

