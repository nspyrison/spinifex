context("manual_tour")

library("spinifex")
flea_std <- tourr::rescale(tourr::flea[1:2, 1:6])
rb <- tourr::basis_random(n = ncol(flea_std))
ret <- manual_tour(basis = rb, manip_var = 4)

test_that("is array", {
  expect_is(ret, "array")
})
test_that("slice dim and class", {
  expect_equal(dim(ret[,, 1]), c(6, 2))
  expect_is(ret[,, 1], "matrix")
})