context("create_manip_space")

library("spinifex")
flea_std <- tourr::rescale(tourr::flea[1:2, 1:6])

rb <- basis_random(n = ncol(flea_std))
ret <- create_manip_space(basis = rb, manip_var = 4)

test_that("class and dim", {
  expect_equal(dim(ret), c(6, 3))
  expect_is(ret, "matrix")
})

