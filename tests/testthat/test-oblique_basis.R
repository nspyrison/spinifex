context("oblique_basis")

library("spinifex")
rb <- tourr::basis_random(n = 6)
theta <- runif(1, 0, 2 * pi)
phi   <- runif(1, 0, 2 * pi)
ret <- oblique_basis(basis = rb, manip_var = 4, theta, phi)

test_that("class and dim", {
  expect_is(ret, "matrix")
  expect_equal(dim(ret), c(6, 2))
})

test_that("is orthonormal", {
  expect_true(tourr::is_orthonormal(ret))
})