context("print_manip_space")

library("spinifex")
rb <- tourr::basis_random(n = 6)
theta <- runif(1, 0, 2 * pi)
phi   <- runif(1, 0, 2 * pi)
ret <- print_manip_space(basis = rb, manip_var = 4, theta, phi)

test_that("class and dim", {
  expect_is(ret, "matrix")
  expect_is(ret, "array")
  expect_type(ret, "double")
  expect_equal(dim(ret), c(6, 3))
})

test_that("is orthonormal", {
  expect_true(spinifex::is_orthonormal(ret))
})