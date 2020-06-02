context("rotate_manip_space")

library("spinifex")
flea_std <- tourr::rescale(tourr::flea[1:2, 1:6])
rb  <- basis_random(n = ncol(flea_std))
msp <- create_manip_space(basis = rb, manip_var = 4) 
ret <- rotate_manip_space(msp, theta = runif(1, max = 2 * pi), 
                          phi = runif(1, max = 2 * pi)
)

test_that("class and dim", {
  expect_is(ret, "matrix")
  expect_equal(dim(ret), c(6, 3))
})


test_that("orthonormal (manual)", {
  # tourr::is_orthonormal(ret) returns F, which is wrong; define a new func in spinifex.
  expect_true(spinifex::is_orthonormal(ret))
})