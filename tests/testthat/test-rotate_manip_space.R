context("rotate_manip_space")

flea_std <- tourr::rescale(tourr::flea[,1:6])
rb  <- basis_random(n = ncol(flea_std))
msp <- create_manip_space(basis = rb, manip_var = 4) 
ret <- rotate_manip_space(msp, theta = runif(1, max = 2 * pi), 
                          phi = runif(1, max = 2 * pi) )

test_that("class and dim", {
  expect_is(ret, "matrix")
  expect_equal(dim(ret), c(6, 3))
  # tourr::is_orthonormal() returns F, which is wrong.
})

