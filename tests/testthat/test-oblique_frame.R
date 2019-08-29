context("oblique_frame")

flea_std <- tourr::rescale(tourr::flea[1:2, 1:6])
rb <- tourr::basis_random(n = ncol(flea_std))
theta <- runif(1, 0, 2*pi)
phi <- runif(1, 0, 2*pi)
ret <- oblique_frame(data = flea_std, basis = rb, manip_var = 4, theta, phi)

test_that("with data class and length", {
  expect_is(ret, "gg")
  expect_is(ret, "ggplot")
  expect_equal(length(ret), 9)
})

### TODO: data gets assigned a numeric value, rather than NULL somehow.
#ret <- oblique_frame(basis = rb, manip_var = 4, theta, phi)
# test_that("without data class and length", {
#   expect_is(ret, "gg")
#   expect_is(ret, "ggplot")
#   expect_equal(length(ret), 9)
# })

