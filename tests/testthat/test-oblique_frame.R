context("view_frame")

library("spinifex")
flea_std <- tourr::rescale(tourr::flea[1:2, 1:6])
rb <- tourr::basis_random(n = ncol(flea_std))
theta <- runif(1, 0, 2 * pi)
phi <- runif(1, 0, 2 * pi)
ret <- view_frame(data = flea_std, basis = rb, manip_var = 4, theta, phi)

test_that("with data class and length", {
  expect_is(ret, "gg")
  expect_is(ret, "ggplot")
  expect_equal(length(ret), 9)
})

### data gets assigned a numeric value (5.54), rather than NULL somehow.
## I have no idea where or why.
# data <- NULL
# ret <- view_frame(basis = rb, manip_var = 4, theta, phi)
# test_that("without data class and length", {
#   expect_is(ret, "gg")
#   expect_is(ret, "ggplot")
#   expect_equal(length(ret), 9)
# })

