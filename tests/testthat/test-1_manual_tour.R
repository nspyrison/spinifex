library("spinifex")
library("testthat")
dat_std <- scale_sd(rescale(wine[, 2:6]))
bas     <- basis_pca(dat_std)
clas    <- wine$Type
mv      <- manip_var_of(bas)

##
## MANUAL TOUR WORK HORSES -----
##

### create_manip_space -----

ret <- msp <- create_manip_space(basis = bas, manip_var = mv)

test_that("create_manip_space, class and dim", {
  expect_is(ret, "matrix")
  expect_equal(dim(ret), c(13, 3))
})

ret <- rotate_manip_space(msp, theta = runif(1, max = 2 * pi),
                          phi = runif(1, max = 2 * pi))
ret_same <- rotate_manip_space(msp, theta = 0, phi = 0)
ret_crazy <- rotate_manip_space(msp, theta = 999, phi = -999)


test_that("rotate_manip_space, class and dim", {
  expect_is(ret, "matrix")
  expect_is(ret_same, "matrix")
  expect_is(ret_crazy, "matrix")
  expect_equal(dim(ret), c(13, 3))
  expect_equal(dim(ret_same), c(13, 3))
  expect_equal(dim(ret_crazy), c(13, 3))
})

### manual_tour -----

ret_light <- manual_tour(basis = bas, manip_var = mv)
ret_heavy <- manual_tour(basis = bas, manip_var = mv,
                         theta = pi / 2, phi_min = pi / 16, phi_max = pi, angle = .8)


test_that("manual_tour, class and dim", {
  expect_is(ret_light, "array")
  expect_is(ret_heavy, "array")
  expect_equal(dim(ret_light)[1:2], c(13, 2))
  expect_equal(dim(ret_heavy)[1:2], c(13, 2))
  expect_true(dim(ret_light)[3] > 0 & dim(ret_light)[3] < 200)
  expect_true(dim(ret_heavy)[3] > 0 & dim(ret_heavy)[3] < 200)
})

##
## INTERMEDIATE AND FORMATING -----
##

### DEPRICATED: rotate_basis -----
# rtheta <- runif(1, 0, 2 * pi)
# rphi   <- runif(1, 0, 2 * pi)
# rmv    <- 1 ## don't use rand for unit test maybe.
# ret <- rotate_basis(basis = bas, manip_var = mv, rtheta, rphi)
# ret_crazy <- rotate_basis(basis = bas, manip_var = rmv, -999, 999)
# 
# test_that("manual_tour, class and dim", {
#   expect_is(ret, "matrix")
#   expect_is(ret_crazy, "matrix")
#   expect_equal(dim(ret_light), c(13, 3))
#   expect_equal(dim(ret_heavy), c(13, 3))
#   expect_error(rotate_basis(basis = bas, manip_var = 999, 0, 0))
# })
# 
