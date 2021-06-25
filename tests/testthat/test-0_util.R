library("spinifex")
library("testthat")
dat_std <- scale_sd(wine[, 2:6])
clas <- wine$Type
bas <- basis_pca(dat_std)
mv <- manip_var_of(bas)

##
## MATH AND TRANSFORMS -----
##

rb <- tourr::basis_random(ncol(dat_std), 2)
ib <- tourr::basis_init(n = 4, 2)
b_pca <- basis_pca(dat_std)
b_guide <- basis_guided(data = dat_std, index_f = tourr::holes())
diag4 <- diag(4)
not_orth <- matrix(sample(1:16, 16), ncol=4)

### is_orthonormal -----
test_that("is_orthonormal: bases functions & diag() are orthonormal, rand matrix isn't", {
  expect_true(is_orthonormal(rb))
  expect_true(is_orthonormal(ib))
  expect_true(is_orthonormal(b_pca))
  expect_true(is_orthonormal(b_guide))
  expect_true(is_orthonormal(diag4))
  expect_false(is_orthonormal(not_orth))
})

ret_t <- is_orthonormal(rb)
ret_f <- is_orthonormal(not_orth)
test_that("is_orthonormal: returns are logical.", {
  expect_is(ret_t, "logical")
  expect_is(ret_f, "logical")
})

### array2df -----

## Array with a single frame, as used in view_frame()
array_single <- array(bas, dim = c(dim(bas), 1))
attr(array_single, "manip_var") <- mv
ret_single <- array2df(array = array_single)

## Radial tour array to long df, as used in play_manual_tour()
array_manual <- manual_tour(basis = bas, manip_var = mv)
ret_manual <- array2df(array = array_manual, data = dat_std,
                       basis_label = paste0("MyLabs", 1:nrow(bas)),
                       data_label = paste0("obs# ", 1:nrow(dat_std)))


## tourr::save_history tour array to long df, as used in play_tour_path()
array_save_hist <- tourr::save_history(data = dat_std, max_bases = 10)
class(array_save_hist) <- "array"
ret_save_hist <- array2df(array = array_save_hist, data = dat_std,
                          basis_label = paste0("MyLabs", 1:nrow(bas)),
                          data_label = paste0("obs# ", 1:nrow(dat_std)))

test_that("array2df: class and dim for single frame, spinifex and tourr case.", {
  expect_is(ret_single,    "list")
  expect_is(ret_manual,    "list")
  expect_is(ret_save_hist, "list")
  expect_equal(length(ret_single),    1)
  expect_equal(length(ret_manual),    2)
  expect_equal(length(ret_save_hist), 2)
  expect_equal(dim(ret_single[[1]]),    c(13,  4))
  expect_true(dim(ret_manual[[1]])[1] > 500)
  expect_equal(dim(ret_manual[[1]])[2], 4)
  expect_equal(dim(ret_save_hist[[1]]), c(130, 4))
})



### scale_axes ------
ret <- scale_axes(x = bas, position = "bottomleft")
ret_to <- scale_axes(x = bas, position = "topright", to = wine[, 2:3])

test_that("scale_axes: class and dim", {
  expect_is(ret, "matrix")
  expect_is(ret_to, "matrix")
  expect_equal(dim(ret), c(13, 2))
  expect_equal(dim(ret_to), c(13, 2))
})


### map_absolute -----

ret_mat   <- map_absolute(x = bas, pan = c(-1, 0), zoom = c(2/3, 2/3))
ret_df    <- map_absolute(x = mtcars[,1:2], pan = c(0, 100), zoom = c(.1, .1))

test_that("map_absolute: class and dim", {
  expect_is(ret_mat, "matrix")
  expect_is(ret_df,  "data.frame")
  expect_equal(dim(ret_mat), c(13, 2))
  expect_equal(dim(ret_df),  c(32, 2))
  expect_warning(map_absolute(x = mtcars))
})

##
## GGPLOT2 AESTHETICS ------
##

## no roi for a unit test of theme_spinifex()


##
## BASIS AND MANIP VAR HELPERS -----
##


## no roi for a unit test of basis_pca()


### basis_guided -----

ret_holes <- basis_guided(data = dat_std, index_f = tourr::holes())
ret_cmass <- basis_guided(data = dat_std, index_f = tourr::cmass(),
                          alpha = .4, cooling = .9, max.tries = 30)

test_that("basis_guided: class and dim", {
  expect_is(ret_holes, "matrix")
  expect_is(ret_cmass, "matrix")
  expect_equal(dim(ret_holes), c(13, 2))
  expect_equal(dim(ret_cmass), c(13, 2))
})



### manip_var_of ------
ret <- manip_var_of(bas)
test_that("manip_var_of: class and dim", {
  expect_is(ret, "integer")
  expect_equal(length(ret), 1)
  expect_warning(manip_var_of(not_orth))
})



## no roi for a unit test of basis_pca()

## no roi for a unit test of scale_10()


##
## DEPRICATED: color_of, shape_of -----
##

# ret <- color_of(tourr::flea$species)
# 
# test_that("col_of: class and length", {
#   expect_is(ret, "character")
#   expect_equal(length(ret), 74)
# })
# 
# ret <- shape_of(tourr::flea$species)
# 
# test_that("pch_of: class and length", {
#   expect_is(ret, "integer")
#   expect_equal(length(ret), 74)
# })

