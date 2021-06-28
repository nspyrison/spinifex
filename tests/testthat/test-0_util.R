library("spinifex")
library("testthat")
dat_std <- scale_sd(wine[1:5, 2:6])
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

test_that("array2df: class", {
  expect_is(ret_single,    "list")
  expect_is(ret_manual,    "list")
  expect_is(ret_save_hist, "list")
})
test_that("array2df: length", {
  expect_true(length(ret_single) == 1)
  expect_true(length(ret_manual) == 2)
  expect_true(length(ret_save_hist) == 2)
})
test_that("array2df: dim", {
  expect_true(nrow(ret_manual[[1]]) == 340)
  expect_true(ncol(ret_manual[[1]]) == 4)
  expect_equal(dim(ret_single[[1]]),  c(5, 4))
  expect_equal(dim(ret_save_hist[[1]]), c(50, 4))
})



### map_relative ------
ret <- map_relative(x = bas, position = "bottomleft")
ret_to <- map_relative(x = bas, position = "topright", to = wine[, 2:3])

test_that("map_relative: class and dim", {
  expect_is(ret, "matrix")
  expect_is(ret_to, "matrix")
  expect_equal(dim(ret), c(5, 2))
  expect_equal(dim(ret_to), c(5, 2))
})


### map_absolute -----
ret_mat   <- map_absolute(x = bas, offset = c(-1, 0), scale = c(2/3, 2/3))
ret_df    <- map_absolute(x = mtcars[,1:2], offset = c(0, 100), scale = c(.1, .1))

test_that("map_absolute: class and dim", {
  expect_is(ret_mat, "matrix")
  expect_is(ret_df,  "data.frame")
  expect_equal(dim(ret_mat), c(5, 2))
  expect_equal(dim(ret_df),  c(32, 2))
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
  expect_equal(dim(ret_holes), c(5, 2))
  expect_equal(dim(ret_cmass), c(5, 2))
})



### manip_var_of ------
ret <- manip_var_of(bas)
test_that("manip_var_of: class and dim", {
  expect_is(ret, "integer")
  expect_equal(length(ret), 1)
  expect_warning(manip_var_of(not_orth))
})

## Other basis_* :-----
t <- theme_spinifex() 
g <- ggplot2::ggplot() + t
test_that("theme_spinifex()", {
  expect_is(t, "list")
  expect_is(g, c("gg", "ggplot"))
})

dat  <- tourr::flea[, -7]
clas <- tourr::flea[, 7]

pca <- basis_pca(dat)
olda <- basis_olda(dat, clas)
odp  <- basis_odp(dat, clas)
#olpp <- basis_olpp(dat) ## NOT orthogonal
onpp <- basis_onpp(dat)
half_circ <- basis_half_circle(dat)

test_that("other basis_* class, orth, ", {
  expect_is(pca, "matrix")
  expect_is(olda, "matrix")
  expect_is(odp,  "matrix")
  expect_is(half_circ, "matrix")
  expect_true(is_orthonormal(pca))
  expect_true(is_orthonormal(olda))
  expect_true(is_orthonormal(odp))
  expect_true(is_orthonormal(half_circ))
})

## scale functions ----
s1 <- scale_sd(mtcars)
s2 <- scale_01(dat)
s3 <- mtcars |> as.matrix() |> scale_01()

test_that("scale, class, bounds, dim", {
  expect_is(s1, "matrix") ## coerced to matrix.
  expect_is(s2, "matrix") ## coerced to matrix.
  expect_is(s3, "matrix")
  expect_equal(min(s2), 0)
  expect_equal(max(s2), 1)
  expect_equal(min(s3), 0)
  expect_equal(max(s3), 1)
  expect_equal(dim(s1), dim(mtcars))
  expect_equal(dim(s2), dim(dat))
  expect_equal(dim(s3), dim(mtcars))
})
min(s2)


##
## DEPRICATED:  -----
##

## low roi for testing deprecated.

test_that("other basis_* class, orth, ", {
  expect_warning(sa <- scale_axes(mtcars))
  expect_equal(sa, map_relative(mtcars))
  expect_warning(pz <- pan_zoom(mtcars))
  expect_equal(pz, map_absolute(mtcars))
})
