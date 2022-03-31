{
  library("spinifex")
  library("testthat")
  r_idx <- c(1:3, (nrow(wine) - 2):nrow(wine))
  sub  <- wine[r_idx, ]
  dat  <- scale_sd(sub[, 2:6])
  clas <- sub$Type
  bas  <- basis_pca(dat)
  mv   <- manip_var_of(bas)
}

##
## MATH AND TRANSFORMS -----
##

rb <- tourr::basis_random(ncol(dat), 2)
ib <- tourr::basis_init(n = 4, 2)
b_pca    <- basis_pca(dat)
b_guide  <- basis_guided(data = dat, index_f = tourr::holes())
diag4    <- diag(4)
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
  expect_equal(class(ret_t), "logical")
  expect_equal(class(ret_f), "logical")
})

### array2df -----

## Array with a single frame, as used in view_frame()
array_single <- array(bas, dim = c(dim(bas), 1))
attr(array_single, "manip_var") <- mv
ret_single <- array2df(basis_array = array_single)

## Radial tour array to long df, as used in play_manual_tour()
mt_array <- manual_tour(basis = bas, manip_var = mv)
ret_mt <- array2df(basis_array = mt_array, data = dat,
                       basis_label = paste0("MyLabs", 1:nrow(bas)),
                       data_label = paste0("obs# ", 1:nrow(dat)))


## tourr::save_history tour array to long df, as used in play_tour_path()
gt_array <- save_history(data = dat, max_bases = 10)
class(gt_array) <- "array"
ret_gt <- array2df(basis_array = gt_array, data = dat,
                   basis_label = paste0("MyLabs", 1:nrow(bas)),
                   data_label = paste0("obs# ", 1:nrow(dat)))

test_that("array2df: class", {
  expect_equal(class(ret_single), "list")
  expect_equal(class(ret_mt    ), "list")
  expect_equal(class(ret_gt    ), "list")
})
test_that("array2df: length", {
  expect_equal(length(ret_single), 1)
  expect_equal(length(ret_mt)    , 2)
  expect_equal(length(ret_gt)    , 2)
})


### map_relative ------
ret    <- map_relative(x = bas, position = "bottomleft")
ret_to <- map_relative(x = bas, position = "topright", to = wine[, 2:3])

test_that("map_relative: class and dim", {
  expect_equal(class(ret   ), c("matrix" ,"array"))
  expect_equal(class(ret_to), c("matrix" ,"array"))
  expect_equal(dim(ret), c(5, 2))
  expect_equal(dim(ret_to), c(5, 2))
})


### map_absolute -----
ret_mat   <- map_absolute(x = bas, offset = c(-1, 0), scale = c(2/3, 2/3))
ret_df    <- map_absolute(x = mtcars[,1:2], offset = c(0, 100), scale = c(.1, .1))

test_that("map_absolute: class and dim", {
  expect_equal(class(ret_mat), c("matrix" ,"array"))
  expect_equal(class(ret_df ),  "data.frame")
  expect_equal(dim(ret_mat), c(5, 2))
  expect_equal(dim(ret_df),  c(32, 2))
})

##
## GGPLOT2 AESTHETICS ------
##

### theme_spinifex ----

t <- theme_spinifex()
g <- ggplot2::ggplot() + t
test_that("theme_spinifex: class", {
  expect_equal(class(t), "list")
  expect_equal(class(g), c("gg", "ggplot"))
})

##
## BASIS AND MANIP VAR HELPERS -----
##

### basis_* -----
b1 <- basis_pca(dat)
b2 <- basis_half_circle(dat)
b3 <- basis_odp(dat, clas)
b4 <- basis_olda(wine[, 2:6], wine$Type)

test_that("basis_*", {
  expect_equal(class(b1), c("matrix", "array"))
  expect_equal(class(b2), c("matrix", "array"))
  expect_equal(class(b3), c("matrix", "array"))
  expect_equal(class(b4), c("matrix", "array"))
  expect_equal(dim(b1), c(5, 2))
  expect_equal(dim(b2), c(5, 2))
  expect_equal(dim(b3), c(5, 2))
  expect_equal(dim(b4), c(5, 2))
  expect_equal(is_orthonormal(b1), TRUE)
  expect_equal(is_orthonormal(b2), TRUE)
  expect_equal(is_orthonormal(b3), TRUE)
  expect_equal(is_orthonormal(b4), TRUE)
})

## no roi for a unit test of basis_pca()


### basis_guided -----

ret_holes <- basis_guided(data = dat, index_f = tourr::holes())
ret_cmass <- basis_guided(data = dat, index_f = tourr::cmass(),
                          alpha = .4, cooling = .9, max.tries = 30)

test_that("basis_guided: class and dim", {
  expect_equal(class(ret_holes), c("matrix", "array"))
  expect_equal(class(ret_cmass), c("matrix", "array"))
  expect_equal(dim(ret_holes), c(5, 2))
  expect_equal(dim(ret_cmass), c(5, 2))
  expect_equal(is_orthonormal(ret_holes), TRUE)
  expect_equal(is_orthonormal(ret_cmass), TRUE)
})



### manip_var_of ------
ret <- manip_var_of(bas)
test_that("manip_var_of: class and dim", {
  expect_equal(class(ret), "integer")
  expect_equal(length(ret), 1)
  expect_warning(manip_var_of(not_orth))
})

## scale functions ----
s1 <- scale_sd(mtcars)
s2 <- scale_01(dat)
s3 <- scale_01(as.matrix(mtcars))

test_that("scale, class, bounds, dim", {
  expect_equal(class(s1), c("matrix", "array")) ## coerced to matrix.
  expect_equal(class(s2), c("matrix", "array"))
  expect_equal(class(s3), c("matrix", "array"))
  # expect_equal(min(s1), 0)
  # expect_equal(max(s1), 1)
  expect_equal(min(s2), 0)
  expect_equal(max(s2), 1)
  expect_equal(min(s3), 0)
  expect_equal(max(s3), 1)
  expect_equal(dim(s1), dim(mtcars))
  expect_equal(dim(s2), dim(dat))
  expect_equal(dim(s3), dim(mtcars))
})

##
## DEPRICATED:  -----
##

test_that("other basis_* class, orth, ", {
  expect_warning(sa <- scale_axes(mtcars))
  expect_equal(sa, map_relative(mtcars))
  expect_warning(pz <- pan_zoom(mtcars))
  expect_equal(pz, map_absolute(mtcars))
})
