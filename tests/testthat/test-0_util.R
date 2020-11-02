library("spinifex")
library("testthat")
flea_std <- scale_sd(tourr::flea[, 1:6])
clas <- tourr::flea$species

rb <- tourr::basis_random(ncol(flea_std), 2)
ib <- tourr::basis_init(n = 4, 2)
b_pca <- basis_pca(flea_std)
b_guide <- basis_guided(data = flea_std, index_f = tourr::holes())
diag4 <- diag(4)
not_orth <- matrix(sample(1:16, 16), ncol=4)
### MATH AND TRANSFORMS -----

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
dat_std <- scale_10(wine[, 2:14])
clas <- wine$Type
bas <- basis_pca(dat_std)
mv <- manip_var_pca(dat_std)

## Array with a single frame, as used in view_frame()
single_frame <- array(bas, dim = c(dim(bas), 1))
attr(single_frame, "manip_var") <- mv
ret_single <- array2df(array = single_frame)

## Radial tour array to long df, as used in play_manual_tour()
tour_array <- manual_tour(basis = bas, manip_var = mv)
ret_spinifex <- array2df(array = tour_array, data = dat_std,
         label = paste0("MyLabs", 1:nrow(bas)))


## tourr::save_history tour array to long df, as used in play_tour_path()
hist_array <- tourr::save_history(data = dat_std, max_bases = 10)
class(hist_array) <- "array"
ret_tourr <- array2df(array = hist_array, data = dat_std,
                      label = paste0("MyLabs", 1:nrow(bas)))

test_that("array2df: class and dim for single frame, spinifex and tourr case.", {
  expect_is(ret_single, "list")
  expect_is(ret_spinifex, "list")
  expect_is(ret_tourr, "list")
  expect_equal(length(ret_single),   1)
  expect_equal(length(ret_spinifex), 2)
  expect_equal(length(ret_tourr),    2)
  expect_equal(dim(ret_single[[1]]),   c(13,  4))
  expect_equal(dim(ret_spinifex[[1]]), c(884, 4))
  expect_equal(dim(ret_tourr[[1]]),    c(130, 4))
})




### scale_axes ------

ret <- scale_axes(x = rb, position = "bottomleft")
ret_to <- scale_axes(x = rb, position = "topright", to = wine[, 2:3])

test_that("scale_axes: class and dim, with and without 'to' arg", {
  expect_is(ret, "matrix")
  expect_is(ret_to, "matrix")
  expect_equal(dim(ret), c(6, 2))
  expect_equal(dim(ret_to), c(6, 2))
})


### pan_zoom -----

ret <- pan_ zoom(pan = c(-1, 0), zoom = c(2/3, 2/3), x = rb)



### DEPRICATED: color_of, shape_of -----
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

