

library("spinifex")
flea_sd <- scale_sd(tourr::flea[, 1:6])
clas <- tourr::flea$species

rb <- tourr::basis_random(ncol(flea_sd), 2)
ib <- tourr::basis_init(n = 4, 2)
b_pca <- basis_pca(flea_sd)
b_lda <- basis_lda(flea_sd, clas)
b_guide <- basis_guided(data = flea_sd, index_f = tourr::holes())
diag4 <- diag(4)
not_orth <- matrix(sample(1:16, 16), ncol=4)
### MATH AND TRANSFORMS -----

### _is_orthonormal -----
test_that("is_orthonormal: bases functions & diag() are orthonormal, rand matrix isn't", {
  expect_true(is_orthonormal(rb))
  expect_true(is_orthonormal(ib))
  expect_true(is_orthonormal(b_pca))
  expect_true(is_orthonormal(b_lda))
  expect_true(is_orthonormal(b_guide))
  expect_true(is_orthonormal(diag4))
  expect_false(is_orthonormal(not_orth))
})

ret_t <- is_orthonormal(rb)
ret_f <- is_orthonormal(not_orth)
test_that("is_orthonormal: returns are logical ", {
  expect_is(ret_t, "logical")
  expect_is(ret_f, "logical")
})

# _array2df -----
dat_std <- scale_10(wine[, 2:14])
clas <- wine$Type
bas <- basis_pca(dat_std)
mv <- manip_var_pca(dat_std)

## Array with a single frame, as used in view_frame()
single_frame <- array(bas, dim = c(dim(bas), 1))
attr(single_frame, "manip_var") <- mv
array2df(array = single_frame)

## Radial tour array to long df, as used in play_manual_tour()
tour_array <- manual_tour(basis = bas, manip_var = mv)
array2df(array = tour_array, data = dat_std,
         label = paste0("MyLabs", 1:nrow(bas)))

test_that("scale_axes: class and dim", {
  expect_is(ret, "matrix")
  expect_equal(dim(ret), c(6, 2))
})


ret <- scale_axes(x = rb, position = "bottomleft")

test_that("scale_axes: class and dim", {
  expect_is(ret, "matrix")
  expect_equal(dim(ret), c(6, 2))
})

ret <- view_basis(basis = rb, data = flea_std, position = "bottomleft")

test_that("view_basis: gganimate class and length", {
  expect_is(ret, "gg")
  expect_is(ret, "ggplot")
  expect_equal(length(ret), 9)
})

ret <- view_manip_space(basis = rb, manip_var = 4)

test_that("view_manip_space: gganimate class and length", {
  expect_is(ret, "gg")
  expect_is(ret, "ggplot")
  expect_equal(length(ret), 9)
})

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

