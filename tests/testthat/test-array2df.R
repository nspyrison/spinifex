library("spinifex"); library("testthat");
if(F)
  ?array2df
## SETUP
dat <- tourr::rescale(wine[1:20, 2:14])
bas <- basis_pca(dat)
mv  <- manip_var_pca(dat)

## Array with a single frame, as used in view_frame()
single_frame <- array(bas, dim = c(dim(bas), 1))
attr(single_frame, "manip_var") <- mv
ret <- array2df(array = single_frame)

test_that("single frame, class and ncol correct", {
  expect_is(ret, "list")
  expect_equal(length(ret), 1)
  expect_equal(dim(ret[[1]]), c(13, 4)) ## basis dim
})

## Radial tour array to long df, as used in play_manual_tour()
tour_array <- manual_tour(basis = bas, manip_var = mv)
ret <- array2df(array = tour_array, data = dat,
                lab = paste0("MyLabs", 1:nrow(bas)))

test_that("with data, class and ncol correct", {
  expect_is(ret, "list")
  expect_equal(length(ret), 2)
  expect_equal(ncol(ret[[1]]), 4) ## basis ncols
  expect_equal(ncol(ret[[2]]), 3) ## data ncols
})

## tourr::save_history tour array to long df, as used in play_tour_path()
hist_array <- tourr::save_history(data = dat, max_bases = 10)
ret <- array2df(array = hist_array, data = dat,
                lab = paste0("MyLabs", 1:nrow(bas)))


test_that("without data, class and dim correct", {
  expect_is(ret, "list")
  expect_equal(ncol(ret[[1]]), 4)
})