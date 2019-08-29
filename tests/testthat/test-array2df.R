context("array2df")

flea_std <- tourr::rescale(tourr::flea[1:2, 1:6])
rb <- basis_random(n = ncol(flea_std))
mtour <- manual_tour(basis = rb, manip_var = 4)
ret <- array2df(array = mtour, data = flea_std)

test_that("with data class and dim", {
  expect_is(ret, "list")
  expect_equal(length(ret), 2)
  expect_equal(ncol(ret[[1]]), 4) # basis
  expect_equal(ncol(ret[[2]]), 3) # data
})

ret <- array2df(array = mtour)

test_that("without data class and dim", {
  expect_is(ret, "list")
  expect_equal(ncol(ret[[1]]), 4)
})