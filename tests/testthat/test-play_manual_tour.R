context("play_manual_tour")

flea_std <- tourr::rescale(tourr::flea[,1:6])
rb <- tourr::basis_random(n = ncol(flea_std))
# play_manual_tour(data = flea_std, basis = rb, manip_var = 4)
### TODO: Error in layer(data = data, mapping = mapping, stat = stat, geom = GeomPoint,  : 
##### promise already under evaluation: recursive default argument reference or earlier problems?

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
