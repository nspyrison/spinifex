context("render_gganimate")

library("spinifex")
flea_std <- tourr::rescale(tourr::flea[1:2, 1:6])
rb <- tourr::basis_random(n = ncol(flea_std))
mtour <- manual_tour(basis = rb, manip_var = 4)
df_frames <- array2df(array = mtour, data = flea_std)
ret <- render_gganimate(frames = df_frames)

test_that("gganimate type", {
  expect_type(ret, "character")
})


cat <- tourr::flea[1:2, 7]
ret <- render_gganimate(frames = df_frames, col = cat, pch = cat,
                        lab = paste0("a", 1:6), axes = "off")

test_that("gganimate type", {
  expect_type(ret, "character")
})