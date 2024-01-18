{
  library(spinifex)
  library(testthat)
  
  dat_std <- scale_sd(wine[1L:10L, 2L:5L]) ## small chunk for speed.
  bas     <- basis_pca(dat_std)
  clas    <- wine$Type
  mv      <- manip_var_of(bas)
}

##
## TARGET WRAPPER FUNCTIONS -----
##

### play_tour_path -------
tpath <- save_history(dat_std, tour_path = tourr::grand_tour(), max = 5L)
suppressWarnings( ## suppress 8hr deprecation warning
  ret_light <- play_tour_path(tour_path = tpath, data = dat_std, angle = 1)
)
suppressWarnings( ## suppress 8hr deprecation warning
  ret_heavy <- play_tour_path(tour_path = tpath, data = dat_std, angle = 1,
                              axes = "bottomleft", fps = 8L,
                              aes_args = list(color = clas, shape = clas),
                              identity_args = list(size = .8, alpha = .7),
                              ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")),
                              render_type = render_gganimate)
)

test_that("play_tour_path: gganimate class and length", {
  expect_equal(class(ret_light) , c("plotly", "htmlwidget"))
  expect_equal(length(ret_light), 9L)
  expect_equal(class(ret_heavy) , c("gif_image"))
  expect_equal(length(ret_heavy), 1L)
})


### play_manual_tour -----

suppressWarnings( ## suppress 8hr deprecation warning
  ret_light <- play_manual_tour(basis = bas, data = dat_std, manip_var = mv, angle = 1)
)
suppressWarnings( ## suppress 8hr deprecation warning
  ret_heavy <- play_manual_tour(basis = bas, data = dat_std, manip_var = mv, angle = 1,
                                theta = .5 * pi, axes = "right", fps = 5L,
                                aes_args = list(color = clas, shape = clas),
                                identity_args = list(size = .8, alpha = .7),
                                ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")),
                                render_type = render_gganimate)
)

test_that("play_manual_tour: gganimate class and length", {
  expect_equal(class(ret_light), c("plotly", "htmlwidget"))
  expect_equal(class(ret_heavy), c("gif_image"))
  expect_equal(length(ret_light), 9L)
  expect_equal(length(ret_heavy), 1L)
})

##
## HELPER & INTERMEDIATE VISUALIZATIONS -----
##

### view_frame -----

rtheta <- runif(1L, 0L, 2L * pi)
rphi   <- runif(1L, 0L, 2L * pi)

suppressWarnings( ## suppress 8hr deprecation warning
  ret_light <- view_frame(basis = bas, data = dat_std, manip_var = mv)
)

suppressWarnings( ## suppress 8hr deprecation warning
  ret_heavy <- view_frame(basis = bas, data = dat_std, manip_var = mv,
                          theta = rtheta, phi = rphi,
                          aes_args = list(color = clas, shape = clas),
                          identity_args = list(size = .8, alpha = .7),
                          ggproto = list(ggplot2::theme_void(), ggplot2::ggtitle("My title")))
)

test_that("view_frame: gganimate class and length", {
  expected_length <- length(ggplot2::ggplot())
  expect_equal(class(ret_light), c("gg", "ggplot"))
  expect_equal(class(ret_heavy), c("gg", "ggplot"))
  expect_equal(length(ret_light), expected_length)
  expect_equal(length(ret_heavy), expected_length)
})


### view_manip_space -----

suppressWarnings( ## suppress 8hr deprecation warning
  ret_light <- view_manip_space(basis = bas, manip_var = mv)
)
suppressWarnings( ## suppress 8hr deprecation warning
  ret_heavy <- view_manip_space(basis = bas, manip_var = mv,
                                tilt = 2L / 12L * pi, basis_label = paste0("MyNm", 1L:ncol(dat_std)),
                                manip_col = "purple", manip_sp_col = "orange")
)

test_that("view_manip_space: gganimate class and length", {
  expected_length <- length(ggplot2::ggplot())
  expect_equal(class(ret_light), c("gg", "ggplot"))
  expect_equal(class(ret_heavy), c("gg", "ggplot"))
  expect_equal(length(ret_light), expected_length)
  expect_equal(length(ret_heavy), expected_length)
})

