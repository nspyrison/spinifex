## Setup -----
library("spinifex")
library("testthat")

dat <- scale_sd(wine[1L:10L, 2L:5L]) ## small chunk for speed.
bas <- basis_pca(dat)
clas <- wine$Type
mv <- manip_var_of(bas)

mt <- manual_tour(bas, mv, angle = 1L)
.mute <- capture.output(
  gt <- tourr::save_history(dat, guided_tour(holes()), max_bases = 3L)
)
.mute <- capture.output(
  gt1d <- tourr::save_history(dat, grand_tour(d = 1L), max_bases = 3L)
)

#' @examples 
#' gg_mt   <- ggtour(mt) + proto_default()
#' gg_gt   <- ggtour(gt, angle=1)   + proto_default()
#' gg_gt1d <- ggtour(gt1d, angle=1) + proto_default1d()
#' 
#' test_that("ggtourr", {
#'   expect_is(gg_mt  , c("gg", "ggplot"))
#'   expect_is(gg_gt  , c("gg", "ggplot"))
#'   expect_is(gg_gt1d, c("gg", "ggplot"))
#'   expect_equal(length(gg_mt  ), 9L)
#'   expect_equal(length(gg_gt  ), 9L)
#'   expect_equal(length(gg_gt1d), 9L)
#' })

## ggtourr -----
gg_mt   <- ggtour(mt  , dat       ) + proto_default()
gg_gt   <- ggtour(gt  , angle = 1L) + proto_default()
gg_gt1d <- ggtour(gt1d, angle = 1L) + proto_default1d()
test_that("ggtourr", {
  expect_error(ggtour(mt) + proto_default())
  expect_is(gg_mt  , c("gg", "ggplot"))
  expect_is(gg_gt  , c("gg", "ggplot"))
  expect_is(gg_gt1d, c("gg", "ggplot"))
  expect_equal(length(gg_mt  ), 9L)
  expect_equal(length(gg_gt  ), 9L)
  expect_equal(length(gg_gt1d), 9L)
})

## lapply_rep_len
## will rely on examples for now

## .init4proto
## will rely on examples for now

## animate_gganimate -----
ag_mt   <- animate_gganimate(gg_mt  )
ag_gt   <- animate_gganimate(gg_gt  )
ag_gt1d <- animate_gganimate(gg_gt1d)
test_that("animate_gganimate", {
  expect_true(class(ag_mt  ) %in% c("gif_image", "character"))
  expect_true(class(ag_gt  ) %in% c("gif_image", "character"))
  expect_true(class(ag_gt1d) %in% c("gif_image", "character"))
  expect_true(length(ag_mt)   %in% c(1L, 99L, 100L))
  expect_true(length(ag_gt)   %in% c(1L, 99L, 100L))
  expect_true(length(ag_gt1d) %in% c(1L, 99L, 100L))
})

## animate_plotly -----
ap_mt   <- animate_plotly(gg_mt  )
ap_gt   <- animate_plotly(gg_gt  )
ap_gt1d <- animate_plotly(gg_gt1d)
test_that("animate_plotly", {
  expect_is(ap_mt  , c("plotly", "htmlwidget"))
  expect_is(ap_gt  , c("plotly", "htmlwidget"))
  expect_is(ap_gt1d, c("plotly", "htmlwidget"))
  expect_equal(length(ap_mt  ), 9L)
  expect_equal(length(ap_gt  ), 9L)
  expect_equal(length(ap_gt1d), 9L)
})

## proto_basis -----
pb_mt   <- ggtour(mt  , dat       ) + proto_basis()
pb_gt   <- ggtour(gt  , angle = 1L) + proto_basis()
pb_gt1d <- ggtour(gt1d, angle = 1L) + proto_basis1d()
test_that("proto_basis", {
  expect_is(pb_mt  , c("gg", "ggplot"))
  expect_is(pb_gt  , c("gg", "ggplot"))
  expect_is(pb_gt1d, c("gg", "ggplot"))
  expect_equal(length(pb_mt  ), 9L)
  expect_equal(length(pb_gt  ), 9L)
  expect_equal(length(pb_gt1d), 9L)
})

## proto_point  & density-----
pp_mt   <- ggtour(mt  , dat       ) + proto_point()
pp_gt   <- ggtour(gt,   angle = 1L) + proto_point()
pd_gt1d <- ggtour(gt1d, angle = 1L) + proto_density()
test_that("proto_point", {
  expect_error(ggtour(gt1d, angle = 1L) + proto_point())
  expect_is(pp_mt  , c("gg", "ggplot"))
  expect_is(pp_gt  , c("gg", "ggplot"))
  expect_is(pd_gt1d, c("gg", "ggplot"))
  expect_equal(length(pp_mt  ), 9L)
  expect_equal(length(pp_gt  ), 9L)
  expect_equal(length(pd_gt1d), 9L)
})


## proto_origin -----
po_mt   <- ggtour(mt, dat         ) + proto_origin()
po_gt   <- ggtour(gt,   angle = 1L) + proto_origin()
po_gt1d <- ggtour(gt1d, angle = 1L) + proto_origin1d()
test_that("proto_basis", {
  expect_error(ggtour(gt1d, angle=1) + proto_origin())
  expect_is(po_mt  , c("gg", "ggplot"))
  expect_is(po_gt  , c("gg", "ggplot"))
  expect_is(po_gt1d, c("gg", "ggplot"))
  expect_equal(length(po_mt  ), 9L)
  expect_equal(length(po_gt  ), 9L)
  expect_equal(length(po_gt1d), 9L)
})

## proto_text -----
pt_mt <- ggtour(mt, dat       ) + proto_text()
pt_gt <- ggtour(gt, angle = 1L) + proto_text()
test_that("proto_basis", {
  expect_error(ggtour(gt1d, angle = 1L) + proto_text())
  expect_is(pt_mt, c("gg", "ggplot"))
  expect_is(pt_gt, c("gg", "ggplot"))
  expect_equal(length(pt_mt), 9L)
  expect_equal(length(pt_gt), 9L)
})

## proto_hex -----
ph_mt <- ggtour(mt  , dat    ) + proto_hex()
ph_gt <- ggtour(gt, angle = 1) + proto_hex()
test_that("proto_basis", {
  expect_error(ggtour(gt1d, angle = 1) + proto_hex())
  expect_is(ph_mt, c("gg", "ggplot"))
  expect_is(ph_gt, c("gg", "ggplot"))
  expect_equal(length(ph_mt), 9L)
  expect_equal(length(ph_gt), 9L)
})

## proto_default -----
pd_mt   <- ggtour(mt  , dat       )+ proto_default()
pd_gt   <- ggtour(gt  , angle = 1L) + proto_default()
pd_gt1d <- ggtour(gt1d, angle = 1L) + proto_default1d()
test_that("proto_basis", {
  expect_error(ggtour(gt1d, angle = 1L) + proto_default())
  expect_is(pt_mt  , c("gg", "ggplot"))
  expect_is(pt_gt  , c("gg", "ggplot"))
  expect_is(pd_gt1d, c("gg", "ggplot"))
  expect_equal(length(pt_mt  ), 9L)
  expect_equal(length(pt_gt  ), 9L)
  expect_equal(length(pd_gt1d), 9L)
})

