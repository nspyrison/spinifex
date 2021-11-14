## Setup -----
{
  library("spinifex")
  library("testthat")
  
  r_idx <- 1L:10L
  dat <- scale_sd(wine[r_idx, 2L:5L]) ## small chunk for speed.
  bas <- basis_pca(dat)
  mv  <- manip_var_of(bas)
  clas <- wine[r_idx, ]
  
  mt     <- manual_tour(bas      , mv, data = dat)
  mt1d   <- manual_tour(bas[, 1L], mv, data = dat)
  .m <- capture.output(
    gt   <- tourr::save_history(dat, guided_tour(holes()), max_bases = 3L)
  )
  .m <- capture.output(
    gt1d <- tourr::save_history(dat, grand_tour(d = 1L),   max_bases = 3L)
  )
}


## ggtourr -----
gg_mt   <- ggtour(mt  , angle = 1L) + proto_default()
gg_gt   <- ggtour(gt  , angle = 1L) + proto_default()
gg_mt1d <- ggtour(mt1d, angle = 1L) + proto_default1d()
gg_gt1d <- ggtour(gt1d, angle = 1L) + proto_default1d()
test_that("ggtourr", {
  expect_is(gg_mt  , c("gg", "ggplot"))
  expect_is(gg_gt  , c("gg", "ggplot"))
  expect_is(gg_mt1d, c("gg", "ggplot"))
  expect_is(gg_gt1d, c("gg", "ggplot"))
  expect_equal(length(gg_mt  ), 9L)
  expect_equal(length(gg_gt  ), 9L)
  expect_equal(length(gg_mt1d), 9L)
  expect_equal(length(gg_gt1d), 9L)
})

## lapply_rep_len and eval(.init4proto)
## will rely on examples for now

## animate_gganimate -----
ag_mt   <- animate_gganimate(gg_mt  )
ag_gt   <- animate_gganimate(gg_gt  )
ag_mt1d <- animate_gganimate(gg_mt1d)
ag_gt1d <- animate_gganimate(gg_gt1d)
test_that("animate_gganimate", {
  expect_true(class(ag_mt  ) %in% c("gif_image", "character"))
  expect_true(class(ag_gt  ) %in% c("gif_image", "character"))
  expect_true(class(ag_mt1d) %in% c("gif_image", "character"))
  expect_true(class(ag_gt1d) %in% c("gif_image", "character"))
  expect_true(length(ag_mt)   %in% c(1L, 99L, 100L))
  expect_true(length(ag_gt)   %in% c(1L, 99L, 100L))
  expect_true(length(ag_mt1d) %in% c(1L, 99L, 100L))
  expect_true(length(ag_gt1d) %in% c(1L, 99L, 100L))
})

## animate_plotly -----
ap_mt   <- animate_plotly(gg_mt  )
ap_gt   <- animate_plotly(gg_gt  )
ap_mt1d <- animate_plotly(gg_mt1d)
ap_gt1d <- animate_plotly(gg_gt1d)
test_that("animate_plotly", {
  expect_is(ap_mt  , c("plotly", "htmlwidget"))
  expect_is(ap_gt  , c("plotly", "htmlwidget"))
  expect_is(ap_mt1d, c("plotly", "htmlwidget"))
  expect_is(ap_gt1d, c("plotly", "htmlwidget"))
  expect_equal(length(ap_mt  ), 9L)
  expect_equal(length(ap_gt  ), 9L)
  expect_equal(length(ap_mt1d), 9L)
  expect_equal(length(ap_gt1d), 9L)
})

## filmstrip -----
fs_mt   <- filmstrip(gg_mt  )
fs_gt   <- filmstrip(gg_gt  )
fs_mt1d <- filmstrip(gg_mt1d)
fs_gt1d <- filmstrip(gg_gt1d)
test_that("filmstrip", {
  expect_is(fs_mt  , c("gg", "ggplot"))
  expect_is(fs_gt  , c("gg", "ggplot"))
  expect_is(fs_mt1d, c("gg", "ggplot"))
  expect_is(fs_gt1d, c("gg", "ggplot"))
  expect_equal(length(fs_mt  ), 9L)
  expect_equal(length(fs_gt  ), 9L)
  expect_equal(length(fs_mt1d), 9L)
  expect_equal(length(fs_gt1d), 9L)
})

## proto_basis -----
pb_mt   <- ggtour(mt  , angle = 1L) + proto_basis()
pb_gt   <- ggtour(gt  , angle = 1L) + proto_basis()
pb_mt1d <- ggtour(mt1d, angle = 1L) + proto_basis1d()
pb_gt1d <- ggtour(gt1d, angle = 1L) + proto_basis1d()
test_that("proto_basis", {
  expect_is(pb_mt  , c("gg", "ggplot"))
  expect_is(pb_gt  , c("gg", "ggplot"))
  expect_is(pb_mt1d, c("gg", "ggplot"))
  expect_is(pb_gt1d, c("gg", "ggplot"))
  expect_equal(length(pb_mt  ), 9L)
  expect_equal(length(pb_gt  ), 9L)
  expect_equal(length(pb_mt1d), 9L)
  expect_equal(length(pb_gt1d), 9L)
})

## proto_point & density-----
pp_mt   <- ggtour(mt  , angle = 1L) + proto_point()
pp_gt   <- ggtour(gt  , angle = 1L) + proto_point()
pd_mt1d <- ggtour(mt1d, angle = 1L) + proto_density()
pd_gt1d <- ggtour(gt1d, angle = 1L) + proto_density()
test_that("proto_:point/density", {
  expect_error(ggtour(gt1d, angle = 1L) + proto_point())
  expect_is(pp_mt  , c("gg", "ggplot"))
  expect_is(pp_gt  , c("gg", "ggplot"))
  expect_is(pd_mt1d, c("gg", "ggplot"))
  expect_is(pd_gt1d, c("gg", "ggplot"))
  expect_equal(length(pp_mt  ), 9L)
  expect_equal(length(pp_gt  ), 9L)
  expect_equal(length(pd_mt1d), 9L)
  expect_equal(length(pd_gt1d), 9L)
})


## proto_origin -----
po_mt   <- ggtour(mt,   angle = 1L) + proto_origin()
po_gt   <- ggtour(gt,   angle = 1L) + proto_origin()
po_mt1d <- ggtour(mt1d, angle = 1L) + proto_origin1d()
po_gt1d <- ggtour(gt1d, angle = 1L) + proto_origin1d()
test_that("proto_origin", {
  expect_error(ggtour(gt1d, angle = 1L) + proto_origin())
  expect_is(po_mt  , c("gg", "ggplot"))
  expect_is(po_gt  , c("gg", "ggplot"))
  expect_is(po_mt1d, c("gg", "ggplot"))
  expect_is(po_gt1d, c("gg", "ggplot"))
  expect_equal(length(po_mt  ), 9L)
  expect_equal(length(po_gt  ), 9L)
  expect_equal(length(po_mt1d), 9L)
  expect_equal(length(po_gt1d), 9L)
})

## proto_text -----
pt_mt <- ggtour(mt, angle = 1L) + proto_text()
pt_gt <- ggtour(gt, angle = 1L) + proto_text()
test_that("proto_text", {
  expect_error(ggtour(gt1d, angle = 1L) + proto_text())
  expect_is(pt_mt, c("gg", "ggplot"))
  expect_is(pt_gt, c("gg", "ggplot"))
  expect_equal(length(pt_mt), 9L)
  expect_equal(length(pt_gt), 9L)
})

## proto_hex -----
ph_mt <- ggtour(mt, angle = 1L, data = dat) + proto_hex()
ph_gt <- ggtour(gt, angle = 1L, data = dat) + proto_hex()
test_that("proto_hex", {
  expect_error(ggtour(gt1d, angle = 1L) + proto_hex())
  expect_is(ph_mt, c("gg", "ggplot"))
  expect_is(ph_gt, c("gg", "ggplot"))
  expect_equal(length(ph_mt), 9L)
  expect_equal(length(ph_gt), 9L)
})

## proto_default -----
pd_mt   <- ggtour(mt  , angle = 1L) + proto_default()
pd_gt   <- ggtour(gt  , angle = 1L) + proto_default()
pd_mt1d <- ggtour(mt1d, angle = 1L) + proto_default1d()
pd_gt1d <- ggtour(gt1d, angle = 1L) + proto_default1d()
test_that("proto_default", {
  expect_error(ggtour(gt1d, angle = 1L) + proto_default())
  expect_is(pd_mt  , c("gg", "ggplot"))
  expect_is(pd_gt  , c("gg", "ggplot"))
  expect_is(pd_mt1d, c("gg", "ggplot"))
  expect_is(pd_gt1d, c("gg", "ggplot"))
  expect_equal(length(pd_mt  ), 9L)
  expect_equal(length(pd_gt  ), 9L)
  expect_equal(length(pd_mt1d), 9L)
  expect_equal(length(pd_gt1d), 9L)
})

## proto_highlight -----
ph_mt   <- ggtour(mt  , angle = 1L) + proto_highlight(row_index = 1L)
ph_gt   <- ggtour(gt  , angle = 1L) + proto_highlight(row_index = 1L:2L)
ph_mt1d <- ggtour(mt1d, angle = 1L) + proto_highlight1d(row_index = 1L:2L)
ph_gt1d <- ggtour(gt1d, angle = 1L) + proto_highlight1d(row_index = 1L)
test_that("proto_highlight", {
  expect_error(ggtour(gt1d, angle = 1L) + proto_default())
  expect_is(ph_mt  , c("gg", "ggplot"))
  expect_is(ph_gt  , c("gg", "ggplot"))
  expect_is(ph_mt1d, c("gg", "ggplot"))
  expect_is(ph_gt1d, c("gg", "ggplot"))
  expect_equal(length(ph_mt  ), 9L)
  expect_equal(length(ph_gt  ), 9L)
  expect_equal(length(ph_mt1d), 9L)
  expect_equal(length(ph_gt1d), 9L)
})

## proto_frame_cor2 -----
pfc_mt   <- ggtour(mt  , angle = 1L) + proto_frame_cor2(row_index = 1L)
pfc_gt   <- ggtour(gt  , angle = 1L) + proto_frame_cor2(row_index = 1L:2L)

test_that("proto_frame_cor2", {
  expect_error(ggtour(mt1d, angle = 1L) + proto_frame_cor2(row_index = 1L:2L))
  expect_error(ggtour(gt1d, angle = 1L) + proto_frame_cor2(row_index = 1L))
  expect_is(pfc_mt, c("gg", "ggplot"))
  expect_is(pfc_gt, c("gg", "ggplot"))
  expect_equal(length(pfc_mt), 9L)
  expect_equal(length(pfc_gt), 9L)
})

## append_fixed_y -----
afy_mt   <- ggtour(mt  , angle = 1L) + append_fixed_y(1L) + proto_point(row_index = 1L)
afy_gt   <- ggtour(gt  , angle = 1L) + append_fixed_y(1L) + proto_point(row_index = 1L:2L)
afy_mt1d <- ggtour(mt1d, angle = 1L) + append_fixed_y(1L) + proto_point(row_index = 1L:2L)
afy_gt1d <- ggtour(gt1d, angle = 1L) + append_fixed_y(1L) + proto_point(row_index = 1L)
test_that("append_fixed_y", {
  expect_is(afy_mt  , c("gg", "ggplot"))
  expect_is(afy_gt  , c("gg", "ggplot"))
  expect_is(afy_mt1d, c("gg", "ggplot"))
  expect_is(afy_gt1d, c("gg", "ggplot"))
  expect_equal(length(afy_mt  ), 9L)
  expect_equal(length(afy_gt  ), 9L)
  expect_equal(length(afy_mt1d), 9L)
  expect_equal(length(afy_gt1d), 9L)
})

facet_wrap_tour


## facet_wrap_tour -----
fwt_mt   <- ggtour(mt  , angle = 1L) + facet_wrap_tour(clas) + proto_point(row_index = 1L)
fwt_gt   <- ggtour(gt  , angle = 1L) + facet_wrap_tour(clas) + proto_point(row_index = 1L:2L)
fwt_mt1d <- ggtour(mt1d, angle = 1L) + facet_wrap_tour(clas) + proto_density(row_index = 1L:2L)
fwt_gt1d <- ggtour(gt1d, angle = 1L) + facet_wrap_tour(clas) + proto_density(row_index = 1L)
test_that("facet_wrap_tour", {
  expect_is(fwt_mt  , c("gg", "ggplot"))
  expect_is(fwt_gt  , c("gg", "ggplot"))
  expect_is(fwt_mt1d, c("gg", "ggplot"))
  expect_is(fwt_gt1d, c("gg", "ggplot"))
  expect_equal(length(fwt_mt  ), 9L)
  expect_equal(length(fwt_gt  ), 9L)
  expect_equal(length(fwt_mt1d), 9L)
  expect_equal(length(fwt_gt1d), 9L)
})
