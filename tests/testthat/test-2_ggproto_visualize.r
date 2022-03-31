## Setup -----
{
  library("spinifex")
  library("testthat")
  
  r_idx <- 1L:10L
  dat   <- scale_sd(wine[r_idx, 2L:5L]) ## small chunk for speed.
  bas   <- basis_pca(dat)
  mv    <- manip_var_of(bas)
  clas  <- wine$Type[r_idx]
  
  mt   <- manual_tour(bas      , mv, data = dat)
  mt1d <- manual_tour(bas[, 1L], mv, data = dat)
  .m <- capture.output(
    gt <- tourr::save_history(dat, guided_tour(holes()), max_bases = 3L)
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
  expect_equal(class(gg_mt  ), c("gg", "ggplot"))
  expect_equal(class(gg_gt  ), c("gg", "ggplot"))
  expect_equal(class(gg_mt1d), c("gg", "ggplot"))
  expect_equal(class(gg_gt1d), c("gg", "ggplot"))
})

## lapply_rep_len and eval(.init4proto)
## will rely on examples for now

## animate_gganimate -----
ag_mt   <- animate_gganimate(gg_mt  )
ag_gt   <- animate_gganimate(gg_gt  )
ag_mt1d <- animate_gganimate(gg_mt1d)
ag_gt1d <- animate_gganimate(gg_gt1d)
test_that("animate_gganimate", {
  expect_equal(class(ag_mt  ) , "gif_image")
  expect_equal(class(ag_gt  ) , "gif_image")
  expect_equal(class(ag_mt1d) , "gif_image")
  expect_equal(class(ag_gt1d) , "gif_image")
})

## animate_plotly -----
ap_mt   <- animate_plotly(gg_mt  )
ap_gt   <- animate_plotly(gg_gt  )
ap_mt1d <- animate_plotly(gg_mt1d)
ap_gt1d <- animate_plotly(gg_gt1d)
test_that("animate_plotly", {
  expect_equal(class(ap_mt  ), c("plotly", "htmlwidget"))
  expect_equal(class(ap_gt  ), c("plotly", "htmlwidget"))
  expect_equal(class(ap_mt1d), c("plotly", "htmlwidget"))
  expect_equal(class(ap_gt1d), c("plotly", "htmlwidget"))
})

## filmstrip -----
fs_mt   <- filmstrip(gg_mt  )
fs_gt   <- filmstrip(gg_gt  )
fs_mt1d <- filmstrip(gg_mt1d)
fs_gt1d <- filmstrip(gg_gt1d)
test_that("filmstrip", {
  expect_equal(class(fs_mt  ), c("gg", "ggplot"))
  expect_equal(class(fs_gt  ), c("gg", "ggplot"))
  expect_equal(class(fs_mt1d), c("gg", "ggplot"))
  expect_equal(class(fs_gt1d), c("gg", "ggplot"))
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
test_that("proto_basis/1d", {
  expect_equal(class(pb_mt  ), c("gg", "ggplot"))
  expect_equal(class(pb_gt  ), c("gg", "ggplot"))
  expect_equal(class(pb_mt1d), c("gg", "ggplot"))
  expect_equal(class(pb_gt1d), c("gg", "ggplot"))
})

## draw_basis -----
proj <- as.data.frame(dat %*% bas)
db <- ggplot() +
  geom_point(aes(PC1, PC2), proj) +
  draw_basis(bas, proj, "left") +
  coord_fixed()

test_that("draw_basis", {
  expect_equal(class(db), c("gg", "ggplot"))
})

## Aesthetics and basis on specific facet levels
proj <- cbind(proj, clas = clas)
bas  <- cbind(as.data.frame(bas), clas = levels(clas)[2])
ggplot() +
  facet_wrap(vars(clas)) +
  geom_point(aes(PC1, PC2, color = clas, shape = clas), proj) +
  draw_basis(bas, proj, "left") +
  theme_spinifex()

## proto_point & density-----
pp_mt   <- ggtour(mt  , angle = 1L) + proto_point()
pp_gt   <- ggtour(gt  , angle = 1L) + proto_point()
pd_mt1d <- ggtour(mt1d, angle = 1L) + proto_density()
pd_gt1d <- ggtour(gt1d, angle = 1L) + proto_density()
test_that("proto_:point/density", {
  expect_error(ggtour(gt1d, angle = 1L) + proto_point())
  expect_equal(class(pp_mt  ), c("gg", "ggplot"))
  expect_equal(class(pp_gt  ), c("gg", "ggplot"))
  expect_equal(class(pd_mt1d), c("gg", "ggplot"))
  expect_equal(class(pd_gt1d), c("gg", "ggplot"))
})

## proto_point & density with row_index & args-----
pp_mt   <- ggtour(mt  , angle = 1L) +
  proto_point(
    list(color = clas, shape = clas),
    list(alpha = .9, size = 2L), row_index = 1:3, "green")
pp_gt   <- ggtour(gt  , angle = 1L) +
  proto_point(
    list(color = clas, shape = clas),
    list(alpha = .9, size = 2L), row_index = 1:3, "green")
pd_mt1d <- ggtour(mt1d, angle = 1L) +
  proto_density(
    list(fill = clas, color = clas),
    list(alpha = .9, size = 2L), row_index = 1:3)
pd_gt1d <- ggtour(gt1d, angle = 1L) +
  proto_density(
    list(fill = clas, color = clas),
    list(alpha = .9, size = 2L), row_index = 1:3)
test_that("proto_:point/density", {
  expect_error(ggtour(gt1d, angle = 1L) + proto_point())
  expect_equal(class(pp_mt  ), c("gg", "ggplot"))
  expect_equal(class(pp_gt  ), c("gg", "ggplot"))
  expect_equal(class(pd_mt1d), c("gg", "ggplot"))
  expect_equal(class(pd_gt1d), c("gg", "ggplot"))
})



## proto_origin -----
po_mt   <- ggtour(mt,   angle = 1L) + proto_origin()
po_gt   <- ggtour(gt,   angle = 1L) + proto_origin()
po_mt1d <- ggtour(mt1d, angle = 1L) + proto_origin1d()
po_gt1d <- ggtour(gt1d, angle = 1L) + proto_origin1d()
test_that("proto_origin", {
  expect_error(ggtour(gt1d, angle = 1L) + proto_default())
  expect_equal(class(po_mt  ), c("gg", "ggplot"))
  expect_equal(class(po_gt  ), c("gg", "ggplot"))
  expect_equal(class(po_mt1d), c("gg", "ggplot"))
  expect_equal(class(po_gt1d), c("gg", "ggplot"))
})

## proto_text -----
pt_mt <- ggtour(mt, angle = 1L) + proto_text()
pt_gt <- ggtour(gt, angle = 1L) + proto_text()
test_that("proto_text", {
  expect_error(ggtour(gt1d, angle = 1L) + proto_text())
  expect_equal(class(pt_mt), c("gg", "ggplot"))
  expect_equal(class(pt_gt), c("gg", "ggplot"))
})

## proto_hex -----
ph_mt <- ggtour(mt, angle = 1L, data = dat) + proto_hex()
ph_gt <- ggtour(gt, angle = 1L, data = dat) + proto_hex()
test_that("proto_hex", {
  expect_error(ggtour(gt1d, angle = 1L) + proto_hex())
  expect_equal(class(ph_mt), c("gg", "ggplot"))
  expect_equal(class(ph_gt), c("gg", "ggplot"))
})

## proto_default -----
pd_mt   <- ggtour(mt  , angle = 1L) + proto_default()
pd_gt   <- ggtour(gt  , angle = 1L) + proto_default()
pd_mt1d <- ggtour(mt1d, angle = 1L) + proto_default1d()
pd_gt1d <- ggtour(gt1d, angle = 1L) + proto_default1d()
test_that("proto_default/1d", {
  expect_error(ggtour(gt1d, angle = 1L) + proto_default())
  expect_equal(class(pd_mt  ), c("gg", "ggplot"))
  expect_equal(class(pd_gt  ), c("gg", "ggplot"))
  expect_equal(class(pd_mt1d), c("gg", "ggplot"))
  expect_equal(class(pd_gt1d), c("gg", "ggplot"))
})

## proto_highlight -----
ph_mt   <- ggtour(mt  , angle = 1L) + proto_highlight(row_index = 1L)
ph_gt   <- ggtour(gt  , angle = 1L) + proto_highlight(row_index = 1L:2L)
ph_mt1d <- ggtour(mt1d, angle = 1L) + proto_highlight1d(row_index = 1L:2L)
ph_gt1d <- ggtour(gt1d, angle = 1L) + proto_highlight1d(row_index = 1L)
test_that("proto_highlight/1d", {
  expect_error(ggtour(gt1d, angle = 1L) + proto_default())
  expect_equal(class(ph_mt  ), c("gg", "ggplot"))
  expect_equal(class(ph_gt  ), c("gg", "ggplot"))
  expect_equal(class(ph_mt1d), c("gg", "ggplot"))
  expect_equal(class(ph_gt1d), c("gg", "ggplot"))
})

## proto_frame_cor2 -----
pfc_mt   <- ggtour(mt  , angle = 1L) + proto_frame_cor2(row_index = 1L)
pfc_gt   <- ggtour(gt  , angle = 1L) + proto_frame_cor2(row_index = 1L:2L)
test_that("proto_frame_cor2", {
  expect_error(ggtour(mt1d, angle = 1L) + proto_frame_cor2(row_index = 1L:2L))
  expect_error(ggtour(gt1d, angle = 1L) + proto_frame_cor2(row_index = 1L))
  expect_equal(class(pfc_mt), c("gg", "ggplot"))
  expect_equal(class(pfc_gt), c("gg", "ggplot"))
})

## append_fixed_y -----
afy_mt   <- ggtour(mt  , angle = 1L) + append_fixed_y(1L) + proto_point(row_index = 1L)
afy_gt   <- ggtour(gt  , angle = 1L) + append_fixed_y(1L) + proto_point(row_index = 1L:2L)
afy_mt1d <- ggtour(mt1d, angle = 1L) + append_fixed_y(1L) + proto_point(row_index = 1L:2L)
afy_gt1d <- ggtour(gt1d, angle = 1L) + append_fixed_y(1L) + proto_point(row_index = 1L)
test_that("append_fixed_y", {
  expect_equal(class(afy_mt  ), c("gg", "ggplot"))
  expect_equal(class(afy_gt  ), c("gg", "ggplot"))
  expect_equal(class(afy_mt1d), c("gg", "ggplot"))
  expect_equal(class(afy_gt1d), c("gg", "ggplot"))
})


## facet_wrap_tour -----
fwt_mt   <- ggtour(mt  , angle = 1L) + facet_wrap_tour(clas) + proto_point(row_index = 1L)
fwt_gt   <- ggtour(gt  , angle = 1L) + facet_wrap_tour(clas) + proto_point(row_index = 1L:2L)
fwt_mt1d <- ggtour(mt1d, angle = 1L) + facet_wrap_tour(clas) + proto_density(row_index = 1L:2L)
fwt_gt1d <- ggtour(gt1d, angle = 1L) + facet_wrap_tour(clas) + proto_density(row_index = 1L)
test_that("facet_wrap_tour", {
  expect_equal(class(fwt_mt  ), c("gg", "ggplot"))
  expect_equal(class(fwt_gt  ), c("gg", "ggplot"))
  expect_equal(class(fwt_mt1d), c("gg", "ggplot"))
  expect_equal(class(fwt_gt1d), c("gg", "ggplot"))
})


## expect cycle warning ----
dat  <- scale_sd(penguins_na.rm[, 1:4]) ## PENG
clas <- flea$species                    ## FLEAS
bas  <- matrix(c(1,2,3,4), ncol=1)      ## NON ortho bas

test_that("manual tour not ortho basis", {
  expect_warning(mt <- manual_tour(bas, manip_var = 2))
})

test_that(".lapply_rep_len cycle check", {
  expect_warning(ggt <- ggtour(mt, dat, angle = .3) +
    proto_density(aes_args = list(color = clas, fill = clas)))
})

