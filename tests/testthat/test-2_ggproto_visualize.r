## Setup -----
library("spinifex")
library("testthat")

dat <- scale_sd(wine[1L:10L, 2L:5L]) ## small chunk for speed.
bas <- basis_pca(dat)
clas <- wine$Type
mv <- manip_var_of(bas)

mt <- manual_tour(bas, mv)
gt <- tourr::save_history(dat, max_bases = 3L)
gt1d <- tourr::save_history(dat, grand_tour(d = 1L), max_bases = 3L)

#' @examples 
#' gg_mt   <- ggtour(mt  ) + proto_default()
#' gg_gt   <- ggtour(gt  ) + proto_default()
#' gg_gt1d <- ggtour(gt1d) + proto_default1d()
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
gg_mt   <- ggtour(mt  ) + proto_default()
gg_gt   <- ggtour(gt  ) + proto_default()
gg_gt1d <- ggtour(gt1d) + proto_default1d()

test_that("ggtourr", {
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

## animate_plotly -----


## proto_basis -----


## proto_basis1d -----


## proto_point -----


## proto_origin -----


## proto_density -----


## proto_text -----


## proto_hex -----


## proto_default -----


