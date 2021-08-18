## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  echo       = TRUE,   # code
  include    = TRUE,   # plots
  results    = "show", # text: "hide", "show"
  eval       = TRUE,   # chunk
  message    = FALSE,
  warning    = FALSE,
  error      = FALSE,
  collapse   = TRUE,
  comment    = "#>",
  fig.height = 4,
  fig.width  = 6,
  fig.align  = "center",
  cache      = FALSE
)

## -----------------------------------------------------------------------------
library("ggplot2")
library("magrittr")

## A ggproto:
gp <- geom_point()
class(gp)

## A list of ggplot elements, a 'head-less' ggplot call
gg_ls <- list(
  gp,
  geom_smooth(method = "loess", formula = y ~ x),
  ggtitle("proto_* functions return lists of geoms_* functions.", "These lists can be stored, extended, and added to ggplot(). \n We use this to include the animation of ggplots."),
  facet_grid(cols = vars(Tree))
)
lapply(gg_ls, class)

## ggplot call, without geoms, a 'body-less' ggplot call
gghead <- ggplot(Orange, aes(age, circumference, color = Tree))

## Evaluate together
gghead +
  gg_ls

## ---- echo = FALSE------------------------------------------------------------
data.frame(
  object = c("head", "body", "render"),
  ggplot2 = c("ggplot()", "geom_*()", "NA"),
  `ggproto api` = c("ggtour()", "proto_*()", "animate_*()"),
  `previous api` = c("play_manual_tour()", "play_manual_tour()", "play_manual_tour(render_*())")
) %>%
  knitr::kable()

## -----------------------------------------------------------------------------
library("tourr")
library("spinifex")

## Scale our numeric data
dat <- scale_sd(tourr::flea[, 1:6])
## Use species as a class to set color and shape with
clas <- tourr::flea$species

## Manual tour, manipulating the contribution of a selected variable 
bas <- basis_pca(dat) ## Start basis
mv <- manip_var_of(bas) ## Number of the variable to manipulate
mt_path <- manual_tour(bas, manip_var = mv) ## Tour path

## Create a static ggplot2 plot with all frames of the tour
ggt <- ggtour(mt_path, dat, angle = .1) +
  proto_basis() +
  proto_point(aes_args = list(color = clas, shape = clas),
              identity_args = list(size = 1.5))

## ---- eval=FALSE--------------------------------------------------------------
#  ## Animate the tour with an animate_* function.
#  animate_plotly(ggt, fps = 5) ## As a plotly .html widget.
#  #animate_gganimate(ggt, fps = 8, rewind = TRUE) ## As a gganimate .gif

## -----------------------------------------------------------------------------
## (Quietly create) a grand tour, projecting through randomly selected bases
.mute <- utils::capture.output(
  gt_path <- tourr::save_history(dat, grand_tour(), max_bases = 3)
)

## Static ggplot of all frames in the tour
ggt <- ggtour(gt_path, dat, angle = .1) + ## Include geodesic interpolation angle between the selected bases.
  proto_basis(position = "topright") +
  proto_point(list(color = clas, shape = clas))

## ---- eval=FALSE--------------------------------------------------------------
#  ## Animate
#  animate_plotly(ggt)
#  animate_gganimate(ggt)

## -----------------------------------------------------------------------------
## (Quietly create) a 1d guided tour, optimizing the projection space for the holes() function
guided_path <- save_history(dat, guided_tour(holes(), d = 1))

## Static ggplot of all frames in the tour
ggt <- ggtour(guided_path, dat, angle = .1) + ## Include geodesic interpolation angle between the selected bases.
  proto_basis1d() +
  proto_density(list(fill = clas), density_position = "stack")
## Note that proto_density(density_position = "stack") does not work with animate_plotly().

## ---- eval=FALSE--------------------------------------------------------------
#  ## Animate
#  animate_gganimate(ggt)

## -----------------------------------------------------------------------------
ggt <- ggt +
  theme_bw() +
  ggtitle("My Tour animation") +
  labs(x = "Projection Y1", y = "density")

## ---- eval=FALSE--------------------------------------------------------------
#  animate_gganimate(ggt)

## ---- echo = FALSE------------------------------------------------------------
data.frame(
  `proto functions` =
    c("ggtour", "proto_point", "proto_text", "proto_hex", "proto_origin/1d", "proto_density", "proto_basis/1d", "proto_default/1d", "animate_plotly", "animate_gganimate"),
  `related ggplot2 function` =
    c("ggplot", "geom_point", "geom_text", "geom_hex", "NA", "geom- _density & _rect", "geom- _segment & _text", "several protos", "plotly::ggplotly (with animation)", "gganimate::animate"),
  detail =
    c("Also perfroms setup for the tour.", "-", "-", "Heatmap hexegons, for high observation density", "Line segments for the origin, the space where 0 values project to", "1D density with run hash marks underneath, `position = 'stack'` not working with {plotly}.", ".html widget, row numbers added as tooltip on hover. {plotly} doesn't presicly map all {ggplot2} settings; legends, point size and opacity may vary.", "Direction and magnetude of variables to the projection disp~", "Default protos for 2/1D tours", ".gif animation. {gganimate} consumes native ggplots, aestheics should be consistant."), check.names = F
) %>%
  knitr::kable()

