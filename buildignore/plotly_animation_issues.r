# Plotly contention issue:
## some geoms fight for existence across frames
## seems to stem from differing dim basis (p*d*frames) and data (n*p*frames) -----
if(F) ##
  browseURL("https://github.com/plotly/plotly.R/issues/2060")

##Example: spinifex::ggtour doc with issue
library(spinifex)
dat <- scale_sd(tourr::flea[, 1:6])
clas <- tourr::flea$species
bas <- basis_pca(dat)
mv <- manip_var_of(bas)
mt_path <- manual_tour(bas, manip_var = mv)

## d = 2 case
ggt <- ggtour(mt_path, dat, angle = .15) +
  proto_basis() +
  proto_point(list(color = clas, shape = clas),
              list(size = 1.5))
## Not run:
animate_plotly(ggt)


## Idea: would plotly ids help?? -----
### _ie:_ is plotly improperly recycling something across frames?
if(F){
  browseURL("https://linking.plotly-r.com/animating-views.html#fig:animation-ggplotly")
  ## Example: above link fig 14.1
  data(gapminder, package = "gapminder")
  gg <- ggplot(gapminder, aes(gdpPercap, lifeExp, color = continent)) +
    geom_point(aes(size = pop, frame = year, ids = country)) +
    scale_x_log10()
  ggplotly(gg)
}

## toy example
library(ggplot2)
library(plotly)



## animation, aes has frame, NO ids
g_anim1 <- ggplot() +
  geom_text(aes(x,y, label=label, frame = frame), df_txt) +
  geom_point(aes(x,y, tooltip=tooltip, frame = frame), df_pts)
plotly::ggplotly(g_anim1, tooltip = "tooltip")


## Adding aes(`ids`) doesn't seem to help
library(ggplot2)
library(plotly)
df_txt <- data.frame(x=1, y=1, label="No tooltip here, please", frame = 1:2, id = 1)
df_pts <- data.frame(x=rep(c(1, 1), each=2), y=c(.9, .95, .8, .85),
                     tooltip=paste("obs_num: ", 1:2, "tooltip exclusively for points with different dim"),
                     frame = rep(1:2, each=2),
                     id = rep(2:3, 2))
## animation, aes has frame AND ids
g_anim2 <- ggplot() +
  geom_text(aes(x,y, label=label, frame = frame, ids=id), df_txt) +
  geom_point(aes(x,y, tooltip=tooltip, frame = frame, ids=id), df_pts)
plotly::ggplotly(g_anim2, tooltip = "tooltip")


## plotly improve performance?? -----
### _ie:_ try passing plotly obj to toWebGL() and/or partial_bundle()?
if(F){
  browseURL("https://linking.plotly-r.com/performance.html")
  toWebGL()
  partial_bundle()
}

library(spinifex)
library(plotly)
dat <- scale_sd(tourr::flea[, 1:6])
clas <- tourr::flea$species
bas <- basis_pca(dat)
mv <- manip_var_of(bas)
mt_path <- manual_tour(bas, manip_var = mv)

## d = 2 case
ggt <- ggtour(mt_path, dat, angle = .15) +
  proto_basis() +
  proto_point(list(color = clas, shape = clas),
              list(size = 1.5))
## Not run:
system.time(
  print(mbm <- microbenchmark::microbenchmark(
    times = 10L,
    #animate_plotly(ggt),
    redraw_T = ggt %>% ggplotly %>% animation_opts(redraw = TRUE) %>% print,
    redraw_F = ggt %>% ggplotly %>% animation_opts(redraw = FALSE) %>% print,
    redraw_F_webgl = ggt %>% ggplotly %>% animation_opts(redraw = FALSE) %>% toWebGL %>% print,
    redraw_F_bundle = ggt %>% ggplotly %>% animation_opts(redraw = FALSE) %>% partial_bundle %>% print,
    baseline = animate_plotly(ggt) %>% print, ## baseline: 1.892  2.023
    webgl = toWebGL(animate_plotly(ggt)) %>% print,
    bundle = partial_bundle(animate_plotly(ggt)) %>% print,
    bundle_webgl = partial_bundle(toWebGL(animate_plotly(ggt))) %>% print ## best: 1.586  1.459, ~ 16-28% gains
  ))
)[3L]
mbm
ggplot2::autoplot(mbm)
## mbm:
# Unit: milliseconds
# expr            mean median
# redraw_T        1318 1412
# redraw_F        1423 1455
# redraw_F_webgl  1396 1467
# redraw_F_bundle 1395 1466
# baseline 1494   1506 1703
# webgl 1398      1316 1693
# bundle 1450     1472 1747
# bundle_webgl    1662 1683

## takeaways:
#-inconsistent display: some of the non-animate_plotly have more, but still wrong display.
#- baseline arguable best/safest to go with.

## Vary proto ordering and arguments

## variants of protos to explore issue appearance: basis with data dim the root cause?
## baseline bad
ggt.3 <- ggtour(mt_path, dat, angle = .3) +
  proto_basis() +
  proto_point(list(color = clas, shape = clas),
              list(size = 1.5))
animate_plotly(ggt.3)
## change angle
ggt.5 <- ggtour(mt_path, dat, angle = .5) +
  proto_basis() +
  proto_point(list(color = clas, shape = clas),
              list(size = 1.5))
## remove arg_ls 
animate_plotly(ggt.5) ## better in some frames
ggt_w.o_arg_ls <- ggtour(mt_path, dat, angle = .5) +
  proto_basis() +
  proto_point()
animate_plotly(ggt_w.o_arg_ls) ## much better
ggt_pts <- ggtour(mt_path, dat, angle = .5) +
  proto_point() + proto_origin()
animate_plotly(ggt_pts) ## good, but no basis
ggt_bas <- ggtour(mt_path, dat, angle = .5) +
  proto_basis() + proto_origin()
animate_plotly(ggt_bas) ## good, but no pts
ggt_pts_1st <- ggtour(mt_path, dat, angle = .5) +
  proto_point() + proto_basis() + proto_origin()
animate_plotly(ggt_pts_1st) ## WTF, working?
ggt_pts_1st <- ggtour(mt_path, dat, angle = .5) +
  proto_origin() + proto_point() + proto_basis()
animate_plotly(ggt_pts_1st) ## WTF, working?
ggt_def <- ggtour(mt_path, dat, angle = .2) +
  proto_default(list(color = clas, shape = clas),
                list(size = 1.5)) ## frame 13 choked
animate_plotly(ggt_def) ## WTF, working?


ggt <- ggtour(mt_path, dat, angle = .2) +
  proto_point(list(color = clas, shape = clas),
              list(size = 1.5)) +
  proto_basis() +
  proto_origin()
animate_plotly(ggt)

system.time(
  print(mbm <- microbenchmark::microbenchmark(
    times = 25L,
    print(animate_plotly(ggt)), ## baseline: 1.892  2.023
    print(partial_bundle(toWebGL(animate_plotly(ggt)))) ## best: 1.586  1.459, ~ 16-28% gains
  ))
)[3L]
mbm
ggplot2::autoplot(mbm)
