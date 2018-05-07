### Testing and scratchpad:
library(devtools)
library(roxygen2)
devtools::load_all()
#library(ggplot2)

### load
devtools::install_github("nspyrison/spinifex")
library(spinifex)
?data_proj #test documentation

### init
data <- flea[, 1:6]
p <- ncol(data)
b_rand <- basis_random(p = p)

dp <-
  data_proj(
    data = data,
    basis = b_rand,
    manip_var = 3,
    manip = "radial",
    from = 0,
    to = pi
  )
head(dp)
slideshow(dp, col = flea[, 7])


stop()
stop()

#theta # angular rotation: atan(y_dist/x_dist)
#phi   # angular rotation: length of mouse region/size of plot region

##### Vignette example 1: flea
data <- flea[, 1:6]
p <- ncol(data)
b_rand <- basis_random(p = p)

dp <-
  data_proj(
    data = data,
    basis = b_rand,
    manip_var = 3,
    manip = "radial",
    from = 0,
    to = pi
  )
slideshow(dp)


##### Vignette example 2: nasa
nasa <- nasa[nasa$date >= as.POSIXct("1998-01-01") &
               nasa$lat >= 20 &
               nasa$lat <= 40 &
               nasa$long >= -80 &
               nasa$long <= -60
             , ]
str(nasa) #1:6 demographic, 7:13 var, 14:17 demographic,
#dim(table(nasa$day)) #grain: month. 36 month-apart days, (3 years)
head(nasa[, 7:13]) #12 is surftemp

#?GGally::glyphs #see example
#library(GGally)
temp.gly <-
  GGally::glyphs(nasa, "long", "day", "lat", "surftemp", height = 2.5)

g1 <-
  ggplot2::ggplot(temp.gly, ggplot2::aes(gx, gy, group = gid)) +
  GGally::add_ref_lines(temp.gly, color = "grey90") +
  GGally::add_ref_boxes(temp.gly, color = "grey90") +
  ggplot2::geom_path() +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "", y = "")

##Do a horizontal rotation on time (day), changing it's a_2.
tmp <- nasa[, c(15, 7:11)] #exclude surftemp, temp
p <- ncol(tmp)
b_iden <- basis_identity(p = p)
dp_day <-
  data_proj(
    data = tmp,
    basis = b_iden,
    manip_var = "day",
    manip = "horizontal",
    from = 0,
    to = 1.4
  )
#slideshow(dp_day)

##Do a vertical rotation on temp, changing b_2.
tmp <- nasa[, c(12, 7:11)] #exclude day, temp
p <- ncol(tmp)
b_iden <- basis_identity(p = p)
dp_surftemp <-
  data_proj(
    data = tmp,
    basis = b_iden,
    manip_var = "day",
    manip = "vertical",
    from = 0,
    to = 1.88
  )
#slideshow(dp_surftemp)

library(dplyr)
adj_day <-
  filter(as.data.frame(dp_day), index == max(index))[, 1] #x
adj_surftemp <-
  filter(as.data.frame(dp_surftemp), index == max(index))[, 2] #y
nasa.gly <- cbind(
  select(nasa, c("long", "day", "lat", "surftemp")),
  adj_day = filter(as.data.frame(dp_day), index == max(index))[, 1], #x
  adj_surftemp = filter(as.data.frame(dp_surftemp), index == max(index))[, 2] #y
)
head(nasa.gly)

temp.gly <-
  GGally::glyphs(nasa, "long", "day", "lat", "surftemp", height = 2.5)
g1 <-
  ggplot2::ggplot(temp.gly, ggplot2::aes(gx, gy, group = gid)) +
  GGally::add_ref_lines(temp.gly, color = "grey90") +
  GGally::add_ref_boxes(temp.gly, color = "grey90") +
  ggplot2::geom_path() +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "", y = "")

temp.gly <-
  GGally::glyphs(nasa, "long", "adj_day", "lat", "adj_surftemp", height = 2.5)
g2 <-
  ggplot2::ggplot(temp.gly, ggplot2::aes(gx, gy, group = gid)) +
  GGally::add_ref_lines(temp.gly, color = "grey90") +
  GGally::add_ref_boxes(temp.gly, color = "grey90") +
  ggplot2::geom_path() +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "", y = "")

gridExtra::grid.arrange(g1, g2, ncol=2)



cat(
  "how do we map back to one dim to use? we have p=6 being rotated by our variable of interest.
  We have a [index*n, p] or [n, p]@index. "
)

##### Vignette example 3: guided tour on flea
#library(tourr)
data <- flea[, 1:6]
p <- ncol(data)
##holes is unsupervised.
holes <-
  tourr::save_history(data, tourr::guided_tour(index = holes), max_bases = 25)
holes_basis <-
  matrix(as.numeric(holes[, , dim(holes)[3]]), ncol = 2)
##lda is supervised.
#lda <- tourr::save_history(data, tourr::guided_tour(
#  index = tourr::lda_pp(flea$species)),max_bases = 25)
#lda_basis <- matrix(as.numeric(lda[,,dim(lda)[3]]),ncol=2)

###can't solve for Phi, even after seting the manip space:
#tmp2 <- create_manip_space(holes_basis,manip_var=3)
#tmp2 <- as.data.frame(cbind(tmp2, sqrt(tmp2[,1]^2+tmp2[,2]^2), atan(tmp2[,1]/tmp2[,2]) #))
#colnames(tmp2) <- c("x","y","z","h","theta")
#tmp2
#atan(tmp2$h/tmp2$z)

head(data)
GGally::ggpairs(data)
#basis_help(holes_basis, data=data)
### phi found manually. theta set via basis
tars1 <-
  data_proj(
    data = data,
    basis = holes_basis,
    manip_var = "tars1",
    manip = "radial",
    from = 0,
    to = 1.57
  ) #, theta = -0.23
tars2 <-
  data_proj(
    data = data,
    basis = holes_basis,
    manip_var = "tars2",
    manip = "radial",
    from = 0,
    to = 1.41,
    theta = 1.32
  )
head <-
  data_proj(
    data = data,
    basis = holes_basis,
    manip_var = "head",
    manip = "radial",
    from = 0,
    to = 1.26,
    theta = 1.33
  )
aede1 <-
  data_proj(
    data = data,
    basis = holes_basis,
    manip_var = "aede1",
    manip = "radial",
    from = 0,
    to = 1.41,
    theta = 1.29
  )
aede2 <-
  data_proj(
    data = data,
    basis = holes_basis,
    manip_var = "aede2",
    manip = "radial",
    from = 0,
    to = 1.41,
    theta = -0.203
  )
aede3 <-
  data_proj(
    data = data,
    basis = holes_basis,
    manip_var = "aede3",
    manip = "radial",
    from = 0,
    to = 1.73,
    theta = 1.43
  )
slideshow(tars1, col = flea[, 7])
slideshow(tars2, col = flea[, 7])
slideshow(head, col = flea[, 7])
slideshow(aede1, col = flea[, 7])
slideshow(aede2, col = flea[, 7])
slideshow(aede3, col = flea[, 7])
