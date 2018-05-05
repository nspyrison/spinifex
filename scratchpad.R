##Testing and scratchpad:
#library(devtools)
#library(roxygen2)
#library(ggplot2)
devtools::load_all()

### load
devtools::install_github("nspyrison/spinifex")
library(spinifex)
?create_manip_space #test documentation

### init
data <- flea[, 1:6]
p <- ncol(data)
b_rand <- basis_random(p = p)

### data_proj
data_proj(data=data[1:3, 1:6], manip_var=1,  manip="radial", to=pi/10)
dp <- data_proj(data=data, basis=b_rand, manip_var=3, manip="radial",
          from=0, to=pi, by=pi/10, theta=pi/4)
head(dp)
slideshow(dp, plotly=T)


### manipulations
horizontal_manip(manip_space = m_sp, phi = pi/3, theta = pi/4)
#passing theta doesn't influence horizontal/vertical_manip
vertical_manip(manip_space = m_sp, phi = pi/3)
radial_manip(manip_space = m_sp, phi = pi/3, theta = pi/4)


stop()
stop()

#theta # angular rotation: atan(y_dist/x_dist)
#phi   # angular rotation: length of mouse region/size of plot region

##### Vignette example 1: flea
data <- flea[, 1:6]
p <- ncol(data)
b_rand <- basis_random(p = p)

dp <- data_proj(data=data, basis=b_rand, manip_var=3, manip="radial",
                from=0, to=pi, by=pi/10, theta = pi/4)
slideshow(dp, ggplot=T)
slideshow(dp, ggplot=F, from=2, to=8, by=2, delay=2)

##### Vignette example 2: nasa
nasa <- nasa[
  nasa$date >= as.POSIXct("1998-01-01") &
  nasa$lat >= 20 &
  nasa$lat <= 40 &
  nasa$long >= -80 &
  nasa$long <= -60
  , ]
str(nasa) #1:6 demographic, 7:13 var, 14:17 demographic,
dim(table(nasa$day)) #grain: month. 72 months, (6 years)
head(nasa[,7:13]) #12 is surftemp

#?GGally::glyphs #see example
temp.gly <- glyphs(nasa, "long", "day", "lat", "surftemp", height=2.5)
str(temp.gly)
ggplot2::ggplot(temp.gly, ggplot2::aes(gx, gy, group = gid)) +
  add_ref_lines(temp.gly, color = "grey90") +
  add_ref_boxes(temp.gly, color = "grey90") +
  ggplot2::geom_path() +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "", y = "")

data <- nasa[,7:13]
p <- ncol(nasa)

##### Vignette example 3: guided tour on flea
library(tourr)
data <- flea[, 1:6]
p <- ncol(data)
#holes is unsupervised.
holes <- tourr::save_history(data, tourr::guided_tour(index = holes), max_bases = 25)
holes_basis <- matrix(as.numeric(holes[,,dim(holes)[3]]),ncol=2)
#lda is supervised.
lda <- tourr::save_history(data, tourr::guided_tour(
  index = tourr::lda_pp(flea$species)),max_bases = 25)
lda_basis <- matrix(as.numeric(lda[,,dim(lda)[3]]),ncol=2)
#holes_basis, lda_basis


str(data)
GGally::ggpairs(data)
#head, aede1, tars1?
#aede1 shows promise theta=90, phi close to 90
head <- data_proj(data=data, basis=holes_basis, manip_var="head", manip="radial",
                   from=0, to=pi, by=pi/10, theta = 0)
aede2 <- data_proj(data=data, basis=holes_basis, manip_var="aede1", manip="radial",
                   from=0, to=pi, by=pi/10, theta = 0)
tars2 <- data_proj(data=data, basis=holes_basis, manip_var="tars1", manip="radial",
                from=0, to=pi, by=pi/10, theta = 0)
slideshow(head)
slideshow(aede2)
slideshow(tars2)

off <- data_proj(data=data, basis=holes_basis, manip_var="aede3", manip="radial",
                   from=0, to=pi, by=pi/10, theta = pi/5)
slideshow(off)
#aede2 and head do well in distinguishing species!
#aede1, aede3, and tars1 do ok in distinguishing species.
#tars2 does a poor job in distinguishing species.

