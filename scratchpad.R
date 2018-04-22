#Testing and scratchpad:
library(devtools)
library(roxygen2)
library(ggplot2)
devtools::load_all()

### load
devtools::install_github("nspyrison/spinifex")
library(spinifex)
?create_manip_space #test documentation

### init
data <- flea[, 1:6]
p <- ncol(data)
basis <- basis_random(p = p)
m_sp <- create_manip_space(basis = b, manip_var = 3)

### data_proj
data_proj(data=flea[1:3, 1:6], manip="radial", to=2)
dp <- data_proj(data=data,  manip_var=3, manip="radial",
          from=0, to=pi, by=pi/10,
          manip_space = m_sp, theta = pi/4)
slideshow(dp, ggplot=T)
slideshow(dp, ggplot=F)

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
basis <- basis_random(p = p)
m_sp <- create_manip_space(basis = b, manip_var = 3)

data_proj(data=flea[1:3, 1:6], manip="radial", to=2)
dp <- data_proj(data=data,  manip_var=3, manip="radial",
                from=0, to=pi, by=pi/10,
                manip_space = m_sp, theta = pi/4)
slideshow(dp, ggplot=T)
slideshow(dp, ggplot=F)

##### Vignette example 2: nasa

data <- nasa[
  nasa$date >= as.POSIXct("1998-01-01") &
  nasa$lat >= 20 &
  nasa$lat <= 40 &
  nasa$long >= -80 &
  nasa$long <= -60
  , ]
p <- ncol(data)
str(nasa) #1:6 demographic, 7:13 var, 14:17 demographic,
head(nasa[,7:13]) #12 is surftemp

#?GGally::glyphs #see example
temp.gly <- glyphs(data, "long", "day", "lat", "surftemp", height=2.5)
str(temp.gly)
ggplot2::ggplot(temp.gly, ggplot2::aes(gx, gy, group = gid)) +
  add_ref_lines(temp.gly, color = "grey90") +
  add_ref_boxes(temp.gly, color = "grey90") +
  ggplot2::geom_path() +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "", y = "")


