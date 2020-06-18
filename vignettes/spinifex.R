## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  echo       = TRUE,   # code
  include    = TRUE,   # plots
  results    = "hide", # text: "hide", "show"
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

## ----pkgs---------------------------------------------------------------------
library("spinifex")
library("tourr")
library("ggplot2")
library("dplyr")

## ----view-basis---------------------------------------------------------------
flea_std <- tourr::rescale(tourr::flea[,-7])
str(flea_std)

flea_rb <- tourr::basis_random(n = ncol(flea_std), d = 2)
view_basis(basis = flea_rb, lab = colnames(flea_std))

## ----view-manip-space---------------------------------------------------------
view_manip_space(basis = flea_rb, manip_var = 3, lab = colnames(flea_std))

## ---- eval=F------------------------------------------------------------------
#  play_manual_tour(data = flea_std, basis = flea_rb, manip_var = 3)

## ---- echo=F------------------------------------------------------------------
nasa <- select(GGally::nasa, lat, long, day, surftemp)

temp.gly <-
  GGally::glyphs(nasa, "long", "day", "lat", "surftemp", height = 2.5)
glyph <-
  ggplot(temp.gly, aes(gx, gy, group = gid)) +
  GGally::add_ref_lines(temp.gly, color = "grey90") +
  GGally::add_ref_boxes(temp.gly, color = "grey90") +
  geom_path() + theme_bw() + labs(x = "", y = "")

glyph

## ----Horizontal---------------------------------------------------------------
# Initialize
nasa_std <- 
  cbind(GGally::nasa[c("x", "y")], tourr::rescale(GGally::nasa[c("day", "surftemp")]))
bas <- tourr::basis_init(ncol(nasa_std), 2)

# Horizontal roation
m_sp_x    <- create_manip_space(basis = bas, manip_var = 3)
rot_mat_x <- rotate_manip_space(manip_space = m_sp_x, 
                                theta = 0, phi = pi/6)
rot_x     <- data.frame(as.matrix(nasa_std) %*% as.matrix(rot_mat_x))

## ----Vertical-----------------------------------------------------------------
# Vertical roation
m_sp_y    <- create_manip_space(basis = bas, manip_var = 4)
rot_mat_y <- rotate_manip_space(manip_space = m_sp_y, 
                                theta = pi/2, phi = pi/6)
rot_y     <- data.frame(as.matrix(nasa_std) %*% as.matrix(rot_mat_y))

# Combine rotations
rot_xy <- bind_cols(rot_x, rot_y) %>% 
  select(x = proj_3...3, y = proj_3...6)

ggplot(rot_xy, aes(x = x, y = y)) + geom_point(size = 0.3) +
  theme_bw() + labs(x = "", y = "")

## -----------------------------------------------------------------------------
flea_std <- tourr::rescale(tourr::flea[,1:6])
fpath    <- tourr::save_history(flea_std, tourr::guided_tour(tourr::holes()))

## ---- eval=F------------------------------------------------------------------
#  play_tour_path(tour_path = fpath, data = flea_std, angle = .15,
#    render_type = render_gganimate, col = tourr::flea$species, fps = 4)

## -----------------------------------------------------------------------------
f_holes_bas  <- matrix(as.numeric(fpath[,, dim(fpath)[3]]), ncol = 2)
view_basis(f_holes_bas, lab = colnames(flea_std))

## ---- eval=F------------------------------------------------------------------
#  play_manual_tour(data = flea_std, basis = f_holes_bas,
#                   manip_var = 5, col = tourr::flea$species)

