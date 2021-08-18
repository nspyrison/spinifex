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
library("tourr")
library("spinifex")
library("ggplot2")
library("dplyr")

## ----view-basis---------------------------------------------------------------
dat_std <- scale_sd(tourr::flea[,-7])
bas_pca <- basis_pca(dat_std)
clas <- tourr::flea[, 7]

ggtour(basis_array = bas_pca, data = dat_std) +
  proto_default(aes_args = list(color = clas))

## ----view-manip-space---------------------------------------------------------
view_manip_space(basis = bas_pca, manip_var = 3) 

## -----------------------------------------------------------------------------
mt_path <- manual_tour(basis = bas_pca, manip_var = 3)

my_ggtour <- ggtour(basis_array = mt_path, data = dat_std) +
  proto_default(aes_args = list(color = clas, shape = clas))

## ---- eval=FALSE--------------------------------------------------------------
#  animate_plotly(ggtour = my_ggtour)

## ---- echo=F------------------------------------------------------------------
nasa <- select(GGally::nasa, lat, long, day, surftemp)

temp.gly <- GGally::glyphs(nasa, "long", "day", "lat", "surftemp", height = 2.5)
glyph <-
  ggplot(temp.gly, aes(gx, gy, group = gid)) +
  GGally::add_ref_lines(temp.gly, color = "grey90") +
  GGally::add_ref_boxes(temp.gly, color = "grey90") +
  geom_path() + theme_bw() + labs(x = "", y = "")

glyph

## ----Horizontal---------------------------------------------------------------
## Initialize
nasa_std <- cbind(
  GGally::nasa[c("x", "y")],
  scale_sd(GGally::nasa[c("day", "surftemp")])
)
bas <- tourr::basis_init(ncol(nasa_std), 2)

## Horizontal rotation
m_sp_x    <- create_manip_space(basis = bas, manip_var = 3)
rot_mat_x <- rotate_manip_space(manip_space = m_sp_x, 
                                theta = 0, phi = pi / 6)
rot_x     <- data.frame(as.matrix(nasa_std) %*% as.matrix(rot_mat_x))
colnames(rot_x) <- c("x1", "x2", "x_manip_sp")

## ----Vertical-----------------------------------------------------------------
## Vertical rotation
m_sp_y    <- create_manip_space(basis = bas, manip_var = 4)
rot_mat_y <- rotate_manip_space(manip_space = m_sp_y, 
                                theta = pi / 2, phi = pi / 6)
rot_y     <- data.frame(as.matrix(nasa_std) %*% as.matrix(rot_mat_y))
colnames(rot_y) <- c("y1", "y2", "y_manip_sp")

## Combine rotations
rot_xy <- bind_cols(rot_x, rot_y, .name_repair = "unique") %>% 
  select(x = x1, y = y2)

ggplot(rot_xy, aes(x = x, y = y)) + geom_point(size = 0.3) +
  theme_bw() + labs(x = "", y = "")

## -----------------------------------------------------------------------------
dat_std <- scale_sd(tourr::flea[, 1:6])
holes_path <- tourr::save_history(dat_std, tourr::guided_tour(tourr::holes(), ))

ggt <- ggtour(holes_path, dat_std) + proto_default()

## ---- eval=FALSE--------------------------------------------------------------
#  animate_plotly(ggt)

## -----------------------------------------------------------------------------
## We added the basis_* functions, including basis_guided() to help:
holes_bas <- basis_guided(dat_std, index_f = tourr::holes(), d = 2)

ggtour(holes_bas, dat_std) +
  proto_default(aes_args = list(color = clas, shape = clas))

## -----------------------------------------------------------------------------
## Alternatively, ask for the variable by rank of the magnitude in the basis:
(mv <- manip_var_of(holes_bas, rank = 1))
mt_path <- manual_tour(holes_bas, mv)
ggt <- ggtour(mt_path, dat_std) +
  proto_default(aes_args = list(color = clas, shape = clas))

## ---- eval=FALSE--------------------------------------------------------------
#  animate_plotly(ggt)

