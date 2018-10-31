library(spinifex)
library(tidyverse)
library(ggthemes)
library(GGally)
library(plotly)

# This code does a spatio-temporal example, making small time series at 
# a grid of spatial locations
scale01 <- function(x) {
  x <- (x-min(x))/(max(x)-min(x))
}

data(nasa)
nasa_sub <- nasa %>% select(x, y, day, surftemp) %>%
  mutate(surftemp = scale01(surftemp)) %>%
  mutate(day = scale01(day))
ggplot(nasa_sub, aes(x=x, y=y)) + geom_point()
ggplot(nasa_sub, aes(x=x+day/2, y=y+surftemp/2)) + geom_point(size=0.3)

basis = create_identity_basis(p = ncol(nasa_sub))

# Horizontal
manip_space = create_manip_space(basis = basis, manip_var = 3)
rot_mat <- rotate_manip_space(manip_space, 0, pi/6)
nasa_sub_rot_x <- data.frame(as.matrix(nasa_sub) %*% as.matrix(rot_mat))

# Vertical
manip_space = create_manip_space(basis = basis, manip_var = 4)
rot_mat <- rotate_manip_space(manip_space, pi/2, pi/6)
nasa_sub_rot_y <- data.frame(as.matrix(nasa_sub) %*% as.matrix(rot_mat))
nasa_sub_rot <- bind_cols(nasa_sub_rot_x, nasa_sub_rot_y) %>%
  select(x, y1) %>%
  rename(x=x, y=y1)

ggplot(nasa_sub_rot, aes(x=x, y=y)) + geom_point(size=0.3)

# Creating an animation from a manual tour
# I think that it would be better to split up the proj_data into making
# one projected data, and then a separate function to string a bunch of
# projections together
data(flea)
flea_std <- apply(flea[,2:7], 2, function(x) ((x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)))

basis <- create_random_basis(p = ncol(flea_std))
flea_std_proj1 <- data.frame(as.matrix(flea_std) %*% basis)
flea_std_proj1$species <- flea$species
ggplot(flea_std_proj1, aes(x=X1, y=X2, colour=species)) + geom_point() + theme(aspect.ratio=1)

flea_std_proj <-
  proj_data(
    data = flea_std,
    basis = basis,
    manip_var = 4, 
    manip_type = "rad",
    center = FALSE,
    scale = FALSE,
    phi_from = 0,
    phi_to = pi,
    n_slides = 60
  )
str(flea_std_proj)
flea_std_proj$proj_basis[flea_std_proj$proj_basis$index == 1,]
flea_proj_step1 <- flea_std_proj$proj_data[flea_std_proj$proj_data$index==1,]
flea_proj_step1$species <- flea$species
flea_basis_step1 <- flea_std_proj$proj_basis[flea_std_proj$proj_basis$index==1,]
flea_basis_step1$xstart <- 0
flea_basis_step1$ystart <- 0
flea_basis_step1$color <- "black"
flea_basis_step1$color[flea_basis_step1$manip_var] <- "purple"
circle <- data.frame(x=c(seq(-1, 1, 0.01), seq(1, -1, -0.01)), y=c(sqrt(1-seq(-1, 1, 0.01)^2), -sqrt(1-seq(1, -1, -0.01)^2)))
ggplot(flea_proj_step1, aes(x=x, y=y, colour=species)) + geom_point() + 
  geom_segment(data=flea_basis_step1, aes(x=xstart, y=ystart, xend=x, yend=y), colour=I(flea_basis_step1$color)) +
  geom_text(data=flea_basis_step1, aes(x=x, y=y, label=var_name), colour=I(flea_basis_step1$color)) +
  geom_path(data=circle, aes(x=x, y=y), colour="black") +
  #theme_solid() + 
  xlim(c(-3,3)) + ylim(c(-3,3)) +
  theme(aspect.ratio=1, legend.position="none")

fl <- "inst/doc/img/plot"
for (i in 1:max(flea_std_proj$proj_basis$index)) {
  cat(i, "\n")
  fln <- paste0(fl, i, ".png")
  flea_proj_step1 <- flea_std_proj$proj_data[flea_std_proj$proj_data$index==i,]
  flea_proj_step1$species <- flea$species
  flea_basis_step1 <- flea_std_proj$proj_basis[flea_std_proj$proj_basis$index==i,]
  flea_basis_step1$xstart <- 0
  flea_basis_step1$ystart <- 0
  flea_basis_step1$color <- "black"
  flea_basis_step1$color[flea_basis_step1$manip_var] <- "purple"
  p <- ggplot(flea_proj_step1, aes(x=x, y=y, colour=species)) + geom_point() + 
    geom_segment(data=flea_basis_step1, aes(x=xstart, y=ystart, xend=x, yend=y), colour=I(flea_basis_step1$color)) +
    geom_text(data=flea_basis_step1, aes(x=x, y=y, label=var_name), colour=I(flea_basis_step1$color)) +
    geom_path(data=circle, aes(x=x, y=y), colour="black") +
    theme_solid() + xlim(c(-3,3)) + ylim(c(-3,3)) +
    theme(aspect.ratio=1, legend.position="none")
  ggsave(fln, p, "png")
}

# To make animation
proj_data <- flea_std_proj$proj_data
gg1 <- ggplot(data = proj_data, 
              ggplot2::aes(x = x, y = y)) +
  suppressWarnings( # suppress to ignore unused aes "frame"
    ggplot2::geom_point(size = .7,
                        ggplot2::aes(frame = index) 
    )
  ) + 
  ggplot2::ylab("") + ggplot2::xlab("") + ggplot2::coord_fixed() 
ggplotly(gg1)

# Testing re-structured code
#devtools::install()
#devtools::document()
library(spinifex)
data(flea)
flea_std <- 
  apply(flea[,1:6], 2, function(x) 
    ((x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)))
data <- flea_std
p <- ncol(data) 
r_basis <- create_random_basis(p = p)
pch <- flea$species
col <- flea$species

proj <-
  proj_data(
    data = data,
    basis = r_basis,
    manip_var = 4,
    manip_type = "radial",
    phi_from = 0,
    phi_to = pi,
    n_slides = 20
  )