#### Reprex for spinifex issue where plotly has geom existence issues, 
#### in Rstudio viewer, and shiny app (html widget) and especially with 
#### density and basis text toward the limits of the plot.

## Things to try: 
## - adj ggplot2 limits
## - adj plotly dimensions
## - adj shiny/plotly html container dim

library(spinifex)
library(dplyr)
library(ggplot2)
library(plotly)

# Original case setup ----
## direct from ?ggtour, 1d case working fine, but 2d from basis is wrong. 
## Seems less likely to be dimesnions though to me.

dat     <- scale_sd(penguins[, 1:4])
clas    <- penguins$species
bas     <- basis_pca(dat)
mv      <- manip_var_of(bas)
mt_path <- manual_tour(bas, manip_var = mv)

## d = 2 case
ggt <- ggtour(basis_array = mt_path, data = dat, angle = .3) +
  proto_default()#aes_args = list(color = clas, shape = clas),
                 #identity_args = list(size = 1.5, alpha = .8))
animate_plotly(ggt)

## Does proto order effect? Not in this case -----
if(F){
  ggt <- ggtour(basis_array = mt_path, data = dat, angle = .3) +
    proto_basis() + proto_point()
  animate_plotly(ggt)
  
  ggt <- ggtour(basis_array = mt_path, data = dat, angle = .3) +
    proto_point() + proto_basis()
  animate_plotly(ggt)
}
## Not in this case

## Does ggplot expanding limits effect? NO -----
?ggplot2::expand_limits
lim <- 8
ggt2 <- ggt + ggplot2::expand_limits(x = c(-lim, lim), y = c(0, lim))
animate_plotly(ggt2)
animate_plotly(ggt)
## Doesn't seem to help

## Do they work in isolation? Yes -----
ggt_b <- ggtour(basis_array = mt_path, data = dat, angle = .3) +
  proto_basis()
ggt_d <-   ggt <- ggtour(basis_array = mt_path, data = dat, angle = .3) +
  proto_point()
animate_plotly(ggt_b)
animate_plotly(ggt_d)

## Point static ggplot2 have the geoms; a plotly issue -----
ggt2

## Plotly::layout changes the widget within the viewer pane/container -----
animate_plotly(ggt2) %>% layout(width = 400, height = 400)

# direct plolty reprex would be insightful -----
library(plotly)
?plotly::ggplotly
library(plotly)
if(F)
  browseURL("https://plotly-r.com/animating-views.html")

## base gap minder
data(gapminder, package = "gapminder")
gg <- ggplot(gapminder, aes(gdpPercap, lifeExp, color = continent)) +
  geom_point(aes(size = pop, frame = year, ids = country), shape = 3) +
  scale_x_log10()
ggplotly(gg)

## as 1d density
gg <- ggplot(gapminder, aes(x=gdpPercap, color = continent)) +
  geom_density(aes(frame = year, fill = continent),stat = ....) +
  scale_x_log10()
ggplotly(gg)
