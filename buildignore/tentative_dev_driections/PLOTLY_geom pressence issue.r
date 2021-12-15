#### Reprex for spinifex issue where plotly has geom existence issues, 
#### in Rstudio viewer, and shiny app (html widget) and especially with 
#### density and basis text toward the limits of the plot.

# Original issue not showing issue now...; checking cheem app.

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
## Seems less likely to be dimenions though to me.
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
library(plotly)
if(F)
  browseURL("https://plotly-r.com/animating-views.html")
## ggpmisc won't work with plotly, or frame; 
## Go to manual creation with geom_text probably. see proto_frame_cor and basis1d

## base gap minder
data(gapminder, package = "gapminder")
gg <- ggplot(gapminder, aes(gdpPercap, lifeExp, color = continent)) +
  geom_point(aes(size = pop, frame = year, ids = country), shape = 3) +
  scale_x_log10()
ggplotly(gg)

## as 1d density
gg <- ggplot(gapminder, aes(x=gdpPercap, y=..ndensity.., color = continent)) +
  geom_density(aes(frame = year, fill = continent)) +
  scale_x_log10() + theme_minimal() + theme(legend.position = "off")
## theme, limits, and plotly::layout(h/w) don't impair density alone
ggplotly(gg) %>% layout(height = 400, width = 400)

## as 1d density
df <- gapminder %>%
  dplyr::filter(continent %in% c("Asia", "Americas")) %>% 
  spinifex::scale_01
dummy_bas <- spinifex::basis_pca(dat[,3:6])
gg <- ggplot(dat, aes(x=gdpPercap, y=..ndensity.., color = continent)) +
  geom_density(aes(frame = year, fill = continent)) +
  #scale_x_log10() +
  spinifex::draw_basis(
    dummy_bas, map_to = data.frame(range(dat[, "gdpPercap"]+40000), c(0,1))
  )
## theme, limits, and plotly::layout(h/w) don't impair density alone
ggplotly(gg)

## with animation options
ggplotly(gg + theme_void()) %>% 
  plotly::animation_opts(frame = 1L / 4 * 1000L,
                         transition = 0L, redraw = TRUE) %>%
  plotly::animation_slider(
    active = 0L, ## 0 indexed first frame
    currentvalue = list(prefix = "Frame: ", font = list(color = "black"))) %>%
  ## Remove button bar and zoom box
  plotly::config(displayModeBar = FALSE,
                 modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d")) %>%
  ## Remove legends and axis lines
  plotly::layout(dragmode = FALSE, legend = list(x = 100L, y = 0.5),
                 #fixedrange = TRUE, ## This is a curse, do not use.
                 showlegend = FALSE,
                 yaxis = list(showgrid = FALSE, showline = FALSE),
                 xaxis = list(showgrid = FALSE, showline = FALSE,
                              scaleanchor = "y", scalaratio = 1L))

## basis
bas_df <- data.frame(x = , )

## adding a 1d basis
gg2 <- 
list(

  ## Variable abbreviation text
  ggplot2::geom_text(
    ggplot2::aes(x, y, label = label), .df_txt,
    size = text_size, color = "grey60", hjust = 1L),
  ## Contribution segments of current basis, changing with frame
  suppressWarnings(ggplot2::geom_segment(
    ggplot2::aes(x = .df_zero$x, y, xend = x, yend = y, frame = frame),
    .df_seg, color = .axes_col, size = .axes_siz))
)





#############
# Pkg load order? -- doesn't matter -----

library(spinifex)
#library(dplyr)
#library(ggplot2)
#library(plotly)

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
