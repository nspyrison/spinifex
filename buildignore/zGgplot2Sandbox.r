library("ggplot2")
x <- flea
x
col <- as.factor(x$species)
col <- "blue"

old<-  theme_get()
theme_set(theme_minimal()) #capture current theme
z <- ggplot(tourr::flea, aes(x=tars1, y=tars2, color = col)) +
  geom_point(size = 5) + 
  geom_abline(slope = .5, color="green", size= 4) +
  theme(legend.position = "none")
theme_set(old)
z



g + scale_color_brewer(palette = "Set1") + scale_color_identity()


## Renders as classic, good 
 
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
(d <- ggplot(dsamp, aes(carat, price)) +
    geom_point(aes(colour = clarity)))
d + scale_colour_brewer(palette = "Set1")

d +old
d
g
