library("ggplot2")
x <- flea
x
col <- as.factor(x$species)
col <- "blue"

old<-  theme_get()


theme_set(theme_void()) #capture current theme

myPlot <- function(...){
  ggplot(tourr::flea, aes(x=tars1, y=tars2)) +
    geom_point(size = 5, 
               mapping = aes(
                 ...
               )
    ) + 
    theme_bw() +
    theme_dark() +
    geom_abline(slope = .5, color="green", size= 4)
}
myPlot(color = c("blue", "red", "green"))


g + scale_color_brewer(palette = "Set1") + scale_color_identity()


## Renders as classic, good 
 
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
(d <- ggplot(dsamp, aes(carat, price)) +
    geom_point(aes(colour = clarity)))
d + scale_colour_brewer(palette = "Set1")

d +old
d
g
