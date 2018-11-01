dat <- tibble::as.tibble(mtcars)
dat2 <- tibble::as.tibble(apply(dat, 2, function(x) .8*x))
dat2$cyl <- dat2$cyl / .8
labels <- substr(rownames(dat), 1, 4)

angle <- seq(0, 2 * pi, length = 360)
circ  <- data.frame(x = cos(angle), y = sin(angle))

library(ggplot2)
library(plotly)
(g1 <- ggplot(dat, mapping=aes(x=mpg, y=disp, frame=cyl)) + geom_point())
(g2 <- g1 + geom_point(dat2, colour="blue",
                       mapping=aes(x=mpg, y=disp, frame=cyl)) )
ggplotly(g2)

(g3 <- g2 + geom_text(dat, mapping=aes(x=mpg, y=disp, frame=cyl, label=labels)))
ggplotly(g3)
(g4 <- g3 + geom_path(circ, mapping=aes(x=x, y=y*30), inherit.aes = FALSE) )
ggplotly(g4)
# 
# ggplot2::geom_segment(
#   data = bases_slides, size = .3,
#   ggplot2::aes(x = V1, y = V2, xend = 0, yend = 0, frame = slide)
# )