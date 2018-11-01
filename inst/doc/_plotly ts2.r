dat <- tibble::as.tibble(mtcars)
#dat2 <- tibble::as.tibble(apply(dat, 2, function(x) .8*x))
#dat2$cyl <- dat2$cyl / .8
labels <- substr(rownames(dat), 1, 4)

angle <- seq(0, 2 * pi, length = 360)
circ  <- data.frame(x = cos(angle), y = sin(angle))

library(ggplot2)
library(plotly)
(g1 <- ggplot() + geom_path(circ, inherit.aes = FALSE,
                            mapping=aes(x=x, y=y*30) ) )
ggplotly(g1)

(g2 <- g1 + 
    geom_text(dat, mapping=aes(x=mpg, y=disp, frame=cyl, label=labels)) +
    geom_segment(dat, size = .3, 
                 mapping=aes(x=mpg*.8, y=disp*.8, xend=0, yend=0, frame=cyl)) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") #+
    #ggplot2::coord_fixed(ratio = 1)
)
ggplotly(g2)

(g3 <- g2 + geom_point(dat, colour="blue",
                       mapping=aes(x=mpg, y=disp, frame=cyl)) )
ggplotly(g3)
