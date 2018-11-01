library(ggplot2)
library(plotly)
library(dplyr)

df <- as.data.frame(mtcars)
df$labels <- substr(rownames(df), 1, 4)

angles <- seq(0, 2 * pi, length = 6)
circ <- data.frame(x = cos(angles), y = sin(angles), rn = row_number(angles))

cross <- merge(df, circ, all = TRUE) # cross/cartesian join
cross$disp <- cross$disp * .03

gg <- ggplot() + 
  geom_path(circ, mapping = aes(x=x, y=y), inherit.aes = F) +
  geom_point(cross, mapping = aes(x=mpg+x, y=disp+y, frame=rn)) +
  geom_text(cross, mapping = aes(x=mpg+x, y=disp+y+1, frame=rn, label=labels))

(ggp <- ggplotly(gg))
View(ggp[[1]]$data) #[[1]] for path, [[2:7]] for point, [[8:13]] for text.
