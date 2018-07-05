## plotly
#https://plot.ly/r/animations/
library(plotly)
library(ggplot2)
packageVersion('plotly')

df1 <- data.frame(
  x = c(1,2,3,4), 
  y = c(1,2,3,4), 
  f = c(1,2)
)
df2 <- data.frame(
  x = c(2,3), 
  y = c(2,3), 
  f = c(1,2)
)

g1 <- ggplot(df1, aes(x=x, y=y)) + geom_point(aes(frame=f))
g2 <- ggplot(df2, aes(x=x, y=y)) + geom_point(aes(frame=f))
#gridExtra::grid.arrange(g, g2, ncol = 2)

gly1 <- ggplotly(g1)
gly2 <- ggplotly(g2)
subplot(gly1, gly2, nrows = 1, widths = c(0.3, 0.7))

###color
plot_ly(flea[,1:6], x = ~flea[,1], y = ~flea[,2], type="scatter", 
        mode = "markers" , color = ~flea$species , 
        marker=list( size=20 , opacity=0.5) )

plot_ly(flea[,1:6], x = ~flea[,1], y = ~flea[,2], type="scatter", 
        mode = "markers" , color = I("green") ,  
        marker=list( size=20 , opacity=0.5) )

###GAPMINDER NOT WORKING
library(ggplot2)
library(plotly)
library(gapminder)

p <- gapminder %>%
  plot_ly(
    x = ~gdpPercap, 
    y = ~lifeExp, 
    line(x=1000, y=50),
    size = ~pop, 
    color = ~continent, 
    frame = ~year, 
    text = ~country, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )
  )
p


###### GGANIMATE

# NOTE: gganimate is heavy: 
# devtools::install_github("dgrtwo/gganimate")
# install.packages("installr")
# installr::install.ImageMagick() ###
## too many installation issues: https://github.com/dgrtwo/gganimate/issues/54
??gganimae
library(gapminder)
library(ggplot2)
theme_set(theme_bw())

p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent, frame = year)) +
  geom_point() +
  scale_x_log10()
p

library(gganimate)
gganimate(p)
gganimate(p, "output.gif")

p2 <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop)) +
  geom_point() +
  geom_point(aes(frame = year), color = "red") +
  scale_x_log10()

gganimate(p2)

