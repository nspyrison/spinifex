## plotly
#https://plot.ly/r/animations/
library(plotly)
library(ggplot2)
packageVersion('plotly')

# Sandbox ggplot2 behaviours
### theme_void() doesn't remove legend.
### frame should be in aes(). it will not message in ggplot(), but
###   it will message if frame is within geom_point()
dat <- as_tibble(mtcars)

g1 <-
  ggplot(dat, 
         aes(x = disp, y = hp, col = as.factor(cyl), 
             blah1="hi", blah2 = am, drats = drat, frame = vs) ) +
  # suppressWarnings(geom_point(aes(blahblah = vs)) ) + 
  geom_point(aes(blahblah = vs)) + 
  theme_void()

ggplotly(g1)

# Dummy plotly animation.
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

### Removing removing grids, cleaning axes. similar to theme_void().

#slideshow <-
  layout(
	pgg4, showlegend = F, yaxis = list(showgrid = F, showline = F),
	xaxis = list(scaleanchor = "y", scaleratio = 1, showgrid = F, showline =F)
  #)

### Color
plot_ly(flea[,1:6], x = ~flea[,1], y = ~flea[,2], type="scatter", 
        mode = "markers" , color = ~flea$species , 
        marker=list( size=20 , opacity=0.5) )

plot_ly(flea[,1:6], x = ~flea[,1], y = ~flea[,2], type="scatter", 
        mode = "markers" , color = I("green") ,  
        marker=list( size=20 , opacity=0.5) )

### Exporting with plotly::export()
library("ggplot2")
library("plotly")
#p <- plot_ly(z = ~volcano) %>% add_surface()

?export
#export(plot_ly(economics, x = ~date, y = ~pce), file="plotly_out_test.png")

df1 <- data.frame(
  x = c(1,2,3,4), 
  y = c(1,2,3,4), 
  f = c(1,2)
)
g1 <- ggplot(df1, aes(x=x, y=y)) + geom_point(aes(frame=f))
gly1 <- ggplotly(g1)

export(gly1, file="plotly_out_test1.svg") 
  # .png, .pdf static, .gif not supported. ".html" not supported
export(gly1) #gpes to .png
?export
print("basically, use .rmd to go to html.")

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