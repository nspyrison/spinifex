## plotly
#https://plot.ly/r/animations/
library(plotly)
packageVersion('plotly')

df <- data.frame(
  x = c(1,2), 
  y = c(1,2), 
  f = c(1,2)
)
p1 <- df %>%
  plotly::plot_ly(
    x = ~x,
    y = ~y,
    frame = ~f,
    #text = ~paste0(x,y),
    hoverinfo = "other",
    type = 'scatter',
    mode = 'markers',
    showlegend = F
  )
p1
  
q = c(4,5,6,7)
df <- data.frame(
  x = c(1,0,3,2),
  y = c(1,2,3,4),
  f = c(1,1,2,2)
)
p2 <- df %>%
  plotly::plot_ly(
    x = ~x,
    y = ~y,
    frame = ~f,
    #line(x=x,y=y),
    #text = ~paste0(x,y),
    hoverinfo = "other",
    type = 'scatter',
    mode = 'markers',
    showlegend = F
  )
p2 %>% 
  add_text(x=1,y=2,text = "some text") %>% 
  #add_lines(x= c(0,1,2,3), y= c(0,2,4,6), color=I("red")) #%>% 
  #add_lines(x= c(0,2), y= c(0,1), color=I("gray80"))
  add_segments(x= c(0), y= c(0), xend=q/4, yend=q/4, color=I("gray80"), text="seg")
animation_opts()
?animation_opts
subplot(p1, p2, nrows = 1, widths = c(0.3, 0.7)) %>%
  hide_legend() %>%
  animation_opts(1000, redraw = FALSE) %>%
  layout(hovermode = "y", margin = list(l = 100)) %>%
  highlight("plotly_selected", color = "blue", opacityDim = 1, hoverinfo = "none")
ggplotly(p1)

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



N
?parse

###color
plot_ly(flea[,1:6], x = ~flea[,1], y = ~flea[,2],  type="scatter", 
        mode = "markers" , color = ~flea$species , 
        marker=list( size=20 , opacity=0.5)  )

plot_ly(flea[,1:6], x = ~flea[,1], y = ~flea[,2],  type="scatter", 
        mode = "markers" , color = I("green") ,  
        marker=list( size=20 , opacity=0.5)  )


### Subplot animations
gap <- gapminder %>%
  dplyr::left_join(countryByArea, by = "country") %>%
  transform(popDen = pop / area) %>%
  transform(country = forcats::fct_reorder(country, popDen))

gapKey <- crosstalk::SharedData$new(gap, ~country)

subplot(p1, p2, nrows = 1, widths = c(0.3, 0.7), titleX = TRUE) %>%
  hide_legend() %>%
  animation_opts(1000, redraw = FALSE) %>%
  layout(hovermode = "y", margin = list(l = 100)) %>%
  highlight("plotly_selected", color = "blue", opacityDim = 1, hoverinfo = "none")

p1 <- plot_ly(gap, y = ~country, x = ~popDen, hoverinfo = "x") %>%
  add_markers(alpha = 0.1, color = I("black")) %>%
  add_markers(data = gapKey, frame = ~year, ids = ~country, color = I("red")) %>%
  layout(xaxis = list(type = "log"))

p2 <- plot_ly(gap, x = ~gdpPercap, y = ~lifeExp, size = ~popDen, 
              text = ~country, hoverinfo = "text") %>%
  add_markers(color = I("black"), alpha = 0.1) %>%
  add_markers(data = gapKey, frame = ~year, ids = ~country, color = I("red")) %>%
  layout(xaxis = list(type = "log"))

subplot(p1, p2, nrows = 1, widths = c(0.3, 0.7), titleX = TRUE) %>%
  #hide_legend() %>%
  animation_opts(1000, redraw = FALSE) %>%
  layout(hovermode = "y", margin = list(l = 100)) %>%
  highlight("plotly_selected", color = "blue", opacityDim = 1, hoverinfo = "none")

df <- data.frame(
  x = c(0,0,0,0,0, 1,2,3,4,5, 0,0,0,0,0, 1,2,3,4,5),
  y = c(0,0,0,0,0, 1,1,1,1,1, 0,0,0,0,0, 2,2,2,2,2),
  f = rep(1:5,4)
)
pn <- plotly::plot_ly(df,
                              x = ~x,
                              y = ~y,
                              frame = ~f,
                              color = I("grey80"),
                              type = 'scatter',
                              mode = 'markers',
                              showlegend = F
) 
pn


## Create a shareable link to your chart
## Set up API credentials: https://plot.ly/r/getting-started
#chart_link = api_create(p, filename="animations-basic")
#chart_link


####################

## gganimate
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

