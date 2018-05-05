#https://plot.ly/r/animations/
library(plotly)
packageVersion('plotly')

df <- data.frame(
  x = c(1,2,1), 
  y = c(1,2,1), 
  f = c(1,2,3)
)

p <- df %>%
  plotly::plot_ly(
    x = ~x,
    y = ~y,
    frame = ~f,
    text = ~paste0(x,y),
    hoverinfo = "other",
    type = 'scatter',
    mode = 'markers',
    showlegend = F
  )

p

## Create a shareable link to your chart
## Set up API credentials: https://plot.ly/r/getting-started
#chart_link = api_create(p, filename="animations-basic")
#chart_link

head(dp)
dff <- as.data.frame(dp)

p2 <- dff %>%
  plot_ly(
    x = ~x,
    y = ~y,
    #color="black",
    type = 'scatter',
    mode = 'markers',
    frame = ~index,
    showlegend = F
  )

p2
