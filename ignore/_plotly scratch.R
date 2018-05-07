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
??gganimate
## Create a shareable link to your chart
## Set up API credentials: https://plot.ly/r/getting-started
#chart_link = api_create(p, filename="animations-basic")
#chart_link

head(dp)
dff <- as.data.frame(dp)

col <- rainbow(length(unique(flea$species)))[as.numeric(as.factor(flea$species))]
col <- rep(col,max(dff$index)) #need to rep col across max(index)

p2 <- plotly::plot_ly(dff,
    x = ~x,
    y = ~y,
    color= ~col,
    type = 'scatter',
    mode = 'markers',
    frame = ~index,
    showlegend = F
  )

p2

###color
# left
plot_ly(flea[,1:6], x = ~flea[,1], y = ~flea[,2],  type="scatter", 
        mode = "markers" , color = ~flea$species , 
        marker=list( size=20 , opacity=0.5)  )

plot_ly(flea[,1:6], x = ~flea[,1], y = ~flea[,2],  type="scatter", 
        mode = "markers" , colors = "Set2" ,  
        color = NULL,
        marker=list( size=20 , opacity=0.5)  )

