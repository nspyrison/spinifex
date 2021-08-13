##### Setup -----
library(ggplot2)

server <- function(input, output, session) {
  ##### Reacters -----
  myPlot <- reactive({
    .ang <- seq(0, 360, length.out = 360) * pi / 180
    .circ <- data.frame(x = cos(.ang), y = sin(.ang))
    ggplot() + theme_bw() + 
      geom_point(mapping = aes(input$x_slider, input$y_slider)) +
      xlim(-1, 1) + ylim(-1, 1) + coord_fixed() + 
      geom_path(data = .circ, mapping = aes(x, y))
  })
  output$myPlot <- renderPlot(myPlot())
  
  rad <- reactive(round(sqrt(input$x_slider^2 + input$y_slider^2), 2))
  output$rad_val <- renderText(rad()) 
  
  ##### Observers -----
  observeEvent(input$rand_basis, {
    x <- round(runif(1, -1, 1), 1)
    y <- round(runif(1, -1, 1), 1)
    updateSliderInput(session, "x_slider", value = x)
    updateSliderInput(session, "y_slider", value = y)
  })
  
  ### Development help -- uncomment message at bottom on ui to use
  output$dev_msg <- renderPrint({
    cat("Dev msg -- \n",
        "x_slider: ", input$x_slider, "\n",
        "y_slider: ", input$y_slider, "\n",
        "rad(): ", rad(), "\n",
        sep = "")
  })
}

###### ur.r -----
ui <- fluidPage(
  sliderInput("x_slider", "X value",
              min = -1, max = 1, value = 0, step = .1),
  sliderInput("y_slider", "Y value",
               min = -1, max = 1, value = 0, step = .1),
  h5("Radius value"),
  textOutput("rad_val"),
  actionButton("rand_basis", "Random point"),
  plotOutput("myPlot"),
  verbatimTextOutput("dev_msg")
)

## shinyApp call
shinyApp(ui, server)

