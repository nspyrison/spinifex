server <- function(input, output, session) {
  ##### Initialize ----
  rv <- reactiveValues() # rv needed for x,y,r sliders and gallery
  rv$x <- NULL
  rv$y <- NULL
  rv$rad <- NULL
  
  ### _Interactive observes ----
  #TODO: Fix sliders not yielding control on re-run
  
  myPlot <- reactive({
    .ang <- seq(0,360, length.out = 360) * pi / 180
    ggplot() + theme_bw() + geom_point(mapping = aes(rv$x, rv$y)) +
    xlim(-1,1) + ylim(-1,1) + coord_fixed() + 
    geom_path(mapping = aes(x=cos(.ang),y=sin(.ang)))
  })
  
  ### Display interactive
  observeEvent(input$obl_run, {
    rv$x <- round(runif(1,-1,1),1)
    rv$y <- round(runif(1,-1,1),1)
    rv$rad <- sqrt(rv$x^2 + rv$y^2)
    output$obl_plot <- renderPlot(myPlot())
  })
    
  ### Observe sliders
  observeEvent(input$x_slider, {
    rv$x <- input$x_slider
    
  })
  observeEvent(input$y_slider, {
    rv$y <- input$y_slider
  })
  observeEvent(input$rad_slider, {
    ang <- atan(input$y_slider / input$x_slider)
    rv$rad <- input$rad_slider
  })
  
  
  observe({
    rv$rad <- sqrt(rv$x^2 + rv$y^2)
    updateSliderInput(session, "x_slider", value = rv$x)
    updateSliderInput(session, "y_slider", value = rv$y)
    updateSliderInput(session, "rad_slider", value = rv$rad)
  })
  
  ### Development help -- uncomment message at bottom on ui to use
  output$dev_msg <- renderPrint({
    cat("Dev msg --\n",
        "obl_run: ", input$obl_run, "\n",
        "x_slider: ", input$x_slider, "\n",
        "y_slider: ", input$y_slider, "\n",
        "rad_slider: ", input$rad_slider, "\n",
        sep = "")
  })
}


###### UI ----
ui <- fluidPage(
  sliderInput("x_slider", "X",
              min = -1, max = 1, value = 0, step = .1)
  ,sliderInput("y_slider", "Y ",
              min = -1, max = 1, value = 0, step = .1)
  ,sliderInput("rad_slider", "radius",
               min = 0, max = 1, value = .1,)
  ,actionButton("obl_run", "Run")
  , plotOutput("obl_plot")
  , verbatimTextOutput("dev_msg")
)


shinyApp(ui, server)

