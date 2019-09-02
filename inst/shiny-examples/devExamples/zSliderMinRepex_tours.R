server <- function(input, output, session) {
  ##### Initialize ----
  rv <- reactiveValues() # rv needed for x,y,r sliders and gallery
  rv$x <- NULL
  rv$y <- NULL
  rv$rad <- NULL
  
  ### _Interactive observes ----
  #TODO: Fix sliders not yielding control on re-run
  
  myPlot <- reactive({
    if(rv$x==0) { # default of UI.
      rv$x <- round(runif(1,-1,1),1)
      rv$y <- round(runif(1,-1,1),1)
      rv$rad <- sqrt(rv$x^2 + rv$y^2)
    }
    .ang <- seq(0,360, length.out = 360) * pi / 180
    ggplot() + theme_bw() + geom_point(mapping = aes(rv$x, rv$y)) +
    xlim(-1,1) + ylim(-1,1) + coord_fixed() + 
    geom_path(mapping = aes(x=cos(.ang),y=sin(.ang)))
  })
  
  ### Display interactive
  output$obl_plot <- renderPlot(myPlot())
  
  x_slider_throt <- throttle(reactive(input$slider), 100)
  
  ### Observe sliders
  observe({
    rv$x <- x_slider_throt
    rv$y <- input$y_slider
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
    cat("Dev msg -- \n",
        "x_slider: ", input$x_slider, "\n",
        "y_slider: ", input$y_slider, "\n",
        "rad_slider: ", input$rad_slider, "\n",
        "rv:x: ", rv$x, "\n",
        "rv:y: ", rv$y, "\n",
        sep = "")
  })
}


###### UI ----
ui <- fluidPage(
  sliderInput("x_slider", "X",
              min = -1, max = 1, value = 0, step = .1)
  ,sliderInput("y_slider", "Y",
              min = -1, max = 1, value = 0, step = .1)
  ,sliderInput("rad_slider", "radius",
               min = 0, max = 1, value = 0, step = .1)
  , plotOutput("obl_plot")
  , verbatimTextOutput("dev_msg")
)


shinyApp(ui, server)

