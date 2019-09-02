# TODO: for 'live' ploting may look at: https://stackoverflow.com/questions/29222603/shiny-slider-restrict-reaction-to-releasing-left-mouse-button/47699299
# apparently 250 ms is the default react time. could it go to 50, or 100?
## Only run examples in interactive R sessions

### USE shiny::thottle(), not shinyCustom

  options(device.ask.default = FALSE)
  
  library(shiny)
  library(magrittr)
  
  ui <- fluidPage(
    plotOutput("plot", click = clickOpts("hover")),
    helpText("Quickly click on the plot above, while watching the result table below:"),
    sliderInput("slider", "X", min = -1, max = 1, value = 0, step = .1),
    tableOutput("result")
  )
  
  server <- function(input, output, session) {
    hover <- reactive({
      if (is.null(input$hover))
        list(x = NA, y = NA)
      else
        input$hover
    })
    hover_d <- hover %>% debounce(1000)
    hover_t <- hover %>% throttle(1000)
    
    slider <- reactive(input$slider)
    slider_d <- debounce(slider, 1000)
    slider_t <- throttle(slider, 10)
    
    
    output$plot <- renderPlot({
      plot(cars)
    })
    
    output$result <- renderTable({
      data.frame(
        mode = c("raw", "throttle", "debounce"),
        x = c(hover()$x, hover_t()$x, hover_d()$x),
        y = c(hover()$y, hover_t()$y, hover_d()$y),
        slider = c(slider(), slider_t(), slider_d())
      )
    })
  }
  
  shinyApp(ui, server)


