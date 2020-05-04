library(shiny)

ui <- fluidPage(
  
  sliderInput(inputId = 'slider', 
              label = div(style='width:300px;', 
                          div(style='float:left;', 'sooner'), 
                          div(style='float:right;', 'later')), 
              min = 0, max = 10, value = 5, width = '300px')
  
)

server <- function(input, output) {
  
}

shinyApp(ui, server)