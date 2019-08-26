options(shiny.reactlog = TRUE)

library(shiny)
library(reactlog)
library(plotly)

ui <- fluidPage(
  selectInput("var", "Choose a variable", choices = names(diamonds)),
  plotlyOutput("plot")
)

server <- function(input, output, session) {
  output$plot <- renderPlotly({
    plot_ly(x = diamonds[[input$var]])
  })
  


    dput(shiny::reactlog(), 
         file = paste0("reactlog_", substr(Sys.time(), 1, 16), ".txt")
    )


  
}

shinyApp(ui, server)