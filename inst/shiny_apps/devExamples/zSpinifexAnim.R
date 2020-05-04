library(shiny)
library(plotly)
library(spinifex)

f_dat  <- tourr::rescale(flea[,1:6])
f_cat  <- factor(flea$species)
f_path <- save_history(f_dat, guided_tour(holes()))
f_bas  <- matrix(f_path[,, max(dim(f_path)[3])], ncol=2)
f_mvar <- 5

p <- play_manual_tour(data = f_dat, basis = f_bas, manip_var = f_mvar,
                      col = f_cat, pch = f_cat)



ui <- fluidPage(
  headerPanel("Shiny app dev"),
  numericInput(inputId = "x", label="x val", value=1),
  numericInput(inputId = "y", label="y val", value=2),
  plotOutput("plottest"),
  plotlyOutput("plotlytest")
)

server <- function(input, output) {

  output$plotlytest <- renderPlotly({
    p
  })
  output$plottest <- renderPlot({
    plot(input$x, input$y)
  })
}

shinyApp(ui = ui, server = server)
