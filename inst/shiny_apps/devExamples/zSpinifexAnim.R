##### Setup -----
library(shiny)
library(plotly)
library(spinifex)
library(tourr)

f_dat  <- tourr::rescale(tourr::flea[, 1:6])
f_cat  <- factor(tourr::flea$species)
f_path <- tourr::save_history(f_dat, guided_tour(holes()))
f_bas  <- matrix(f_path[,, max(dim(f_path)[3])], ncol=2)
f_mvar <- 5

p <- play_manual_tour(data = f_dat, basis = f_bas, manip_var = f_mvar,
                      col = f_cat, pch = f_cat)

##### server.r -----
server <- function(input, output) {
  output$plotlytest <- renderPlotly({
    p
  })
  output$plottest <- renderPlot({
    plot(input$x, input$y)
  })
}

##### ui.r -----
ui <- fluidPage(
  headerPanel("Shiny app dev"),
  numericInput(inputId = "x", label="x val", value=1),
  numericInput(inputId = "y", label="y val", value=2),
  plotOutput("plottest"),
  plotlyOutput("plotlytest")
)

###### Call shiny::shinyApp()
shinyApp(ui = ui, server = server)
