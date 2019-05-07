library(shiny)
library(plotly)
library(spinifex)
library(dplyr)
# library(eechidna); eechidna::launch_app()
# file.edit("C:/Users/spyri/Documents/R/functionSectioning/app.R")
# write.csv(tourr::olive, file="./data/olive.csv",row.names=FALSE)


launchApp <- function(.data = NULL, .basis = NULL) {
  if (is.null(.data)|T) {
    .data  <- tourr::flea
  }
  
  vars <- names(.data)
  nSelected <- min(ncol(.data), 15)

  
  source('ui.R', local = TRUE)
  
  server <- function(input, output) {
    
    datasetReact <- reactive({
      # move this to a reactive function?
      if (!is.null(input$file)) {
        .data <- read.csv(input$file$datapath, header = T, sep = ",")
      }
      
      
      
    })
    
    output$str_data <- renderPrint({

      
      str(.data)
    })
    
    output$plotlyAnim <- renderPlotly({
      # move this to a reactive function?
      num_data <- .data[, which(colnames(.data) %in% input$variables)]
      cat_var  <- .data[, which(colnames(.data) == input$cat_var)]
      n <- ncol(num_data)
      if (input$rescale_data) num_data <- tourr::rescale(num_data)
      if (input$rand_basis) .basis <- tourr::basis_random(n = n, d = 2)
      
      play_manual_tour(
        data = num_data,
        basis = .basis,
        manip_var = which(colnames(.data) == input$manip_var),
        col = cat_var,
        pch = cat_var,
        axes = input$axes,
        angle = input$angle
      )
    })
    
    
  }
  shinyApp(ui, server)
}

launchApp()