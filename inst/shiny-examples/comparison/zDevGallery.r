library(shiny)
library(DT)

shinyApp(
  ui <- fluidPage(DT::dataTableOutput("data")),
  
  server <- function(input, output) {
    
    values <- reactiveValues(data = NULL)
    
    values$data <- as.data.frame(
      cbind(c("a", "d", "b", "c", "e", "f"),
            c(1463, 159, 54, 52, 52, 220),
            c(0.7315, 0.0795, 0.027, 0.026, 0.026, 0.11)
      )
    )
    
    shinyInput <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
      }
      inputs
    }
    
    
    rows_to_remove <- reactiveVal()
    
    df <- reactive({
      data = data.frame(
        Plot = shinyInput(actionButton, nrow(values$data), 'button_', label = "Plot", 
                            onclick = 'Shiny.onInputChange(\"plot_button\",  this.id)'),
        SaveToFile = shinyInput(actionButton, nrow(values$data), 'button_', label = "SaveToFile", 
                            onclick = 'Shiny.onInputChange(\"save_button\",  this.id)'),
        Delete = shinyInput(actionButton, nrow(values$data), 'button_', label = "Remove", 
                            onclick = 'Shiny.onInputChange(\"delete_button\",  this.id)'),
        as.data.frame(values$data),
        stringsAsFactors = FALSE,
        row.names = 1:nrow(values$data)
      )
      data[!rownames(data) %in% rows_to_remove(), ]
    })
    
    output$data <- DT::renderDataTable(
      df(), server = FALSE, escape = FALSE, selection = 'none'
    )
    
    observeEvent(input$delete_button, {
      selectedRow <- as.numeric(strsplit(input$delete_button, "_")[[1]][2])
      rows_to_remove(c(rows_to_remove(),selectedRow)) # update the rows to remove
    })
    
  }
)