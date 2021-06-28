library(shiny)
library(DT)

shinyApp(
  ui <- fluidPage(DT::dataTableOutput("data")),
  
  server <- function(input, output) {
    
    rv <- reactiveValues(data = NULL)
    
    rv$data <- as.data.frame(
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
        Plot = shinyInput(actionButton, nrow(rv$data), 'button_', label = "Plot", 
                            onclick = 'Shiny.onInputChange(\"plot_button\",  this.id)'),
        SaveToFile = shinyInput(actionButton, nrow(rv$data), 'button_', label = "SaveToFile", 
                            onclick = 'Shiny.onInputChange(\"save_button\",  this.id)'),
        Delete = shinyInput(actionButton, nrow(rv$data), 'button_', label = "Remove", 
                            onclick = 'Shiny.onInputChange(\"delete_button\",  this.id)'),
        as.data.frame(rv$data),
        stringsAsFactors = FALSE,
        row.names = 1:nrow(rv$data)
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



#### AND SPARKLINES
# 
# library(dplyr)
# library(sparkline)
# library(DT)
# 
# dfO <- data.frame(Type = c("A", "B", "C"),
#                   Value_1 = c("1,1,2,2", "2,2,3,3", "3,3,4,4"), 
#                   Value_2 = c("0,1,2,3", "2,3,4,5", "4,5,6,7"))
# library(tidyr)
# library(dplyr)
# 
# df <- dfO %>% 
#   separate_rows(Value_1, Value_2) %>% 
#   mutate_at(vars(starts_with('Value')) ,funs(as.integer))
# 
# df
# 
# 
# df %>% 
#   group_by(Type) %>% 
#   summarize(l1 = spk_chr(Value_1#,
#                          # lineColor = 'black', 
#                          # fillColor = '#ccc',
#                          # chartRangeMin = 0,
#                          # chartRangeMax = 8,
#                          # width = 80,
#                          # height = 60,
#                          # highlightLineColor = 'orange', 
#                          # highlightSpotColor = 'orange'
#                          ),
#             l2 = spk_chr(Value_2#,
#                          # lineColor = 'black', 
#                          # fillColor = '#ccc',
#                          # chartRangeMin = 0,
#                          # chartRangeMax = 8,
#                          # width = 80,
#                          # height = 60,
#                          # highlightLineColor = 'orange', 
#                          # highlightSpotColor = 'orange'
#                          )) %>% 
#   datatable(escape = F,
#             rownames = F
#   ) %>% 
#   spk_add_deps()
# 
