library(shiny)
library(plotly)
library(spinifex)
library(dplyr)
# write.csv(tourr::flea, file="./data/flea.csv",row.names=FALSE)

numVars <- NULL

### GENERAL LOADING/INIT
parseData <- function(.data, rv){
  rv$numVars <- sapply(.data, is.numeric)
  rv$groupVars <- sapply(.data, function(x) is.character(x)|is.factor(x))
  rv$d <- .data[rv$numVars] # rv$d is only numeric vars
  rv$groups <- .data[rv$groupVars]
  rv$nSelected <- min(ncol(rv$d), 6)
}
# numVars <- reactive(sapply(data(), is.numeric))
# groupVars <- reactive(sapply(data(), function(x) is.character(x)|is.factor(x)))
# d <- reactive(data()[numVars()]) # d is only numeric vars
# groups <- reactive(data()[groupVars()])
# nSelected <- reactive(min(ncol(d()), 6))


updateParam <- function(rv, input, output, session) {
  
  updateCheckboxGroupInput(session,
                           "variables",
                           choices = names(rv$d),
                           selected = names(rv$d[1:rv$nSelected]))
  output$str_data <- renderPrint({str(rv$d)})
  
  if (length(rv$groups) >= 1) {
    updateSelectInput(session,
                      "col_var",
                      choices = names(rv$groups))
    updateSelectInput(session,
                      "pch_var",
                      choices = names(rv$groups))
  } else { # list "none", if there are not character or factor vars.
    updateSelectInput(session,
                      "col_var",
                      choices = c("None"))
    updateSelectInput(session,
                      "pch_var",
                      choices = c("None"))
  }
}

initInput <- function(rv, input) {
  # Apply inputs to data
  rv$selected_dat <- rv$d[, which(colnames(rv$d) %in% input$variables)]
  if (input$rescale_data) rv$selected_dat <- tourr::rescale(rv$selected_dat)
  rv$col_var <- rv$groups[, which(colnames(rv$groups) == input$col_var)] # a column
  rv$pch_var <- rv$groups[, which(colnames(rv$groups) == input$pch_var)] # a column
  rv$n <- ncol(rv$selected_dat)
  rv$manip_var <- which(colnames(rv$d) == input$manip_var) # a number

  # Basis init
  if (input$basis_init == "Random") rv$basis <- tourr::basis_random(n = rv$n, d = 2)
  if (input$basis_init == "PCA")    rv$basis <- prcomp(rv$selected_dat)[[2]][, 1:2]
}
### END GENERAL LOADING/INIT

