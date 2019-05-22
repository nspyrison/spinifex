library(shiny)
library(plotly)
library(spinifex)
library(dplyr)
# library(eechidna); eechidna::launch_app()
# file.edit("C:/Users/spyri/Documents/R/functionSectioning/app.R")
# file.edit("C:/Users/spyri/Documents/R/functionSectioning/inputUpdates.R")
# file.edit("C:/Users/spyri/Documents/R/functionSectioning/helper.R")
# write.csv(tourr::flea, file="./data/flea.csv",row.names=FALSE)

# separate data into numeric and group vars.
splitInput <- function(inData, rv){
  rv$numVars <- sapply(inData, is.numeric)
  rv$groupVars <- sapply(inData, function(x) is.character(x)|is.factor(x))
  rv$d <- inData[rv$numVars] # rv$d is only numeric vars
  rv$groups <- inData[rv$groupVars]
  rv$nSelected <- min(ncol(rv$d), 6)

}

updateContent <- function(rv, input, output, session) {
  updateCheckboxGroupInput(session,
                           "variables",
                           choices = names(rv$d),
                           selected = names(rv$d[1:rv$nSelected]))
  output$str_data <- renderPrint({str(rv$d)})
  
  if (length(rv$groups)>=1) {
    updateSelectInput(session,
                      "col_var",
                      choices = names(rv$groups))
    updateSelectInput(session,
                      "pch_var",
                      choices = names(rv$groups))
  } else { #list "none", if there are not character or factor vars.
    updateSelectInput(session,
                      "col_var",
                      choices = c("None"))
    updateSelectInput(session,
                      "pch_var",
                      choices = c("None"))
  }
}

# read input file, update ui
readInput <- function(file, rv, input, output, session){
  splitInput(read.csv(file$datapath, stringsAsFactors = FALSE), rv)
  output$messages <- 
    renderText(validate(need(
      rv$nSelected > 2,
      "Error: Can only display tour for more than 2 parameters!"
    )))
  # change included variable choices and selection.
  updateContent(rv, input, output, session)
}
