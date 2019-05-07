library(shiny)
library(plotly)
library(spinifex)
library(dplyr)
# library(eechidna); eechidna::launch_app()
# file.edit("C:/Users/spyri/Documents/R/functionSectioning/app.R")
# file.edit("C:/Users/spyri/Documents/R/functionSectioning/inputUpdates.R")
# file.edit("C:/Users/spyri/Documents/R/functionSectioning/helper.R")
# write.csv(tourr::olive, file="./data/olive.csv",row.names=FALSE)

# separate data into numeric and group vars.
splitInput <- function(inData, rv){
  rv$numVars <- sapply(inData, is.numeric)
  rv$groupVars <- sapply(inData, function(x) is.character(x)|is.factor(x))
  rv$d <- inData[rv$numVars] # rv$d is only numeric vars
  rv$nSelected <- min(ncol(rv$d), 6)
  rv$groups <- inData[rv$groupVars]
}

# read input file, update ui
readInput <- function(file, rv, output, session){
  splitInput(read.csv(file$datapath, stringsAsFactors = FALSE), rv)
  output$messages <- 
    renderText(validate(need(
      rv$nSelected > 2,
      "Error: Can only display tour for more than 2 parameters!"
    )))
  updateCheckboxGroupInput(session,
                           "variables",
                           choices = names(rv$d),
                           selected = names(rv$d[1:rv$nSelected]))
  if (sum(rv$groupVars)) { # if only 1 groupVar, then select.
    updateSelectInput(session,
                      "cat_var",
                      choices = names(rv$groups))}
  else {
    updateSelectInput(session,
                      "cat_var",
                      choices = c("None"))
  }
}