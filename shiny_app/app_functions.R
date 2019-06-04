library(shiny)
library(plotly)
library(spinifex)
library(dplyr)
# write.csv(tourr::flea, file="./data/flea.csv",row.names=FALSE)


### GENERAL LOADING (orginally for radial manual)
# separate data into numeric and group vars.
splitInput <- function(inData, rv){
  rv$numVars <- sapply(inData, is.numeric)
  rv$groupVars <- sapply(inData, function(x) is.character(x)|is.factor(x))
  rv$d <- inData[rv$numVars] # rv$d is only numeric vars
  rv$groups <- inData[rv$groupVars]
  rv$nSelected <- min(ncol(rv$d), 6)
}

#TODO: CONTINUE MOVING OVER CONENT FROM SCRIPT TO FUNC AND WRIRE UP TO CORRECT RV VALUES.
initInput <- function(rv, input) {
  # from Input tab
  rv$selected_dat <- rv$d[, which(colnames(rv$d) %in% input$variables)]
  if (input$rescale_data) rv$selected_dat <- tourr::rescale(selected_dat)
  rv$col_var <- rv$groups[, which(colnames(rv$groups) == input$col_var)] # a column
  rv$pch_var <- rv$groups[, which(colnames(rv$groups) == input$pch_var)] # a column
  rv$n <- ncol(selected_dat)
  # From sepecific tabs:
  manip_var <- which(colnames(rv$d) == input$manip_var) # a number
  
  if (input$basis_init == "Random") .basis <- tourr::basis_random(n = n, d = 2)
  if (input$basis_init == "PCA")    .basis <- prcomp(selected_dat)[[2]][, 1:2]
  if (input$basis_init == "Manual") {
    path <- input$basispath$datapath
    ext <- tolower(substr(path, nchar(path)-4+1, nchar(path)))
    if (ext == ".csv")
      .basis <- read.csv(path, stringsAsFactors = FALSE)
    if (ext == ".rda"){ #load the object, not just name.
      tmp <- new.env()
      load(file = path, envir = tmp)
      .basis <- tmp[[ls(tmp)[1]]]
    }
  }
}

updateContent <- function(rv, input, output, session) {
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
  updateContent(rv, input, output, session)
}
### END GENERAL LOADING


### STATIC LINEAR PROJECTION
staticProjection <- function(dat, method, col, pch) {
  if (method == "PCA") {
    library(ggfortify)
    return(suppressWarnings(
      autoplot(prcomp(dat), colour = col_of(col), shape = pch_of(pch) + 15,
               loadings = T, loadings.label = T, 
               loadings.colour = 'gray50', loadings.label.colour = 'gray30')
    ))
  }
  if (method == "LDA") { # LDA NEEDS A CLASS, have to ref frame col by name.
    # library(lfda)
    # # Local Fisher Discriminant Analysis (LFDA)
    # model <- lfda(dat, col, r = 3, metric="plain")
    # return(
    #   autoplot(model, data = flea, frame = T, frame.colour = 'species')
    # )
  }
  if (method == "SPLOM") {
    library(GGally)
    splom_pch <- as.character(pch)
    return(
      GGally::ggpairs(data = dat, aes(colour = col, shape = splom_pch, alpha=.2))
    )
  }
}
# dat <- tourr::flea[,1:6]; method <- "PCA"; 
# col <- col_of(tourr::flea[,7]); pch <- pch_of(tourr::flea[,7])
# staticProjection(dat, method, col, pch)

