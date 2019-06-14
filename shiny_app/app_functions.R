library(shiny)
library(plotly)
library(spinifex)
library(dplyr)
### Projection Pursuit
library(scagnostics) # "Skinny", "Striated", "Convex", "Clumpy" 
library(mbgraphic) #splines2d, dcor2d
library(minerva) #MIC, TIC
# write.csv(tourr::flea, file="./data/flea.csv",row.names=FALSE)


### GENERAL LOADING/INIT
parseData <- function(.data, rv){
  rv$numVars <- sapply(.data, is.numeric)
  rv$groupVars <- sapply(.data, function(x) is.character(x)|is.factor(x))
  rv$d <- .data[rv$numVars] # rv$d is only numeric vars
  rv$groups <- .data[rv$groupVars]
  rv$nSelected <- min(ncol(rv$d), 6)
}

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
  # Basis from file
  if (input$basis_init == "From file") {
    path <- input$basispath$datapath
    ext <- tolower(substr(path, nchar(path)-4+1, nchar(path)))
    if (ext == ".csv") rv$basis <- read.csv(path, stringsAsFactors = FALSE)
    if (ext == ".rda"){ # load .rda object, not just name.
      tmp <- new.env()
      load(file = path, envir = tmp)
      rv$basis <- tmp[[ls(tmp)[1]]]
    }
  }
  # basis from end of Proj pursuit.
  if (input$basis_init == "Projection pursuit") {
    tourFunc <- getGuidedTour(input$pp_type)
    tour <- save_history(rv$selected_dat, tourFunc)
    basis_len <- dim(tour)[3]
    rv$basis <- matrix(as.numeric(tour[,, basis_len]), ncol = 2)
  }
}
### END GENERAL LOADING/INIT


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
# TEST STATIC
# dat <- tourr::flea[,1:6]; method <- "PCA"; 
# col <- col_of(tourr::flea[,7]); pch <- pch_of(tourr::flea[,7])
# staticProjection(dat, method, col, pch)
### END OF STATIC

### PROJECTION PURSUIT
guidedTourOptions <- c("cmass", "holes", "Skinny", "Striated", "Convex", "Clumpy"
                       ,"splines2d", "dcor2d", "MIC", "TIC") 
scags <- function(scagMetricIndex) {
  function(mat) {return (scagnostics(mat)[scagMetricIndex])}
}
getGuidedTour <- function(indexName, grId=NA){ # reurtns a tour function
  if(indexName=="cmass"){return(guided_tour(cmass()))}
  if(indexName=="holes"){return(guided_tour(holes()))}
  if(indexName %in% c("Skinny", "Striated", "Convex", "Clumpy")){return(guided_tour(scags(indexName)))}
  if(indexName=="splines2d"){return(guided_tour(splineIndex()))}
  if(indexName=="dcor2d"){return(guided_tour(dcorIndex()))}
  if(indexName %in% c("MIC", "TIC")){return(guided_tour(mineIndex(indexName)))}
  if(indexName=="lda_pp"){return(guided_tour(lda_pp(grId)))} #TODO:no grID 
  if(indexName=="pda_pp"){return(guided_tour(pda_pp(grId)))} # as of now
  else return(guided_tour(holes()))
}
###