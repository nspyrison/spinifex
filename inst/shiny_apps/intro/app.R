### "Intro" app.R -----
# options(shiny.error = FALSE)

#' Shiny app for exploring toy multivariate datasets with the manual tour
#' 
#' @author Nicholas Spyrison
#' @export
#' @examples 
#' \dontrun{
#' spinifex::run_app("intro")
#' }

source('../global_shinyApps.r', local = TRUE)
source('ui.R', local = TRUE)

server <- function(input, output, session) {
  
  ##### Reactives ----
  ### Data initialize
  rawDat <- reactive({
    if (is.null(input$dat)) {return()}
    if (input$dat == "flea") return(tourr::flea)
    if (input$dat == "olive") return(tourr::olive)
    if (input$dat == "wine") return(spinifex::wine)
    if (input$dat == "weather") return(spinifex::weather)
    if (input$dat == "breastcancer") return(spinifex::breastcancer)
    if (input$dat == "mtcars") return(mtcars)
    if (input$data_source == "Upload .csv file"){
      path <- input$data_file$datapath
      ext <- tolower(substr(path, nchar(path) - 4 + 1, nchar(path)))
      ## assumptions
      if ((is.null(path) | length(path) == 0)) stop("Error in filepath length.")
      if (!(ext %in% c(".csv", ".rda"))) stop("unexpected filepath extension.")
      if (ext == ".csv")
        return(read.csv(path, stringsAsFactors = FALSE))
      if (ext == ".rda")
        return(load(file = path))
    }
  stop("Unexpected error reading data.")
  })
  numericVars <- reactive(sapply(rawDat(), is.numeric))
  clusterVars <- reactive(sapply(rawDat(), function(x) is.character(x)|is.factor(x)))
  numericDat  <- reactive(rawDat()[numericVars()]) ## dat of only numeric vars
  clusterDat  <- reactive(rawDat()[clusterVars()])
  colToSelect <- reactive(min(ncol(numericDat()), 6))
  
  ### Input initialize
  projMat <- reactive({
    x <- numericDat()[, which(colnames(numericDat()) %in% input$variables)]
    if (input$rescale_data) x <- tourr::rescale(x)
    x <- as.matrix(x)
    return(x)
  })
  col_var <- reactive({ ## a column of values
    clusterDat()[, which(colnames(clusterDat()) == input$col_nm)] 
  })
  pch_var <- reactive({ ## a column of values
    clusterDat()[, which(colnames(clusterDat()) == input$pch_nm)] 
  })
  n <- reactive(ncol(projMat()))
  manip_var <- reactive(which(colnames(numericDat()) == input$manip_var)) # number of var
  basis <- reactive({prcomp(projMat())[[2]][, 1:2]}) # init basis to PC1:2
  
  ##### Observes -----
  ### Update include variable checkbox
  observeEvent(rawDat() ,{
    numDat <- numericDat()
    updateCheckboxGroupInput(session,
                             "variables",
                             choices = names(numDat),
                             selected = names(numDat[1:colToSelect()]))
  })
  
  ### Update manip_var based on selected include varables
  observeEvent(input$variables, {
    updateSelectInput(session,
                      "manip_var",
                      choices = input$variables)
  })
  
  ### Update pch/col var if clusterDat changes
  observeEvent(clusterDat(), {
    if (length(clusterDat()) >= 1) {
      updateSelectInput(session,
                        "col_nm",
                        choices = c(names(clusterDat()), "<none>"))
      updateSelectInput(session,
                        "pch_nm",
                        choices = c(names(clusterDat()), "<none>"))
    } else { ## list "none", if there are not character or factor vars.
      updateSelectInput(session,
                        "col_nm",
                        choices = c("<none>"))
      updateSelectInput(session,
                        "pch_nm",
                        choices = c("<none>"))
    }
  })
  
  
  ### Output ----
  ## Radial tour
  observeEvent(input$radial_button, {
    tour_path <- reactive({manual_tour(basis = basis(),
                                       manip_var = manip_var(),
                                       angle = input$angle)
    })
    
    output$plotlyAnim <- plotly::renderPlotly({
      play_manual_tour(data = projMat(),
                       basis = basis(),
                       manip_var = manip_var(),
                       col = col_of(col_var()),
                       pch = pch_of(pch_var()),
                       axes = "left",
                       fps = 9
      )
    })
  }) ## end of radial tour
  
  output$rawDat_summary <- renderPrint({
    dat <- as.data.frame(rawDat())
    tibble::as.tibble(dat)
  })
  output$projDat_summary <- renderPrint({
    dat <- as.data.frame(projMat())
    tibble::as.tibble(dat)
  })
  
  #### Dev tools -----
  ## toggle display by setting .include_dev_display at the top of ../global_shinyApps.r
  
  ## Development help -- to display dev tools see the top of 'global_shinyApps.r'
  if (.include_dev_display == TRUE) {
    shinyjs::show("dev_toggle")
  } ## else (.include_dev_display != TRUE) dev content remains hidden.
  
  ## Browser button
  observeEvent(input$browser, {browser()})
  
  ## Development message 
  output$dev_msg <- renderPrint({
    cat("Dev msg -- \n",
        "input$dat ", input$dat, "\n",
        ## ect. add as needed.
        sep = ""
    )
  })
}

shinyApp(ui = ui, server = server)

