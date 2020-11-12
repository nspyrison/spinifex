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

## Don't source(~global_shinyApps.r), moved setup to 'ui.R'
source('ui.R', local = TRUE)

server <- function(input, output, session) {
  
  ##### Reactives ----
  ### Data initialize
  rawDat <- reactive({
    if (is.null(input$dat)) {return(tourr::flea)}
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
  
  ### Input initialize
  selDat <- reactive({
    dat <- rawDat()
    ret <- dat[, which(colnames(dat) %in% input$projVars)]
    if (input$rescale_data) ret <- tourr::rescale(ret)
    if (!is.matrix(ret)) ret <- as.matrix(ret)
    return(ret)
  })
  sel_col <- reactive({
    var_nm <- input$col_var_nm
    if (is.null(var_nm) | length(var_nm) == 0) var_nm <- "<none>"
    if (var_nm == "<none>") {
      var <- rep("a", n())
    } else {
      dat <- rawDat()
      var <- dat[, which(colnames(dat) == var_nm)]
    }
    var
  })
  sel_pch <- reactive({
    var_nm <- input$pch_var_nm
    if (is.null(var_nm) | length(var_nm) == 0) var_nm <- "<none>"
    if (var_nm == "<none>") {
      var <- rep("a", n())
    } else {
      dat <- rawDat()
      var <- dat[, which(colnames(dat) == var_nm)]
    }
    var
  })
  n <- reactive(ncol(selDat()))
  manip_var_num <- reactive(which(colnames(selDat()) == input$manip_var_nm)) ## Number of the var
  basis <- reactive({prcomp(selDat())[[2]][, 1:2]}) ## Init basis to PC1:2
  
  ## Tour and display
  tour_path <- reactive({
    manual_tour(basis = basis(),
                manip_var = manip_var_num(),
                angle = input$angle)
  })
  plotly_anim <- reactive({
    play_manual_tour(data  = selDat(),
                     basis = basis(),
                     manip_var = manip_var_num(),
                     aes_args = list(color = sel_col(), shape = sel_pch()),
                     axes = "left",
                     fps  = 9
    )
  })
  
  ##### Observes -----
  ## If rawDat() changes, update the projection variables.
  observeEvent(rawDat() ,{
    dat <- rawDat()
    ## Logical for columns that are numeric AND column-complete
    numVars_TF  <- sapply(dat, function(x) {
      is.numeric(x) & all(complete.cases(x))
    })
    numVars_nms <- names(dat[numVars_TF])
    numSelected <- 1:min(length(numVars_nms), 6)
    updateCheckboxGroupInput(session,
                             "projVars",
                             choices  = numVars_nms,
                             selected = numVars_nms[numSelected])
  })
  
  ### If rawDat() changes, Update pch/col var 
  observeEvent({rawDat()}, {
    dat  <- rawDat()
    ## Logical for columns that are (character OR factor) AND column-complete
    clusterVars_TF  <- sapply(dat,  function(x) {
      (is.character(x) | is.factor(x)) & all(complete.cases(x))
    })
    clusterVars_nms <- names(dat)[clusterVars_TF]
    opts <- c(clusterVars_nms, "<none>")
    updateSelectInput(session, "col_var_nm", choices = opts)
    updateSelectInput(session, "pch_var_nm", choices = opts)
  }
  )
  
  ### If projection variables change, update input$manip_var_nm
  observeEvent(input$projVars, {
    updateSelectInput(session, "manip_var_nm", choices = input$projVars)
  })
  
  ### Output ----
  output$rawDat_summary <- renderPrint({
    dat <- as.data.frame(rawDat()) ## For naming
    tibble::as_tibble(dat)
  })
  output$selDat_summary <- renderPrint({
    dat <- as.data.frame(selDat()) ## For naming
    tibble::as_tibble(dat)
  })
  output$plotlyAnim <- plotly::renderPlotly(plotly_anim())
  
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
        "input$col_var_nm ", input$col_var_nm, "\n",
        ## ect. add as needed.
        sep = ""
    )
  })
}

shinyApp(ui = ui, server = server)

