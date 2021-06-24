### "radial_tour" app.R -----
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
    if(is.null(input$dat)) {return(tourr::flea)}
    if(input$dat == "flea") return(tourr::flea)
    if(input$dat == "olive") return(tourr::olive)
    if(input$dat == "wine") return(spinifex::wine)
    if(input$dat == "weather") return(spinifex::weather)
    if(input$dat == "breast cancer") return(spinifex::breastcancer)
    if(input$dat == "diabetes, long") return(spinifex::PimaIndiansDiabetes_long)
    if(input$dat == "diabetes, wide") return(spinifex::PimaIndiansDiabetes_wide)
    if(input$dat == "Upload file"){
      req(input$data_file)
      path <- input$data_file$datapath
      ext <- tolower(substr(path, nchar(path) - 4L + 1L, nchar(path)))
      ## assumptions
      if((is.null(path) | length(path) == 0L)) stop("Error in filepath length.")
      if(!(ext %in% c(".csv", ".rda"))) stop("unexpected filepath extension.")
      if(ext == ".csv")
        return(read.csv(path, stringsAsFactors = TRUE, sep = ","))
      if(ext == ".rda")
        return(load(file = path))
    }
    stop("Unexpected data selection.")
  })
  
  ### Input initialize
  selDat <- reactive({
    req(rawDat())
    req(input$projVars)
    dat <- rawDat()
    ret <- dat[complete.cases(dat), which(colnames(dat) %in% input$projVars)]
    if(input$rescale_data) ret <- scale_sd(ret)
    if(!is.matrix(ret)) ret <- as.matrix(ret)
    return(ret)
  })
  sel_col <- reactive({
    var_nm <- input$col_var_nm
    if(is.null(var_nm) | length(var_nm) == 0L) var_nm <- "<none>"
    if(var_nm == "<none>") {
      vect <- rep("a", n())
    } else {
      dat <- rawDat()
      vect <- dat[complete.cases(dat), which(colnames(dat) == var_nm)]
    }
    vect
  })
  sel_pch <- reactive({
    var_nm <- input$pch_var_nm
    if(is.null(var_nm) | length(var_nm) == 0L) var_nm <- "<none>"
    if(var_nm == "<none>") {
      vect <- rep("a", n())
    } else {
      dat <- rawDat()
      vect <- dat[complete.cases(dat), which(colnames(dat) == var_nm)]
    }
    vect
  })
  n <- reactive(ncol(selDat()))
  manip_var_num <- reactive(which(colnames(selDat()) == input$manip_var_nm)) ## Number of the var
  basis <- reactive({prcomp(selDat())[[2L]][, 1L:2L]}) ## Init basis to PC1:2
  
  ## Tour and display
  tour_path <- reactive({
    manual_tour(basis = basis(),
                manip_var = manip_var_num(),
                angle = input$angle)
  })
  plotly_anim <- reactive({
    req(manip_var_num())
    play_manual_tour(data  = selDat(),
                     basis = basis(),
                     manip_var = manip_var_num(),
                     aes_args = list(color = sel_col(), shape = sel_pch()),
                     axes = "left",
                     fps  = 9L
    )
  })
  
  ##### Observes -----
  ## If rawDat() changes, update the projection variables.
  output$inputProjVars <- renderUI({
    req(rawDat())
    dat <- rawDat()
    col_idx  <- sapply(dat, is.numeric)
    nms <- names(dat[col_idx])
    col_selected <- 1L:min(length(nms), 6L)
    checkboxGroupInput("projVars",
                       label = "Projection variables",
                       choices  = nms,
                       selected = nms[col_selected]
    )
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
  })
  
  ### If projection variables change, update input$manip_var_nm
  observeEvent(input$projVars, {
    updateSelectInput(session, "manip_var_nm", choices = input$projVars)
  })
  
  ### Output ----
  output$rawDat_summary <- renderPrint({
    dat <- as.data.frame(rawDat()) ## For naming
    summary(dat)
  })
  output$selDat_summary <- renderPrint({
    req(selDat())
    dat <- as.data.frame(selDat()) ## For naming
    summary(dat)
  })
  output$plotlyAnim <- plotly::renderPlotly(plotly_anim())
  outputOptions(output, "plotlyAnim", suspendWhenHidden = FALSE) ## Eager evaluation
  
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

