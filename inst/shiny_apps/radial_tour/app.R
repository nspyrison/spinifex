### "radial_tour" app.R -----
# options(shiny.error = FALSE)

#' Shiny app for exploring toy multivariate datasets with the manual tour
#' 
#' @author Nicholas Spyrison
#' @export
#' @examples 
#' \donttest{
#' spinifex::run_app(app_nm = "radial_tour")
#' spinifex::run_app(app_nm = "radial_tour", display.mode = "showcase")
#' }

## Don't source(~global_shinyApps.r), moved setup to 'ui.R'
source('ui.R', local = TRUE)

server <- function(input, output, session) {
  ## Reactives ----
  raw_dat <- reactive({
    req(input$dat)
    if(input$dat == "flea")           return(tourr::flea)
    if(input$dat == "olive")          return(tourr::olive)
    if(input$dat == "wine")           return(spinifex::wine)
    if(input$dat == "weather")        return(spinifex::weather_na.rm)
    if(input$dat == "breast cancer")  return(spinifex::BreastCancer_na.rm)
    if(input$dat == "diabetes, long") return(spinifex::PimaIndiansDiabetes_long)
    if(input$dat == "diabetes, wide") return(spinifex::PimaIndiansDiabetes_wide)
    if(input$dat == "Upload file"){
      path <- req(input$data_file$datapath)
      ext <- tolower(substr(path, nchar(path) - 4L + 1L, nchar(path)))
      ## Assumptions
      if((is.null(path) | length(path) == 0L)) stop("Error in filepath length.")
      if(!(ext %in% c(".csv", ".rds"))) stop("unexpected filepath extension.")
      if(ext == ".csv")
        return(read.csv(path, stringsAsFactors = TRUE, sep = ","))
      if(ext == ".rds")
        return(readRDS(file = path))
    }
    stop("Unexpected data selection.")
  })
  idx_complete_rows <- reactive({
    complete.cases(req(raw_dat())[, req(input$proj_vars)])
  })
  ## Selected data
  sel_dat <- reactive({
    dat <- req(raw_dat())
    ret <- dat[req(idx_complete_rows()), which(colnames(dat) %in% req(input$proj_vars))]
    if(input$rescale_data) ret <- scale_sd(ret)
    if(!is.matrix(ret))    ret <- as.matrix(ret)
    return(ret)
  })
  ## Names of columns that are characters or factors, for color and shape
  char_fct_col_nms <- reactive({
    dat <- req(raw_dat())
    cluster_vars_idx <- sapply(dat, function(c) {
      (is.character(c) | is.factor(c)) & all(complete.cases(c))
    })
    return(names(dat)[cluster_vars_idx] %>% c("<none>"))
  })
  ## Selected color, the number of the column of the selected column name
  sel_col <- reactive({
    var_nm <- req(input$col_var_nm)
    if(is.null(var_nm) | length(var_nm) == 0L) var_nm <- "<none>"
    if(var_nm == "<none>") {
      vect <- "a"
    }else{
      dat  <- req(raw_dat())
      vect <- dat[idx_complete_rows(), which(colnames(dat) == var_nm)]
    }
    return(vect)
  })
  ## Selected shape, the number of the column of the selected column name
  sel_pch <- reactive({
    var_nm <- input$pch_var_nm
    if(is.null(var_nm) | length(var_nm) == 0L) var_nm <- "<none>"
    if(var_nm == "<none>") {
      vect <- "a"
    }else{
      dat  <- req(raw_dat())
      vect <- dat[req(idx_complete_rows()), which(colnames(dat) == var_nm)]
    }
    return(vect)
  })
  manip_var_num <- reactive({
    which(colnames(req(sel_dat())) == req(input$manip_var_nm))
  })
  basis <- reactive(prcomp(req(sel_dat()))[[2L]][, 1L:2L]) ## Init basis to PC1:2
  
  ## Observes & inputs -----
  ## Create input for "proj_vars" based on the numeric columns in the data.
  output$ui__na_msg <- renderUI({
    .n_na <- sum(!req(idx_complete_rows()), na.rm = TRUE)
    if(.n_na > 0L){
      msg <- paste0(.n_na, " rows contained NA values and were excluded.")
    }else{
      msg <- "No rows were identified as NA."}
    p(msg)
  })
  output$input__proj_vars <- renderUI({
    dat <- req(raw_dat())
    num_col_idx <- sapply(dat, is.numeric)
    nms <- names(dat[num_col_idx])
    col_selected <- 1L:min(length(nms), 6L)
    checkboxGroupInput("proj_vars",
                       label = "Projection variables",
                       choices  = nms,
                       selected = nms[col_selected])
  })
  
  ## Create "manip_var_nm" from numeric projection variables
  output$input__manip_var_nm <- renderUI({
    selectInput("manip_var_nm",
                label = "Manipulation variable:",
                choices  = input$proj_vars,
                selected = input$proj_vars[1L])
  })
  
  ## Create "col_var_nm" from char or  projection variables
  output$input__col_var_nm <- renderUI({
    opts <- req(char_fct_col_nms())
    selectInput("col_var_nm",
                label = "Point color on:",
                choices  = opts,
                selected = opts[1L])
  })
  output$input__pch_var_nm <- renderUI({
    opts <- req(char_fct_col_nms())
    selectInput("pch_var_nm",
                label = "Point shape on:",
                choices  = opts,
                selected = opts[1L])
  })
  
  ## Output ----
  output$raw_dat_summary <- renderPrint({
    req(raw_dat()) %>% as.data.frame() %>% summary()
  })
  output$sel_dat_summary <- renderPrint({
    req(sel_dat()) %>% as.data.frame() %>% summary()
  })
  output$plotly_anim <- plotly::renderPlotly({
    mt  <- manual_tour(req(basis()), req(manip_var_num()))
    ggt <- ggtour(mt, req(sel_dat()), angle = .08) +
      proto_basis() +
      proto_point(list(color = req(sel_col()), shape = req(sel_pch())),
                  list(size = 2L))
    animate_plotly(ggt, fps = 9L)
  })
  outputOptions(output, "plotly_anim", suspendWhenHidden = FALSE) ## Eager evaluation
}

shinyApp(ui = ui, server = server)

