#' Shiny app for exploring toy multivariate datasets with the manual tour
#' 
#' @author Nicholas Spyrison
#' @export
#' @examples 
#' \dontrun{
#' spinifex::run_app("intro")
#' }

requireNamespace("shiny")
requireNamespace("plotly")
requireNamespace("spinifex")
requireNamespace("dplyr")

source('ui.R', local = TRUE)

server <- function(input, output, session) {
  
  ##### Reactives ----
  ### Data initialize
  dat <- reactive({
    if (is.null(input$dat)) {return()}
    if (input$dat == "flea") return(tourr::flea)
    if (input$dat == "olive") return(tourr::olive)
    if (input$dat == "wine") return(spinifex::wine)
    if (input$dat == "weather") return(spinifex::weather)
    if (input$dat == "breastcancer") return(spinifex::breastcancer)
    if (input$dat == "mtcars") return(mtcars)
    return()
  })
  numericVars <- reactive(sapply(dat(), is.numeric))
  clusterVars <- reactive(sapply(dat(), function(x) is.character(x)|is.factor(x)))
  numericDat  <- reactive(dat()[numericVars()]) # dat of only numeric vars
  clusterDat  <- reactive(dat()[clusterVars()])
  colToSelect <- reactive(min(ncol(numericDat()), 6))
  
  ### Input initialize
  selected_dat <- reactive({
    x <- numericDat()[, which(colnames(numericDat()) %in% input$variables)]
    if (input$rescale_data) x <- tourr::rescale(x)
    return(x)
  })
  col_var <- reactive({ # a column of values
    clusterDat()[, which(colnames(clusterDat()) == input$col_var)] 
  })
  pch_var <- reactive({ # a column of values
    clusterDat()[, which(colnames(clusterDat()) == input$pch_var)] 
  })
  n <- reactive(ncol(selected_dat()))
  manip_var <- reactive(which(colnames(numericDat()) == input$manip_var)) # number of var
  basis <- reactive({prcomp(selected_dat())[[2]][, 1:2]}) # init basis to PC1:2
  
  ##### Observes -----
  ### Update include variable checkbox
  observeEvent(dat() ,{
    updateCheckboxGroupInput(session,
                             "variables",
                             choices = names(numericDat()),
                             selected = names(numericDat()[1:colToSelect()]))
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
                        "col_var",
                        choices = names(clusterDat()))
      updateSelectInput(session,
                        "pch_var",
                        choices = names(clusterDat()))
    } else { # list "none", if there are not character or factor vars.
      updateSelectInput(session,
                        "col_var",
                        choices = c("None"))
      updateSelectInput(session,
                        "pch_var",
                        choices = c("None"))
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
      play_manual_tour(data = selected_dat(),
                       basis = basis(),
                       manip_var = manip_var(),
                       col = col_of(col_var()),
                       pch = pch_of(pch_var()),
                       axes = input$axes,
                       angle = input$angle,
                       fps = input$fps
      )
    })
  }) ## end of radial tour
  
  output$str_data <- renderPrint({str(dat())})
  
  output$devMessage <- renderPrint({
    paste0("Development Message: nSelected(): ", head(numVars()))
  })
  
}
shinyApp(ui, server)
