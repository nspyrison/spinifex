#' Shiny app for exploring toy multivariate datasets with the manual tour
#' 
#' @author Nicholas Spyrison
#' @export
#' @examples 
#' \dontrun{
#' library(spinifex)
#' run_app("intro")
#' }

library(shiny)
library(plotly)
library(spinifex)
library(dplyr)

source('ui.R', local = TRUE)

server <- function(input, output, session) {

  data <- reactive({
    if (is.null(input$dat)) {return()}
    if (input$dat == "flea") .data <- tourr::flea
    if (input$dat == "olive") .data <- tourr::olive
    if (input$dat == "wine") .data <- spinifex::wine
    if (input$dat == "weather") .data <- spinifex::weather
    if (input$dat == "breastcancer") .data <- spinifex::breastcancer
    if (input$dat == "mtcars") .data <- mtcars
    return(.data)
  })
  
  ### Data initialize
  numericVars <- reactive(sapply(data(), is.numeric))
  groupVars   <- reactive(sapply(data(), function(x) is.character(x)|is.factor(x)))
  numericDat  <- reactive(data()[numericVars()]) # d is only numeric vars
  groupDat    <- reactive(data()[groupVars()])
  colToSelect <- reactive(min(ncol(numericDat()), 6))
  
  ### Input initialize
  selected_dat <- reactive({
    x <- numericDat()[, which(colnames(numericDat()) %in% input$variables)]
    if (input$rescale_data) x <- tourr::rescale(x)
    return(x)
  })
  col_var <- reactive({ # a column
    groupDat()[, which(colnames(groupDat()) == input$col_var)] 
  })
  pch_var <- reactive({
    groupDat()[, which(colnames(groupDat()) == input$pch_var)] # a column
  })
  n <- reactive(ncol(selected_dat()))
  manip_var <- reactive(which(colnames(numericDat()) == input$manip_var)) # number
  basis <- reactive({
    if (input$basis_init == "Random") x <- tourr::basis_random(n = n(), d = 2)
    if (input$basis_init == "PCA")    x <- prcomp(selected_dat())[[2]][, 1:2]
    return(x)
  })
  
  ### Update dropdown lists
  observe({
    updateCheckboxGroupInput(session,
                             "variables",
                             choices = names(numericDat()),
                             selected = names(numericDat()[1:colToSelect()]))
    
    updateSelectInput(session,
                      "manip_var",
                      choices = input$variables)
    
    if (length(groupDat()) >= 1) {
      updateSelectInput(session,
                        "col_var",
                        choices = names(groupDat()))
      updateSelectInput(session,
                        "pch_var",
                        choices = names(groupDat()))
    } else { # list "none", if there are not character or factor vars.
      updateSelectInput(session,
                        "col_var",
                        choices = c("None"))
      updateSelectInput(session,
                        "pch_var",
                        choices = c("None"))
    }
  })
  
  ### Output
  output$str_data <- renderPrint({str(data())})
  ## Radial tour
  observeEvent(input$radial_button, {
    tour_path <- reactive({manual_tour(basis = basis(),
                                       manip_var = manip_var(),
                                       angle = input$angle)
    })
    
    output$plotlyAnim <- renderPlotly({
      play_manual_tour(data = selected_dat(),
                       basis = basis(),
                       manip_var = manip_var(),
                       col = col_of(col_var()),
                       pch = pch_of(pch_var()),
                       axes = input$axes,
                       angle = input$angle
      )
    })
  }) ## end of radial tour
  
  output$devMessage <- renderPrint({
    numericVars()
    #paste("Development Message: nSelected(): ", head(numVars()))
  })
  
}
shinyApp(ui, server)
