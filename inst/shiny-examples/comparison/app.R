#' Shiny app for exploring multivariate data, comparing manual tours with 
#' alternative techniques
#' 
#' @author Nicholas Spyrison
#' @export
#' @examples \dontrun{
#' library(spinifex)
#' run_app("comparison")
#' }

source('global.R', local = TRUE)
source('ui.R', local = TRUE)



server <- function(input, output, session) {
  
  data <- reactive({
      if (is.null(input$file)) {return(tourr::flea)}
      read.csv(input$file$datapath, stringsAsFactors = FALSE)
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
  
  ##### Output
  ### Input tab
  output$str_data <- renderPrint({str(data())})
  ### Radial tab
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
  })
  # Save button (radial)
  observeEvent(input$save, {
    if (is.null(tour_path())) return()
    out <- tour_path()[,, input$basistosave]
    # save(out, file = paste0("tour_basis_", input$basistosave, ".rda")) # .rda file
    write.csv2(out, row.names = FALSE, col.names = FALSE, 
               file = paste0("tour_basis_", input$basistosave, ".csv"))
    output$last_save <- renderTable({ out })
  })
  
  ### Static tab
  observeEvent(input$static_button, {
    output$static_plot <- renderPlot({
      staticProjection(dat = selected_dat(), # defined in app_function.R
                       method = input$static_method, 
                       col = col_var(), 
                       pch = pch_var()
      )
    })
  })
  
  ### glyphmap tab -- Work in progess
  observeEvent(input$SET_OF_INPUTS, { # will need to obs many inputs
    # initialize
    initInput(rv, input)
    output$glyphmap_plot <- renderPlot({
      ## working from vignette example #2:
      
    })
  })
  
  ### Development help -- uncomment message at bottom on ui.R to use
  output$devMessage <- renderPrint({
    numericVars()
    #paste("Development Message: nSelected(): ", head(numVars()))
  })
  
}
shinyApp(ui, server)


##### DEPRICATING OBLIQUE
### Oblique: this gives 1 frame at phi1, cannot change phi1
### but will need to use some of the earlier internal functions to change theta and phi
# flea_std <- tourr::rescale(tourr::flea[,1:6])
# basis <- tourr::basis_random(n = ncol(flea_std))
# manip_var <- 4
# phi1 <- acos(sqrt(basis[manip_var, 1]^2 + basis[manip_var, 2]^2))
# 
# play_manual_tour(data = flea_std, basis = basis, manip_var = 4, 
#                  phi_min = phi1, phi_max = phi1)
#####