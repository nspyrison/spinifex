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
  output$str_data <- renderPrint({str(data())})
  
  ##### Radial tab
  ### Initialize input 
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
  # for saving a basis
  tour_path <- reactive({manual_tour(basis = basis(),
                                     manip_var = manip_var(),
                                     angle = input$angle)
  })
  
  ### Update inputs
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
  
  ### Radial tour animation
  observeEvent(input$radial_button, {
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
  observeEvent(input$radial_save, {
    if (is.null(tour_path())) return()
    out <- tour_path()[,, input$basistosave]
    # save(out, file = paste0("tour_basis_", input$basistosave, ".rda")) # .rda file
    write.csv2(out, row.names = FALSE, col.names = FALSE, 
               file = paste0("tour_basis_", input$basistosave, ".csv"))
    output$last_save <- renderTable(out)
  })
  ##### End of radial tab
  
  ##### Static tab
  observeEvent(input$static_button, {
    output$static_plot <- renderPlot({
      staticProjection(dat = selected_dat(),
                       method = input$static_method, 
                       col = col_var(), 
                       pch = pch_var()
      )
    })
  })
  ##### End of static tab
  
  ##### Oblique tab
  ### Initialize oblique input 
  obl_manip_var <- reactive(which(colnames(numericDat()) == input$obl_manip_var)) # number
  obl_basis <- reactive({
    if (input$obl_basis_init == "Random") x <- tourr::basis_random(n = n(), d = 2)
    if (input$obl_basis_init == "PCA")    x <- prcomp(selected_dat())[[2]][, 1:2]
    return(x)
  })
  
  ### Update oblique inputs
  observe({
    updateSelectInput(session,
                      "obl_manip_var",
                      choices = input$variables)
  })
  
  ### Oblique output
  observeEvent(input$obl_button, {
    # Basis
    output$obl_basis_out <- renderTable(
      oblique_basis(basis = obl_basis(),
                    manip_var = obl_manip_var(), 
                    theta = NULL, #TODO: fix phi and theta #obl_x_slider
                    phi = NULL))
    # Frame
    output$obl_plotlyAnim <- renderPlot({
      oblique_frame(data = selected_dat(),
                    basis = obl_basis(), 
                    manip_var = obl_manip_var(),
                    theta = NULL, #TODO: fix phi and theta #obl_x_slider
                    phi = NULL,
                    col = col_of(col_var()),
                    pch = pch_of(pch_var()),
                    axes = input$axes)
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