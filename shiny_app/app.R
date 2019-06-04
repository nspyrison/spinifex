source('app_functions.R', local = TRUE)

launchApp <- function(.data = NULL, .basis = NULL) {
  # Initiate reactive values, rv
  rv <- reactiveValues()
  source('ui.R', local = TRUE)
  
  server <- function(input, output, session) {
    #default to flea
    if (is.null(.data)) {
      .data <- tourr::flea
      observe(splitInput(.data, rv))
      observe(updateContent(rv, input, output, session))
    }
    
    # if data changes process it
    observeEvent(input$file, {
      if (is.null(input$file)) {return()}
      readInput(input$file, rv, input, output, session)
    })
    # Update manip_var dropdown if vars selected changes
    observeEvent(input$variables, {
      updateSelectInput(session,
                        "manip_var",
                        choices = input$variables)
    })
    output$str_data <- renderPrint({str(.data)})
    
    ### Radial tour
    observeEvent(input$radial_button, {
      output$plotlyAnim <- renderPlotly({

        
        play_manual_tour(
          data = selected_dat,
          basis = .basis,
          manip_var = manip_var,
          col = col_of(col_var),
          pch = pch_of(pch_var),
          axes = input$axes,
          angle = input$angle
        )
      })
    })
    
    ### Static projections
    observeEvent(input$static_button, {
      output$static_plot <- renderPlot({
        # Init
        selected_dat <- rv$d[, which(colnames(rv$d) %in% input$variables)]
        if (input$rescale_data) selected_dat <- tourr::rescale(selected_dat)
        col_var <- rv$groups[, which(colnames(rv$groups) == input$col_var)]
        pch_var <- rv$groups[, which(colnames(rv$groups) == input$pch_var)]
        
        staticProjection(dat = selected_dat, 
                         method = input$static_tech, 
                         col = col_var, 
                         pch = pch_var
        )
      })
    })
    
    ### Oblique tour
    observeEvent(input$SET_OF_INPUTS, { # will need to obs many inputs
      output$oblique_plot <- renderPlot({
        ###TODO: this gives 1 frame at phi1, cannot change phi1
        ###but will need to use some of the earlier internal functions to change theta and phi
        # flea_std <- tourr::rescale(tourr::flea[,1:6])
        # basis <- tourr::basis_random(n = ncol(flea_std))
        # manip_var <- 4
        # phi1 <- acos(sqrt(basis[manip_var, 1]^2 + basis[manip_var, 2]^2))
        # 
        # play_manual_tour(data = flea_std, basis = basis, manip_var = 4, 
        #                  phi_min = phi1, phi_max = phi1)
      })
    })
    
    ### Development/troubleshooting output: 
    output$devMessage <- renderPrint({
      paste("input$col_var: ", input$col_var, 
            " col_var column num: ", which(colnames(rv$groups) == input$col_var) )
    })
    output$devMessage2 <- renderPrint({ input$init_func })
    output$devMessage3 <- renderPrint({ input$variables })
    
  }
  shinyApp(ui, server)
}

launchApp()
# shiny::runApp("shiny_app", display.mode="showcase")