source('app_functions.R', local = TRUE)

launchApp <- function(.data = NULL, .basis = NULL) {
  # Init reactive values, rv
  rv <- reactiveValues()
  
  # Default to flea
  if (is.null(.data)) {
    .data <- tourr::flea
    reactive(splitInput(tourr::flea, rv))
    reactive(updateContent(rv, input, output, session))
  }
  vars <- names(.data)
  nSelected <- min(ncol(.data), 6)
  
  source('ui.R', local = TRUE)
  
  
  server <- function(input, output, session) {
    
    # data structure output init
    output$str_data <- renderPrint({str(.data)})
    
    # if data changes process it.
    observeEvent(input$file, {
      if (is.null(input$file)) {
        return()
      }
      readInput(input$file, rv, input, output, session)
      #output$updateButtonPress <- renderText("Data uploaded__________")
    })
    
    # Updates manip_var dropdown
    observeEvent(input$variables, {
    updateSelectInput(session,
                      "manip_var",
                      choices = input$variables)
    })
    
    # If genrate button pressed, make tour
    observeEvent(input$generate, {
      output$generate <- renderText("Generating tour...")
      
      # tour animation
      output$plotlyAnim <- renderPlotly({
        selected_vars <- rv$d[, which(colnames(rv$d) %in% input$variables)]
        #TODO: fix cat_var. even calulating cat_var causes error..
        #cat_var <- rv$groupVars[, which(colnames(rv$groupVars) == input$cat_var)]
        manip_var <- which(colnames(rv$d) == input$manip_var)
        n <- ncol(selected_vars)
        if (input$rescale_data) selected_vars <- tourr::rescale(selected_vars)
        if (input$rand_basis) .basis <- tourr::basis_random(n = n, d = 2)
        
        play_manual_tour(
          data = selected_vars,
          basis = .basis,
          manip_var = manip_var,
          #col = cat_var,
          #pch = cat_var,
          axes = input$axes,
          angle = input$angle
        )
      })
    })
    
    #TODO: Trouble shooting output: 
    output$cat_var_decode <- renderPrint({ cat_var <- rv$groupVars[, which(colnames(rv$groupVars) == input$cat_var)]})
    output$cat_var <- renderPrint({ input$cat_var })
    
    output$variables <- renderPrint({ input$variables })
  }
  shinyApp(ui, server)
}

launchApp()