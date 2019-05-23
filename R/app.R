# Debugging:
# https://shiny.rstudio.com/articles/debugging.html
# Also use browser(), break points, "showcase mode"

source('app_functions.R', local = TRUE)

launchApp <- function(.data = NULL, .basis = NULL) {
  # Initiate reactive values, rv
  rv <- reactiveValues()
  
  # Default to flea
  if (is.null(.data)) {
    .data <- tourr::flea
    #TODO: doesn't update the parameter selections as it should.
    observe(splitInput(.data, rv))
    observe(updateContent(rv, input, output, session))
  }
  vars <- names(.data)
  nSelected <- min(ncol(.data), 6)
  source('ui.R', local = TRUE)
  
  server <- function(input, output, session) {
    # data structure output init
    output$str_data <- renderPrint({str(.data)})
    
    # if data changes process it.
    observeEvent(input$file, {
      if (is.null(input$file)) {return()}
      readInput(input$file, rv, input, output, session)
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
        selected_dat <- rv$d[, which(colnames(rv$d) %in% input$variables)]
        col_var <- rv$groups[, which(colnames(rv$groups) == input$col_var)]
        pch_var <- rv$groups[, which(colnames(rv$groups) == input$pch_var)]
        manip_var <- which(colnames(rv$d) == input$manip_var)
        n <- ncol(selected_dat)
        if (input$init_func == "Random") .basis <- tourr::basis_random(n = n, d = 2)
        if (input$init_func == "PCA")    .basis <- prcomp(selected_dat)[[2]][, 1:2]
        if (input$init_func == "manual") .basis <- input$basis
        if (input$rescale_data) selected_dat <- tourr::rescale(selected_dat)
        
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
    
    # Development/trouble shooting output: 
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
#shiny::runApp(display.mode="showcase")