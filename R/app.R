source('app_functions.R', local = TRUE)

launchApp <- function(.data = NULL, .basis = NULL) {
  # Init reactive values, rv
  rv <- reactiveValues()
  
  # Default to flea
  if (is.null(.data)) {
    .data <- tourr::flea
    #splitInput(tourr::flea, rv) # requires reactive :(
  }
  vars <- names(.data)
  nSelected <- min(ncol(.data), 6)
  
  source('ui.R', local = TRUE)
  
  
  server <- function(input, output, session) {
    
    # if data changes process it.
    observeEvent(input$file, {
      if (is.null(input$file)) {
        return()
      }
      readInput(input$file, rv, output, session)
      # splitInput(read.csv(input$file$datapath, stringsAsFactors = FALSE), rv)
      # updateCheckboxGroupInput(session,
      #                          "variables",
      #                          choices = names(rv$d),
      #                          selected = names(rv$d[1:rv$nSelected]))
      output$updateButtonPress <- renderText("Data uploaded__________")
    })
    
    # data structure output
    output$str_data <- renderPrint({
      str(.data)
    })
    
    # If button pressed...
    observeEvent(input$updateButton, {
      
      updateCheckboxGroupInput(session,
                               "variables",
                               choices = vars,
                               selected = vars[1:2])
      output$updateButtonPress <- renderText("pressed")
    })
    
    # animation output
    output$plotlyAnim <- renderPlotly({
      # move this to a reactive function? for default, no
      num_data <- rv$d[, which(colnames(rv$d) %in% input$variables)]
      cat_var  <- rv$groupVars[, which(colnames(rv$groupVars) == input$cat_var)]
      n <- ncol(num_data)
      if (input$rescale_data) num_data <- tourr::rescale(num_data)
      if (input$rand_basis) .basis <- tourr::basis_random(n = n, d = 2)
      
      play_manual_tour(
        data = num_data,
        basis = .basis,
        manip_var = which(colnames(.data) == input$manip_var),
        col = cat_var,
        pch = cat_var,
        axes = input$axes,
        angle = input$angle
      )
    })
    
    
  }
  shinyApp(ui, server)
}

launchApp()