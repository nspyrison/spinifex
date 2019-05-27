# Debugging:
# https://shiny.rstudio.com/articles/debugging.html
# Also use browser(), break points, "showcase mode"

source('app_functions.R', local = TRUE)

launchApp <- function(.data = NULL, .basis = NULL) {
  # Initiate reactive values, rv
  rv <- reactiveValues()
  
  ### Default to flea
  if (is.null(.data)) {
    .data <- tourr::flea
  }
  # defaults to populated choice values (before server)
  vars <- names(.data)
  nSelected <- min(ncol(.data), 6)
  source('ui.R', local = TRUE)
  
  server <- function(input, output, session) {
    # update parameters based on default data
    observe(splitInput(.data, rv))
    observe(updateContent(rv, input, output, session))
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
        selected_dat <- rv$d[, which(colnames(rv$d) %in% input$variables)]
        if (input$rescale_data) selected_dat <- tourr::rescale(selected_dat)
        col_var <- rv$groups[, which(colnames(rv$groups) == input$col_var)]
        pch_var <- rv$groups[, which(colnames(rv$groups) == input$pch_var)]
        manip_var <- which(colnames(rv$d) == input$manip_var)
        n <- ncol(selected_dat)

        if (input$basis_init == "Random") .basis <- tourr::basis_random(n = n, d = 2)
        if (input$basis_init == "PCA")    .basis <- prcomp(selected_dat)[[2]][, 1:2]
        if (input$basis_init == "Manual") {
          path <- input$basispath$datapath
          ext <- tolower(substr(path, nchar(path)-4+1, nchar(path)))
          if (ext == ".csv")
            .basis <- read.csv(path, stringsAsFactors = FALSE)
          if (ext == ".rda"){ #load the object, not just name.
            tmp <- new.env()
            load(file = path, envir = tmp)
            .basis <- tmp[[ls(tmp)[1]]]
          }
        }
        
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
    
    ### Development/trouble shooting output: 
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