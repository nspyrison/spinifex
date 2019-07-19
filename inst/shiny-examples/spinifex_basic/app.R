## #' Shiny app for exploring multivariate data, comparing manual tours with 
## #' alternative techniques
## #' 
## #' @author Nicholas Spyrison
## #' @export
## #' @examples \dontrun{
## #' library(spinifex)
## #' launchApp()
## #' }

source('global.R', local = TRUE)

launchApp <- function(.data = NULL, .basis = NULL) {
  # Initiate reactive values, rv
  rv <- reactiveValues()
  source('ui.R', local = TRUE)
  
  server <- function(input, output, session) {
    
    ### Input tab
    observeEvent(input$dat, {
      if (is.null(input$dat)) {return()}
      if (input$dat == "flea") .data <- tourr::flea
      if (input$dat == "olive") .data <- tourr::olive
      if (input$dat == "wine") .data <- as.data.frame(spinifex::wine)
      if (input$dat == "weather") .data <- spinifex::weather
      if (input$dat == "breastcancer") .data <- spinifex::breastcancer
      if (input$dat == "mtcars") .data <- mtcars
      parseData(.data, rv)
      updateParam(rv, input, output, session)
    })
    # Update manip_var 
    observeEvent(input$variables, {
      updateSelectInput(session,
                        "manip_var",
                        choices = input$variables)
    })
    
    ### Radial tour
    observeEvent(input$radial_button, {
      # initialize
      initInput(rv, input)
      
      rv$fullTour <- manual_tour(basis = rv$basis,
                                 manip_var = rv$manip_var,
                                 angle = input$angle
      )
      
      output$plotlyAnim <- renderPlotly({
        play_manual_tour(data = rv$selected_dat,
                         basis = rv$basis,
                         manip_var = rv$manip_var,
                         col = col_of(rv$col_var),
                         pch = pch_of(rv$pch_var),
                         axes = input$axes,
                         angle = input$angle
        )
      })
    })
    # Save button (radial manual)
    observeEvent(input$save, {
      browser()
      if (is.null(rv$fullTour)) return()
      out <- rv$fullTour[,,input$basis2save]
      # save(out, file = paste0("tour_basis_", input$basis2save, ".rda")) # .rda file
      write.csv2(out, row.names = FALSE, col.names = FALSE, 
                 file = paste0("tour_basis_", input$basis2save, ".csv"))
    })
    
    ### Static projections
    observeEvent(input$static_button, {
      # initialize
      initInput(rv, input)
      
      output$static_plot <- renderPlot({
        staticProjection(dat = rv$selected_dat, # defined in app_function.R
                         method = input$static_method, 
                         col = rv$col_var, 
                         pch = rv$pch_var
        )
      })
    })
    
    ### glyphmap tour
    observeEvent(input$SET_OF_INPUTS, { # will need to obs many inputs
      # initialize
      initInput(rv, input)
      output$glyphmap_plot <- renderPlot({
        ## working from vignette example #2:
        
      })
    })
    
    ### Development/troubleshooting output: 
    output$devMessage <- renderPrint({
      paste("input$col_var: ", input$col_var, 
            " col_var column num: ", which(colnames(rv$groups) == input$col_var) )
    })
    output$devMessage2 <- renderPrint({ input$variables })
    
  }
  shinyApp(ui, server)
}

launchApp()