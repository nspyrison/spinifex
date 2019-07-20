#' Shiny app for exploring multivariate data, comparing manual tours with 
#' alternative techniques
#' 
#' @author Nicholas Spyrison
#' @export
#' @examples \dontrun{
#' library(spinifex)
#' launchApp("comparison")
#' }

source('global.R', local = TRUE)

launchApp <- function(.data = NULL, .basis = NULL) {
  # Initiate reactive values, rv
  rv <- reactiveValues()
  source('ui.R', local = TRUE)
  
  server <- function(input, output, session) {
    # initialize default: flea data
    if (is.null(.data)) {
      .data <- tourr::flea
    }
    isolate(parseData(.data, rv))
    isolate(updateParam(rv, input, output, session))
    
    ### Input tab
    observeEvent(input$file, {
      if (is.null(input$file)) {return()}
      .data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
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
      
      rv$fullTo <- manual_tour(basis = rv$basis,
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
      if (is.null(rv$fullTour)) return()
      out <- rv$fullTour[,,input$basistosave]
      # save(out, file = paste0("tour_basis_", input$basistosave, ".rda")) # .rda file
      write.csv2(out, row.names = FALSE, col.names = FALSE, 
                 file = paste0("tour_basis_", input$basistosave, ".csv"))
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
      paste("Dev Message: input$col_var: ", input$col_var, 
            " col_var column num: ", which(colnames(rv$groups) == input$col_var) )
    })
    
  }
  shinyApp(ui, server)
}

launchApp()

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