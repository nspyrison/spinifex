# Preamble ------
# options(shiny.error = browser)

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
  # Initialize ----
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
  
  ##### Radial tab ----
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
    if (input$basis_init == "From file") {
      path <- input$basispath$datapath
      ext <- tolower(substr(path, nchar(path)-4+1, nchar(path)))
      if (ext == ".csv") x <- read.csv(path, stringsAsFactors = FALSE)
      if (ext == ".rda"){ # load .rda object, not just name.
        tmp <- new.env()
        load(file = path, envir = tmp)
        x <- tmp[[ls(tmp)[1]]]
      }
    }
    if (input$basis_init == "Projection pursuit") {
      tourFunc <- getGuidedTour(input$pp_type)
      tourHist <- save_history(selected_dat(), tourFunc)
      tourLen <- dim(tourHist)[3]
      x <- matrix(as.numeric(tourHist[,, tourLen]), ncol = 2)
    }
    return(x)
  })
  # used for saving a basis
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
  
  ##### Static tab ----
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
  
  ##### Oblique tab ----
  ### Initialize oblique input 
  obl_manip_var <- reactive(which(colnames(numericDat()) == input$obl_manip_var)) # number
  obl_INIT_basis <- reactive({  # current basis stored in rv$obl_basis
    if (input$obl_basis_init == "Random") x <- tourr::basis_random(n = n(), d = 2)
    if (input$obl_basis_init == "PCA")    x <- prcomp(selected_dat())[[2]][, 1:2]
    if (input$obl_basis_init == "From file") {
      path <- input$basispath$datapath
      ext <- tolower(substr(path, nchar(path)-4+1, nchar(path)))
      if (ext == ".csv") x <- read.csv(path, stringsAsFactors = FALSE)
      if (ext == ".rda"){ # load .rda object, not just name.
        tmp <- new.env()
        load(file = path, envir = tmp)
        x <- tmp[[ls(tmp)[1]]]
      }
    }
    if (input$obl_basis_init == "Projection pursuit") {
      tourFunc <- getGuidedTour(input$pp_type)
      tourHist <- save_history(selected_dat(), tourFunc)
      tourLen <- dim(tourHist)[3]
      x <- matrix(as.numeric(tourHist[,, tourLen]), ncol = 2)
    }
    return(x)
  })
  ## Initialize basis and graph and table 
  rv <- reactiveValues() 
  observeEvent(input$obl_button, {
    rv$obl_basis <- obl_INIT_basis()
    ## update sliders
    mv_sp <- create_manip_space(rv$obl_basis, obl_manip_var())[obl_manip_var(), ]
    phi.x_zero <- atan(mv_sp[3] / mv_sp[1]) - (pi/2*sign(mv_sp[1]))
    phi.y_zero <- atan(mv_sp[3] / mv_sp[2]) - (pi/2*sign(mv_sp[2]))
    x_i <- -phi.x_zero / (pi/2)
    y_i <- -phi.y_zero / (pi/2)
    updateSliderInput(session, "obl_x_slider", value = x_i)
    updateSliderInput(session, "obl_y_slider", value = y_i)
    
    ## after button and init, then observe sliders
    observeEvent(input$obl_x_slider, {
      rv$obl_basis <- obl_basis_x()
      output$obl_basis_out <- renderTable(obl_basis_x())
      output$obl_ggplot_out <- renderPlot(obl_frame_x())
    })
    observeEvent(input$obl_y_slider, {
      rv$obl_basis <- obl_basis_y()
      output$obl_basis_out <- renderTable(obl_basis_y())
      output$obl_ggplot_out <- renderPlot(obl_frame_y())
    })
  })
  
  ### Update oblique inputs
  observe({
    updateSelectInput(session,
                      "obl_manip_var",
                      choices = input$variables)
  })
  
  ##x and y reactives for oblique_frame(), _basis()
  # x motion reactives
  obl_basis_x <- reactive({
    theta <- 0
    mv_sp <- create_manip_space(rv$obl_basis, obl_manip_var())[obl_manip_var(), ]
    phi.x_zero <- atan(mv_sp[3] / mv_sp[1]) - (pi / 2 * sign(mv_sp[1]))
    phi <- input$obl_x_slider * pi/2 + phi.x_zero
    oblique_basis(basis = rv$obl_basis,
                  manip_var = obl_manip_var(),
                  theta,
                  phi)
  })
  obl_frame_x <- reactive({
    theta <- 0
    mv_sp <- create_manip_space(rv$obl_basis, obl_manip_var())[obl_manip_var(), ]
    phi.x_zero <- atan(mv_sp[3] / mv_sp[1]) - (pi / 2 * sign(mv_sp[1]))
    phi <- input$obl_x_slider * pi/2 + phi.x_zero
    oblique_frame(data = selected_dat(),
                  basis = rv$obl_basis,
                  manip_var = obl_manip_var(),
                  theta,
                  phi,
                  col = col_of(col_var()),
                  pch = pch_of(pch_var()),
                  axes = input$obl_axes)
  })
  # y motion reactives
  obl_basis_y <- reactive({
    theta <- pi/2
    mv_sp <- create_manip_space(rv$obl_basis, obl_manip_var())[obl_manip_var(), ]
    phi.y_zero <- atan(mv_sp[3] / mv_sp[2]) - (pi / 2 * sign(mv_sp[2]))
    phi <- input$obl_y_slider * pi/2 + phi.y_zero
    oblique_basis(basis = rv$obl_basis,
                  manip_var = obl_manip_var(),
                  theta,
                  phi)
  })
  obl_frame_y <- reactive({
    theta <- pi/2
    mv_sp <- create_manip_space(rv$obl_basis, obl_manip_var())[obl_manip_var(), ]
    phi.y_zero <- atan(mv_sp[3] / mv_sp[2]) - (pi / 2 * sign(mv_sp[2]))
    phi <- input$obl_y_slider * pi/2 + phi.y_zero
    oblique_frame(data = selected_dat(),
                  basis = rv$obl_basis,
                  manip_var = obl_manip_var(),
                  theta,
                  phi,
                  col = col_of(col_var()),
                  pch = pch_of(pch_var()),
                  axes = input$obl_axes)
  })
  
  ### Development help -- uncomment message at bottom on ui.R to use
  output$devMessage <- renderPrint({
    rv$obl_basis
    #paste("Development Message: nSelected(): ", head(numVars()))
  })
  
}
shinyApp(ui, server)

