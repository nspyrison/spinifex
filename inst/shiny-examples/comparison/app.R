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
  groupVars   <- reactive(sapply(data(), function(x) {is.character(x)|is.factor(x)}))
  numericDat  <- reactive(data()[numericVars()])
  groupDat    <- reactive(data()[groupVars()])
  colToSelect <- reactive(min(ncol(numericDat()), 6))
  n <- reactive(nrow(selected_dat()))
  p <- reactive(ncol(selected_dat()))
  output$str_data <- renderPrint({str(data())})
  
  ##### Animation tab (radial) ----
  ### Initialize input 
  selected_dat <- reactive({
    x <- numericDat()[, which(colnames(numericDat()) %in% input$variables)]
    if (input$rescale_data) x <- tourr::rescale(x)
    return(x)
  })
  col_var <- reactive({
    if (input$col_var == "<none>") {rep("a", n())
    } else {groupDat()[, which(colnames(groupDat()) == input$col_var)]} # a column
  })
  pch_var <- reactive({
    if (input$pch_var == "<none>") {rep("a", n())
    } else {groupDat()[, which(colnames(groupDat()) == input$pch_var)]} # a column
  })
  manip_var <- reactive(which(colnames(numericDat()) == input$manip_var)) # number
  basis <- reactive({
    if (input$basis_init == "Random") x <- tourr::basis_random(n = p(), d = 2)
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
  observeEvent(input$radial_button,{
    updateCheckboxGroupInput(session,
                             "variables",
                             choices = names(numericDat()),
                             selected = names(numericDat()[1:colToSelect()])
    )
  })
  observe({
    updateSelectInput(session,
                      "manip_var",
                      choices = input$variables)
    
    if (length(groupDat()) >= 1) {
      updateSelectInput(session,
                        "col_var",
                        choices = c(names(groupDat()), "<none>"))
      updateSelectInput(session,
                        "pch_var",
                        choices = c(names(groupDat()), "<none>"))
    } else { # list "none", if there are not character or factor vars.
      updateSelectInput(session,
                        "col_var",
                        choices = c("<none>"))
      updateSelectInput(session,
                        "pch_var",
                        choices = c("<none>"))
    }
  })
  
  ### Radial tour animation
  observeEvent(input$radial_button, {
    output$plotlyAnim <- renderPlotly({
      play_radial_tour(data = selected_dat(),
                       basis = basis(),
                       manip_var = manip_var(),
                       col = col_of(col_var()),
                       pch = pch_of(pch_var()),
                       axes = input$axes,
                       angle = input$angle,
                       alpha = input$alpha
      )
    })
  })
  # Save button (radial)
  observeEvent(input$radial_save, {
    if (is.null(tour_path())) return()
    out <- tour_path()[,, input$basistosave] # no variable or column names
    # save(out, file = paste0("tour_basis_", input$basistosave, ".rda")) # .rda file
    write.csv2(out, row.names = FALSE, col.names = FALSE, 
               file = paste0("tour_basis_", input$basistosave, ".csv"))
    output$last_save <- renderTable(out)
  })
  ##### End of radial tab
  
  ##### Static tab ----
  observeEvent(input$static_button, {
    output$static_plot <- renderPlot({
      staticProjection(dat = selected_dat(), # legwork is a function in global.R
                       method = input$static_method, 
                       col = col_var(), 
                       pch = pch_var(),
                       alpha = input$static_alpha
      )
    })
  })
  ##### End of static tab
  
  ##### Interactive tab (oblique) ----
  ### Initialize oblique input 
  obl_manip_var <- reactive(which(colnames(numericDat()) == input$obl_manip_var)) # number
  obl_INIT_basis <- reactive({  # current basis stored in rv$obl_basis
    if (input$obl_basis_init == "Random") x <- tourr::basis_random(n = p(), d = 2)
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
  ## Initialize basis and graph
  rv <- reactiveValues() 
  observeEvent(input$obl_button, {
    rv$obl_basis <- obl_INIT_basis()
    update_sliders()
    ## also update when changing manip_var, basis_init, or file  
    observeEvent(c(input$obl_manip_var, input$obl_basis_init, input$file) , {
      update_sliders()
    })
    
    ## after button, observe sliders
    observeEvent(input$obl_x_slider, {
      rv$obl_basis <- obl_basis_x()
      update_sliders()
      output$obl_basis_out <- renderTable(obl_basis_x())
      output$obl_ggplot_out <- renderPlot(obl_frame_x())
    })
    observeEvent(input$obl_y_slider, {
      rv$obl_basis <- obl_basis_y()
      update_sliders()
      output$obl_basis_out <- renderTable(obl_basis_y())
      output$obl_ggplot_out <- renderPlot(obl_frame_y())
    })
    # observeEvent(input$obl_rad_slider, {
    #   rv$obl_basis <- obl_basis_rad()
    #   isolate(update_sliders())
    #   output$obl_basis_out <- renderTable(obl_basis_rad())
    #   output$obl_ggplot_out <- renderPlot(obl_frame_rad())
    # })
    observeEvent(input$obl_theta_slider, {
      rv$obl_basis <- obl_basis_theta()
      #updateSliders()
      output$obl_basis_out <- renderTable(obl_basis_theta())
      output$obl_ggplot_out <- renderPlot(obl_frame_theta())
    })
  })
  
  ### Update oblique inputs
  observe({
    updateSelectInput(session, "obl_manip_var", choices = input$variables)
  })
  
  ## after button, update sliders reactive expression
  update_sliders <- reactive({
    mv_sp <- create_manip_space(rv$obl_basis, obl_manip_var())[obl_manip_var(), ]
    phi.x_zero <- atan(mv_sp[3] / mv_sp[1]) - (pi/2*sign(mv_sp[1]))
    phi.y_zero <- atan(mv_sp[3] / mv_sp[2]) - (pi/2*sign(mv_sp[2]))
    x_i <- -phi.x_zero / (pi/2)
    y_i <- -phi.y_zero / (pi/2)
    updateSliderInput(session, "obl_x_slider", value = x_i)
    updateSliderInput(session, "obl_y_slider", value = y_i)
    phi_i <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2)) ##issue here, acos(1.136, is NaN)
    rad_i <- cos(phi_i)
    updateSliderInput(session, "obl_rad_slider", value = rad_i)
    theta_i <- atan(mv_sp[2] / mv_sp[1]) # radians
    theta_i <- (theta_i * (180 / pi) + 360) %% 360 # positive degrees [0,360]
    updateSliderInput(session, "obl_theta_slider", value = theta_i)
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
                  axes = input$obl_axes,
                  alpha = input$obl_alpha)
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
                  axes = input$obl_axes,
                  alpha = input$obl_alpha)
  })
  ## Radius motion reactives
  obl_basis_rad <- reactive({
    mv_sp <- create_manip_space(rv$obl_basis, obl_manip_var())[obl_manip_var(), ]
    theta <- atan(mv_sp[2] / mv_sp[1])
    phi_start <- acos(sqrt(mv_sp[1]^2 + mv_sp[1]^2))
    phi <- acos(input$obl_rad_slider) - phi_start
    oblique_basis(basis = rv$obl_basis,
                  manip_var = obl_manip_var(),
                  theta,
                  phi)
  })
  obl_frame_rad <- reactive({
    mv_sp <- create_manip_space(rv$obl_basis, obl_manip_var())[obl_manip_var(), ]
    theta <- atan(mv_sp[2] / mv_sp[1])
    phi_start <- acos(sqrt(mv_sp[1]^2 + mv_sp[1]^2))
    phi <- acos(input$obl_rad_slider) - phi_start
    oblique_frame(data = selected_dat(),
                  basis = rv$obl_basis,
                  manip_var = obl_manip_var(),
                  theta,
                  phi,
                  col = col_of(col_var()),
                  pch = pch_of(pch_var()),
                  axes = input$obl_axes,
                  alpha = input$obl_alpha)
  })
  #TODO: See zObliquePolarDev.R as well. make
  # Theta motion reactives
  obl_basis_theta <- reactive({
    mv_sp <- create_manip_space(rv$obl_basis, obl_manip_var())[obl_manip_var(), ]
    phi <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
    theta_start <- atan(mv_sp[2] / mv_sp[1])
    theta <- 0 #TODO: AS A FUNCTION OF SLIDER AND THETA_START
    oblique_basis(basis = rv$obl_basis,
                  manip_var = obl_manip_var(),
                  theta,
                  phi)
  })
  #TODO: change below to theta
  obl_frame_theta <- reactive({
    mv_sp <- create_manip_space(rv$obl_basis, obl_manip_var())[obl_manip_var(), ]
    phi <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
    theta_start <- atan(mv_sp[2] / mv_sp[1])
    theta <- 0 #TODO: AS A FUNCTION OF SLIDER AND THETA_START
    oblique_frame(data = selected_dat(),
                  basis = rv$obl_basis,
                  manip_var = obl_manip_var(),
                  theta,
                  phi,
                  col = col_of(col_var()),
                  pch = pch_of(pch_var()),
                  axes = input$obl_axes,
                  alpha = input$obl_alpha)
  })
  
  ### Development help -- uncomment message at bottom on ui.R to use
  output$devMessage <- renderPrint({
    paste0("dev msg -- obl_rad_slider: ", input$obl_rad_slider, 
           "  obl_theta_slider: ", input$obl_theta_slider)
    # paste("Development Message: nSelected(): ", head(numVars()))
  })
  
}
shinyApp(ui, server)

