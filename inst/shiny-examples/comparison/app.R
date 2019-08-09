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
  observe({
    updateCheckboxGroupInput(session,
                             "variables",
                             choices = names(numericDat()),
                             selected = names(numericDat()[1:colToSelect()]))
  })
  observe({
    updateSelectInput(session,
                      "manip_var",
                      choices = input$variables)
    updateSelectInput(session,
                      "col_var",
                      choices = c(names(groupDat()), "<none>"))
    updateSelectInput(session,
                      "pch_var",
                      choices = c(names(groupDat()), "<none>"))
  })
  
  ### Radial tour animation
  observeEvent(input$button, {
    output$main_plot <- renderPlotly({
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
  
  ##### Interactive tab ----- 
  ### interactve, Initialize and reactives ====
  rv <- reactiveValues() # for current basis
  ## how to update all sliders
  update_sliders <- reactive({
    mv_sp <- create_manip_space(rv$curr_basis, manip_var())[manip_var(), ]
    phi.x_zero <- atan(mv_sp[3] / mv_sp[1]) - (pi/2*sign(mv_sp[1]))
    x_i <- -phi.x_zero / (pi/2)
    isolate(updateSliderInput(session, "x_slider", value = x_i))
    phi.y_zero <- atan(mv_sp[3] / mv_sp[2]) - (pi/2*sign(mv_sp[2]))
    y_i <- -phi.y_zero / (pi/2)
    isolate(updateSliderInput(session, "y_slider", value = y_i))
    phi_i <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
    rad_i <- cos(phi_i)
    isolate(updateSliderInput(session, "rad_slider", value = rad_i))
    # theta_i <- atan(mv_sp[2] / mv_sp[1]) # radians
    # theta_i <- (theta_i * (180 / pi) + 360) %% 360 # positive degrees [0,360]
    # updateSliderInput(session, "theta_slider", value = theta_i)
    
    print("update_sliders:")
    print(paste0("x: ", x_i, ", y: ", y_i, ", rad: ", rad_i))#, ", theta: ", theta_i))
  })
  
  ### x, y, radius, theta reactives for oblique_basis()
  # x motion 
  basis_x <- reactive({
    theta <- 0
    mv_sp <- create_manip_space(rv$curr_basis, manip_var())[manip_var(), ]
    phi.x_zero <- atan(mv_sp[3] / mv_sp[1]) - (pi / 2 * sign(mv_sp[1]))
    phi <- input$x_slider * pi/2 + phi.x_zero
    basis <- oblique_basis(basis = rv$curr_basis,
                           manip_var = manip_var(),
                           theta,
                           phi)
    print(paste0("basis_x, input$x_slider: ", input$x_slider, ", new basis:"))
    print(basis)
    return(basis)
  })
  # y motion
  basis_y <- reactive({
    theta <- pi/2
    mv_sp <- create_manip_space(rv$curr_basis, manip_var())[manip_var(), ]
    phi.y_zero <- atan(mv_sp[3] / mv_sp[2]) - (pi / 2 * sign(mv_sp[2]))
    phi <- input$y_slider * pi/2 + phi.y_zero
    basis <- oblique_basis(basis = rv$curr_basis,
                           manip_var = manip_var(),
                           theta,
                           phi)
    print(paste0("basis_y, input$y_slider: ", input$y_slider, ", new basis:"))
    print(basis)
    return(basis)
  })
  # Radius motion
  basis_rad <- reactive({
    mv_sp <- create_manip_space(rv$curr_basis, manip_var())[manip_var(), ]
    theta <- atan(mv_sp[2] / mv_sp[1])
    phi_start <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
    phi <- acos(input$rad_slider) - phi_start
    basis <- oblique_basis(basis = rv$curr_basis,
                           manip_var = manip_var(),
                           theta,
                           phi)
    print(paste0("basis_rad, input$rad_slider: ", input$rad_slider, ", new basis:"))
    print(basis)
  })
  # #TODO: See zObliquePolarDev.R as well. make
  # # Theta motion
  # basis_theta <- reactive({
  #   mv_sp <- create_manip_space(rv$curr_basis, manip_var())[manip_var(), ]
  #   phi <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
  #   theta_start <- atan(mv_sp[2] / mv_sp[1])
  #   theta <- 0 #TODO: AS A FUNCTION OF SLIDER AND THETA_START
  #   oblique_basis(basis = rv$curr_basis,
  #                 manip_var = manip_var(),
  #                 theta,
  #                 phi)
  # })
  
  ## Interactive, observers + update parameters ====
  ### Running oblique tours 
  observeEvent(input$obl_button, {
    print("obl_button start")
    rv$curr_basis <- basis()
    isolate(update_sliders())
    output$main_plot2 <- renderPlot(
      oblique_frame(data = selected_dat(),
                    basis = rv$curr_basis,
                    manip_var = manip_var(),
                    theta = 0,
                    phi = 0,
                    col = col_of(col_var()),
                    pch = pch_of(pch_var()),
                    axes = input$axes,
                    alpha = input$alpha)
    )
    
    ## also update when changing manip_var, basis_init, or file  
    observe({isolate(update_sliders())})
    
    ## after button, observe sliders
    observeEvent(input$x_slider, {
      rv$curr_basis <- basis_x()
      isolate(update_sliders())
      output$curr_basis_out <- renderTable(rv$curr_basis)
    })
    observeEvent(input$y_slider, {
      rv$curr_basis <- basis_y()
      isolate(update_sliders())
      output$curr_basis_out <- renderTable(rv$curr_basis)
    })
    observeEvent(input$rad_slider, {
      rv$curr_basis <- basis_rad()
      isolate(update_sliders())
      output$curr_basis_out <- renderTable(rv$curr_basis)
    })
    # observeEvent(input$theta_slider, {
    #   rv$curr_basis <- basis_theta()
    #   isolate(update_sliders())
    #   output$curr_basis_out <- renderTable(rv$curr_basis)
    # })
    print("obl_button end")
  })
  
  ### Development help -- uncomment message at bottom on ui.R to use
  output$devMessage <- renderPrint({
    paste0("dev msg -- button: ", input$button, 
           "  obl_button: ", input$obl_button)
    # paste("Development Message: nSelected(): ", head(numVars()))
  })
  
}
shinyApp(ui, server)

