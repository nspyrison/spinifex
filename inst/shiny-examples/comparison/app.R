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
  ##### Initialize ----

  ### Initialize data reactives (global)
  data <- reactive({
    if (is.null(input$data_file)) {return(tourr::flea)}
    read.csv(input$data_file$datapath, stringsAsFactors = FALSE)
  })
  numericVars <- reactive(sapply(data(), is.numeric))
  groupVars   <- reactive(sapply(data(), function(x) {is.character(x)|is.factor(x)}))
  numericDat  <- reactive(data()[numericVars()])
  groupDat    <- reactive(data()[groupVars()])
  colToSelect <- reactive(min(ncol(numericDat()), 6))
  n <- reactive(nrow(selected_dat()))
  p <- reactive(ncol(selected_dat()))
  output$str_data <- renderPrint({str(data())})
  selected_dat <- reactive({
    x <- numericDat()[, which(colnames(numericDat()) %in% input$variables)]
    if (input$rescale_data) x <- tourr::rescale(x)
    return(x)
  })
  col_var <- reactive({
    if (input$col_var == "<none>") {rep("a", n())
    } else {groupDat()[, which(colnames(groupDat()) == input$col_var)]}
  })
  pch_var <- reactive({
    if (input$pch_var == "<none>") {rep("a", n())
    } else {groupDat()[, which(colnames(groupDat()) == input$col_var)]}
  })
  
  ##### Manual tour tab ----
  ### Initialize tour reactives 
  manip_var <- reactive(which(colnames(numericDat()) == input$manip_var)) # number
  basis <- reactive({
    if (input$basis_init == "Random") x <- tourr::basis_random(n = p(), d = 2)
    if (input$basis_init == "PCA")    x <- prcomp(selected_dat())[[2]][, 1:2]
    if (input$basis_init == "From file") {
      path <- input$basis_file$datapath
      ext <- tolower(substr(path, nchar(path)-4+1, nchar(path)))
      if (ext == ".csv") x <- read.csv(path, stringsAsFactors = FALSE)
      if (ext == ".rda"){ # load .rda object, not just name.
        tmp <- new.env()
        load(file = path, envir = tmp)
        x <- tmp[[ls(tmp)[1]]]
      }
    }
    if (input$basis_init == "Projection pursuit") {
      tour_func <- getGuidedTour(input$pp_type)
      tour_hist <- save_history(selected_dat(), tour_func)
      tour_len <- dim(tour_hist)[3]
      x <- matrix(as.numeric(tour_hist[,, tour_len]), ncol = 2)
    }
    return(x)
  })
  
  ### Update tour inputs
  observe({
    updateCheckboxGroupInput(session, "variables",
                             choices = names(numericDat()),
                             selected = names(numericDat()[1:colToSelect()]))
  })
  observe({
    updateSelectInput(session, "manip_var", choices = input$variables)
    cat_names <- c(names(groupDat()), "<none>")
    updateSelectInput(session, "col_var", choices = cat_names)
    updateSelectInput(session, "pch_var", choices = cat_names)
  })
  

  ##### _Interactive specific ----
  ### Interactive uses oblique manipulation, commonly has obl_ prefix.
  rv <- reactiveValues() # reactive values needed for x,y,r sliders and saves
  rv$curr_basis <- NULL
  
  ##### __Initialize reactives (interactive) ----
  ### x, y, radius reactives
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
    
    #TODO: manual logging mirate to proper logging
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
    
    #TODO: manual logging mirate to proper logging
    print(paste0("basis_y, input$y_slider: ", input$y_slider, ", new basis:"))
    print(basis)
    return(basis)
  })
  # Radial motion
  basis_rad <- reactive({
    mv_sp <- create_manip_space(rv$curr_basis, manip_var())[manip_var(), ]
    theta <- atan(mv_sp[2] / mv_sp[1])
    phi_start <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
    phi <- acos(input$rad_slider) - phi_start
    basis <- oblique_basis(basis = rv$curr_basis,
                           manip_var = manip_var(),
                           theta,
                           phi)
    
    #TODO: manual logging mirate to proper logging
    print(paste0("basis_rad, input$rad_slider: ", input$rad_slider, ", new basis:"))
    print(basis)
  })
  
  ### How to update sliders
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
    
    #TODO: manual logging mirate to proper logging
    print("update_sliders:")
    print(paste0("x: ", x_i, ", y: ", y_i, ", rad: ", rad_i))#, ", theta: ", theta_i))
  })

  ### __Update inputs (interactive) ----
  #TODO
  #observe({isolate(update_sliders())})
  ### Display interactive
  observeEvent(input$obl_run, {
    #TODO: manual logging mirate to proper logging
    print("obl_run start")
    rv$curr_basis <- basis()
    isolate(update_sliders())
    output$obl_plot <- renderPlot(
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
    ### After run button, observe sliders
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
    
    #TODO: manual logging mirate to proper logging
    print("obl_run end")
  })
  
  ### Save current basis (interactive)
  observeEvent(input$obl_save, {
    if (is.null(rv$curr_basis)) return()
    save_file <- sprintf("tour_basis%03d", 1)
    write.csv(rv$curr_basis, file = paste0(save_file, ".csv"), row.names = FALSE)
    gg_out <- oblique_frame(data = selected_dat(),
                            basis = rv$curr_basis,
                            manip_var = manip_var(),
                            theta = 0,
                            phi = 0,
                            col = col_of(col_var()),
                            pch = pch_of(pch_var()),
                            axes = input$axes,
                            alpha = input$alpha)
    ggplot2::ggsave(paste0(save_file,".png"), gg_out)
    output$obl_save_msg <- renderPrint(paste0("Basis saved as ", save_file))
  })
  
  ##### _Animimation specific ----
  ### Plot the animation
  observeEvent(input$anim_run, {
    output$anim_plot <- renderPlotly({
      play_radial_tour(selected_dat(), basis(), manip_var(),
                       col = col_of(col_var()), pch = pch_of(pch_var()),
                       axes = input$axes,
                       angle = input$angle,
                       alpha = input$alpha)
    })
  })
  ### Save the animation
  observeEvent(input$anim_save, {
    if (is.null(output$anim_plot)) return()
    output$anim_save_msg <- renderPrint("Saving gif...")
    anim <- play_radial_tour(selected_dat(), basis(), manip_var(),
                             col = col_of(col_var()), pch = pch_of(pch_var()),
                             axes = input$axes,
                             angle = input$angle,
                             alpha = input$alpha,
                             render_type = render_gganimate)
    save_file <- sprintf("tour_animation%03d.gif", 1)
    gganimate::anim_save(save_file, anim)
    output$anim_save_msg <- renderPrint(paste0("Animation saved as ", save_file))
  })
  
  ##### Static tab ----
  observeEvent(input$static_run, {
    output$static_plot <- renderPlot({
      staticProjection(dat = selected_dat(), # legwork is a function in global.R
                       method = input$static_method, 
                       col = col_var(), 
                       pch = pch_var(),
                       alpha = input$static_alpha
      )
    })
  })
  
  ### Development help -- uncomment message at bottom on ui to use
  output$dev_msg <- renderPrint({
    cat("Dev msg --\n",
           "obl_run: ", input$obl_run, "\n",
           "anim_run: ", input$anim_run
           )
  })
  
}
shinyApp(ui, server)

