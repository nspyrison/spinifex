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
  rv <- reactiveValues() # rv needed for x,y,r sliders and gallery
  rv$curr_basis <- NULL
  rv$gallery_bases <- NULL
  rv$gallery_n_saved <- 0
  
  obl_save_cnt <- reactive({
    if (is.null(input$olb_save)) {return(rv$gallery_n_saved) # NULL is 0
      } else {input$olb_save + rv$gallery_n_saved} 
    }) 
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
  
  
  ##### _Interactive reactives ----
  ### Interactive uses oblique manipulation, commonly has obl_ prefix.
  
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
    
    #TODO: manual logging migrate to proper logging
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
    
    #TODO: manual logging migrate to proper logging
    print(paste0("basis_y, input$y_slider: ", input$y_slider, ", new basis:"))
    print(basis)
    return(basis)
  })
  # Radial motion
  basis_rad <- reactive({
    mv_sp <- create_manip_space(rv$curr_basis, manip_var())[manip_var(), ]
    theta <- atan(mv_sp[2] / mv_sp[1])
    phi_start <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
    phi <- (acos(input$rad_slider) - phi_start) * -sign(mv_sp[1])
    basis <- oblique_basis(basis = rv$curr_basis,
                           manip_var = manip_var(),
                           theta,
                           phi)
    
    #TODO: manual logging migrate to proper logging
    print(paste0("basis_rad, input$rad_slider: ", input$rad_slider, ", new basis:"))
    print(basis)
  })
  
  ### How to update sliders
  observe({
    if (is.null(rv$curr_basis)) return()
    mv_sp <- create_manip_space(rv$curr_basis, manip_var())[manip_var(), ]
    phi.x_zero <- atan(mv_sp[3] / mv_sp[1]) - (pi/2*sign(mv_sp[1]))
    x_i <- -phi.x_zero / (pi/2)
    (updateSliderInput(session, "x_slider", value = x_i))
    phi.y_zero <- atan(mv_sp[3] / mv_sp[2]) - (pi/2*sign(mv_sp[2]))
    y_i <- -phi.y_zero / (pi/2)
    (updateSliderInput(session, "y_slider", value = y_i))
    phi_i <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
    rad_i <- cos(phi_i)
    (updateSliderInput(session, "rad_slider", value = rad_i))
    
    #TODO: manual logging migrate to proper logging
    # print("update_sliders:")
    # print(paste0("x: ", x_i, ", y: ", y_i, ", rad: ", rad_i))#, ", theta: ", theta_i))
  })
  
  ### _Interactive observes ----
  #TODO: Fix 
  
  ### Display interactive
  observeEvent(input$obl_run, {
    #TODO: manual logging migrate to proper logging
    print("obl_run start")
    rv$curr_basis <- basis()
    output$obl_plot <- renderPlot({
      oblique_frame(data = selected_dat(),
                    basis = rv$curr_basis,
                    manip_var = manip_var(),
                    theta = 0,
                    phi = 0,
                    col = col_of(col_var()),
                    pch = pch_of(pch_var()),
                    axes = input$axes,
                    alpha = input$alpha)
    })
    
    ### After run button, observe sliders
    observeEvent(input$x_slider, {
      rv$curr_basis <- basis_x()
      output$curr_basis_out <- renderTable(rv$curr_basis)
    })
    observeEvent(input$y_slider, {
      rv$curr_basis <- basis_y()
      output$curr_basis_out <- renderTable(rv$curr_basis)
    })
    observeEvent(input$rad_slider, {
      rv$curr_basis <- basis_rad()
      output$curr_basis_out <- renderTable(rv$curr_basis)
    })
    
    #TODO: manual logging migrate to proper logging
    print("obl_run end")
  })
  
  ### Save current basis (interactive)
  observeEvent(input$obl_save, {
    if (is.null(rv$curr_basis)) return()
    save_file <- sprintf("tour_basis%03d", obl_save_cnt())
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
    output$obl_save_msg <- renderPrint(paste0("Basis saved as ", save_file, "."))
  })
  
  ### Send current basis to gallery
  observeEvent(input$obl_to_gallery, {
    if (is.null(rv$curr_basis)) return()
    gallery_info <- data.frame(`Manip var`  = input$manip_var, 
                               `Manip type` = input$manip_type, 
                               `Time saved` = substr(Sys.time(), 12,19))
    basis_row <- t(data.frame(as.vector(rv$curr_basis)))
    colnames(basis_row) <- c(paste0("x", 1:p()), paste0("y", 1:p()))
    
    gallery_row <- data.frame(cbind(gallery_info, basis_row))
    rv$gallery_bases <- rbind(rv$gallery_bases, gallery_row)
    
    output$obl_save_msg <- renderPrint(paste0("Basis sent to the gallery."))
  })
  
  
  ##### _Animimation ----
  ### Plot the animation
  observeEvent(input$anim_run, {
    output$anim_plot <- renderPlotly({
      play_manual_tour(selected_dat(), basis(), manip_var(),
                       col = col_of(col_var()), pch = pch_of(pch_var()),
                       axes = input$axes,
                       angle = input$angle,
                       alpha = input$alpha)
    })
  })
  
  ### Save the animation
  #TODO: saving/generating gif message fix.
  observeEvent(input$anim_save, {
    if (input$anim_run == 0) {
      output$anim_save_msg <- renderPrint("Saving gif...") #TODO fix this
      return()
      }
    output$anim_save_msg <- renderPrint("Saving gif...")
    anim <- play_manual_tour(selectned_dat(), basis(), manip_var(),
                             col = col_of(col_var()), pch = pch_of(pch_var()),
                             axes = input$axes,
                             angle = input$angle,
                             alpha = input$alpha,
                             render_type = render_gganimate)
    save_file <- sprintf("tour_animation%03d.gif", input$anim_save)
    gganimate::anim_save(save_file, anim)
    output$anim_save_msg <- renderPrint(paste0("Animation saved as ", save_file, "."))
  })
  
  ##### Gallery tab ----
  
  ### Display table with buttons
  rows_to_remove <- reactiveVal()
  gallery_disp <- reactive({
    if (is.null(rv$gallery_bases)) {
      output$gallery_msg <- renderPrint("Send a basis to the gallery.")
      return()
    }
    df = data.frame(
      Plot = shinyInput(actionButton, nrow(rv$gallery_bases), 'button_', label = "Plot", 
                        onclick = 'Shiny.onInputChange(\"gallery_plot\",  this.id)'),
      `Save (csv & png)` = shinyInput(actionButton, nrow(rv$gallery_bases), 'button_', label = "SaveToFile", 
                              onclick = 'Shiny.onInputChange(\"gallery_save\",  this.id)'),
      Delete = shinyInput(actionButton, nrow(rv$gallery_bases), 'button_', label = "Remove", 
                          onclick = 'Shiny.onInputChange(\"gallery_delete\",  this.id)'),
      as.data.frame(rv$gallery_bases),
      stringsAsFactors = FALSE,
      row.names = 1:nrow(rv$gallery_bases)
    )
    df[!rownames(df) %in% rows_to_remove(), ]
  })
  
  output$gallery <- DT::renderDataTable(
    gallery_disp(), server = FALSE, escape = FALSE, selection = 'none'
  )
  
  ### Plot button (gallery)
  observeEvent(input$gallery_plot, {

    selectedRow <- as.numeric(strsplit(input$gallery_plot, "_")[[1]][2])
    gallery_basis_v <- rv$gallery_bases[selectedRow, 4:(4 + 2*p() - 1)]
    
    rv$curr_basis <- matrix(gallery_basis_v, ncol=2, byrow = F)
    output$gallery_msg <- renderText(paste0("Row ", selectedRow, " is now the current basis."))
  })
  
  ### Save button (gallery)
  observeEvent(input$gallery_save, {
    selectedRow <- as.numeric(strsplit(input$gallery_save, "_")[[1]][2])
    rv$gallery_n_saved <- rv$gallery_n_saved + 1
    save_file <- sprintf("tour_basis%03d", obl_save_cnt())
    gallery_basis_v <- rv$gallery_bases[selectedRow, 4:(4 + 2*p() - 1)]
    gallery_basis <- matrix(gallery_basis_v, ncol=2, byrow = F)
    
    write.csv(gallery_basis, file = paste0(save_file, ".csv"), row.names = FALSE)
    gg_out <- oblique_frame(data = selected_dat(),
                            basis = gallery_basis,
                            manip_var = manip_var(),
                            theta = 0,
                            phi = 0,
                            col = col_of(col_var()),
                            pch = pch_of(pch_var()),
                            axes = input$axes,
                            alpha = input$alpha)
    ggplot2::ggsave(paste0(save_file,".png"), gg_out)
    output$gallery_msg <- renderText(paste0("Saved row ", selectedRow, " as ", save_file, ".")) 
  })
  
  ### Delete button (gallery)
  observeEvent(input$gallery_delete, {
    selectedRow <- as.numeric(strsplit(input$gallery_delete, "_")[[1]][2])
    rows_to_remove(c(rows_to_remove(), selectedRow)) # updates the rows to remove
  })
  
  ##### Static tab ----
  observeEvent(input$static_run, {
    output$static_plot <- renderPlot({
      staticProjection(dat = selected_dat(),
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
        "obl_save: ", input$obl_save, "\n",
        "anim_run: ", input$anim_run, "\n",
        "anim_save: ", input$anim_save, "\n"
    )
  })
  
  ### Create log file.
  dput(shiny::reactlog(), 
       file = paste0("reactlog_", substr(Sys.time(), 1, 16), ".txt")
  )
}
shinyApp(ui, server)

