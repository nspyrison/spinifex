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
  rv$curr_basis    <- NULL
  rv$gallery_bases <- NULL
  rv$png_save_cnt  <- 0
  rv$gif_save_cnt  <- 0
  rv$gallery_n_rows <- 0
  rv$tour_array <- NULL
  
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
  output$data_str <- renderPrint({str(data())})
  output$data_summary <- renderPrint({summary(data())})
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
  obl_plot <- reactive({
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
  
  ### x, y, radius reactives
  # x motion 
  basis_x <- reactive({
    .theta <- 0
    mv_sp <- create_manip_space(rv$curr_basis, manip_var())[manip_var(), ]
    phi.x_zero <- atan(mv_sp[3] / mv_sp[1]) - (pi / 2 * sign(mv_sp[1]))
    .phi <- input$x_slider * pi/2 + phi.x_zero
    oblique_basis(basis = rv$curr_basis, manip_var = manip_var(),
                  theta = .theta, phi = .phi)
  })
  # y motion
  basis_y <- reactive({
    .theta <- pi/2
    mv_sp <- create_manip_space(rv$curr_basis, manip_var())[manip_var(), ]
    phi.y_zero <- atan(mv_sp[3] / mv_sp[2]) - (pi / 2 * sign(mv_sp[2]))
    .phi <- input$y_slider * pi/2 + phi.y_zero
    oblique_basis(basis = rv$curr_basis, manip_var = manip_var(),
                  theta = .theta, phi = .phi)
  })
  # Radial motion
  basis_rad <- reactive({
    mv_sp <- create_manip_space(rv$curr_basis, manip_var())[manip_var(), ]
    .theta <- atan(mv_sp[2] / mv_sp[1])
    phi_start <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
    .phi <- (acos(input$rad_slider) - phi_start) * - sign(mv_sp[1])
    oblique_basis(basis = rv$curr_basis, manip_var = manip_var(),
                  theta = .theta, phi = .phi)
  })
  
  ### _Interactive observes ----
  #TODO: Fix sliders not yielding control on re-run
  
  ### Run button, interative initialize
  observeEvent(input$obl_run, {
    #rv$curr_basis <- NULL
    rv$curr_basis <- basis()

    
    ### How to update sliders
    observe({
      mv_sp <- create_manip_space(rv$curr_basis, manip_var())[manip_var(), ]
      phi.x_zero <- atan(mv_sp[3] / mv_sp[1]) - (pi/2*sign(mv_sp[1]))
      x_i <- round(-phi.x_zero / (pi/2), 1)
      (updateSliderInput(session, "x_slider", value = x_i))
      phi.y_zero <- atan(mv_sp[3] / mv_sp[2]) - (pi/2*sign(mv_sp[2]))
      y_i <- round(-phi.y_zero / (pi/2), 1)
      (updateSliderInput(session, "y_slider", value = y_i))
      phi_i <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
      rad_i <- round(cos(phi_i), 1)
      (updateSliderInput(session, "rad_slider", value = rad_i))
    })
    
    ### Observe slider values
    observeEvent(input$x_slider, {
      rv$curr_basis <- basis_x()
    })
    observeEvent(input$y_slider, {
      rv$curr_basis <- basis_y()
    })
    observeEvent(input$rad_slider, {
      rv$curr_basis <- basis_rad()
    })
    
    output$curr_basis_tbl <- renderTable(rv$curr_basis)
    output$obl_plot <- renderPlot({obl_plot()})
  })
  
  
  ### Save current basis (interactive)
  observeEvent(input$obl_save, {
    if (is.null(rv$curr_basis)) return()
    rv$png_save_cnt <- rv$png_save_cnt + 1
    save_file <- sprintf("tour_basis%03d", rv$png_save_cnt)
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
    rv$gallery_n_rows <- rv$gallery_n_rows + 1
    gallery_row <- data.frame(Id = rv$gallery_n_rows,
                           `Manip var`  = input$manip_var, 
                           `Manip type` = input$manip_type, 
                           `Time saved` = substr(Sys.time(), 12,19),
                           check.names = FALSE)
    
    gallery_row$basis <- list(rv$curr_basis)
    rv$gallery_bases <- rbind(rv$gallery_bases, gallery_row)
    
    output$obl_save_msg <- renderPrint(paste0("Basis sent to the gallery."))
  })
  
  
  ##### _Animimation ----
  observeEvent(input$anim_run, {
    ### Processing message for gif
    withProgress(message = 'Rendering animation ...', value = 0, {
      ### Manual tour animation
      app_tour_array <- function(...) { # for code reduction, handle theta.
        rv$tour_array <- manual_tour(basis = basis(), 
                                     data = selected_dat(), 
                                     manip_var = manip_var(),
                                     col = col_of(col_var()), 
                                     pch = pch_of(pch_var()),
                                     axes = input$axes,
                                     angle = input$anim_angle,
                                     alpha = input$alpha,
                                     ...) # Allows theta to vary
      }
      if (input$anim_type %in% c("Radial", "Horizontal", "Vertical")) {
        if (input$anim_type == "Radial")     {rv$tour_array <- app_tour_array()} # default theta to radial
        if (input$anim_type == "Horizontal") {rv$tour_array <- app_tour_array(theta = 0)}
        if (input$anim_type == "Vertical")   {rv$tour_array <- app_tour_array(theta = pi/2)}
      } else {
        # TODO: trouble shoot PP and tourr paths.
        ### Projection pursuit
        if (input$anim_type == "Projection pursuit") {
          tour_func <- getGuidedTour(input$pp_type)
          tour_hist <- save_history(selected_dat(), tour_func)
        }
        ### Grand, little and local tours
        if(input$anim_type == "Grand (8 bases)") {
          rv$tour_array <- save_history(selected_dat(), tour_path = grand_tour(), 
                                        max_bases = 8, angle = input$anim_angle)}
        if(input$anim_type == "Little (8 bases)") {
          rv$tour_array <- save_history(selected_dat(), tour_path = little_tour(), 
                                        max_bases = 8, angle = input$anim_angle)}
        if(input$anim_type == "Local (8 bases)") {
          rv$tour_array <- save_history(selected_dat(), tour_path = local_tour(), 
                                        max_bases = 8, angle = input$anim_angle)}
      }
      
      this_frame <- oblique_frame(basis = rv$tour_array[,, input$anim_slider], 
                                  data = selected_dat(), 
                                  manip_var = manip_var(),
                                  col = col_of(col_var()), 
                                  pch = pch_of(pch_var()),
                                  axes = input$axes,
                                  angle = input$anim_angle,
                                  alpha = input$alpha) 
      
      updateSliderInput(session, "anim_slider", value = 1, max = dim(rv$tour_array)[3])
      rv$curr_basis <- rv$tour_array[,, input$anim_slider]
      
      output$curr_basis_tbl <- renderTable(rv$curr_basis)
      output$obl_plot <- renderPlot(this_frame)
    })
    setProgress(1)
  })
  
  ### Change plot with slider
  observeEvent(input$anim_slider, {
    this_frame <- oblique_frame(basis = rv$tour_array[,, input$anim_slider], 
                                data = selected_dat(), 
                                manip_var = manip_var(),
                                col = col_of(col_var()), 
                                pch = pch_of(pch_var()),
                                axes = input$axes,
                                angle = input$anim_angle,
                                alpha = input$alpha)
    
    rv$curr_basis <- rv$tour_array[,, input$anim_slider]
    output$obl_plot <- renderPlot(this_frame)
  })
  
  ### Save the animation
  observeEvent(input$anim_save, {
    if (input$anim_run == 0) {return()}
    rv$gif_save_cnt <- rv$gif_save_cnt + 1
    
    withProgress(message = 'Rendering animation ...', value = 0, {
      anim <- play_tour_path(tour_path = rv$tour_array,
                             data = selected_dat(),
                             col = col_of(col_var()), pch = pch_of(pch_var()),
                             axes = input$axes,
                             alpha = input$alpha,
                             render_type = render_gganimate)
      save_file <- sprintf("tour_animation%03d.gif", input$anim_save)
      gganimate::anim_save(save_file, anim)
    })
    setProgress(1)
    
    output$anim_save_msg <- renderPrint(paste0("Animation saved as ", save_file, ".", sep=""))
  })
  
  ##### Gallery tab ----
  
  ### Display table with buttons
  rows_to_remove <- reactiveVal()
  gallery_disp <- reactive({ # disp for display table
    if (is.null(rv$gallery_bases)) {
      output$gallery_msg <- renderPrint("Send a basis to the gallery.")
      return()
    }
    
    disp = data.frame(
      Label = shinyInput(textInput, nrow(rv$gallery_bases), 'text_', label = "",
                         placeholder = "user label", width = '108px'),
      Plot = 
        shinyInput(actionButton, nrow(rv$gallery_bases), 'button_', label = "Plot", 
                   onclick = 'Shiny.onInputChange(\"gallery_plot\",  this.id)'),
      `Save (csv & png)` = 
        shinyInput(actionButton, nrow(rv$gallery_bases), 'button_', label = "Save", 
                   onclick = 'Shiny.onInputChange(\"gallery_save\",  this.id)'),
      Delete = 
        shinyInput(actionButton, nrow(rv$gallery_bases), 'button_', label = "Remove", 
                   onclick = 'Shiny.onInputChange(\"gallery_delete\",  this.id)'),
      rv$gallery_bases[, -which(colnames(rv$gallery_bases) == "basis")],
      stringsAsFactors = FALSE,
      row.names = NULL,
      check.names = FALSE
    )
    disp <- disp[!rownames(disp) %in% rows_to_remove(), ]
    
    disp
  })
  
  ### TODO: fix, not working on clayton desktop.
  # output$gallery <- DT::renderDataTable(
  #   rv$gallery_bases[, which(colnames(rv$gallery_bases) == "basis")],
  #   gallery_disp(), server = FALSE, escape = FALSE, selection = 'none',
  #   options = list(dom = 't', pageLength = 100)
  # )
  
  ### TODO: ADD icon ggplot here.
  gallery_icons <- reactive({
    row_info <- rv$gallery_bases[, -which(colnames(rv$gallery_bases) == "basis")]
    bases <- rv$gallery_bases[, which(colnames(rv$gallery_bases) == "basis")]
    
    
    bases_df <- NULL
    for (i in 1:length(bases_ls)){
      this_basis <- bases_ls[[i]]
      this_basis_df <- data.frame(basis_num = i, this_basis)
      bases_df <- cbind(bases_df, this_basis_df)
    }
    colnames(bases_df) <- c("basis_num","X","Y")
    
    ggplot(bases_df) + 
      geom_segment(mapping = aes(x = X, y = Y, xend = 0, yend = 0))
    
  })
  
  
  ### Plot button (gallery)
  observeEvent(input$gallery_plot, {
    selectedRow <- as.numeric(strsplit(input$gallery_plot, "_")[[1]][2])
    rv$curr_basis <- 
      rv$gallery_bases[selectedRow, which(colnames(rv$gallery_bases) == "basis")][[1]]
    output$gallery_msg <- renderText(
      paste0("Row ", selectedRow, " is now the current basis.", sep = ""))
  })
  
  ### Save button (gallery)
  observeEvent(input$gallery_save, {
    rv$png_save_cnt <- rv$png_save_cnt + 1
    selectedRow <- as.numeric(strsplit(input$gallery_save, "_")[[1]][2])
    save_file <- sprintf("tour_basis%03d", rv$png_save_cnt)
    save_basis <- 
      rv$gallery_bases[selectedRow, which(colnames(rv$gallery_bases) == "basis")][[1]]
    
    write.csv(save_basis, file = paste0(save_file, ".csv"), row.names = FALSE)
    gg_out <- oblique_frame(data = selected_dat(),
                            basis = save_basis,
                            manip_var = manip_var(),
                            theta = 0,
                            phi = 0,
                            col = col_of(col_var()),
                            pch = pch_of(pch_var()),
                            axes = input$axes,
                            alpha = input$alpha)
    ggplot2::ggsave(paste0(save_file,".png"), gg_out)
    output$gallery_msg <- renderText(paste0("Saved row ", selectedRow, " as ", save_file, ".", sep="")) 
  })
  
  ### Delete button (gallery)
  observeEvent(input$gallery_delete, {
    selectedRow <- as.numeric(strsplit(input$gallery_delete, "_")[[1]][2])
    rows_to_remove(c(rows_to_remove(), selectedRow))
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
    paste0("Dev msg --\n",
        "obl_run: ", input$obl_run, "\n",
        "obl_save: ", input$obl_save, "\n",
        "anim_run: ", input$anim_run, "\n",
        "anim_save: ", input$anim_save, "\n",
        sep = ""
    )
  })
  
  # ### Create log file.
  # dput(shiny::reactlog(), 
  #      file = paste0("reactlog_", 
  #                    substr(gsub(":", "_", Sys.time()), 1, 16), 
  #                    ".txt")
  # )
}

shinyApp(ui, server)

