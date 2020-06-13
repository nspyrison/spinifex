### "Primary" app.R -----
# options(shiny.error = FALSE)

#' Shiny app for exploring multivariate data, comparing manual tours with 
#' alternative techniques
#' 
#' @author Nicholas Spyrison
#' @export
#' @examples \dontrun{
#' spinifex::run_app("primary")
#' }

#TODO: Go to the other logging method. see spinifex_study app

source('../global_shinyApps.r', local = TRUE)
source('ui.R', local = TRUE)

server <- function(input, output, session) {
  ##### Init reactiveValues
  rv                      <- reactiveValues()
  rv$curr_basis           <- tourr::basis_random(6)
  rv$tour_array           <- NULL
  rv$anim_playing         <- FALSE
  rv$anim_slide           <- 1
  rv$png_save_cnt         <- 0
  rv$gif_save_cnt         <- 0
  rv$gallery_bases        <- NULL
  rv$gallery_n_rows       <- 0
  rv$gallery_rows_removed <- NULL
  
  ### Data reactives -----
  rawDat <- reactive({
    if (is.null(input$data_file)) {return(tourr::flea)}
    read.csv(input$data_file$datapath, stringsAsFactors = FALSE)
  })
  selDat <- reactive({
    dat <- rawDat()
    ret <- dat[, which(colnames(dat) %in% input$projVars)]
    if (input$rescale_data) ret <- tourr::rescale(ret)
    return(as.data.frame(ret))
  })
  
  n <- reactive(nrow(selDat()))
  p <- reactive(ncol(selDat()))
  numericVars_TF <- reactive({ ## Columns that are numeric AND column-complete
    sapply(rawDat(), function(x) {
      is.numeric(x) & all(complete.cases(x))
    })
  }) 
  clusterVars_TF <- reactive({ ## Columns that are (character OR factor) AND column-complete
    sapply(rawDat(), function(x) {
      (is.character(x)|is.factor(x)) & all(complete.cases(x))
    })
  })

  ## Throttle manip_slider, tries to return value while held
  manip_slider   <- reactive(input$manip_slider)
  manip_slider_t <- throttle(manip_slider, 50) ## Throttle time in milliseconds

  #### Aesthetics
  sel_col <- reactive({
    var_nm <- input$col_var_nm
    if (is.null(var_nm) | length(var_nm) == 0) var_nm <- "<none>"
    if (var_nm == "<none>") {
      var <- rep("a", n())
    } else {
      dat <- rawDat()
      var <- dat[, which(colnames(dat) == var_nm)]
    }
    col_of(var) ## A column of hexidecmal color code strings
  })
  sel_pch <- reactive({
    var_nm <- input$pch_var_nm
    if (is.null(var_nm) | length(var_nm) == 0) var_nm <- "<none>"
    if (var_nm == "<none>") {
      var <- rep("a", n())
    } else {
      dat <- rawDat()
      var <- dat[, which(colnames(dat) == var_nm)]
    }
    pch_of(var) ## A column of integers ,the 'pch' of the data point
  })
  
  ## Add col and row names  to the current basis
  format_curr_basis <- reactive({
    function(basis){
      colnames(basis) <- c("x", "y")
      row.names(basis) <- colnames(selDat())
      rv$curr_basis <- basis
      return(basis)
    }
  })
  
  ## Tour args:
  basis <- reactive({
    if (input$basis_init == "Random") ret <- tourr::basis_random(n = p(), d = 2)
    if (input$basis_init == "PCA")    ret <- prcomp(selDat())[[2]][, 1:2]
    if (input$basis_init == "From file") {
      ##TODO: trouble shoot
      browser()
      path <- input$basis_file$datapath
      ext <- tolower(substr(path, nchar(path)-4+1, nchar(path)))
      if (ext == ".csv") x <- read.csv(path, stringsAsFactors = FALSE)
      if (ext == ".rda"){ # load .rda object, not just name.
        tmp <- new.env()
        load(file = path, envir = tmp)
        ret <- tmp[[ls(tmp)[1]]]
      }
    }
    if (input$basis_init == "Projection pursuit") {
      pp_cluster <- NA

      if (input$pp_type %in% c("lda_pp", "pda_pp")){
        culster_dat <- rawDat()[clusterVars_TF()]
        pp_cluster <- culster_dat[input$pp_cluster]
      }
      tour_func <- getGuidedTour(input$pp_type, pp_cluster)
      tour_hist <- save_history(selDat(), tour_func)
      tour_len  <- dim(tour_hist)[3]
      ret <- matrix(as.numeric(tour_hist[,, tour_len]), ncol = 2)
    }
    colnames(ret) <- c("x", "y")
    row.names(ret) <- colnames(selDat())
    return(ret)
  })
  
  manip_var_num <- reactive({
    m_var <- input$manip_var_nm
    if (m_var == "<none>") {return(1)}
    if (input$manual_method == "animation") {return(1)}
    
    num_dat <- rawDat()[numericVars_TF()]
    which(colnames(num_dat) == m_var)
  }) 
  
  ### Interactive and animated plot
  main_plot <- reactive({
    if (input$manual_method == 'Interactive') {
      if (is.null(rv$curr_basis)) {format_curr_basis()(basis())}
      return( ## A gg plot of the current frame
        spinifex::oblique_frame(basis     = rv$curr_basis,
                                data      = selDat(),
                                manip_var = manip_var_num(),
                                col       = sel_col(),
                                pch       = sel_pch(),
                                axes      = "right") 
      )
    } ## Close if (input$manual_method == 'Interactive')
    if (input$manual_method == 'Animation') {
      gg_anim <- NULL
      if (input$anim_type %in% c("Radial", "Horizontal", "Vertical")) {# spinifex funcs with manip var.
        gg_anim <- oblique_frame(basis     = rv$curr_basis,
                                 data      = selDat(),
                                 manip_var = manip_var_num(),
                                 col       = sel_col(),
                                 pch       = sel_pch(),
                                 axes      = "right")
      } else { # tourr funcs without manip var.
        gg_anim <- view_basis(basis = rv$curr_basis,
                              data  = selDat(),
                              col   = sel_col(),
                              pch   = sel_pch(),
                              axes  = "right")
      }
      return(gg_anim) ## a 
    } ## Close if (input$manual_method == 'Animation')
  }) ## Close main_plot reactive function
  
  ### Output ----
  output$rawDat_summary <- renderPrint({
    dat <- as.data.frame(rawDat()) ## For naming
    tibble::as_tibble(dat)
  })
  output$selDat_summary <- renderPrint({
    dat <- as.data.frame(selDat()) ## For naming
    tibble::as_tibble(dat)
  })
  output$curr_basis_tbl <- renderTable(rv$curr_basis, rownames = TRUE)
  output$main_plot <- renderPlot(suppressMessages(main_plot()))
  
  ##### Data observes ----
  ## If basis() changes, update rv$curr_basis
  observeEvent(basis(), {rv$curr_basis <- basis()})
  
  ## If the rawDat() changes, update input$projVars
  observeEvent(rawDat(), {
    num_dat_nms <- names(rawDat()[numericVars_TF()])
    colToSelect <- 1:min(length(num_dat_nms), 6)
    updateCheckboxGroupInput(session, "projVars",
                             choices  = num_dat_nms,
                             selected = num_dat_nms[colToSelect])
  })
  
  ### If input$projVars Update manip variable choices when projection variables change
  observeEvent({input$projVars}, {
    updateSelectInput(session, "manip_var_nm", choices = input$projVars, 
                      selected = input$projVars[1])
  })
  
  ### If rawDat() changes, update pch/col choices
  observeEvent({rawDat()}, {
    culster_dat <- rawDat()[clusterVars_TF()]
    opts <- names(culster_dat)
    opts_none <- c(opts, "<none>")
    updateSelectInput(session, "col_var_nm", choices = opts_none,
                      selected = opts_none[1])
    updateSelectInput(session, "pch_var_nm", choices = opts_none,
                      selected = opts_none[1])
    updateSelectInput(session, "pp_cluster", choices = opts,
                      selected = opts[1])
    updateSelectInput(session, "anim_pp_cluster", choices = opts,
                      selected = opts[1])
  })
  
  ### On "Back to start" button, set current basis to the initial basis.
  observeEvent(input$re_init, {
    format_curr_basis()(basis())
  })
  
  ### Save current basis (interactive)
  observeEvent(input$save, {
    if (is.null(rv$curr_basis)) return()
    rv$png_save_cnt <- rv$png_save_cnt + 1
    save_file <- sprintf("tour_basis%03d", rv$png_save_cnt)
    write.csv(rv$curr_basis, file = paste0(save_file, ".csv"), row.names = FALSE)
    
    gg_out <- oblique_frame(data = selDat(),
                            basis = rv$curr_basis,
                            manip_var = manip_var_num(),
                            theta = 0,
                            phi = 0,
                            col = sel_col(),
                            pch = sel_pch(),
                            axes = "right")
    ggplot2::ggsave(paste0(save_file, ".png"), gg_out,
                    width = 4, height = 4, units = "in")
    
    output$save_msg <- renderPrint(paste0(
      "Basis saved as ", save_file, " (csv & png)."))
  })
  
  ### Send current basis to gallery
  observeEvent(input$to_gallery, {
    if (is.null(rv$curr_basis)) return()
    rv$gallery_n_rows <- rv$gallery_n_rows + 1
    gallery_row <- data.frame(Id = rv$gallery_n_rows,
                              `Manip var`  = input$manip_var_nm, 
                              `Manip type` = input$manip_type, 
                              `Time saved` = substr(Sys.time(), 12, 19),
                              check.names = FALSE)
    
    gallery_row$basis <- list(rv$curr_basis)
    rv$gallery_bases <- rbind(rv$gallery_bases, gallery_row)
    
    output$save_msg <- renderPrint(cat(
      "Basis sent to the gallery as row ", rv$gallery_n_rows, ".", sep = ""))
  })
  
  
  
  
  ### Oblique reactives -----
  
  
  ### x, y, radius oblique motion 
  basis_obl <- reactive({
    if (length(manip_var_num()) == 1) {
      theta <- phi <- NULL
      mv_sp <- create_manip_space(rv$curr_basis, manip_var_num())[manip_var_num(), ]
      if (input$manip_type == "Horizontal") {
        theta <- 0
        phi.x_zero <- atan(mv_sp[3] / mv_sp[1]) - (pi / 2 * sign(mv_sp[1]))
        phi <- manip_slider_t() * pi/2 + phi.x_zero
      }
      if (input$manip_type == "Vertical") {
        theta <- pi/2
        phi.y_zero <- atan(mv_sp[3] / mv_sp[2]) - (pi / 2 * sign(mv_sp[2]))
        phi <- manip_slider_t() * pi/2 + phi.y_zero
      }
      if (input$manip_type == "Radial") {
        theta <- atan(mv_sp[2] / mv_sp[1])
        phi_start <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
        phi <- (acos(manip_slider_t()) - phi_start) * - sign(mv_sp[1])
      }
      ret <- oblique_basis(basis = rv$curr_basis, manip_var = manip_var_num(),
                           theta = theta, phi = phi)
      row.names(ret) <- colnames(selDat())
      
      return(ret)
    }
  })
  
  ### No uniquely oblique outputs
  
  
  ##### Oblique observes -----
  ### Slider values
  observeEvent(manip_slider_t(), {
    format_curr_basis()(basis_obl())
  })
  
  ### Update sliders
  observeEvent({
    rv$curr_basis
    manip_var_num()
  }, {
    if (length(manip_var_num()) != 0 & input$manual_method == 'Interactive') {
      if(is.null(rv$curr_basis)) {format_curr_basis()(basis())}
      mv_sp <- create_manip_space(rv$curr_basis, manip_var_num())[manip_var_num(), ]
      if (input$manip_type == "Horizontal") {
        phi.x_zero <- atan(mv_sp[3] / mv_sp[1]) - (pi / 2 * sign(mv_sp[1]))
        x_val <- round(-phi.x_zero / (pi/2), 1)
        updateSliderInput(session, "manip_slider", value = x_val)
      }
      if (input$manip_type == "Vertical") {
        phi.y_zero <- atan(mv_sp[3] / mv_sp[2]) - (pi / 2 * sign(mv_sp[2]))
        y_val <- round(-phi.y_zero / (pi/2), 1)
        updateSliderInput(session, "manip_slider", value = y_val)
      }
      if (input$manip_type == "Radial") {
        phi_i <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
        rad_val <- round(cos(phi_i), 1)
        updateSliderInput(session, "manip_slider", value = rad_val)
      }
    }
  })
  
  
  ##### Animation reactives -----
  ### Set rv$tour_array when any param changes.
  observeEvent({
    selDat()
    input$anim_type
    ## spinifex param
    basis() 
    manip_var_num()
    input$anim_angle
    ## tourr param 
    input$pp_type
    }, {
    ### Processing message for gif
    # withProgress(message = 'Rendering animation ...', value = 0, {
      ### Manual tour animation
      app_manual_tour <- function(...) { ## for code reduction, handle different theta.
        manual_tour(basis = basis(), 
                    data = selDat(), 
                    manip_var = manip_var_num(),
                    angle = input$anim_angle,
                    ...) ## Allows theta to vary
      }
      if (input$anim_type %in% c("Radial", "Horizontal", "Vertical")) {
        t_array <- NULL 
        if (input$anim_type == "Radial")     {t_array <- app_manual_tour()} # default theta to radial
        if (input$anim_type == "Horizontal") {t_array <- app_manual_tour(theta = 0)}
        if (input$anim_type == "Vertical")   {t_array <- app_manual_tour(theta = pi/2)}
        rv$tour_array <- t_array
      } else {
        ### Projection pursuit
        # TODO: trouble shoot PP and tourr paths.
        t_path <- NULL
        if (input$anim_type == "Projection pursuit") {
          pp_cluster <- NA
          if (input$anim_pp_type %in% c("lda_pp", "pda_pp")) {
            culster_dat <- rawDat()[clusterVars_TF()]
            pp_cluster <- culster_dat[input$anim_pp_cluster]
          }
          tour_func <- getGuidedTour(input$anim_pp_type, pp_cluster)
          t_path <- tourr::save_history(selDat(), tour_func, 
                                        start = rv$curr_basis)
        }
        ### Grand, little and local tours
        if(input$anim_type == "Grand (6 bases)") {
          t_path <- tourr::save_history(selDat(), grand_tour(),
                                        max_bases = 6, start = rv$curr_basis)
        }
        if(input$anim_type == "Little (6 bases)") {
          t_path <- tourr::save_history(selDat(), little_tour(),
                                        max_bases = 6, start = rv$curr_basis)
        }
        if(input$anim_type == "Local (6 bases)") {
          t_path <- tourr::save_history(selDat(), max_bases = 6,
                                        tour_path = local_tour(rv$curr_basis, pi/4))
        }
        rv$tour_array <- interpolate(t_path, angle = input$anim_angle)
      }
      
    ### end processing message for .gif 
    # setProgress(1)
    # })
  })
  
  ##### Animation observes -----
  ### Update anim_slider when rv$tour_array changes.
  observeEvent(rv$tour_array, {
    rv$anim_playing <- FALSE
    updateSliderInput(session, "anim_slider", value = 1, 
                      min = 1, max = dim(rv$tour_array)[3])
  })
  
  ### Set curr basis when anim_slider changes.
  observeEvent(input$anim_slider, {
    ## TODO: causes play to stop every tick, dispite play being isolated.
    #rv$anim_playing <- FALSE
    format_curr_basis()(matrix(rv$tour_array[,, input$anim_slider], ncol = 2))
  })
  
  ### Change curr basis when slider changes
  observeEvent(input$anim_slider, {
    format_curr_basis()(matrix(rv$tour_array[,, input$anim_slider],  ncol = 2 ))
  })
  

  ### Save animation to .gif
  observeEvent(input$anim_save, {
    rv$gif_save_cnt <- rv$gif_save_cnt + 1
    
    ##TODO:: DEBUGGING
    flea_std <- rescale(tourr::flea[,1:6])
    tpath <- save_history(flea_std, tour_path = grand_tour(), max = 3)
    str(tpath)
    str(rv$tour_array)
    debug(interpolate)
    #debug(geodesic_path)
    writeClipboard("debug(tour)")
    browser()
    str(tpath)
    str(rv$tour_array)
    cat("error in tourr::interpolate(tour(tour_path(geodesic_path(geodesic_info(is_orthonormal. it's functions all the way down...")
    cat("massive hair ball does a tumble")
    play_tour_path(tour_path = rv$tour_array, data = selDat(),
                   render_type = render_gganimate)
    writeClipboard("debug(tour_path)")
    undebug(interpolate)
    undebug(tour_path)
    undebug(geodesic_path)
    undebug(geodesic_info)
    ##### --
    ex <- play_tour_path(tour_path = tpath, data = flea_std,
                         render_type = render_gganimate)
    # gganimate::anim_save("myGif.gif", ex)
    ex2 <- play_tour_path(tour_path = rv$tour_array, data = selDat(),
                         render_type = render_gganimate)
    # gganimate::anim_save("myGif.gif", ex2)
    ##TODO: END
    
    
    withProgress(message = 'Rendering animation ...', value = 0, {
      anim <- play_tour_path(tour_path = rv$tour_array,
                             data = selDat(),
                             col = sel_col(), 
                             pch = sel_pch(),
                             axes = "right",
                             render_type = render_gganimate,
                             angle = input$angle,
                             fps = input$fps)
      save_file <- sprintf("tour_animation%03d.gif", rv$gif_save_cnt)
      gganimate::anim_save(save_file, anim)
    })
    setProgress(1)
    
    output$save_msg <- renderPrint(paste0("Animation saved as ", save_file, ".", sep=""))
  })
  
  ### Play animation button.
  observe({ # Play anim while odd number of clicks
    invalidateLater(1000 / input$fps, session)
    isolate({
      if (rv$anim_playing == TRUE) { # on play
        isolate(updateSliderInput(session, "anim_slider", 
                                   value = input$anim_slider + 1))
        if (input$anim_slider == dim(rv$tour_array)[3]) { # pause on last slide 
          rv$anim_playing <- !rv$anim_playing
        }
      }
    })
  })
  # play/pause labeling
  observeEvent(rv$anim_playing, {
    if (rv$anim_playing == TRUE) {
      updateActionButton(session, "anim_play", label = "pause")
    }
    if (rv$anim_playing == FALSE) {
      updateActionButton(session, "anim_play", label = "play")
    }
  })
  observeEvent(input$anim_play, { # Button label switching and end of anim handling
    if (input$anim_slider == dim(rv$tour_array)[3]) { # if at the last slide, start at slide 1
      updateSliderInput(session, "anim_slider", value = 1)
    }
    rv$anim_playing <- !rv$anim_playing
  })
  

  ##### Gallery tab ----
  
  ### Display table with buttons
  gallery_df <- reactive({
    if (is.null(rv$gallery_bases)) {
      output$gallery_msg <- renderPrint(cat("Send a basis to the gallery."))
      return()
    }
    shinyInput <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
      }
      inputs
    }
    
    df_full = data.frame(
      Label = shinyInput(textInput, nrow(rv$gallery_bases), 'text_', label = "",
                         placeholder = "Custom user label", width = '200px'),
      Plot = 
        shinyInput(actionButton, nrow(rv$gallery_bases), 'button_', label = "Plot", 
                   onclick = 'Shiny.onInputChange(\"gallery_plot\",  this.id)'),
      `Save (csv & png)` = 
        shinyInput(actionButton, nrow(rv$gallery_bases), 'button_', label = "Save", 
                   onclick = 'Shiny.onInputChange(\"gallery_save\",  this.id)'),
      Delete = 
        shinyInput(actionButton, nrow(rv$gallery_bases), 'button_', label = "Remove", 
                   onclick = 'Shiny.onInputChange(\"gallery_delete\",  this.id)'),
      rv$gallery_bases[, -which(colnames(rv$gallery_bases) %in% c("basis", "Id"))],
      stringsAsFactors = FALSE,
      row.names = NULL,
      check.names = FALSE
    )
    gallery_df <- df_full[!rownames(df_full) %in% rv$gallery_rows_removed, ]
    
    if (nrow(gallery_df) == 0) { # if all basis removed
      output$gallery_msg <- renderPrint(cat("Send a basis to the gallery."))
      return()
    }
    output$gallery_msg <- NULL
    
    return(gallery_df)
  })
  
  ### gallery table output
  output$gallery_df <- DT::renderDataTable(
    gallery_df(), server = FALSE, escape = FALSE, selection = 'none', 
    options = list(dom = 't', pageLength = 100)
  )
  
  ##### Gallery icons with data
  gallery_icons <- reactive({
    bas <- rv$gallery_bases
    if (is.null(bas) | length(bas) == 0) {return()}
    
    df <- bas[!rownames(bas) %in% rv$gallery_rows_removed, ]
    if (is.null(df)) {return()}
    if (nrow(df) == 0) {return()}
    # Init
    n_bases <- nrow(df)
    n <- nrow(selDat())
    p <- ncol(selDat())
    
    df_gg_data <- NULL # Unlist basis into p rows
    for (i in 1:n_bases){
      rows <- data.frame(id = rep(df$Id[i], n),
                         as.matrix(selDat()) %*% as.matrix(df$basis[[i]]),
                         col = sel_col(),
                         pch = sel_pch())
      df_gg_data <- rbind(df_gg_data, rows)
    }
    ### Add data points to gallery icons
      ggplot2::ggplot() +
        ggplot2::scale_color_brewer(palette = "Dark2") +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::coord_fixed() +
        ggplot2::geom_point(data = df_gg_data, size = .3,
                            mapping =  ggplot2::aes(x = x, y = y, 
                                                    color = col,
                                                    shape = as.character(pch))) +
        ggplot2::facet_grid(rows = vars(id)) + 
        ggplot2::theme(strip.background = ggplot2::element_blank(),
                       strip.text.y     = ggplot2::element_blank(),
                       panel.spacing.y   = unit(1.5, "lines"))
  })
  
  ## TODO: this causes the gallery_icon error message, resolve it.
  output$gallery_icons <- renderPlot(
    gallery_icons(), width = 84,
    height = function(){
      bas <- rv$gallery_bases
      if (is.null(bas) | length(bas) == 0) {return(1)}
      n_icons <- nrow(bas[!rownames(bas) %in% rv$gallery_rows_removed, ])
      84 * n_icons
    }
  )
  
  ### Plot button (gallery)
  observeEvent(input$gallery_plot, {
    selectedRow <- as.numeric(strsplit(input$gallery_plot, "_")[[1]][2])
    format_curr_basis()(
      rv$gallery_bases[selectedRow, which(colnames(rv$gallery_bases) == "basis")][[1]]
    )
    output$gallery_msg <- renderText(
      paste0("The basis from row ", selectedRow, " is now the current basis.", sep = ""))
  })
  
  ### Save button (gallery)
  observeEvent(input$gallery_save, {
    rv$png_save_cnt <- rv$png_save_cnt + 1
    selectedRow <- as.numeric(strsplit(input$gallery_save, "_")[[1]][2])
    save_file <- sprintf("tour_basis%03d", rv$png_save_cnt)
    save_basis <- 
      rv$gallery_bases[selectedRow, which(colnames(rv$gallery_bases) == "basis")][[1]]
    
    write.csv(save_basis, file = paste0(save_file, ".csv"), row.names = FALSE)
    gg_out <- oblique_frame(data = selDat(),
                            basis = save_basis,
                            manip_var = manip_var_num(),
                            theta = 0,
                            phi = 0,
                            col = sel_col(),
                            pch = sel_pch(),
                            axes = "right")
    ggplot2::ggsave(paste0(save_file,".png"), gg_out,
                    width = 4, height = 4, units = "in")
    output$gallery_msg <- renderText(paste0("Saved row ", selectedRow, " as ", save_file, " (csv & png).")) 
  })
  
  ### Delete button (gallery)
  observeEvent(input$gallery_delete, {
    selectedRow <- as.numeric(strsplit(input$gallery_delete, "_")[[1]][2])
    rv$gallery_rows_removed <- c(rv$gallery_rows_removed, selectedRow)
  })
  
  ## Development help -- to display dev tools see the top of 'global_shinyApps.r'
  if (.include_dev_display == TRUE) {
    shinyjs::show("dev_toggle")
  } ## else (.include_dev_display != TRUE) dev content remains hidden.
  
  ### Development display -- can be toggled by setting .include_dev_display at the top of global.R
  observeEvent(input$browser, {browser()})
  output$dev_msg <- renderPrint({
    cat("Dev msg -- \n",
        "rv$curr_basis: ",         rv$curr_basis,             "\n",
        "is null? ",               is.null(rv$curr_basis),    "\n",
        "basis(): ",               basis(),                   "\n",
        "input$manip_var_nm: ",       input$manip_var_nm,           "\n",
        "manip_var_num(): ",           manip_var_num(),               "\n",
        "rv$gallery_bases: ",      unlist(rv$gallery_bases),  "\n",
        "is.null? ",               is.null(rv$gallery_bases), "\n",
        "input$pp_type: ",         input$pp_type,             "\n",
        "input$anim_pp_cluster: ", input$anim_pp_cluster,     "\n",
        sep = ""
    )
  })
  
  #TODO: Go to the other logging method.
}

shinyApp(ui = ui, server = server)
