### "DevUnderConstruction" app.R -----
# options(shiny.error = FALSE)

#' Shiny app for exploring multivariate data, comparing manual tours with 
#' alternative techniques
#' 
#' @author Nicholas Spyrison
#' @export
#' @examples \dontrun{
#' spinifex::run_app("devUnderConstruction")
#' }


##TODO: Debug guided tours
##TODO: Fix manip slider o.o...

##TODO: Add logging? see spinifex_study app
##TODO: Add input$size?
##TODO: Add input$alpha? see spinifex_study app


source('../global_shinyApps.r', local = TRUE)
source('ui.R', local = TRUE)

server <- function(input, output, session) {
  ##### Init reactiveValues
  rv                      <- reactiveValues()
  rv$curr_basis           <- tourr::basis_random(6)
  rv$manual_basis_array   <- NULL
  rv$manual_plot_array    <- NULL
  rv$is_manual_debounced  <- TRUE
  rv$anim_basis_array     <- NULL
  rv$anim_slide           <- 1
  rv$is_anim_playing      <- FALSE
  rv$png_save_cnt         <- 0
  rv$gif_save_cnt         <- 0
  rv$gallery_bases        <- NULL
  rv$gallery_n_rows       <- 0
  rv$gallery_rows_removed <- NULL
  
  
  ####### REACTERS -----
  ##### _Util reacs: dim and aesthetics ----
  n <- reactive(nrow(projMat()))
  p <- reactive(ncol(projMat()))
  
  ### Throttle manip_slider
  manip_slider_index_t <- 1 + 10 * throttle(reactive(input$manip_slider), 100) ## throttle time in milliseconds
  
  col_var <- reactive({
    if (input$col_nm == "<none>") return(rep("a", n())) ## Create dummy column in "<none>"
    dat <- rawDat()
    dat[, which(colnames(dat) %in% input$col_nm)]
  })
  
  pch_var <- reactive({
    if (input$pch_nm == "<none>") return(rep("a", n())) ## Create dummy column in "<none>"
    dat <- rawDat()
    dat[, which(colnames(dat) %in% input$pch_nm)]
  })
  
  manip_num <- reactive({ 
    if (input$manip_nm == "<none>") {return(1)}
    if (input$tour_class == "Guided") {return(1)}
    which(colnames(projMat()) == input$manip_nm)
  }) 
  
  manip_assumptions <- reactive({
    mNum <- manip_num()
    mSlider <- input$manip_slider
    if (is.null(mNum) | length(mNum) == 0) 
    {M(manip_assumptions);return(FALSE)}
    if (mNum %in% 1:nrow(projMat()) == FALSE)  
    {M(manip_assumptions);return(FALSE)}
    if (mSlider < 0 | mSlider > 1) 
    {M(manip_assumptions);return(FALSE)}
    TRUE
  })
  
  pch_assumptions <- reactive({
    pchVarNm <- input$pch_nm
    if (is.null(pchVarNm) | length(pchVarNm) == 0) 
    {M(pch_assumptions);return(FALSE)}
    if (pchVarNm %in% c(colnames(rawDat()), "<none>") == FALSE) 
    {M(pch_assumptions);return(FALSE)}
    TRUE
  })
  
  col_assumptions <- reactive({
    colVarNm <- input$col_nm
    if (is.null(colVarNm) | length(colVarNm) == 0) 
    {M(col_assumptions);return(FALSE)}
    if ((colVarNm %in% c(colnames(rawDat()), "<none>")) == FALSE) 
    {M(col_assumptions);return(FALSE)}
    TRUE
  })
  
  ##### _Data react -----
  inputPath_assumptions <- reactive({
    path <- input$data_file$datapath
    ext <- tolower(substr(path, nchar(path) - 4 + 1, nchar(path)))
    if (is.null(path) | length(path) == 0) 
      {M(inputPath_assumptions);return(FALSE)}
    if (ext %in% c(".csv", ".rda") == FALSE) 
      {M(inputPath_assumptions);return(FALSE)}
    TRUE
  })
  
  ## Raw input data
  rawDat <- reactive({
    output$data_msg <- NULL
    if (input$data_source == "Example"){
      if (input$data_example == "flea" )         return(tourr::flea)
      if (input$data_example == "wine" )         return(spinifex::wine)
      if (input$data_example == "breastcancer" ) return(spinifex::breastcancer)
      if (input$data_example == "weather" )      return(spinifex::weather)
    }
    if (input$data_source == "Upload"){
      path <- input$data_file$datapath
      if (inputPath_assumptions() == FALSE) {
        if ((is.null(path) | length(path) == 0) == FALSE) {
          output$data_msg <- renderPrint(paste0(
            "Data file: '", path, "' failed to upload."))
        }
        return()
      }
      if (ext == ".csv")
        return(read.csv(path, stringsAsFactors = FALSE))
      if (ext == ".rda")
        return(load(file = path))
    }
  })
  
  rawData_assumptions <- reactive({
    raw <- rawDat()
    if (is.null(raw) | length(raw) == 0) 
      {M(rawData_assumptions);return(FALSE)}
    TRUE
  })
  
  ## Data to project
  projMat <- reactive({
    if (rawData_assumptions()  == FALSE) return()
    if (projData_assumptions() == FALSE) return()
    
    dat <- rawDat()
    dat <- dat[, which(colnames(dat) %in% input$projVar_nms)]
    dat <- dat[complete.cases(dat), ] ## Row-wise complete
    if (input$rescale_data) dat <- tourr::rescale(dat)
    
    if (!is.matrix(dat))    dat <- as.matrix(dat)
    return(dat)
  })
  
  projData_assumptions <- reactive({
    dat <- rawDat()
    if (is.null(dat) | length(dat) == 0) 
      {M(projData_assumptions);return(FALSE)}
    TRUE
  })
  
  projVars_defaultNms <- reactive({
    if (rawData_assumptions() == FALSE) return()
    dat <- rawDat()
    numeric_col_nums <- sapply(dat, is.numeric)
    colnames(dat[, numeric_col_nums])
  })
  
  projVars_assumptions <- reactive({
    if (rawData_assumptions() == FALSE) return()
    varNms <- projVars_defaultNms()
    if (is.null(varNms) | length(varNms) == 0)
      {M(projVars_assumptions);return(FALSE)}
    TRUE
  })
  
  
  #### _Basis react -----
  init_basis <- reactive({
    ### Condition handling
    if (projData_assumptions() == FALSE) return()
    ## Causes issue for manual tour; due to othonormalization of manip sp:
    ## if (input$basis_init == "Identity") ret <- diag(p())[, 1:2] 
    if (input$basis_init == "Random")   ret <- tourr::basis_random(n = p(), d = 2)
    if (input$basis_init == "PCA")      ret <- prcomp(projMat())[[2]][, 1:2]
    if (input$basis_init == "From file") {
      if (inputPath_assumptions() == FALSE) return()
      path <- input$basis_file$datapath
      ext <- tolower(substr(path, nchar(path) - 4 + 1, nchar(path)))
      if (ext == ".csv") x <- read.csv(path, stringsAsFactors = FALSE)
      if (ext == ".rda"){ ## load .rda object, not just name:
        tmp <- new.env()
        load(file = path, envir = tmp)
        ret <- tmp[[ls(tmp)[1]]]
      }
    }
    if (input$basis_init == "Projection pursuit") {
      basis_pp_cluster <- NA
      if (input$pp_type %in% c("lda_pp", "pda_pp")){
        dat <- rawDat()
        basis_pp_cluster <- dat[input$basis_pp_cluster]
      }
      tour_func <- appGetGuidedTour(input$pp_type, basis_pp_cluster)
      tour_hist <- save_history(projMat(), tour_func)
      tour_len  <- dim(tour_hist)[3]
      ret <- matrix(as.numeric(tour_hist[,, tour_len]), ncol = 2)
    }
    
    ret <- format_basis()(ret)
    return(ret)
  })
  
  basis_assumptions <- reactive({
    bas <- rv$curr_basis
    if (is.null(bas) | length(bas) == 0) 
      {M(basis_assumptions);return(FALSE)}
    if (!is.numeric(bas)) 
      {M(basis_assumptions);return(FALSE)}
    TRUE
  })
  
  ### Add col and row names, and set rv$curr_basis.
  format_basis <- reactive({
    format_basis_func <- function(bas) {
      if (basis_assumptions() == FALSE) return()
      #if (input$tour_class == "Manual" & rv$is_manual_debounced == TRUE) return()
      rv$is_manual_debounced <<- TRUE
      colnames(bas)  <- c("x", "y")
      row.names(bas) <- colnames(projMat())
      
      rv$curr_basis <- bas
      return(bas)
    }
  })
  
  ### _main_plot react ------
  main_plot <- reactive({
    if (basis_assumptions()    == FALSE) return()
    if (projData_assumptions() == FALSE) return()
    if (manip_assumptions()    == FALSE) return()
    if (pch_assumptions()      == FALSE) return()
    if (col_assumptions()      == FALSE) return()
    
    if (input$tour_class == 'Manual') {
      dat <- projMat()
      m_var_num <- manip_num()
      ### Make rv$manual_basis_array if null
      if(length(rv$manual_basis_array) == 0){
        rv$manual_basis_array <- array(NA, dim = c(p(), 2, 11))
        mv_sp <- create_manip_space(rv$curr_basis, m_var_num)[m_var_num, ]
        theta <- phi <- NULL
        if(input$manip_type == "Horizontal") theta <- 0
        if(input$manip_type == "Vertical")   theta <- pi / 2
        if(input$manip_type == "Radial")     theta <- atan(mv_sp[2] / mv_sp[1])
        phi_start <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
        phi_slide <- acos((0:10)/10) ## possible values of manip_slider.
        phi_vals  <- (phi_slide - phi_start) * - sign(mv_sp[1]) 
        
        i_max <- length(phi_vals) ## ~11; 1:11, by 1
        for (i in 1:i_max){
          rv$manual_basis_array[,, i] <-
            oblique_basis(basis = rv$curr_basis, manip_var = m_var_num,
                          theta = theta, phi = phi_vals[i])
        }
        for (i in 1:i_max){
          rv$manual_plot_array[,, i] <-
            spinifex::oblique_frame(basis     = rv$manual_basis_array[,, i],
                                    data      = dat,
                                    manip_var = m_var_num,
                                    col       = col_of(col_var()),
                                    pch       = pch_of(pch_var()),
                                    axes      = "right")
        }
      } ### End rv$manual_basis_array if null
      .slider_index <- manip_slider_index_t()
      format_basis()(rv$manual_basis_array[,, .slider_index])
      return(rv$manual_plot_array[,, .slider_index])
    }
    if (input$tour_class == 'Guided') {
      anim_frame <- NULL
      if (input$anim_type %in% c("Radial", "Horizontal", "Vertical")) {
        anim_frame <- spinifex::oblique_frame(basis     = rv$curr_basis,
                                              data      = projMat(),
                                              manip_var = manip_num(),
                                              col       = col_of(col_var()),
                                              pch       = pch_of(pch_var()),
                                              axes      = "right")
      } else { ## For tourr funcs without manip var.
        anim_frame <- spinifex::view_basis(basis = rv$curr_basis,
                                           data  = projMat(),
                                           col   = col_of(col_var()),
                                           pch   = pch_of(pch_var()),
                                           axes  = "right")
      }
      return(anim_frame)
    } ## end of if (input$tour_class == 'Guided')
  })
  
  
  ##### _Gallery react -----
  gallery_basis_assumptions <- reactive({
    bas <- rv$gallery_bases
    if (is.null(bas)) ## Can be length 0 if all rows removed
    {M(gallery_basis_assumptions);return(FALSE)}
    TRUE
  })
  
  gallery_df <- reactive({
    if (gallery_basis_assumptions() == FALSE) {
      output$gallery_msg <- renderPrint(cat("Send a basis to the gallery."))
      return()
    }
    bases <- rv$gallery_bases
    n_bases <- nrow(bases)
    full_gallery_df = data.frame(
      `Label` = 
        appShinyInput(textInput, n_bases, 'text_', label = "",
                      placeholder = "User label", width = '200px'),
      `Plot` = 
        appShinyInput(actionButton, n_bases, 'button_', label = "Plot", 
                      onclick = 'Shiny.onInputChange(\"gallery_plot\",  this.id)'),
      `Save (csv & png)` =
        appShinyInput(actionButton, n_bases, 'button_', label = "Save", 
                      onclick = 'Shiny.onInputChange(\"gallery_save\",  this.id)'),
      `Delete` = 
        appShinyInput(actionButton, n_bases, 'button_', label = "Remove", 
                      onclick = 'Shiny.onInputChange(\"gallery_delete\",  this.id)'),
      bases[, -which(colnames(bases) %in% c("basis", "Id"))],
      stringsAsFactors = FALSE,
      row.names = NULL,
      check.names = FALSE
    )
    display_gallery_df <- 
      full_gallery_df[!rownames(full_gallery_df) %in% rv$gallery_rows_removed, ]
    
    if (gallery_basis_assumptions() == FALSE) { ## _ie._ all bases removed
      output$gallery_msg <- renderPrint(cat("Send a basis to the gallery."))
      return()
    }
    output$gallery_msg <- NULL
    
    return(display_gallery_df)
  })
  
  ##### Gallery icons with data
  gallery_icons <- reactive({
    if (gallery_basis_assumptions() == FALSE) return()
    
    bases <- rv$gallery_bases
    df <- bases[!rownames(bases) %in% rv$gallery_rows_removed, ]
    n_bases <- nrow(df)
    n <- n()
    
    df_gg_data <- NULL ## Unlist bases
    for (i in 1:n_bases){
      rows <- data.frame(id = rep(df$Id[i], n),
                         projMat() %*% df$basis[[i]],
                         col = col_of(col_var()))
      df_gg_data <- rbind(df_gg_data, rows)
    }
    output$gallery_icons_str <- renderText(str(df_gg_data))
    
    ## Add data points to gallery icons
    ggplot2::ggplot() +
      ggplot2::scale_color_brewer(palette = "Dark2") +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::coord_fixed() +
      ggplot2::geom_point(data = df_gg_data, size = .3,
                          mapping =  ggplot2::aes(x = x, y = y, 
                                                  color = col)) +
      ggplot2::facet_grid(rows = vars(id)) + 
      ggplot2::theme(strip.background = ggplot2::element_blank(),
                     strip.text.y     = ggplot2::element_blank(),
                     panel.spacing.y  = unit(1.5, "lines"))
  })
  
  
  ####### OBERSERVERS -----
  ##### _Data obs ----
  ### When rawDat() changes...
  observeEvent({rawDat()}, {
    appObsMsg("rawDat")
    dat <- rawDat()
    nms <- names(dat)
    rv$manual_basis_array <- NULL ## clear manual bases
    
    ### ... update projection variable choices
    updateCheckboxGroupInput(session,
                             inputId  = "projVar_nms",
                             choices  = nms,
                             selected = projVars_defaultNms())
    
    ### ... update aes choices
    pos_cols <- sapply(dat, is.factor)
    if (length(pos_cols) == 0) 
      pos_cols <- sapply(dat, is.character)
    if (length(pos_cols) == 0) 
      pos_cols <- sapply(dat, length(unique(x)) < 10)
    
    if (length(pos_cols) == 0) {.select <- "<none>"
    } else {.select <- nms[pos_cols][1]}
    .choices <- c("<none>", nms)
    
    updateSelectInput(session,
                      inputId  = "col_nm",
                      choices  = .choices,
                      selected = .select)
    updateSelectInput(session,
                      inputId  = "pch_nm",
                      choices  = .choices,
                      selected = .select)
    updateSelectInput(session,
                      inputId  = "basis_pp_cluster",
                      choices  = .choices,
                      selected = .select)
    updateSelectInput(session, 
                      inputId  = "anim_pp_cluster", 
                      choices  = .choices,
                      selected = .select)
  })
  
  ### When input$projVariables changes...
  observeEvent({input$projVar_nms}, {
    appObsMsg("input$projVar_nms")
    
    ### ...pdate manip var choices 
    updateSelectInput(session,
                      inputId  = "manip_nm", 
                      choices  = input$projVar_nms, 
                      selected = input$projVar_nms[1])
  })
  
  ### On "Initiaze to above parameters" button, set current basis to the initializ basis.
  observeEvent(input$re_init, {
    appObsMsg("input$re_init")
    init_basis()
  })
  
  ### Save current basis (Manual)
  observeEvent(input$save, {
    appObsMsg("input$save")
    if (basis_assumptions == FALSE) return()
    
    rv$png_save_cnt <- rv$png_save_cnt + 1
    save_file <- sprintf("spinifexApp_basis%03d", rv$png_save_cnt)
    write.csv(rv$curr_basis, file = paste0(save_file, ".csv"), row.names = FALSE)
    
    gg_out <- spinifex::oblique_frame(data = projMat(),
                                      basis = rv$curr_basis,
                                      manip_var = manip_num(),
                                      col = col_of(col_var()),
                                      pch = pch_of(pch_var()),
                                      axes = "right")
    ggplot2::ggsave(paste0(save_file, ".png"), gg_out,
                    width = 4, height = 4, units = "in")
    
    output$save_msg <- renderPrint(paste0(
      "Basis saved as ", save_file, " (csv & png)."))
  })
  
  ### Send current basis to gallery
  observeEvent(input$to_gallery, {
    appObsMsg("input$to_gallery")
    if (basis_assumptions() == FALSE) return()
    
    rv$gallery_n_rows <- rv$gallery_n_rows + 1
    gallery_row <- data.frame(Id = rv$gallery_n_rows,
                              `Data source` = datNm(),
                              `Time saved` = substr(Sys.time(), 12, 19),
                              check.names = FALSE)
    gallery_row$basis <- list(rv$curr_basis)
    
    bases   <- rv$gallery_bases
    n_bases <- nrow(bases)
    if (all(rv$curr_basis == bases$basis[[n_bases]])){
      output$save_msg <- renderPrint(
        "The basis is the same as the last basis in the gallery, duplicate not sent to gallery again.")
    } else { ## save non-duplicates
      rv$gallery_bases <- rbind(bases, gallery_row)
      
      output$save_msg <- renderPrint(cat(
        "Basis sent to the gallery as row ", rv$gallery_n_rows, ".", sep = ""))
    }
  })
  
  
  ##### _Manual tour obs -----
  ### Debounce manip changes for .5 sec
  observe({
    if (input$tour_class != "Manual") return()
    if (rv$is_manual_debounced == TRUE) {
      invalidateLater(2000, session)
      isolate({
        rv$is_manual_debounced <- FALSE
      })
    }
  })
  
  ### Update sliders
  observeEvent({
    rv$curr_basis
    manip_num()
  }, {
    message(paste0("rv$is_manual_debounced: ", rv$is_manual_debounced))
    if (input$tour_class != "Manual")   return()
    if (basis_assumptions() == FALSE)   return()
    #if (rv$is_manual_debounced == TRUE) return()
    rv$is_manual_debounced <<- TRUE
    appObsMsg("rv$curr_basis, manip_num()")
    mv_sp <- create_manip_space(rv$curr_basis, manip_num())[manip_num(), ]
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
  })
  
  
  ##### _Guided tour obs -----
  ### When related paramaters change...
  observeEvent({
    projMat()
    input$anim_type
    ## spinifex parameter change:
    manip_num()
    ## tourr param parameters change:
    input$anim_angle
    input$pp_type
    }, {
      if (input$tour_class != "Guided") return()
      ### Processing message for gif
      # withProgress(message = 'Rendering guided tour ...', value = 0, {
      ## Manual, guided tour
      app_manual_tour <- function(...) { ## For code reduction, to handle different theta values.
        if (basis_assumptions() == FALSE)    return()
        if (projData_assumptions() == FALSE) return()

        manual_tour(basis = rv$currbasis,
                    data  = projMat(), 
                    manip_var = manip_num(),
                    angle = input$anim_angle,
                    ...) ## Allows theta to vary
      }
      
      if (input$anim_type %in% c("Radial", "Horizontal", "Vertical")) {
        t_array <- NULL 
        if (input$anim_type == "Radial")     {t_array <- app_manual_tour()} # Default theta to radial
        if (input$anim_type == "Horizontal") {t_array <- app_manual_tour(theta = 0)}
        if (input$anim_type == "Vertical")   {t_array <- app_manual_tour(theta = pi/2)}
        rv$anim_basis_array <- t_array
      } else {
        ### Projection pursuit
        # TODO: trouble shoot PP and tourr paths.
        t_path <- NULL
        if (input$anim_type == "Projection pursuit") {
          pp_cluster <- NA
          if (input$anim_pp_type %in% c("lda_pp", "pda_pp")) {
            dat <- rawDat()
            pp_cluster <- dat[input$anim_pp_cluster]
          }
          tour_func <- appGetGuidedTour(input$anim_pp_type, pp_cluster)
          t_path <- tourr::save_history(projMat(), tour_func, 
                                        start = rv$curr_basis)
        }
        # Grand, little and local tours
        if (input$anim_type == "Grand (6 bases)") {
          t_path <- tourr::save_history(projMat(), grand_tour(),
                                        max_bases = 6, start = rv$curr_basis)
        }
        if (input$anim_type == "Little (6 bases)") {
          t_path <- tourr::save_history(projMat(), little_tour(),
                                        max_bases = 6, start = rv$curr_basis)
        }
        if (input$anim_type == "Local (6 bases)") {
          t_path <- tourr::save_history(projMat(), max_bases = 6,
                                        tour_path = local_tour(rv$curr_basis, pi/4))
        }
        rv$anim_basis_array <- interpolate(t_path, angle = input$anim_angle)
      }
    ### end processing message for .gif 
    # setProgress(1)
    # })
  })
  
  ### Update anim_slider when rv$anim_basis_array changes.
  observeEvent(rv$anim_basis_array, {
    if (input$tour_class != "Guided") return()
    rv$is_anim_playing <- FALSE
    updateSliderInput(session, "anim_slider", 
                      value = 1, 
                      min = 1, 
                      max = dim(rv$anim_basis_array)[3])
  })
  
  ### Set curr basis when anim_slider changes.
  observeEvent(input$anim_slider, {
    if (input$tour_class != "Guided") return()
    if (basis_assumptions() == FALSE)     return()
    format_basis()(matrix(rv$anim_basis_array[,, input$anim_slider], ncol = 2))
  })
  
  ### Change curr basis when slider changes
  observeEvent(input$anim_slider, {
    if (input$tour_class != "Guided") return()
    if (basis_assumptions() == FALSE)     return()
    format_basis()(matrix(rv$anim_basis_array[,, input$anim_slider],  ncol = 2))
  })
  
  ### Save guided tour to .gif
  observeEvent(input$anim_save, {
    if (input$tour_class != "Guided") return()
    rv$gif_save_cnt <- rv$gif_save_cnt + 1
    
    ##TODO:: DEBUGGING
    flea_std <- rescale(tourr::flea[,1:6])
    tpath <- save_history(flea_std, tour_path = grand_tour(), max = 3)
    str(tpath)
    str(rv$anim_basis_array)
    debug(interpolate)
    #debug(geodesic_path)
    writeClipboard("debug(tour)")
    browser()
    str(tpath)
    str(rv$anim_basis_array)
    cat("error in tourr::interpolate(tour(tour_path(geodesic_path(geodesic_info(is_orthonormal. it's functions all the way down...")
    cat("massive hair ball does a tumble")
    play_tour_path(tour_path = rv$anim_basis_array, data = projMat(),
                   render_type = render_gganimate)
    writeClipboard("debug(tour_path)")
    undebug(interpolate)
    undebug(tour_path)
    undebug(geodesic_path)
    undebug(geodesic_info)
    ##### --TEMP DEBUG CODE:
    ex <- play_tour_path(tour_path = tpath, data = flea_std,
                         render_type = render_gganimate)
    # gganimate::anim_save("myGif.gif", ex)
    ex2 <- play_tour_path(tour_path = rv$anim_basis_array, data = projMat(),
                         render_type = render_gganimate)
    # gganimate::anim_save("myGif.gif", ex2)
    ##TODO: END
    
    
    withProgress(message = 'Rendering guided tour ...', value = 0, {
      anim <- play_tour_path(tour_path = rv$anim_basis_array,
                             data = projMat(),
                             col = col_of(col_var()), pch = pch_of(pch_var()),
                             axes = "right",
                             render_type = render_gganimate,
                             angle = input$angle,
                             fps = input$fps)
      save_file <- sprintf("spinifexApp_tour%03d.gif", rv$gif_save_cnt)
      gganimate::anim_save(save_file, anim)
    })
    setProgress(1)
    
    output$save_msg <- renderPrint(paste0("Guided tour saved as ", save_file, ".", sep=""))
  })
  
  ### Play guided tour button.
  observe({ ## Play anim while odd number of clicks
    invalidateLater(1000 / input$fps, session)
    isolate({
      if (rv$is_anim_playing == TRUE) {
        isolate(updateSliderInput(session, "anim_slider", 
                                   value = input$anim_slider + 1))
        if (input$anim_slider == dim(rv$anim_basis_array)[3]) { ## pause on last slide 
          rv$is_anim_playing <- !rv$is_anim_playing
        }
      }
    })
  })
  # play/pause labeling
  observeEvent(rv$is_anim_playing, {
    if (rv$is_anim_playing == TRUE) {
      updateActionButton(session, "anim_play", label = "pause")
    }
    if (rv$is_anim_playing == FALSE) {
      updateActionButton(session, "anim_play", label = "play")
    }
  })
  observeEvent(input$anim_play, { ## Button label switching and end of anim handling
    if (input$anim_slider == dim(rv$anim_basis_array)[3]) { ## if at the last slide, start at slide 1
      updateSliderInput(session, "anim_slider", value = 1)
    }
    rv$is_anim_playing <- !rv$is_anim_playing
  })
  
  ##### _Gallery obs -----
  ### Plot button (gallery)
  observeEvent(input$gallery_plot, {
    selectedRow <- as.numeric(strsplit(input$gallery_plot, "_")[[1]][2])
    g_bas <- rv$gallery_bases[selectedRow, which(colnames(rv$gallery_bases) == "basis")][[1]]
    format_basis()(g_bas)
    
    output$gallery_msg <- renderText(
      paste0("The basis from row ", selectedRow, " has been sent to the 'Manual tour' tab.", sep = ""))
  })
  
  ### Save button (gallery)
  observeEvent(input$gallery_save, {
    rv$png_save_cnt <- rv$png_save_cnt + 1
    selectedRow <- as.numeric(strsplit(input$gallery_save, "_")[[1]][2])
    save_file <- sprintf("spinifexApp_basis%03d", rv$png_save_cnt)
    save_basis <- 
      rv$gallery_bases[selectedRow, which(colnames(rv$gallery_bases) == "basis")][[1]]
    
    write.csv(save_basis, file = paste0(save_file, ".csv"), row.names = FALSE)
    gg_out <- spinifex::oblique_frame(data = projMat(),
                                      basis = save_basis,
                                      manip_var = manip_num(),
                                      theta = 0,
                                      phi = 0,
                                      col = col_of(col_var()),
                                      pch = pch_of(pch_var()),
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
  
  ### Development display -- can be toggled by setting .include_dev_display at the top of global.R
  observeEvent(input$browser, {browser()})
  
  ###### OUTPUTS -----
  ### Data tab
  output$rawDat_summary <- renderPrint({
    if (rawData_assumptions() == FALSE) return()
    tibble::tibble(rawDat())
  })
  output$projMat_summary <- renderPrint({
    if (projData_assumptions() == FALSE) return()
    proj <- as.data.frame(projMat())
    tibble::tibble(proj)
  })
  ### Main tab
  output$curr_basis_tbl  <- renderTable({
    if (basis_assumptions() == FALSE) return()
    as.data.frame(rv$curr_basis)
  }, 
    striped  = TRUE,  
    spacing  = 'l',  
    rownames = TRUE,  
    digits   = 2
  )
  output$main_plot <- renderPlot({suppressMessages(main_plot())})
  ### Gallery tab
  output$gallery_df <- DT::renderDataTable(
    gallery_df(), server = FALSE, escape = FALSE, selection = 'none', 
    options = list(dom = 't', pageLength = 100)
  )
  output$gallery_icons <- renderPlot(
    gallery_icons(), width = 97,
    height = function(){
      if (gallery_basis_assumptions() == FALSE) {return(1)}
      n_icons <- nrow(rv$gallery_bases[!rownames(rv$gallery_bases) %in% rv$gallery_rows_removed, ])
      97 * n_icons
    }
  )
  ### Dev display
  output$dev_msg <- renderPrint({
    if (.include_dev_display == TRUE){
      cat("Dev msg -- \n",
          "rv$curr_basis: ",         rv$curr_basis, "\n",
          "is null? ",               is.null(rv$curr_basis), "\n",
          "init_basis(): ",          init_basis(), "\n",
          "input$manip_nm: ",        input$manip_nm, "\n",
          "manip_num(): ",           manip_num(), "\n",
          "rv$gallery_bases: ",      unlist(rv$gallery_bases), "\n",
          "is.null? ",               is.null(rv$gallery_bases), "\n",
          "input$pp_type: ",         input$pp_type, "\n",
          "input$anim_pp_cluster: ", input$anim_pp_cluster, "\n",
          sep = ""
      )
    }
  })
  
  
}

shinyApp(ui = ui, server = server)


