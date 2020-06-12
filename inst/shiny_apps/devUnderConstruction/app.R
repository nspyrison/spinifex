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
##TODO: remove all rv$is_manual_debounced lines?
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
  # rv$is_manual_debounced  <- TRUE 
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
  n <- reactive(nrow(selDat()))
  p <- reactive(ncol(selDat()))
  
  ### Throttle manip_slider
  manip_slider_index   <- reactive(1 + 10 * isolate(input$manip_slider)) ## input$manip_slider in [0,1]
  manip_slider_index_t <- throttle(manip_slider_index, 50) ## throttle time in milliseconds
  anim_slider_index    <- reactive(input$anim_slider) ## anim_slider in [1:n_bases]
  anim_slider_index_t  <- throttle(anim_slider_index, 50) ## throttle time in milliseconds
  
  sel_col <- reactive({
    colVarNm <- input$col_nm
    if (colVarNm == "<none>") {
      var <- rep("a", n()) ## Create dummy column in "<none>"
    } else {
      dat <- rawDat()
      var <- dat[, which(colnames(dat) == colVarNm)]
    }
    ret <- col_of(var)
    
    ## Assumptions and return
    if (length(levels(ret)) > 12) 
      warning(paste0("Selected col varable '", substitute(var), 
                     "' has more then 12 levels. Consider subseting to contain less than 12 levels."))
    if (is.null(colVarNm) | length(colVarNm) == 0) 
      M(sel_col);return(FALSE)
    if ((colVarNm %in% c(colnames(dat), "<none>")) == FALSE) 
      M(sel_col);return(FALSE)
    return(ret)
  })
  
  sel_pch <- reactive({
    pchVarNm <- input$pch_nm
    if (pchVarNm == "<none>") {
      var <- rep("a", n()) ## Create dummy column in "<none>"
    } else {
      dat <- rawDat()
      var <- dat[, which(colnames(dat) == pchVarNm)]
    }
    
    ret <- pch_of(var)
    ## Assumptions and return
    if (length(levels(ret)) > 12) 
      warning(paste0("Selected pch varable '", substitute(var), 
                     "' has more then 12 levels. Consider subseting to contain less than 12 levels."))
    if (is.null(pchVarNm) | length(pchVarNm) == 0) 
      M(sel_pch);return(FALSE)
    if (pchVarNm %in% c(colnames(dat), "<none>") == FALSE) 
      M(sel_pch);return(FALSE)
    return(ret)
  })
  
  manip_num <- reactive({ 
    if (input$manip_nm == "<none>") {return(1)}
    if (input$tour_class == "Guided") {return(1)}
    selDat <- selDat()
    mNum <- which(colnames(selDat) == input$manip_nm)
    
    ## Assumptions and return
    if (is.null(mNum) | length(mNum) == 0) 
      M(manip_num);return(FALSE)
    if (mNum < 1 | mNum > ncol(selDat))  
      M(manip_num);return(FALSE)
    return(mNum)
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
      ext <- tolower(substr(path, nchar(path) - 4 + 1, nchar(path)))
      ## Assumtions on input$data_file
      if ((is.null(path) | length(path) == 0) |
          (ext %in% c(".csv", ".rda") == FALSE)) {
        output$data_msg <- renderPrint(paste0(
          "Data file: '", path, "' failed to upload."))
        M(init_basis);return(FALSE)
      }
      
      if (ext == ".csv")
        return(read.csv(path, stringsAsFactors = FALSE))
      if (ext == ".rda")
        return(load(file = path))
    }
  })
  
  rawDat_assumptions <- reactive({
    raw <- rawDat()
    if (is.null(raw) | length(raw) == 0) 
      {M(rawDat_assumptions);return(FALSE)}
    TRUE
  })
  
  ## Selected data to project
  selDat <- reactive({
    if (rawDat_assumptions()  == FALSE) return()
    if (selDat() == FALSE) return()
    
    dat <- rawDat()
    dat <- dat[, which(colnames(dat) %in% input$projVar_nms)]
    dat <- dat[complete.cases(dat), ] ## Row-wise complete
    if (input$rescale_data) dat <- tourr::rescale(dat)
    if (!is.matrix(dat))    dat <- as.matrix(dat)
    
    ## Assumptions and return
    if (is.null(dat) | length(dat) == 0)
      M(selDat);return(FALSE)
    return(dat)
  })
  
  selNumVar_inputDefaultNms <- reactive({
    if (rawDat_assumptions() == FALSE) return()
    dat <- selDat()
    numVar_nms <- sapply(dat, is.numeric)
    .m <- min(6, length(numVar_nms))
    ret <- colnames(dat[, 1:.m])
    
    ## Assumptions and return
    if (is.null(ret) | length(ret) == 0)
      M(selNumVar_assumptions);return(FALSE)
    ret 
  })
  
  
  
  #### _Basis react -----
  init_basis <- reactive({
    ### Condition handling
    if (selDat() == FALSE) return()
    ## Causes issue for manual tour; due to othonormalization of manip sp:
    if (input$basis_init == "Identity") ret <- diag(p())[, 1:2] 
    if (input$basis_init == "Random")   ret <- tourr::basis_random(n = p(), d = 2)
    if (input$basis_init == "PCA")      ret <- prcomp(selDat())[[2]][, 1:2]
    if (input$basis_init == "From file") {
      ## Assumtions on input$basis_init
      path <- input$basis_init$datapath
      ext <- tolower(substr(path, nchar(path) - 4 + 1, nchar(path)))
      if (is.null(path) | length(path) == 0)
        M(init_basis);return(FALSE)
      if (ext %in% c(".csv", ".rda") == FALSE)
        M(init_basis);return(FALSE)
      
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
      tour_hist <- save_history(selDat(), tour_func)
      tour_len  <- dim(tour_hist)[3]
      ret <- matrix(as.numeric(tour_hist[,, tour_len]), ncol = 2)
    }
    
    return(format_basis()(ret))
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
      colnames(bas)  <- c("x", "y")
      row.names(bas) <- colnames(selDat())
      
      rv$curr_basis <- bas
      return(bas)
    }
  })
  
  ### _main_plot react ------
  main_plot <- reactive({
    if (basis_assumptions() == FALSE) return()
    if (selDat()            == FALSE) return()
    if (manip_num()         == FALSE) return()
    if (sel_pch()           == FALSE) return()
    if (sel_col()           == FALSE) return()
    
    if (input$tour_class == 'Manual') {
      dat <- selDat()
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
        phi_opts <- acos((0:10)/10) ## possible values of manip_slider.
        phi_vals  <- (phi_opts - phi_start) * - sign(mv_sp[1]) 
        
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
                                    col       = sel_col(),
                                    pch       = sel_pch(),
                                    axes      = "right")
        }
      } ### End rv$manual_basis_array if null
      .slider_index <- isolate(manip_slider_index_t())
      format_basis()(rv$manual_basis_array[,, .slider_index])
      return(rv$manual_plot_array[,, .slider_index])
    }
    if (input$tour_class == 'Guided') {
      anim_frame <- NULL
      if (input$anim_type %in% c("Radial", "Horizontal", "Vertical")) {
        anim_frame <- spinifex::oblique_frame(basis     = rv$curr_basis,
                                              data      = selDat(),
                                              manip_var = manip_num(),
                                              col       = sel_col(),
                                              pch       = sel_pch(),
                                              axes      = "right")
      } else { ## For tourr funcs without manip var.
        anim_frame <- spinifex::view_basis(basis = rv$curr_basis,
                                           data  = selDat(),
                                           col   = sel_col(),
                                           pch   = sel_pch(),
                                           axes  = "right")
      }
      return(anim_frame)
    } ## end of if (input$tour_class == 'Guided')
  })
  
  
  ##### _Gallery react -----
  gallery_basis_assumptions <- reactive({
    gal_bas <- rv$gallery_bases
    if (is.null(gal_bas)){ ## Can be length 0 if all rows removed
      output$gallery_msg <- renderPrint(cat("Send a basis to the gallery."))
      M(gallery_basis_assumptions);return(FALSE)
    }
    if (length(gal_bas) == 0){ ## If all bases removed, change gallery message, but return TRUE.
      output$gallery_msg <- renderPrint(cat("All bases removed, send a basis to the gallery."))
    }
    TRUE
  })
  
  gallery_df <- reactive({
    if (gallery_basis_assumptions() == FALSE) return()
    gal_bas <- rv$gallery_bases
    n_bases <- nrow(gal_bas)
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
    subset_gallery_df <- 
      full_gallery_df[!rownames(full_gallery_df) %in% rv$gallery_rows_removed, ]
    
    output$gallery_msg <- NULL
    return(subset_gallery_df)
  })
  
  ##### Gallery icons with data
  gallery_icons <- reactive({
    if (gallery_basis_assumptions() == FALSE) return()
    
    full <- rv$gallery_bases
    subset <- full[!rownames(full) %in% rv$gallery_rows_removed, ]
    n_bases <- nrow(subset)
    n <- n()
    
    ## Unlist bases
    icons_df <- NULL
    for (i in 1:n_bases){
      .rows <- data.frame(id = rep(subset$Id[i], n),
                         selDat() %*% subset$basis[[i]],
                         col = sel_col())
      icons_df <- rbind(icons_df, .rows)
    }
    output$gallery_icons_str <- renderText(str(icons_df))
    
    ## Add data points to gallery icons
    ggplot2::ggplot() +
      ggplot2::scale_color_brewer(palette = "Dark2") +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::coord_fixed() +
      ggplot2::geom_point(data = icons_df, size = .3,
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
    dat <- selDat()
    nms <- names(dat)
    rv$manual_basis_array <- NULL ## clear manual bases
    
    ### ... update projection variable choices
    updateCheckboxGroupInput(session,
                             inputId  = "projVar_nms",
                             choices  = nms,
                             selected = rawNumVar_inputDefaultNms())
    
    ### ... update aes choices
    pos_cols <- sapply(dat, is.factor)
    if (length(pos_cols) == 0) 
      pos_cols <- sapply(dat, is.character)
    if (length(pos_cols) == 0) 
      pos_cols <- sapply(dat, length(unique(x)) < 13)
    
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
    
    ### ...update manip var choices 
    updateSelectInput(session,
                      inputId  = "manip_nm", 
                      choices  = input$projVar_nms, 
                      selected = input$projVar_nms[1])
  })
  
  ### Save current basis (Manual)
  observeEvent(input$save, {
    appObsMsg("input$save")
    if (basis_assumptions == FALSE) return()
    
    rv$png_save_cnt <- rv$png_save_cnt + 1
    save_file <- sprintf("spinifexApp_basis%03d", rv$png_save_cnt)
    write.csv(rv$curr_basis, file = paste0(save_file, ".csv"), row.names = FALSE)
    
    gg_out <- spinifex::oblique_frame(data = selDat(),
                                      basis = rv$curr_basis,
                                      manip_var = manip_num(),
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
  # observe({
  #   if (input$tour_class != "Manual") return()
  #   if (rv$is_manual_debounced == TRUE) {
  #     invalidateLater(2000, session)
  #     isolate({
  #       rv$is_manual_debounced <- FALSE
  #     })
  #   }
  # })
  
  ### Update sliders
  observeEvent({
    rv$curr_basis
    manip_num()
  }, {
    # message(paste0("rv$is_manual_debounced: ", rv$is_manual_debounced))
    if (input$tour_class != "Manual")   return()
    if (basis_assumptions() == FALSE)   return()
    #if (rv$is_manual_debounced == TRUE) return()
    # rv$is_manual_debounced <<- TRUE
    appObsMsg("rv$curr_basis, manip_num()")
    mNum <- manip_num()
    mv_sp <- create_manip_space(rv$curr_basis, mNum)[mNum, ]
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
      browser()
      isolate(updateSliderInput(session, "manip_slider", value = rad_val))
      ## Changes rv$currbasis, which inturn tries to change this manip slider here.
    }
  })
  
  
  ##### _Guided tour obs -----
  ### When related paramaters change...
  observeEvent({
    selDat()
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
        if (selDat() == FALSE) return()

        manual_tour(basis = rv$currbasis,
                    data  = selDat(), 
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
          t_path <- tourr::save_history(selDat(), tour_func, 
                                        start = rv$curr_basis)
        }
        # Grand, little and local tours
        if (input$anim_type == "Grand (6 bases)") {
          t_path <- tourr::save_history(selDat(), grand_tour(),
                                        max_bases = 6, start = rv$curr_basis)
        }
        if (input$anim_type == "Little (6 bases)") {
          t_path <- tourr::save_history(selDat(), little_tour(),
                                        max_bases = 6, start = rv$curr_basis)
        }
        if (input$anim_type == "Local (6 bases)") {
          t_path <- tourr::save_history(selDat(), max_bases = 6,
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
    play_tour_path(tour_path = rv$anim_basis_array, data = selDat(),
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
    ex2 <- play_tour_path(tour_path = rv$anim_basis_array, data = selDat(),
                         render_type = render_gganimate)
    # gganimate::anim_save("myGif.gif", ex2)
    ##TODO: END
    
    
    withProgress(message = 'Rendering guided tour ...', value = 0, {
      anim <- play_tour_path(tour_path = rv$anim_basis_array,
                             data = selDat(),
                             col = sel_col(), pch = sel_pch(),
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
    gg_out <- spinifex::oblique_frame(data = selDat(),
                                      basis = save_basis,
                                      manip_var = manip_num(),
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
  
  ### Development display -- can be toggled by setting .include_dev_display at the top of global.R
  observeEvent(input$browser, {browser()})
  
  ###### OUTPUTS -----
  ### Data tab
  output$rawDat_summary <- renderPrint({
    if (rawDat_assumptions() == FALSE) return()
    tibble::tibble(as.data.frame(rawDat()))
  })
  output$selDat_summary <- renderPrint({
    if (selDat() == FALSE) return()
    tibble::tibble(as.data.frame(selDat()))
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


