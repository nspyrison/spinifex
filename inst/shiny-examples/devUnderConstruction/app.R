# Preamble ------
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

source('global.R', local = TRUE)
source('ui.R', local = TRUE)

server <- function(input, output, session) {
  ##### Init reactiveValues
  rv                      <- reactiveValues()
  rv$curr_basis           <- diag(6)[, 1:2]
  rv$tour_array           <- NULL
  rv$anim_playing         <- FALSE
  rv$anim_slide           <- 1
  rv$png_save_cnt         <- 0
  rv$gif_save_cnt         <- 0
  rv$gallery_bases        <- NULL
  rv$gallery_n_rows       <- 0
  rv$gallery_rows_removed <- NULL
  ### For anim_window
  rv$xmin <- 0
  rv$xmax <- 0
  rv$ymin <- 0
  rv$ymax <- 0
  
  ####### REACTERS -----
  ### _Data reactives -----
  inputPath_assumptions <- reactive({
    path <- input$basis_file$datapath
    ext <- tolower(substr(path, nchar(path) - 4 + 1, nchar(path)))
    if(is.null(path) | length(path) == 0) 
      M(inputPath_assumptions);return(FALSE)
    if(ext %in% c(".csv", ".rda")) 
      M(inputPath_assumptions);return(FALSE)
    TRUE
  })
  
  ### Raw input data
  rawDat <- reactive({
    if (input$data_source == "Example"){
      if (input$data_example == "flea" )return(tourr::flea)
      if (input$data_example == "wine" )return(spinifex::wine)
      if (input$data_example == "breastcancer" )return(tourr::brestcancer)
      if (input$data_example == "weather" )return(tourr::weather)
    }
    if (input$data_source == "Upload"){
      if (inputPath_assumptions() == FALSE) return(tourr::flea)
      return(read.csv(input$data_file$datapath, stringsAsFactors = FALSE))
    }
  })
  
  rawData_assumptions <- reactive({
    raw <- rawDat()
    if (is.null(raw) | length(raw) == 0) 
      {M(rawData_assumptions);return(FALSE)}
    TRUE
  })
  
  ### Data to project
  projDat <- reactive({
    if(rawData_assumptions() == FALSE)  return()
    if(projData_assumptions() == FALSE) return()
    
    ret <- rawDat()[input$projVars_nms]
    ret <- ret[complete.cases(ret), ] ## Rowwise complete
    #### Tranforms are beyond the scope right now.
    ## if (input$data_transform == "sphere") ret <- tourr::sphere_data(ret)
    ## if (input$data_transform == "PCA")    ret <- prcomp(ret)$rotation
    if (input$rescale_data) ret <- tourr::rescale(ret)
    
    if (!is.matrix(ret))    ret <- as.matrix(ret)
    return(ret)
  })
  
  projData_assumptions <- reactive({
    dat <- rawDat()
    if (is.null(dat) | length(dat) == 0) 
      {M(projData_assumptions);return(FALSE)}
    TRUE
  })
  
  projVars_defaultNms <- reactive({
    if(rawData_assumptions() == FALSE) return()
    dat <- projDat()
    nms <- colnames(dat)
    default_cols <- apply(dat, 2, function(x) {is.numeric(x)})
    nms[default_cols]
  })
  
  projVars_assumptions <- reactive({
    if(rawData_assumptions() == FALSE) return()
    varNms <- projVars_defaultNms()
    if (is.null(varNms) | length(varNms) == 0)
      {M(projVars_assumptions);return(FALSE)}
    TRUE
  })
  
  n <- reactive(nrow(projDat()))
  p <- reactive(ncol(projDat()))
  
  col_var <- reactive({
    if (input$col_nm == "<none>") return(rep("a", n())) ## Create dummy column in "<none>"
    dat <- rawDat()
    dat[input$col_nm]
  })
  
  pch_var <- reactive({
    if (input$pch_nm == "<none>") return(rep("a", n())) ## Create dummy column in "<none>"
    dat <- rawDat()
    dat[input$pch_nm]
  })
  
  manip_num <- reactive({ 
    if (input$manip_nm == "<none>") {return(1)}
    if (input$disp_method == "animation") {return(1)}
    which(colnames(projDat()) == input$manip_nm)
  }) 
  
  manip_assumptions <- reactive({
    mNum <- manip_num()
    mSlider <- input$manip_slider
    if (is.null(mNum) | length(mNum) == 0) 
      {M(manip_assumptions);return(FALSE)}
    if (mNum %in% 1:nrow(projDat()) == FALSE)  
      {M(manip_assumptions);return(FALSE)}
    if (mSlider < 0 | mSlider > 1) 
      {M(manip_assumptions);return(FALSE)}
    TRUE
  })
  
  pch_assumptions <- reactive({
    pchVarNm <- input$pch_nm
    if (is.null(pchVarNm) | length(pchVarNm) != 0) 
      {M(pch_assumptions);return(FALSE)}
    if (pchVarNm %in% c(colnames(rawDat()), "<none>") == FALSE) 
      {M(pch_assumptions);return(FALSE)}
    TRUE
  })
  
  col_assumptions <- reactive({
    colVarNm <- input$col_nm
    if (is.null(colVarNm) | length(colVarNm) != 0) 
      {M(col_assumptions);return(FALSE)}
    if ((colVarNm %in% c(colnames(rawDat()), "<none>")) == FALSE) 
      {M(col_assumptions);return(FALSE)}
    TRUE
  })
  
  
  ###### Basis reactives
  ### basis
  init_basis <- reactive({
    ### Condition handling
    if(projData_assumptions() == FALSE) return()
    if (input$basis_init == "Identity") ret <- diag(p())[, 1:2]
    if (input$basis_init == "Random")   ret <- tourr::basis_random(n = p(), d = 2)
    if (input$basis_init == "PCA")      ret <- prcomp(projDat())[[2]][, 1:2]
    if (input$basis_init == "From file") {
      if(inputPath_assumptions() == FALSE) return()
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
      pp_cluster <- NA
      if (input$pp_type %in% c("lda_pp", "pda_pp")){
        dat <- rawDat()
        pp_cluster <- dat[input$pp_cluster]
      }
      tour_func <- appGetGuidedTour(input$pp_type, pp_cluster)
      tour_hist <- save_history(projDat(), tour_func)
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
  
  gallery_basis_assumptions <- reactive({
    bas <- rv$gallery_bases
    if (is.null(bas) | length(bas) == 0) 
      {M(gallery_basis_assumptions);return(FALSE)}
    if (!is.numeric(bas)) 
      {M(gallery_basis_assumptions);return(FALSE)}
    if (nrow(df) == 0) 
      {M(gallery_basis_assumptions);return(FALSE)}
    TRUE
  })
  
  ### Add col and row names, and set rv$curr_basis.
  format_basis <- reactive({
    format_basis_func <- function(bas) {
      if(basis_assumptions() == FALSE) return()
      colnames(bas)  <- c("x", "y")
      row.names(bas) <- colnames(projDat())
      
      rv$curr_basis <- bas # can I assign rv$ from a reactive or use OBS?
      return(bas)
    }
  })
  
  ### Interactive and animated plot
  main_plot <- reactive({
    browser()
    if(basis_assumptions()     == FALSE) return()
    if(projData_assumptions()  == FALSE) return()
    if(manip_assumptions()     == FALSE) return()
    if(pch_assumptions()       == FALSE) return()
    if(col_assumptions()       == FALSE) return()
    
    if (input$disp_method == 'Interactive') {
      return(
        spinifex::oblique_frame(basis     = rv$curr_basis,
                                data      = projDat(),
                                manip_var = manip_num(),
                                theta     = 0,
                                phi       = 0,
                                col       = col_of(col_var()),
                                pch       = pch_of(pch_var()),
                                axes      = input$axes,
                                alpha     = input$alpha))
    } ## end of if (input$disp_method == 'Interactive')
    if (input$disp_method == 'Animation' ) {
      anim_frame <- NULL
      if (input$anim_type %in% c("Radial", "Horizontal", "Vertical")) {
        anim_frame <- spinifex::oblique_frame(basis     = rv$curr_basis,
                                           data      = projDat(),
                                           manip_var = manip_num(),
                                           theta     = 0,
                                           phi       = 0,
                                           col       = col_of(col_var()),
                                           pch       = pch_of(pch_var()),
                                           axes      = input$axes,
                                           alpha     = input$alpha)
      } else { ## For tourr funcs without manip var.
        anim_frame <- spinifex::view_basis(basis = rv$curr_basis,
                                        data  = projDat(),
                                        col   = col_of(col_var()),
                                        pch   = pch_of(pch_var()),
                                        axes  = input$axes,
                                        alpha = input$alpha)
      }
      return(anim_frame)
    } ## end of if (input$disp_method == 'Animation')
  })
  

  
  
  ### _Interactive reactives -----
  ### hor, vert, radial interactive motion 
  basis_obl <- reactive({
    if(basis_assumptions() == FALSE) return()
    if(manip_assumptions() == FALSE) return()
    theta <- phi <- NULL
    mv_sp <- create_manip_space(rv$curr_basis, manip_num())[manip_num(), ]
    if (input$manip_type == "Horizontal") {
      theta <- 0
      phi.x_zero <- atan(mv_sp[3] / mv_sp[1]) - (pi / 2 * sign(mv_sp[1]))
      phi <- input$manip_slider * pi/2 + phi.x_zero
    }
    if (input$manip_type == "Vertical") {
      theta <- pi/2
      phi.y_zero <- atan(mv_sp[3] / mv_sp[2]) - (pi / 2 * sign(mv_sp[2]))
      phi <- input$manip_slider * pi/2 + phi.y_zero
    }
    if (input$manip_type == "Radial") {
      theta <- atan(mv_sp[2] / mv_sp[1])
      phi_start <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
      phi <- (acos(input$manip_slider) - phi_start) * - sign(mv_sp[1])
    }
    browser()
    ret <- oblique_basis(basis = rv$curr_basis, manip_var = manip_num(),
                         theta = theta, phi = phi)
    
    ret <- format_basis()(ret)
    return(ret)
  })
  
  ### Data output
  output$rawDat_summary  <- renderPrint({
    if (rawData_assumptions() == FALSE) return()
    tibble::tibble(rawDat())
  })
  output$projDat_summary <- renderPrint({
    if (projData_assumptions() == FALSE) return()
    tibble::tibble(projDat())
  })
  output$curr_basis_tbl  <- renderTable({
    if(basis_assumptions() == FALSE) return()
    round(rv$curr_basis, 2)
  })
  output$main_plot       <- renderPlot({
    suppressMessages(main_plot() + 
                       xlim(-1, 1) +
                       ylim(-1, 1)
    )
  })
  
  ##### _Gallery reactives -----
  gallery_df <- reactive({
    if (gallery_basis_assumptions() == FALSE) {
      output$gallery_msg <- renderPrint(cat("Send a basis to the gallery."))
      return()
    }
    
    full_gallery_df = data.frame(
      Label = 
        appShinyInput(textInput, 
                      nrow(rv$gallery_bases), 'text_', label = "",
                      placeholder = "user label", width = '108px'),
      Plot = 
        appShinyInput(actionButton, 
                      nrow(rv$gallery_bases), 'button_', label = "Plot", 
                      onclick = 'Shiny.onInputChange(\"gallery_plot\",  this.id)'),
      `Save (csv & png)` =
        appShinyInput(actionButton, 
                      nrow(rv$gallery_bases), 'button_', label = "Save", 
                      onclick = 'Shiny.onInputChange(\"gallery_save\",  this.id)'),
      Delete = 
        appShinyInput(actionButton, nrow(rv$gallery_bases), 'button_', label = "Remove", 
                      onclick = 'Shiny.onInputChange(\"gallery_delete\",  this.id)'),
      rv$gallery_bases[, -which(colnames(rv$gallery_bases) %in% c("basis", "Id"))],
      stringsAsFactors = FALSE,
      row.names = NULL,
      check.names = FALSE
    )
    display_gallery_df <- full_gallery_df[!rownames(full_gallery_df) %in% rv$gallery_rows_removed, ]
    
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
    
    df <- rv$gallery_bases[!rownames(rv$gallery_bases) %in% rv$gallery_rows_removed, ]
    n_bases <- nrow(df)
    n <- n()
    p <- p()
    
    df_gg_data <- NULL # Unlist basis into p rows
    for (i in 1:n_bases){
      rows <- data.frame(id = rep(df$Id[i], n),
                         projDat() %*% df$basis[[i]],
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
  ##### _Data observes ----
  # ### Set rv$curr_basis when init_basis() changes
  # observeEvent(init_basis(),{ ##TODO Doesn't this defeat the purpose of a re_init button?
  #   format_basis()(init_basis())
  # })
  
  ### Update input$projVars_nms when rawDat() changes.
  observeEvent({rawDat()}, {
    appDebugMsg(rawDat)
    
    updateCheckboxGroupInput(session,
                             inputId  = "projVars_nms",
                             choices  = names(rawDat()),
                             selected = projVars_defaultNms())
  })
  
  ### Update input$manip_var choices when input$projVariables changes.
  observeEvent({input$projVariables}, {
    appDebugMsg(input$projVariables)
    
    updateSelectInput(session,
                      inputId  = "manip_var", 
                      choices  = input$projVar, 
                      selected = input$projVar[1])
  })
  
  ### Update col, pch,and pp choices when rawDat() changes
  observeEvent({rawDat()}, {
    appDebugMsg(rawDat)
    
    dat <- rawDat()
    nms <- names(dat)
    pos_cols <- sapply(dat, function(x){is.factor(x)})
    if (length(pos_cols) == 0) 
      pos_cols <- sapply(dat, function(x){is.character(x)})
    if (length(pos_cols) == 0) 
      pos_cols <- sapply(dat, function(x){length(unique(x)) < 10})
    if (length(pos_cols) == 0) {selec <- "<none>"
    } else {selec <- nms[pos_cols][1]}
    choice <- c("<none>", nms)
   
    updateSelectInput(session,
                      inputId  = "col_var",
                      choices  = choice,
                      selected = selec)
    updateSelectInput(session,
                      inputId  = "pch_var",
                      choices  = choice,
                      selected = selec)
    updateSelectInput(session,
                      inputId  = "pp_cluster",
                      choices  = choice,
                      selected = selec)
    updateSelectInput(session, 
                      inputId  = "anim_pp_cluster", 
                      choices  = choice,
                      selected = selec)
  })
  
  ### On "Initiaze to above parameters" button, set current basis to the initializ basis.
  observeEvent(input$re_init, {
    appDebugMsg(input$re_init)
    init_basis()
  })
  
  ### Save current basis (interactive)
  observeEvent(input$save, {
    appDebugMsg(input$save)
    if (basis_assumptions == FALSE) return()
    
    rv$png_save_cnt <- rv$png_save_cnt + 1
    save_file <- sprintf("spinifexApp_basis%03d", rv$png_save_cnt)
    write.csv(rv$curr_basis, file = paste0(save_file, ".csv"), row.names = FALSE)
    
    gg_out <- spinifex::oblique_frame(data = projDat(),
                                      basis = rv$curr_basis,
                                      manip_var = manip_num(),
                                      theta = 0,
                                      phi = 0,
                                      col = col_of(col_var()),
                                      pch = pch_of(pch_var()),
                                      axes = input$axes,
                                      alpha = input$alpha)
    ggplot2::ggsave(paste0(save_file, ".png"), gg_out,
                    width = 4, height = 4, units = "in")
    
    output$save_msg <- renderPrint(paste0(
      "Basis saved as ", save_file, " (csv & png)."))
  })
  
  ### Send current basis to gallery
  observeEvent(input$to_gallery, {
    appDebugMsg(input$to_gallery)
    if (basis_assumptions()) return()
    
    rv$gallery_n_rows <- rv$gallery_n_rows + 1
    gallery_row <- data.frame(Id = rv$gallery_n_rows,
                              `Manip var`  = input$manip_nm, 
                              `Manip type` = input$manip_type, 
                              `Time saved` = substr(Sys.time(), 12, 19),
                              check.names = FALSE)
    
    gallery_row$basis <- list(rv$curr_basis)
    rv$gallery_bases <- rbind(rv$gallery_bases, gallery_row)
    
    output$save_msg <- renderPrint(cat(
      "Basis sent to the gallery as row ", rv$gallery_n_rows, ".", sep = ""))
  })
  
  

  
  ##### _Interactive observes -----
  ### Slider values
  observeEvent(input$manip_slider, {basis_obl()})
  
  ### Update sliders
  observeEvent({
    rv$curr_basis
    manip_num()
  }, {
    if (input$disp_method != 'Interactive') return()
    if(basis_assumptions() == FALSE)      return()
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
  
  
  ##### _Animation observes -----
  ### Set rv$tour_array when related paramater changes.
  observeEvent({
    projDat()
    input$anim_type
    ## spinifex parameter change:
    manip_num()
    input$anim_angle
    ## tourr param parameters change:
    input$pp_type
    projDat()
    }, {
      if (input$disp_method != "animation") return()
      ### Processing message for gif
      # withProgress(message = 'Rendering animation ...', value = 0, {
      ## Manual tour animation
      app_manual_tour <- function(...) { ## For code reduction, to handle different theta values.
        manual_tour(basis = init_basis(), ##TODO: should be rv$currbasis??? 
                    data = projDat(), 
                    manip_var = manip_num(),
                    angle = input$anim_angle,
                    ...) ## Allows theta to vary
      }
      if (input$anim_type %in% c("Radial", "Horizontal", "Vertical")) {
        t_array <- NULL 
        if (input$anim_type == "Radial")     {t_array <- app_manual_tour()} # Default theta to radial
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
            dat <- rawDat()
            pp_cluster <- dat[input$anim_pp_cluster]
          }
          tour_func <- appGetGuidedTour(input$anim_pp_type, pp_cluster)
          t_path <- tourr::save_history(projDat(), tour_func, 
                                        start = rv$curr_basis)
        }
        # Grand, little and local tours
        if(input$anim_type == "Grand (6 bases)") {
          t_path <- tourr::save_history(projDat(), grand_tour(),
                                        max_bases = 6, start = rv$curr_basis)
        }
        if(input$anim_type == "Little (6 bases)") {
          t_path <- tourr::save_history(projDat(), little_tour(),
                                        max_bases = 6, start = rv$curr_basis)
        }
        if(input$anim_type == "Local (6 bases)") {
          t_path <- tourr::save_history(projDat(), max_bases = 6,
                                        tour_path = local_tour(rv$curr_basis, pi/4))
        }
        rv$tour_array <- interpolate(t_path, angle = input$anim_angle)
      }
    ### end processing message for .gif 
    # setProgress(1)
    # })
  })
  
  ### Update anim_slider when rv$tour_array changes.
  observeEvent(rv$tour_array, {
    if (input$disp_method != "animation") return()
    rv$anim_playing <- FALSE
    updateSliderInput(session, "anim_slider", 
                      value = 1, 
                      min = 1, 
                      max = dim(rv$tour_array)[3])
  })
  
  ### Set curr basis when anim_slider changes.
  observeEvent(input$anim_slider, {
    if(input$disp_method != "animation") return()
    if(basis_assumptions() == FALSE)     return()
    format_basis()(matrix(rv$tour_array[,, input$anim_slider], ncol = 2))
  })
  
  ### Change curr basis when slider changes
  observeEvent(input$anim_slider, {
    if(input$disp_method != "animation") return()
    if(basis_assumptions() == FALSE)     return()
    format_basis()(matrix(rv$tour_array[,, input$anim_slider],  ncol = 2 ))
  })
  

  ### Save animation to .gif
  observeEvent(input$anim_save, {
    if (input$disp_method != "animation") return()
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
    play_tour_path(tour_path = rv$tour_array, data = projDat(),
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
    ex2 <- play_tour_path(tour_path = rv$tour_array, data = projDat(),
                         render_type = render_gganimate)
    # gganimate::anim_save("myGif.gif", ex2)
    ##TODO: END
    
    
    withProgress(message = 'Rendering animation ...', value = 0, {
      anim <- play_tour_path(tour_path = rv$tour_array,
                             data = projDat(),
                             col = col_of(col_var()), pch = pch_of(pch_var()),
                             axes = input$axes,
                             alpha = input$alpha,
                             render_type = render_gganimate,
                             angle = input$angle,
                             fps = input$fps)
      save_file <- sprintf("spinifexApp_animation%03d.gif", rv$gif_save_cnt)
      gganimate::anim_save(save_file, anim)
    })
    setProgress(1)
    
    output$save_msg <- renderPrint(paste0("Animation saved as ", save_file, ".", sep=""))
  })
  
  ### Play animation button.
  observe({ ## Play anim while odd number of clicks
    invalidateLater(1000 / input$fps, session)
    isolate({
      if (rv$anim_playing == TRUE) {
        isolate(updateSliderInput(session, "anim_slider", 
                                   value = input$anim_slider + 1))
        if (input$anim_slider == dim(rv$tour_array)[3]) { ## pause on last slide 
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
  observeEvent(input$anim_play, { ## Button label switching and end of anim handling
    if (input$anim_slider == dim(rv$tour_array)[3]) { ## if at the last slide, start at slide 1
      updateSliderInput(session, "anim_slider", value = 1)
    }
    rv$anim_playing <- !rv$anim_playing
  })
  
  ##### _Gallery observes -----
  ### Plot button (gallery)
  observeEvent(input$gallery_plot, {
    selectedRow <- as.numeric(strsplit(input$gallery_plot, "_")[[1]][2])
    g_bas <- rv$gallery_bases[selectedRow, which(colnames(rv$gallery_bases) == "basis")][[1]]
    format_basis()(g_bas)
    
    output$gallery_msg <- renderText(
      paste0("The basis from row ", selectedRow, " has been sent to the Manual tour tab.", sep = ""))
  })
  
  ### Save button (gallery)
  observeEvent(input$gallery_save, {
    rv$png_save_cnt <- rv$png_save_cnt + 1
    selectedRow <- as.numeric(strsplit(input$gallery_save, "_")[[1]][2])
    save_file <- sprintf("spinifexApp_basis%03d", rv$png_save_cnt)
    save_basis <- 
      rv$gallery_bases[selectedRow, which(colnames(rv$gallery_bases) == "basis")][[1]]
    
    write.csv(save_basis, file = paste0(save_file, ".csv"), row.names = FALSE)
    gg_out <- spinifex::oblique_frame(data = projDat(),
                                      basis = save_basis,
                                      manip_var = manip_num(),
                                      theta = 0,
                                      phi = 0,
                                      col = col_of(col_var()),
                                      pch = pch_of(pch_var()),
                                      axes = input$axes,
                                      alpha = input$alpha)
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
  output$rawDat_summary  <- renderPrint({
    if (rawData_assumptions() == FALSE) return()
    tibble::tibble(rawDat())
  })
  output$projDat_summary <- renderPrint({
    if (projData_assumptions() == FALSE) return()
    tibble::tibble(projDat())
  })
  ### Main tab
  output$curr_basis_tbl  <- renderTable({
    if(basis_assumptions() == FALSE) return()
    round(rv$curr_basis, 2)
  })
  output$main_plot       <- renderPlot({
    suppressMessages(main_plot() + 
                       xlim(-1, 1) +
                       ylim(-1, 1)
    )
  })
  ### Gallery tab
  output$gallery_df <- DT::renderDataTable(
    gallery_df(), server = FALSE, escape = FALSE, selection = 'none', 
    options = list(dom = 't', pageLength = 100)
  )
  output$gallery_icons <- renderPlot(
    gallery_icons(), width = 84,
    height = function(){
      if (gallery_basis_assumptions() == FALSE) {return(1)}
      n_icons <- nrow(rv$gallery_bases[!rownames(rv$gallery_bases) %in% rv$gallery_rows_removed, ])
      84 * n_icons
    }
  )
  ### Dev display
  output$dev_msg <- renderPrint({
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
  })
  
  
}

shinyApp(ui, server)

