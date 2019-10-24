library(ggplot2)
library(tourr)
library(spinifex)

m_var <- 1

server <- function(input, output, session) {
  ##### Initialize ----
  rv <- reactiveValues() # rv needed for x,y,r sliders
  rv$slider <- NULL
  rv$curr_basis <- NULL
  
  manip_var_num <- reactive({1})
  
  myPlot <- reactive({
    if (is.null(rv$curr_basis)) {
      rb <- tourr::basis_random(6, 2)
      rownames(rb) <- colnames(tourr::flea[, 1:6])
      rv$curr_basis <- rb
    } # init curr_basis
    # Init
    dat_std <- tourr::rescale(tourr::flea[,1:6])
    col <- pch <- tourr::flea$species
    m_var <- manip_var_num()
    
    return(oblique_frame(data      = dat_std, 
                         basis     = rv$curr_basis, 
                         manip_var = m_var, 
                         theta     = 0, # perform rotation when setting rv$curr_basis
                         phi       = 0, 
                         col       = col,
                         pch       = pch,
                         axes      = "bottomleft",
                         alpha     = 1))
  })
  
  #basis_obl <- reactive({
  observe({
    #browser()
    if(is.null(rv$curr_basis)) {
      rb <- tourr::basis_random(6, 2)
      rownames(rb) <- colnames(tourr::flea[, 1:6])
      rv$curr_basis <- rb
    }
    theta <- phi <- NULL
    mv_sp <- create_manip_space(rv$curr_basis, manip_var_num())[manip_var_num(), ]
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
    ret <- oblique_basis(basis = rv$curr_basis, manip_var = manip_var_num(),
                         theta = theta, phi = phi)
    row.names(ret) <- colnames(tourr::flea[,1:6])
    
    rv$curr_basis <- ret
  })
  

  observeEvent(input$obl_run, {
    rv$curr_basis <- NULL
    rb <- tourr::basis_random(6, 2)
    rownames(rb) <- colnames(tourr::flea[, 1:6])
    rv$curr_basis <- rb
  })
  
  output$obl_plot <- renderPlot(myPlot())
  output$curr_basis <- renderTable(rv$curr_basis, rownames = T)
  
  ### Observe sliders
  observe({
    rv$x <- input$x_slider
    rv$y <- input$y_slider
    rv$rad <- input$rad_slider
  })
  
  observe({
    this_val <-  .5
    updateSliderInput(session, "manip_slider", value = this_val)
  })
  
  ### Development help -- uncomment message at bottom on ui to use
  output$dev_msg <- renderPrint({
    cat("Dev msg -- \n",
        "x_slider: ", input$x_slider, "\n",
        "y_slider: ", input$y_slider, "\n",
        "rad_slider: ", input$rad_slider, "\n",
        "rv:x: ", rv$x, "\n",
        "rv:y: ", rv$y, "\n",
        sep = "")
  })
}


###### UI ----
ui <- fluidPage(
  selectInput("manip_type", "Type of manipulation", 
              choices = c("Horizontal", "Vertical", "Radial"))
  , sliderInput("manip_slider", "magnitude",
              min = 0, max = 1, value = 0, step = .1)
  , actionButton("obl_run", "Random basis")
  , plotOutput("obl_plot")
  , tableOutput("curr_basis")
  , verbatimTextOutput("dev_msg")
)


shinyApp(ui, server)

