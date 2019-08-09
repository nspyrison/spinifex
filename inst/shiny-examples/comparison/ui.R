### Input tab ----
tabInput <- tabPanel(
  "Input", fluidPage(
    sidebarPanel(
      # Input csv file
      fileInput("file", "Data file (.csv format)",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
      ),
      # include which variables
      checkboxGroupInput(
        "variables",
        label = "Variables to include",
        choices = names(tourr::flea[, 1:6]),
        selected = names(tourr::flea[, 1:6])
      ),
      # Point color, shape and rescale [0,1] data
      selectInput('col_var', 'Point color', "<none>"),
      selectInput('pch_var', 'Point shape', "<none>"),
      checkboxInput("rescale_data", "Rescale values to [0, 1]", value = TRUE)
    ),
    mainPanel(h3("Data structure"),
              verbatimTextOutput("str_data")
              #,verbatimTextOutput("devMessage3")
    )
    
  )
)


### Manual tab (radial) ----
tabManual <-  tabPanel(
  "Tours", fluidPage(
    sidebarPanel(
      # generate tour button
      actionButton("button", "Run animation"),
      actionButton("obl_button", "Run interactive"),
      # basis init
      radioButtons("basis_init", "Start basis",
                   choices = c("Random", "PCA", "Projection pursuit", "From file"),
                   selected = "Random"),
      ## Projection pursuit options
      conditionalPanel("input.basis_init == 'Projection pursuit'",
                       selectInput("pp_type", "Pursuit index", guidedTourOptions)
      ),
      ## From file options
      conditionalPanel(
        "input.basis_init == 'From file'",
        fileInput("basispath", "Basis file (.csv or .rda, [p x 2] matrix)",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
      ),
      # manip var, axes, angle, save
      selectInput('manip_var', 'Manip var', "none"),
      selectInput('manip_type', "Manipulation type",
                  c("horizontal", "vertical", "radial")
      ),
      # Slider controls
      conditionalPanel("input.manip_type == 'horizontal'",
                       sliderInput("x_slider", "Horizontal",
                                   min = -1, max = 1, value = 0, step = .1)
      ),
      conditionalPanel("input.manip_type == 'vertical'",
                       sliderInput("y_slider", "Vertical",
                                   min = -1, max = 1, value = 0, step = .1)
      ),
      conditionalPanel("input.manip_type == 'radial'",
                       sliderInput("rad_slider", "Radial",
                                   min = 0, max = 1, value = .5, step = .05)
      ),
      selectInput('axes', 'Reference axes location', 
                  c('center', 'bottomleft', 'off'),
                  'center',  multiple = FALSE
      ),
      sliderInput('alpha', "Alpha opacity", min = 0, max = 1, value = 1, step = .1),
      sliderInput('angle', 'Angle step size (animation only)', value = .05, min = .01, max = .3),
      numericInput("basistosave", "Basis to save (.csv)", 1,
                   min = 1, max = 1000),
      actionButton("radial_save", "Save"),
      h5("Save current basis (.csv)"),
      actionButton("obl_save", "Save")
    ),
    # Output display
    mainPanel(
      plotlyOutput("main_plot"),
      plotOutput("main_plot2"),
      h4("Current basis"),
      tableOutput("curr_basis_out"),
      conditionalPanel("input.radial_save",
                       h4("Last basis saved"),
                       tableOutput("last_save")
      )
    )
  )
)

### Static tab ----
tabStatic <- tabPanel(
  "Static projections", fluidPage(
    sidebarPanel(
      actionButton("static_button", "Run"),
      radioButtons("static_method", "Projection",
                   choices = c("PCA", "SPLOM"), # others to consider: k-means, LDA, MDS, TNSE, Sammon's mapping, etc
                   selected = "PCA"),
      sliderInput("static_alpha", "Alpha opacity", min = 0, max = 1, value = 1, step = .1)
    ),
    mainPanel(
      plotOutput("static_plot")
    )
  )
)


### Tabs combined ----
ui <- fluidPage(
  navbarPage(
    "Manual tours -- comparison",
    tabManual,
    tabInput
    #tabStatic
  )
   ,verbatimTextOutput("devMessage")
)

