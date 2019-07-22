

### Input tab
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
        choices = "none"
      ),
      # Point color, shape and rescale [0,1] data
      selectInput('col_var', 'Point color', "none"),
      selectInput('pch_var', 'Point shape', "none"),
      tags$hr(),
      checkboxInput("rescale_data", "Rescale values", value = TRUE)
    ),
    mainPanel(h3("Data structure"),
              verbatimTextOutput("str_data")
              #,verbatimTextOutput("devMessage3")
    )
    
  )
)


### Radial tab
tabRadial <-  tabPanel(
  "Radial", fluidPage(
    sidebarPanel(
      # generate tour button
      actionButton("radial_button", "Generate tour"),
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
      tags$hr(),
      selectInput('axes', 'Reference axes location', c('center', 'bottomleft', 'off'),
                  'center',  multiple = FALSE),
      sliderInput('angle', 'Angle step size', value = .05, min = .01, max = .3),
      tags$hr(),
      numericInput("basistosave", "Basis to save", 1,
                   min = 1, max = 1000),
      actionButton("radial_save", "Save basis")
    ),
    # Output display
    mainPanel(
      plotlyOutput("plotlyAnim"),
      conditionalPanel("input.radial_save",
                       h4("Last basis saved"),
                       tableOutput("last_save")
      )
    )
  )
)

### Static tab
tabStatic <- tabPanel(
  "Static projections", fluidPage(
    sidebarPanel(
      actionButton("static_button", "Generate projection"),
      tags$hr(),
      radioButtons("static_method", "Linear projection technique",
                   choices = c("PCA", "LDA (holder)", "SPLOM"),
                   selected = "PCA")
    ),
    mainPanel(
      plotOutput("static_plot")
    )
  )
)

### Oblique tab
tabOblique <- tabPanel(
  "Oblique", fluidPage(
    sidebarPanel(
      # generate tour button
      actionButton("obl_button", "Generate tour"),
      # basis init and rescale
      radioButtons("obl_basis_init", "Start basis",
                   choices = c("Random", "PCA", "Projection pursuit", "From file"),
                   selected = "Random"),
      ## Projection pursuit options
      conditionalPanel("input.obl_basis_init == 'Projection pursuit'",
                       selectInput("pp_type", "Pursuit index", guidedTourOptions)
      ),
      ## Basis From file select
      conditionalPanel(
        "input.obl_basis_init == 'From file'",
        fileInput("basispath", "Basis file (.csv or .rda, [p x 2] matrix)",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
      ),
      # manip var, ref axes placement
      selectInput('obl_manip_var', 'Manip var', "none"),
      selectInput('obl_axes', 'Reference axes location', c('center', 'bottomleft', 'off'),
                  'center',  multiple = FALSE),
      tags$hr(),
      # Slider controls
      sliderInput("obl_x_slider", "X contribution", min = -1, max = 1, value = 0, step = .1),
      sliderInput("obl_y_slider", "Y contribution", min = -1, max = 1, value = 0, step = .1)
    ),
    
    mainPanel(
      plotlyOutput("obl_plotlyAnim"),
      h4("Current basis"),
      tableOutput("obl_basis_out")
    )
  )
)


### Combined tabs
ui <- fluidPage(
  navbarPage(
    "Manual tours -- comparison",
    tabInput,
    tabRadial,
    tabStatic,
    tabOblique
  )
  # ,verbatimTextOutput("devMessage")
)

