# Example: see function sectioning
# file.edit("C:/Users/spyri/Documents/R/functionSectioning/ui.R")
# ,textOutput("devMessage")

### Input Tab
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
        label = "Choose variables to display",
        choices = "none"
      ),
      # Point color, shape and rescale [0,1] data
      selectInput('col_var', 'Point color', "none"),
      selectInput('pch_var', 'Point shape', "none"),
      tags$hr(),
      checkboxInput("rescale_data", "Rescale values", value = TRUE)
    ),
    mainPanel(h2("Data structure"),
              verbatimTextOutput("str_data")
              ,verbatimTextOutput("devMessage3")
    )
    
  )
)

### Radial Manual Tab
tabRadial <-  tabPanel(
  "Radial Manual", fluidPage(
    sidebarPanel(
      # generate tour button
      actionButton("radial_button", "Generate tour"),
      tags$hr(),
      # basis init and rescale
      radioButtons("basis_init", "Start basis",
                   choices = c("Random", "PCA", "From file"),
                   selected = "Random"),
      conditionalPanel(
        "input.basis_init == 'Manual'",
        fileInput("basispath", "Basis file (.csv or .rda) [p by d=2] matrix",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
      ),
      
      # manip, col, and pch vars
      selectInput('manip_var', 'Manip var', "none"),
      
      # More options: axes placement and angle step size
      tags$hr(),
      selectInput('axes', 'Reference axes location', c('center', 'bottomleft', 'off'),
                  'center',  multiple = FALSE),
      sliderInput('angle', 'Angle step size', value = .05, min = .01, max = .3)
    ),
    
    mainPanel(
      plotlyOutput("plotlyAnim")
    )
  )
)

### Static Linear Tab
tabStatic <- tabPanel(
  "Linear projection", fluidPage(
    sidebarPanel(
      actionButton("static_button", "Generate projection"),
      tags$hr(),
      radioButtons("static_tech", "Linear projection technique",
                   choices = c("PCA", "LDA (holder)", "SPLOM"),
                   selected = "PCA")
    ),
    mainPanel(
      plotOutput("static_plot")
    )
  )
)

### Oblique Manual Tab
tabOblique <- tabPanel(
  "Oblique Manual", fluidPage(
    sidebarPanel(
      # basis init and rescale
      radioButtons("obl_basis_init", "Start basis",
                   choices = c("Random", "PCA", "From file"),
                   selected = "Random"),
      conditionalPanel(
        "input.obl_basis_init == 'Manual'",
        fileInput("basispath", "Basis file (.csv or .rda) [p by d=2] matrix",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
      ),
      
      # manip var, ref axes placement
      selectInput('obl_manip_var', 'Manip var', "none"),
      selectInput('obl_axes', 'Reference axes location', c('center', 'bottomleft', 'off'),
                  'center',  multiple = FALSE),
      tags$hr(),
      # Slider controls
      sliderInput("obl_x_slider", "X contribution", min = -1, max = 1, value = 0),
      sliderInput("obl_y_slider", "Y contribution", min = -1, max = 1, value = 0)
    ),
    
    mainPanel(
      plotlyOutput("obl_plotlyAnim")
    )
  )
)


### Combined tabs
ui <- fluidPage(
  navbarPage(
    "Radial manual tours",
    tabInput,
    tabRadial,
    tabStatic,
    tabOblique
  )
)

