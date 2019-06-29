## No external functions, so included in .Rbuildignore.
## presumably still useable by server.R.

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
              #,verbatimTextOutput("devMessage3")
    )
    
  )
)

### Radial Manual Tab
tabRadial <-  tabPanel(
  "Radial manual", fluidPage(
    sidebarPanel(
      # generate tour button
      actionButton("radial_button", "Generate tour"),
      conditionalPanel("input.radial_button == TRUE",
                       actionButton("save", "Save basis"),
                       numericInput("basis2save", "basis to save", 1,
                                    min = 1, max = 100)
      ),
      tags$hr(),
      # basis init and rescale
      radioButtons("basis_init", "Start basis",
                   choices = c("Random", "PCA", "Projection pursuit", "From file"),
                   selected = "Random"),
      ## Projection pursuit options
      conditionalPanel("input.basis_init == 'Projection pursuit'", # condition not working, starts visible.
                       selectInput("pp_type", "Pursuit index", guidedTourOptions)
      ),
      ## From file options
      conditionalPanel(
        "input.basis_init == 'From file'",
        fileInput("basispath", "Basis file (.csv or .rda, [p x 2] matrix)",
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

### Glyphmap Manual Tab
tabGlyphmap <- tabPanel(
  "Glyphmap manual", fluidPage(
    sidebarPanel(
      # basis init and rescale
      radioButtons("gly_basis_init", "Start basis",
                   choices = c("Random", "PCA", "From file"),
                   selected = "Random"),
      conditionalPanel(
        "input.gly_basis_init == 'From file'",
        fileInput("basispath", "Basis file (.csv or .rda, [p x 2] matrix)",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
      ),
      
      # manip var, ref axes placement
      selectInput('gly_manip_var', 'Manip var', "none"),
      selectInput('gly_axes', 'Reference axes location', c('center', 'bottomleft', 'off'),
                  'center',  multiple = FALSE),
      tags$hr(),
      # Slider controls
      sliderInput("gly_x_slider", "X contribution", min = -1, max = 1, value = 0),
      sliderInput("gly_y_slider", "Y contribution", min = -1, max = 1, value = 0)
    ),
    
    mainPanel(
      plotlyOutput("gly_plotlyAnim")
    )
  )
)


### Combined tabs
ui <- fluidPage(
  navbarPage(
    "Manual tours",
    tabInput,
    tabRadial,
    tabStatic,
    tabGlyphmap
  )
)

