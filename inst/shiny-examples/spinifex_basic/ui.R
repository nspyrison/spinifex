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
      selectInput('dat', 'Data set', 
                  c('flea', 'olive', 'weather', 'wine', 'breastcancer', 'mtcars')),
      # include which variables in the projection
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
      tags$hr(),
      # basis init and rescale
      radioButtons("basis_init", "Start basis",
                   choices = c("Random", "PCA"),
                   selected = "Random"),
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



### Combined tabs
ui <- fluidPage(
  navbarPage(
    "Manual tours -- basic",
    tabInput,
    tabRadial
  )
)

