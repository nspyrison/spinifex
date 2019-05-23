#Example: see function sectioning:
# file.edit("C:/Users/spyri/Documents/R/functionSectioning/ui.R")

# First tab, INPUT
tabInput <- tabPanel(
  "input", fluidPage(
    sidebarPanel(
      # Input csv file
      fileInput("file", "Data file (CSV format)",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
      ),
      # include which variables
      checkboxGroupInput(
        "variables",
        label = "Choose variables to display",
        choices = vars,
        selected = vars[1:nSelected]
      ),
      # basis init and rescale
      radioButtons("init_func", "Start basis",
                   choices = c("Random",
                               "PCA",
                               "Manual"),
                   selected = "Random"),
      conditionalPanel(
        "input.init_func == 'Manual'",
        fileInput("basis", "Basis file (CSV format) [p by d=2] matrix",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
      ),
      tags$hr(),
      checkboxInput("rescale_data", "Rescale values", value = TRUE)
    ),
    mainPanel(h2("Data structure"),
              verbatimTextOutput("str_data")
              # ,verbatimTextOutput("devMessage2")
              # ,verbatimTextOutput("devMessage3")
    )
  )
)

# Second tab, RESULTS
tabResults <-  tabPanel(
  "results", fluidPage(
    sidebarPanel(
      # generate tour button
      actionButton("generate", "Generate tour"),
      textOutput("generate"),
      tags$hr(),
      
      # manip, col, and pch vars
      selectInput('manip_var', 'Manip var', vars, 
                  selected = if(is.null(.data)) "aede2"),
      selectInput('col_var', 'Point color', vars, 
                  selected = if(is.null(.data)) "species"),
      selectInput('pch_var', 'Point shape', vars, 
                  selected = if(is.null(.data)) "species"),
      
      # More options: axes placement and angle step size
      tags$hr(),
      selectInput('axes', 'Reference axes location', c('center', 'bottomleft', 'off'),
                  'center',  multiple = FALSE),
      sliderInput('angle', 'Angle step size', value = .05, min = .01, max = .3)
    ),
    
    
    mainPanel(
      plotlyOutput("plotlyAnim")
      # ,textOutput("devMessage")
    )
  )
)

# Third tab, GALLERY
tabGallery <- tabPanel(
  "gallery", fluidPage(
  )
)


# Combining tabs together in the ui.
ui <- fluidPage(
  navbarPage(
    "Radial manual tours",
    tabInput,
    tabResults,
    tabGallery
  )
)

