#Example: see function sectioning:
# file.edit("C:/Users/spyri/Documents/R/functionSectioning/ui.R")


# First tab
tabInput <- tabPanel(
  "input", fluidPage(
    sidebarPanel(
      # Input csv file
      fileInput("file", "data file (CSV format)",
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
      radioButtons("init_type", "Start basis:",
                   choices = c("Random",
                               "PCA",
                               "Manual"),
                   selected = "unif"),
      tags$hr(),
      checkboxInput("rescale_data", "Rescale values", value = TRUE),
      checkboxInput("rand_basis", "Random basis", value = TRUE)
    ),
    mainPanel(h2("Data structure"), 
              verbatimTextOutput("str_data"),
              verbatimTextOutput("variables")
    )
  )
)

# Second tab
tabResults <-  tabPanel(
  "results", fluidPage(
    sidebarPanel(
      textOutput("tsMessage"),
      # generate tour button
      actionButton("generate", "Generate tour"),
      textOutput("generate"),
      tags$hr(),
      
      # manip and Cat vars
      selectInput('manip_var', 'Manip var', vars, 
                  selected = if(is.null(.data)) "aede2"),
      selectInput('col_var', 'Point color', vars, 
                  selected = if(is.null(.data)) "species"),
      selectInput('pch_var', 'Point shape', vars, 
                  selected = if(is.null(.data)) "species"),
      
      # More options: axes placement and angle step size
      tags$hr(),
      #checkboxInput("show", "More options", value = T),
      #conditionalPanel(
      #  "input.show",
        selectInput('axes', 'Reference axes', c('center', 'bottomleft', 'off'),
                    'center',  multiple = FALSE),
        sliderInput('angle', 'Angle step size',value = .05, min = .01, max = .3)
      #)
    ),
    mainPanel(plotlyOutput("plotlyAnim"))
  )
)

# Second tab
tabGallery <-  tabPanel(
  "gallery", fluidPage(
    sidebarPanel(
    )
  )
)


# Combining tabs together in the ui.
ui <- fluidPage(
  navbarPage(
    "Radial manual tours",
    tabInput,
    tabResults,
    tabGallery
  ))

