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
      # rescale and random basis initiation
      tags$hr(),
      checkboxInput("rescale_data", "Rescale", value = TRUE),
      checkboxInput("rand_basis", "Random basis", value = TRUE),
      # update button
      tags$hr(),
      textOutput("updateButtonPress"),
      actionButton("updateButton", "Update results")
    ),
    mainPanel(h2("Data structure"), 
              verbatimTextOutput("str_data"),
              verbatimTextOutput("messages")
    )
  )
)

# Second tab
tabResults <-  tabPanel(
  "results", fluidPage(
    sidebarPanel(
      # manip and Cat vars
      selectInput('manip_var', 'Manip var', vars, selected = "aede2"),
      selectInput('cat_var', 'Categorical var', vars, selected = "species"),
      # More options: axes placement and angle step size
      tags$hr(),
      checkboxInput("show", "More options"),
      conditionalPanel(
        "input.show",
        selectInput('axes', 'Reference axes', c('center', 'bottomleft', 'off'),
                    'center',  multiple = FALSE),
        sliderInput('angle', 'Angle step size',value = .05, min = .01, max = .3)
      )
    ),
    mainPanel(plotlyOutput("plotlyAnim"))
  )
)

# Combining tabs together in the ui.
ui <- fluidPage(
  navbarPage(
    "Radial manual tours",
    tabInput,
    tabResults
  ))

