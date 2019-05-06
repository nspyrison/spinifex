#Example: see function sectioning:
# file.edit("C:/Users/spyri/Documents/R/functionSectioning/ui.R")


# First tab
tabInput <- tabPanel("input", fluidPage(
  sidebarPanel(
    fileInput("file", "data file (CSV format)",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"
              )
    ),
    checkboxGroupInput(
      "variables",
      label = "Choose variables to display",
      choices = vars,
      selected = if (is.null(.data)) vars[1:6] else vars[1:nVars]
    ),
    checkboxInput("rescale_data", "Rescale", value = TRUE),
    checkboxInput("rand_basis", "Random basis", value = TRUE)
  ),
  mainPanel(h2("Data structure"), verbatimTextOutput("str_data"))
))

tabResults <-  tabPanel("results", fluidPage(
  sidebarPanel(
    selectInput('manip_var', 'Manip var', vars, selected = "aede2"),
    selectInput('cat_var', 'Categorical var', vars, selected = "species"),
    checkboxInput("show", "More options"),
    conditionalPanel(
      "input.show",
      selectInput('axes', 'Reference axes', c('center', 'bottomleft', 'off'),
                  'center',  multiple = FALSE),
      sliderInput('angle', 'Angle step size',value = .05, min = .01, max = .3)
      # numericInput("angle", "Angular step size", 0.05, min = 0.01, max = .3),
      ## update every button change.
    )
  ),
  mainPanel(plotlyOutput("plotlytest"))
))

#putting the ui together
ui <- fluidPage(
  navbarPage(
    "Radial manual tours",
    tabInput,
    tabResults
  ))

