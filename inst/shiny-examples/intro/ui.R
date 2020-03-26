### Input Tab -----
tabInput <- tabPanel(
  "Input", fluidPage(
    sidebarPanel(
      width = 3,
      selectInput('dat', 'Dataset', 
                  c('flea', 'olive', 'weather', 'wine', 'breastcancer', 'mtcars')),
      ## include which variables in the projection
      checkboxGroupInput(
        "variables",
        label = "Projection variables",
        choices = "none"
      ),
      checkboxInput("rescale_data", "Normalize values to [0, 1]", value = TRUE)
    ),
    mainPanel(width = 3,
              h3("Data structure"),
              verbatimTextOutput("str_data")
    )
    
  )
)

### Radial Manual Tab ----
tabRadial <- tabPanel(
  "Radial manual", fluidPage(
    sidebarPanel(
      width = 3,
      selectInput('manip_var', 'Manip var', "none"),
      fluidRow(column(6, selectInput("col_nm", "Point color", "<none>")),
               column(6, selectInput("pch_nm", "Point shape", "<none>"))),
      actionButton("radial_button", "Render tour")
    ),
    
    mainPanel(
      width = 9,
      plotly::plotlyOutput("plotlyAnim",
                           height = "600px")
    )
  )
)

##### ui -----
### Tabs combined
ui <- fluidPage(theme = shinythemes::shinytheme("flatly"), ## Esp: "flatly", "spacelab", "journal"
  navbarPage(
    "Manual tours -- intro",
    tabInput,
    tabRadial
  )
)

