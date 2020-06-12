### "Intro" ui.R -----

### Data Tab -----
tabData <- tabPanel(
  "Data", fluidPage(
    sidebarPanel(
      width = 3,
      selectInput("dat", "Dataset", 
                  c("flea", "olive", "weather", 
                    "wine", "breastcancer", "mtcars", 
                    "Upload file"),
                  "flea"
      ),
      conditionalPanel("input.dat == 'Upload file'",
                       fileInput("data_file", "Data file",
                                 placeholder = "<Select a .csv or .rda file>",
                                 accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                       ),
                       verbatimTextOutput("data_msg")
      ),
      ## Include which variables in the projection
      checkboxGroupInput(
        "variables",
        label = "Projection variables",
        choices = "none"
      ),
      checkboxInput("rescale_data", "Normalize values to [0, 1]", value = TRUE)
    ),
    mainPanel(width = 9,
              h4("Input data summary"),
              verbatimTextOutput("rawDat_summary"),
              h4("Selected data summary"),
              verbatimTextOutput("selDat_summary")
    )
    
  )
)

### Radial Manual Tab ----
tabRadial <- tabPanel(
  "Radial manual tour", fluidPage(
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
                ## Make the lines, hr() black:
                tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
                useShinyjs(),
                #### Content::
                navbarPage(paste0("Spinifex app -- ", .local_path, ""),
                           tabData,
                           tabRadial
                ),
                h5(contextLine, style = "color: #A9A9A9"),
                hidden(div(id = "dev_toggle",
                           actionButton("browser", "browser()"),
                           verbatimTextOutput("dev_msg")
                ))
)

