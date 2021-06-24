### "radial_tour" ui.R -----
require("spinifex")
require("shinythemes")     ## Themes for shiny, think css files.
require("shinycssloaders") ## shinycssloaders::withSpinner()

##### Initialize ----
## Create contextLine, a string containing App name, spinifex version, and sys date.
.wd <- getwd()
.regex <- regexpr("\\/[^\\/]*$", .wd)
.local_path <- substr(.wd, .regex + 1L, nchar(.wd))
contextLine <- paste0("Spinifex app, '", .local_path,
                      "' --- (spinifex v", packageVersion("spinifex"),
                      ") --- Ran on ", Sys.Date())

### tabData -----
tabData <- tabPanel(
  "Process projection data", 
  fluidPage(
    sidebarPanel(
      width = 3L,
      ## Select data
      selectInput("dat", "Dataset",
                  c("flea", "Upload file", "olive", "weather", "wine", "breast cancer",
                    "diabetes, long", "diabetes, wide"),
                  "flea"),
      conditionalPanel("input.dat == 'Upload file'",
                       fileInput("data_file", "Data file",
                                 placeholder = "<Select a .csv or .rda file>",
                                 accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                       ),
                       verbatimTextOutput("data_msg")),
      ## Select the projection variables
      uiOutput("inputProjVars"),
      p("Rows with NA values excluded if present."),
      checkboxInput("rescale_data", "Standardize values to by Std.Dev.", value = TRUE)
    ),
    mainPanel(width = 9L,
              h4("Input data summary"),
              verbatimTextOutput("rawDat_summary"),
              h4("Processed data summary"),
              verbatimTextOutput("selDat_summary")
    )
    
  )
)

### tabRadial ----
tabRadial <- tabPanel(
  "Radial manual tour", 
  fluidPage(
    sidebarPanel(
      width = 3L,
      selectInput('manip_var_nm', 'Manip var', 1L),
      fluidRow(column(6L, selectInput("col_var_nm", "Point color", "<none>")),
               column(6L, selectInput("pch_var_nm", "Point shape", "<none>")))
    ),
    mainPanel(
      width = 9L,
      shinycssloaders::withSpinner(
        plotly::plotlyOutput("plotlyAnim", height = "600px"), 
        type = 8L)
    )
  )
) ## Assign tab

##### ui, combine tabs -----
ui <- fluidPage(theme = shinythemes::shinytheme("flatly"), 
                ## Esp see the themes: "flatly", "spacelab", "journal"
                ## Make the lines, hr() black:
                tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
                #### Content:
                navbarPage(paste0("Spinifex app -- ", .local_path, ""),
                           tabData,
                           tabRadial
                ),
)

