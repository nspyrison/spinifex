### "radial_tour" ui.R -----
require("spinifex")
require("shinythemes")     ## Themes for shiny, think css files.
require("shinycssloaders") ## shinycssloaders::withSpinner()
require("plotly")

##### Initialize ----
## Create contextLine, a string containing App name, spinifex version, and sys date.
.wd <- getwd()
.regex <- regexpr("\\/[^\\/]*$", .wd)
.local_path <- substr(.wd, .regex + 1L, nchar(.wd))
contextLine <- paste0("Spinifex app, '", .local_path,
                      "' --- (spinifex v", packageVersion("spinifex"),
                      ") --- ", Sys.Date())

### tabData -----
tabData <- tabPanel(
  "Process projection data", 
  fluidPage(
    sidebarPanel(
      width = 3L,
      ## Select data
      selectInput("dat", "Dataset",
                  c("flea", "Upload file", "olive", "weather", "wine",
                    "breast cancer", "diabetes, long", "diabetes, wide"),
                  "flea"),
      conditionalPanel("input.dat == 'Upload file'",
                       fileInput("data_file", "Data file",
                                 placeholder = "<Select a .csv or .rda file>",
                                 accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                       ),
                       verbatimTextOutput("data_msg")),
      ## Select the projection variables
      uiOutput("input__proj_vars"),
      p("Rows with NA values excluded if present."),
      checkboxInput("rescale_data", "Standardize values to by Std.Dev.", value = TRUE)
    ),
    mainPanel(width = 9L,
              h4("Input data summary"),
              verbatimTextOutput("raw_dat_summary"),
              h4("Processed data summary"),
              verbatimTextOutput("sel_dat_summary")
    )
    
  )
)

### tabRadial ----
tabRadial <- tabPanel(
  "Radial manual tour",
  fluidPage(
    sidebarPanel(
      width = 3L,
      uiOutput("input__manip_var_nm"),
      fluidRow(column(6L, uiOutput("input__col_var_nm")),
               column(6L, uiOutput("input__pch_var_nm")))
    ),
    mainPanel(width = 9L,
              shinycssloaders::withSpinner(
                plotly::plotlyOutput("plotly_anim", height = "600px"), type = 8L)
    )
  )
) ## Assign tab

##### ui, combine tabs -----
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  ## Also see the themes: "flatly", "spacelab", "journal"
  ## Make the lines, hr() black:
  tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
  #### Content:
  navbarPage(
    paste0("Spinifex app -- ", .local_path, ""),
    tabData,
    tabRadial
  ),
)

