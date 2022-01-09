### "radial_tour" ui.R -----
#options(show.error.locations = TRUE) #, warn = 2L, error = browser)
require("spinifex")
require("shinythemes")     ## Themes for shiny, think css files.
require("shinycssloaders") ## shinycssloaders::withSpinner()
require("plotly")


##### Initialize ----
## Create contextLine, a string containing App name, spinifex version, and sys date.
.wd <- getwd()
.regex <- regexpr("\\/[^\\/]*$", .wd)
.local_path <- substr(.wd, .regex + 1L, nchar(.wd))
contextLine <- paste0(.local_path, " app, ",
                      " --- {spinifex} ver. ", packageVersion("spinifex"),
                      " --- ", Sys.Date())

### tabData -----
tabData <- tabPanel(
  "Process projection data",
  fluidPage(
    sidebarPanel(
      width = 3L,
      ## Select data
      selectInput("dat", "Dataset",
                  c("Upload file", "flea", "olive", "weather", "wine",
                    "breast cancer", "diabetes, long", "diabetes, wide"),
                  "flea"),
      conditionalPanel("input.dat == 'Upload file'",
                       fileInput("data_file", "Data file",
                                 placeholder = "<Select a .csv or .rds file>",
                                 accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv",  ".rds")
                       ),
                       verbatimTextOutput("data_msg")),
      ## Select the projection variables
      uiOutput("input__proj_vars"),
      uiOutput("ui__na_msg"),
      checkboxInput("rescale_data", "Standardize values to by standard deviation?", value = TRUE)
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
                plotly::plotlyOutput("plotly_anim", height = "600px"), type = 8L),
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
    paste0("{spinifex} app --- ", .local_path),
    tabData,
    tabRadial
  ),
  h5(contextLine, style = "color: #A9A9A9")
)

