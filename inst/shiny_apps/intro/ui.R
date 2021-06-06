### "Intro" ui.R -----

##### Setup ----
.include_dev_display  <- FALSE
.include_obs_msg      <- FALSE
.obs_msg_counter <- 0L
## Creat lontextLine, a string containing App name, spinifex version, and sys date.
.wd <- getwd()
.regex <- regexpr("\\/[^\\/]*$", .wd)
.local_path <- substr(.wd, .regex + 1L, nchar(.wd))
contextLine <- paste0("Spinifex app, '", .local_path, 
                      "' --- (spinifex v", packageVersion("spinifex"),
                      ") --- Ran on ", Sys.Date())

require("spinifex")
require("tourr")
require("ggplot2")
require("tibble")
require("shinythemes") ## Themes for shiny, think css files.
require("shinyjs")     ## Extend JavaScript (Think HTML interactivity) control and formating, 
## also see ?shinyjs::toggle   &   https://daattali.com/shiny/shinyjs-basic/

default_projVars <- names(tourr::flea[, 1L:6L])

### Data Tab -----
tabData <- tabPanel(
  "Data", fluidPage(
    sidebarPanel(
      width = 3L,
      ## Select data
      selectInput("dat", "Dataset",
                  c("flea", "olive", "weather", "wine", "breast cancer",
                    "diabetes, long", "diabetes, wide"), #Upload file"), ## upload causing other errors
                  "flea"),
      conditionalPanel("input.dat == 'Upload file'",
                       fileInput("data_file", "Data file",
                                 placeholder = "<Select a .csv or .rda file>",
                                 accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                       ),
                       verbatimTextOutput("data_msg")),
      ## Select the projection variables
      checkboxGroupInput(
        "projVars",
        label = "Projection variables",
        choices  = default_projVars,
        selected = default_projVars
      ),
      checkboxInput("rescale_data", "Standardize values to by Std.Dev.", value = TRUE)
    ),
    mainPanel(width = 9L,
              h4("Raw input data summary"),
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
      width = 3L,
      selectInput('manip_var_nm', 'Manip var', 1L),
      fluidRow(column(6L, selectInput("col_var_nm", "Point color", "<none>")),
               column(6L, selectInput("pch_var_nm", "Point shape", "<none>")))
    ),
    mainPanel(
      width = 9L,
      plotly::plotlyOutput("plotlyAnim",
                           height = "600px")
    )
  )
)

##### ui -----
### Tabs combined
ui <- fluidPage(theme = shinythemes::shinytheme("flatly"), 
                ## Esp see the themes: "flatly", "spacelab", "journal"
                ## Make the lines, hr() black:
                tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
                shinyjs::useShinyjs(),
                #### Content:
                navbarPage(paste0("Spinifex app -- ", .local_path, ""),
                           tabData,
                           tabRadial
                ),
                h5(contextLine, style = "color: #A9A9A9"),
                shinyjs::hidden(
                  div(id = "dev_toggle",
                      actionButton("browser", "browser()"),
                      verbatimTextOutput("dev_msg")
                  )
                )
)

