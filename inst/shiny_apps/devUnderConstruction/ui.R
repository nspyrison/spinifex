### "DevUnderConstruction" ui.R -----

##### Data tab -----
tabData <- tabPanel(
  title = "Data", 
  fluidPage(
    sidebarPanel(
      width = 3,
      ## Input csv file
      radioButtons("data_source", "Data source",
                   choices = c("Example", "Upload file"),
                   selected = "Example"),
      conditionalPanel("input.data_source == 'Example'",
                       selectInput("data_example", "Example data",
                                   choice = c("flea", "wine", "breastcancer", "weather"), ## See tourr:: "olive", "ozone", "laser", "tao", "ratcns"
                                   selected = "flea")
      ),
      conditionalPanel("input.data_source == 'Upload file'",
                       fileInput("data_file", "Data file",
                                 placeholder = "<Select a .csv or .rda file>",
                                 accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                       ),
                       verbatimTextOutput("data_msg")
      ),
      ## Variables to project
      shinyBS::tipify(
        checkboxGroupInput(
          "projVar_nms",
          label = "Variables to project",
          choices  = names(tourr::flea),
          selected = names(tourr::flea)[1:6]
        ),
        title = "Expects numeric or discrete ordinal",
        placement = "top"
      ),
      tags$b("Tranformations"),
      tags$div(title = "Normalize every variable to [0, 1]?",
               checkboxInput("rescale_data", 
                              "Normalize", value = TRUE)
      ),
      h5("Note: rows containing missing values in any projection variable have been remove.")
    ),
    mainPanel(width = 9,
              h4("Input data summary"),
              verbatimTextOutput("rawDat_summary"),
              h4("Projection data summary"),
              verbatimTextOutput("selDat_summary")
    )
  )
)


##### Manual tour tab ----
tabManual <- tabPanel("Manual tour", fluidPage(
  ##### _Sidebar globar inputs ----
  sidebarPanel(
    width = 3,
    radioButtons("basis_init", "Start basis",
                 choices = c("PCA", "Projection pursuit", "From file", "Random"),
                 selected = "PCA"),
    conditionalPanel("input.basis_init == 'Projection pursuit'",
                     selectInput("pp_type", "Pursuit index", 
                                 c("holes", "cmass", "lda_pp", "pda_pp")
                     )),
    conditionalPanel("input.basis_init == 'Projection pursuit' && 
                     (input.pp_type == 'lda_pp' || input.pp_type == 'pda_pp')",
                     selectInput("basis_pp_cluster", "Pursuit cluster (obl)",
                                 choices = c("<no categorical variables found>" = NA) )),
    conditionalPanel("input.basis_init == 'From file'",
                     fileInput("basis_file", "Basis file (.csv or .rda, [p x 2] matrix)",
                               accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))),
    actionButton("re_init", "Return to the basis above"),
    selectInput("manip_nm", "Manipulation variable", "<none>"),
    fluidRow(column(6, selectInput("col_nm", "Point color", "<none>")),
             column(6, selectInput("pch_nm", "Point shape", "<none>"))),
    hr(),
    radioButtons("tour_class", "Class of tour",
                 choices = c("Manual", "Guided"),
                 selected = "Manual"),
    ##### _Sidebar: manual tour inputs ----
    conditionalPanel(
      "input.tour_class == 'Manual'", 
        h4("Manual tours"),
        selectInput("manip_type", "Manipulation direction",
                    c("Radial", "Horizontal", "Vertical")),
        sliderInput("manip_slider", "Radius",
                    min = 0, max = 1, value = .5, step = .1)
    ),
    ##### _Sidebar: guided tour inputs ----
    conditionalPanel(
      "input.tour_class == 'Guided'",
        h4("Guided tours"),
        selectInput("anim_type", "Type of tour",
                    c("Radial", "Horizontal", "Vertical"
                      ,"Grand (6 bases)", "Little (6 bases)", "Local (6 bases)",
                      "Projection pursuit")),
        conditionalPanel("input.anim_type == 'Projection pursuit'",
                         selectInput("anim_pp_type", "Pursuit index",
                                     c("holes", "cmass", "lda_pp", "pda_pp"))),
        conditionalPanel("input.anim_type == 'Projection pursuit' && 
                         (input.anim_pp_type == 'lda_pp' || input.anim_pp_type == 'pda_pp')", 
                         selectInput("anim_pp_cluster", "Pursuit cluster (anim)",
                                     choices = c("<no categorical variables found>" = NA) )),
      sliderInput("anim_angle", "Angle step size", value = .15, min = .05, max = .3),
      sliderInput("fps", "Frames per second", min = 1, max = 6, value = 3, step = 1)
    ),
    
    fluidRow(column(4, actionButton("save", "Save basis")), ## (csv & png)
             column(4, actionButton("to_gallery", "Send to gallery")),
             column(4, 
                    conditionalPanel("input.tour_class == 'Guided'",
                                     actionButton("anim_save", "Save anim"))) ## (gif)
    ),
    verbatimTextOutput("save_msg")
  ), ## End of sidebar
  
  ##### _MainPanel: Plot display ----
  mainPanel(
    width = 9,
    fluidRow(
      column(11, plotOutput("main_plot")),
      column(1, fluidRow(HTML("<h3 style='white-space: nowrap;'>Current basis</h3>"),
                         tableOutput("curr_basis_tbl")))
    ),
    
    conditionalPanel(
      "input.tour_class == 'Guided'", 
      fluidRow(column(1, actionButton("anim_play", "Play")),
               column(7, sliderInput("anim_slider", "Tour index",
                                     min = 1, max = 10, value = 1, step = 1, 
                                     width = "100%"))
      ), ## Align the play botton with a top margin:
      tags$style(type = "text/css", "#anim_play {margin-top: 40px;}")
    )
  ) ## End of main panel
)) ## End of tabManual


##### Gallery tab -----
tabGallery <- tabPanel(
  "Gallery", fluidPage(
    mainPanel(
      width = 12,
      verbatimTextOutput("gallery_msg"),
      fluidRow(column(1, plotOutput("gallery_icons")),
               column(11, DT::dataTableOutput("gallery_df"))
      ), 
      ## A icons with a top margin: 
      tags$style(type = "text/css", "#gallery_icons {margin-top: 36px;}"),
      verbatimTextOutput("gallery_icons_str")
    )
  )
)

###### Full app layout ----
ui <- fluidPage(theme = shinythemes::shinytheme("flatly"), ## Esp: "flatly", "spacelab", "journal"
                ## Make the lines, hr() black:
                tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
                shinyjs::useShinyjs(),
                #### Content::
                navbarPage(paste0("Spinifex app -- ", .local_path, ""),
                           tabData,
                           tabManual,
                           tabGallery
                ),
                h5(contextLine, style = "color: #A9A9A9"),
                shinyjs::hidden(div(id = "dev_toggle",
                           actionButton("browser", "browser()"),
                           verbatimTextOutput("dev_msg")
                ))
)

