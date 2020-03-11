##### Data tab -----
tabData <- tabPanel(
  "Data", fluidPage(
    sidebarPanel(
      # Input csv file
      fileInput("data_file", "Data file (.csv format)",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
      ),
      # Variables to project
      checkboxGroupInput(
        "projVars_nms",
        label = "Variables to project (numeric or ordinal)",
        choices  = names(tourr::flea[, 1:6]),
        selected = names(tourr::flea[, 1:6])
      ),
      # Variables to map to aesthetics 
      checkboxGroupInput(
        "aesVars",
        label = "Variables to map to aesthetics (esp. categorical)",
        choices  = names(tourr::flea),
        selected = names(tourr::flea[, 7])
      ),
      # Point color, shape, and rescale
      fluidRow(column(5, selectInput('col_var', 'Point color', "<none>")),
               column(5, selectInput('pch_var', 'Point shape', "<none>"))),
      checkboxInput("rescale_data", "Rescale values to [0, 1]", value = TRUE)
    ),
    mainPanel(h3("Data summary -- skimr::skim()")
              #, verbatimTextOutput("data_str")
              , verbatimTextOutput("data_summary")
    )
  )
)


##### Manual tab ----
tabManual <- tabPanel("Manual tour", fluidPage(
  ##### _Sidebar tour inputs ----
  sidebarPanel(
    radioButtons("basis_init", "Start basis",
                 choices = c("PCA", "Projection pursuit", "From file",
                             "Identity", "Random"),
                 selected = "PCA"),
    conditionalPanel("input.basis_init == 'Projection pursuit'",
                     selectInput("pp_type", "Pursuit index", 
                                 c("holes", "cmass", "lda_pp", "pda_pp")
                     )),
    conditionalPanel("input.basis_init == 'Projection pursuit' && 
                     (input.pp_type == 'lda_pp' || input.pp_type == 'pda_pp')",
                     selectInput("pp_cluster", "Pursuit cluster (obl)",
                                 choices = c("<no categorical variables found>" = NA) )),
    conditionalPanel("input.basis_init == 'From file'",
                     fileInput("basis_file", "Basis file (.csv or .rda, [p x 2] matrix)",
                               accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))),
    selectInput("manip_nm", "Variable to manipulate", "<none>"),
    actionButton("re_init", "Initiaze to above parameters"),
    radioButtons("disp_method", "",
                 choices = c("Interactive", "Animation"),
                 selected = "Interactive"),
    hr(),
    ##### _Sidebar interactive inputs ----
    conditionalPanel(
      "input.disp_method == 'Interactive'", 
        h4("Interactive"),
        selectInput("manip_type", "Manipulation type",
                    c("Radial", "Horizontal", "Vertical")),
        sliderInput("manip_slider", "Contribution",
                    min = -1, max = 1, value = 0, step = .1)
    ),
    ##### _Sidebar animation inputs ----
    conditionalPanel(
      "input.disp_method == 'Animation'",
        h4("Animation"),
        selectInput("anim_type", "Animation type",
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
      sliderInput('anim_angle', 'Angle step size', value = .15, min = .05, max = .3),
      sliderInput('fps', "Frames per second", min = 1, max = 6, value = 3, step = 1)
    ),
    ##### _Sidebar optional inputs ----
    fluidRow(column(4, actionButton("save", "Save basis")), # (csv & png)
             column(4, actionButton("to_gallery", "Send to gallery")),
             column(4, 
                    conditionalPanel("input.disp_method == 'Animation'",
                                     actionButton("anim_save", "Save anim"))) # (gif)
    ),
             verbatimTextOutput("save_msg"),
             hr(),
    h5("Optional Settings"),
    selectInput('axes', 'Reference axes location', 
                c('center', 'bottomleft', 'off'),
                'center',  multiple = FALSE),
    sliderInput('alpha', "Alpha opacity", min = 0, max = 1, value = 1, step = .1)
  ),
  
  ##### _Plot display ----
  mainPanel(
    fluidRow(
      column(9, plotOutput("main_plot")),
      column(3, fluidRow(h4("Current basis"),
                         tableOutput("curr_basis_tbl")))
    ),
    
    conditionalPanel(
      "input.disp_method == 'Animation'", 
      fluidRow(column(1, actionButton("anim_play", "Play")),
               column(7, sliderInput("anim_slider", "Animation index",
                                     min = 1, max = 10, value = 1, step = 1, 
                                     width = '100%'))
      ), # Align the play botton with a top margin:
      tags$style(type='text/css', "#anim_play {margin-top: 40px;}")
    )
  )
))



##### Gallery -----
tabGallery <- tabPanel(
  "Gallery", fluidPage(
    mainPanel(
      verbatimTextOutput("gallery_msg")
      , fluidRow(column(2, plotOutput("gallery_icons")),
                 column(10, DT::dataTableOutput("gallery_df"))
      ) # align icons with a top margin: 
      , tags$style(type='text/css', "#gallery_icons {margin-top: 70px;}")
      , verbatimTextOutput("gallery_icons_str")
    )
  )
)

###### Tabs combined ----
ui <- fluidPage(
  ### Make the lines, hr() black
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
  navbarPage("spinifex -- dev app",
             tabData,
             tabManual,
             tabGallery
  )
  , verbatimTextOutput("dev_msg")
)

