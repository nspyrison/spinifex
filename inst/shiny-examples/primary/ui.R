##### Input tab ----
tabInput <- tabPanel(
  "Input", fluidPage(
    sidebarPanel(
      # Input csv file
      fileInput("data_file", "Data file (.csv format)",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
      ),
      # include which variables
      checkboxGroupInput(
        "variables",
        label = "Variables to include",
        choices = names(tourr::flea[, 1:6]),
        selected = names(tourr::flea[, 1:6])
      ),
      # Point color, shape and rescale
      fluidRow(column(6, selectInput('col_var', 'Point color', "<none>")),
               column(6, selectInput('pch_var', 'Point shape', "<none>"))),
      checkboxInput("rescale_data", "Rescale values to [0, 1]", value = TRUE)
    ),
    mainPanel(h3("Data structure")
              , verbatimTextOutput("data_str")
              # , verbatimTextOutput("data_summary")
    )
  )
)


##### Manual tab ----
tabManual <- tabPanel("Manual tour", fluidPage(
  ##### _Sidebar tour inputs ----
  sidebarPanel(
    fluidRow(
      column(6, fluidRow(
        radioButtons("basis_init", "Start basis",
                     choices = c("Random", "PCA", "Projection pursuit", "From file"),
                     selected = "Random"),
        conditionalPanel("input.basis_init == 'Projection pursuit'",
                         selectInput("pp_type", "Pursuit index", 
                                     c("cmass", "holes", "Skinny", "Striated", "Convex", 
                                       "Clumpy", "splines2d", "dcor2d", "MIC", "TIC"))),
        conditionalPanel("input.basis_init == 'From file'",
                         fileInput("basis_file", "Basis file (.csv or .rda, [p x 2] matrix)",
                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))))
      ),
      column(6, selectInput('manip_var', 'Manip var', "none"))),
    hr(),
    ##### _Sidebar interactive inputs ----
    h4("Interactive"),
    selectInput('manip_type', "Manipulation type",
                c("Horizontal", "Vertical", "Radial")),
    conditionalPanel("input.manip_type == 'Horizontal'",
                     sliderInput("x_slider", "X contribution",
                                 min = -1, max = 1, value = 0, step = .1)),
    conditionalPanel("input.manip_type == 'Vertical'",
                     sliderInput("y_slider", "Y contribution",
                                 min = -1, max = 1, value = 0, step = .1)),
    conditionalPanel("input.manip_type == 'Radial'",
                     sliderInput("rad_slider", "Radial contribution",
                                 min = 0, max = 1, value = 0, step = .1)),
    fluidRow(column(4, actionButton("obl_run", "Run")),
             column(4, actionButton("obl_save", "Save (csv & png)")),
             column(4, actionButton("obl_to_gallery", "Send to gallery"))
    ),
    verbatimTextOutput("obl_save_msg"),
    hr(),
    ##### _Sidebar animation inputs ----
    h4("Animation"),
    selectInput('anim_type', 'Tour type',
                c("Radial", "Horizontal", "Vertical"
                  # ,"Grand (8 bases)", "Little (8 bases)", "Projection pursuit"
                )),
    conditionalPanel("anim_type == 'Projection pursuit'",
                     selectInput("anim_pp_type", "Pursuit index",
                                 c("cmass", "holes", "Skinny", "Striated", "Convex",
                                   "Clumpy", "splines2d", "dcor2d", "MIC", "TIC"))),
    sliderInput('anim_angle', 'Angle step size', value = .05, min = .01, max = .3),
    fluidRow(column(4, actionButton("anim_run", "Run")),
             column(8, actionButton("anim_save", "Save (gif)"))),
             verbatimTextOutput("anim_save_msg"),
             hr(),
             ##### _Sidebar optional inputs ----
             h5("Optional Settings"),
             selectInput('axes', 'Reference axes location', 
                         c('center', 'bottomleft', 'off'),
                         'center',  multiple = FALSE),
             sliderInput('alpha', "Alpha opacity", min = 0, max = 1, value = 1, step = .1)
    ),
  
  ##### _Plot display ----
  mainPanel(
    fluidRow(
      column(9, plotOutput("obl_plot")),
      column(3, fluidRow(h4("Current basis"),
                         tableOutput("curr_basis_tbl"))
      )
    )
  ),
  
  conditionalPanel(
    "input.anim_run > 0", 
    fluidRow(column(1, actionButton("anim_play", "Play")),
             column(5, sliderInput("anim_slider", "Animation index",
                                   min = 1, max = 10, value = 1, step = 1, 
                                   width = '80%'))
    ), # align the play botton with a top margin:
    tags$style(type='text/css', "#anim_play {margin-top: 40px;}")
  )
))


##### Static tab ----
tabStatic <- tabPanel(
  "Static projections", fluidPage(
    sidebarPanel(
      actionButton("static_run", "Run"),
      radioButtons("static_method", "Projection",
                   choices = c("PCA", "SPLOM"), # others to consider: k-means, LDA, MDS, TNSE, Sammon's mapping, etc
                   selected = "PCA"),
      sliderInput("static_alpha", "Alpha opacity", min = 0, max = 1, value = 1, step = .1)
    ),
    mainPanel(
      plotOutput("static_plot")
    )
  )
)

##### Gallery -----
tabGallery <- tabPanel(
  "Gallery", fluidPage(
    mainPanel(
      fluidRow(column(1, plotOutput("gallery_icons")),
               column(1, plotOutput("gallery_icons_data")),
               column(10, DT::dataTableOutput("gallery"))
      ) # align icons with a top margin: 
      , tags$style(type='text/css', "#gallery_icons {margin-top: 40px;}")
      , verbatimTextOutput("gallery_icons_str")
      , verbatimTextOutput("gallery_msg")
    )
  )
)

###### Tabs combined ----
ui <- fluidPage(
  ### Make the lines, hr() black
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
  navbarPage("Manual tours -- comparison",
             tabInput,
             tabManual,
             tabGallery
             #, tabStatic
  )
  , verbatimTextOutput("dev_msg")
)

