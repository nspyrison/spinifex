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
    mainPanel(h3("Data structure"),
              verbatimTextOutput("str_data")
    )
  )
)


##### Manual tab ----
tabManual <-
  tabPanel("Manual tour", fluidPage(
    ##### __Sidebar inputs ----
    ### Flow: general, interactive, animation, optional settings 
    sidebarPanel(
      ### General tour settings
      fluidRow(
        column(6, fluidRow(
          radioButtons("basis_init", "Start basis",
                       choices = c("Random", "PCA", "Projection pursuit", "From file"),
                       selected = "Random"),
          conditionalPanel("input.basis_init == 'Projection pursuit'",
                           selectInput("pp_type", "Pursuit index", guidedTourOptions)),
          conditionalPanel("input.basis_init == 'From file'",
                           fileInput("basis_file", "Basis file (.csv or .rda, [p x 2] matrix)",
                                     accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))))
        ),
        column(6, selectInput('manip_var', 'Manip var', "none"))),
      hr(),
      ### Interactive specific settings
      h4("Interactive"),
      selectInput('manip_type', "Manipulation type",
                  c("horizontal", "vertical", "radial")),
      conditionalPanel("input.manip_type == 'horizontal'",
                       sliderInput("x_slider", "X contribution",
                                   min = -1, max = 1, value = 0, step = .1)),
      conditionalPanel("input.manip_type == 'vertical'",
                       sliderInput("y_slider", "Y contribution",
                                   min = -1, max = 1, value = 0, step = .1)),
      conditionalPanel("input.manip_type == 'radial'",
                       sliderInput("rad_slider", "Radial contribution",
                                   min = 0, max = 1, value = .5, step = .05)),
      fluidRow(column(4, actionButton("obl_run", "Run")),
               column(4, actionButton("obl_save", "Save (csv & png)")),
               column(4, actionButton("obl_to_gallery", "Send to gallery"))
      ),
      verbatimTextOutput("obl_save_msg"),
      hr(),
      ### Animation settings
      h4("Animation"),
      sliderInput('angle', 'Angle step size (animation only)', value = .05, min = .01, max = .3),
      fluidRow(column(4, actionButton("anim_run", "Run")),
               column(8, actionButton("anim_save", "Save (gif)"))),
      verbatimTextOutput("anim_save_msg"),
      hr(),
      ### Optional settings
      h5("Optional Settings"),
      selectInput('axes', 'Reference axes location', 
                  c('center', 'bottomleft', 'off'),
                  'center',  multiple = FALSE),
      sliderInput('alpha', "Alpha opacity", min = 0, max = 1, value = 1, step = .1)
    ),
    
    ##### __Display plots ----
    mainPanel(
      h3("Interactive"),
      fluidRow(
        column(9, plotOutput("obl_plot")),
        column(3,
               fluidRow(
                 h4("Current basis"),
                 tableOutput("curr_basis_out")))
      ),
      hr(),
      h3("Animation"),
      column(9, plotlyOutput("anim_plot"))
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
      DT::dataTableOutput("gallery")
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

