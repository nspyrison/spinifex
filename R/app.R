library(shiny)
library(plotly)
library(spinifex)
library(dplyr)
# library(eechidna); eechidna::launch_app()

f_dat  <- tourr::rescale(flea[,1:6])
f_cat  <- factor(flea$species)
# TODO make basis/ gtour init an option.
f_path <- save_history(f_dat, guided_tour(holes()))
f_bas  <- matrix(f_path[,, max(dim(f_path)[3])], ncol=2)

olive %>% 
  count(paste(olive$area, olive$palmitic), sort=T)
  
?olive
head(olive)
o_dat <- olive[, 3:10]
o_cat <- olive$area


ui <- fluidPage(
  headerPanel("Spinifex app - radial manual tour"),
  
  sidebarPanel(
    radioButtons("radio_data", label = h2("Radio buttons"),
                 choices = list("flea" = 1, "olive" = 2), 
                 selected = 1),
  
      verbatimTextOutput("data_value"),
    
    # selectInput('guide', 'Guided tour basis', 
    #             c('holes()','cmass()','lda_pp()'), 'holes()', multiple = FALSE),
    
    selectInput('mvar', 'Manip var', colnames(f_dat), selected = "aede2"),
    
    checkboxInput("show", "More options"),
    
    conditionalPanel("input.show",
                     selectInput('axes', 'Reference axes', 
                                 c('center','bottomleft','off'), 'center', multiple = FALSE),
                     sliderInput('angle', 'Angular speed', min=.025, max=.3, value=.1),
                     sliderInput('fps', 'Frames/sec', min=1, max=6, value=3)
    )
  ),
  mainPanel(
    plotlyOutput("plotlytest")
  )
)

server <- function(input, output) {
  
  output$data_value <- renderPrint({ #input$radio_data 
    if (input$radio_data == 1) {"flea dataset"
    } else if (input$radio_data == 2) {"olive dataset"
    } else {"data not found"}
  })
  
  output$plotlytest <- renderPlotly({
    play_manual_tour(data = f_dat, basis = f_bas, 
                     manip_var = which( colnames(f_dat)==input$mvar ),
                     col = f_cat, pch = f_cat,
                     axes = input$axes,
                     angle = input$angle, 
                     fps = input$fps) #axes, fps, angle, 
  })

}

shinyApp(ui = ui, server = server)

