library(shiny)
library(gravitas)
vic_elec <- tsibbledata::vic_elec

ui <- fluidPage(
  headerPanel(" Sayani's super awesome app"),
 
  sidebarPanel(
    selectInput('lgran', 'lowest gra', gravitas:::lookup_table$granularity, "hour"),
    selectInput('ugran', 'highest gran', gravitas:::lookup_table$granularity, "week"),
    selectInput('facet', 'facet Variable', "<select>"), #"<needs update>"), 
    # search_gran(vic_elec, "hour", "minute")
    selectInput('xcol', 'X Variable', "<select>"),
    selectInput('ycol', 'Y Variable', names(vic_elec),
      selected = names(vic_elec)[[2]]),
    selectInput('plot_type', 'plot type', c("boxplot"))

  ),
  mainPanel(
    plotOutput('plot1')
  )
)

server <- function(input, output, session) {

  selectedData <- reactive({
    vic_elec
  })


  observe({
    my_choices <- search_gran(vic_elec, input$ugran, input$lgran)
    updateSelectInput(session,
                      "facet",
                      choices = my_choices)
  })
  observe({
    my_choices <- search_gran(vic_elec, input$ugran, input$lgran)
    my_choices2 <- my_choices[-match(input$facet,my_choices)]
    updateSelectInput(session,
                      "xcol",
                      choices = rev(my_choices2))
  })


  # output$plot1 <- renderPlot({
  #   par(mar = c(5.1, 4.1, 0, 1))
  #   plot(selectedData(),
  #        pch = 20, cex = 3)
  # })

  output$plot1 <- renderPlot({
    suppressWarnings(
      granplot(.data = selectedData(),
               gran1 = input$facet,
               gran2 = input$xcol,
               response = input$ycol,
               plot_type = input$plot_type)
    )
  })

}

shinyApp(ui = ui, server = server)




# granplot(.data = vic_elec,
#          gran1 = "hour_day",
#          gran2 = "hour_day",
#          response = "Demand",
#          plot_type = "boxplot")
