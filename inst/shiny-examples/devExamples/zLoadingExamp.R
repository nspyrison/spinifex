runApp(list(
  ui = pageWithSidebar(
    headerPanel("Test"),
    sidebarPanel(
      HTML('<script type="text/javascript">
        $(document).ready(function() {
          $("#DownloadButton").click(function() {
            $("#Download").text("Loading...");
          });
        });
      </script>
'),
      tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #CCFF66;
               z-index: 105;
             }
          ")),
      numericInput('n', 'Number of obs', 100),
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div("Loading...",id="loadmessage"))
    ),
    mainPanel(plotOutput('plot'))
  ),
  server = function(input, output) {
    showModal(modalDialog("Doing a function", footer=NULL))
    output$plot <- renderPlot({ Sys.sleep(2); hist(runif(input$n)) })
    removeModal()
  }
))
