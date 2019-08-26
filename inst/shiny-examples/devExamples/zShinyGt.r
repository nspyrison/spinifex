gt_tbl <-
  gtcars %>%
  gt() %>%
  cols_hide(contains("_"))

ui <- fluidPage(
  
  gt_output(outputId = "table")
)

server <- function(input,
                   output,
                   session) {
  
  output$table <-
    render_gt(
      expr = gt_tbl,
      height = px(600),
      width = px(600)
    )
}
# NOT RUN {
shinyApp(ui, server)
# }