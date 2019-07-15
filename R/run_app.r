#' Runs a shiny app demonstrating manual tours
#' 
#' Runs a local shiny app that demonstrates manual tour and comparable 
#' traditional techniques for static projections of multivariate data sets.
#' 
#' @return opens a local shiny app
#' 
#' @examples 
#' run_app()
#' @export

# For adjusting or adding more apps it may be useful to follow: 
# https://deanattali.com/2015/04/21/r-package-shiny-app/
run_app <- function() {
  appDir <- system.file("shiny-examples", "spinifex_app", package = "spinifex")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `spinifex`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
