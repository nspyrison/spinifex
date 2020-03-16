#' Runs a shiny app demonstrating manual tours
#' 
#' Runs a local shiny app that demonstrates manual tour and comparable 
#' traditional techniques for static projections of multivariate data sets.
#' 
#' @param example name of the shiny app to run. Expects "intro" or "primary".
#' @return opens a local shiny app
#' @export
#' @examples 
#' \dontrun{
#' run_app(example = "intro")
#' run_app(example = "primary")
#' }


# For adjusting or adding more apps it may be useful to read: 
# https://deanattali.com/2015/04/21/r-package-shiny-app/
run_app <- function(example) {
  if (example %in% c("primary", "devUnderConstruction")) {
    requireNamespace("skinythemes") ## Themes for shiny, think css files.
    requireNamespace("shinyBS")     ## BootStrap functionality, see ?shinyBS::bsTooltip
    requireNamespace("shinyjs")     ## Extend JS control and formating, see ?shinyjs::toggle
    requireNamespace("reactlog")    ## Custom shiny logging
    requireNamespace("DT")          ## HTML tabbles for the gallery table
  }
  
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shiny-examples", package = "spinifex"))
  
  validExamplesMsg <- paste0("Valid examples are: '",
                             paste(validExamples, collapse = "', '"),
                             "'")
  
  # if an invalid example is given, throw an error
  if (missing(example) || !nzchar(example) ||
      !example %in% validExamples) {
    stop(
      'Please run `runExample()` with a valid example app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }
  
  # find and launch the app
  appDir <- system.file("shiny-examples", example, package = "spinifex")
  shiny::runApp(appDir, display.mode = "normal")
}