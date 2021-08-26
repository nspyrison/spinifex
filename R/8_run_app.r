#' Runs a shiny app demonstrating manual tours
#' 
#' Runs a local shiny app that demonstrates manual tour and comparable 
#' traditional techniques for static projections of multivariate data sets.
#' 
#' @param app_nm name of the shiny app to run. Expects "manual_tour".
#' @param ... Other arguments passed into `shiny::runApp()`. 
#' Such as display.mode = "showcase".
#' @return Runs a locally hosted shiny app.
#' @export
#' @examples
#' \dontrun{
#' run_app("radial_tour")
#' run_app(app_nm = "radial_tour", display.mode = "showcase")}
# For adjusting or adding more apps it may be useful to read: 
# https://deanattali.com/2015/04/21/r-package-shiny-app/
run_app <- function(app_nm = 'radial_tour', ...) {
  ### Additional dependencies for shiny app.
  shiny_depends <-  c("shinythemes", "shinycssloaders", "plotly")
  pkgs_needed <- !sapply(shiny_depends, requireNamespace)
  if(max(pkgs_needed) == TRUE) { ## Needs at least 1 package.
    .inst_code <-
      paste0("install.packages('", shiny_depends[pkgs_needed], "'); ", collapse = "")
    stop(
      paste0("app_nm = '", app_nm, "' has additional dependancies that were not found. ",
             "Please run the following code before trying again.  \n", .inst_code),
      call. = FALSE)
  }
  
  ## Locate all the shiny app names that exist
  valid_app_nms <- list.files(system.file("shiny_apps", package = "spinifex"))
  valid_msg <- paste0(
    "Valid app_nm : '", paste(valid_app_nms, collapse = "', '"), "'")
  
  ## If an invalid app_nm is given, throw an error
  if(!nzchar(app_nm) || !(app_nm %in% valid_app_nms)) {
    stop(paste0('App_nm was missing or invalid. \n', valid_msg,
                "\n are the only valid app_nm at the for this version."),
         call. = FALSE)
  }
  
  ## Find and launch the app
  app_dir <- system.file("shiny_apps", app_nm, package = "spinifex")
  shiny::runApp(app_dir, ...)
}
