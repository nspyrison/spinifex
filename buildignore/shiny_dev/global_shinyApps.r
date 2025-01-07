##### Setup ----
.include_dev_display  <- T
.include_obs_msg      <- T

requireNamespace("spinifex")
requireNamespace("tourr")
requireNamespace("ggplot2")
requireNamespace("tibble")
requireNamespace("shinythemes") ## Themes for shiny, think css files.
requireNamespace("shinyjs")     ## Extend JavaScript (Think HTML interactivity) control and formating, 
## Also see ?shinyjs::toggle   &   https://daattali.com/shiny/shinyjs-basic/
##### Additionally used in 'primary' and 'devUnderConstruction':
requireNamespace("shinyBS")  ## BootStrap functionality, such as tooltips and popovers
## Also see ?shinyBS::bsTooltip   &   https://github.com/ebailey78/shinyBS/
requireNamespace("reactlog") ## Custom shiny logging
requireNamespace("DT")       ## HTML tabbles for the gallery table

## For saving files use:
# write.csv(tourr::flea, file="./inst/shiny_apps/comparison/flea.csv",row.names=FALSE)
# save(df, file="./inst/shiny_apps/comparison/df.rda")

.obs_msg_counter <- 0
##### Creates a string for App name, spinifex version, and sys date.
.wd <- getwd()
.regex <- regexpr("\\/[^\\/]*$", .wd)
.local_path <- substr(.wd, .regex + 1, nchar(.wd))
contextLine <- paste0("Spinifex app, '", .local_path, 
                      "' --- (spinifex v", packageVersion("spinifex"),
                      ") --- Ran on ", Sys.Date())

# messages the console with OBServe counter and increments it 
# IFF .include_obs_msg == TRUE
appObsMsg <- function(obsNm = NULL){
  if(.include_obs_msg == FALSE) return()
  if(!is.character(obsNm)) obsNm <- substitute(obsNm)
  .obs_msg_counter <<- .obs_msg_counter + 1
  message(paste0("Observe #", .obs_msg_counter, "; obs of ", obsNm))
  if(is.null(obsNm) == T) environment()
  return()
}

##### GLOBALL HELPER FUNCTIONS:
### PROJECTION PURSUIT ----
appGetGuidedTour <- function(indexName, clId = NA){ ## Returns a tourr::guided_tour(PP()) tourr function
  if(indexName == "holes") {return(guided_tour(holes()))}
  if(indexName == "cmass") {return(guided_tour(cmass()))}
  if(indexName == "lda_pp"){return(guided_tour(lda_pp(clId)))}
  if(indexName == "pda_pp"){return(guided_tour(pda_pp(clId)))}
  else return(error("index not found"))
} #ex # animate_xy(flea[, 1:6], tourr::guided_tour(tourr::pda_pp(flea$species)), sphere = TRUE)
### END OF PROJECTION PURSUIT

##### GALLERY ----
appShinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}

