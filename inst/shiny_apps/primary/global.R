##### Preamble ----
.include_dev_display <- T
.include_debug_msg   <- T
.debug_msg_counter   <- 0

require("shiny")
require("plotly")
require("spinifex")
require("tibble")
##### include in requireNamespace() in run_app:
require("DT")          # Gallery table
require("shinythemes") ## Themes for shiny, think css files.
require("shinyBS")     ## BootStrap functionality, see ?shinyBS::bsTooltip
require("shinyjs")     ## Extend JS control and formating, see ?shinyjs::toggle

contextLine <- paste0("Spinifex app --- spinifex (v" ,packageVersion("spinifex"),") --- ", Sys.Date())

## for saving files:
# write.csv(tourr::flea, file="./inst/shiny_apps/comparison/flea.csv",row.names=FALSE)
# save(df, file="./inst/shiny_apps/comparison/df.rda")


appDebugMsg <- function(actionNm = NULL){
  if (.include_debug_msg == F) return()
  if (!is.character(actionNm)) actionNm <- substitute(actionNm)
  .debug_msg_counter <<- .debug_msg_counter + 1
  message(paste0("Observe #", .debug_msg_counter, "; obs of ", actionNm))
  if (is.null(actionNm) == T) environment()
  return()
}


### PROJECTION PURSUIT
getGuidedTour <- function(indexName, grId = NA){ # returns a tour function
  if(indexName == "holes"){return(guided_tour(holes()))}
  if(indexName == "cmass"){return(guided_tour(cmass()))}
  if(indexName == "lda_pp"){return(guided_tour(lda_pp(grId)))}
  if(indexName == "pda_pp"){return(guided_tour(pda_pp(grId)))}
  else return(error("index not found"))
} 

##### GALLERY
shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}