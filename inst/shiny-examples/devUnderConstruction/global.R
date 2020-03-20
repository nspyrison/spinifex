##### Setup ----
.include_dev_display <- T
.include_debug_msg   <- T
.debug_msg_counter   <- 0

##TODO: go to custom loggin and remove this:
options(shiny.reactlog = TRUE) # Logging with reactlog

# options(error = recover)
# options(warning = recover)

require("spinifex")
require("ggplot2")
require("tibble")
require("shinythemes") ## Themes for shiny, think css files.
require("shinyBS")     ## BootStrap functionality, see ?shinyBS::bsTooltip
require("shinyjs")     ## Extend JS control and formating, see ?shinyjs::toggle
require("reactlog")    ## Custom shiny logging
require("DT")          ## HTML tabbles for the gallery table

contextLine <- paste0("Spinifex app --- spinifex (v" ,packageVersion("spinifex"),") --- ", Sys.Date())

## for saving files use:
# write.csv(tourr::flea, file="./inst/shiny-examples/comparison/flea.csv",row.names=FALSE)
# save(df, file="./inst/shiny-examples/comparison/df.rda")


appDebugMsg <- function(actionNm = NULL){
  if (.include_debug_msg == F) return()
  if (!is.character(actionNm)) actionNm <- substitute(actionNm)
  .debug_msg_counter <<- .debug_msg_counter + 1
  message(paste0("Observe #", .debug_msg_counter, "; obs of ", actionNm))
  if (is.null(actionNm) == T) environment()
  return()
}
M <- function(assumptionNm = "An assumption"){
  msg <- paste0(substitute(assumptionNm), " was not met.")
  message(msg)
}

### PROJECTION PURSUIT ----
appGetGuidedTour <- function(indexName, clId = NA){ # returns a tour function
  if(indexName == "holes") {return(guided_tour(holes()))}
  if(indexName == "cmass") {return(guided_tour(cmass()))}
  if(indexName == "lda_pp"){return(guided_tour(lda_pp(clId)))}
  if(indexName == "pda_pp"){return(guided_tour(pda_pp(clId)))}
  else return(error("index not found"))
} #ex # animate_xy(flea[, 1:6], guided_tour(pda_pp(flea$species)), sphere = TRUE)
### END OF PROJECTION PURSUIT

##### GALLERY ----
appShinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}