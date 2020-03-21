##### Setup ----
.include_dev_display <- T
.include_obs_msg     <- T
.obs_msg_counter     <- 0

require("spinifex")
require("ggplot2")
require("ggrepel")
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


appObsMsg <- function(obsNm = NULL){
  if (.include_obs_msg == FALSE) return()
  if (!is.character(obsNm)) obsNm <- substitute(obsNm)
  .obs_msg_counter <<- .obs_msg_counter + 1
  message(paste0("Observe #", .obs_msg_counter, "; obs of ", obsNm))
  if (is.null(obsNm) == T) environment()
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