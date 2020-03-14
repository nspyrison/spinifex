##### Setup ----
include_dev_display <- T

library("shiny")
library("plotly")
library("spinifex")
library("dplyr")
## include in requireNamespace() in run_app:
library("shinythemes")
library("shinyBS")     # BootStrap controls, _ie_ ?shinyBS::bsTooltip
library("shinyjs")     # futher control, esp. toggle()
library("DT")          # Gallery table, HTML in cells
library("reactlog")    # Logging

options(shiny.reactlog = TRUE) # Logging with reactlog


signOff <- paste0("Spinifex app --- spinifex (v" ,packageVersion("spinifex"),") --- ", Sys.Date())

## for saving files use:
# write.csv(tourr::flea, file="./inst/shiny-examples/comparison/flea.csv",row.names=FALSE)
# save(df, file="./inst/shiny-examples/comparison/df.rda")


### PROJECTION PURSUIT ----
getGuidedTour <- function(indexName, clId = NA){ # returns a tour function
  if(indexName == "holes") {return(guided_tour(holes()))}
  if(indexName == "cmass") {return(guided_tour(cmass()))}
  if(indexName == "lda_pp"){return(guided_tour(lda_pp(clId)))}
  if(indexName == "pda_pp"){return(guided_tour(pda_pp(clId)))}
  else return(error("index not found"))
} #ex # animate_xy(flea[, 1:6], guided_tour(pda_pp(flea$species)), sphere = TRUE)
### END OF PROJECTION PURSUIT

##### GALLERY ----
shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}