##### Preamble ----
options(shiny.reactlog = TRUE) # Logging with reactlog

requireNamespace("shiny")
requireNamespace("plotly")
requireNamespace("spinifex")
requireNamespace("dplyr")
##### include in requireNamespace() in run_app:
requireNamespace("DT")          # Gallery table
requireNamespace("reactlog")    # Logging
requireNamespace("purrr")       # sparkplot prep

## for saving files:
# write.csv(tourr::flea, file="./inst/shiny-examples/comparison/flea.csv",row.names=FALSE)
# save(df, file="./inst/shiny-examples/comparison/df.rda")

### PROJECTION PURSUIT ----
getGuidedTour <- function(indexName, grId = NA){ # returns a tour function
  if(indexName == "holes"){return(guided_tour(holes()))}
  if(indexName == "cmass"){return(guided_tour(cmass()))}
  if(indexName == "lda_pp"){return(guided_tour(lda_pp(grId)))}
  if(indexName == "pda_pp"){return(guided_tour(pda_pp(grId)))}
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