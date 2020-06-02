##### Setup ----
.include_dev_display  <- T
.include_obs_msg      <- T
.run_in_showcase_mode <- F

require("spinifex")
require("ggplot2")
require("ggrepel")
require("tibble")
require("shinythemes") ## Themes for shiny, think css files.

### Used in 'primary' and 'devUnderConstruction'.
require("shinyBS")     ## BootStrap functionality, such as tooltips and popovers
                       ##   also see ?shinyBS::bsTooltip   &   https://github.com/ebailey78/shinyBS/
require("shinyjs")     ## Extend JavaScript (Think HTML interactivity) control and formating, 
                       ##   also see ?shinyjs::toggle   &   https://daattali.com/shiny/shinyjs-basic/
require("reactlog")    ## Custom shiny logging
require("DT")          ## HTML tabbles for the gallery table

## for saving files use:
# write.csv(tourr::flea, file="./inst/shiny_apps/comparison/flea.csv",row.names=FALSE)
# save(df, file="./inst/shiny_apps/comparison/df.rda")

.obs_msg_counter     <- 0
##### Creates a string for App name, spinifex version, and sys date.
.wd <- getwd()
.regex <- regexpr("\\/[^\\/]*$", .wd)
.local_path <- substr(.wd, .regex + 1, nchar(.wd))
contextLine <- paste0("Spinifex app, '", .local_path, 
                      "' --- (spinifex v", packageVersion("spinifex"),
                      ") --- Ran on ", Sys.Date()
)

#' messages the console with OBServe counter and increments it 
#' IFF .include_obs_msg == TRUE
appObsMsg <- function(obsNm = NULL){
  if (.include_obs_msg == FALSE) return()
  if (!is.character(obsNm)) obsNm <- substitute(obsNm)
  .obs_msg_counter <<- .obs_msg_counter + 1
  message(paste0("Observe #", .obs_msg_counter, "; obs of ", obsNm))
  if (is.null(obsNm) == T) environment()
  return()
}

## If remote messaging/cat() is needed use the following:
# cat(file=stderr(), ..., "\n")

###  
msgAssumption <- function(assumptionNm = "An assumption"){
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

