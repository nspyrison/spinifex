##### Preamble ----
options(shiny.reactlog = TRUE) # Logging with reactlog

library("shiny")
library("plotly")
library("spinifex")
library("dplyr")
##### include in requireNamespace() in run_app:
library("DT")          # Gallery table
library("reactlog")    # Logging
library("purrr")       # sparkplot prep

## for saving files:
# write.csv(tourr::flea, file="./inst/shiny-examples/comparison/flea.csv",row.names=FALSE)
# save(df, file="./inst/shiny-examples/comparison/df.rda")


### STATIC LINEAR PROJECTIONS ----
staticProjection <- function(dat, method, col, pch, alpha) {
  if (method == "PCA") {
    return(suppressWarnings(
      ggfortify::autoplot(prcomp(dat), colour = col_of(col), shape = pch_of(pch) + 15,
                          alpha = alpha, loadings = T, loadings.label = T,
                          loadings.colour = 'gray50', loadings.label.colour = 'gray30')
    ))
  }
  if (method == "SPLOM") {
    splom_pch <- as.character(pch)
    return(
      GGally::ggpairs(data = dat, alpha = min(alpha/5, .2),
                      aes(colour = col, shape = splom_pch)
      )
    )
  }
}
## TEST STATIC
# dat <- tourr::flea[,1:6]; method <- "PCA"
# col <- col_of(tourr::flea[,7]); pch <- pch_of(tourr::flea[,7]); alpha=.5
# staticProjection(dat, method, col, pch, alpha)
### END OF STATIC

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