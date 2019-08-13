##### Preamble ----
library(shiny)
library(plotly)
library(spinifex)
library(dplyr)
### Projection pursuit indices
library(scagnostics) # "Skinny", "Striated", "Convex", "Clumpy" 
library(mbgraphic)   # splines2d, dcor2d
library(minerva)     # MIC, TIC

## for saving files:
# write.csv(tourr::flea, file="./inst/shiny-examples/comparison/flea.csv",row.names=FALSE)
# save(df, file="./inst/shiny-examples/comparison/df.rda")

### STATIC LINEAR PROJECTIONS ----
staticProjection <- function(dat, method, col, pch, alpha) {
  if (method == "PCA") {
    library(ggfortify)
    return(suppressWarnings(
      autoplot(prcomp(dat), colour = col_of(col), shape = pch_of(pch) + 15,
               alpha = alpha, loadings = T, loadings.label = T,
               loadings.colour = 'gray50', loadings.label.colour = 'gray30')
    ))
  }
  if (method == "SPLOM") {
    library(GGally)
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
guidedTourOptions <- c("cmass", "holes", "Skinny", "Striated", "Convex", 
                       "Clumpy", "splines2d", "dcor2d", "MIC", "TIC")
scags <- function(scagMetricIndex) {
  function(mat) {return (scagnostics(mat)[scagMetricIndex])}
}
getGuidedTour <- function(indexName, grId=NA){ # reurtns a tour function
  if(indexName == "cmass"){return(guided_tour(cmass()))}
  if(indexName == "holes"){return(guided_tour(holes()))}
  if(indexName %in% c("Skinny", "Striated", "Convex", "Clumpy")){return(guided_tour(scags(indexName)))}
  if(indexName == "splines2d"){return(guided_tour(splineIndex()))}
  if(indexName == "dcor2d"){return(guided_tour(dcorIndex()))}
  if(indexName %in% c("MIC", "TIC")){return(guided_tour(mineIndex(indexName)))}
  if(indexName == "lda_pp"){return(guided_tour(lda_pp(grId)))} #TODO:no grID
  if(indexName == "pda_pp"){return(guided_tour(pda_pp(grId)))} # as of now
  else return(guided_tour(holes()))
}
### END OF PROJECTION PURSUIT
