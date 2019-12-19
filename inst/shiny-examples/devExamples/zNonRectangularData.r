# # -------------------------------------------------------------------------------
# EXCERPT FOR FEATURE SELECTION ON NON-RECTANGULAR DATA FROM SEVVANDI
# https://github.com/sevvandi/Outlier-Basis/blob/master/Scripts_And_Data/Figures_For_Paper_2.R
#
# EX 5 - LESMIS DATASET
# -------------------------------------------------------------------------------

# CHANGE FOLDER TO DIRECTORY OF SCRIPTS AND DATA IN THE LINE BELOW
folder <- paste( getwd(), "/paper/", sep="")
source(paste(folder, "/Functions_For_Paper.R", sep=""))

# IF DOBIN IS NOT INSTALLED, PLEASE RUN THE NEXT LINE
# devtools::install_github("sevvandi/dobin")

library("dobin")
library("OutliersO3")
library("graphics")
library("ggplot2")
library("mbgraphic")
library("gridExtra")
library("tidyverse")
library("SOMbrero")



# -------------------------------------------------------------------------------
# EX 5 - LESMIS DATASET, sLes Misérables (/le?? ??m??z??'r????b??l, -bl??)
# -------------------------------------------------------------------------------
data(lesmis)
igraph.options(plot.layout=layout.circle, vertex.size=10)
plot(lesmis,vertex.label.color="black", vertex.label.dist=1, vertex.edge.dist=20, vertex.size=4, vertex.label.cex=0.7)

# Feature selection
centrality <- alpha_centrality(lesmis, alpha=0.9)
# https://igraph.org/r/doc/alpha_centrality.html
trans <- transitivity(lesmis, type="local", vids=V(lesmis))
trans[which(is.nan(trans))] <- 0
# https://igraph.org/r/doc/transitivity.html
close <- closeness(lesmis)
between <- betweenness(lesmis)
deg <- degree(lesmis)
neigh <- knn(lesmis)
neigh <- neigh$knn
pgrk <- page_rank(lesmis)
pgrk <- pgrk$vector

# Colation and prep
dat <- cbind.data.frame(centrality,trans, close, between, deg, neigh, pgrk)
dat <- as.matrix(dat)
row.names(dat) <- V(lesmis)$label
head(dat)
cat("boom, ready for projection! 
similar approach for time series, book data, etc.")

