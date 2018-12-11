#install.packages("mlbench")
#install.packages("rattle.data")
library("mlbench")
library("rattle.data")

### mlbench::BreastCancer\
library("mlbench")
?BreastCancer
data("BreastCancer")
d <- BreastCancer
d <- d[!duplicated(d), ]
d <- d[complete.cases(d), ]
mat <- as.matrix(d[ , 2:9])
mat <- apply(mat, 2, as.numeric)

BreastCancer <- as.tibble(data.frame(Id = d$Id, mat, Class = d$Class))
save(BreastCancer, file = "./data/BreastCancer.rda")

### rattle.data::wine
?wine
data("wine")
wine <- as.tibble(wine)
save(wine, file = "./data/wine.rda")

### rattle.data::weatherAUS
?weather # Canberra airport, Nov 2007 - Nov 2008
library("rattle.data")
data("weather")
weather <- as.tibble(weather[ , c(1,3:7,9,12:24)])
save(weather, file = "./data/weather.rda")

### tourr::flea
?flea
library("tourr")
data("flea")
flea <- as.tibble(flea)
save(flea, file = "./data/flea.rda")
