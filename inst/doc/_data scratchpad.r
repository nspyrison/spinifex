#install.packages("mlbench")
#install.packages("rattle.data")
library("mlbench")
library("rattle.data")

### mlbench::BreastCancer
data("BreastCancer")
d <- BreastCancer[ , 2:11]
d <- d[!duplicated(d), ]
d <- d[complete.cases(d), ]
labels <- d$Class
d <- as.matrix(d[ , 1:9])
d <- apply(d, 2, as.numeric)

BreastCancer <- bind_cols(c(d, labels))
save(BreastCancer, file = "./data/BreastCancer.rda")

### mlbench::Soybean
data("Soybean")
d <- Soybean
d <- d[!duplicated(d[,2:36]), ]
d <- d[complete.cases(d[,2:36]), ]
labels <- d$Class
d <- as.matrix(d[ , 2:36])
d <- apply(d, 2, as.numeric)

Soybean <- embed(labels, d)
save(Soybean, file = "./data/Soybean")

### rattle.data::wine
data("wine")
str(wine)
save(wine, file = "./data/wine.rda")

### rattle.data::weatherAUS
data("weather")
save(weather, file = "./data/weather.rda")