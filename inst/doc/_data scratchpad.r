#install.packages("mlbench")
#install.packages("rattle.data")
library("mlbench")
library("rattle.data")

### mlbench::BreastCancer
data("BreastCancer")
d <- BreastCancer
d <- d[!duplicated(d),]
d <- d[complete.cases(d),]
Id <- d$Id
Class <- d$Class
d <- as.matrix(d[, 1:9]) # remove `Id` and `Class`
d <- apply(d, 2, as.numeric)
BreastCancer <- as_tibble(cbind(d, Id, Class))
save(BreastCancer, file = "./data/BreastCancer.rda")

### mlbench::Soybean
data(Soybean)
d <- Soybean
d <- d[!duplicated(d), ]
d <- d[complete.cases(d), ]
date <- d$date
class <- d$Class
d <- as.matrix(d[ , 3:36]) # remove `Class` and `date`
d <- apply(d, 2, as.numeric)
soybean <- as_tibble(cbind(d, date, class))
save(soybean, file = "./data/soybean.rda")

### rattle.data::wine
wine <- rattle.data::wine
d <- wine
Type <- d$Type
d <- as.matrix(d[ , 2:14])
d <- apply(d, 2, as.numeric)
wine <- as_tibble(cbind(d, Type))
save(wine, file = "./data/wine.rda")

### rattle.data::weatherAUS
weatherAUS <- rattle.data::weatherAUS
d <- weatherAUS
d <- d[lubridate::year(d$Date) %in% c(2008, 2010, 2012, 2014, 2016), #~half obs.
       !names(d) %in% c("Evaporation", "Sunshine", "WindGustDir", "WindDir9am",
                        "WindDir3pm","RainToday", "RainTomorrow", "Cloud9am", 
                        "Cloud3pm")
       ] #remove categorical and common NA's. # no dups remain. 
d <- d[complete.cases(d), ]
Date <- d$Date
Location <- d$Location
d <- as.matrix(d[ , 3:14]) # remove `Date` and `Location`
d <- apply(d, 2, as.numeric)
weatherAUS <- as_tibble(cbind(d, Date, Location))
save(weatherAUS, file = "./data/weatherAUS.rda")
