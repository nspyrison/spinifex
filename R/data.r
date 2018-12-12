#' Flea beatle measurements
#'
#' This data is from a paper by A. A. Lubischew, "On the Use of Discriminant
#' Functions in Taxonomy", Biometrics, Dec 1962, pp.455-477.
#'
#' Data frame (tibble) 74 observations of 6 numeric variables, followed
#' by target class, `spieces`:
#' \itemize{
#'   \item tars1, width of the first joint of the first tarsus in microns
#'   (the sum of measurements for both tarsi)
#'   \item tars2, the same for the second joint
#'   \item head, the maximal width of the head between the external edges of the
#'   eyes in 0.01 mm
#'   \item ade1, the maximal width of the aedeagus in the fore-part in microns
#'   \item ade2, the front angle of the aedeagus ( 1 unit = 7.5 degrees)
#'   \item ade3, the aedeagus width from the side in microns
#'   \item species, which species is being examined - concinna, heptapotamica, heikertingeri
#' }
#'
#' @name Flea measurements
#' @aliases flea
#' @docType data
#' @format Data frame (tibble) 74 observations of 6 numeric variables, followed
#' by target class, `spieces`
#' @keywords datasets
#' @examples
#'
#' head(flea)
"flea"

#' Wisconsin Breast Cancer Database
#'
#' Formatted subset of `mlbench::BreastCancer`. See `mlbench` for original data 
#' more context.
#' 
#' The objective is to identify each of a number of benign or malignant classes.
#' Samples arrive periodically as Dr. Wolberg reports his clinical cases. The 
#' database therefore reflects this chronological grouping of the data. This 
#' grouping information appears immediately below, having been removed from 
#' the data itself. Each variable except for the first was converted into 11 
#' primitive numerical attributes with values ranging from 0 through 10. There 
#' are 16 missing attribute values.
#'
#' Data frame (tibble) with 675 observations on 10 variables: a factor Id, 
#' 9 numeric variables, and target class:
#' \itemize{
#'   \item Id, Sample code number
#'   \item Cl.thickness, Clump thickness
#'   \item Cell.size, Uniformity of cell size
#'   \item Cell.shape, Uniformity of cell shape
#'   \item Marg.adhesion, Marginal adhesion
#'   \item Epith.c.size, Single Epthelial cell size
#'   \item Bare.nuclei, Bare nuclei
#'   \item Bl.cromatin, Bland chromatin
#'   \item Normal.nucleoli, Normal Nucleoli
#'   \item Class, Class
#' }
#' 
#' Reproducing this dataset:
#' `library("mlbench")`
#' `d <- BreastCancer`
#' `d <- d[!duplicated(d), ]`
#' `d <- d[complete.cases(d), ]`
#' `mat <- as.matrix(d[ , 2:9])`
#' `mat <- apply(mat, 2, as.numeric)`
#' `BreastCancer <- as.tibble(data.frame(Id = d$Id, mat, Class = d$Class))`
#'
#' @name BreastCancer cells
#' @aliases breastcancer
#' @docType data
#' @format Data frame (tibble) with 675 observations on 10 variables: a factor 
#' Id, 9 numeric variables, and target class.
#' @keywords datasets
#' @examples
#'
#' head(BreastCancer)
"BreastCancer"

#' The wine dataset from the UCI Machine Learning Repository.
#' 
#' A subset from `rattle.data::weather`, instructions to reproduce below.
#' 
#' One year of daily weather observations collected from the Canberra airport 
#' in Australia was obtained from the Australian Commonwealth Bureau of 
#' Meteorology and processed to create this sample dataset for illustrating 
#' data mining using R and Rattle.
#' 
#' The data has been processed to provide a target variable RainTomorrow 
#' (whether there is rain on the following day - No/Yes) and a risk variable 
#' `RISK_MM`` (how much rain recorded in millimetres). Various transformations 
#' were performed on the source data. The dataset is quite small and is useful 
#' only for repeatable demonstration of various data science operations.
#' 
#' The source dataset is Copyright by the Australian Commonwealth Bureau of 
#' Meteorology and is provided as part of the rattle package with permission.
#'
#' Data frame (tibble) of 366 observations of 20 variables, one year of 
#' daily observations of weather variables at Canberra airport in Australia 
#' starting November 2007:
#' \itemize{
#'   \item Date, The date of observation (a Date object).
#'   \item MinTemp, The minimum temperature in degrees celsius.
#'   \item MaxTemp, The maximum temperature in degrees celsius.
#'   \item Rainfall, The amount of rainfall recorded for the day in mm.
#'   \item Evaporation, The so-called Class A pan evaporation (mm) in the 24 hours to 9am.
#'   \item Sunshine, The number of hours of bright sunshine in the day.
#'   \item WindGustSpeed, The speed (km/h) of the strongest wind gust in the 24 hours to midnight.
#'   \item WindSpeed9am, Wind speed (km/hr) averaged over 10 minutes prior to 9am.
#'   \item WindSpeed3pm, Wind speed (km/hr) averaged over 10 minutes prior to 3pm.
#'   \item Humid9am, Relative humidity (percent) at 9am.
#'   \item Humid3pm, Relative humidity (percent) at 3pm.
#'   \item Pressure9am, Atmospheric pressure (hpa) reduced to mean sea level at 9am.
#'   \item Pressure3pm, Atmospheric pressure (hpa) reduced to mean sea level at 3pm.
#'   \item Cloud9am, Fraction of sky obscured by cloud at 9am. This is measured in "oktas", which are a unit of eigths. It records how many eigths of the sky are obscured by cloud. A 0 measure indicates completely clear sky whilst an 8 indicates that it is completely overcast.
#'   \item Cloud3pm, Fraction of sky obscured by cloud (in "oktas": eighths) at 3pm. See Cload9am for a description of the values.
#'   \item Temp9am, Temperature (degrees C) at 9am.
#'   \item Temp3pm, Temperature (degrees C) at 3pm.
#'   \item RainToday, Integer: 1 if precipitation (mm) in the 24 hours to 9am exceeds 1mm, otherwise 0.
#'   \item RISK_MM, The amount of rain. A kind of measure of the "risk".
#'   \item RainTomorrow, The target variable. Did it rain tomorrow?
#' }
#' 
#' Reproducing this dataset:
#' `library("rattle.data")``
#' `weather <- as.tibble(weather[ , c(1,3:7,9,12:24)])``
#'
#' @name weather
#' @aliases Weather
#' @docType data
#' @format Data frame (tibble) of 366 observations of 20 variables, one year of 
#' daily observations of weather variables at Canberra airport in Australia 
#' starting November 2007.
#' @source The daily observations are available from http://www.bom.gov.au/climate/data. 
#' Copyright Commonwealth of Australia 2010,
#' Bureau of Meteorology. 
#'
#' Definitions adapted from http://www.bom.gov.au/climate/dwo/IDCJDW0000.shtml
#' @references Data source: http://www.bom.gov.au/climate/dwo/ and http://www.bom.gov.au/climate/data.
#' @keywords datasets
#' @examples
#'
#' head(weather)
"weather"

#' The wine dataset from the UCI Machine Learning Repository.
#'
#' The wine dataset contains the results of a chemical analysis of wines grown 
#' in a specific area of Italy. Three types of wine are represented in the 178 
#' samples, with the results of 13 chemical analyses recorded for each sample. 
#' The Type variable has been transformed into a categoric variable.
#' 
#' The data contains no missing values and consits of only numeric data, with a 
#' three class target variable (Type) for classification.
#'
#' Data frame (tibble) of 178 observations of 13 variables, target 
#' class `Type` and 12 numeric variables:
#' \itemize{
#'   \item Type, The type of wine, into one of three classes, 1 (59 obs), 
#'   2(71 obs), and 3 (48 obs).
#'   \item Alcohol, Alcohol
#'   \item Malic, Malic acid
#'   \item Ash, Ash
#'   \item Alcalinity, Alcalinity of ash
#'   \item Magnesium, Magnesium
#'   \item Phenols, Total phenols
#'   \item Flavanoids, Flavanoids
#'   \item Nonflavanoids, Nonflavanoid phenols
#'   \item Proanthocyanins, Proanthocyanins
#'   \item Color, Color intsity
#'   \item Hue, Hue
#'   \item Dilution, D280/OD315 of diluted wines
#'   \item Proline, Proline
#'
#' @name wine
#' @aliases Wine
#' @docType data
#' @format data frame (tibble) of 178 observations of 13 variables, target 
#' class `Type` and 12 numeric variables.
#' @examples
#'
#' head(wine)
"wine"