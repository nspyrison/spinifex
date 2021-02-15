##### BreastCancer -----
#' Wisconsin Breast Cancer Database
#'
#' Formatted subset of `mlbench's` `BreastCancer` (not explicitly exported). 
#' See `help(BreastCancer, package = "mlbench")` for the original documentation.
#' 
#' The objective is to identify each of a number of benign or malignant classes.
#' Samples arrive periodically as Dr. Wolberg reports his clinical cases. The 
#' database therefore reflects this chronological grouping of the data. This 
#' grouping information appears immediately below, having been removed from 
#' the data itself. Each variable except for the first was converted into 11 
#' primitive numerical attributes with values ranging from 0 through 10. Rows
#' with missing attribute values and duplicate rows removed.
#'
#' Data frame with 675 observations of 11 variables: factor Id, 
#' 9 numeric variables, and target factor Class.
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
#'   \item Mitoses, Mitoses
#'   \item Class, Class of cancer, either "benign" or "malignant"
#' }
#' @details 
#' Reproducing this dataset:
#' ```
#' require("mlbench")
#' data(BreastCancer)
#' 
#' d <- BreastCancer
#' d <- d[!duplicated(d), ] ## Remove 8 duplicate rows
#' d <- d[complete.cases(d), ] ## Remove 16 row-wise incomplete rows
#' mat <- as.matrix(d[ , 2:10])
#' mat <- sapply(mat, as.integer)
#' BreastCancer <- 
#'   data.frame(Id = as.factor(d$Id), mat, Class = as.factor(d$Class))
#' ## save(BreastCancer, file = "./data/BreastCancer.rda")
#' ```
#' @name BreastCancer
#' @docType data
#' @format Data frame with 675 observations of 11 variables: factor Id, 
#' 9 integer variables, and target factor Class.
#' @examples
#' str(BreastCancer)
#' \dontrun{
#' dat <- scale_sd(BreastCancer[, 2:10])
#' clas <- BreastCancer$Class
#' bas <- prcomp(dat)$rotation[, 1:2]
#' mvar <- which(abs(bas[, 1]) == max(abs(bas[, 1])))
#' 
#' play_manual_tour(basis = bas, data = dat, manip_var = mvar,
#'                  render_type = render_gganimate, color = clas, shape = clas)
#' }
"BreastCancer"

##### PimaIndiansDiabetes_wide -----
#' Pima Indians Diabetes Database
#'
#' Formatted subset of `mlbench's` `PimaIndiansDiabetes2` (not explicitly exported). 
#' See `help(PimaIndiansDiabetes2, package = "mlbench")` for the original documentation.
#' 
#' The data set PimaIndiansDiabetes2 contains a corrected version of the 
#' original data set. While the UCI repository index claims that there are no 
#' missing values, closer inspection of the data shows several physical 
#' impossibilities, e.g., blood pressure or body mass index of 0. In 
#' PimaIndiansDiabetes2, all zero values of glucose, pressure, triceps, insulin 
#' and mass have been set to NA, see also Wahba et al (1995) and Ripley (1996).
#'
#' Data frame with 392 observations of 9 variables:
#' 8 numeric variables, and target factor diabetes.
#' \itemize{
#'   \item pregnant, Number of times pregnant
#'   \item glucose, Plasma glucose concentration (glucose tolerance test)
#'   \item pressure, Diastolic blood pressure (mm Hg)
#'   \item triceps, Triceps skin fold thickness (mm)
#'   \item insulin, 2-Hour serum insulin (mu U/ml)
#'   \item mass, Body mass index (weight in kg/(height in m)\^2)
#'   \item pedigree, Diabetes pedigree function
#'   \item age, Age (years)
#'   \item diabetes, Class variable (test for diabetes), either "pos" or "neg"
#' }
#' @details 
#' Reproducing this dataset:
#' ```
#' require("mlbench")
#' data(PimaIndiansDiabetes2)
#' 
#' d <- PimaIndiansDiabetes2
#' d <- d[complete.cases(d), ] ## Remove ~350 row-wise incomplete rows
#' PimaIndiansDiabetes_wide <- d
#' ## save(PimaIndiansDiabetes_wide, file = "./data/PimaIndiansDiabetes_wide.rda")
#' ```
#' @name PimaIndiansDiabetes_wide
#' @docType data
#' @format Data frame with 392 observations of 9 variables:
#' 8 numeric variables, and target factor diabetes.
#' @examples
#' str(PimaIndiansDiabetes_wide)
#' \dontrun{
#' dat <- scale_sd(PimaIndiansDiabetes_wide[, 1:8])
#' clas <- PimaIndiansDiabetes_wide$diabetes
#' bas <- prcomp(dat)$rotation[, 1:2]
#' mvar <- which(abs(bas[, 1]) == max(abs(bas[, 1])))
#' 
#' play_manual_tour(basis = bas, data = dat, manip_var = mvar),
#'                  render_type = render_gganimate, color = clas, shape = clas)
#' }
"PimaIndiansDiabetes_wide"

##### PimaIndiansDiabetes_long -----
#' Pima Indians Diabetes Database
#'
#' Formatted subset of `mlbench's` `PimaIndiansDiabetes2` (not explicitly exported). 
#' See `help(PimaIndiansDiabetes2, package = "mlbench")` for the original documentation.
#' 
#' The data set PimaIndiansDiabetes2 contains a corrected version of the 
#' original data set. While the UCI repository index claims that there are no 
#' missing values, closer inspection of the data shows several physical 
#' impossibilities, e.g., blood pressure or body mass index of 0. In 
#' PimaIndiansDiabetes2, all zero values of glucose, pressure, triceps, insulin 
#' and mass have been set to NA, see also Wahba et al (1995) and Ripley (1996).
#'
#' Data frame with 724 observations of 7 variables:
#' 6 numeric variables, and target factor diabetes.
#' \itemize{
#'   \item pregnant, Number of times pregnant
#'   \item glucose, Plasma glucose concentration (glucose tolerance test)
#'   \item pressure, Diastolic blood pressure (mm Hg)
#'   \item mass, Body mass index (weight in kg/(height in m)\^2)
#'   \item pedigree, Diabetes pedigree function
#'   \item age, Age (years)
#'   \item diabetes, Class variable (test for diabetes), either "pos" or "neg"
#' }
#' @details 
#' Reproducing this dataset:
#' ```
#' require("mlbench")
#' data(PimaIndiansDiabetes2)
#' 
#' d <- PimaIndiansDiabetes2
#' d <- d[, c(1:3, 6:9)] ## Remove 2 colulmns with the most NAs
#' d <- d[complete.cases(d), ] ## Remove ~44 row-wise incomplete rows
#' PimaIndiansDiabetes_long <- d
#' ## save(PimaIndiansDiabetes_long, file = "./data/PimaIndiansDiabetes_long.rda")
#' ```
#' @name PimaIndiansDiabetes_long
#' @docType data
#' @format Data frame with 724 observations of 7 variables:
#' 6 numeric variables, and target factor diabetes.
#' @examples
#' str(PimaIndiansDiabetes_long)
#' \dontrun{
#' dat <- scale_sd(PimaIndiansDiabetes_long[, 1:6])
#' clas <- PimaIndiansDiabetes_long$diabetes
#' bas <- prcomp(dat)$rotation[, 1:2]
#' mvar <- which(abs(bas[, 1]) == max(abs(bas[, 1])))
#' 
#' play_manual_tour(basis = bas, data = dat, manip_var = mvar,
#'                  render_type = render_gganimate, color = clas, shape = clas)
#' }
"PimaIndiansDiabetes_long"

##### weather -----
#' Sample dataset of daily weather observations from Canberra airport in Australia.
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
#' `RISK_MM` (how much rain recorded in millimeters). Various transformations 
#' were performed on the source data. The dataset is quite small and is useful 
#' only for repeatable demonstration of various data science operations.
#' 
#' The source dataset is Copyright by the Australian Commonwealth Bureau of 
#' Meteorology and is provided as part of the rattle package with permission.
#'
#' Data frame of 366 observations of 20 variables. One year of 
#' daily observations of weather variables at Canberra airport in Australia 
#' starting November 2007:
#' \itemize{
#'   \item Date, The date of observation (Date class).
#'   \item MinTemp, The minimum temperature in degrees Celsius.
#'   \item MaxTemp, The maximum temperature in degrees Celsius.
#'   \item Rainfall, The amount of rainfall recorded for the day in mm.
#'   \item Evaporation, The "Class A pan evaporation" (mm) in the 24 hours to 9am.
#'   \item Sunshine, The number of hours of bright sunshine in the day.
#'   \item WindGustSpeed, The speed (km/h) of the strongest wind gust in the 24 hours to midnight.
#'   \item WindSpeed9am, Wind speed (km/hr) averaged over 10 minutes prior to 9am.
#'   \item WindSpeed3pm, Wind speed (km/hr) averaged over 10 minutes prior to 3pm.
#'   \item Humid9am, Relative humidity (percent) at 9am.
#'   \item Humid3pm, Relative humidity (percent) at 3pm.
#'   \item Pressure9am, Atmospheric pressure (hpa) reduced to mean sea level at 9am.
#'   \item Pressure3pm, Atmospheric pressure (hpa) reduced to mean sea level at 3pm.
#'   \item Cloud9am, Fraction of sky obscured by cloud at 9am. This is measured in "oktas", which are a unit of eighths. It records how many eighths of the sky are obscured by cloud. A 0 measure indicates completely clear sky whilst an 8 indicates that it is completely overcast.
#'   \item Cloud3pm, Fraction of sky obscured by cloud (in "oktas": eighths) at 3pm. See Cloud9am for a description of the values.
#'   \item Temp9am, Temperature (degrees C) at 9am.
#'   \item Temp3pm, Temperature (degrees C) at 3pm.
#'   \item RISK_MM, The amount of rain. A kind of measure of the "risk".
#'   \item RainToday, Factor: "yes" if precipitation (mm) in the 24 hours to 9am exceeds 1mm, otherwise 0.
#'   \item RainTomorrow, Factor: "yes" if it rained the following day, the target variable.
#' }
#' @details 
#' Reproducing this dataset:
#' ```
#' require("rattle.data")
#' weather <- weather[, c(1, 3:7, 9, 12:21, 23, 22, 24)]
#' ## save(weather, file = "./data/weather.rda")
#' ```
#' @name weather
#' @docType data
#' @format Data frame of 366 observations of 20 variables. One year of 
#' daily observations of weather variables at Canberra airport in Australia 
#' between November 1, 2007 and October 31, 2008.
#' @source The daily observations are available from http://www.bom.gov.au/climate/data. 
#' Copyright Commonwealth of Australia 2010,
#' Bureau of Meteorology.
#'
#' Definitions adapted from http://www.bom.gov.au/climate/dwo/IDCJDW0000.shtml
#' @references Data source: http://www.bom.gov.au/climate/dwo/ and http://www.bom.gov.au/climate/data.
#' @examples
#' str(weather)
#' \dontrun{
#' dat <- scale_sd(weather[, 2:18])
#' clas_x <- weather$RainToday
#' clas_y <- weather$RainTomorrow
#' bas <- prcomp(dat)$rotation[, 1:2]
#' mvar <- which(abs(bas[, 1]) == max(abs(bas[, 1])))
#' 
#' play_manual_tour(basis = bas, data = dat, manip_var = mvar,
#'                  render_type = render_gganimate, color = clas_y, shape = clas_x)
#' }
"weather"

##### wine -----
#' The wine dataset from the UCI Machine Learning Repository.
#'
#' The wine dataset contains the results of a chemical analysis of wines grown 
#' in a specific area of Italy. Three types of wine are represented in the 178 
#' samples, with the results of 13 chemical analyses recorded for each sample. 
#' The Type variable has been transformed into a categorical variable.
#' 
#' The data contains no missing values and consist of only numeric data, with a 
#' three class target variable (Type) for classification.
#'
#' Data frame of 178 observations of 13 variables, target 
#' class `Type` and 12 numeric variables:
#' \itemize{
#'   \item Type, The type of wine, the target factor, 1 (59 obs), 
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
#'   \item Color, Color intensity
#'   \item Hue, Hue
#'   \item Dilution, D280/OD315 of diluted wines
#'   \item Proline, Proline
#' }
#' @details 
#' Reproducing this dataset:
#' ```
#' requireNamespace("rattle.data")
#' wine
#' ```
#' @name wine
#' @docType data
#' @format Data frame of 178 observations of 13 variables, target 
#' class `Type` and 12 numeric variables.
#' @examples
#' str(wine)
#' \dontrun{
#' dat <- scale_sd(wine[, 2:14])
#' clas <- wine$Type
#' bas <- prcomp(dat)$rotation[, 1:2]
#' mvar <- which(abs(bas[, 1]) == max(abs(bas[, 1])))
#' 
#' play_manual_tour(basis = bas, data = dat, manip_var = mvar,
#'                  render_type = render_gganimate, color = clas, shape = clas)
#' }
"wine"