##### BreastCancer -----
#' Wisconsin Breast Cancer Database
#'
#' The objective is to identify each of a number of benign or malignant classes.
#' Samples arrive periodically as Dr. Wolberg reports his clinical cases. The 
#' database therefore reflects this chronological grouping of the data. This 
#' grouping information appears immediately below, having been removed from 
#' the data itself. Each variable except for the first was converted into 11 
#' primitive numerical attributes with values ranging from 0 through 10. Rows
#' with missing attribute values and duplicate rows removed.
#'
#' This is a cleaned subset of `mlbench`'s `BreastCancer`.
#' See `help(BreastCancer, package = "mlbench")` for the original.
#'
#' @format A data frame with 675 observations of 8 numeric variables and 
#' target factor `Class`.
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
#' Replicating this dataset:
#' ```
#' require("mlbench")
#' data(BreastCancer)
#' 
#' raw <- BreastCancer
#' ## rownumber index of 8 duplicate 16 incomplete rows
#' idx <- !duplicated(raw) & complete.cases(raw) 
#' d <- raw[idx, 3:10]
#' d <- apply(d, 2L, as.integer)
#' d <- data.frame(d, Class = as.factor(raw$Class[idx]))
#' BreastCancer <- d
#' ## save(BreastCancer, file = "./data/BreastCancer.rda")
#' ```
#' @name BreastCancer
#' @docType data
#' @source {J.W. Smith., el al. 1988. Using the ADAP learning algorithm to forecast the onset of diabetes mellitus. In Proceedings of the Symposium on Computer Applications and Medical Care (pp. 261--265). IEEE Computer Society Press.}
#' @source {mlbench, R package. F. Leisch & E. Dimitriadou, 2021. mlbench: Machine Learning Benchmark Problems} \url{https://CRAN.R-project.org/package=mlbench}
#' @examples
#' library("spinifex")
#' str(spinifex::BreastCancer)
#' dat <- scale_sd(spinifex::BreastCancer[, 1:8])
#' clas <- spinifex::BreastCancer$Class
#' 
#' bas <- basis_pca(dat)
#' mv <- manip_var_of(bas)
#' mt <- manual_tour(bas, mv)
#' 
#' ggt <- ggtour(mt, dat, angle = .2) +
#'   proto_default(list(color = clas, shape = clas))
#' \dontrun{
#' animate_plotly(ggt)}
"BreastCancer"

##### PimaIndiansDiabetes_wide -----
#' Pima Indians Diabetes Dataset, wide
#'
#' The data set PimaIndiansDiabetes2 contains a corrected version of the 
#' original data set. While the UCI repository index claims that there are no 
#' missing values, closer inspection of the data shows several physical 
#' impossibilities, e.g., blood pressure or body mass index of 0. In 
#' PimaIndiansDiabetes2, all zero values of glucose, pressure, triceps, insulin 
#' and mass have been set to NA, see also Wahba et al (1995) and Ripley (1996).
#'
#' This is a cleaned subset of `mlbench`'s `PimaIndiansDiabetes2`.
#' See `help(PimaIndiansDiabetes2, package = "mlbench")`.
#'
#' @format A data frame with 392 observations of 
#' 8 numeric variables, and target factor `diabetes`.
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
#' Replicating this dataset:
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

#' @examples
#' library("spinifex")
#' str(PimaIndiansDiabetes_wide)
#' dat <- scale_sd(PimaIndiansDiabetes_wide[, 1:8])
#' clas <- PimaIndiansDiabetes_wide$diabetes
#' 
#' bas <- basis_pca(dat)
#' mv <- manip_var_of(bas)
#' mt <- manual_tour(bas, mv)
#' 
#' ggt <- ggtour(mt, dat, angle = .2) +
#'   proto_default(list(color = clas, shape = clas))
#' \dontrun{
#' animate_plotly(ggt)}
"PimaIndiansDiabetes_wide"

##### PimaIndiansDiabetes_long -----
#' Pima Indians Diabetes Dataset, long
#' 
#' The data set PimaIndiansDiabetes2 contains a corrected version of the 
#' original data set. While the UCI repository index claims that there are no 
#' missing values, closer inspection of the data shows several physical 
#' impossibilities, e.g., blood pressure or body mass index of 0. In 
#' PimaIndiansDiabetes2, all zero values of glucose, pressure, triceps, insulin 
#' and mass have been set to NA, see also Wahba et al (1995) and Ripley (1996).
#'
#' This is a cleaned subset of `mlbench's` `PimaIndiansDiabetes2`. 
#' See `help(PimaIndiansDiabetes2, package = "mlbench")`.
#'
#' @format A data frame with 724 observations of
#' 6 numeric variables, and target factor `diabetes`.
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
#' Replicating this dataset:
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
#' @source {J.W. Smith., el al. 1988. Using the ADAP learning algorithm to forecast the onset of diabetes mellitus. In Proceedings of the Symposium on Computer Applications and Medical Care (pp. 261--265). IEEE Computer Society Press.}
#' @source {mlbench, R package. F. Leisch & E. Dimitriadou, 2021. mlbench: Machine Learning Benchmark Problems} \url{https://CRAN.R-project.org/package=mlbench}
#' @examples
#' library("spinifex")
#' str(PimaIndiansDiabetes_long)
#' dat <- scale_sd(PimaIndiansDiabetes_long[, 1:6])
#' clas <- PimaIndiansDiabetes_long$diabetes
#' 
#' bas <- basis_pca(dat)
#' mv <- manip_var_of(bas)
#' mt <- manual_tour(bas, mv)
#' 
#' ggt <- ggtour(mt, dat, angle = .2) +
#'   proto_default(list(color = clas, shape = clas))
#' \dontrun{
#' animate_plotly(ggt)}
"PimaIndiansDiabetes_long"

##### weather -----
#' Sample dataset of daily weather observations from Canberra airport in Australia.
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
#' This is a cleaned subset of `rattle::weather`.
#'
#' @format A data frame of 354 observations of 20 variables. One year of 
#' daily observations of weather variables at Canberra airport in Australia 
#' between November 1, 2007 and October 31, 2008.
#' \itemize{
#'   \item Date, The date of observation (Date class).
#'   \item MinTemp, The minimum temperature in degrees Celsius.
#'   \item MaxTemp, The maximum temperature in degrees Celsius.
#'   \item Rainfall, The amount of rainfall recorded for the day in mm.
#'   \item Evaporation, The "Class A pan evaporation" (mm) in the 24 hours to 9am.
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
#' Copyright Commonwealth of Australia 2010,
#' Bureau of Meteorology.
#' Definitions adapted from http://www.bom.gov.au/climate/dwo/IDCJDW0000.shtml
#' @details 
#' Replicating this dataset:
#' ```
#' require("rattle")
#' d <- rattle::weather[, c(1, 3:7, 9, 12:21, 23, 22, 24)]
#' d <- d[complete.cases(d), ] ## Remove ~12 row-wise incomplete rows
#' d <- as.data.frame(d)  ## Remove tibble dependency
#' weather <- d
#' ## save(weather, file = "./data/weather.rda")
#' ```
#' @name weather
#' @docType data
#' @source {Bureau of Meteorology, Commonwealth of Australia} \url{http://www.bom.gov.au/climate/data/}
#' @source {rattle, R package. G. Williams, 2020. rattle: Graphical User Interface for Data Science in R} \url{https://CRAN.R-project.org/package=rattle}
#' @examples
#' library("spinifex")
#' str(spinifex::weather)
#' dat <- scale_sd(spinifex::weather[, 2:18])
#' clas <- spinifex::weather$RainTomorrow
#' 
#' bas <- basis_pca(dat)
#' mv <- manip_var_of(bas)
#' mt <- manual_tour(bas, mv)
#' 
#' ggt <- ggtour(mt, dat, angle = .2) +
#'   proto_default(list(color = clas, shape = clas))
#' \dontrun{
#' animate_plotly(ggt)}
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
#' @format A data frame of 178 observations of target 
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
#' @source {rattle, R package. G. Williams, 2020. rattle: Graphical User Interface for Data Science in R} \url{https://CRAN.R-project.org/package=rattle}
#' @source {PARVUS. M. Forina. et al. 1988. Elsevier, Amsterdam, PARVUS: An extendable package of programs for data exploration, classification and correlation. ISBN 0‐444‐430121z}
#' @details
#' Replicating this dataset:
#' ```
#' require("rattle")
#' str(rattle::wine)
#' ## save(wine, file = "./data/wine.rda")
#' ```
#' @name wine
#' @docType data
#' @examples
#' library("spinifex")
#' str(wine)
#' dat <- scale_sd(wine[, 2:6])
#' clas <- wine$Type
#' 
#' bas <- basis_pca(dat)
#' mv <- manip_var_of(bas)
#' mt <- manual_tour(bas, mv)
#' 
#' ggt <- ggtour(mt, dat, angle = .2) +
#'   proto_default(list(color = clas, shape = clas))
#' \dontrun{
#' animate_plotly(ggt)}
"wine"

##### penguins -----
#' Size measurements for adult foraging penguins near Palmer Station, Antarctica
#' 
#' Includes measurements for penguin species, island in Palmer Archipelago,
#' size (flipper length, body mass, bill dimensions), and sex.
#' 
#' This is a cleaned subset of `palmerpenguins::penguins`.
#'
#' @format A data frame with 333 rows and 4 numeric variables and 3 factor variables
#' \describe{
#'   \item{bill_length_mm}{a number denoting bill length (millimeters)}
#'   \item{bill_depth_mm}{a number denoting bill depth (millimeters)}
#'   \item{flipper_length_mm}{an integer denoting flipper length (millimeters)}
#'   \item{body_mass_g}{an integer denoting body mass (grams)}
#'   \item{species}{a factor denoting penguin species (Adelie, Chinstrap and Gentoo)}
#'   \item{sex}{a factor denoting penguin sex (female, male)}
#'   \item{island}{a factor denoting island in Palmer Archipelago, Antarctica (Biscoe, Dream or Torgersen)}
#' }
#' @source {palmerpenguins R package. A. Horst, 2020. Palmer Archipelago (Antarctica) Penguin Data.} \url{https://CRAN.R-project.org/package=palmerpenguins}
#' @source {Adelie penguins: Palmer Station Antarctica LTER and K. Gorman. 2020. Structural size measurements and isotopic signatures of foraging among adult male and female Adelie penguins (Pygoscelis adeliae) nesting along the Palmer Archipelago near Palmer Station, 2007-2009 ver 5. Environmental Data Initiative} \doi{10.6073/pasta/98b16d7d563f265cb52372c8ca99e60f}
#' @source {Gentoo penguins: Palmer Station Antarctica LTER and K. Gorman. 2020. Structural size measurements and isotopic signatures of foraging among adult male and female Gentoo penguin (Pygoscelis papua) nesting along the Palmer Archipelago near Palmer Station, 2007-2009 ver 5. Environmental Data Initiative} \doi{10.6073/pasta/7fca67fb28d56ee2ffa3d9370ebda689}
#' @source {Chinstrap penguins: Palmer Station Antarctica LTER and K. Gorman. 2020. Structural size measurements and isotopic signatures of foraging among adult male and female Chinstrap penguin (Pygoscelis antarcticus) nesting along the Palmer Archipelago near Palmer Station, 2007-2009 ver 6. Environmental Data Initiative} \doi{10.6073/pasta/c14dfcfada8ea13a17536e73eb6fbe9e}
#' @source {Originally published in: Gorman KB, Williams TD, Fraser WR (2014) Ecological Sexual Dimorphism and Environmental Variability within a Community of Antarctic Penguins (Genus Pygoscelis). PLoS ONE 9(3): e90081. doi:10.1371/journal.pone.0090081}
#' @details 
#' Replicating this dataset:
#' ```
#' require("palmerpenguins")
#' d <- palmerpenguins::penguins
#' d <- d[!is.na(d$sex), ] ## Remove missing
#' d <- d[, c(3:6, 1, 7, 2)] ## Numeric to front, group factors, remove year
#' penguins <- as.data.frame(d) ## Remove {tibble} dependency
#' ## save(penguins, file = "./data/penguins.rda")
#' ```
#' @examples
#' library("spinifex")
#' str(spinifex::penguins)
#' dat <- scale_sd(spinifex::penguins[, 1:4])
#' clas1 <- spinifex::penguins$species
#' clas2 <- spinifex::penguins$sex
#' 
#' bas <- basis_pca(dat)
#' mv <- manip_var_of(bas)
#' mt <- manual_tour(bas, mv)
#' 
#' ggt <- ggtour(mt, dat, angle = .2) +
#'   proto_default(list(color = clas1, shape = clas2))
#' \dontrun{
#' animate_plotly(ggt)}
"penguins"
