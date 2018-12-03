#' Flea beatle measurements
#'
#' This data is from a paper by A. A. Lubischew, "On the Use of Discriminant
#' Functions in Taxonomy", Biometrics, Dec 1962, pp.455-477.
#'
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
#' @format A 74 x 7 numeric array
#' @keywords datasets
#' @examples
#'
#' head(flea)

#' Wisconsin Breast Cancer Database
#'
#' The objective is to identify each of a number of benign or malignant classes.
#' Samples arrive periodically as Dr. Wolberg reports his clinical cases. The 
#' database therefore reflects this chronological grouping of the data. This 
#' grouping information appears immediately below, having been removed from 
#' the data itself. Each variable except for the first was converted into 11 
#' primitive numerical attributes with values ranging from 0 through 10. There 
#' are 16 missing attribute values. See cited below for more details.
#'
#' A data frame with 699 observations on 11 variables, one being a character 
#' variable, 9 being ordered or nominal, and 1 target class.
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
#'   \item Class, Class
#'
#'
#' @name BreastCancer cells
#' @aliases breastcancer
#' @docType data
#' @format A 675 x 11 data frame, character id, 
#' @keywords datasets
#' @examples
#'
#' head(flea)
#'
NULL

## file.edit("./inst/doc/_data scratchpad.r")
## ?mlbench::BreastCancer
## ?mlbench::Soybean
## ?rattle.data::wine
## ?rattle.data::weatherAUS