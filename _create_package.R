###create the package:
#ns 5/04/2018.

library(tourr)
#devtools::create("spacemanip")
devtools::document()
#getwd()
#setwd("C:/Users/nspy0001/Dropbox/_Main/R_dir/spacemanip/spacemanip")
#setwd("C:/Users/nspy0001/Dropbox/_Main/R_dir/spacemanip")
devtools::use_data(flea)
devtools::use_vignette("introduction")

#library(roxygen2)
#library(devtools)
#sessionInfo()
packageVersion("roxygen2") #‘6.0.1’
packageVersion("devtools") #‘1.13.5’

?create_manip_space # works once the right .rd file has been made.


devtools::document() #Invalid DESCRIPTION: Malformed maintainer field.
options(devtools.desc.author="'First Last <first.last@example.com> [aut, cre]'") # didn't fix.
