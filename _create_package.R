###create the package:
#ns 5/04/2018.


#devtools::create("spacemanip")
devtools::document()
#getwd()
#setwd("C:/Users/nspy0001/Dropbox/_Main/R_dir/spacemanip/spacemanip")
#setwd("C:/Users/nspy0001/Dropbox/_Main/R_dir/spacemanip")
#devtools::use_data(flea)
devtools::use_vignette("introduction")

#library(roxygen2)
#library(devtools)
#sessionInfo()
packageVersion("roxygen2") #‘6.0.1’
packageVersion("devtools") #‘1.13.5’

?create_manip_space # works once the right .rd file has been made.


devtools::load_all() #loads into session like library()

devtools::install() #installs like install.package()
library("spacemanip")
