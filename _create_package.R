###create the package:
#ns 5/04/2018.

library(tourr)
#devtools::create("spacemanip")
devtools::document()

devtools::use_data(flea)
devtools::use_vignette("introduction")
??naniar #Vignettes; getting-started-w-naniar.

library(GGally)
devtools::use_data(nasa) #from GGally
?GGally::glyphs #see example


#library(roxygen2)
#library(devtools)
#sessionInfo()
packageVersion("roxygen2") #‘6.0.1’
packageVersion("devtools") #‘1.13.5’

?create_manip_space # works once the right .rd file has been made.


devtools::document() #Invalid DESCRIPTION: Malformed maintainer field.
options(devtools.desc.author="'First Last <first.last@example.com> [aut, cre]'") # didn't fix.



###create the testThat:
#ns 7/04/2018
#http://r-pkgs.had.co.nz/tests.html

#library(devtools)
#devtools::install_github("nspyrison/spacemanip")
library(spacemanip)

devtools::use_testthat() #creates /test/ and test/testthat/

devtools::test() #runs the tests in test/testthat/"test%" and returns the results.

stop()
stop()
cat("see example file; ~/tests/testthat/test_util.R")
file.edit("./tests/testthat/test_util.R")

#test file names must start with test and reside in:
#tests/testthat/
