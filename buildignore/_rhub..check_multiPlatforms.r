## Following along with:
# browseURL("https://r-hub.github.io/rhub/articles/rhub.html")
library("rhub")

validate_email(email = NULL, token = NULL)
## Good, my personal email @gmail.com is validated.

z <- as.data.frame(platforms())
dim(z)

library(dplyr) 
zz <- dplyr::filter(z, 
                    !is.na(`cran-name`) & 
                      rversion == "r-release" &
                      compilers != "Oracle Developer Studio 12.6")
zz[, 1:5] ## Good platforms to check
sel_platforms <- zz$name

## added the following to __packages.r // ns99

nsCheck <- function(platforms_to_check = 
                      c("debian-gcc-release", "macos-highsierra-release-cran",
                        "solaris-x86-patched", "windows-x86_64-release"),
                    validated_email = "list_validated_emails()[1]",
                    ...) { ## Additional arguments to pass to rhub::check()
  require("beepr"); require("tictoc"); require("rhub");
  if (validated_email == "list_validated_emails()[1]")
    validated_email <- as.character(rhub::list_validated_emails()[1])
  tictoc::tic("Starting rhub::check()")
  rhub::check(platform = platforms_to_check, email = validated_email, ...)
  tictoc::toc()
  beepr::beep(4)
}

## Previous checks
nsChecks_prev <- function(validated_email = "list_validated_emails()[1]", 
                          last_n = 3){
  require("beepr"); require("tictoc"); require("rhub");
  if (validated_email == "list_validated_emails()[1]")
    validated_email <- rhub::list_validated_emails()[1]
  tictoc::tic("Starting rhub::check()")
  rhub::list_package_checks(email = validated_email, howmany = last_n)
  tictoc::toc()
  beepr::beep(4)
}

####

devtools::release()