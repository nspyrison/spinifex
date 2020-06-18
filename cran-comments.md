# cran-comments.md

DOI: 10.2307/1390747 works fine in manual checks.

## examples; (4x) dontrun{}

The 4 functions that contain `\dontrun{}` produce animations that take >5sec that users need to know about, but cause issues in examples and checks. The manipulations before the animation rending is already tested in the examples of the other functions.

## Test environments
- local:R version 4.0.0 (2020-04-24), Windows 10, x86_64 mingw32
- rhub::check(c("debian-gcc-release", "macos-highsierra-release-cran",
                "solaris-x86-patched", "windows-x86_64-release"))
- devtools::check_win_devel()

## local: R CMD check

-- R CMD check results ------------------------------------- spinifex 0.2.0 ----
Duration: 58.7s

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded


## check_rhub()




## devtools::check_win_devel()

- Seems to fail tests that involve print/rendering plotly objects. Specifically when calling base::loadNamespace().

ERRORS:
* checking tests ...
  Running 'testthat.R' [19s]
 ERROR
Running the tests in 'tests/testthat.R' failed.
Last 13 lines of output:
    6. base::loadNamespace(name)
    5. base::getNamespace(ns)
    9. base::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
   10. base::withRestarts(stop(cond), retry_loadNamespace = function() NULL)
   11. base:::withOneRestart(expr, restarts[[1L]])
   12. base:::doWithOneRestart(return(expr), restart)
  
  == testthat results  ===========================================================
  [ OK: 40 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 3 ]
  1. Error: (unknown) (@test-play_manual_tour.R#6) 
  2. Error: (unknown) (@test-play_tour_path.R#6) 
  3. Error: (unknown) (@test-render_plotly.R#8) 
  
  Error: testthat unit tests failed
  Execution halted




