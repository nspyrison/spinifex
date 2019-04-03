## cran-comments.md

## Test environments
* local Windows 10 x64, R 3.5.3
* linux, osx (on travis-ci),
* win-builder (release)


## R CMD check

-- R CMD check results -------------------------------- spinifex 0.1.0 ----
Duration: 3m 34s

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded


## Travis CI

linux oldrel: error
linux release: succeeded
linux devel: error
osx oldrel: error
osx release: succeeded
osx devel: error


## check_win_release()

Installation time in seconds: 4
Check time in seconds: 352
Status: 3 NOTEs
R version 3.5.3 (2019-03-11)

The Title field should be in title case, current version then in title case:
'Manual tours, manual control of dynamic projections of numeric multivariate data'
'Manual Tours, Manual Control of Dynamic Projections of Numeric Multivariate Data'

** running examples for arch 'i386' ... [116s] NOTE
Examples with CPU or elapsed time > 10s
                  user system elapsed
render_gganimate 83.71   3.42   73.58
render_plotly     9.04   0.56   10.55
** running examples for arch 'x64' ... [104s] NOTE
Examples with CPU or elapsed time > 10s
                  user system elapsed
render_gganimate 64.63   3.84   60.20
render_plotly    10.11   0.42   11.13
breastcancer      9.23   0.35   10.09

