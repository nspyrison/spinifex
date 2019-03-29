## cran-comments.md

## Test environments
* local Windows 10 x64, R 3.5.3
* linux, osx (on travis-ci),
* win-builder (release)

## R CMD check

-- R CMD check results -------------------------------- spinifex 0.0.0.9000 ----
Duration: 1m 50.3s

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded

## Travis CI

linux oldrel: error
linux release: succeeded
linux oldrel: error
osx oldrel: error
osx release: succeeded
osx oldrel: error

## check_win_release()

Status: 3 NOTEs

The Title field should be in title case, current version then in title case:
'Manual tours, manual control of dynamic projections of numeric multivariate data'
'Manual Tours, Manual Control of Dynamic Projections of Numeric Multivariate Data'

** running examples for arch 'i386' ... [100s] NOTE
Examples with CPU or elapsed time > 10s
                  user system elapsed
render_gganimate 71.03   3.26   62.63
render_plotly     9.51   0.50   10.22
** running examples for arch 'x64' ... [96s] NOTE
Examples with CPU or elapsed time > 10s
                  user system elapsed
render_gganimate 65.47   3.11   59.71

