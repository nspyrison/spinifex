## cran-comments.md

### dontrun{} examples (4x)

The 4 functions that contain `\dontrun{}` produce animations that take >5sec that users need to know about, but cause issues in examples and checks. The manipulations before the animation rending is already tested in the examples of the other functions.


### Test environments
* local Windows 10 x64, R 3.5.3
* linux, osx via Travis CI (oldrel, release, devel)
* win-builder (devel)
* rhub


### R CMD check

-- R CMD check results -------------------------------------------------------- spinifex 0.1.0 ----
Duration: 23.9s

0 errors √ | 0 warnings √ | 0 notes √


### check_rhub()

-- spinifex 0.1.0: NOTE

  Build ID:   spinifex_0.1.0.tar.gz-10124a2285d34216ad4d61ded9f7c238
  Platform:   Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  Submitted:  3m 59.7s ago
  Build time: 3m 55.6s

> checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Nicholas Spyrison <spyrison@gmail.com>'
  
  New submission

0 errors √ | 0 warnings √ | 1 note x


### check_win_devel()

Installation time in seconds: 8
Check time in seconds: 101
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Nicholas Spyrison <spyrison@gmail.com>'

New submission


### Travis CI

- linux
    - release pass
    - devel pass
- osx
    - release pass
    - devel ERROR:
    
Installing R
295.75s$ brew update >/dev/null
==> Downloading https://homebrew.bintray.com/bottles-portable-ruby/portable-ruby-2.3.7.mavericks.bottle.tar.gz
######################################################################## 100.0%
==> Pouring portable-ruby-2.3.7.mavericks.bottle.tar.gz
3.54s$ curl -fLo /tmp/R.pkg https://r.research.att.com/el-capitan/R-devel/R-devel-el-capitan-signed.pkg
curl: (22) The requested URL returned error: 404 Not Found
The command "eval curl -fLo /tmp/R.pkg https://r.research.att.com/el-capitan/R-devel/R-devel-el-capitan-signed.pkg " failed. Retrying, 2 of 3.
...

    
