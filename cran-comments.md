## Test environments

- local:
    R version 4.1.2 (2021-11-01) -- "Bird Hippie"
    Platform: x86_64-w64-mingw32/x64 (64-bit)
    Running under: Windows 10 x64 (build 19042)
- rhub::check:
    "debian-clang-devel"
    "macos-highsierra-release-cran"
    "macos-m1-bigsur-release"
    "ubuntu-rchk"
    "windows-x86_64-devel"
    "windows-x86_64-oldrel"

## R CMD check results

### Local:

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded

### Virtual:

- Some flavors exceed 5 seconds for: ggtour and proto_highlight.
-- they take ~ 2.5 sec locally when ignoring don't runs.


* This is a new release
