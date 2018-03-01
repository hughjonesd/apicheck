
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/hughjonesd/pastapi.svg?branch=master)](https://travis-ci.org/hughjonesd/pastapi) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/hughjonesd/pastapi?branch=master&svg=true)](https://ci.appveyor.com/project/hughjonesd/pastapi) [![Coverage status](https://codecov.io/gh/hughjonesd/pastapi/branch/master/graph/badge.svg)](https://codecov.io/github/hughjonesd/pastapi?branch=master)

pastapi
=======

`pastapi`, pronounced "Pasta Pi", is a small R package to explore the past API of functions in CRAN packages. It is designed to help you work out minimum version requirements for packages mentioned in your DESCRIPTION file.

`pastapi` works by downloading package versions from MRAN and temporarily installing them. So, results can be slow!

Example
-------

From [huxtable](https://github.com/hughjonesd/huxtable)'s NEWS file, version 3.0.0:

-   New `quick_xlsx` function.

``` r
library(pastapi)
fn_exists_at("quick_xlsx", "huxtable", "3.0.0")
#> [1] TRUE
fn_exists_at("quick_xlsx", "huxtable", "2.0.2")
#> [1] FALSE
fn_first_exists("quick_xlsx", "huxtable")
#> [1] "3.0.0"
```

From the same place:

-   New `tidy_args` argument to huxreg allows per-model customization of the call to `tidy`.

``` r

api_same_at("huxreg", "huxtable", "3.0.0")
#> [1] TRUE
api_same_at("huxreg", "huxtable", "2.0.2")
#> [1] FALSE
api_first_same("quick_xlsx", "huxtable")
#> [1] "3.0.0"
```
