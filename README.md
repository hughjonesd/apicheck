
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/hughjonesd/pastapi.svg?branch=master)](https://travis-ci.org/hughjonesd/pastapi) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/hughjonesd/pastapi?branch=master&svg=true)](https://ci.appveyor.com/project/hughjonesd/pastapi) [![Coverage status](https://codecov.io/gh/hughjonesd/pastapi/branch/master/graph/badge.svg)](https://codecov.io/github/hughjonesd/pastapi?branch=master)

pastapi
=======

`pastapi`, pronounced "Pasta Pi", is a small R package to explore the historical API of functions in CRAN packages. It is designed to help you work out minimum version requirements for packages mentioned in your DESCRIPTION file.

`pastapi` works by downloading package versions from MRAN and temporarily installing them in a special directory. Results take time.

Example
=======

From [clipr](https://github.com/mdlincoln/clipr/)'s NEWS file:

clipr 0.4.0:
------------

### Introduces `dr_clipr()` ...

``` r
library(pastapi)
fn_exists_at("clipr::dr_clipr", version =  "0.4.0")
#> [1] TRUE
fn_exists_at("clipr::dr_clipr", version =  "0.3.3")
#> [1] FALSE
when_fn_exists("clipr::dr_clipr", report = "brief") # binary search
#> [1] "0.4.0"
```

clipr 0.2.0:
------------

### Several changes to `write_clip` ...

``` r

api_same_at("clipr::write_clip", version = "0.2.0")
#> [1] TRUE
api_same_at("clipr::write_clip", version = "0.1.1")
#> [1] FALSE
when_api_same("clipr::write_clip", report = "full", search = "all") # check all versions
#>   version       date available          result
#> 9   0.1.0 2015-09-02      TRUE Known different
#> 8   0.1.1 2015-09-03      TRUE Known different
#> 7   0.2.0 2015-10-06      TRUE      Known same
#> 6   0.2.1 2016-06-23      TRUE      Known same
#> 5   0.3.0 2016-11-19      TRUE      Known same
#> 4   0.3.1 2016-12-02      TRUE      Known same
#> 3   0.3.2 2017-01-09      TRUE      Known same
#> 2   0.3.3 2017-06-19      TRUE      Known same
#> 1   0.4.0 2017-11-03      TRUE      Known same
```
