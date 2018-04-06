
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/hughjonesd/apicheck.svg?branch=master)](https://travis-ci.org/hughjonesd/apicheck) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/hughjonesd/apicheck?branch=master&svg=true)](https://ci.appveyor.com/project/hughjonesd/apicheck) [![Coverage status](https://codecov.io/gh/hughjonesd/apicheck/branch/master/graph/badge.svg)](https://codecov.io/github/hughjonesd/apicheck?branch=master)

apicheck
========

`apicheck` is a small R package to explore the historical API of functions in CRAN packages. It is designed to help you work out minimum version requirements for packages mentioned in your DESCRIPTION file.

`apicheck` works by downloading package versions from CRAN and temporarily installing them in a special directory.

Installation
============

``` r
install.packages("remotes") 
# You'll need these:
remotes::install_github("hughjonesd/rcheology")
remotes::install_github("r-lib/pkgapi")

remotes::install_github("hughjonesd/apicheck")
```

Example
=======

From [clipr](https://github.com/mdlincoln/clipr/)'s NEWS file:

`clipr 0.4.0` introduces `dr_clipr()`:

``` r
library(apicheck)
fun_exists_at("clipr::dr_clipr", "0.4.0")
#> [1] TRUE
fun_exists_at("clipr::dr_clipr", "0.3.3")
#> [1] FALSE
when_fun_exists("clipr::dr_clipr", report = "brief") # binary search
#> [1] "0.4.0"
```

`clipr 0.2.0` : several changes to `write_clip`.

``` r

api_same_at("clipr::write_clip", "0.2.0")
#> [1] TRUE
api_same_at("clipr::write_clip", "0.1.1")
#> [1] FALSE
when_api_same("clipr::write_clip", report = "full", search = "all") # check all versions
#>   version       date          result
#> 1   0.1.0 2015-09-02 Known different
#> 2   0.1.1 2015-09-04 Known different
#> 3   0.2.0 2015-10-06      Known same
#> 4   0.2.1 2016-06-23      Known same
#> 5   0.3.0 2016-11-19      Known same
#> 6   0.3.1 2016-12-02      Known same
#> 7   0.3.2 2017-01-09      Known same
#> 8   0.3.3 2017-06-19      Known same
#> 9   0.4.0 2017-11-03      Known same
```

To dig more deeply:

``` r
args(fun_at("clipr::write_clip", "0.1.1"))
#> function (content, sep = NULL, eos = NULL) 
#> NULL
args(fun_at("clipr::write_clip", "0.2.0"))
#> function (content, object_type = c("auto", "character", "table"), 
#>     breaks = NULL, eos = NULL, return_new = TRUE, ...) 
#> NULL
help_at("clipr::write_clip", "0.1.1") 
```

To see all changes between two versions, use `compare_versions`:

``` r
cv <- compare_versions("clipr", "0.1.1", "0.2.0")
summary(cv)
#> Version 1: 0.1.1 
#> Version 2: 0.2.0 
#> # A tibble: 2 x 5
#>   function_1 function_2 change      api_1                           api_2 
#>   <chr>      <chr>      <chr>       <chr>                           <chr> 
#> 1 <NA>       clear_clip Added       ""                              ""    
#> 2 write_clip write_clip API changed content, sep = NULL, eos = NULL "cont…
```

You can also check base packages, using the info from [rcheology](../rcheology):

``` r
fun_exists_at("base::paste0", "2.14.0")
#> [1] FALSE
fun_exists_at("base::paste0", "2.15.0")
#> [1] TRUE
```

Performance
===========

[Performance tests](https://hughjonesd.github.io/apicheck/performance2.html).
