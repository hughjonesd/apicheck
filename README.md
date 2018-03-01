
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/hughjonesd/pastapi.svg?branch=master)](https://travis-ci.org/hughjonesd/pastapi)

pastapi
=======

`pastapi`, pronounced "Pasta Pi", is a small R package to explore the past API of functions in CRAN packages.

Example
-------

From huxtable's NEWS file, version 3.0.0:

-   New quick\_xlsx function.

``` r
library(pastapi)
fn_exists_at("quick_xlsx", "huxtable", "3.0.0")
fn_exists_at("quick_xlsx", "huxtable", "2.0.2")
fn_first_exists("quick_xlsx", "huxtable")
```
