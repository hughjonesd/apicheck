
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/hughjonesd/apihistory.svg?branch=master)](https://travis-ci.org/hughjonesd/apihistory)

apihistory
==========

A small R package to explore the history of a library, testing when functions were introduced or changed API.

Example
-------

From huxtable's NEWS file, version 3.0.0:

-   New quick\_xlsx function.

``` r
library(apihistory)
fn_exists_at("quick_xlsx", "huxtable", "3.0.0")
fn_exists_at("quick_xlsx", "huxtable", "2.0.2")
fn_first_exists("quick_xlsx", "huxtable")
```
