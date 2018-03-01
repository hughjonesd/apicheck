
context("Whitebox tests for internal functions")

test_that("parse_fn works", {
  expect_identical(pastapi:::parse_fn("foo::bar"), c("foo", "bar"))
})


test_that("clean_versions works", {
  expect_silent(vns <- pastapi:::clean_versions("longurl"))
  # check caching:
  expect_silent(vns2 <- pastapi:::clean_versions("longurl"))
  expect_identical(vns, vns2)
  expect_identical(names(vns), c("version", "date", "available"))
  expect_true(all(vns$available))
  expect_identical(vns, vns[order(vns$date, decreasing = TRUE), ])
})


test_that("binary_search_versions works", {
  vns <- data.frame(versions = 1:10)
  mytest <- function (x) x >= 5
  expect_equal(pastapi:::binary_search_versions(vns, mytest), 5)
  mytest <- function (x) x >= 0
  expect_equal(pastapi:::binary_search_versions(vns, mytest), 1)
  mytest <- function (x) x >= 20
  expect_null(pastapi:::binary_search_versions(vns, mytest))
})


test_that("cached_install and load_version_namespace work", {
  skip_on_cran()
  skip_on_travis() # slow

  # expect possible warnings etc. but no errors
  expect_error(d1 <- pastapi:::cached_install("longurl", "0.3.0"), regexp = NA)
  expect_error(d2 <- pastapi:::cached_install("longurl", "0.3.0"), regexp = NA)
  expect_error(d3 <- pastapi:::cached_install("longurl", "0.2.0"), regexp = NA)
  expect_identical(d1, d2)
  expect_false(identical(d1, d3))

  test <- function (namespace) "OK"
  # new download:
  expect_error(pastapi:::load_version_namespace("longurl", "0.1.1", test), regexp = NA)
  # already downloaded:
  expect_error(pastapi:::load_version_namespace("longurl", "0.3.0", test), regexp = NA)
})
