
context("Whitebox tests for internal functions")

test_that("parse_fn works", {
  expect_identical(apihistory:::parse_fn("foo::bar"), c("foo", "bar"))
})


test_that("clean_versions works", {
  expect_silent(vns <- apihistory:::clean_versions("huxtable"))
  expect_silent(vns2 <- apihistory:::clean_versions("huxtable"))
  expect_identical(vns, vns2)
  expect_identical(names(vns), c("version", "date", "available"))
  expect_true(all(vns$available))
  expect_identical(vns, vns[order(vns$date, decreasing = TRUE), ])
})


test_that("binary_search_versions works", {
  vns <- data.frame(versions = 1:10)
  mytest <- function (x) x >= 5
  expect_equal(apihistory:::binary_search_versions(vns, mytest), 5)
  mytest <- function (x) x >= 0
  expect_equal(apihistory:::binary_search_versions(vns, mytest), 1)
  mytest <- function (x) x >= 20
  expect_null(apihistory:::binary_search_versions(vns, mytest))
})


test_that("cached_install and load_version_namespace work", {
  # expect possible warnings etc. but no errors
  expect_error(d1 <- apihistory:::cached_install("longurl", "0.3.0"), regexp = NA)
  expect_error(d2 <- apihistory:::cached_install("longurl", "0.3.0"), regexp = NA)
  expect_error(d3 <- apihistory:::cached_install("longurl", "0.2.0"), regexp = NA)
  expect_identical(d1, d2)
  expect_false(identical(d1, d3))

  test <- function (namespace) "OK"
  # new download:
  expect_error(apihistory:::load_version_namespace("longurl", "0.1.1", test), regexp = NA)
  # already downloaded:
  expect_error(apihistory:::load_version_namespace("longurl", "0.3.0", test), regexp = NA)
})
