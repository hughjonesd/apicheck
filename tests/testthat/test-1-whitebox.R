
context("Whitebox tests")

old <- NULL

setup({
  old <<- getOption("pastapi.lib_dir") # don't use get_lib_dir as it never returns NULL
  # we want a new directory for these tests, otherwise last one will delete your pre-cached files
  set_lib_dir(NULL)
})


teardown({
  set_lib_dir(old)
})


test_that("parse_fn works", {
  expect_identical(pastapi:::parse_fn("foo::bar"), c("foo", "bar"))
})


test_that("Failure to install a file does not leave a directory on disk", {
  tempdir <- tempfile(pattern = "testing", tmpdir = normalizePath(tempdir()))
  dir.create(tempdir)
  set_lib_dir(tempdir)
  with_mock(`versions::install.versions` = function (...) stop("Some crazy failure"), {
    expect_error(get_fn_at("expand_urls", "longurl", "0.3.0"), "Some crazy failure")
    expect_false(dir.exists(file.path(tempdir, "longurl")))
  })
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


test_that("mran_versions works", {
  expect_silent(vns <- pastapi:::mran_versions("longurl"))
  # check caching:
  expect_silent(vns2 <- pastapi:::mran_versions("longurl"))
  expect_identical(vns, vns2)
  expect_identical(names(vns), c("version", "date", "available"))
  expect_true(all(vns$available))
  expect_identical(vns, vns[order(vns$date, decreasing = TRUE), ])
})


test_that("cached_install and call_with_namespace work", {
  skip_on_cran()
  skip_on_travis() # slow
  # can't skip_if_mran_down() because it uses this very function, so instead:

  # expect possible warnings etc. but no errors
  # res is NULL if there is an error
  res <- expect_error(d1 <- pastapi:::cached_install("clipr", "0.4.0"), regexp = NA)
  if (is.null(res)) skip("cached_install failed, no point trying the others")
  expect_error(d2 <- pastapi:::cached_install("clipr", "0.4.0"), regexp = NA)
  expect_error(d3 <- pastapi:::cached_install("clipr", "0.3.3"), regexp = NA)
  expect_identical(d1, d2)
  expect_false(identical(d1, d3))

  test <- function (namespace) "OK"
  # already downloaded
  expect_error(x <- call_with_namespace("clipr", "0.3.3", test), regexp = NA)
  expect_identical(x, "OK")
  expect_error(y <- call_with_namespace("clipr", "0.3.2", test), regexp = NA)
  expect_identical(y, "OK")
})



test_that("clear_package_cache works", {
  ld <- get_lib_dir()
  cat("blah", file = file.path(ld, "notarealpackage-0.2.0"))
  clear_package_cache()
  expect_length(list.files(ld, all.files = TRUE), 0)
})
