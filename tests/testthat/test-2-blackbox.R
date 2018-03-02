context("Blackbox tests")

old <- NULL

setup({
  old <<- getOption("pastapi.lib_dir") # don't use get_lib_dir as it never returns NULL
  set_lib_dir("testing_lib_dir")
})

teardown({
  set_lib_dir(old)
})



test_that("load_version_namespace and call_with_namespace", {
  skip_on_cran()
  skip_on_travis() # slow
  # can't skip_if_mran_down() because it uses this very function, so instead:

  # expect possible warnings etc. but no errors
  # res is NULL if there is an error
  res <- expect_error(d1 <- load_version_namespace("clipr", "0.4.0"), regexp = NA)
  if (is.null(res)) skip("load_version_namespace failed, no point trying the others")
  expect_error(d2 <- load_version_namespace("clipr", "0.4.0"), regexp = NA)
  expect_error(d3 <- load_version_namespace("clipr", "0.2.0"), regexp = NA)
  expect_equal(d1, d2) # but not identical for some reason
  expect_false(isTrue(all.equal(d1, d3)))
  unloadNamespace("clipr")

  test <- function (namespace) {
    stopifnot(is.environment(namespace))
    stop("Ran test ok")
  }
  # already downloaded
  expect_error(call_with_namespace("clipr", "0.4.0", test), regexp = "Ran test ok")
  expect_false(isNamespaceLoaded("clipr"))
  # not downloaded
  expect_error(call_with_namespace("clipr", "0.3.2", test), regexp = "Ran test ok")
  expect_false(isNamespaceLoaded("clipr"))
})


test_that("Can call functions with different calling conventions", {
  skip_on_cran()

  # expect_identical doesn't work for functions
  expect_equal(
          get_fn_at("clipr::write_clip", version = "0.4.0"),
          get_fn_at("write_clip", "clipr", version = "0.4.0")
        )
})


test_that("fn_exists_at", {
  skip_on_cran()

  expect_true(fn_exists_at("clipr::dr_clipr", version = "0.4.0"))
  expect_false(fn_exists_at("clipr::dr_clipr", version = "0.2.0"))
})


test_that("api_same_at", {
  skip_on_cran()

  wc4 <- get_fn_at("clipr::write_clip", version = "0.4.0")
  wc011 <- get_fn_at("clipr::write_clip", version = "0.1.1")
  expect_false(api_same_at("clipr::write_clip", version = "0.1.1", current_fn = wc4)) # gained an argument
  dr_c <- get_fn_at("clipr::dr_clipr", version = "0.4.0")
  # should warn because dr_clipr didn't exist back then:
  expect_warning(x <- api_same_at("clipr::dr_clipr",  version = "0.1.1", current_fn = dr_c))
  expect_false(x)
})


test_that("fn_first_exists and api_first_same", {
  skip_on_cran()
  skip_on_travis()
  skip_if_mran_down()

  expect_equal(fn_first_exists("clipr::dr_clipr"), "0.4.0")
  dr_c <- get_fn_at("clipr::dr_clipr", version = "0.4.0")
  wc   <- get_fn_at("clipr::write_clip", version = "0.4.0")
  expect_identical(api_first_same("clipr::dr_clipr", current_fn = dr_c), "0.4.0") # new function
  expect_identical(api_first_same("clipr::write_clip", current_fn = wc), "0.2.0")    # API change
})



test_that("Can set lib_dir", {
  skip_on_cran()
  skip_on_travis()

  tempdir <- tempfile(pattern = "testing", tmpdir = normalizePath(tempdir()))
  dir.create(tempdir)
  set_lib_dir(tempdir)
  prepare <- try(load_version_namespace("clipr", "0.4.0"))
  if (class(prepare) == "try-error") skip("Couldn't download package for testing")
  expect_true(dir.exists(file.path(tempdir, "clipr-0.4.0", "clipr")))
})
