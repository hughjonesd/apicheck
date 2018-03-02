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
  expect_error(d3 <- load_version_namespace("clipr", "0.3.3"), regexp = NA)
  expect_identical(d1, d2)
  expect_false(identical(d1, d3))

  test <- function (namespace) "OK"
  # already downloaded
  expect_error(x <- call_with_namespace("clipr", "0.4.0", test), regexp = NA)
  expect_identical(x, "OK")
  expect_error(y <- call_with_namespace("clipr", "0.3.2", test), regexp = NA)
  expect_identical(y, "OK")
})


test_that("Can call functions with different calling conventions", {
  skip_on_cran()

  # expect_identical doesn't work for these functions, maybe different
  expect_equal(
          get_fn_at("huxtable::insert_row", version = "3.0.0"),
          get_fn_at("insert_row", "huxtable", version = "3.0.0")
        )
})


test_that("fn_exists_at", {
  skip_on_cran()

  expect_true(fn_exists_at("as_Workbook", "huxtable", "3.0.0"))
  expect_false(fn_exists_at("as_Workbook", "huxtable", "2.0.2"))
})


test_that("api_same_at", {
  skip_on_cran()

  hr3 <- get_fn_at("huxreg", "huxtable", "3.0.0")
  hr202 <- get_fn_at("huxreg", "huxtable", "2.0.2")
  expect_false(api_same_at("huxreg", "huxtable", "2.0.2", current_fn = hr3)) # gained an argument
  qx <- get_fn_at("quick_xlsx", "huxtable", "3.0.0")
  # should warn because insert_row didn't exist back then:
  expect_warning(x <- api_same_at("quick_xlsx", "huxtable", "2.0.2", current_fn = qx))
  expect_false(x)
})


test_that("fn_first_exists and api_first_same", {
  skip_on_cran()
  skip_on_travis()
  skip_if_mran_down()

  expect_equal(fn_first_exists("as_Workbook", "huxtable"), "3.0.0")
  qx <- get_fn_at("quick_xlsx", "huxtable", "3.0.0")
  expect_identical(api_first_same("quick_xlsx", "huxtable", current_fn = qx), "3.0.0") # new function
  expect_identical(api_first_same("huxreg", "huxtable", current_fn = hr3), "3.0.0")    # API change

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
