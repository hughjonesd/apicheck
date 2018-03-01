context("Blackbox tests of API")

old <- NULL
setup(old <- options("pastapi.lib_dir"))
teardown(if (! is.null(old)) options(pastapi.lib_dir = old))

test_that("Can call functions with or without package", {
  # expect_identical doesn't work for these functions, maybe different
  expect_equal(
          get_fn_at("as_Workbook", "huxtable", "3.0.0"),
          get_fn_at("huxtable::as_Workbook", version = "3.0.0")
        )
})


test_that("fn_exists_at works for functions, generics and S3 methods", {
  expect_true(fn_exists_at("as_Workbook", "huxtable", "3.0.0"))
  expect_false(fn_exists_at("as_Workbook", "huxtable", "2.0.2"))
  expect_true(fn_exists_at("as_Workbook.huxtable", "huxtable", "3.0.0"))
  expect_false(fn_exists_at("as_Workbook.huxtable", "huxtable", "2.0.2"))
  expect_true(fn_exists_at("insert_row", "huxtable", "0.3.1"))
  expect_false(fn_exists_at("insert_row", "huxtable", "0.3.0"))
})


test_that("fn_first_exists works for functions, generics and S3 methods", {
  expect_equal(fn_first_exists("as_Workbook", "huxtable"), "3.0.0")
  expect_equal(fn_first_exists("as_Workbook.huxtable", "huxtable"), "3.0.0")
  expect_equal(fn_first_exists("insert_row", "huxtable"), "0.3.1")
})


test_that("api_same_at works for functions, generics and S3 methods", {
  f <- get_fn_at("insert_row", "huxtable", "3.0.0")
  expect_true(api_same_at("insert_row", "huxtable", "2.0.2", current_fn = f))
  # should warn because insert_row didn't exist back then:
  expect_warning(x <- api_same_at("insert_row", "huxtable", "0.3.0", current_fn = f))
  expect_false(x)
  to_s <- get_fn_at("to_screen.huxtable", "huxtable", "3.0.0")
  expect_true(api_same_at("to_screen.huxtable", "huxtable", "1.0.0", current_fn = to_s))
  expect_false(api_same_at("to_screen.huxtable", "huxtable", "0.2.0", current_fn = to_s))
  to_s <- get_fn_at("to_screen", "huxtable", "3.0.0")
  expect_true(api_same_at("to_screen", "huxtable", "1.0.0", current_fn = to_s))
  # hard to find a case when a generic changes its API
})


test_that("api_first_same works for functions, generics and S3 methods", {
  f <- get_fn_at("insert_row", "huxtable", "3.0.0")
  expect_identical(api_first_same("insert_row", "huxtable", current_fn = f), "0.3.1")
  hr <- get_fn_at("huxreg", "huxtable", "2.0.2")
  expect_identical(api_first_same("huxreg", "huxtable", current_fn = hr, quick = FALSE), "1.2.0")
})


test_that("Can set pastapi.lib_dir and functions work", {
  skip_on_os("windows")

  tempdir <- tempfile(pattern = "testing", tmpdir = normalizePath(tempdir()))
  dir.create(tempdir)
  options(pastapi.lib_dir = tempdir)
  id <- parallel::mcparallel(get_fn_at("expand_urls", "longurl", "0.3.0"))
  parallel::mccollect(id)
  expect_true(dir.exists(file.path(tempdir, "longurl-0.3.0", "longurl")))
  expect_error(get_fn_at("expand_urls", "longurl", "0.3.0"), regexp = NA)
})
