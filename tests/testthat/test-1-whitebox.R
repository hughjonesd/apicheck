
context("Whitebox tests")

old_lib_dir <- NULL
old_opts <- NULL

setup({
  old_lib_dir <<- get_lib_dir()
  old_opts <<- options(mc.cores = 2) # for travis
  # we want a new directory for these tests, otherwise last test will delete your pre-cached files
  set_lib_dir(NULL)
})


teardown({
  clear_lib_dir()
  options(old_opts)
  set_lib_dir(old_lib_dir)
})


test_that("parse_fun", {
  expect_identical(apicheck:::parse_fun("foo::bar"), c("foo", "bar"))
})


test_that("Failure to install a file does not leave a directory on disk", {
  tempdir <- tempfile(pattern = "testing", tmpdir = normalizePath(tempdir()))
  dir.create(tempdir)
  set_lib_dir(tempdir)
  fail <- function (...) stop("Some crazy failure")
  with_mock(
    `versions::install.versions` = fail,
    `remotes::install_version`   = fail, {
    expect_error(fun_at("expand_urls", "longurl", "0.3.0"), "Some crazy failure")
    expect_false(dir.exists(file.path(tempdir, "longurl")))
  })
})


test_that("na_binary_search", {
  run_bs <- function (x) {
    f <- function (n) x[n]
    apicheck:::na_binary_search(1L, length(!!x), f)
  }

  # TRUE can be "assumed FALSE" -1, if L of a FALSE; but never known FALSE -2;
  # FALSE can be "assumed TRUE" 1, if R of a TRUE; but never known TRUE 2;
  # NA can be -1, 0 or 1 but never 2 or -2
  check_result <- function (input) {
    output <- run_bs(input)
    expect_length(output, length(input))
    expect_type(output, "integer")
    expect_true(all(output %in% seq(-2L, 2L)))
    nas <- is.na(input)
    expect_true(all(output[nas] %in% seq(-1L, 1L)))
    output <- output[! nas]
    input <- input[! nas]
    if (length(output) == 0) return()
    expect_true(all(!! output[input] > -2L))
    expect_true(all(!! output[! input] < 2L))
    # check assumed TRUE/FALSE are to right/left of a known TRUE/FALSE value:
    assumed_true <- which( output == 1L)
    assumed_false <- which(output == -1L)
    # if the args to which are zero-length, then throw an error,
    # as there are no known TRUE/FALSE values:
    expect_true(all(assumed_true > min(which(input), length(input) + 1)))
    expect_true(all(assumed_false < max(which(! input), 0)))
  }

  check_result(TRUE)
  check_result(FALSE)
  check_result(NA)
  for (ln in 2:10) {
    for (i in 1:30) {
      x <- sample(c(TRUE, FALSE, NA), ln, replace = TRUE)
      check_result(x)
    }
  }
})


test_that("search_versions", {

    test_vars <- function (search, ...) {
      x <- as.logical(list(...))
      test <- function (y) x[y]
      apicheck:::search_versions(1:3, test, search)
    }

    expect_equal(test_vars("forward", TRUE, FALSE, TRUE), c(2L, 1L, 1L))
    expect_equal(test_vars("forward", TRUE, NA, TRUE), c(2L, 1L, 1L))
    expect_equal(test_vars("forward", FALSE, NA, TRUE), c(-2L, 0L, 2L))

    expect_equal(test_vars("backward", TRUE, FALSE, TRUE), c(-1L, -2L, 2L))
    expect_equal(test_vars("backward", TRUE, FALSE, TRUE), c(-1L, -2L, 2L))
    expect_equal(test_vars("backward", TRUE, NA, FALSE), c(-1L, -1L, -2L))
})


test_that("search_all", {
  test_vars <- function (search, ...) {
    x <- as.logical(list(...))
    test <- function (y) x[y]
    apicheck:::search_all(seq_along(x), test, search)
  }

  for (strat in c("all", "parallel")) expect_equal(test_vars(strat, FALSE, NA, TRUE), c(-2L, 0, 2L))
})


test_that("clear_lib_dir", {
  ld <- get_lib_dir()
  cat("blah", file = file.path(ld, "notarealpackage-0.2.0"))
  clear_lib_dir()
  expect_length(list.files(ld), 0)
})


test_that("get_current_ns", {
  expect_silent(ns <- apicheck:::get_current_ns("desc"))
  expect_is(ns, "environment")
  expect_equivalent(getNamespaceName(ns), "desc")
  expect_error(apicheck:::get_current_ns("nonexistentpackage"))
})


test_that("args_match", {
  fun_named <- function (a) NULL
  fun_named_default <- function (a = 1) NULL
  fun_2names_default <- function (a = 1, b = 2) NULL
  fun_dots <- function (...) NULL
  fun_named_dots <- function (a, ...) NULL

  am <- apicheck:::args_match
  expect_true(am(fun_named, a = 1))
  expect_true(am(fun_named, 1))
  expect_true(am(fun_named)) # can be called even though might fail due to lacking arguments
  expect_false(am(fun_named, b = 1))

  expect_true(am(fun_named_default, a = 1))
  expect_true(am(fun_named_default, 1))
  expect_true(am(fun_named_default))
  expect_false(am(fun_named_default, b = 1))

  expect_true(am(fun_2names_default, a = 1, b = 2))
  expect_true(am(fun_2names_default, 1, b = 2))
  expect_true(am(fun_2names_default, b = 1, a = 2))
  expect_true(am(fun_2names_default, b = 1, 2))
  expect_true(am(fun_2names_default, a = 1))
  expect_true(am(fun_2names_default, b = 1))
  expect_true(am(fun_2names_default))

  expect_true(am(fun_dots))
  expect_true(am(fun_dots, 1))
  expect_true(am(fun_dots, 1, 2))
  expect_true(am(fun_dots, bar = 1, 2, baz = 3))

  expect_true(am(fun_named_dots, 1))
  expect_true(am(fun_named_dots, a = 1))
  expect_true(am(fun_named_dots, a = 1, 2))
  expect_true(am(fun_named_dots, 2, a = 1))
  expect_true(am(fun_named_dots, b = 1)) # again this is ok
})


test_that("fun_names_in_ns", {
  expect_silent(desc_funs <- apicheck:::fun_names_in_ns(getNamespace("desc"), FALSE))
  expect_true("desc" %in% desc_funs)
  expect_false("format.DescriptionRemotes" %in% desc_funs)
  expect_silent(desc_fun_methods <- apicheck:::fun_names_in_ns(getNamespace("desc"), TRUE))
  expect_true("desc" %in% desc_fun_methods)
  expect_true("format.DescriptionRemotes" %in% desc_fun_methods)
})


test_that("previous_version", {
  expect_equivalent(apicheck:::previous_version("clipr", "0.4.0"), "0.3.3")
  expect_equivalent(apicheck:::previous_version("clipr", "0.3.1"), "0.3.0")
})


test_that("current_version", {
  # vioplot: last updated on CRAN in 2005 :-D
  expect_equivalent(apicheck:::current_version("vioplot"), "0.2")
})
